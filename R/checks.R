# ============================================================================
# checks.R - Schema validators + Path B exported validator + provider preflight
#
# 11 functions per Sec. 37.5 Step 2.3:
#   6 internal schema validators with (x, abort = TRUE) signature
#   1 exported cacs_acs_validate() - Sec. 6.7 Path B wrapper
#   4 provider preflight (OSRM/ORS functional, Mapbox/r5r fail-loud stubs)
# ============================================================================


# Sec. 12.3.1 canonical 23 mandatory long-format columns.
# v0.2 F4 grew the canonical schema from 20 to 22 by adding
# `n_tracts_num` + `n_tracts_den` on derived rate rows (NA on source rows;
# the existing `n_tracts` column is preserved unchanged to keep the
# `is.na(n_tracts)` rate-row filter idiom working). v0.3 adds
# `ring_topology` to make cumulative-ring provenance explicit.
.LONG_REQUIRED_COLS <- c(
  "site_id", "drive_time_min", "ring_topology", "variable", "estimate", "moe",
  "weight_sum", "n_tracts", "n_tracts_num", "n_tracts_den",
  "provider", "profile", "osm_snapshot_date",
  "acs_year", "weight_method", "estimand_family", "weight_basis",
  "moe_formula_requested", "moe_formula_effective", "moe_fallback",
  "moe_fallback_reason", "failure_origin", "weight_uncertainty_propagated"
)

# Sec. 12.3.1.1 internal aggregation carrier attribute keys (v0.2: 10 cols).
# `n_tracts` added in v0.2 (Step 2.3) so derive-rates.R can populate the
# new `n_tracts_num` / `n_tracts_den` columns via the existing
# `.carrier_key_lookup()` infrastructure (Sec. 22.6 packed-key contract).
.CARRIER_REQUIRED_COLS <- c(
  "site_id", "drive_time_min", "variable", "estimand_family",
  "est_total", "var_total_raw", "est_mean", "var_mean_raw", "weight_sum",
  "n_tracts"
)


#' Validate carrier attribute table (internal)
#' @keywords internal
#' @noRd
.validate_carrier_tbl <- function(car,
                                  x = NULL,
                                  abort = TRUE,
                                  require_symmetric_keys = FALSE) {
  fail <- function(message) {
    if (abort) .cli_abort_schema(message)
    invisible(FALSE)
  }

  if (!is.data.frame(car)) {
    return(fail("{.attr cacs_aggregation_carriers} must be a data frame or tibble."))
  }

  car_missing <- setdiff(.CARRIER_REQUIRED_COLS, names(car))
  if (length(car_missing) > 0L) {
    return(fail(c(
      "Carrier attribute missing required column(s): {.field {car_missing}}."
    )))
  }

  if (!is.character(car$site_id)) {
    return(fail("{.field site_id} in carrier must be character."))
  }
  if (!is.character(car$variable)) {
    return(fail("{.field variable} in carrier must be character."))
  }
  if (!is.character(car$estimand_family)) {
    return(fail("{.field estimand_family} in carrier must be character."))
  }
  if (!is.integer(car$drive_time_min) && !is.numeric(car$drive_time_min)) {
    return(fail("{.field drive_time_min} in carrier must be integer or whole-number numeric."))
  }
  bad_drive_time <- !is.na(car$drive_time_min) &
    (car$drive_time_min != floor(car$drive_time_min))
  if (any(bad_drive_time)) {
    return(fail("{.field drive_time_min} in carrier must contain whole-minute values."))
  }

  key_cols <- c("site_id", "drive_time_min", "variable")
  if (anyNA(car[key_cols])) {
    return(fail("{.attr cacs_aggregation_carriers} key columns must not contain NA."))
  }

  car_key <- do.call(paste, c(car[key_cols], sep = "\r"))
  if (anyDuplicated(car_key)) {
    return(fail("{.attr cacs_aggregation_carriers} contains duplicate site/time/variable keys."))
  }

  bad_family <- setdiff(unique(car$estimand_family), .ESTIMAND_FAMILIES)
  if (length(bad_family) > 0L) {
    return(fail(c(
      "{.field estimand_family} in carrier contains unsanctioned value(s).",
      "x" = "Got {.val {bad_family}}."
    )))
  }

  numeric_cols <- c("est_total", "var_total_raw", "est_mean",
                    "var_mean_raw", "weight_sum")
  non_numeric <- numeric_cols[!vapply(car[numeric_cols], is.numeric, logical(1))]
  if (length(non_numeric) > 0L) {
    return(fail("Carrier numeric column(s) not numeric: {.field {non_numeric}}."))
  }

  for (nm in c("var_total_raw", "var_mean_raw", "weight_sum")) {
    bad <- !is.na(car[[nm]]) & car[[nm]] < 0
    if (any(bad)) {
      return(fail("{.field {nm}} in carrier must be nonnegative where nonmissing."))
    }
  }

  if (!is.null(x)) {
    long_key_rows <- if ("failure_origin" %in% names(x)) {
      is.na(x$failure_origin) | x$failure_origin == "none"
    } else {
      rep(TRUE, nrow(x))
    }
    long_key <- do.call(paste, c(x[long_key_rows, key_cols], sep = "\r"))
    missing_long_key <- setdiff(unique(long_key), unique(car_key))
    if (length(missing_long_key) > 0L) {
      return(fail(c(
        "Carrier attribute is missing key(s) for non-failure output rows.",
        "x" = "Missing key count: {.val {length(missing_long_key)}}."
      )))
    }
    if (isTRUE(require_symmetric_keys)) {
      extra_carrier_key <- setdiff(unique(car_key), unique(long_key))
      if (length(extra_carrier_key) > 0L) {
        return(fail(c(
          "Carrier attribute contains unexpected key(s) with no non-failure output row.",
          "x" = "Extra key count: {.val {length(extra_carrier_key)}}."
        )))
      }
    }
  }

  invisible(TRUE)
}


# ============================================================================
# 1/6 - .validate_iso_schema()  (Sec. 19.5 + Sec. 12.2.2)
# ============================================================================

#' Validate isochrone sf schema (internal)
#' @keywords internal
#' @noRd
.validate_iso_schema <- function(x, abort = TRUE) {
  if (!inherits(x, "sf")) {
    if (abort) {
      .cli_abort_schema_with_template(
        "{.arg iso_sf} must be an {.cls sf} object.",
        list(.schema_issue(
          check = "iso_not_sf",
          actual = paste0("<", .schema_first_class(x), ">"),
          expected = "sf object with canonical cacs_isochrone() columns",
          fix_hint = "Pass cacs_isochrone() output, or convert a data frame with geometry back to sf.",
          example = "iso_sf <- sf::st_as_sf(iso_sf, sf_column_name = \"geometry\", crs = 4326)"
        )),
        help = paste(
          "Run cacs_validate_iso(iso_sf) before intersection.",
          .schema_vignette_hint()
        )
      )
    }
    return(invisible(FALSE))
  }
  has_ring_topology <- "ring_topology" %in% names(x)
  ring_topology_chr <- if (has_ring_topology) {
    as.character(x$ring_topology)
  } else {
    character(0)
  }
  annulus_by_topology <- has_ring_topology &&
    any(ring_topology_chr == "annulus", na.rm = TRUE)
  annulus_by_isomin <- FALSE
  if ("isomin" %in% names(x)) {
    isomin_num <- suppressWarnings(as.numeric(x$isomin))
    annulus_by_isomin <- any(isomin_num > 0, na.rm = TRUE)
  }
  if (annulus_by_topology || annulus_by_isomin) {
    if (abort) {
      cumulative_labeled_isomin <- !annulus_by_topology &&
        has_ring_topology &&
        any(ring_topology_chr == "cumulative", na.rm = TRUE) &&
        annulus_by_isomin
      if (cumulative_labeled_isomin) {
        .cli_abort_annulus_input(c(
          "{.arg iso_sf} is labeled {.code ring_topology = \"cumulative\"} but has {.code isomin > 0} rows.",
          "i" = "v0.3.0 cumulative rings require {.code isomin == 0} on every row.",
          "i" = "Re-derive or convert with: {.code iso_cumulative <- cacs_rings_to_cumulative(iso_sf)}",
          "i" = "Preflight with: {.code cacs_validate_iso(iso_sf)}"
        ))
      } else {
        .cli_abort_annulus_input(c(
          "{.arg iso_sf} has annulus topology ({.code (isomin, isomax]}).",
          "i" = "v0.3.0 accepts cumulative-only rings ({.code [0, isomax]}) for {.fn cacs_intersect_weight}.",
          "i" = "Convert explicitly with: {.code iso_cumulative <- cacs_rings_to_cumulative(iso_sf)}",
          "i" = "Preflight with: {.code cacs_validate_iso(iso_sf)}"
        ))
      }
    }
    return(invisible(FALSE))
  }
  required <- .ISO_CANONICAL_COLS
  missing  <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    if (abort) {
      issues <- lapply(missing, function(missing_col) {
        .schema_issue(
          check = "iso_missing_column",
          col = missing_col,
          actual = "missing",
          expected = "present in canonical 16-column schema",
          fix_hint = .iso_missing_col_hint(missing_col),
          example = .iso_missing_col_example(missing_col)
        )
      })
      .cli_abort_schema_with_template(
        "{.arg iso_sf} missing required column{?s}: {.field {missing}}.",
        issues,
        help = paste(
          "Run cacs_validate_iso(iso_sf) to list all schema issues.",
          .schema_vignette_hint()
        )
      )
    }
    return(invisible(FALSE))
  }
  if (!is.character(x$ring_topology) || anyNA(x$ring_topology) ||
      any(x$ring_topology != "cumulative")) {
    if (abort) {
      .cli_abort_schema_with_template(
        "{.field ring_topology} must be {.val cumulative} for v0.3.0.",
        list(.schema_issue(
          check = "iso_ring_topology",
          col = "ring_topology",
          actual = paste(.schema_values(x$ring_topology), collapse = ", "),
          expected = "cumulative",
          fix_hint = "Use cumulative rings only.",
          example = "iso_sf <- cacs_rings_to_cumulative(iso_sf)"
        )),
        help = paste(
          "Run cacs_validate_iso(iso_sf) before intersection.",
          .schema_vignette_hint()
        )
      )
    }
    return(invisible(FALSE))
  }
  crs <- sf::st_crs(x)$epsg
  if (is.na(crs) || !identical(as.integer(crs), 4326L)) {
    if (abort) {
      .cli_abort_schema_with_template(
        "{.arg iso_sf} CRS must be {.val EPSG:4326}.",
        list(.schema_issue(
          check = "iso_crs",
          col = "geometry",
          actual = if (is.na(crs)) "NA" else paste0("EPSG:", as.integer(crs)),
          expected = "EPSG:4326",
          fix_hint = "Re-project the isochrone to WGS84.",
          example = "iso_sf <- sf::st_transform(iso_sf, 4326)"
        )),
        help = paste(
          "Run cacs_validate_iso(iso_sf) to check CRS and schema.",
          .schema_vignette_hint()
        )
      )
    }
    return(invisible(FALSE))
  }
  gtypes <- as.character(sf::st_geometry_type(x))
  bad_gtype <- setdiff(unique(gtypes), c("POLYGON", "MULTIPOLYGON"))
  if (length(bad_gtype) > 0L) {
    if (abort) {
      .cli_abort_schema(c(
        "{.arg iso_sf} geometry type must be {.val POLYGON} or {.val MULTIPOLYGON}.",
        "x" = "Got {.val {bad_gtype}}."
      ))
    }
    return(invisible(FALSE))
  }
  provider_choices <- .SANCTIONED_PROVIDERS
  bad_prov <- setdiff(unique(x$provider), provider_choices)
  if (length(bad_prov) > 0L) {
    if (abort) {
      .cli_abort_schema(c(
        "{.field provider} contains unsupported value(s).",
        "x" = "Got {.val {bad_prov}}.",
        "i" = "Sanctioned set: {.val {provider_choices}}."
      ))
    }
    return(invisible(FALSE))
  }
  bad_prov_req <- setdiff(unique(x$provider_requested), provider_choices)
  if (length(bad_prov_req) > 0L) {
    if (abort) {
      .cli_abort_schema(c(
        "{.field provider_requested} contains unsupported value(s).",
        "x" = "Got {.val {bad_prov_req}}.",
        "i" = "Sanctioned set: {.val {provider_choices}}."
      ))
    }
    return(invisible(FALSE))
  }
  if (!all(is.na(x$drive_time_min) | x$drive_time_min > 0)) {
    if (abort) {
      .cli_abort_schema(c(
        "{.field drive_time_min} must be positive.",
        "i" = "Negative or zero values violate Sec. 12.2.2 contract."
      ))
    }
    return(invisible(FALSE))
  }
  if (!is.character(x$site_id) || anyNA(x$site_id) || any(!nzchar(x$site_id))) {
    if (abort) {
      .cli_abort_schema("{.field site_id} must be non-empty character values.")
    }
    return(invisible(FALSE))
  }
  if (!is.integer(x$drive_time_min)) {
    if (abort) {
      .cli_abort_schema("{.field drive_time_min} must be integer.")
    }
    return(invisible(FALSE))
  }
  if (!is.logical(x$isochrone_empty) ||
      !is.logical(x$provider_downgrade)) {
    if (abort) {
      logical_cols <- c("isochrone_empty", "provider_downgrade")
      bad_logical <- logical_cols[
        !vapply(logical_cols, function(logical_col) {
          is.logical(x[[logical_col]])
        }, logical(1))
      ]
      issues <- lapply(bad_logical, function(logical_col) {
        .schema_issue(
          check = paste0("iso_", logical_col, "_type"),
          col = logical_col,
          actual = paste0("<", .schema_first_class(x[[logical_col]]), ">"),
          expected = "logical TRUE/FALSE",
          fix_hint = "Use unquoted TRUE/FALSE values.",
          example = paste0("iso_sf$", logical_col, " <- FALSE")
        )
      })
      .cli_abort_schema_with_template(
        "{.field isochrone_empty} and {.field provider_downgrade} must be logical.",
        issues,
        help = paste(
          "Run cacs_validate_iso(iso_sf) to list all schema issues.",
          .schema_vignette_hint()
        )
      )
    }
    return(invisible(FALSE))
  }
  if (!is.integer(x$retry_count)) {
    if (abort) {
      .cli_abort_schema("{.field retry_count} must be integer.")
    }
    return(invisible(FALSE))
  }
  invisible(TRUE)
}


# ============================================================================
# 2/6 - .validate_acs_schema()  (Sec. 20.5 + Sec. 12.2.3 + Sec. 6.4 Step 3)
# ============================================================================

#' Validate ACS tract sf schema (internal)
#' @keywords internal
#' @noRd
.validate_acs_schema <- function(x, abort = TRUE) {
  if (!inherits(x, "sf")) {
    if (abort) {
      .cli_abort_schema(c(
        "{.arg acs_sf} must be an {.cls sf} object.",
        "x" = "Got {.cls {class(x)[[1]]}}.",
        "i" = "See {.help cacs_acs_validate} for the Sec. 6.4 Step 3 6-check spec."
      ))
    }
    return(invisible(FALSE))
  }
  crs <- sf::st_crs(x)$epsg
  if (is.na(crs) || !identical(as.integer(crs), 4269L)) {
    if (abort) {
      .cli_abort_schema(c(
        "{.arg acs_sf} CRS must be {.val EPSG:4269} (NAD83).",
        "x" = "Got {.val {if (is.na(crs)) 'NA' else as.character(crs)}}.",
        "i" = "tidycensus defaults to NAD83."
      ))
    }
    return(invisible(FALSE))
  }
  required <- c("GEOID", "NAME", "variable", "estimate", "moe", "geometry")
  missing  <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    if (abort) {
      .cli_abort_schema(c(
        "{.arg acs_sf} missing required column{?s}: {.field {missing}}.",
        "i" = "Expected tidy ACS schema per Sec. 12.2.3."
      ))
    }
    return(invisible(FALSE))
  }
  if (nrow(x) == 0L) {
    if (abort) {
      .cli_abort_schema(c(
        "{.arg acs_sf} has zero rows.",
        "i" = "Empty ACS sf cannot drive downstream intersection."
      ))
    }
    return(invisible(FALSE))
  }
  if (!all(grepl("^\\d{11}$", x$GEOID))) {
    if (abort) {
      bad <- utils::head(x$GEOID[!grepl("^\\d{11}$", x$GEOID)], 5L)
      .cli_abort_schema(c(
        "{.field GEOID} must be exactly 11 digits.",
        "x" = "First malformed values: {.val {bad}}."
      ))
    }
    return(invisible(FALSE))
  }
  if (!is.numeric(x$estimate) || !is.numeric(x$moe)) {
    if (abort) {
      .cli_abort_schema(c(
        "{.field estimate} and {.field moe} must be {.cls numeric}.",
        "x" = "Got {.cls {class(x$estimate)[[1]]}} / {.cls {class(x$moe)[[1]]}}."
      ))
    }
    return(invisible(FALSE))
  }
  invisible(TRUE)
}


# ============================================================================
# 3/6 - .validate_bg_pop_schema()  (v0.1 stub-only - Decision #5)
# ============================================================================

#' Validate block-group population sf schema (internal, v0.1 stub)
#' @keywords internal
#' @noRd
.validate_bg_pop_schema <- function(x, abort = TRUE) {
  if (!inherits(x, "sf")) {
    if (abort) {
      .cli_abort_schema(c(
        "{.arg bg_pop_sf} must be an {.cls sf} object.",
        "x" = "Got {.cls {class(x)[[1]]}}.",
        "i" = "Block-group population sf is required for {.val weight_method = \"population\"} (v0.2)."
      ))
    }
    return(invisible(FALSE))
  }
  required <- c("GEOID", "tract_geoid", "population_est", "geometry")
  missing  <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    if (abort) {
      .cli_abort_schema(c(
        "{.arg bg_pop_sf} missing required column{?s}: {.field {missing}}."
      ))
    }
    return(invisible(FALSE))
  }
  crs <- sf::st_crs(x)$epsg
  if (is.na(crs)) {
    if (abort) {
      .cli_abort_schema(c(
        "{.arg bg_pop_sf} CRS must be defined.",
        "i" = "Set CRS before using block-group population weighting."
      ))
    }
    return(invisible(FALSE))
  }
  if (!all(grepl("^\\d{12}$", x$GEOID))) {
    if (abort) {
      bad <- utils::head(x$GEOID[!grepl("^\\d{12}$", x$GEOID)], 5L)
      .cli_abort_schema(c(
        "{.field GEOID} must be exactly 12 digits for block groups.",
        "x" = "First malformed values: {.val {bad}}."
      ))
    }
    return(invisible(FALSE))
  }
  if (!all(grepl("^\\d{11}$", x$tract_geoid))) {
    if (abort) {
      bad <- utils::head(x$tract_geoid[!grepl("^\\d{11}$", x$tract_geoid)], 5L)
      .cli_abort_schema(c(
        "{.field tract_geoid} must be exactly 11 digits.",
        "x" = "First malformed values: {.val {bad}}."
      ))
    }
    return(invisible(FALSE))
  }
  if (!all(substr(x$GEOID, 1L, 11L) == x$tract_geoid)) {
    if (abort) {
      .cli_abort_schema(c(
        "{.field tract_geoid} must match the first 11 digits of {.field GEOID}."
      ))
    }
    return(invisible(FALSE))
  }
  if (!is.numeric(x$population_est) ||
      any(is.na(x$population_est)) ||
      any(x$population_est < 0)) {
    if (abort) {
      .cli_abort_schema("{.field population_est} must be non-missing, numeric, and nonnegative.")
    }
    return(invisible(FALSE))
  }
  invisible(TRUE)
}


# ============================================================================
# 4/6 - .validate_intersect_output()  (Sec. 12.3.1 long schema + carrier)
# ============================================================================

#' Validate cacs_intersect_weight() canonical long output (internal)
#' @keywords internal
#' @noRd
.validate_intersect_output <- function(x,
                                       abort = TRUE,
                                       require_carrier = TRUE,
                                       require_symmetric_keys = FALSE) {
  if (!inherits(x, "tbl_df")) {
    if (abort) {
      .cli_abort_schema(c(
        "{.fn cacs_intersect_weight} output must be a {.cls tbl_df}.",
        "x" = "Got {.cls {class(x)[[1]]}}."
      ))
    }
    return(invisible(FALSE))
  }
  missing <- setdiff(.LONG_REQUIRED_COLS, names(x))
  if (length(missing) > 0L) {
    if (abort) {
      .cli_abort_schema(c(
        "{.fn cacs_intersect_weight} output missing column{?s}: {.field {missing}}.",
        "i" = "Sec. 12.3.1 mandates a 23-column long schema (v0.3 adds ring_topology)."
      ))
    }
    return(invisible(FALSE))
  }
  if (!is.character(x$ring_topology) || anyNA(x$ring_topology) ||
      any(x$ring_topology != "cumulative")) {
    if (abort) {
      .cli_abort_schema(c(
        "{.field ring_topology} in long output must be {.val cumulative}.",
        "i" = "Annulus isochrones must be converted before weighting."
      ))
    }
    return(invisible(FALSE))
  }
  car <- attr(x, "cacs_aggregation_carriers")
  if (is.null(car)) {
    if (!isTRUE(require_carrier)) {
      sv <- attr(x, "cacs_schema_version")
      if (is.null(sv) || !identical(sv, "1.0")) {
        if (abort) {
          .cli_abort_schema(c(
            "{.code attr(x, \"cacs_schema_version\")} must be {.val 1.0}.",
            "x" = "Got {.val {if (is.null(sv)) 'NULL' else as.character(sv)}}."
          ))
        }
        return(invisible(FALSE))
      }
      return(invisible(TRUE))
    }
    if (abort) {
      .cli_abort_schema(c(
        "{.fn cacs_intersect_weight} output missing carrier attribute.",
        "i" = "Sec. 12.3.1.1 requires the carrier tibble for downstream MOE / rate dispatch."
      ))
    }
    return(invisible(FALSE))
  }
  ok_carrier <- .validate_carrier_tbl(
    car,
    x = x,
    abort = abort,
    require_symmetric_keys = require_symmetric_keys
  )
  if (!isTRUE(ok_carrier)) {
    return(invisible(FALSE))
  }
  sv <- attr(x, "cacs_schema_version")
  if (is.null(sv) || !identical(sv, "1.0")) {
    if (abort) {
      .cli_abort_schema(c(
        "{.code attr(x, \"cacs_schema_version\")} must be {.val 1.0}.",
        "x" = "Got {.val {if (is.null(sv)) 'NULL' else as.character(sv)}}."
      ))
    }
    return(invisible(FALSE))
  }
  audit <- attr(x, "cacs_tract_audit", exact = TRUE)
  if (!is.null(audit)) {
    audit_cols <- c("site_id", "drive_time_min", "GEOID", "area_wt",
                    "int_area_m2", "tract_area_m2")
    fail_audit <- function(message) {
      if (abort) .cli_abort_schema(message)
      invisible(FALSE)
    }
    if (!inherits(audit, c("tbl_df", "data.frame"))) {
      return(fail_audit("{.attr cacs_tract_audit} must be a data frame or tibble."))
    }
    missing_audit <- setdiff(audit_cols, names(audit))
    if (length(missing_audit) > 0L) {
      return(fail_audit(c(
        "{.attr cacs_tract_audit} missing column{?s}: {.field {missing_audit}}.",
        "i" = "Expected: {.field {audit_cols}}."
      )))
    }
    extra_audit <- setdiff(names(audit), audit_cols)
    if (length(extra_audit) > 0L) {
      return(fail_audit(c(
        "{.attr cacs_tract_audit} contains unexpected column{?s}: {.field {extra_audit}}.",
        "i" = "Expected exactly: {.field {audit_cols}}."
      )))
    }
    if (!is.character(audit$site_id) ||
        !is.integer(audit$drive_time_min) ||
        !is.character(audit$GEOID) ||
        !is.numeric(audit$area_wt) ||
        !is.numeric(audit$int_area_m2) ||
        !is.numeric(audit$tract_area_m2)) {
      return(fail_audit(c(
        "{.attr cacs_tract_audit} columns have invalid storage types.",
        "i" = "Expected character/integer/character/numeric/numeric/numeric."
      )))
    }
    if (nrow(audit) > 0L) {
      if (anyNA(audit$site_id) || anyNA(audit$drive_time_min) ||
          anyNA(audit$GEOID)) {
        return(fail_audit("{.attr cacs_tract_audit} key columns must not contain NA."))
      }
      if (anyNA(audit$area_wt) ||
          anyNA(audit$int_area_m2) ||
          anyNA(audit$tract_area_m2) ||
          any(!is.finite(audit$area_wt) | audit$area_wt < 0 |
              audit$area_wt > 1, na.rm = TRUE) ||
          any(!is.finite(audit$int_area_m2) | audit$int_area_m2 < 0,
              na.rm = TRUE) ||
          any(!is.finite(audit$tract_area_m2) | audit$tract_area_m2 <= 0,
              na.rm = TRUE)) {
        return(fail_audit(c(
          "{.attr cacs_tract_audit} numeric columns are out of range.",
          "i" = "{.field area_wt} must be in [0, 1], {.field int_area_m2} >= 0, and {.field tract_area_m2} > 0."
        )))
      }
      audit_key <- do.call(paste, c(audit[, c("site_id", "drive_time_min", "GEOID")],
                                    sep = "\r"))
      if (anyDuplicated(audit_key)) {
        return(fail_audit("{.attr cacs_tract_audit} contains duplicate site/time/GEOID keys."))
      }
    }
  }
  invisible(TRUE)
}


# ============================================================================
# 5/6 - .validate_propagate_output()  (Sec. 22.3 / Sec. 12.3.1)
# ============================================================================

#' Validate cacs_propagate_moe() output (internal)
#' @keywords internal
#' @noRd
.validate_propagate_output <- function(x, abort = TRUE) {
  ordinary_rows <- if ("failure_origin" %in% names(x)) {
    is.na(x$failure_origin) | x$failure_origin == "none"
  } else {
    rep(TRUE, nrow(x))
  }
  require_carrier <- any(
    ordinary_rows & x$estimand_family != "metadata_only",
    na.rm = TRUE
  )
  ok <- .validate_intersect_output(x, abort = FALSE,
                                   require_carrier = require_carrier)
  if (!isTRUE(ok)) {
    if (abort) {
      .cli_abort_schema(c(
        "{.fn cacs_propagate_moe} output failed inherited Sec. 12.3.1 long-schema validation."
      ))
    }
    return(invisible(FALSE))
  }
  return_se <- isTRUE(getOption("cacs.return_se", FALSE))
  if (return_se && !"se" %in% names(x)) {
    if (abort) {
      .cli_abort_schema(c(
        "{.fn cacs_propagate_moe} output missing {.field se} column.",
        "i" = "{.code options(cacs.return_se = TRUE)} is set; Sec. 12.3.1 mandates the column."
      ))
    }
    return(invisible(FALSE))
  }
  bad_formula_na <- ordinary_rows & is.na(x$moe_formula_effective)
  if (any(bad_formula_na)) {
    if (abort) {
      .cli_abort_schema(c(
        "{.field moe_formula_effective} contains NA for ordinary output rows.",
        "i" = "Sec. 22.3 requires a sanctioned label when {.field failure_origin} is {.val none}; failure sentinel rows may carry NA."
      ))
    }
    return(invisible(FALSE))
  }
  invisible(TRUE)
}


# ============================================================================
# 6/6 - .validate_derive_rates_output()  (Sec. 23 / Sec. 12.3.1)
# ============================================================================

#' Validate cacs_derive_rates() output (internal)
#' @keywords internal
#' @noRd
.validate_derive_rates_output <- function(x, abort = TRUE) {
  ok <- .validate_intersect_output(x, abort = FALSE, require_carrier = FALSE)
  if (!isTRUE(ok)) {
    if (abort) {
      .cli_abort_schema(c(
        "{.fn cacs_derive_rates} output failed inherited Sec. 12.3.1 long-schema validation."
      ))
    }
    return(invisible(FALSE))
  }
  sanctioned_rates <- names(.SANCTIONED_RATES_V1)
  rate_rows <- x$variable %in% sanctioned_rates
  if (!any(rate_rows)) {
    if (abort) {
      .cli_abort_schema(c(
        "{.fn cacs_derive_rates} output has no sanctioned rate rows.",
        "i" = "Sanctioned set: {.val {sanctioned_rates}}."
      ))
    }
    return(invisible(FALSE))
  }
  car <- attr(x, "cacs_aggregation_carriers")
  if (!is.null(car)) {
    car_missing <- setdiff(.CARRIER_REQUIRED_COLS, names(car))
    if (length(car_missing) > 0L) {
      if (abort) {
        .cli_abort_schema(c(
          "{.fn cacs_derive_rates} carrier attribute is partially mutated.",
          "i" = "Sec. 12.3.1.1 - carriers must be either preserved intact or removed entirely."
        ))
      }
      return(invisible(FALSE))
    }
  }
  invisible(TRUE)
}


# ============================================================================
# Exported - cacs_acs_validate()  (Sec. 6.7 Path B + Sec. 10.3 Tier 2)
# ============================================================================

#' Validate a user-supplied ACS tract table
#'
#' Checks that a user-supplied American Community Survey (ACS) `sf` tibble
#' (a spatial data frame of census tracts) meets the schema catchmentACS
#' expects before you feed it to [cacs_run()]. The six checks are: the object
#' is an `sf` object; its coordinate reference system is `NAD83`
#' (`EPSG:4269`); the required columns are present; every `GEOID` is an
#' 11-digit tract identifier; `estimate` and `moe` (margin of error) are
#' numeric; and the table has at least one row. Run this before calling
#' `cacs_run(acs = my_sf, ...)` with your own pre-fetched ACS data.
#'
#' @param x An ACS tract `sf` tibble.
#' @return Invisibly `TRUE` on success; aborts with a
#'   `catchmentACS_error_schema` condition on the first failed check.
#' @seealso [cacs_acs_prefetch()] for fetching a schema-valid ACS table,
#'   [cacs_intersect_weight()] and [cacs_run()] for the consumers that
#'   require this schema, and [cacs_validate_iso()] for the isochrone-side
#'   counterpart.
#' @family validation and conditions
#' @export
#' @examples
#' acs <- readRDS(system.file("extdata", "sample_alabama_subset.rds",
#'                            package = "catchmentACS"))
#' cacs_acs_validate(acs)
#'
#' \dontrun{
#' my_acs <- tidycensus::get_acs(
#'   geography = "tract", variables = "B01003_001",
#'   state = "AL", year = 2023, geometry = TRUE, output = "tidy"
#' )
#' cacs_acs_validate(my_acs)
#' }
cacs_acs_validate <- function(x) {
  .validate_acs_schema(x, abort = TRUE)
}


# ============================================================================
# Provider preflight 1/4 - .check_osrm_reachable()  (Sec. 19.4 row 1, v0.1 stub)
# ============================================================================

#' Preflight: OSRM provider availability (internal, v0.1 stub)
#' @keywords internal
#' @noRd
.check_osrm_reachable <- function(server = NULL) {
  rlang::check_installed("osrm", reason = "for provider = 'osrm'")
  invisible(TRUE)
}


# ============================================================================
# Provider preflight 2/4 - .check_ors_token()  (Sec. 19.4 row 2, v0.1 stub)
# ============================================================================

#' Preflight: openrouteservice package + token (internal, v0.1 stub)
#' @keywords internal
#' @noRd
.check_ors_token <- function(api_key = NULL) {
  rlang::check_installed("openrouteservice",
                         reason = "for provider = 'ors'")
  if (!is.null(api_key) && !nzchar(api_key)) {
    .cli_abort_credential(c(
      "{.field provider} = {.val ors} requires a non-empty {.envvar ORS_API_KEY}.",
      "x" = "Got an empty string.",
      "i" = "Set in {.file .Renviron} then restart R."
    ))
  }
  invisible(TRUE)
}


# ============================================================================
# Provider preflight 3/4 - .check_mapbox_token()  (Decision #5: fail-loud future)
# ============================================================================

#' Preflight: Mapbox token (internal, future-release deferral - fail-loud)
#' @keywords internal
#' @noRd
.check_mapbox_token <- function(token = NULL) {
  .cli_abort_credential(c(
    "Mapbox provider is deferred to a future release.",
    "i" = "Use {.val osrm} or {.val ors} in the current release."
  ))
}


# ============================================================================
# Provider preflight 4/4 - .check_r5r_core()  (Decision #5: fail-loud future)
# ============================================================================

#' Preflight: r5r core (internal, future-release deferral - fail-loud)
#' @keywords internal
#' @noRd
.check_r5r_core <- function(r5r_core = NULL) {
  .cli_abort_credential(c(
    "r5r provider is deferred to a future release.",
    "i" = "Use {.val osrm} or {.val ors} in the current release."
  ))
}
