# checks.R - the checks on the form of the data the steps hand each other, the
# exported check on ACS data, and the checks made before a routing provider is
# used.
#
# Every internal check here takes an `abort` argument: with `abort = TRUE` it
# stops at the first problem it finds, and with `abort = FALSE` it returns
# `FALSE` instead, so a caller can ask whether the data fit without stopping.


# The 23 columns that the long output of every step must have. On a rate row
# `n_tracts` is NA and the two counts are in `n_tracts_num` and
# `n_tracts_den`, so code that picks rate rows with `is.na(n_tracts)` keeps
# working (the rate row built in `.build_rate_row()`, R/derive-rates.R).
.LONG_REQUIRED_COLS <- c(
  "site_id", "drive_time_min", "ring_topology", "variable", "estimate", "moe",
  "weight_sum", "n_tracts", "n_tracts_num", "n_tracts_den",
  "provider", "profile", "osm_snapshot_date",
  "acs_year", "weight_method", "estimand_family", "weight_basis",
  "moe_formula_requested", "moe_formula_effective", "moe_fallback",
  "moe_fallback_reason", "failure_origin", "weight_uncertainty_propagated"
)

# The 10 columns of the `cacs_aggregation_carriers` attribute, the totals that
# cacs_intersect_weight() keeps for the later steps. `n_tracts` is one of them
# so that cacs_derive_rates() can read the tract counts of a rate's numerator
# and denominator from this one table.
.CARRIER_REQUIRED_COLS <- c(
  "site_id", "drive_time_min", "variable", "estimand_family",
  "est_total", "var_total_raw", "est_mean", "var_mean_raw", "weight_sum",
  "n_tracts"
)


#' Check the table of totals kept for the later steps
#'
#' Checks the `cacs_aggregation_carriers` attribute: its 10 columns and their
#' types, one row for each site, drive time, and variable, and no negative
#' variance or weight. When the output table `x` is given, every row of it that
#' is not a failure row must have a row here.
#'
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
      "The {.attr cacs_aggregation_carriers} attribute is missing the column{?s} {.field {car_missing}}."
    )))
  }

  if (!is.character(car$site_id)) {
    return(fail("{.field site_id} in the {.attr cacs_aggregation_carriers} attribute must be character."))
  }
  if (!is.character(car$variable)) {
    return(fail("{.field variable} in the {.attr cacs_aggregation_carriers} attribute must be character."))
  }
  if (!is.character(car$estimand_family)) {
    return(fail("{.field estimand_family} in the {.attr cacs_aggregation_carriers} attribute must be character."))
  }
  if (!is.integer(car$drive_time_min) && !is.numeric(car$drive_time_min)) {
    return(fail("{.field drive_time_min} in the {.attr cacs_aggregation_carriers} attribute must be integer or whole-number numeric."))
  }
  bad_drive_time <- !is.na(car$drive_time_min) &
    (car$drive_time_min != floor(car$drive_time_min))
  if (any(bad_drive_time)) {
    return(fail("{.field drive_time_min} in the {.attr cacs_aggregation_carriers} attribute must contain whole-minute values."))
  }

  key_cols <- c("site_id", "drive_time_min", "variable")
  if (anyNA(car[key_cols])) {
    return(fail("{.attr cacs_aggregation_carriers} key columns must not contain NA."))
  }

  car_key <- do.call(paste, c(car[key_cols], sep = "\r"))
  if (anyDuplicated(car_key)) {
    return(fail("{.attr cacs_aggregation_carriers} has more than one row for the same site, drive time, and variable."))
  }

  bad_family <- setdiff(unique(car$estimand_family), .ESTIMAND_FAMILIES)
  if (length(bad_family) > 0L) {
    return(fail(c(
      "{.field estimand_family} in the {.attr cacs_aggregation_carriers} attribute has a value that is not allowed.",
      "x" = "Got {.val {bad_family}}.",
      "i" = "Allowed values: {.val {(.ESTIMAND_FAMILIES)}}."
    )))
  }

  numeric_cols <- c("est_total", "var_total_raw", "est_mean",
                    "var_mean_raw", "weight_sum")
  non_numeric <- numeric_cols[!vapply(car[numeric_cols], is.numeric, logical(1))]
  if (length(non_numeric) > 0L) {
    return(fail("The column{?s} {.field {non_numeric}} of the {.attr cacs_aggregation_carriers} attribute must be numeric."))
  }

  for (nm in c("var_total_raw", "var_mean_raw", "weight_sum")) {
    bad <- !is.na(car[[nm]]) & car[[nm]] < 0
    if (any(bad)) {
      return(fail("{.field {nm}} in the {.attr cacs_aggregation_carriers} attribute must not be negative."))
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
        "The {.attr cacs_aggregation_carriers} attribute has no totals for {length(missing_long_key)} site, drive time, and variable combination{?s} in the rows.",
        "i" = "Results combined with {.fn dplyr::bind_rows} keep this attribute of the first result only; combine the drive-time areas or the ACS data before {.fn cacs_intersect_weight} instead."
      )))
    }
    if (isTRUE(require_symmetric_keys)) {
      extra_carrier_key <- setdiff(unique(car_key), unique(long_key))
      if (length(extra_carrier_key) > 0L) {
        return(fail(c(
          "The {.attr cacs_aggregation_carriers} attribute has totals for {length(extra_carrier_key)} site, drive time, and variable combination{?s} that have no row in the result."
        )))
      }
    }
  }

  invisible(TRUE)
}


#' Check the form of drive-time areas
#'
#' Checks what cacs_intersect_weight() needs of `iso_sf`: an sf object with the
#' 16 columns that cacs_isochrone() returns, rings that start at the site
#' (`ring_topology` is `"cumulative"`, and no row has `isomin` above 0),
#' EPSG:4326, polygon geometry, and the type of each column.
#' cacs_validate_iso() lists the same problems without stopping.
#'
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
          expected = "sf object with the columns of cacs_isochrone() output",
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
          "{.arg iso_sf} has {.code isomin > 0} rows but is labeled {.code ring_topology = \"cumulative\"}.",
          "i" = "{.fn cacs_intersect_weight} needs cumulative rings, with {.code isomin == 0} on every row.",
          "i" = "Convert them with:",
          " " = "{.code iso_cumulative <- cacs_rings_to_cumulative(iso_sf)}",
          "i" = "Check the areas first with {.code cacs_validate_iso(iso_sf)}."
        ))
      } else {
        .cli_abort_annulus_input(c(
          "{.arg iso_sf} has annulus topology ({.code (isomin, isomax]}).",
          "i" = "{.fn cacs_intersect_weight} accepts only cumulative rings ({.code [0, isomax]}).",
          "i" = "Convert them with:",
          " " = "{.code iso_cumulative <- cacs_rings_to_cumulative(iso_sf)}",
          "i" = "Check the areas first with {.code cacs_validate_iso(iso_sf)}."
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
          expected = "present in cacs_isochrone() output",
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
        "{.field ring_topology} must be {.val cumulative}.",
        list(.schema_issue(
          check = "iso_ring_topology",
          col = "ring_topology",
          actual = paste(.schema_values(x$ring_topology), collapse = ", "),
          expected = "cumulative",
          fix_hint = "If each area starts at the site, set ring_topology to \"cumulative\"; areas between two drive times need cacs_rings_to_cumulative().",
          example = "iso_sf$ring_topology <- \"cumulative\""
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
        "i" = "Allowed values: {.val {provider_choices}}."
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
        "i" = "Allowed values: {.val {provider_choices}}."
      ))
    }
    return(invisible(FALSE))
  }
  if (!all(is.na(x$drive_time_min) | x$drive_time_min > 0)) {
    if (abort) {
      .cli_abort_schema(c(
        "{.field drive_time_min} must be positive."
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


#' Check the form of ACS tract data
#'
#' The checks listed on the help page of cacs_acs_validate(), which calls
#' this function, and a check that no tract has two rows for the same
#' variable. The values are not checked, only the form.
#'
#' @keywords internal
#' @noRd
.validate_acs_schema <- function(x, abort = TRUE) {
  if (!inherits(x, "sf")) {
    if (abort) {
      .cli_abort_schema(c(
        "{.arg acs_sf} must be an {.cls sf} object.",
        "x" = "Got {.cls {class(x)[[1]]}}.",
        "i" = "See {.help cacs_acs_validate} for the checks."
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
        "i" = "{.fn cacs_acs_prefetch} returns these columns, as does",
        " " = "{.code tidycensus::get_acs(geometry = TRUE)}."
      ))
    }
    return(invisible(FALSE))
  }
  if (nrow(x) == 0L) {
    if (abort) {
      .cli_abort_schema(c(
        "{.arg acs_sf} has zero rows.",
        "i" = "With no rows there are no tracts to intersect with the drive-time areas."
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
  # A tract with two rows for one variable would be added twice.
  repeated <- duplicated(data.frame(GEOID = x$GEOID, variable = x$variable,
                                    stringsAsFactors = FALSE))
  if (any(repeated)) {
    if (abort) {
      first <- which(repeated)[[1L]]
      .cli_abort_schema(c(
        "{.arg acs_sf} has {sum(repeated)} row{?s} repeating a tract and variable.",
        "x" = "First repeated: {.field GEOID} {.val {x$GEOID[[first]]}}, {.field variable} {.val {x$variable[[first]]}}.",
        "i" = "Each tract needs one row for each variable; a repeated row would be counted twice."
      ))
    }
    return(invisible(FALSE))
  }
  invisible(TRUE)
}


#' Check the form of block-group population data
#'
#' Not reached: the only caller is the population-weighting branch of
#' `cacs_intersect_weight()` (R/intersect-weight.R), and that argument value
#' gives an error before the branch runs.
#'
#' @keywords internal
#' @noRd
.validate_bg_pop_schema <- function(x, abort = TRUE) {
  if (!inherits(x, "sf")) {
    if (abort) {
      .cli_abort_schema(c(
        "{.arg bg_pop_sf} must be an {.cls sf} object.",
        "x" = "Got {.cls {class(x)[[1]]}}.",
        "i" = "A block-group population {.cls sf} is required for {.code weight_method = \"population\"}."
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


#' Check the long output of cacs_intersect_weight()
#'
#' Checks the 23 columns, the `cacs_schema_version` attribute, and, with
#' `require_carrier = TRUE`, the totals table checked by
#' .validate_carrier_tbl(). The tract table, when it is kept, is checked too.
#' The two functions below call this one for the outputs of the later steps.
#'
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
        "i" = "Pass the result of {.fn cacs_intersect_weight} without removing columns."
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
        "{.fn cacs_intersect_weight} output has no {.attr cacs_aggregation_carriers} attribute.",
        "i" = "{.fn cacs_propagate_moe} and {.fn cacs_derive_rates} use this attribute, so keep it when you change the result."
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


#' Check the output of cacs_propagate_moe()
#'
#' The checks above, plus the `se` column when `options(cacs.return_se = TRUE)`
#' is set and a formula name on every row that is not a failure row. The totals
#' table is required only when such a row has an estimand family other than
#' `"metadata_only"`.
#'
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
        "{.fn cacs_propagate_moe} could not return a result in the form of {.fn cacs_intersect_weight} output.",
        "i" = "Pass the result of {.fn cacs_intersect_weight} with its columns and attributes kept, including {.attr cacs_schema_version}."
      ))
    }
    return(invisible(FALSE))
  }
  return_se <- isTRUE(getOption("cacs.return_se", FALSE))
  if (return_se && !"se" %in% names(x)) {
    if (abort) {
      .cli_abort_schema(c(
        "{.fn cacs_propagate_moe} output missing {.field se} column.",
        "i" = "{.code options(cacs.return_se = TRUE)} is set, so the {.field se} column is required."
      ))
    }
    return(invisible(FALSE))
  }
  bad_formula_na <- ordinary_rows & is.na(x$moe_formula_effective)
  if (any(bad_formula_na)) {
    if (abort) {
      .cli_abort_schema(c(
        "{.field moe_formula_effective} is {.code NA} on rows whose {.field failure_origin} is {.val none}.",
        "i" = "Rows of ACS codes that are not combined, such as {.val C17002_002}, have {.field estimand_family} {.val metadata_only} and no formula; remove those codes from the ACS data first."
      ))
    }
    return(invisible(FALSE))
  }
  invisible(TRUE)
}


#' Check the output of cacs_derive_rates()
#'
#' The checks above, plus at least one row for one of the five rates. The
#' totals table may be absent, because this step removes it, but a table that
#' is there must still have its 10 columns.
#'
#' @keywords internal
#' @noRd
.validate_derive_rates_output <- function(x, abort = TRUE) {
  ok <- .validate_intersect_output(x, abort = FALSE, require_carrier = FALSE)
  if (!isTRUE(ok)) {
    if (abort) {
      .cli_abort_schema(c(
        "{.fn cacs_derive_rates} could not return a result in the form of {.fn cacs_intersect_weight} output.",
        "i" = "Pass the result of {.fn cacs_propagate_moe} with its columns and attributes kept, including {.attr cacs_schema_version}."
      ))
    }
    return(invisible(FALSE))
  }
  sanctioned_rates <- names(.SANCTIONED_RATES_V1)
  rate_rows <- x$variable %in% sanctioned_rates
  if (!any(rate_rows)) {
    if (abort) {
      .cli_abort_schema(c(
        "{.fn cacs_derive_rates} output has no rows for the built-in rates.",
        "i" = "Built-in rates: {.val {sanctioned_rates}}."
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
          "The {.attr cacs_aggregation_carriers} attribute of {.fn cacs_derive_rates} output is missing columns.",
          "i" = "Keep the attribute as it is, or remove it entirely."
        ))
      }
      return(invisible(FALSE))
    }
  }
  invisible(TRUE)
}


#' Check the form of ACS data for census tracts
#'
#' Checks that American Community Survey (ACS) estimates for census tracts,
#' such as a table from `tidycensus::get_acs()` with `geometry = TRUE`, have
#' the form that [cacs_intersect_weight()] and the `acs` argument of
#' [cacs_run()] require. [cacs_run()] checks at the start only that `acs` is
#' an `sf` object, and makes the other checks at its intersection step,
#' after the routing step.
#'
#' The checks are made in this order, and the first one that fails stops the
#' function with an error:
#'
#' 1. `x` is an `sf` object.
#' 2. Its coordinate reference system is NAD83 (EPSG:4269).
#' 3. It has the columns `GEOID`, `NAME`, `variable`, `estimate`, `moe` (the
#'    margin of error), and `geometry`.
#' 4. It has at least one row.
#' 5. Every `GEOID` has 11 digits, like a census tract code.
#' 6. `estimate` and `moe` are numeric.
#' 7. No tract has two rows for the same variable.
#'
#' Whether `variable` holds ACS codes, and the values of `estimate` and
#' `moe`, are not checked. A name in place of an ACS code, such as `"pop"`
#' from `tidycensus::get_acs(variables = c(pop = "B01003_001"))`, gets `NA`
#' values, and [cacs_run()] then stops with an error about ACS codes that are
#' not combined. Census Bureau annotation codes, such as `-555555555`, pass
#' the checks. [cacs_intersect_weight()] treats them as missing, and its
#' warning counts them, except a margin-of-error code next to a missing
#' estimate; the example data below have only such codes and give no
#' warning. A tract with no row for one of the variables is not reported,
#' and it is left out of that variable's total or average without a warning.
#' Whether every tract has one row for each variable can be checked with
#' `all(table(acs$GEOID, acs$variable) == 1)`.
#'
#' @param x An `sf` object of ACS estimates with one row for each tract and
#'   variable, or any other object to check.
#' @return `TRUE`, invisibly, when every check passes. Otherwise the function
#'   stops with an error of class `catchmentACS_error_schema` at the first
#'   check that fails.
#' @seealso [cacs_acs_prefetch()] downloads ACS data in this form.
#' @family validation and conditions
#' @export
#' @examples
#' acs <- readRDS(system.file("extdata", "sample_alabama_subset.rds",
#'                            package = "catchmentACS"))
#' cacs_acs_validate(acs)
#'
#' # tidycensus downloads the data, which needs a Census API key.
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


#' Check that OSRM can be used
#'
#' Checks that the osrm package is installed. No request is sent here, so an
#' unreachable server is only found when the drive-time areas are computed;
#' cacs_validate_osrm_endpoint() tests a server on its own.
#'
#' @keywords internal
#' @noRd
.check_osrm_reachable <- function(server = NULL) {
  rlang::check_installed("osrm", reason = "for provider = 'osrm'")
  invisible(TRUE)
}


#' Check that openrouteservice can be used
#'
#' Checks that the openrouteservice package is installed and that a key given
#' as `api_key` is not an empty string. Whether the key works is only found
#' when the service answers.
#'
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


#' Stop for the Mapbox provider
#'
#' Mapbox is not implemented, so this function always gives an error, whatever
#' `token` is.
#'
#' @keywords internal
#' @noRd
.check_mapbox_token <- function(token = NULL) {
  .cli_abort_credential(c(
    "Mapbox provider is deferred to a future release.",
    "i" = "Use {.val osrm} or {.val ors} in the current release."
  ))
}


#' Stop for the r5r provider
#'
#' r5r is not implemented, so this function always gives an error, whatever
#' `r5r_core` is.
#'
#' @keywords internal
#' @noRd
.check_r5r_core <- function(r5r_core = NULL) {
  .cli_abort_credential(c(
    "r5r provider is deferred to a future release.",
    "i" = "Use {.val osrm} or {.val ors} in the current release."
  ))
}
