# ============================================================================
# describe.R - cacs_describe() provenance helper (v0.3 Step 4.3)
#
# Thin S3 surface over the attributes stabilized in Phases 2-4. The helper is
# intentionally read-only: it never mutates the object, re-runs computation, or
# attempts to reconstruct carriers consumed by cacs_derive_rates().
# ============================================================================


#' Describe catchmentACS provenance attached to an object
#'
#' `cacs_describe()` prints a compact "where did this data come from" map for
#' common catchmentACS objects and returns the same information as a structured
#' `cacs_description` list. It is useful after [cacs_run()],
#' [cacs_intersect_weight()], [cacs_propagate_moe()], [cacs_derive_rates()],
#' or [cacs_isochrone()] when you want to inspect schema version, run metadata,
#' carrier availability, margin-of-error (MOE) and rate provenance, ring
#' topology, and cache state.
#'
#' @param x A catchmentACS object, such as a `cacs_run_result`, canonical
#'   long `tbl_df`, or `sf` object.
#' @param ... Reserved for future formatting options.
#'
#' @return Invisibly returns a `cacs_description` list with `object_type`,
#'   `sections`, `data`, and `attributes_present` fields.
#'
#' @section Carrier provenance schema:
#' For weighted and propagated long tibbles, carrier provenance lives in
#' `attr(x, "cacs_aggregation_carriers")`: a 10-column tibble keyed by
#' `site_id`, `drive_time_min`, and `variable`, with `estimand_family`,
#' `est_total`, `var_total_raw`, `est_mean`, `var_mean_raw`, `weight_sum`,
#' and `n_tracts`. `cacs_derive_rates()` consumes and drops this carrier
#' attribute after using it, then records rate-level counts in
#' `attr(x, "cacs_rate_provenance")`.
#'
#' Full pipeline examples are best run through
#' `vignette("getting-started", package = "catchmentACS")`; this help page
#' keeps the example network-free by using bundled fixtures.
#'
#' @examples
#' old <- options(catchmentACS.progress = "off")
#' old_cache <- getOption("catchmentACS.cache_enabled", NULL)
#' on.exit(options(old), add = TRUE)
#' on.exit(options(catchmentACS.cache_enabled = old_cache), add = TRUE)
#' cacs_set_cache(FALSE, scope = "session")
#'
#' sites <- readRDS(system.file("extdata", "legacy_2025_sites.rds",
#'                             package = "catchmentACS"))
#' iso <- readRDS(system.file("extdata", "legacy_2025_isochrones.rds",
#'                            package = "catchmentACS"))
#' acs <- readRDS(system.file("extdata", "sample_alabama_subset.rds",
#'                            package = "catchmentACS"))
#'
#' site_id <- "AL_SITE_03"
#' result <- cacs_run(
#'   sites = sites[sites$site_id == site_id, , drop = FALSE],
#'   state = "AL",
#'   year = 2023,
#'   drive_times = 5,
#'   variables = unname(cacs_acs_default_vars),
#'   provider = "osrm",
#'   precomputed_isochrones = iso[iso$site_id == site_id &
#'                                  iso$drive_time_min == 5, , drop = FALSE],
#'   acs = acs,
#'   weight_method = "area",
#'   output = "long",
#'   verbose = FALSE
#' )
#'
#' desc <- suppressMessages(cacs_describe(result))
#' desc$object_type
#' names(desc$sections)
#'
#' @seealso [cacs_run()], [cacs_intersect_weight()], [cacs_propagate_moe()],
#'   [cacs_derive_rates()], [summary.cacs_run_result()],
#'   [print.cacs_run_result()], [print.cacs_run_summary()],
#'   [as_tibble.cacs_run_result()], [group_by.cacs_run_result()],
#'   [cacs_summary_as_markdown()], [cacs_get_cache_state()].
#' @family result summaries
#' @export
cacs_describe <- function(x, ...) {
  UseMethod("cacs_describe")
}


#' @export
cacs_describe.cacs_run_result <- function(x, ...) {
  meta <- attr(x, "cacs_run_result_metadata")
  run_prov <- attr(x, "cacs_run_provenance")
  rate_prov <- attr(x, "cacs_rate_provenance")

  sections <- list(
    "Object" = c(
      paste0("Type: cacs_run_result"),
      paste0("Rows: ", nrow(x)),
      paste0("Columns: ", ncol(x)),
      paste0("Schema version: ", .cacs_desc_value(attr(x, "cacs_schema_version")))
    ),
    "Run" = c(
      paste0("Provider/profile: ",
             .cacs_desc_value(.cacs_desc_first_non_null(meta$provider, run_prov$provider)),
             " / ",
             .cacs_desc_value(.cacs_desc_first_non_null(meta$profile, NA_character_))),
      paste0("Drive times: ",
             .cacs_desc_collapse(.cacs_desc_first_non_null(meta$drive_times,
                                                           run_prov$drive_times))),
      paste0("Sites: ",
             .cacs_desc_value(meta$n_sites_success), " success / ",
             .cacs_desc_value(meta$n_sites_failed), " failed / ",
             .cacs_desc_value(meta$n_sites_total), " total"),
      paste0("Wall clock seconds: ", .cacs_desc_value(meta$wall_clock_seconds))
    ),
    "Rates" = .cacs_desc_rate_lines(x, rate_prov),
    "Cache" = .cacs_desc_cache_lines()
  )

  desc <- .cacs_new_description("cacs_run_result", sections, list(
    metadata = meta,
    run_provenance = run_prov,
    rate_provenance = rate_prov,
    cache_state = .cacs_desc_cache_state()
  ), x = x)
  print(desc)
  invisible(desc)
}


#' @export
cacs_describe.tbl_df <- function(x, ...) {
  object_type <- .cacs_desc_tbl_type(x)
  carriers <- attr(x, "cacs_aggregation_carriers")
  agg_prov <- attr(x, "cacs_aggregation_provenance")
  moe_prov <- attr(x, "cacs_moe_provenance")
  rate_prov <- attr(x, "cacs_rate_provenance")

  sections <- list(
    "Object" = c(
      paste0("Type: ", object_type),
      paste0("Rows: ", nrow(x)),
      paste0("Columns: ", ncol(x)),
      paste0("Schema version: ", .cacs_desc_value(attr(x, "cacs_schema_version"))),
      paste0("Sites: ", .cacs_desc_n_distinct(x, "site_id")),
      paste0("Drive times: ", .cacs_desc_n_distinct(x, "drive_time_min")),
      paste0("Variables: ", .cacs_desc_n_distinct(x, "variable")),
      paste0("Ring topology: ", .cacs_desc_unique_col(x, "ring_topology"))
    ),
    "Carrier Table" = .cacs_desc_carrier_lines(carriers),
    "MOE Propagation" = .cacs_desc_moe_lines(moe_prov),
    "Rate Derivation" = .cacs_desc_rate_lines(x, rate_prov),
    "Aggregation" = .cacs_desc_agg_lines(agg_prov)
  )

  desc <- .cacs_new_description(object_type, sections, list(
    aggregation_provenance = agg_prov,
    moe_provenance = moe_prov,
    rate_provenance = rate_prov,
    carrier_rows = if (is.null(carriers)) 0L else nrow(carriers)
  ), x = x)
  print(desc)
  invisible(desc)
}


#' @export
cacs_describe.data.frame <- function(x, ...) {
  has_known_attr <- length(.cacs_desc_attributes_present(x)) > 0L
  has_canonical_cols <- any(c(
    "site_id", "drive_time_min", "variable", "estimand_family",
    "ring_topology", "failure_origin"
  ) %in% names(x))
  if (has_known_attr || has_canonical_cols) {
    return(cacs_describe.tbl_df(tibble::as_tibble(x), ...))
  }
  cacs_describe.default(x, ...)
}


#' @export
cacs_describe.list <- function(x, ...) {
  if (all(c("long", "list_column") %in% names(x)) &&
      inherits(x$long, "data.frame")) {
    return(cacs_describe(x$long, ...))
  }
  cacs_describe.default(x, ...)
}


#' @export
cacs_describe.sf <- function(x, ...) {
  iso_prov <- attr(x, "cacs_isochrone_provenance")
  acs_prov <- attr(x, "cacs_acs_provenance") %||% attr(x, "cacs_provenance")
  has_iso_cols <- all(c("site_id", "drive_time_min", "ring_topology") %in%
                        names(x))
  has_acs_cols <- all(c("GEOID", "variable", "estimate", "moe") %in%
                        names(x))
  object_type <- if (has_iso_cols) {
    "isochrone_sf"
  } else if (has_acs_cols) {
    "acs_sf"
  } else {
    "sf"
  }

  crs <- sf::st_crs(x)
  valid <- tryCatch(sf::st_is_valid(x), error = function(e) NA)
  sections <- list(
    "Object" = c(
      paste0("Type: ", object_type),
      paste0("Rows: ", nrow(x)),
      paste0("Columns: ", ncol(x)),
      paste0("EPSG: ", .cacs_desc_value(crs$epsg)),
      paste0("Geometry type: ", .cacs_desc_collapse(unique(as.character(sf::st_geometry_type(x))))),
      paste0("Valid geometries: ", sum(valid, na.rm = TRUE), " / ", length(valid))
    ),
    "Spatial Provenance" = c(
      paste0("Provider/profile: ",
             .cacs_desc_value(iso_prov$provider), " / ",
             .cacs_desc_value(iso_prov$profile)),
      paste0("res_param: ",
             .cacs_desc_value(attr(x, "cacs_res_param", exact = TRUE) %||%
                                iso_prov$res_param)),
      paste0("Ring topology: ",
             .cacs_desc_first_available_text(
               .cacs_desc_unique_col(x, "ring_topology"),
               .cacs_desc_value(iso_prov$ring_topology)
             )),
      paste0("Drive times: ", .cacs_desc_unique_col(x, "drive_time_min")),
      paste0("ACS variables: ", .cacs_desc_unique_col(x, "variable"))
    ),
    "ACS Provenance" = c(
      paste0("State/year: ",
             .cacs_desc_value(acs_prov$state), " / ",
             .cacs_desc_value(acs_prov$year)),
      paste0("Survey/geography: ",
             .cacs_desc_value(acs_prov$survey), " / ",
             .cacs_desc_value(acs_prov$geography)),
      paste0("Variable source: ", .cacs_desc_value(acs_prov$variable_source))
    )
  )

  desc <- .cacs_new_description(object_type, sections, list(
    isochrone_provenance = iso_prov,
    acs_provenance = acs_prov
  ), x = x)
  print(desc)
  invisible(desc)
}


#' @export
cacs_describe.default <- function(x, ...) {
  sections <- list(
    "Object" = c(
      paste0("Type: ", class(x)[[1L]]),
      "No catchmentACS provenance available for this object."
    )
  )
  desc <- .cacs_new_description("unknown", sections, list(), x = x)
  print(desc)
  invisible(desc)
}


.cacs_new_description <- function(object_type, sections, data, x = NULL) {
  structure(
    list(
      object_type = object_type,
      sections = sections,
      data = data,
      attributes_present = .cacs_desc_attributes_present(x)
    ),
    class = c("cacs_description", "list")
  )
}


#' @rdname cacs_describe
#' @export
print.cacs_description <- function(x, ...) {
  cli::cli_h1("catchmentACS provenance map")
  cli::cli_text("Object type: {x$object_type}")
  for (section in names(x$sections)) {
    cli::cli_h2(section)
    lines <- x$sections[[section]]
    if (length(lines) == 0L) {
      cli::cli_text("(none)")
    } else {
      for (line in lines) {
        cli::cli_text("{line}")
      }
    }
  }
  invisible(x)
}


.cacs_desc_value <- function(x) {
  if (is.null(x) || length(x) == 0L) return("n/a")
  x <- x[[1L]]
  if (is.na(x)) return("n/a")
  if (inherits(x, "POSIXt")) return(format(x, "%Y-%m-%d %H:%M:%S %Z"))
  if (is.numeric(x)) return(format(signif(x, 6), trim = TRUE, scientific = FALSE))
  as.character(x)
}


.cacs_desc_collapse <- function(x, max_items = 6L) {
  if (is.null(x) || length(x) == 0L) return("n/a")
  vals <- unique(as.character(stats::na.omit(x)))
  if (length(vals) == 0L) return("n/a")
  suffix <- if (length(vals) > max_items) "..." else ""
  paste(c(utils::head(vals, max_items), suffix), collapse = ", ")
}


.cacs_desc_first_non_null <- function(...) {
  vals <- list(...)
  for (val in vals) {
    if (!is.null(val) && length(val) > 0L) return(val)
  }
  NULL
}


.cacs_desc_first_available_text <- function(...) {
  vals <- list(...)
  for (val in vals) {
    if (!is.null(val) && length(val) > 0L &&
        !is.na(val[[1L]]) && !identical(val[[1L]], "n/a")) {
      return(as.character(val[[1L]]))
    }
  }
  "n/a"
}


.cacs_desc_attributes_present <- function(x) {
  if (is.null(x)) return(character(0))
  known <- c(
    "cacs_schema_version", "cacs_run_provenance",
    "cacs_run_result_metadata", "cacs_run_warnings",
    "cacs_aggregation_carriers", "cacs_aggregation_provenance",
    "cacs_moe_provenance", "cacs_rate_provenance",
    "cacs_rate_audit", "cacs_confidence_level",
    "cacs_isochrone_provenance", "cacs_acs_provenance",
    "cacs_provenance", "cacs_res_param", "skipped_geoids"
  )
  known[!vapply(known, function(nm) is.null(attr(x, nm, exact = TRUE)),
                logical(1))]
}


.cacs_desc_n_distinct <- function(x, col) {
  if (!col %in% names(x)) return("n/a")
  as.character(dplyr::n_distinct(x[[col]], na.rm = TRUE))
}


.cacs_desc_unique_col <- function(x, col) {
  if (!col %in% names(x)) return("n/a")
  .cacs_desc_collapse(x[[col]])
}


.cacs_desc_tbl_type <- function(x) {
  if (!is.null(attr(x, "cacs_rate_provenance"))) return("derived_rates")
  if (!is.null(attr(x, "cacs_moe_provenance"))) return("propagated_seam")
  if (!is.null(attr(x, "cacs_aggregation_carriers")) ||
      !is.null(attr(x, "cacs_aggregation_provenance"))) {
    return("weighted_seam")
  }
  "tbl_df"
}


.cacs_desc_carrier_lines <- function(carriers) {
  if (is.null(carriers)) {
    return(c("Carrier attribute: absent or already consumed."))
  }
  c(
    paste0("Carrier rows: ", nrow(carriers)),
    paste0("Carrier variables: ", .cacs_desc_n_distinct(carriers, "variable")),
    paste0("Carrier sites: ", .cacs_desc_n_distinct(carriers, "site_id")),
    paste0("Missing carrier estimates: ",
           sum(is.na(carriers$est_total) | is.na(carriers$var_total_raw),
               na.rm = TRUE)),
    paste0("Weight sum range: ",
           .cacs_desc_range(carriers$weight_sum))
  )
}


.cacs_desc_agg_lines <- function(agg_prov) {
  if (is.null(agg_prov)) return(c("Aggregation provenance: absent."))
  c(
    paste0("Weight method: ", .cacs_desc_value(agg_prov$weight_method)),
    paste0("Input sites: ", .cacs_desc_value(agg_prov$n_sites_input)),
    paste0("Sites with data: ", .cacs_desc_value(agg_prov$n_sites_with_data)),
    paste0("Unique variables: ", .cacs_desc_value(agg_prov$n_variables_unique)),
    paste0("Output groups missing ACS estimate: ",
           .cacs_desc_value(agg_prov$n_output_groups_missing_acs_estimate))
  )
}


.cacs_desc_moe_lines <- function(moe_prov) {
  if (is.null(moe_prov)) return(c("MOE provenance: absent."))
  c(
    paste0("Confidence level: ", .cacs_desc_value(moe_prov$level)),
    paste0("Family A rows: ", .cacs_desc_value(moe_prov$n_rows_family_a)),
    paste0("Family B rows: ", .cacs_desc_value(moe_prov$n_rows_family_b)),
    paste0("C1 to C2 fallbacks: ",
           .cacs_desc_value(moe_prov$n_rows_fallback_c1_to_c2)),
    paste0("Zero-denominator rows: ",
           .cacs_desc_value(moe_prov$n_rows_na_zero_den)),
    paste0("Missing-MOE rows: ",
           .cacs_desc_value(moe_prov$n_rows_na_missing_moe))
  )
}


.cacs_desc_rate_lines <- function(x, rate_prov) {
  rate_vars <- names(.SANCTIONED_RATES_V1)
  n_rate_rows <- if ("variable" %in% names(x)) {
    sum(x$variable %in% rate_vars, na.rm = TRUE)
  } else {
    0L
  }
  n_carrier <- if ("failure_origin" %in% names(x)) {
    sum(x$variable %in% rate_vars & x$failure_origin == "carrier",
        na.rm = TRUE)
  } else {
    NA_integer_
  }
  c(
    paste0("Rate rows: ", n_rate_rows),
    paste0("Rates computed: ",
           .cacs_desc_collapse(rate_prov$rates_computed %||% rate_vars)),
    paste0("Formula dispatch: ",
           .cacs_desc_value(rate_prov$formula_dispatch)),
    paste0("Carrier-missing rate rows: ",
           .cacs_desc_value(rate_prov$n_carrier_missing %||% n_carrier)),
    paste0("Out-of-range audit rows: ",
           .cacs_desc_value(rate_prov$n_rate_audit_out_of_bounds)),
    paste0("Formula downgrades: ",
           .cacs_desc_value(rate_prov$n_formula_downgraded))
  )
}


.cacs_desc_cache_state <- function() {
  tryCatch(cacs_get_cache_state(), error = function(e) NULL)
}


.cacs_desc_cache_lines <- function() {
  state <- .cacs_desc_cache_state()
  if (is.null(state) || nrow(state) == 0L) {
    return(c("Cache state: unavailable."))
  }
  hits <- tryCatch(sum(unlist(state$hits[[1L]]), na.rm = TRUE),
                   error = function(e) NA_integer_)
  misses <- tryCatch(sum(unlist(state$misses[[1L]]), na.rm = TRUE),
                     error = function(e) NA_integer_)
  c(
    paste0("Enabled: ", .cacs_desc_value(state$enabled)),
    paste0("Namespace mode: ", .cacs_desc_value(state$namespace_mode)),
    paste0("Fingerprint algorithm: ",
           .cacs_desc_value(state$fingerprint_algorithm)),
    paste0("Session hits: ", .cacs_desc_value(hits)),
    paste0("Session misses: ", .cacs_desc_value(misses))
  )
}


.cacs_desc_range <- function(x) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) return("n/a")
  rng <- range(x, na.rm = TRUE)
  paste0(.cacs_desc_value(rng[[1L]]), " to ", .cacs_desc_value(rng[[2L]]))
}
