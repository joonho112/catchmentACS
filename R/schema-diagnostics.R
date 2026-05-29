# ============================================================================
# schema-diagnostics.R - user-facing, non-aborting schema preflight helpers.
# ============================================================================


#' Preflight-check an isochrone object against the catchmentACS schema
#'
#' Checks whether an object is usable as a catchmentACS isochrone input. An
#' isochrone is a polygon enclosing everywhere reachable within a given travel
#' time; catchmentACS represents one as an `sf` object (a spatial data frame).
#' Unlike the internal validators used inside the pipeline, this helper never
#' aborts on ordinary schema failures. Instead it returns a tibble with one
#' row per issue and a copy-paste repair example for each, so you can fix an
#' object interactively before passing it downstream.
#'
#' @param iso_sf Object to validate, usually an `sf` object returned by
#'   [cacs_isochrone()].
#'
#' @return A tibble with columns `severity`, `check`, `col`, `actual`,
#'   `expected`, `fix_hint`, and `example`. Zero rows means the object passes
#'   every isochrone schema check.
#' @seealso [cacs_isochrone()] for producing a schema-valid isochrone,
#'   [cacs_run()] for the pipeline that consumes it, and [cacs_acs_validate()]
#'   for the ACS-side counterpart.
#' @family validation and conditions
#' @export
#'
#' @examples
#' iso <- readRDS(system.file("extdata", "legacy_2025_isochrones.rds",
#'                            package = "catchmentACS"))
#' cacs_validate_iso(iso)
cacs_validate_iso <- function(iso_sf) {
  .schema_issue_tbl(.collect_iso_schema_issues(iso_sf))
}


#' Collect v0.3 isochrone schema issues (internal)
#' @keywords internal
#' @noRd
.collect_iso_schema_issues <- function(iso_sf) {
  issues <- list()
  add_issue <- function(issue) {
    issues[[length(issues) + 1L]] <<- issue
    invisible(NULL)
  }

  if (!inherits(iso_sf, "sf")) {
    add_issue(.schema_issue(
      check = "iso_not_sf",
      actual = paste0("<", .schema_first_class(iso_sf), ">"),
      expected = "sf object with canonical cacs_isochrone() columns",
      fix_hint = "Pass cacs_isochrone() output, or convert a data frame with geometry back to sf.",
      example = "iso_sf <- sf::st_as_sf(iso_sf, sf_column_name = \"geometry\", crs = 4326)"
    ))
    return(issues)
  }

  missing_cols <- setdiff(.ISO_CANONICAL_COLS, names(iso_sf))
  if (length(missing_cols) > 0L) {
    for (missing_col in missing_cols) {
      add_issue(.schema_issue(
        check = "iso_missing_column",
        col = missing_col,
        actual = "missing",
        expected = "present in canonical 16-column schema",
        fix_hint = .iso_missing_col_hint(missing_col),
        example = .iso_missing_col_example(missing_col)
      ))
    }
  }

  annulus_by_topology <- "ring_topology" %in% names(iso_sf) &&
    any(as.character(iso_sf$ring_topology) == "annulus", na.rm = TRUE)
  annulus_by_isomin <- FALSE
  if ("isomin" %in% names(iso_sf)) {
    isomin_num <- suppressWarnings(as.numeric(iso_sf$isomin))
    annulus_by_isomin <- any(isomin_num > 0, na.rm = TRUE)
  }
  if (annulus_by_topology || annulus_by_isomin) {
    add_issue(.schema_issue(
      check = "iso_annulus_topology",
      col = if (annulus_by_topology) "ring_topology" else "isomin",
      actual = if (annulus_by_topology) "annulus" else "isomin > 0",
      expected = "cumulative rings ([0, isomax])",
      fix_hint = "Convert annulus rings to cumulative rings before intersection.",
      example = "iso_sf <- cacs_rings_to_cumulative(iso_sf)"
    ))
  } else if ("ring_topology" %in% names(iso_sf)) {
    ring_vals <- as.character(iso_sf$ring_topology)
    bad_ring <- is.na(ring_vals) | ring_vals != "cumulative"
    if (!is.character(iso_sf$ring_topology) || any(bad_ring)) {
      add_issue(.schema_issue(
        check = "iso_ring_topology",
        col = "ring_topology",
        actual = paste(.schema_values(ring_vals[bad_ring]), collapse = ", "),
        expected = "cumulative",
        fix_hint = "Use only cumulative v0.3 rings.",
        example = "iso_sf$ring_topology <- \"cumulative\""
      ))
    }
  }

  crs <- sf::st_crs(iso_sf)$epsg
  if (is.na(crs) || !identical(as.integer(crs), 4326L)) {
    add_issue(.schema_issue(
      check = "iso_crs",
      col = "geometry",
      actual = if (is.na(crs)) "NA" else paste0("EPSG:", as.integer(crs)),
      expected = "EPSG:4326",
      fix_hint = "Re-project the isochrone to WGS84 before passing it downstream.",
      example = "iso_sf <- sf::st_transform(iso_sf, 4326)"
    ))
  }

  gtypes <- as.character(sf::st_geometry_type(iso_sf))
  bad_gtype <- setdiff(unique(gtypes), c("POLYGON", "MULTIPOLYGON"))
  if (length(bad_gtype) > 0L) {
    add_issue(.schema_issue(
      check = "iso_geometry_type",
      col = "geometry",
      actual = paste(.schema_values(bad_gtype), collapse = ", "),
      expected = "POLYGON or MULTIPOLYGON",
      fix_hint = "Pass polygonal isochrone geometry, not points or lines.",
      example = "iso_sf <- cacs_isochrone(sites_df, drive_times = c(10L, 20L))"
    ))
  }

  if ("provider" %in% names(iso_sf)) {
    bad_provider <- .schema_bad_values(iso_sf$provider, .SANCTIONED_PROVIDERS)
    if (length(bad_provider) > 0L) {
      add_issue(.schema_issue(
        check = "iso_provider",
        col = "provider",
        actual = paste(.schema_values(bad_provider), collapse = ", "),
        expected = paste(.SANCTIONED_PROVIDERS, collapse = ", "),
        fix_hint = "Use a sanctioned provider value.",
        example = "iso_sf$provider <- \"osrm\""
      ))
    }
  }

  if ("provider_requested" %in% names(iso_sf)) {
    bad_provider_requested <- .schema_bad_values(
      iso_sf$provider_requested,
      .SANCTIONED_PROVIDERS
    )
    if (length(bad_provider_requested) > 0L) {
      add_issue(.schema_issue(
        check = "iso_provider_requested",
        col = "provider_requested",
        actual = paste(.schema_values(bad_provider_requested), collapse = ", "),
        expected = paste(.SANCTIONED_PROVIDERS, collapse = ", "),
        fix_hint = "Use a sanctioned requested-provider value.",
        example = "iso_sf$provider_requested <- \"osrm\""
      ))
    }
  }

  if ("drive_time_min" %in% names(iso_sf)) {
    bad_drive <- rep(FALSE, length(iso_sf$drive_time_min))
    if (is.numeric(iso_sf$drive_time_min)) {
      bad_drive <- !is.na(iso_sf$drive_time_min) & iso_sf$drive_time_min <= 0
    }
    if (any(bad_drive, na.rm = TRUE)) {
      add_issue(.schema_issue(
        check = "iso_drive_time_positive",
        col = "drive_time_min",
        actual = paste(.schema_values(iso_sf$drive_time_min[bad_drive]), collapse = ", "),
        expected = "positive integer minutes",
        fix_hint = "Drop zero or negative drive-time rows before intersection.",
        example = "iso_sf <- iso_sf[is.na(iso_sf$drive_time_min) | iso_sf$drive_time_min > 0, ]"
      ))
    }
    if (!is.integer(iso_sf$drive_time_min)) {
      add_issue(.schema_issue(
        check = "iso_drive_time_type",
        col = "drive_time_min",
        actual = paste0("<", .schema_first_class(iso_sf$drive_time_min), ">"),
        expected = "integer",
        fix_hint = "Store drive-time minutes as whole-number integers.",
        example = "iso_sf$drive_time_min <- as.integer(iso_sf$drive_time_min)"
      ))
    }
  }

  if ("site_id" %in% names(iso_sf)) {
    if (!is.character(iso_sf$site_id)) {
      add_issue(.schema_issue(
        check = "iso_site_id_type",
        col = "site_id",
        actual = paste0("<", .schema_first_class(iso_sf$site_id), ">"),
        expected = "character",
        fix_hint = "Convert site identifiers to character.",
        example = "iso_sf$site_id <- as.character(iso_sf$site_id)"
      ))
    } else if (any(is.na(iso_sf$site_id) | !nzchar(iso_sf$site_id))) {
      add_issue(.schema_issue(
        check = "iso_site_id_empty",
        col = "site_id",
        actual = "NA or empty string",
        expected = "non-empty character",
        fix_hint = "Remove or repair rows with missing site identifiers.",
        example = "iso_sf <- iso_sf[!is.na(iso_sf$site_id) & nzchar(iso_sf$site_id), ]"
      ))
    }
  }

  for (logical_col in c("isochrone_empty", "provider_downgrade")) {
    if (logical_col %in% names(iso_sf) && !is.logical(iso_sf[[logical_col]])) {
      add_issue(.schema_issue(
        check = paste0("iso_", logical_col, "_type"),
        col = logical_col,
        actual = paste0("<", .schema_first_class(iso_sf[[logical_col]]), ">"),
        expected = "logical TRUE/FALSE",
        fix_hint = "Use unquoted TRUE/FALSE values.",
        example = paste0("iso_sf$", logical_col, " <- FALSE")
      ))
    }
  }

  if ("retry_count" %in% names(iso_sf) && !is.integer(iso_sf$retry_count)) {
    add_issue(.schema_issue(
      check = "iso_retry_count_type",
      col = "retry_count",
      actual = paste0("<", .schema_first_class(iso_sf$retry_count), ">"),
      expected = "integer",
      fix_hint = "Store retry counts as integers.",
      example = "iso_sf$retry_count <- as.integer(iso_sf$retry_count)"
    ))
  }

  issues
}


#' First visible class/type for diagnostics (internal)
#' @keywords internal
#' @noRd
.schema_first_class <- function(x) {
  cls <- class(x)
  if (length(cls) > 0L) cls[[1L]] else typeof(x)
}

#' Format a small set of values for diagnostics (internal)
#' @keywords internal
#' @noRd
.schema_values <- function(x, max_values = 5L) {
  values <- unique(as.character(x))
  values <- values[!is.na(values)]
  if (any(is.na(x))) values <- c(values, "NA")
  if (length(values) == 0L) values <- "none"
  utils::head(values, max_values)
}

#' Return values not in an allowed set, preserving NA as bad (internal)
#' @keywords internal
#' @noRd
.schema_bad_values <- function(x, allowed) {
  values <- unique(as.character(x))
  values[is.na(values) | !values %in% allowed]
}

#' Missing-column repair hint (internal)
#' @keywords internal
#' @noRd
.iso_missing_col_hint <- function(col) {
  switch(col,
    "ring_topology" = "Add cumulative-ring provenance or rebuild with cacs_isochrone().",
    "provider_downgrade" = "Add the logical downgrade flag expected by v0.3.",
    "isochrone_empty" = "Add the logical empty-geometry flag expected by v0.3.",
    "retry_count" = "Add integer retry provenance.",
    "geometry" = "Restore sf geometry before using the object as an isochrone.",
    "Use cacs_isochrone() output or add the missing canonical column."
  )
}

#' Missing-column copy-paste example (internal)
#' @keywords internal
#' @noRd
.iso_missing_col_example <- function(col) {
  switch(col,
    "ring_topology" = "iso_sf$ring_topology <- \"cumulative\"",
    "provider_downgrade" = "iso_sf$provider_downgrade <- FALSE",
    "isochrone_empty" = "iso_sf$isochrone_empty <- FALSE",
    "retry_count" = "iso_sf$retry_count <- 0L",
    "geometry" = "iso_sf <- sf::st_as_sf(iso_sf, sf_column_name = \"geometry\", crs = 4326)",
    "iso_sf <- cacs_isochrone(sites_df, drive_times = c(10L, 20L))"
  )
}
