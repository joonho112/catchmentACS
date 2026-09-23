# schema-diagnostics.R - cacs_validate_iso(), which lists what is wrong with a
# set of drive-time areas instead of stopping at the first problem.


#' Check drive-time areas and list the problems found
#'
#' Checks whether an object can be used as drive-time areas (isochrones) by
#' [cacs_intersect_weight()], or by [cacs_run()] through its
#' `precomputed_isochrones` argument, and returns a table with one row for
#' each problem found. Those two functions stop with an error at the first of
#' these problems. [cacs_run()] checks at the start only that
#' `precomputed_isochrones` is an `sf` object, and makes the other checks
#' after the download step.
#'
#' The object must be an `sf` object; if it is not, that is the only problem
#' reported. It must have the 16 columns of the result of [cacs_isochrone()],
#' with `ring_topology` equal to `"cumulative"` in every row, meaning that
#' each area contains the areas of the shorter drive times. An `isomin`
#' column, if present, must be 0 in every row (see
#' [cacs_rings_to_cumulative()]). The coordinate reference system must be
#' EPSG:4326, and the geometry type `POLYGON` or `MULTIPOLYGON`. `provider` and
#' `provider_requested` must be `"osrm"`, `"ors"`, `"mapbox"`, or `"r5r"`,
#' and `site_id` must hold strings that are neither missing nor empty.
#' `drive_time_min` must be above 0, `drive_time_min` and `retry_count` must
#' be stored as integers, and `isochrone_empty` and `provider_downgrade` must
#' be logical.
#'
#' The areas themselves are not compared, so a 5-minute area larger than the
#' 10-minute area of the same site passes. So does a site and drive time
#' that appears in two rows, which [cacs_intersect_weight()] turns into a row
#' of `NA` values. The function gives an error only for an `sf` object
#' without a usable geometry column, such as one made by selecting rows with
#' `[` before the sf package is loaded.
#'
#' Areas made with another tool, such as the result of
#' [cacs_rings_to_cumulative()], can be used once the missing columns hold
#' the values that [cacs_isochrone()] gives an area built without a problem
#' (see its Value section). `NA` in `isochrone_empty` and `""` in
#' `failure_reason` pass the checks here, but [cacs_run()] treats an area as
#' a routing failure unless its `isochrone_empty` is `FALSE` and its
#' `failure_reason` is `NA`.
#'
#' @param iso_sf An `sf` object of drive-time areas with one row for each
#'   site and drive time, or any other object to check.
#'
#' @return A tibble with one row for each problem, and no rows when every
#'   check passes. Its seven columns are strings:
#'   \describe{
#'     \item{`severity`}{Always `"error"`.}
#'     \item{`check`}{A short name for the check, such as `"iso_crs"`.}
#'     \item{`col`}{The column concerned, or `NA`.}
#'     \item{`actual`, `expected`}{What was found, and what is required.}
#'     \item{`fix_hint`, `example`}{A suggested fix, and a line of R code for
#'       it, written for an object named `iso_sf`.}
#'   }
#' @family validation and conditions
#' @export
#'
#' @examples
#' # The drive-time areas bundled with the package: no problems, no rows
#' iso <- readRDS(system.file("extdata", "legacy_2025_isochrones.rds",
#'                            package = "catchmentACS"))
#' cacs_validate_iso(iso)
#'
#' # Drive times stored as decimal numbers instead of integers
#' iso$drive_time_min <- as.numeric(iso$drive_time_min)
#' cacs_validate_iso(iso)
cacs_validate_iso <- function(iso_sf) {
  .schema_issue_tbl(.collect_iso_schema_issues(iso_sf))
}


#' List the problems of one set of drive-time areas
#'
#' Returns a list of `.schema_issue()` entries, one for each problem found.
#' An object that is not an `sf` object returns after the first entry, because
#' the other checks need the geometry and the columns.
#'
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
      expected = "sf object with the columns of cacs_isochrone() output",
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
        expected = "present in cacs_isochrone() output",
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
        fix_hint = "If each area starts at the site, set ring_topology to \"cumulative\"; areas between two drive times need cacs_rings_to_cumulative().",
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
      fix_hint = "Transform the drive-time areas to longitude and latitude (WGS 84) first.",
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
        fix_hint = "Record the routing service that made the areas; it must be one of the expected values.",
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
        fix_hint = "Use one of the expected values.",
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


#' The first class of an object, or its type when it has none
#' @keywords internal
#' @noRd
.schema_first_class <- function(x) {
  cls <- class(x)
  if (length(cls) > 0L) cls[[1L]] else typeof(x)
}

#' The first few different values, for the `actual` column of the report
#'
#' Missing values are shown as `"NA"`, and a column with nothing to show gives
#' `"none"`, so the report never has an empty cell.
#'
#' @keywords internal
#' @noRd
.schema_values <- function(x, max_values = 5L) {
  values <- unique(as.character(x))
  values <- values[!is.na(values)]
  if (any(is.na(x))) values <- c(values, "NA")
  if (length(values) == 0L) values <- "none"
  utils::head(values, max_values)
}

#' The values that are not among those allowed, counting `NA` as one of them
#' @keywords internal
#' @noRd
.schema_bad_values <- function(x, allowed) {
  values <- unique(as.character(x))
  values[is.na(values) | !values %in% allowed]
}

#' What to do about a missing column, for the `fix_hint` column
#' @keywords internal
#' @noRd
.iso_missing_col_hint <- function(col) {
  switch(col,
    "ring_topology" = "Add the column ring_topology with the value \"cumulative\" (each area starts at the site), or rebuild with cacs_isochrone().",
    "provider_downgrade" = "Add the logical column provider_downgrade (whether another service was used instead).",
    "isochrone_empty" = "Add the logical column isochrone_empty (TRUE when the row has no area).",
    "retry_count" = "Add the integer column retry_count (the number of routing attempts).",
    "geometry" = "Restore sf geometry before using the object as an isochrone.",
    "Use cacs_isochrone() output or add the missing column."
  )
}

#' A line of code that adds a missing column, for the `example` column
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
