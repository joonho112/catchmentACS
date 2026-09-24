# intersect-weight.R - cacs_intersect_weight(), which combines ACS tract
# estimates into estimates for drive-time areas by area weighting, and its
# internal helpers.
#
# Only weight_method = "area" is implemented; "population" gives an error
# before any other work is done.


#' Aggregate ACS tract estimates to drive-time areas by area weighting
#'
#' Combines the American Community Survey (ACS) estimates of the census
#' tracts that overlap each drive-time area (isochrone) into an estimate and
#' a margin of error for every site, drive time, and variable. Counts, such
#' as total population (`B01003_001`), are added up with each tract weighted
#' by the share of its area inside the drive-time area (its coverage weight),
#' which assumes that whatever a variable counts is spread evenly over each
#' tract's area. The assumption is made for each variable separately, and it
#' is a stronger one for a subgroup, such as the people below the poverty
#' level, than for the population as a whole. Medians and per-person values,
#' such as median household income (`B19013_001`) and per capita income
#' (`B19301_001`), are averaged over the overlapping tracts with weights
#' proportional to the area each tract shares with the drive-time area. This
#' average stands in for the median or per-person value of the drive-time
#' area, but its weights follow area and ignore how many people live in each
#' tract. It can therefore be far from that value when the overlapping tracts
#' differ in population density.
#'
#' Like the ACS margins they are combined from, the margins of error are
#' half-widths of 90 percent confidence intervals, and they treat the
#' weights as fixed and the tract estimates as independent (see
#' [cacs_propagate_moe()]). Rates, such as the poverty rate, are computed
#' later by [cacs_derive_rates()], each as the ratio of two of these weighted
#' counts.
#'
#' @param iso_sf An `sf` object of drive-time areas as returned by
#'   [cacs_isochrone()], with one row for each site and drive time. Data from
#'   other sources must have the same columns, which [cacs_validate_iso()]
#'   checks, and each area must contain the shorter ones
#'   (`ring_topology = "cumulative"`; see [cacs_rings_to_cumulative()]).
#' @param acs_sf An `sf` object of ACS estimates for census tracts as
#'   returned by [cacs_acs_prefetch()], with one row for each tract and
#'   variable. Data from other sources must have the same columns, which
#'   [cacs_acs_validate()] checks.
#' @param bg_pop_sf An `sf` object of block-group population estimates, or
#'   `NULL` (the default). It has no effect on the result (see
#'   `weight_method`).
#' @param weight_method A string giving the weighting method: `"area"` (the
#'   default) or `"population"`, which is not implemented yet; using it gives
#'   an error before any other work is done.
#' @param min_weight A single number in [0, 1); the default is `1e-6`.
#'   Tracts whose coverage weight is at or below it are dropped (see step 4
#'   of Details).
#' @param verbose A logical value. If `TRUE` (the default), progress messages
#'   are shown: a summary line when the step finishes and, with five or more
#'   site and drive-time pairs, also a line as each pair finishes. See the
#'   "Progress messages" section of [cacs_run()] for how to turn them off or
#'   show the lines for any number of pairs.
#' @param keep_tract_audit A logical value, `FALSE` (the default) or `TRUE`.
#'   If `TRUE`, the result has a `cacs_tract_audit` attribute: a table with
#'   one row for each tract used in each drive-time area. Its columns are
#'   `site_id`, `drive_time_min`, `GEOID`, the tract's coverage weight
#'   (`area_wt`), and the areas of the overlap (`int_area_m2`) and of the
#'   tract (`tract_area_m2`) in square meters.
#' @param cache_dir A path to the cache folder, or `NULL` (the default) to use
#'   [cacs_cache_dir()]. The result is saved in, and looked for in, its
#'   `intersect` subfolder. A folder outside the temporary folder of the R
#'   session is tidied as described in [cacs_cache_dir()].
#'
#' @return A tibble with one row for each site, drive time, and ACS variable,
#'   sorted by `site_id`, `drive_time_min`, and `variable`, followed by one
#'   row for each site and drive-time pair with no tract left (see Details).
#'   It has the columns that [cacs_run()] returns in its long form,
#'   described in its Value section, except `est_total`, `var_total_raw`,
#'   `est_mean`, and `var_mean_raw`, which [cacs_propagate_moe()] adds. The
#'   main columns are:
#'   \describe{
#'     \item{`estimate`, `moe`}{The estimate and its margin of error, at the
#'       90 percent level (see Details).}
#'     \item{`weight_sum`, `n_tracts`}{The sum of the coverage weights of the
#'       tracts combined, also on the rows for medians and per-person values,
#'       and the number of those tracts, including any with a missing
#'       estimate.}
#'     \item{`estimand_family`, `weight_basis`}{The kind of quantity and the
#'       weights used: `"spatial_total"` (a count) with `"coverage"`;
#'       `"median_proxy"` (a median) or `"area_weighted_scalar_proxy"` (a
#'       per-person value) with `"area_mean"`; or `"metadata_only"` (a code
#'       that is not combined) with `"none"`, and [cacs_propagate_moe()]
#'       gives an error for such rows.}
#'     \item{`failure_origin`}{`"none"`, or `"intersection"` on the row of a
#'       pair with no tract left.}
#'   }
#'   The result has these attributes:
#'   \describe{
#'     \item{`cacs_aggregation_carriers`}{A table of the weighted sums and
#'       averages of the tract estimates, with their variances, for each
#'       site, drive time, and variable. [cacs_propagate_moe()] and
#'       [cacs_derive_rates()] read it, and [cacs_derive_rates()] removes it.}
#'     \item{`cacs_aggregation_provenance`}{A list recording `weight_method`,
#'       `min_weight`, `acs_year`, the time the result was computed, the
#'       versions of catchmentACS, sf, GEOS, and PROJ, and counts of
#'       variables and of missing estimates. `n_sites_input` is the number of
#'       site and drive-time pairs, and `n_sites_with_data` and
#'       `n_sites_empty` are the numbers of rows of the result with and
#'       without tracts.}
#'     \item{`skipped_geoids`}{The `GEOID` of each row of `acs_sf` skipped in
#'       step 3 of Details, so a skipped tract appears once for each of its
#'       variables; an empty character vector when nothing is skipped.}
#'   }
#'   It also has `cacs_schema_version`, the version label (`"1.0"`) of the
#'   column layout, and, with `keep_tract_audit = TRUE`, `cacs_tract_audit`
#'   (see that argument).
#'
#' @details
#' The function works in these steps.
#'
#' 1. Checks the inputs. `iso_sf` must be in EPSG:4326 (longitude and
#'    latitude on WGS 84) and `acs_sf` in EPSG:4269 (NAD83), as returned by
#'    [cacs_isochrone()] and [cacs_acs_prefetch()], and two rows of `acs_sf`
#'    for the same tract and variable give an error. Both must lie within a
#'    box around the contiguous United States and the District of Columbia,
#'    so data for Alaska, Hawaii, or Puerto Rico give an error. The codes
#'    that the Census Bureau's data API puts in place of some estimates and
#'    margins of error (-222222222, -333333333, -555555555, -666666666,
#'    -888888888, and -999999999) are set to `NA`, with a warning that counts
#'    them; a margin-of-error code next to a missing estimate is not counted.
#'    Other negative values are used as they are. A warning is also given
#'    when the bounding box of the drive-time areas extends beyond that of
#'    the tracts (see below).
#' 2. Transforms both inputs to EPSG:5070 (NAD83 / Conus Albers), an
#'    equal-area projection, and measures all areas there, in square meters.
#'    Invalid geometries are repaired, with a warning.
#' 3. Skips, with a warning, the tracts whose area is zero or not finite,
#'    such as a water tract with an empty boundary, and stops with an error
#'    if every tract is like that. The skipped tracts are listed in the
#'    `skipped_geoids` attribute.
#' 4. For each site and drive time, finds the tracts that overlap the area.
#'    A tract's coverage weight is the area of its overlap divided by the
#'    tract's area. Tracts with a coverage weight at or below `min_weight`,
#'    including tracts that only touch the edge of the area, are dropped.
#'    Each area is used whole, so the 10-minute results include the tracts
#'    of the 5-minute area.
#' 5. Combines the remaining tracts for each variable. A count is the sum of
#'    the tract estimates multiplied by their coverage weights, and a median
#'    or per-person value is the average of the tract estimates weighted by
#'    the area of each overlap. The margin of error is
#'    \eqn{\sqrt{\sum_i (w_i M_i)^2}}{sqrt(sum((w_i * M_i)^2))}, where
#'    \eqn{M_i} is the margin of error of tract \eqn{i} and \eqn{w_i} its
#'    weight. For a count the weight is the coverage weight, and for a
#'    median or per-person value it is the area share (see the "Coverage
#'    weights and area shares" section). A missing estimate in any of the
#'    tracts, including a code set to `NA` in step 1, makes the variable's
#'    estimate and margin of error `NA`; a missing margin of error makes only
#'    the margin of error `NA`. A rate that uses the variable is `NA` in both
#'    cases (see [cacs_derive_rates()]). [cacs_acs_prefetch()] sets negative
#'    margins of error to `NA`.
#'
#' Whether a variable is a count, a median, or a per-person value is decided
#' by its ACS code. Tables `B19013` and `B25077` are medians, and table
#' `B19301` is a per-person value. Every other code of the form `B`, five
#' digits, an underscore, and three digits (such as `B17001_002`) is treated
#' as a count, so a median or per-person value from another table is added
#' up like a count. Other codes, such as those of tables whose names begin
#' with `C` or `S` or end with a letter (such as `B17001A`), are not combined
#' and get `NA` values.
#'
#' Only the tracts in `acs_sf` are used: any part of a drive-time area outside
#' them (for example, across a state line) adds nothing, so counts come out
#' too small, and rates and medians come from the remaining tracts. The only
#' warning about this, in step 1, compares the bounding box of all the areas
#' in `iso_sf` with the bounding box of all the tracts in `acs_sf`: it is
#' given when the first extends more than 0.05 degrees (about 5 km) beyond
#' the second on some side. The tracts of a neighboring state can be
#' included in `acs_sf`, for example by combining two [cacs_acs_prefetch()]
#' results with [dplyr::bind_rows()]; the combined data no longer record the
#' ACS year, so `acs_year` in the result is `NA`.
#'
#' A site and drive time can have no tract left after step 4: no tract
#' overlaps the area, the area is empty or appears twice in `iso_sf`, or
#' every tract is at or below `min_weight`. Such a pair gives one row with
#' `variable = NA`, `NA` values, `n_tracts = 0`, and
#' `failure_origin = "intersection"`. No warning is given. The same row is
#' given when the overlap calculation for a pair fails, so one failing pair
#' does not stop the others.
#'
#' Results are cached (see [cacs_set_cache()]) in the folder given by
#' `cache_dir` or [cacs_cache_dir()]: a call that matches an earlier one
#' returns the saved result without repeating steps 2 to 5 or their
#' warnings. The drive-time areas are matched by the `cache_key` that
#' [cacs_isochrone()] attaches to its result together with their contents:
#' the geometries, the coordinate reference system, and the columns
#' `site_id`, `drive_time_min`, `provider`, `profile`, `osm_snapshot_date`,
#' and `ring_topology`. A subset or an edited copy of a [cacs_isochrone()]
#' result is therefore computed again, while the same areas in another row
#' order use the saved result. `options(catchmentACS.cache_intersect = FALSE)`
#' turns this cache off.
#'
#' @section Coverage weights and area shares:
#' For one site, drive time, and variable, let \eqn{a_i} be the area of the
#' overlap between tract \eqn{i} and the drive-time area and \eqn{A_i} the
#' area of the tract. The tracts are those kept in step 4 of Details that
#' have a row for the variable in `acs_sf`; a tract without such a row is
#' left out, without a warning. A count then does not include that tract,
#' and `n_tracts` on the rows of that variable is smaller than on the rows of
#' the variables that have a row for the tract ([cacs_acs_validate()]
#' describes a check). Counts are combined with the coverage
#' weights (`weight_basis = "coverage"`)
#' \deqn{c_i = \frac{a_i}{A_i},}{c_i = a_i / A_i,}
#' and medians and per-person values with the area shares
#' (`weight_basis = "area_mean"`)
#' \deqn{s_i = \frac{a_i}{\sum_k a_k},}{s_i = a_i / sum(a_k),}
#' which sum to one.
#'
#' @section Totals kept for later steps:
#' The `cacs_aggregation_carriers` attribute has the columns `site_id`,
#' `drive_time_min`, `variable`, `estimand_family`, `weight_sum`, and
#' `n_tracts`, with the same values as in the result. It has four more:
#' `est_total`, the sum \eqn{\sum_i c_i X_i}{sum(c_i * X_i)} of the tract
#' estimates \eqn{X_i}; `est_mean`, the average
#' \eqn{\sum_i s_i X_i}{sum(s_i * X_i)}; and `var_total_raw` and
#' `var_mean_raw`, their variances (the squares of their standard errors).
#' The sum and the average are computed for every variable; `estimate` and
#' `moe` come from the sum for counts and from the average for medians and
#' per-person values.
#'
#' @seealso [cacs_describe()] prints a summary drawn from the attributes of
#'   the result. Area weighting is explained at more length in
#'   `vignette("theory-spatial-aggregation", package = "catchmentACS")`,
#'   online at
#'   <https://joonho112.github.io/catchmentACS/articles/theory-spatial-aggregation.html>.
#' @family steps of the calculation
#' @export
#' @examples
#' # Turn the cache off while this example runs (see ?cacs_set_cache).
#' old <- options(catchmentACS.cache_enabled = FALSE)
#'
#' # Example data bundled with the package: the drive-time areas are circles
#' # with a radius of 1 km per minute, and the ACS data are made up.
#' library(sf)
#' iso <- readRDS(system.file("extdata", "legacy_2025_isochrones.rds",
#'                            package = "catchmentACS"))
#' acs <- readRDS(system.file("extdata", "sample_alabama_subset.rds",
#'                            package = "catchmentACS"))
#' iso_07 <- iso[iso$site_id == "AL_SITE_07" & iso$drive_time_min == 10, ]
#' agg <- cacs_intersect_weight(iso_sf = iso_07, acs_sf = acs,
#'                              keep_tract_audit = TRUE, verbose = FALSE)
#'
#' # The coverage weight and the area share of each tract in the area (the
#' # three tracts have the same area, so here the area shares are the
#' # coverage weights divided by their sum)
#' tracts <- attr(agg, "cacs_tract_audit")
#' tracts$area_share <- tracts$int_area_m2 / sum(tracts$int_area_m2)
#' tracts[, c("GEOID", "area_wt", "area_share")]
#'
#' # A count, a median, and a per-person value, with the weights each one uses
#' agg[agg$variable %in% c("B01003_001", "B19013_001", "B19301_001"),
#'     c("variable", "estimate", "moe", "weight_basis", "n_tracts")]
#'
#' options(old)
cacs_intersect_weight <- function(iso_sf,
                                   acs_sf,
                                   bg_pop_sf     = NULL,
                                   weight_method = c("area", "population"),
                                   min_weight    = 1e-6,
                                   verbose       = TRUE,
                                   keep_tract_audit = FALSE,
                                   cache_dir     = NULL) {

  weight_method <- match.arg(weight_method)

  # Population weighting is not implemented. Stop here, before the inputs are
  # checked, transformed, or intersected, so no work is wasted. The error has
  # class `catchmentACS_error_credential`, like the errors for the Mapbox and
  # r5r providers, which are not implemented either.
  if (identical(weight_method, "population")) {
    .cli_abort_credential(c(
      "{.field weight_method} = {.val population} is deferred to a future release.",
      "i" = "Use {.val area} in the current release."
    ))
  }

  # Check the inputs (R/checks.R): the columns and CRS of iso_sf (EPSG:4326)
  # and acs_sf (EPSG:4269), then the other arguments.
  .validate_iso_schema(iso_sf)
  .validate_acs_schema(acs_sf)
  acs_sf <- .cacs_acs_codes_to_na(acs_sf)

  # Checks of bg_pop_sf for population weighting. Not reached: the error for
  # weight_method = "population" above stops the function first.
  if (identical(weight_method, "population")) {                         # nocov start
    if (is.null(bg_pop_sf)) {
      .cli_abort_schema(c(
        "{.arg weight_method = 'population'} requires {.arg bg_pop_sf}.",
        "*" = "Either set {.arg weight_method = 'area'} or pass a block-group population surface.",
        "i" = "Expected columns: {.field GEOID}, {.field tract_geoid}, {.field population_est}, {.field geometry}."
      ))
    }
    .validate_bg_pop_schema(bg_pop_sf)
  }                                                                     # nocov end

  .validate_min_weight(min_weight)
  if (!is.logical(keep_tract_audit) || length(keep_tract_audit) != 1L ||
      is.na(keep_tract_audit)) {
    .cli_abort_schema(c(
      "{.arg keep_tract_audit} must be a non-NA logical scalar.",
      "x" = "Got {.cls {class(keep_tract_audit)[[1L]]}} of length {.val {length(keep_tract_audit)}}."
    ))
  }
  .validate_verbose(verbose)
  if (!is.null(cache_dir) &&
      (!is.character(cache_dir) || length(cache_dir) != 1L || is.na(cache_dir))) {
    .cli_abort_schema(c(
      "{.arg cache_dir} must be {.cls character(1)} or {.code NULL}.",
      "x" = "Got {.cls {class(cache_dir)[[1L]]}} of length {.val {length(cache_dir)}}."
    ))
  }

  # Areas are measured in EPSG:5070 (NAD83 / Conus Albers), which EPSG defines
  # for the contiguous 48 states, so both inputs must lie inside a box around
  # the contiguous United States and DC, widened by 0.25 degrees
  # (.bbox_outside_conus_dc()).
  iso_bbox <- sf::st_bbox(iso_sf)
  acs_bbox <- sf::st_bbox(acs_sf)
  bad_input <- character(0)
  if (.bbox_outside_conus_dc(iso_bbox)) bad_input <- c(bad_input, "iso_sf")
  if (.bbox_outside_conus_dc(acs_bbox)) bad_input <- c(bad_input, "acs_sf")
  if (length(bad_input) > 0L) {
    .cli_abort_operator(c(
      "The inputs must lie in the contiguous United States or the District of Columbia.",
      "x" = "Outside that area: {.arg {bad_input}}.",
      "i" = "Areas are measured in the Albers projection for the contiguous states (EPSG:5070), which does not cover Alaska, Hawaii, or Puerto Rico."
    ))
  }

  # Parts of the drive-time areas outside the tracts of acs_sf (for example,
  # across a state line) add nothing, so warn when iso_sf extends more than
  # 0.05 degrees beyond acs_sf. Only the bounding boxes are compared.
  if (.bbox_extends_beyond(iso_bbox, acs_bbox)) {
    .cli_warn_provenance(c(
      "The bounding box of {.arg iso_sf} extends beyond the bounding box of {.arg acs_sf}.",
      "i" = "Parts of the drive-time areas outside the tracts in {.arg acs_sf}, for example across a state line, add nothing to the results."
    ), phase = "intersect")
  }

  # Look up a saved result before the transformation and the loop over pairs,
  # which take most of the time. The key (.cache_key_intersect()) covers the
  # cache_key attribute of iso_sf together with a checksum of its areas (so a
  # subset or an edited copy of a cacs_isochrone() result, which keeps the
  # attribute, gets its own key), the cache_key attribute and a checksum of
  # acs_sf, bg_pop_sf, weight_method, min_weight, the ring topology and OSRM
  # res of iso_sf, and the versions of catchmentACS, sf, GEOS, PROJ, and R.
  intersect_cache_key <- NULL
  if (isTRUE(.cacs_cache_intersect_enabled())) {
    intersect_cache_key <- .cache_key_intersect(
      iso_sf        = iso_sf,
      acs_sf        = acs_sf,
      bg_pop_sf     = bg_pop_sf,
      weight_method = weight_method,
      min_weight    = min_weight
    )
    cached <- suppressMessages(
      .cacs_cache_get(intersect_cache_key, "intersect", cache_dir = cache_dir)
    )
    if (!is.null(cached)) {
      # keep_tract_audit is not part of the key: a saved result without the
      # tract table is computed again when the table is wanted, and the table
      # is removed from a saved result when it is not.
      if (isTRUE(keep_tract_audit) &&
          is.null(attr(cached, "cacs_tract_audit", exact = TRUE))) {
        cached <- NULL
      } else {
        if (!isTRUE(keep_tract_audit)) {
          attr(cached, "cacs_tract_audit") <- NULL
        }
        if (isTRUE(verbose)) {
          .cli_inform_cache(
            c("Using the result saved in the cache folder.",
              "i" = "A call with the same drive-time areas, ACS data, and settings reads this copy."),
            phase = "intersect"
          )
        }
        return(cached)
      }
    }
  }

  # Turn off s2 (spherical geometry in sf) while this function runs. sf keeps
  # the setting in the option sf_use_s2 (sf 1.0-9 and later) and reads it
  # there; on.exit() puts the option back as it was, also after an error and
  # also when it was not set, which sf::sf_use_s2(prev) would set to TRUE.
  prev_s2 <- options(sf_use_s2 = FALSE)
  on.exit(options(prev_s2), add = TRUE, after = FALSE)

  # Measure all areas in EPSG:5070 (NAD83 / Conus Albers), an equal-area
  # projection: the area of a shape on the projected map is its area on the
  # ground. .safe_transform_5070() turns a failed transformation (for example,
  # of a broken CRS) into an error that names the argument.
  iso_5070    <- .safe_transform_5070(iso_sf,    "iso_sf")
  acs_5070    <- .safe_transform_5070(acs_sf,    "acs_sf")
  bg_pop_5070 <- if (identical(weight_method, "population")) {
    .safe_transform_5070(bg_pop_sf, "bg_pop_sf")
  } else {
    NULL
  }

  # Repair invalid geometries; .repair_geometry() (R/utils.R) warns when it
  # changes any.
  iso_5070 <- .repair_geometry(iso_5070)
  acs_5070 <- .repair_geometry(acs_5070)
  if (!is.null(bg_pop_5070)) bg_pop_5070 <- .repair_geometry(bg_pop_5070)

  # Tract areas in square meters, measured once before the loop. sf::st_area()
  # gives one value per feature; for a MULTIPOLYGON it is the sum of the areas
  # of its parts, the whole tract area that each coverage weight divides by.
  acs_5070$tract_area_m2 <- as.numeric(sf::st_area(acs_5070))
  stopifnot(
    "Internal: tract_area_m2 length must match nrow(acs_5070)" =
      length(acs_5070$tract_area_m2) == nrow(acs_5070)
  )

  # A tract with zero or non-finite area (such as a water tract with an empty
  # boundary) cannot give a coverage weight. cacs_acs_prefetch() drops water
  # tracts by default (.drop_water_tracts()), so this catches acs_sf from
  # other sources or downloaded with drop_water_tracts = FALSE. Such rows are
  # skipped with a warning; the function stops only when no tract is left.
  # The GEOIDs are those of rows, so a tract appears once per variable.
  skipped_geoids_acc <- character(0)
  degen_mask <- !is.finite(acs_5070$tract_area_m2) | acs_5070$tract_area_m2 <= 0
  if (any(degen_mask)) {
    degen_geoids <- acs_5070$GEOID[degen_mask]
    if (all(degen_mask)) {
      .cli_abort_geometry(c(
        "Every tract in {.arg acs_sf} has an area that is zero, negative, or not a finite number.",
        "x" = "First GEOIDs: {.val {utils::head(unique(degen_geoids), 5L)}}.",
        "i" = "Remove tracts with an empty boundary, such as water tracts; {.fn cacs_acs_prefetch} removes them unless {.code drop_water_tracts = FALSE}."
      ))
    }
    # Some tracts are left: warn, keep the GEOIDs for the skipped_geoids
    # attribute, and go on with the other rows.
    .warn_skip_water_tract(degen_geoids)
    skipped_geoids_acc <- c(skipped_geoids_acc, degen_geoids)
    acs_5070 <- acs_5070[!degen_mask, , drop = FALSE]
  }

  # With verbose = TRUE, say how much work the loop has before it starts: the
  # numbers of sites, drive times, and tracts, and the number of pairs, which
  # is at most sites times drive times.
  if (isTRUE(verbose)) {
    n_sites_unique <- length(unique(iso_5070$site_id))
    n_dt_unique    <- length(unique(iso_5070$drive_time_min))
    n_tracts       <- length(unique(acs_5070$GEOID))
    .cli_inform_progress(c(
      "Intersecting {n_sites_unique} site{?s} and {n_dt_unique} drive time{?s} with {n_tracts} tract{?s} of ACS data.",
      "i" = "That makes up to {n_sites_unique * n_dt_unique} site and drive-time pair{?s}."
    ), phase = "intersect")
  }

  # One entry per site and drive time, sorted, so the rows of the result come
  # in the same order whatever the row order of iso_sf. A pair that appears
  # twice in iso_sf gets one entry here, and .intersect_one_site() then gives
  # it an empty row.
  pair_index <- iso_5070 |>
    sf::st_drop_geometry() |>
    dplyr::distinct(.data$site_id, .data$drive_time_min) |>
    dplyr::arrange(.data$site_id, .data$drive_time_min)

  n_iter <- nrow(pair_index)
  if (n_iter == 0L) {
    .cli_abort_operator(c(
      "{.arg iso_sf} has zero unique (site_id, drive_time_min) pair(s).",
      "i" = "Check that {.fn cacs_isochrone} produced a non-empty batch."
    ))
  }

  # Loop over the pairs. An error in one pair (a failed intersection, for
  # example) must not stop the others: tryCatch() turns it into the empty
  # row of .empty_site_result() (failure_origin = "intersection"), so every
  # pair gives rows that bind_rows() can combine. lapply() is used rather
  # than purrr::map() to avoid a dependency on purrr.
  #
  # Progress: tick() after each pair, whether it succeeded or failed, and a
  # summary line from finish() on exit, also if the function stops partway.
  # The error handlers below count the failed pairs in n_failed_iw_state.
  iw_prog <- .cacs_progress_reporter(n = n_iter, label = "Intersect+weight",
                                     verbose = verbose)
  n_failed_iw_state <- new.env(parent = emptyenv())
  n_failed_iw_state$n_success <- 0L
  n_failed_iw_state$n_failed  <- 0L
  on.exit(
    iw_prog$finish(
      n_success = n_failed_iw_state$n_success,
      n_failed  = n_failed_iw_state$n_failed
    ),
    add = TRUE
  )

  bag_per_site <- lapply(seq_len(n_iter), function(i) {
    sid <- pair_index$site_id[[i]]
    dt  <- pair_index$drive_time_min[[i]]
    res <- tryCatch(
      .intersect_one_site(
        sid        = sid,
        dt         = dt,
        iso_5070   = iso_5070,
        acs_5070   = acs_5070,
        min_weight = min_weight
      ),
      catchmentACS_error = function(e) {
        n_failed_iw_state$n_failed <- n_failed_iw_state$n_failed + 1L
        .empty_site_result(
          site_id        = sid,
          drive_time_min = dt,
          failure_reason = conditionMessage(e)
        )
      },
      error = function(e) {
        n_failed_iw_state$n_failed <- n_failed_iw_state$n_failed + 1L
        .empty_site_result(
          site_id        = sid,
          drive_time_min = dt,
          failure_reason = conditionMessage(e)
        )
      }
    )
    iw_prog$tick(detail = paste0(sid, "/", dt, "min"))
    res
  })
  # Pairs that succeeded: all pairs minus those caught by the handlers. A pair
  # with no tract but no error counts as a success.
  n_failed_iw_state$n_success <- as.integer(n_iter -
                                            n_failed_iw_state$n_failed)

  # One table with a row for each site, drive time, tract, and variable, with
  # area_wt, int_area_m2, and the ACS columns (variable, estimate, moe). The
  # empty rows of pairs with no tract have variable = NA and n_tracts = 0;
  # bind_rows() fills their missing columns with NA, and
  # .compute_family_aggregation() sets them aside as empty_carry.
  intersect_all <- dplyr::bind_rows(bag_per_site)
  tract_audit_tbl <- if (isTRUE(keep_tract_audit)) {
    .collect_tract_audit(intersect_all)
  } else {
    NULL
  }

  # Combine the tracts for each site, drive time, and variable: the weights,
  # the weighted sums and averages with their variances, and the estimate,
  # margin of error, and weight basis chosen by the kind of variable. Returns
  # list(public, carriers, empty_carry) and two counts of missing estimates.
  family_out <- .compute_family_aggregation(                            # nolint: object_usage_linter
    intersect_all = intersect_all,
    weight_method = weight_method,
    min_weight    = min_weight
  )

  # Build the rows of the result. family_out$public has one row per site,
  # drive time, and variable (failure_origin = "none"), and
  # family_out$empty_carry the rows of pairs with no tract (failure_origin =
  # "intersection"); family_out$carriers becomes the
  # cacs_aggregation_carriers attribute. bind_rows() fills the columns that
  # only one of the two tables has with NA. The columns that describe the
  # drive-time area are then joined from iso_sf, and the rest are set below.
  #
  # empty_carry is the part of intersect_all with variable = NA. It can be an
  # sf object (bind_rows() keeps the class of the first pair's rows), and it
  # has the per-tract and isochrone columns, NA on these rows. Drop the
  # geometry, the columns that the join or the assignments below supply (the
  # join would otherwise make .x and .y copies), and the per-tract and
  # isochrone columns. failure_reason stays for now and is removed below.
  upstream_supplied <- c("provider", "profile", "osm_snapshot_date",
                         "ring_topology")
  mutate_supplied   <- c(
    "acs_year", "weight_method",
    "moe_formula_requested", "moe_fallback", "moe_fallback_reason"
  )

  empty_norm <- family_out$empty_carry
  if (inherits(empty_norm, "sf")) {
    empty_norm <- sf::st_drop_geometry(empty_norm)
  }
  drop_from_empty <- intersect(
    names(empty_norm),
    c(upstream_supplied, mutate_supplied,
      # per-tract and isochrone columns that are not columns of the result
      "GEOID", "NAME", "tract_area_m2", "area_wt", "int_area_m2",
      "routing_engine_version", "polygon_simplification_tolerance",
      "generated_at", "isochrone_empty", "provider_requested",
      "provider_downgrade", "osm_snapshot_status", "retry_count")
  )
  empty_norm <- empty_norm[, setdiff(names(empty_norm), drop_from_empty),
                           drop = FALSE]
  empty_norm <- tibble::as_tibble(empty_norm)

  out <- dplyr::bind_rows(family_out$public, empty_norm)

  # The columns that describe the drive-time area (provider, profile,
  # osm_snapshot_date, ring_topology) are the same for every variable of a
  # site and drive time, so join them by site_id and drive_time_min. The
  # geometry is dropped first so that `out` stays a plain tibble.
  iso_provenance <- iso_5070 |>
    sf::st_drop_geometry() |>
    dplyr::distinct(
      .data$site_id, .data$drive_time_min,
      .data$provider, .data$profile, .data$osm_snapshot_date,
      .data$ring_topology
    )

  out <- out |>
    dplyr::left_join(iso_provenance, by = c("site_id", "drive_time_min"))

  # acs_year comes from the cacs_provenance attribute that cacs_acs_prefetch()
  # attaches to its result; ACS data without it give NA (.extract_acs_year()).
  acs_year_resolved <- .extract_acs_year(acs_sf)

  # moe_formula_requested: the default formula for the kind of variable, from
  # the same map as moe_formula_effective, so the two columns are equal here.
  # cacs_propagate_moe() sets both again.
  out$moe_formula_requested <- .dispatch_moe_formula_effective(
    out$estimand_family
  )

  # Columns with the same value on every row. weight_uncertainty_propagated is
  # FALSE because the margins of error treat the weights as fixed.
  out$acs_year                      <- acs_year_resolved
  out$weight_method                 <- weight_method
  out$moe_fallback                  <- FALSE
  out$moe_fallback_reason           <- "n/a"
  out$weight_uncertainty_propagated <- FALSE

  # Rows without a formula get moe_fallback = NA, as no formula applies to
  # them: the rows of pairs with no tract and the "metadata_only" rows (codes
  # that are not combined). Copying moe_formula_requested into
  # moe_formula_effective changes nothing, because moe_formula_requested is
  # NA on the same rows.
  na_eff <- is.na(out$moe_formula_effective)
  if (any(na_eff)) {
    out$moe_formula_effective[na_eff] <- out$moe_formula_requested[na_eff]
    out$moe_fallback[na_eff]          <- NA
  }

  # `failure_origin` is "none" on the combined rows and "intersection" on the
  # rows of pairs with no tract; an NA, which should not occur, becomes
  # "none".
  out$failure_origin <- ifelse(is.na(out$failure_origin),
                               "none",
                               out$failure_origin)

  # The weighted sums and averages, attached below as the
  # cacs_aggregation_carriers attribute.
  carrier_tbl <- family_out$carriers

  # failure_reason is not a column of the result, so the reason a pair has no
  # tract (or the error message of a pair that failed) is dropped here; only
  # failure_origin = "intersection" is left.
  out$failure_reason <- NULL

  # n_tracts_num and n_tracts_den belong to rate rows, which
  # cacs_derive_rates() adds and fills; they are NA on every row here.
  out$n_tracts_num <- NA_integer_
  out$n_tracts_den <- NA_integer_

  # Keep the columns of the result, in the order of .LONG_REQUIRED_COLS
  # (R/checks.R); this also drops the working columns.
  out <- out[, .LONG_REQUIRED_COLS]

  # Attach the attributes now, after the steps above that rebuild `out`.
  # cacs_aggregation_carriers has one row per site, drive time, and variable
  # of the combined rows, with the columns in .CARRIER_REQUIRED_COLS
  # (R/checks.R); cacs_propagate_moe() and cacs_derive_rates() look up the
  # weighted sums there (for a rate, those of its numerator and denominator).
  attr(out, "cacs_aggregation_carriers") <- carrier_tbl
  if (isTRUE(keep_tract_audit)) {
    attr(out, "cacs_tract_audit") <- tract_audit_tbl
  }

  # The GEOID of each row of acs_sf skipped for zero or non-finite area.
  # Always attached, as character(0) when nothing was skipped, so callers
  # need not check that the attribute exists.
  attr(out, "skipped_geoids") <- skipped_geoids_acc

  # A record of the settings, counts, and software versions. Despite their
  # names, n_sites_input counts site and drive-time pairs, n_sites_with_data
  # counts rows with tracts (one per pair and variable), and n_sites_empty
  # counts the rows of pairs with no tract.
  attr(out, "cacs_aggregation_provenance") <- list(
    n_sites_input        = nrow(pair_index),
    n_sites_with_data    = sum(out$n_tracts > 0L, na.rm = TRUE),
    n_sites_empty        = sum(out$n_tracts == 0L, na.rm = TRUE),
    weight_method        = weight_method,
    min_weight           = min_weight,
    generated_at         = as.POSIXct(Sys.time(), tz = "UTC"),
    sf_ver               = as.character(utils::packageVersion("sf")),
    geos_ver             = as.character(sf::sf_extSoftVersion()[["GEOS"]]),
    proj_ver             = as.character(sf::sf_extSoftVersion()[["PROJ"]]),
    cacs_ver             = as.character(utils::packageVersion("catchmentACS")),
	    acs_year             = acs_year_resolved,
	    n_variables_unique   = length(unique(stats::na.omit(out$variable))),
	    n_input_rows_missing_acs_estimate =
	      family_out$n_input_rows_missing_acs_estimate %||% 0L,
	    n_output_groups_missing_acs_estimate =
	      family_out$n_output_groups_missing_acs_estimate %||% 0L
	  )

  attr(out, "cacs_schema_version") <- "1.0"

  # Check the result before it is saved or returned: the columns, the ring
  # topology, cacs_aggregation_carriers (column types, unique keys that match
  # the combined rows, no negative variances), the tract table if any, and the
  # layout version (.validate_intersect_output() in R/checks.R).
  .validate_intersect_output(out, abort = TRUE,
                             require_symmetric_keys = TRUE)

  # Save the result in the cache. The key computed for the lookup is reused;
  # it is computed here only if the cache was off at the lookup. If writing
  # fails, .cacs_cache_put() gives a warning and the result is still
  # returned.
  if (isTRUE(.cacs_cache_intersect_enabled())) {
    if (is.null(intersect_cache_key)) {
      intersect_cache_key <- .cache_key_intersect(
        iso_sf        = iso_sf,
        acs_sf        = acs_sf,
        bg_pop_sf     = bg_pop_sf,
        weight_method = weight_method,
        min_weight    = min_weight
      )
    }
    cache_out <- out
    if (isTRUE(keep_tract_audit) && !is.null(tract_audit_tbl)) {
      attr(cache_out, "cacs_tract_audit") <- tract_audit_tbl
    }
    .cacs_cache_put(cache_out, intersect_cache_key, "intersect",
                    cache_dir = cache_dir)
    if (isTRUE(verbose)) {
      .cli_inform_cache(
        c("Saved the result in the cache folder.",
          "i" = "A later call with the same drive-time areas, ACS data, and settings reads this copy."),
        phase = "intersect"
      )
    }
  }

  out
}


#' Population weights, not implemented
#'
#' Population weighting is not implemented, and no code calls this function.
#' Called directly, it stops with the same `catchmentACS_error_credential`
#' error class as `cacs_intersect_weight(weight_method = "population")`.
#'
#' @param inter_one The rows of `.intersect_one_site()` for one site and
#'   drive time (not used).
#' @param bg_pop_5070 Block-group population data in EPSG:5070 (not used).
#'
#' @return Does not return; always gives an error.
#'
#' @keywords internal
#' @noRd
.apply_pop_weight <- function(inter_one, bg_pop_5070) {
  .cli_abort_credential(c(
    "{.fn .apply_pop_weight} is deferred to a future release.",
    "i" = "The current release supports {.val area} weighting only."
  ))
}


# Combining the tracts of each site, drive time, and variable.

#' Weighted sum of tract values
#'
#' Returns `sum(w * x)`, or `NA` when any value of `x` is missing.
#'
#' @keywords internal
#' @noRd
.sum_weighted_estimate <- function(w, x) {
  if (anyNA(x)) return(NA_real_)
  sum(w * x)
}


# Values that the Census Bureau's data API puts in place of an estimate or a
# margin of error that it does not publish or that does not apply, such as
# -666666666 (too few sample observations) and -555555555 (a controlled
# estimate, with no margin of error).
.ACS_ANNOTATION_CODES <- c(-999999999, -888888888, -666666666, -555555555,
                           -333333333, -222222222)

#' Set Census Bureau annotation codes in ACS data to NA
#'
#' The codes in `.ACS_ANNOTATION_CODES` are not numbers to add up: an
#' estimate code would enter the weighted sums and a margin-of-error code would
#' be squared. Both become `NA`, which gives `NA` values downstream as the
#' help page of cacs_intersect_weight() describes. A warning counts the
#' estimates and the margins of error that were set to `NA` and names the
#' codes found among them; a margin-of-error code next to a missing estimate is
#' not counted, because the result is `NA` either way (the bundled sample data
#' have such rows).
#'
#' @param acs_sf The `acs_sf` argument, already checked.
#' @return `acs_sf` with the codes replaced by `NA`.
#' @keywords internal
#' @noRd
.cacs_acs_codes_to_na <- function(acs_sf) {
  est_code <- !is.na(acs_sf$estimate) & acs_sf$estimate %in% .ACS_ANNOTATION_CODES
  moe_code <- !is.na(acs_sf$moe) & acs_sf$moe %in% .ACS_ANNOTATION_CODES
  moe_counted <- moe_code & !is.na(acs_sf$estimate) & !est_code
  n_est <- sum(est_code)
  n_moe <- sum(moe_counted)
  if (n_est > 0L || n_moe > 0L) {
    # The codes counted in the message, in the order of .ACS_ANNOTATION_CODES.
    found <- c(acs_sf$estimate[est_code], acs_sf$moe[moe_counted])
    codes <- .ACS_ANNOTATION_CODES[.ACS_ANNOTATION_CODES %in% found]
    .cli_warn_runtime(c(
      "{.arg acs_sf} has Census Bureau annotation codes in place of {n_est} estimate{?s} and {n_moe} margin{?s} of error; they are treated as {.code NA}.",
      "i" = "{cli::qty(length(codes))}Code{?s} in {.arg acs_sf}: {.val {codes}}.",
      "i" = "A missing estimate makes the estimate and margin of error of that variable {.code NA} for the areas that include the tract, and a missing margin of error makes its margin of error {.code NA}; a rate that uses the variable is {.code NA} in both cases."
    ), phase = "intersect")
  }
  acs_sf$estimate[est_code] <- NA_real_
  acs_sf$moe[moe_code] <- NA_real_
  acs_sf
}


#' Variance of a weighted sum of tract estimates
#'
#' ACS margins of error are published at the 90 percent level, so each is
#' divided by 1.645 to give a standard error, and the variance of
#' `sum(w * estimate)` is taken as `sum((w * se)^2)`. This treats the tract
#' estimates as independent (no covariance terms) and the weights as fixed.
#' With coverage weights it is the Census Bureau's approximation for a sum of
#' estimates (U.S. Census Bureau 2020, chapter 8) applied to the weighted
#' tract estimates; with area shares, for medians and per-person values, it
#' is an approximation made by the package. Returns `NA` when any estimate or
#' margin of error is missing.
#'
#' @keywords internal
#' @noRd
.sum_weighted_variance <- function(w, estimate, moe) {
  if (anyNA(estimate) || anyNA(moe)) return(NA_real_)
  sum((w * moe / .Z_ACS_90)^2)
}


#' Combine the tracts of each site, drive time, and variable
#'
#' Takes the rows of all pairs (one row per site, drive time, tract, and
#' variable; the rows of pairs with no tract have `variable = NA`). For each
#' site, drive time, and variable it computes the sum of the tract estimates
#' weighted by coverage weights (`est_total`) and their average weighted by
#' area shares (`est_mean`), each with its variance, and then chooses the
#' estimate, margin of error, and `weight_basis` by the kind of variable
#' (`estimand_family`, from `.classify_acs_variable_batch()`).
#'
#' Returns a list with
#'   - `public`: one row per site, drive time, and variable, with the columns
#'     of `.empty_family_public()`;
#'   - `carriers`: the weighted sums, averages, and variances of the same
#'     rows (the columns of `.empty_family_carriers()`), which become the
#'     `cacs_aggregation_carriers` attribute;
#'   - `empty_carry`: the rows with `variable = NA`, unchanged;
#'   - `n_input_rows_missing_acs_estimate` and
#'     `n_output_groups_missing_acs_estimate`: the numbers of input rows with
#'     a missing estimate and of output rows with at least one.
#'
#' @param intersect_all The rows of all pairs, bound together.
#' @param weight_method `"area"`; `cacs_intersect_weight()` stops earlier for
#'   `"population"`.
#' @param min_weight Variables whose summed coverage weight is below it are
#'   dropped.
#'
#' @keywords internal
#' @noRd
.compute_family_aggregation <- function(intersect_all,
                                        weight_method = "area",
                                        min_weight    = 1e-6) {

  # Set aside the rows of pairs with no tract (variable = NA, from
  # .empty_site_result()). They are returned unchanged as empty_carry and get
  # no row in `carriers`.
  is_empty    <- is.na(intersect_all$variable)
  empty_carry <- intersect_all[is_empty, , drop = FALSE]
  success     <- intersect_all[!is_empty, , drop = FALSE]

  # The geometry is not needed from here on, and sf would keep it through
  # the dplyr steps below (the geometry column of an sf object is sticky).
  if (inherits(success, "sf")) {
    success <- sf::st_drop_geometry(success)
  }

  # No pair has a tract: return zero-row tables with the usual columns, so
  # that cacs_intersect_weight() can still bind the rows and attach
  # cacs_aggregation_carriers.
  if (nrow(success) == 0L) {
    return(list(
      public      = .empty_family_public(),
      carriers    = .empty_family_carriers(),
      empty_carry = empty_carry,
      n_input_rows_missing_acs_estimate = 0L,
      n_output_groups_missing_acs_estimate = 0L
    ))
  }

  # Two weights for each row. coverage_wt is area_wt, the share of the
  # tract's area inside the drive-time area; counts are summed with it.
  # mean_wt is the tract's area share, its overlap area divided by the
  # overlap area of all tracts combined; medians and per-person values are
  # averaged with it, so it must sum to one within each site, drive time,
  # and variable. The grouping includes `variable` because `success` has one
  # row per tract and variable: grouped by site and drive time only, each
  # overlap area would be counted once per variable, and every average
  # divided by the number of variables.
  success <- success |>
    dplyr::group_by(.data$site_id, .data$drive_time_min, .data$variable) |>
    dplyr::mutate(
      coverage_wt = .data$area_wt,
      mean_wt     = .data$int_area_m2 /
                      sum(.data$int_area_m2, na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  weight_basis_total  <- "coverage"
  weight_basis_scalar <- if (identical(weight_method, "area")) {
    "area_mean"
  } else {
    "population_mean"          # nocov (population weighting is not implemented)
  }

  # The kind of each variable (estimand_family), from its ACS code: each
  # distinct code is classified once and the result is looked up for every
  # row.
  uniq_vars     <- unique(success$variable)
  family_map    <- .classify_acs_variable_batch(uniq_vars)
  family_lookup <- stats::setNames(family_map, uniq_vars)
  success$estimand_family <- unname(family_lookup[success$variable])

  agg <- success |>
    dplyr::group_by(
      .data$site_id, .data$drive_time_min,
      .data$variable, .data$estimand_family
    ) |>
    dplyr::summarise(
      # For every kind of variable: the sum of the coverage weights, and the
      # number of tracts, including those with a missing estimate
      weight_sum    = sum(.data$coverage_wt, na.rm = TRUE),
      n_tracts      = dplyr::n(),
      n_missing_estimate = sum(is.na(.data$estimate)),

      # Sum with coverage weights and its variance: the estimate of a count,
      # and the numerator or denominator of a rate in cacs_derive_rates()
      est_total     = .sum_weighted_estimate(.data$coverage_wt,
                                             .data$estimate),
      var_total_raw = .sum_weighted_variance(.data$coverage_wt,
                                             .data$estimate, .data$moe),

      # Average with area shares and its variance: the estimate of a median
      # or a per-person value
      est_mean      = .sum_weighted_estimate(.data$mean_wt,
                                             .data$estimate),
      var_mean_raw  = .sum_weighted_variance(.data$mean_wt,
                                             .data$estimate, .data$moe),

      .groups       = "drop"
    )

  # Choose the estimate, margin of error, and weight basis by the kind of
  # variable: counts take the weighted sum, medians and per-person values the
  # average, and "metadata_only" codes NA. .classify_acs_variable_batch()
  # gives only these kinds, so the branches for "derived_rate",
  # "population_weighted_scalar_proxy", and "area_weighted_rate_proxy" are
  # not reached here: rates are made later by cacs_derive_rates(), and
  # population weighting is not implemented.
  #
  # Then drop variables whose summed coverage weight is below min_weight. In
  # cacs_intersect_weight() this removes nothing, because every tract that
  # .intersect_one_site() keeps has a coverage weight above min_weight.
  agg <- agg |>
    dplyr::mutate(
      estimate = dplyr::case_when(
        .data$estimand_family == "spatial_total"                    ~ .data$est_total,
        .data$estimand_family == "area_weighted_scalar_proxy"       ~ .data$est_mean,
        .data$estimand_family == "population_weighted_scalar_proxy" ~ .data$est_mean,
        .data$estimand_family == "area_weighted_rate_proxy"         ~ .data$est_mean,
        .data$estimand_family == "median_proxy"                     ~ .data$est_mean,
        .data$estimand_family == "derived_rate"                     ~ NA_real_,
        .data$estimand_family == "metadata_only"                    ~ NA_real_,
        TRUE                                                        ~ NA_real_
      ),
      moe = dplyr::case_when(
        .data$estimand_family == "spatial_total"                    ~ .Z_ACS_90 * sqrt(.data$var_total_raw),
        .data$estimand_family %in% c("area_weighted_scalar_proxy",
                                     "population_weighted_scalar_proxy",
                                     "area_weighted_rate_proxy",
                                     "median_proxy")                ~ .Z_ACS_90 * sqrt(.data$var_mean_raw),
        .data$estimand_family == "derived_rate"                     ~ NA_real_,
        .data$estimand_family == "metadata_only"                    ~ NA_real_,
        TRUE                                                        ~ NA_real_
      ),
      weight_basis = dplyr::case_when(
        .data$estimand_family == "spatial_total"                    ~ weight_basis_total,
        .data$estimand_family %in% c("area_weighted_scalar_proxy",
                                     "area_weighted_rate_proxy",
                                     "median_proxy")                ~ weight_basis_scalar,
        .data$estimand_family == "population_weighted_scalar_proxy" ~ "population_mean",
        .data$estimand_family == "derived_rate"                     ~ weight_basis_total,
        .data$estimand_family == "metadata_only"                    ~ "none",
        TRUE                                                        ~ "none"
      ),
      moe_formula_effective = .dispatch_moe_formula_effective(
        .data$estimand_family
      ),
      failure_origin                = "none",
      weight_uncertainty_propagated = FALSE   # the weights are taken as fixed
    ) |>
    dplyr::filter(
      .data$weight_sum >= min_weight | .data$estimand_family == "derived_rate"
    )

  # Split into `carriers` (the weighted sums, averages, and variances, keyed
  # by site, drive time, and variable) and the rows of the result, which
  # cacs_intersect_weight() completes. Because est_total and var_total_raw
  # are kept for every variable, cacs_derive_rates() can find the numerator
  # and denominator of a rate by their ACS codes, for example
  #   numerator estimate   = est_total[variable == numerator code]
  #   denominator variance = var_total_raw[variable == denominator code]
  carrier_rows <- agg |>
    dplyr::select(
      "site_id", "drive_time_min", "variable", "estimand_family",
      "est_total", "var_total_raw", "est_mean", "var_mean_raw",
      "weight_sum", "n_tracts"
    )

  public_rows <- agg |>
    dplyr::select(
      "site_id", "drive_time_min", "variable", "estimate", "moe",
      "weight_sum", "n_tracts", "estimand_family", "weight_basis",
      "moe_formula_effective", "failure_origin",
      "weight_uncertainty_propagated"
    )

  list(
    public      = public_rows,
    carriers    = carrier_rows,
    empty_carry = empty_carry,
    n_input_rows_missing_acs_estimate = as.integer(sum(is.na(success$estimate))),
    n_output_groups_missing_acs_estimate = as.integer(sum(agg$n_missing_estimate > 0L))
  )
}


# Helpers used by .compute_family_aggregation() and cacs_intersect_weight().

#' Zero-row table with the columns of the `public` rows
#' @keywords internal
#' @noRd
.empty_family_public <- function() {
  tibble::tibble(
    site_id                       = character(0),
    drive_time_min                = integer(0),
    variable                      = character(0),
    estimate                      = numeric(0),
    moe                           = numeric(0),
    weight_sum                    = numeric(0),
    n_tracts                      = integer(0),
    estimand_family               = character(0),
    weight_basis                  = character(0),
    moe_formula_effective         = character(0),
    failure_origin                = character(0),
    weight_uncertainty_propagated = logical(0)
  )
}

#' Zero-row table with the columns of `cacs_aggregation_carriers`
#' @keywords internal
#' @noRd
.empty_family_carriers <- function() {
  tibble::tibble(
    site_id         = character(0),
    drive_time_min  = integer(0),
    variable        = character(0),
    estimand_family = character(0),
    est_total       = numeric(0),
    var_total_raw   = numeric(0),
    est_mean        = numeric(0),
    var_mean_raw    = numeric(0),
    weight_sum      = numeric(0),
    n_tracts        = integer(0)
  )
}

#' Default margin-of-error formula for each kind of variable
#'
#' Maps `estimand_family` to a formula label: `"weighted_sum"` for counts,
#' `"weighted_mean"` for the averaged kinds (medians and per-person values),
#' `"general_ratio_conservative"` (the ratio formula) for rates, and `NA` for
#' `"metadata_only"` and missing values. `cacs_intersect_weight()` fills both
#' `moe_formula_effective` and `moe_formula_requested` from it.
#'
#' @keywords internal
#' @noRd
.dispatch_moe_formula_effective <- function(estimand_family) {
  dplyr::case_when(
    estimand_family == "spatial_total"                    ~ "weighted_sum",
    estimand_family == "area_weighted_scalar_proxy"       ~ "weighted_mean",
    estimand_family == "population_weighted_scalar_proxy" ~ "weighted_mean",
    estimand_family == "area_weighted_rate_proxy"         ~ "weighted_mean",
    estimand_family == "median_proxy"                     ~ "weighted_mean",
    estimand_family == "derived_rate"                     ~ "general_ratio_conservative",
    estimand_family == "metadata_only"                    ~ NA_character_,
    TRUE                                                  ~ NA_character_
  )
}


# Argument checks and the transformation to EPSG:5070, used before the loop
# over pairs.

#' Check that `min_weight` is a single finite number in [0, 1)
#' @keywords internal
#' @noRd
.validate_min_weight <- function(min_weight) {
  if (!is.numeric(min_weight) ||
      length(min_weight) != 1L ||
      is.na(min_weight) ||
      !is.finite(min_weight) ||
      min_weight < 0 ||
      min_weight >= 1) {
    .cli_abort_schema(c(
      "{.arg min_weight} must be a number that is at least 0 and less than 1.",
      "x" = "Got value {.val {min_weight}}."
    ))
  }
  invisible(TRUE)
}


#' Check that `verbose` is `TRUE` or `FALSE`
#' @keywords internal
#' @noRd
.validate_verbose <- function(verbose) {
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    .cli_abort_schema(c(
      "{.arg verbose} must be a non-missing length-1 logical."
    ))
  }
  invisible(TRUE)
}


#' Transform to EPSG:5070, with an error that names the argument
#'
#' Calls `sf::st_transform(x, 5070)`. If the transformation fails, for
#' example because the CRS of `x` is missing or broken, stops with a
#' `catchmentACS_error_schema` error that names `arg_name` and keeps the
#' original error as its parent.
#'
#' @keywords internal
#' @noRd
.safe_transform_5070 <- function(x, arg_name) {
  tryCatch(
    sf::st_transform(x, 5070),
    error = function(e) {
      .cli_abort_schema(
        c(
          "Failed to reproject {.arg {arg_name}} to EPSG:5070.",
          "x" = "PROJ rejected the transform.",
          "i" = "Check the input's CRS with {.code sf::st_crs(...)}; original error: {conditionMessage(e)}."
        ),
        parent = e
      )
    }
  )
}


#' Is a bounding box outside the contiguous United States and DC?
#'
#' Areas are measured in EPSG:5070 (NAD83 / Conus Albers), which EPSG defines
#' for the contiguous 48 states. The box is longitude -125.0 to -66.93 and
#' latitude 24.39 to 49.39, widened by 0.25 degrees on each side so that
#' tracts and drive-time areas along the coasts and borders still pass.
#' Returns `TRUE` when `bbox` reaches outside the widened box, and also when
#' it is not finite (as for data with only empty geometries).
#'
#' Alaska, Hawaii, and Puerto Rico are outside the widened box on at least one
#' side. In the Census Bureau's 2020 cartographic boundary file of the states,
#' all of Alaska lies north of 51.2 degrees, all of Hawaii west of -154.8
#' degrees, and all of Puerto Rico south of 18.6 degrees.
#'
#' @keywords internal
#' @noRd
.bbox_outside_conus_dc <- function(bbox) {
  pad <- 0.25
  lon_min <- -125.0 - pad     # -125.25
  lon_max <-  -66.93 + pad    #  -66.68
  lat_min <-   24.39 - pad    #   24.14
  lat_max <-   49.39 + pad    #   49.64

  if (any(!is.finite(c(bbox["xmin"], bbox["xmax"], bbox["ymin"], bbox["ymax"])))) {
    return(TRUE)
  }
  bbox["xmin"] < lon_min ||
    bbox["xmax"] > lon_max ||
    bbox["ymin"] < lat_min ||
    bbox["ymax"] > lat_max
}


#' Do the drive-time areas extend beyond the ACS data?
#'
#' Compares bounding boxes: `TRUE` when `iso_bbox` extends more than 0.05
#' degrees beyond `acs_bbox` on any side.
#'
#' @keywords internal
#' @noRd
.bbox_extends_beyond <- function(iso_bbox, acs_bbox) {
  pad <- 0.05
  iso_bbox["xmin"] < acs_bbox["xmin"] - pad ||
    iso_bbox["xmax"] > acs_bbox["xmax"] + pad ||
    iso_bbox["ymin"] < acs_bbox["ymin"] - pad ||
    iso_bbox["ymax"] > acs_bbox["ymax"] + pad
}


# The work for one site and drive time, and the table of tracts.


#' Hide two sf notices while evaluating an expression
#'
#' Hides sf's notice that longitude/latitude coordinates are treated as
#' planar by `st_intersects()` or `st_intersection()` (a message in some sf
#' versions, a warning in others) and its warning that attribute variables
#' are assumed to be spatially constant; other messages and warnings pass
#' through. In `cacs_intersect_weight()` neither notice arises, because the
#' data are already in EPSG:5070 and the attributes are marked constant; the
#' handler matters when `.intersect_one_site()` is called directly on
#' longitude/latitude data, as some tests do.
#'
#' @param expr Expression to evaluate.
#' @return The value of `expr`.
#' @keywords internal
#' @noRd
.cacs_muffle_internal_sf_planar_noise <- function(expr) {
  is_planar_noise <- function(cnd) {
    msg <- conditionMessage(cnd)
    grepl("longitude/latitude", msg, ignore.case = TRUE) &&
      grepl("planar", msg, ignore.case = TRUE) &&
      grepl("st_intersects|st_intersection", msg, ignore.case = TRUE)
  }
  is_attr_noise <- function(cnd) {
    grepl("attribute variables are assumed to be spatially constant",
          conditionMessage(cnd), ignore.case = TRUE)
  }

  withCallingHandlers(
    expr,
    message = function(m) {
      if (is_planar_noise(m)) {
        invokeRestart("muffleMessage")
      }
    },
    warning = function(w) {
      if (is_planar_noise(w) || is_attr_noise(w)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}


#' Intersect one site and drive time with the tracts
#'
#' Selects the area of the pair (`sid`, `dt`), finds the tracts that intersect
#' it, computes the overlaps and their areas, and keeps the tracts whose
#' coverage weight `area_wt` (overlap area divided by tract area) is above
#' `min_weight`. Returns an sf table with one row per kept tract and variable,
#' with `area_wt`, `int_area_m2`, the ACS columns, the columns of the area,
#' and the overlap geometry; or else the one row of `.empty_site_result()`
#' with one of these values of `failure_reason`:
#'
#'   - `iso_empty_or_duplicated`: the pair has no area in `iso_5070`, an
#'     empty one, or more than one;
#'   - `no_tract_intersection`: no tract intersects the area;
#'   - `intersection_empty_after_repair`: the overlaps are empty after repair;
#'   - `all_slivers_below_min_weight`: every tract is at or below
#'     `min_weight`.
#'
#' An error in the intersection becomes a `catchmentACS_error_geometry`
#' error that names the pair. `cacs_intersect_weight()` turns it, like any
#' other error in this function, into the empty row.
#'
#' @param sid A single `site_id`.
#' @param dt A single drive time in minutes.
#' @param iso_5070 The drive-time areas of all pairs, in EPSG:5070.
#' @param acs_5070 The ACS data in EPSG:5070, with the column `tract_area_m2`.
#' @param min_weight Tracts with a coverage weight at or below it are dropped.
#'
#' @return An sf table or a one-row tibble, as described above.
#'
#' @keywords internal
#' @noRd
.intersect_one_site <- function(sid, dt, iso_5070, acs_5070, min_weight) {

  # The area of this pair; none, an empty one, or more than one gives the
  # empty row.
  iso_one <- iso_5070[iso_5070$site_id == sid &
                        iso_5070$drive_time_min == dt, ]

  if (nrow(iso_one) == 0L ||
      nrow(iso_one) > 1L ||
      all(sf::st_is_empty(iso_one))) {
    return(.empty_site_result(
      site_id        = sid,
      drive_time_min = dt,
      n_tracts       = 0L,
      failure_reason = "iso_empty_or_duplicated"
    ))
  }

  # Find the candidate tracts first, so that the exact intersection below runs
  # only on tracts that touch the area. st_intersects(acs_5070, iso_one)
  # gives one list element per row of acs_5070; rows with a non-empty element
  # intersect the area.
  hits <- .cacs_muffle_internal_sf_planar_noise(
    sf::st_intersects(acs_5070, iso_one, sparse = TRUE)
  )
  candidate_idx <- which(lengths(hits) > 0L)
  if (length(candidate_idx) == 0L) {
    return(.empty_site_result(
      site_id        = sid,
      drive_time_min = dt,
      n_tracts       = 0L,
      failure_reason = "no_tract_intersection"
    ))
  }
  acs_candidates <- acs_5070[candidate_idx, ]

  # The exact overlaps. Marking the attributes as constant stops
  # st_intersection() from warning that they are assumed to be spatially
  # constant; the result keeps the columns of both inputs, including
  # tract_area_m2. An error here becomes a catchmentACS_error_geometry error
  # that names the pair.
  sf::st_agr(acs_candidates) <- "constant"
  sf::st_agr(iso_one)        <- "constant"

  intersect_geom <- tryCatch(
    .cacs_muffle_internal_sf_planar_noise(
      sf::st_intersection(acs_candidates, iso_one)
    ),
    error = function(e) {
      .cli_abort_geometry(c(
        "GEOS intersection failed for site {.val {sid}} at drive_time {.val {dt}}.",
        "x" = "Underlying error: {.val {conditionMessage(e)}}",
        "i" = "Inspect {.code sf::st_is_valid(acs_sf)} and {.code sf::st_is_valid(iso_sf)}."
      ))
    }
  )

  intersect_geom <- .repair_geometry(intersect_geom)

  if (nrow(intersect_geom) == 0L || all(sf::st_is_empty(intersect_geom))) {
    return(.empty_site_result(
      site_id        = sid,
      drive_time_min = dt,
      n_tracts       = 0L,
      failure_reason = "intersection_empty_after_repair"
    ))
  }

  # Coverage weight: the overlap area divided by the tract area, both in
  # square meters in EPSG:5070. As a ratio of two areas within the same
  # tract, it changes little with the way area is measured. A tract is kept
  # only when `area_wt` is above `min_weight`, so a tract at or below it, or
  # with an NA weight, is dropped; this also removes tracts that only touch
  # the edge of the area.
  intersect_geom$int_area_m2 <- as.numeric(sf::st_area(intersect_geom))
  intersect_geom$area_wt     <- intersect_geom$int_area_m2 /
                                  intersect_geom$tract_area_m2

  intersect_geom <- intersect_geom[
    !is.na(intersect_geom$area_wt) & intersect_geom$area_wt > min_weight, ]

  if (nrow(intersect_geom) == 0L) {
    return(.empty_site_result(
      site_id        = sid,
      drive_time_min = dt,
      n_tracts       = 0L,
      failure_reason = "all_slivers_below_min_weight"
    ))
  }

  # A tract that lies wholly inside the area can get an area_wt slightly above
  # 1 from floating-point error in the areas. Such weights are set to 1, with
  # a warning only when the excess is above 1e-9.
  over_unit <- which(intersect_geom$area_wt > 1)
  if (length(over_unit) > 0L) {
    excess <- max(intersect_geom$area_wt[over_unit] - 1)
    if (excess > 1e-9) {
      .cli_warn_runtime(c(
        "{.field area_wt} was above 1 for {length(over_unit)} row{?s} and was set to 1.",
        "i" = "The largest excess was {signif(excess, 3)}; a tract that lies wholly inside an area can get a weight slightly above 1 from rounding in the areas."
      ), phase = "intersect")
    }
    intersect_geom$area_wt[over_unit] <- 1
  }

  # st_intersection() has already copied site_id and drive_time_min from
  # iso_one; setting them here makes sure they hold the pair's values, with
  # drive_time_min as an integer.
  intersect_geom$site_id        <- sid
  intersect_geom$drive_time_min <- as.integer(dt)

  intersect_geom
}


#' Table of the tracts used for each site and drive time
#'
#' Builds the `cacs_tract_audit` attribute. The intersection rows have one
#' row per tract and variable, and the table one row per site, drive time,
#' and tract: the six columns below are kept without geometry, rows without
#' a `GEOID` or `area_wt` (the rows of pairs with no tract) are dropped, and
#' the copies for the different variables are removed.
#'
#' @param intersect_all The rows of all pairs, bound together.
#' @return A tibble with the columns site_id, drive_time_min, GEOID, area_wt,
#'   int_area_m2, and tract_area_m2, sorted by the first three.
#' @keywords internal
#' @noRd
.collect_tract_audit <- function(intersect_all) {
  cols <- c("site_id", "drive_time_min", "GEOID", "area_wt",
            "int_area_m2", "tract_area_m2")
  empty <- tibble::tibble(
    site_id        = character(),
    drive_time_min = integer(),
    GEOID          = character(),
    area_wt        = numeric(),
    int_area_m2    = numeric(),
    tract_area_m2  = numeric()
  )
  if (is.null(intersect_all) || nrow(intersect_all) == 0L ||
      !all(cols %in% names(intersect_all))) {
    return(empty)
  }
  x <- intersect_all
  if (inherits(x, "sf")) {
    x <- sf::st_drop_geometry(x)
  }
  x <- x[!is.na(x$GEOID) & !is.na(x$area_wt), cols, drop = FALSE]
  if (nrow(x) == 0L) return(empty)
  x <- tibble::as_tibble(x)
  x$site_id <- as.character(x$site_id)
  x$drive_time_min <- as.integer(x$drive_time_min)
  x$GEOID <- as.character(x$GEOID)
  x$area_wt <- as.numeric(x$area_wt)
  x$int_area_m2 <- as.numeric(x$int_area_m2)
  x$tract_area_m2 <- as.numeric(x$tract_area_m2)
  x |>
    dplyr::distinct() |>
    dplyr::arrange(.data$site_id, .data$drive_time_min, .data$GEOID)
}


# The ACS year of the result and the cache key of a call.


#' ACS year of the ACS data
#'
#' Reads `year` (or `acs_year`) from the list that `cacs_acs_prefetch()`
#' attaches to its result under the names `cacs_provenance` and
#' `cacs_acs_provenance`. ACS data made in other ways usually have neither
#' attribute, and then the result is `NA_integer_` rather than an error, so
#' that such data can still be used.
#'
#' @param acs_sf The ACS data given to `cacs_intersect_weight()`.
#' @return A single integer, or `NA_integer_` when the attribute or the year
#'   is missing or is not a single value.
#' @keywords internal
#' @noRd
.extract_acs_year <- function(acs_sf) {
  prov <- attr(acs_sf, "cacs_provenance") %||% attr(acs_sf, "cacs_acs_provenance")
  if (is.null(prov) || !is.list(prov)) {
    return(NA_integer_)
  }
  yr <- prov$year %||% prov$acs_year
  if (is.null(yr) || length(yr) != 1L || is.na(yr)) {
    return(NA_integer_)
  }
  as.integer(yr)
}


#' Cache key of an input sf object
#'
#' Returns the `cache_key` recorded in the attribute `cacs_provenance`,
#' `cacs_acs_provenance`, or `cacs_isochrone_provenance` (the first one
#' present), which `cacs_acs_prefetch()` and `cacs_isochrone()` attach to
#' their results. Without such a key it returns a SHA-256 checksum of the
#' coordinates and the values of the other columns, so that data made in
#' other ways get a key that changes when the data change; the row names are
#' left out, because they are stored differently depending on whether tibble
#' was loaded when the rows were selected. A recorded key does not change when
#' rows are selected or edited: `[` and `dplyr::filter()` keep the attribute,
#' so a subset or an edited copy of such a result gets the key of the whole
#' result, and the key of a call therefore also uses the contents
#' (`.extract_iso_content_hash()`, `.extract_acs_content_hash()`). Returns
#' `NA` for `NULL`.
#'
#' @param x An sf object (`iso_sf` or `acs_sf`), or `NULL`.
#' @return A single string.
#' @keywords internal
#' @noRd
.extract_cache_key <- function(x) {
  if (is.null(x)) return(NA_character_)
  prov <- attr(x, "cacs_provenance") %||%
          attr(x, "cacs_acs_provenance") %||%
          attr(x, "cacs_isochrone_provenance")
  if (is.list(prov) && is.character(prov$cache_key) &&
      length(prov$cache_key) == 1L && nzchar(prov$cache_key)) {
    return(prov$cache_key)
  }
  # No recorded key: a checksum of the coordinates and the other columns. The
  # CRS and the other sf attributes are left out, so that the same CRS
  # written differently (for example by other versions of sf or PROJ) does
  # not change the key.
  body_cols <- as.list(sf::st_drop_geometry(x))
  coords    <- tryCatch(sf::st_coordinates(x), error = function(e) NULL)
  digest::digest(list(coords = coords, attrs = body_cols),
                 algo = "sha256", serialize = TRUE)
}

# Checksum of the drive-time areas of iso_sf as they enter the result: a
# checksum of each geometry, the columns that are carried into the result or
# name the pairs (site_id, drive_time_min, provider, profile,
# osm_snapshot_date, ring_topology), and the CRS. The rows are sorted, because
# the pairs are computed in the order of site_id and drive_time_min whatever
# the row order, and the row names are not used. Columns that do not reach the
# result, such as generated_at, are left out, so the same areas made again
# still match.
.extract_iso_content_hash <- function(iso_sf) {
  if (is.null(iso_sf)) return(NA_character_)
  keep <- intersect(c("site_id", "drive_time_min", "provider", "profile",
                      "osm_snapshot_date", "ring_topology"), names(iso_sf))
  rows <- as.data.frame(lapply(as.list(sf::st_drop_geometry(iso_sf))[keep],
                               as.character),
                        stringsAsFactors = FALSE)
  wkb <- sf::st_as_binary(sf::st_geometry(iso_sf), EWKB = TRUE)
  rows$.geometry_hash <- vapply(
    wkb,
    function(g) digest::digest(g, algo = "sha256", serialize = FALSE),
    character(1)
  )
  # method = "radix" sorts strings by bytes, whatever the locale.
  ord <- do.call(order, c(unname(as.list(rows)), list(method = "radix")))
  rows <- rows[ord, , drop = FALSE]
  rownames(rows) <- NULL
  crs <- sf::st_crs(iso_sf)
  crs_id <- if (!is.na(crs$epsg)) as.character(crs$epsg) else crs$wkt
  digest::digest(
    list(schema = "iso-content-v1", crs = crs_id, rows = as.list(rows)),
    algo = "sha256",
    serialize = TRUE
  )
}

# Checksum of the contents of acs_sf: the columns GEOID, variable, estimate,
# moe, and NAME with a checksum of each geometry (rows sorted, so that row
# order does not matter), the CRS, and some fields of the cacs_provenance
# (or cacs_acs_provenance) attribute.
.extract_acs_content_hash <- function(acs_sf) {
  if (is.null(acs_sf)) return(NA_character_)
  attrs <- tryCatch(sf::st_drop_geometry(acs_sf), error = function(e) acs_sf)
  attrs <- as.data.frame(attrs, stringsAsFactors = FALSE)
  keep <- intersect(c("GEOID", "variable", "estimate", "moe", "NAME"), names(attrs))
  attrs_keep <- attrs[, keep, drop = FALSE]
  geometry_hash <- tryCatch(
    {
      wkb <- sf::st_as_binary(sf::st_geometry(acs_sf), EWKB = TRUE)
      vapply(
        wkb,
        function(x) digest::digest(x, algo = "sha256", serialize = FALSE),
        character(1)
      )
    },
    error = function(e) {
      geom_txt <- tryCatch(
        as.character(sf::st_as_text(sf::st_geometry(acs_sf))),
        error = function(e2) rep(NA_character_, nrow(attrs_keep))
      )
      vapply(
        geom_txt,
        digest::digest,
        character(1),
        algo = "sha256",
        serialize = TRUE
      )
    }
  )
  attrs_keep$.geometry_hash <- geometry_hash
  ord_cols <- intersect(c("GEOID", "variable", "NAME", "estimate", "moe"),
                        names(attrs_keep))
  if (length(ord_cols) > 0L && nrow(attrs_keep) > 0L) {
    ord <- do.call(order, attrs_keep[ord_cols])
    attrs_keep <- attrs_keep[ord, , drop = FALSE]
  }
  rownames(attrs_keep) <- NULL
  crs_id <- tryCatch({
    crs <- sf::st_crs(acs_sf)
    if (!is.na(crs$epsg)) as.character(crs$epsg) else crs$wkt
  }, error = function(e) NA_character_)
  prov <- attr(acs_sf, "cacs_provenance") %||%
    attr(acs_sf, "cacs_acs_provenance")
  prov_keep <- if (is.list(prov)) {
    prov[intersect(names(prov), c(
      "state", "year", "survey", "geography", "variables",
      "cache_key", "cache_namespace", "acs_geometry_vintage"
    ))]
  } else {
    list()
  }
  digest::digest(
    list(
      schema = "acs-content-v1",
      crs = crs_id,
      rows = attrs_keep,
      provenance = prov_keep
    ),
    algo = "sha256",
    serialize = TRUE
  )
}

# The key part for acs_sf combines its recorded key with the checksum of its
# contents, so that a subset or an edited copy of a cacs_acs_prefetch()
# result does not get the key of the whole result (iso_sf gets the same kind
# of key, see .extract_iso_content_aware_cache_key()).
.extract_acs_content_aware_cache_key <- function(acs_sf) {
  digest::digest(
    list(
      upstream_cache_key = .extract_cache_key(acs_sf),
      acs_content_hash = .extract_acs_content_hash(acs_sf)
    ),
    algo = "sha256",
    serialize = TRUE
  )
}

# The key part for iso_sf, built like the one for acs_sf: its recorded key
# (NA when there is none) with the checksum of its areas, so that a subset or
# an edited copy of a cacs_isochrone() result does not get the key of the
# whole result.
.extract_iso_content_aware_cache_key <- function(iso_sf) {
  prov <- attr(iso_sf, "cacs_isochrone_provenance") %||%
    attr(iso_sf, "cacs_provenance")
  recorded <- if (is.list(prov) && is.character(prov$cache_key) &&
                  length(prov$cache_key) == 1L && nzchar(prov$cache_key)) {
    prov$cache_key
  } else {
    NA_character_
  }
  digest::digest(
    list(
      upstream_cache_key = recorded,
      iso_content_hash = .extract_iso_content_hash(iso_sf)
    ),
    algo = "sha256",
    serialize = TRUE
  )
}

# ring_topology of iso_sf for the cache key: the distinct values of the
# column, else the value in the cacs_isochrone_provenance (or
# cacs_provenance) attribute, else "unknown".
.extract_iso_ring_topology <- function(iso_sf) {
  if (!is.null(iso_sf) && "ring_topology" %in% names(iso_sf)) {
    vals <- sort(unique(as.character(stats::na.omit(iso_sf$ring_topology))))
    if (length(vals) > 0L) return(paste(vals, collapse = "|"))
  }
  prov <- attr(iso_sf, "cacs_isochrone_provenance") %||%
    attr(iso_sf, "cacs_provenance")
  val <- if (is.list(prov)) prov$ring_topology else NULL
  if (is.character(val) && length(val) >= 1L && nzchar(val[[1L]])) {
    return(paste(sort(unique(val)), collapse = "|"))
  }
  "unknown"
}

# The OSRM res of iso_sf for the cache key, as a string, from the
# cacs_isochrone_provenance (or cacs_provenance) attribute or else the
# cacs_res_param attribute; "unknown" when none has it.
.extract_iso_res_param <- function(iso_sf) {
  prov <- attr(iso_sf, "cacs_isochrone_provenance") %||%
    attr(iso_sf, "cacs_provenance")
  val <- if (is.list(prov)) prov$res_param %||% prov$res else NULL
  if (is.null(val)) {
    val <- attr(iso_sf, "cacs_res_param", exact = TRUE)
  }
  if (is.null(val) || length(val) == 0L || is.na(val[[1L]])) {
    return("unknown")
  }
  as.character(val[[1L]])
}


#' Cache key of a cacs_intersect_weight() call
#'
#' Builds a list of 14 named elements and passes it to
#' `.cacs_cache_key(payload, "intersect")`, which checks the number of
#' elements against `.CACS_CACHE_NAMESPACE_DIMS` and returns a SHA-256
#' checksum. The elements are the key and content checksum of `iso_sf`
#' (`.extract_iso_content_aware_cache_key()`), the key and content checksum of
#' `acs_sf`, two checksums of `bg_pop_sf` (`NA` when it is `NULL`),
#' `weight_method`, `min_weight`, the layout version `"1.0"`, the versions of
#' catchmentACS, sf, GEOS, PROJ, and R, and the ring topology and OSRM `res`
#' of `iso_sf`. A change in any of them gives a different key, including a
#' subset or an edited copy of a `cacs_isochrone()` result that keeps its
#' recorded `cache_key`.
#'
#' @param iso_sf,acs_sf,bg_pop_sf,weight_method,min_weight The arguments of
#'   the same names of `cacs_intersect_weight()`.
#' @return A single string.
#' @keywords internal
#' @noRd
.cache_key_intersect <- function(iso_sf,
                                  acs_sf,
                                  bg_pop_sf,
                                  weight_method,
                                  min_weight) {

  payload <- list(
    iso_cache_key    = .extract_iso_content_aware_cache_key(iso_sf),
    acs_cache_key    = .extract_acs_content_aware_cache_key(acs_sf),
    bg_pop_geom_hash = if (!is.null(bg_pop_sf)) {
      digest::digest(sf::st_coordinates(bg_pop_sf), algo = "sha256")
    } else {
      NA_character_
    },
    bg_pop_pop_hash  = if (!is.null(bg_pop_sf) && "population_est" %in% names(bg_pop_sf)) {
      digest::digest(bg_pop_sf$population_est, algo = "sha256")
    } else {
      NA_character_
    },
    weight_method    = weight_method,
    min_weight       = min_weight,
    schema_version   = "1.0",
    package_version  = as.character(utils::packageVersion("catchmentACS")),
    sf_version       = as.character(utils::packageVersion("sf")),
    geos_version     = as.character(sf::sf_extSoftVersion()[["GEOS"]]),
    proj_version     = as.character(sf::sf_extSoftVersion()[["PROJ"]]),
    r_version        = paste(R.version$major, R.version$minor, sep = "."),
    iso_ring_topology = .extract_iso_ring_topology(iso_sf),
    iso_res_param    = .extract_iso_res_param(iso_sf)
  )                                                                  # 14 items

  .cacs_cache_key(payload, namespace = "intersect")
}


#' Whether the intersection cache is used
#'
#' Decides both the lookup and the saving of results in
#' `cacs_intersect_weight()`. `.cacs_cache_enabled("intersect")` (R/cache.R)
#' answers on its own for every setting that matters: `CACS_NO_CACHE`,
#' `CACS_CACHE_ENABLED`, and `options(catchmentACS.cache_enabled = )` first,
#' then `options(catchmentACS.cache_intersect = )` when it is `TRUE` or
#' `FALSE`, then `CACS_CACHE_INTERSECT`, which turns the cache on only for
#' `1`, `true`, `yes`, or `on`. Without any of them the cache is on. The
#' option branch below catches an option that is not `TRUE` or `FALSE`, such
#' as `"no"`, and turns the cache off. The `CACS_CACHE_INTERSECT` branch
#' below is never reached with a value: any value that gets past the call
#' above has already turned the cache on.
#'
#' @return `TRUE` or `FALSE`.
#' @keywords internal
#' @noRd
.cacs_cache_intersect_enabled <- function() {
  if (!.cacs_cache_enabled("intersect")) return(FALSE)
  opt <- getOption("catchmentACS.cache_intersect", NULL)
  if (!is.null(opt)) return(isTRUE(opt))
  envv <- Sys.getenv("CACS_CACHE_INTERSECT", unset = NA)
  if (!is.na(envv) && nzchar(envv)) {
    return(!identical(tolower(envv), "false") &&
             !identical(envv, "0"))
  }
  TRUE
}


#' Default for NULL
#'
#' `x %||% y` is `y` when `x` is `NULL` and `x` otherwise, like the operators
#' of the same name in rlang and, from R 4.4.0, in base R. The package defines
#' its own because NAMESPACE imports nothing from rlang (only two functions
#' from stats) and the package supports R from version 4.1.0. The same
#' definition is also in R/isochrone-dispatch.R, R/isochrone-normalize.R,
#' R/isochrone-ors.R, and R/plot-site.R; the copies are identical, so it does
#' not matter which one the package namespace keeps.
#'
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
