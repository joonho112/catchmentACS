# cacs_run(), which runs the five steps of the package in order, and the
# helpers it uses; the methods for its result (group_by(), print(), summary(),
# as_tibble()); and cacs_summary_as_markdown().


#' Compute ACS estimates for drive-time areas around sites
#'
#' Builds drive-time areas around each site, one for each drive time, and
#' combines the American Community Survey (ACS) estimates of the census
#' tracts that overlap each area. These areas are also called isochrones.
#' Returns a table with an estimate and its margin of error (the half-width
#' of a confidence interval) for every site, drive time, and variable, plus
#' rows for five rates such as the poverty rate
#' ([`cacs_acs_default_rates`]). The function runs
#' [cacs_acs_prefetch()], [cacs_isochrone()], [cacs_intersect_weight()],
#' [cacs_propagate_moe()], and [cacs_derive_rates()] in that order; each of
#' them can also be called on its own. Supplying `precomputed_isochrones` or
#' `acs` skips the corresponding step, and with both supplied no routing
#' service or Census API key is needed.
#'
#' Margins of error are at the 90 percent level unless `moe_args` sets
#' another `level`, and they treat the tract estimates as independent (see
#' [cacs_propagate_moe()]). By default, every rate uses the formula that the
#' Census Bureau's handbook gives for a ratio of two estimates. The five
#' rates are proportions, for which the handbook gives a separate formula
#' whose margins are never wider; `formula_dispatch` can select it for
#' `poverty_rate` and `labor_force_participation`.
#'
#' Site and drive-time pairs for which the routing service returns no area
#' stay in the result with `NA` values and `failure_origin = "isochrone"`
#' (`failure_origin` records the step at which a row failed), and a warning
#' reports how many pairs failed. A routing failure stops the function in two
#' cases: no pair succeeds, or the routing service refuses a request because
#' its request limit has been reached or an API key is not accepted.
#'
#' @references U.S. Census Bureau (2020). *Understanding and Using American
#'   Community Survey Data: What All Data Users Need to Know*. Chapter 8,
#'   Calculating Measures of Error for Derived Estimates.
#'
#' @param sites A data frame with the columns `site_id`, `lon`, and `lat`,
#'   or an `sf` object of points with a `site_id` column, such as
#'   [`cacs_alabama_sites`]. The coordinates are longitude and latitude on
#'   WGS 84 (an `sf` object must be in EPSG:4326), and the `site_id` values
#'   must be unique.
#' @param state A string giving one state, the District of Columbia, or
#'   Puerto Rico, as a two-letter USPS abbreviation (such as `"AL"`) or a
#'   two-digit FIPS code (such as `"01"`). Lowercase codes, such as `"al"`,
#'   give an error unless the ACS data are supplied through `acs`. Only the
#'   tracts of this state are downloaded, so the part of a drive-time area in
#'   another state adds nothing to the estimates unless `acs` also holds the
#'   tracts of that state (see the Details of [cacs_intersect_weight()],
#'   which also say what happens to `acs_year`). Data for Alaska, Hawaii, or
#'   Puerto Rico give an error at the intersection step.
#' @param year A single whole number giving the last year of the ACS 5-year
#'   estimates, from 2009 to 2024; the default is 2023 (the 2019-2023
#'   estimates).
#' @param drive_times A numeric vector of drive times in minutes; the default
#'   is `c(5, 10, 15)`. When the areas are built, up to six whole numbers
#'   from 1 to 60 are accepted.
#' @param variables A character vector of ACS variable codes, or `NULL` (the
#'   default) for [`cacs_acs_default_vars`]; see [cacs_acs_prefetch()] for
#'   the accepted codes. Codes outside [`cacs_acs_default_vars`] are
#'   combined as counts, except those of tables `B19013` and `B25077`
#'   (medians) and `B19301` (per capita income), so a median or per-person
#'   value from any other table is added up over the tracts like a count,
#'   without a warning (see [cacs_intersect_weight()]).
#'   Rates whose codes in [`cacs_acs_default_rates`] are left out are `NA`,
#'   with a warning.
#' @param provider A string giving the routing service used to build the
#'   drive-time areas. `"osrm"` (the default) uses the Open Source Routing
#'   Machine (OSRM) through a public server that needs no key; it requires
#'   the osrm package. `"ors"` uses openrouteservice, which requires the
#'   openrouteservice package and an API key in the `ORS_API_KEY`
#'   environment variable or in `iso_args = list(ors_api_key = ...)`.
#'   `"mapbox"` and `"r5r"` are accepted names, but building areas with them
#'   is not implemented yet. Using them gives an error at the routing step,
#'   so the ACS data are downloaded first unless `acs` is supplied. When
#'   `precomputed_isochrones` is supplied, no areas are built, and `provider`
#'   is only recorded in the `cacs_run_provenance` attribute and shown when
#'   the result is printed.
#' @param weight_method A string giving the weighting method: `"area"` (the
#'   default) or `"population"`. With `"area"`, counts are added up with each
#'   tract weighted by the share of its area inside the drive-time area,
#'   which assumes that whatever a variable counts is spread evenly over each
#'   tract's area. The rates are ratios of these counts. Medians and
#'   per-person values are averaged with weights proportional to each tract's
#'   area inside the drive-time area. These weights ignore how many people
#'   live in each tract, so the average can be far from the value for the
#'   drive-time area when the tracts differ in population density (see
#'   [cacs_intersect_weight()]). `"population"` is not implemented yet and
#'   gives an error before any step runs.
#' @param moe_formula A string, or a list of strings named by ACS variable
#'   code, passed to [cacs_propagate_moe()] as its `formula` argument, or
#'   `NULL` (the default). Each ACS variable accepts only the formula for its
#'   kind (`"weighted_sum"` for a count, `"weighted_mean"` for a median or
#'   per-person value), and any other choice gives an error. The formulas for
#'   the rates are chosen by `formula_dispatch`.
#' @param output A string choosing the form of the result, as described in
#'   the Value section: `"long"` (the default), `"list_column"`, or `"both"`.
#' @param precomputed_isochrones An `sf` object of drive-time areas as
#'   returned by [cacs_isochrone()] (see [cacs_validate_iso()] for data from
#'   other sources), or `NULL` (the default) to build them with `provider`.
#'   When supplied, the routing step is skipped and every area in it is used.
#'   `sites` and `drive_times` do not select areas from it; they are
#'   recorded in the `cacs_run_provenance` attribute and shown when the
#'   result is printed. A site in `sites` that has no area in it gets no rows
#'   and no warning. If `precomputed_isochrones` has areas for sites that are
#'   not in `sites`, the numbers of sites shown when the result is printed
#'   are wrong (see [print.cacs_run_result()]). Areas with
#'   `isochrone_empty = TRUE` or a `failure_reason` are treated as routing
#'   failures (see Details).
#' @param cache_dir A path to the cache folder, or `NULL` (the default) to use
#'   [cacs_cache_dir()]. It is used for the ACS data, drive-time areas, and
#'   intersection results that [cacs_acs_prefetch()], [cacs_isochrone()], and
#'   [cacs_intersect_weight()] save. A folder outside the temporary folder of
#'   the R session is tidied as described in [cacs_cache_dir()].
#' @param acs An `sf` object of ACS tract estimates as returned by
#'   [cacs_acs_prefetch()] (see [cacs_acs_validate()] for data from other
#'   sources), or `NULL` (the default) to download them with tidycensus;
#'   downloading needs a Census API key (see [cacs_acs_prefetch()]). When
#'   supplied, the download step is skipped, every variable in it is used
#'   (`variables` is ignored), and `state` and `year` are only recorded in
#'   the `cacs_run_provenance` attribute. Census Bureau annotation codes, such
#'   as `-555555555`, are treated as missing, so a rate that uses such a value
#'   is `NA` for the areas that include the tract, and two rows for the same
#'   tract and variable give an error (see [cacs_intersect_weight()], which
#'   also says when a warning is given). When a tract has a row for one code
#'   of a rate and not for the other, for example after rows with a missing
#'   estimate were removed, the rate is computed over different tracts
#'   without a warning (see the "Tract counts for rates" section of
#'   [cacs_derive_rates()]). Variable codes that
#'   [cacs_intersect_weight()] does not combine, such as `"C17002_002"` or
#'   `"B19013A_001"`, make [cacs_propagate_moe()] stop with an error.
#' @param bg_pop_sf An `sf` object of block-group population estimates, or
#'   `NULL` (the default). It has no effect on the result (see
#'   `weight_method`).
#' @param rates A named list giving the numerator and denominator ACS codes
#'   of each rate. Only [`cacs_acs_default_rates`] (the default), the list
#'   of the five built-in rates, is accepted; any other list, including a
#'   subset or a reordering of it, gives an error.
#' @param formula_dispatch A string choosing the margin-of-error formula for
#'   the five rates. `"general_ratio_conservative"` (the default) uses the
#'   ratio formula for every rate. `"auto"` and `"proportion_subset"` use the
#'   proportion formula for `poverty_rate` and `labor_force_participation`
#'   and keep the ratio formula for `snap_rate`, `ssi_rate`, and
#'   `unemp_rate`; `"proportion_subset"` also gives a warning naming those
#'   three.
#' @param iso_args A named list of arguments passed on to [cacs_isochrone()]
#'   when the areas are built, or an empty list (the default). The accepted
#'   names are `osrm_mode`, `ors_api_key`, `mapbox_token`, `r5r_core`,
#'   `profile`, `osm_snapshot_date`, and `res`; other names give a warning
#'   and are dropped. Because `osrm.server` is not accepted, another OSRM
#'   server is used by setting `osrm_mode = "docker"` and the option
#'   `catchmentACS.osrm_docker_server` (see the "OSRM servers" section of
#'   [cacs_isochrone()]).
#' @param acs_args A named list of arguments passed on to
#'   [cacs_acs_prefetch()] when `acs` is `NULL`, or an empty list (the
#'   default). The accepted names are `survey`, `geography`, `force_refresh`,
#'   and `write_gpkg`; other names give a warning and are dropped.
#' @param weight_args A named list of arguments passed on to
#'   [cacs_intersect_weight()], or an empty list (the default). The accepted
#'   names are `min_weight` and `keep_tract_audit`; other names give a
#'   warning and are dropped.
#' @param moe_args A named list of arguments passed on to
#'   [cacs_propagate_moe()], or an empty list (the default). The accepted
#'   names are `level`, which sets the confidence level of all the margins of
#'   error (0.9 by default), and `fallback_chain_max`, which has no effect
#'   and gives a warning (see [cacs_propagate_moe()]). The name `formula`
#'   gives an error
#'   (`moe_formula` sets it), and other names give a warning and are dropped.
#' @param rate_args A named list of arguments for [cacs_derive_rates()], or
#'   an empty list (the default). No names are accepted: `formula_dispatch`
#'   gives an error (the argument `formula_dispatch` sets it), and any other
#'   name gives a warning and is dropped.
#' @param verbose A logical value, passed to each step that `cacs_run()`
#'   runs. If `TRUE` (the default), messages report which steps run and how
#'   each step progresses; see the "Progress messages" section.
#'
#' @return With `output = "long"` (the default), a tibble that also has the
#'   class `cacs_run_result`, with one row for each site, drive time, and
#'   variable, where the variable is an ACS variable or one of the five
#'   rates. With `output = "list_column"`, a tibble of the same class with
#'   one row for each site and drive time. With `output = "both"`, a plain
#'   list with the elements `long` and `list_column`, one of each form.
#'
#'   The long form has these columns:
#'   \describe{
#'     \item{`site_id`, `drive_time_min`, `variable`}{The site, the drive
#'       time in minutes, and the ACS variable code or rate name.}
#'     \item{`ring_topology`}{Always `"cumulative"`: each drive-time area
#'       contains the shorter ones.}
#'     \item{`estimate`, `moe`}{The estimate and its margin of error, at the
#'       confidence level given by the `cacs_confidence_level` attribute. For
#'       an ACS variable, both are `NA` when a tract's estimate is missing,
#'       and `moe` is `NA` when a tract's margin of error is missing.}
#'     \item{`weight_sum`}{The sum of the coverage weights of the tracts
#'       combined (a tract's coverage weight is the share of its area inside
#'       the drive-time area); for a rate, the smaller of the sums for its
#'       numerator and denominator, and `NA` on a rate row whose
#'       `failure_origin` is `"carrier"`.}
#'     \item{`n_tracts`}{The number of tracts combined; `NA` on rate rows.}
#'     \item{`n_tracts_num`, `n_tracts_den`}{On rate rows, the numbers of
#'       tracts combined for the numerator and for the denominator; `NA` on
#'       other rows and on a rate row whose `failure_origin` is `"carrier"`
#'       (see [cacs_derive_rates()]).}
#'     \item{`provider`, `profile`, `osm_snapshot_date`}{The routing service,
#'       the routing profile (such as `"car"`), and the date of the
#'       OpenStreetMap data, as recorded on the drive-time areas.}
#'     \item{`acs_year`}{The ACS year from the `cacs_provenance` attribute
#'       that [cacs_acs_prefetch()] puts on the ACS data; `NA` when the data
#'       have no such attribute, as with the sample ACS data in the package.}
#'     \item{`weight_method`, `weight_basis`}{The weighting method (`"area"`)
#'       and the weights used (see the `weight_method` argument): `"coverage"`
#'       for counts and rates, and `"area_mean"` for medians and per-person
#'       values.}
#'     \item{`estimand_family`}{The kind of quantity: `"spatial_total"` (a
#'       count), `"median_proxy"` (a median), `"area_weighted_scalar_proxy"`
#'       (a per-person value such as per capita income), or `"derived_rate"`
#'       (a rate).}
#'     \item{`moe_formula_requested`, `moe_formula_effective`}{The
#'       margin-of-error formula chosen, through `moe_formula` or
#'       `formula_dispatch` (see [cacs_derive_rates()] for the rates), and the
#'       one used: `"weighted_sum"`, `"weighted_mean"`, `"proportion_subset"`
#'       (the proportion formula), or `"general_ratio_conservative"` (the
#'       ratio formula). A rate row whose `failure_origin` is `"carrier"` has
#'       no margin of error, and both columns then name the formula that was
#'       chosen for it rather than one that was used.}
#'     \item{`moe_fallback`, `moe_fallback_reason`}{For a rate, whether the
#'       chosen formula could not be used, and why: `"negative_variance"`
#'       (the ratio formula was used instead) or `"zero_denominator"` (the
#'       rate and its margin of error are `NA`); `"n/a"` otherwise.}
#'     \item{`failure_origin`}{The step at which the row failed: `"none"`;
#'       `"isochrone"` (the routing service returned no area; the row's
#'       estimates and most other values are `NA`); `"intersection"` (no
#'       tract is left for the area; the pair has one such row, with
#'       `variable = NA` and `n_tracts = 0`); or `"carrier"` (the rate is
#'       `NA` because its numerator or denominator, or the margin of error of
#'       either, is missing, as for the rates of a pair with no tract).}
#'     \item{`weight_uncertainty_propagated`}{`FALSE`: the margins of error
#'       treat the weights as fixed.}
#'   }
#'   The columns `est_total`, `var_total_raw`, `est_mean`, and
#'   `var_mean_raw` hold the weighted sums and averages of the tract
#'   estimates, with their variances, from which `estimate` and `moe` are
#'   computed (see [cacs_propagate_moe()]). They are `NA` on rate rows. With
#'   `options(cacs.return_se = TRUE)`, a column `se` (the standard error)
#'   follows; it is also `NA` on rate rows.
#'
#'   The list-column form has these columns:
#'   \describe{
#'     \item{`site_id`}{The site.}
#'     \item{`lon`, `lat`}{The values of the columns of the same names in
#'       `sites`, or `NA` when `sites` has no such columns, even if it is an
#'       `sf` object such as [`cacs_alabama_sites`]; the point coordinates
#'       are not used. Reading the missing columns from an `sf` object gives
#'       two warnings, `Unknown or uninitialised column: 'lon'` and the same
#'       for `'lat'`; the values are `NA` and the rest of the result is
#'       unaffected.}
#'     \item{`drive_time_min`}{The drive time in minutes.}
#'     \item{`isochrone`}{The drive-time area, as a one-row `sf` object.}
#'     \item{`acs_estimates`, `derived_rates`}{Tibbles of the rows of the long
#'       result for that site and drive time: the ACS variables and the
#'       rates.}
#'     \item{`metadata`}{A list of `provider`, `profile`, `osm_snapshot_date`,
#'       `acs_year`, `weight_method`, `failure_origin`, and `n_tracts`, taken
#'       from the first row of `acs_estimates`.}
#'   }
#'
#'   Both forms have these attributes:
#'   \describe{
#'     \item{`cacs_schema_version`}{The version label (`"1.0"`) of the
#'       column layout.}
#'     \item{`cacs_run_provenance`}{A list recording how the result was
#'       produced. It gives the settings of the call: `state`, `year`,
#'       `drive_times`, `provider`, `weight_method`, `moe_formula`,
#'       `formula_dispatch`, `output_format`, `level` (the confidence
#'       level), and `min_weight`. It records which steps were skipped
#'       because their input was supplied, in `bypass_iso`, `bypass_acs`,
#'       and `execution_path`. It also holds counts of sites and of pairs
#'       that succeeded or failed, the time of the run, and the package and
#'       R versions.}
#'     \item{`cacs_run_warnings`}{A list of the warnings given by each step,
#'       in the elements `acs_prefetch`, `isochrone`, `intersect_weight`,
#'       `propagate_moe`, and `derive_rates`; when pairs failed at the
#'       routing step, the element `orchestrator` has an entry whose
#'       `failed_pairs` lists them, with the reason.}
#'     \item{`cacs_aggregation_provenance`, `cacs_moe_provenance`,
#'       `cacs_rate_provenance`}{The settings and counts recorded by
#'       [cacs_intersect_weight()], [cacs_propagate_moe()], and
#'       [cacs_derive_rates()]; the last includes the formula chosen for each
#'       rate.}
#'     \item{`cacs_confidence_level`}{The confidence level of the margins of
#'       error (0.9 unless `moe_args` sets another `level`).}
#'     \item{`cacs_run_result_metadata`}{A list that `print()` and
#'       `summary()` use: the time the result was created, the package
#'       version, the routing service and profile, and the drive times. It
#'       also holds counts of sites, the running time
#'       (`wall_clock_seconds`), `skipped_geoids`, and counts of rate rows
#'       by `moe_fallback_reason` (`moe_fallback_summary`).}
#'   }
#'   The long form also keeps `skipped_geoids`, the tracts skipped because
#'   they have no area (see [cacs_intersect_weight()]), and
#'   `cacs_rate_audit`, the result of the check of each rate against a fixed
#'   range that `options(catchmentACS.audit_rates = TRUE)` turns on (see
#'   [cacs_derive_rates()]). With
#'   `weight_args = list(keep_tract_audit = TRUE)`, it also keeps
#'   `cacs_tract_audit`, a table of the tracts in each area with their
#'   coverage weights. [cacs_describe()] prints a summary drawn from these
#'   attributes.
#'
#' @section Progress messages:
#' The five steps report their progress with messages, whether they are run
#' by `cacs_run()` or called on their own. Each step counts its work in
#' units: sites for [cacs_isochrone()], site and drive-time pairs for
#' [cacs_intersect_weight()], and a single unit for the other three steps. A
#' step can show a summary line when it finishes, with how many units
#' succeeded out of how many, the time taken, and any failures. It can also
#' show a progress line as each unit finishes. An example is
#' \code{Intersect+weight: 21/60 (ETA 0:01) - AL_SITE_07/15min}, where `ETA`
#' is the estimated time left. [cacs_propagate_moe()] and
#' [cacs_derive_rates()] show at most the summary line.
#'
#' The first of these rules that applies decides what a step shows:
#'
#' 1. With `verbose = FALSE`, no progress messages.
#' 2. If the environment variable `CACS_QUIET` is `"1"` (other values are
#'    ignored), no progress messages.
#' 3. If `getOption("catchmentACS.progress")` is `"off"`, no progress
#'    messages; if it is `"force"`, the summary line and the progress lines,
#'    whatever the number of units.
#' 4. Otherwise, as with `"auto"` or when the option is not set (the
#'    default), the summary line alone for fewer than five units, and the
#'    summary line and the progress lines for five or more.
#'
#' With more than 50 units, only the first progress line, every tenth line
#' after it, and the last line are shown;
#' `options(catchmentACS.progress_throttle = k)` changes ten to `k`. A step
#' whose result is read from the cache shows no progress messages. These
#' settings do not affect warnings.
#'
#' With `verbose = TRUE`, `cacs_run()` also shows messages of its own. A
#' message at the start says which steps will run, and one message is shown
#' for each step skipped because `acs` or `precomputed_isochrones` was
#' supplied. At the end, a message gives the numbers of site and drive-time
#' pairs that succeeded and failed. In the same way,
#' [cacs_intersect_weight()] shows a message before it starts, and
#' [cacs_acs_prefetch()] one before it downloads. Rules 2 and 3 leave these
#' messages on; `verbose = FALSE` turns them off. With
#' `output = "list_column"` or `"both"`, a message about the `isochrone`
#' column is shown on every call, whatever `verbose` is.
#'
#' All of these are ordinary R messages, so `suppressMessages()` also hides
#' them. Progress lines have the condition class
#' `catchmentACS_message_progress_tick` and summary lines the class
#' `catchmentACS_message_progress_summary`. Both also have the class
#' `catchmentACS_message_progress`, as do the messages of `cacs_run()` and
#' [cacs_intersect_weight()] that `verbose = FALSE` turns off, so
#' [cacs_capture_conditions()] with that class in `classes` collects all of
#' them. The condition classes of the package are listed in
#' [`catchmentACS-conditions`].
#'
#' @seealso The help pages [print.cacs_run_result()],
#'   [summary.cacs_run_result()], and [as_tibble.cacs_run_result()] describe
#'   how the result is printed, summarized, and converted to a plain tibble.
#'   `vignette("getting-started", package = "catchmentACS")`
#'   (<https://joonho112.github.io/catchmentACS/articles/getting-started.html>)
#'   walks through a first run, and
#'   `vignette("methodology", package = "catchmentACS")`
#'   (<https://joonho112.github.io/catchmentACS/articles/methodology.html>)
#'   describes the calculations.
#' @family steps of the calculation
#' @export
#' @examples
#' # Turn the cache off while this example runs (see ?cacs_set_cache).
#' old <- options(catchmentACS.cache_enabled = FALSE)
#'
#' # Example data bundled with the package: the drive-time areas are circles
#' # with a radius of 1 km per minute, and the ACS data are made up. The
#' # routing columns of the areas, such as provider, hold fixed values that
#' # do not come from a routing service, and the result repeats some of them.
#' library(sf)
#' iso <- readRDS(system.file("extdata", "legacy_2025_isochrones.rds",
#'                            package = "catchmentACS"))
#' acs <- readRDS(system.file("extdata", "sample_alabama_subset.rds",
#'                            package = "catchmentACS"))
#' iso_07 <- iso[iso$site_id == "AL_SITE_07" & iso$drive_time_min == 10, ]
#' # The site at the center of these areas
#' site_07 <- data.frame(site_id = "AL_SITE_07", lon = -85.365, lat = 31.655)
#'
#' # With both the drive-time areas and the ACS data supplied, no routing
#' # service, Census API key, or internet connection is needed.
#' out <- cacs_run(site_07, state = "AL", drive_times = 10,
#'                 precomputed_isochrones = iso_07, acs = acs,
#'                 verbose = FALSE)
#'
#' # The five rates for the 10-minute area of AL_SITE_07
#' tibble::as_tibble(out) |>
#'   dplyr::filter(variable %in% names(cacs_acs_default_rates)) |>
#'   dplyr::select(variable, estimate, moe)
#'
#' options(old)
#'
#' # Needs a Census API key, and builds the areas of five sites on the public
#' # OSRM demo server, which limits the requests it accepts.
#' \dontrun{
#' library(sf)
#' out_live <- cacs_run(cacs_alabama_sites[1:5, ], state = "AL", year = 2023,
#'                      drive_times = c(5, 10, 15), provider = "osrm")
#'
#' # The poverty rate for each site and drive time, highest first
#' tibble::as_tibble(out_live) |>
#'   dplyr::filter(variable == "poverty_rate") |>
#'   dplyr::arrange(dplyr::desc(estimate)) |>
#'   dplyr::select(site_id, drive_time_min, estimate, moe)
#' }
cacs_run <- function(sites,
                     state,
                     year = 2023,
                     drive_times = c(5, 10, 15),
                     variables = NULL,
                     provider = c("osrm", "ors", "mapbox", "r5r"),
                     weight_method = c("area", "population"),
                     moe_formula = NULL,
                     output = c("long", "list_column", "both"),
                     precomputed_isochrones = NULL,
                     cache_dir = NULL,
                     acs = NULL,
                     bg_pop_sf = NULL,
                     rates = cacs_acs_default_rates,
                     formula_dispatch = "general_ratio_conservative",
                     iso_args = list(),
                     acs_args = list(),
                     weight_args = list(),
                     moe_args = list(),
                     rate_args = list(),
                     verbose = TRUE) {

  # Argument checks. These cover types and simple conditions only. The values
  # of `sites`, `drive_times`, `state`, `year`, and `variables` are checked by
  # cacs_isochrone() and cacs_acs_prefetch(), which use them; checking them
  # here as well would duplicate those checks, and the two copies could drift
  # apart. When a step is skipped because its input is supplied, only the
  # checks here apply to its arguments.

  output        <- match.arg(output)
  provider      <- match.arg(provider)
  weight_method <- match.arg(weight_method)

  # Start time for `wall_clock_seconds` in the cacs_run_result_metadata
  # attribute.
  run_start <- Sys.time()

  # An sf object is also a data frame, so this one check accepts both forms.
  if (!inherits(sites, c("sf", "tbl_df", "data.frame"))) {
    .cli_abort_schema(c(
      "{.arg sites} must be an {.cls sf} or {.cls tbl_df}/{.cls data.frame}.",
      "x" = "Got {.cls {class(sites)[[1L]]}}.",
      "i" = "Pass {.code cacs_alabama_sites} or a tibble/data.frame with {.field site_id}, {.field lon}, {.field lat}."
    ))
  }

  # state: one non-empty string. cacs_acs_prefetch() checks the value; when
  # `acs` is supplied, it is only recorded.
  if (!is.character(state) || length(state) != 1L || is.na(state) ||
      !nzchar(state)) {
    .cli_abort_schema(c(
      "{.arg state} must be a non-empty scalar {.cls character}.",
      "x" = "Got {.cls {class(state)[[1L]]}} of length {.val {length(state)}}.",
      "i" = "Use a USPS code (e.g. {.val AL}) or 2-digit FIPS (e.g. {.val 01})."
    ))
  }

  # year: one finite number. cacs_acs_prefetch() checks the range, but it is
  # not called when `acs` is supplied, and `year` is recorded with
  # as.integer() in either case, so the type is checked here.
  if (!is.numeric(year) || length(year) != 1L || is.na(year) ||
      !is.finite(year)) {
    .cli_abort_schema(c(
      "{.arg year} must be a finite scalar {.cls numeric}.",
      "x" = "Got {.cls {class(year)[[1L]]}} of length {.val {length(year)}}."
    ))
  }

  # drive_times: positive finite numbers. cacs_isochrone() applies its own
  # limits when it builds the areas.
  if (!is.numeric(drive_times) || length(drive_times) == 0L ||
      anyNA(drive_times) || any(!is.finite(drive_times)) ||
      any(drive_times <= 0)) {
    .cli_abort_schema(c(
      "{.arg drive_times} must be a positive finite {.cls numeric} vector.",
      "i" = "When {.fn cacs_run} builds the areas, it accepts up to six whole numbers from 1 to 60."
    ))
  }

  if (!is.null(precomputed_isochrones) &&
      !inherits(precomputed_isochrones, "sf")) {
    .cli_abort_schema(c(
      "{.arg precomputed_isochrones} must be an {.cls sf} or {.code NULL}.",
      "x" = "Got {.cls {class(precomputed_isochrones)[[1L]]}}.",
      "i" = "Pass the result of {.fn cacs_isochrone}; {.fn cacs_run} then skips the routing step."
    ))
  }

  if (!is.null(acs) && !inherits(acs, "sf")) {
    .cli_abort_schema(c(
      "{.arg acs} must be an {.cls sf} or {.code NULL}.",
      "x" = "Got {.cls {class(acs)[[1L]]}}.",
      "i" = "Pass ACS data that pass {.fn cacs_acs_validate}; {.fn cacs_run} then skips the download."
    ))
  }

  # weight_method = "population" is not implemented. Stop here, before any
  # download or routing request, rather than after them.
  if (identical(weight_method, "population")) {
    .cli_abort_credential(c(
      "{.field weight_method} = {.val population} is deferred to a future release.",
      "i" = "Use {.val area} in the current release.",
      "i" = "No routing or ACS request was sent."
    ))
  }

  # The five *_args arguments must be lists; the names in them are checked by
  # .validate_run_arg_list() below.
  if (!is.list(iso_args)) {
    .cli_abort_schema(c(
      "{.arg iso_args} must be a {.cls list}.",
      "x" = "Got {.cls {class(iso_args)[[1L]]}}."
    ))
  }
  if (!is.list(acs_args)) {
    .cli_abort_schema(c(
      "{.arg acs_args} must be a {.cls list}.",
      "x" = "Got {.cls {class(acs_args)[[1L]]}}."
    ))
  }
  if (!is.list(weight_args)) {
    .cli_abort_schema(c(
      "{.arg weight_args} must be a {.cls list}.",
      "x" = "Got {.cls {class(weight_args)[[1L]]}}."
    ))
  }
  if (!is.list(moe_args)) {
    .cli_abort_schema(c(
      "{.arg moe_args} must be a {.cls list}.",
      "x" = "Got {.cls {class(moe_args)[[1L]]}}."
    ))
  }
  if (!is.list(rate_args)) {
    .cli_abort_schema(c(
      "{.arg rate_args} must be a {.cls list}.",
      "x" = "Got {.cls {class(rate_args)[[1L]]}}."
    ))
  }

  # The execution path records which steps run, given the inputs supplied:
  # "5-call" (neither), "4-call-A" (precomputed_isochrones), "4-call-B"
  # (acs), or "3-call" (both). It is shown in a message and recorded in the
  # cacs_run_provenance attribute.
  has_iso <- !is.null(precomputed_isochrones)
  has_acs <- !is.null(acs)
  execution_path <- if      (has_iso &&  has_acs) "3-call"
                    else if (has_iso && !has_acs) "4-call-A"
                    else if (!has_iso && has_acs) "4-call-B"
                    else                          "5-call"

  if (isTRUE(verbose)) {
    step_text <- switch(execution_path,
      "5-call"   = "all five steps",
      "4-call-A" = "every step but the routing step",
      "4-call-B" = "every step but the ACS download",
      "3-call"   = "every step but the routing step and the ACS download"
    )
    .cli_inform_progress(
      c("i" = "Running {step_text} ({.field execution_path} {.val {execution_path}})."),
      phase = "run"
    )
  }

  # Names in the *_args lists. `moe_args$formula` and
  # `rate_args$formula_dispatch` give an error because cacs_run() has its own
  # arguments for them (`moe_formula`, `formula_dispatch`); passing them on
  # would allow two values for one setting in the same call. Other names not
  # in .CACS_RUN_ARG_KEYS give a warning and are dropped. These warnings come
  # before the record of warnings below is created, so they are not in the
  # cacs_run_warnings attribute.
  if ("formula" %in% names(moe_args)) {
    .cli_abort_schema(c(
      "{.arg moe_args$formula} cannot be used.",
      "x" = "Pass {.arg moe_formula} at the top level of {.fn cacs_run} instead.",
      "i" = "See {.arg moe_args} in {.help cacs_run} for the names it accepts."
    ))
  }
  if ("formula_dispatch" %in% names(rate_args)) {
    .cli_abort_schema(c(
      "{.arg rate_args$formula_dispatch} cannot be used.",
      "x" = "Pass {.arg formula_dispatch} at the top level of {.fn cacs_run} instead.",
      "i" = "See {.arg rate_args} in {.help cacs_run} for the names it accepts."
    ))
  }

  passthrough_iso <- .validate_run_arg_list(
    iso_args,    "iso", .CACS_RUN_ARG_KEYS$iso
  )
  passthrough_acs <- .validate_run_arg_list(
    acs_args,    "acs", .CACS_RUN_ARG_KEYS$acs
  )
  passthrough_wgt <- .validate_run_arg_list(
    weight_args, "weight", .CACS_RUN_ARG_KEYS$wgt
  )
  passthrough_moe <- .validate_run_arg_list(
    moe_args,    "moe", .CACS_RUN_ARG_KEYS$moe
  )
  passthrough_rat <- .validate_run_arg_list(
    rate_args,   "rate", .CACS_RUN_ARG_KEYS$rat
  )

  # Record of warnings: one element for each of the five steps, and one,
  # `orchestrator`, that gets an entry when some pairs fail at the routing
  # step (below). It is an environment rather than a list so that
  # .call_with_capture() can add to it in place, without every step having to
  # return it. The elements start as empty lists so that
  # c(warning_log$entries[[phase]], ...) needs no check for NULL.
  warning_log <- new.env(parent = emptyenv())
  warning_log$entries <- list(
    isochrone        = list(),
    acs_prefetch     = list(),
    intersect_weight = list(),
    propagate_moe    = list(),
    derive_rates     = list(),
    orchestrator     = list()
  )

  # ACS data: `acs` as supplied, or downloaded by cacs_acs_prefetch(). With
  # `acs` supplied and verbose = TRUE, a message says that the supplied data
  # are used, so the user can see that nothing was downloaded.
  if (!is.null(acs)) {
    acs_resolved <- acs
    if (isTRUE(verbose)) {
      .cli_inform_progress(
        c("i" = "Using the ACS data given in {.arg acs} ({nrow(acs)} row{?s})."),
        phase = "run"
      )
    }
  } else {
    acs_resolved <- .call_with_capture(
      fn          = cacs_acs_prefetch,
      args        = c(
        list(state = state, year = year, variables = variables,
             cache_dir = cache_dir, verbose = verbose),
        passthrough_acs
      ),
      warning_log = warning_log,
      phase       = "acs_prefetch"
    )
  }

  # Drive-time areas: `precomputed_isochrones` as supplied, with a message as
  # for `acs`, or built by cacs_isochrone(). Either way the columns are
  # checked (.validate_iso_schema()) before the next block reads
  # `isochrone_empty` and `failure_reason`.
  if (!is.null(precomputed_isochrones)) {
    iso_resolved <- precomputed_isochrones
    if (isTRUE(verbose)) {
      .cli_inform_progress(
        c("i" = "Using the drive-time areas given in {.arg precomputed_isochrones} ({nrow(precomputed_isochrones)} row{?s})."),
        phase = "run"
      )
    }
  } else {
    iso_resolved <- .call_with_capture(
      fn          = cacs_isochrone,
      args        = c(
        list(sites = sites, drive_times = drive_times,
             provider = provider, cache_dir = cache_dir,
             verbose = verbose),
        passthrough_iso
      ),
      warning_log = warning_log,
      phase       = "isochrone"
    )
  }
  .validate_iso_schema(iso_resolved)

  # Pairs that failed at the routing step. A site and drive-time pair fails
  # when none of its rows has `isochrone_empty = FALSE` (NA counts as empty,
  # through all(na.rm = TRUE)) or when any row has a `failure_reason`. Failed
  # pairs are left out of the later steps and get rows of NA at the end. If
  # every pair failed, there is nothing to compute, so stop.
  iso_summary <- iso_resolved |>
    sf::st_drop_geometry() |>
    dplyr::group_by(.data$site_id, .data$drive_time_min) |>
    dplyr::summarise(
      pair_empty     = all(.data$isochrone_empty, na.rm = TRUE),
      pair_failed    = any(!is.na(.data$failure_reason)),
      pair_invalid   = .data$pair_empty || .data$pair_failed,
      failure_reason = dplyr::case_when(
        .data$pair_failed ~ paste(unique(stats::na.omit(.data$failure_reason)),
                                  collapse = ";"),
        .data$pair_empty  ~ "isochrone_empty",
        TRUE              ~ NA_character_
      ),
      .groups = "drop"
    )

  valid_pairs <- iso_summary |>
    dplyr::filter(!.data$pair_invalid) |>
    dplyr::select("site_id", "drive_time_min")

  failed_pairs <- iso_summary |>
    dplyr::filter(.data$pair_invalid) |>
    dplyr::select("site_id", "drive_time_min", "failure_reason")

  if (nrow(valid_pairs) == 0L) {
    n_pair_total <- nrow(iso_summary)
    .cli_abort_operator(c(
      "Routing gave no area for any of the {n_pair_total} site and drive-time pair{?s}.",
      "x" = "No estimates can be computed.",
      "i" = "Check the site coordinates, the drive times, and the routing service; for {.arg precomputed_isochrones}, see its {.field isochrone_empty} and {.field failure_reason} columns."
    ))
  }

  if (nrow(failed_pairs) > 0L) {
    n_failed <- nrow(failed_pairs)
    n_total  <- nrow(iso_summary)
    .cacs_emit(
      level = "warn",
      message = c(
        "Routing gave no area for {n_failed} of {n_total} site and drive-time pair{?s}.",
        "i" = "Their rows have {.code NA} values and {.code failure_origin = \"isochrone\"}; see the {.attr cacs_run_warnings} attribute of the result."
      ),
      classes = c("catchmentACS_warning_partial",
                  "catchmentACS_warning",
                  "catchmentACS_condition"),
      phase = "run",
      .envir = rlang::current_env()
    )
    warning_log$entries$orchestrator <- c(
      warning_log$entries$orchestrator,
      list(list(
        code         = "W-24-02",
        n_failed     = nrow(failed_pairs),
        n_total      = nrow(iso_summary),
        failed_pairs = failed_pairs,
        captured_at  = Sys.time()
      ))
    )
  }

  # Only the pairs that succeeded go on to the intersection step, so no work
  # is spent on pairs without an area.
  iso_sf_valid <- iso_resolved |>
    dplyr::semi_join(valid_pairs, by = c("site_id", "drive_time_min"))

  # Intersection and weights, margins of error, and rates. As in the first two
  # steps, .call_with_capture() records the warnings of each step under its
  # name and lets them reach the user.
  weighted_acs <- .call_with_capture(
    fn          = cacs_intersect_weight,
    args        = c(
      list(iso_sf = iso_sf_valid, acs_sf = acs_resolved,
           bg_pop_sf = bg_pop_sf, weight_method = weight_method,
           verbose = verbose, cache_dir = cache_dir),
      passthrough_wgt
    ),
    warning_log = warning_log,
    phase       = "intersect_weight"
  )

  moe_propagated <- .call_with_capture(
    fn          = cacs_propagate_moe,
    args        = c(
      list(data = weighted_acs, formula = moe_formula,
           verbose = verbose),
      passthrough_moe
    ),
    warning_log = warning_log,
    phase       = "propagate_moe"
  )

  rate_derived <- .call_with_capture(
    fn          = cacs_derive_rates,
    args        = c(
      list(weighted_acs = moe_propagated, rates = rates,
           formula_dispatch = formula_dispatch, verbose = verbose),
      passthrough_rat
    ),
    warning_log = warning_log,
    phase       = "derive_rates"
  )

  # Rows for the failed pairs, added at the end: for each pair, one row per
  # variable of `rate_derived` and per rate, with NA values,
  # `failure_origin = "isochrone"`, and `n_tracts = NA`. A pair whose area
  # has no tract is different: it keeps the single row that
  # cacs_intersect_weight() gives it, with `failure_origin = "intersection"`
  # and `n_tracts = 0`.
  if (nrow(failed_pairs) > 0L) {
    na_rows <- .build_na_propagation_rows(
      failed_pairs = failed_pairs,
      rates        = rates,
      template     = rate_derived
    )
    long_final <- dplyr::bind_rows(rate_derived, na_rows)
  } else {
    long_final <- rate_derived
  }

  # Form of the result and its attributes. `long_final` keeps the attributes
  # that cacs_derive_rates() left on its result, among them skipped_geoids and
  # cacs_rate_audit (cacs_aggregation_carriers has been removed by then). The
  # list-column form is built anew and has none of them, so the records of
  # the steps are set on each form below. The list-column form takes its
  # areas from `iso_resolved` rather than `iso_sf_valid`, so a pair that
  # failed at the routing step keeps its area row, with `isochrone_empty` and
  # `failure_reason`.
  out <- switch(
    output,
    "long"        = long_final,
    "list_column" = .pivot_to_list_column(long_final, sites,
                                          iso_sf = iso_resolved),
    "both"        = list(
      long        = long_final,
      list_column = .pivot_to_list_column(long_final, sites,
                                          iso_sf = iso_resolved)
    )
  )

  # The run record (22 fields): the settings of the call and what was
  # observed. `execution_path` is kept here as well as in the message, so the
  # result itself shows which steps ran. When `moe_args` or `weight_args` do
  # not set `level` or `min_weight`, the defaults of cacs_propagate_moe() and
  # cacs_intersect_weight() (0.90, 1e-6) are recorded; a NULL `moe_formula`
  # is recorded as "auto".
  run_provenance <- list(
    generated_at                  = Sys.time(),
    cacs_ver                      = as.character(utils::packageVersion("catchmentACS")),
    R_version                     = as.character(getRversion()),
    schema_version                = "1.0",
    execution_path                = execution_path,
    state                         = state,
    year                          = as.integer(year),
    drive_times                   = sort(unique(as.integer(drive_times))),
    provider                      = provider,
    weight_method                 = weight_method,
    moe_formula                   = moe_formula %||% "auto",
    formula_dispatch              = formula_dispatch,
    output_format                 = output,
    level                         = moe_args$level %||% 0.90,
    min_weight                    = weight_args$min_weight %||% 1e-6,
    n_sites_input                 = dplyr::n_distinct(sites$site_id),
    n_pairs_valid                 = nrow(valid_pairs),
    n_pairs_failed                = nrow(failed_pairs),
    n_sites_with_any_valid_pair   = dplyr::n_distinct(valid_pairs$site_id),
    n_sites_with_any_failed_pair  = dplyr::n_distinct(failed_pairs$site_id),
    bypass_iso                    = !is.null(precomputed_isochrones),
    bypass_acs                    = !is.null(acs)
  )

  run_warnings <- as.list(warning_log$entries)

  # The records of the steps are read from each step's own result, so both
  # forms get the same values. Assigning NULL removes an attribute, so a
  # record that a step did not set is absent from the result.
  agg_prov  <- attr(weighted_acs,    "cacs_aggregation_provenance")
  moe_prov  <- attr(moe_propagated,  "cacs_moe_provenance")
  rate_prov <- attr(rate_derived,    "cacs_rate_provenance")
  conf_lvl  <- attr(moe_propagated,  "cacs_confidence_level")

  attach_run_attrs <- function(x) {
    attr(x, "cacs_schema_version")          <- "1.0"
    attr(x, "cacs_run_provenance")          <- run_provenance
    attr(x, "cacs_run_warnings")            <- run_warnings
    attr(x, "cacs_aggregation_provenance") <- agg_prov
    attr(x, "cacs_moe_provenance")          <- moe_prov
    attr(x, "cacs_rate_provenance")         <- rate_prov
    attr(x, "cacs_confidence_level")        <- conf_lvl
    x
  }

  if (identical(output, "both")) {
    # The elements `long` and `list_column` get the same attributes.
    out$long        <- attach_run_attrs(out$long)
    out$list_column <- attach_run_attrs(out$list_column)
  } else {
    out <- attach_run_attrs(out)
  }

  # The class cacs_run_result and the cacs_run_result_metadata attribute,
  # used by print() and summary().

  # Sites: `n_sites_success` counts the sites with at least one pair that
  # succeeded, so a site with some failed pairs counts here (the run record
  # counts those in `n_sites_with_any_failed_pair`). `n_sites_failed` is the
  # number of sites in `sites` minus `n_sites_success`, but not below 0. The
  # successes are counted from the areas, so when `precomputed_isochrones`
  # has areas for sites that are not in `sites`, `n_sites_success` can exceed
  # `n_sites_total`.
  .n_sites_success <- run_provenance$n_sites_with_any_valid_pair
  .n_sites_failed  <- max(
    run_provenance$n_sites_input - run_provenance$n_sites_with_any_valid_pair,
    0L
  )

  # Counts of rows by `moe_fallback_reason`. cacs_derive_rates() sets the
  # reason on rate rows (.compute_one_rate() in R/derive-rates.R):
  # "negative_variance" (the ratio formula replaced the proportion formula),
  # "zero_denominator", or "missing_moe". Other rows, including those of
  # failed pairs, have "n/a". The "missing_moe" count is 0 in results of
  # cacs_run(): a rate whose numerator or denominator has no margin of error
  # becomes a "carrier" row before any formula is applied.
  .moe_fallback_summary <- if ("moe_fallback_reason" %in% colnames(long_final)) {
    .reason <- long_final$moe_fallback_reason
    list(
      c1_to_c2_count    = sum(.reason == "negative_variance", na.rm = TRUE),
      zero_den_count    = sum(.reason == "zero_denominator",  na.rm = TRUE),
      missing_moe_count = sum(.reason == "missing_moe",        na.rm = TRUE)
    )
  } else {
    list(c1_to_c2_count    = NA_integer_,
         zero_den_count    = NA_integer_,
         missing_moe_count = NA_integer_)
  }

  # profile: the `profile` of the first area, whether the areas were supplied
  # or built; if it is NA or empty, `iso_args$profile`, and otherwise
  # "driving".
  .iso_profile <- if (!is.null(iso_resolved) &&
                      "profile" %in% colnames(iso_resolved) &&
                      nrow(iso_resolved) > 0L) {
    .p <- iso_resolved$profile[[1L]]
    if (is.na(.p) || !nzchar(.p)) passthrough_iso$profile %||% "driving"
    else .p
  } else {
    passthrough_iso$profile %||% "driving"
  }

  .metadata <- list(
    generated_at         = run_provenance$generated_at,
    cacs_version         = utils::packageVersion("catchmentACS"),
    provider             = provider,
    profile              = .iso_profile,
    drive_times          = sort(unique(as.numeric(drive_times))),
    n_sites_total        = run_provenance$n_sites_input,
    n_sites_success      = .n_sites_success,
    n_sites_failed       = .n_sites_failed,
    wall_clock_seconds   = as.numeric(difftime(Sys.time(), run_start,
                                               units = "secs")),
    skipped_geoids       = attr(weighted_acs, "skipped_geoids") %||% character(0),
    moe_fallback_summary = .moe_fallback_summary
  )

  if (identical(output, "both")) {
    out$long        <- .attach_run_result_class(out$long,        .metadata)
    out$list_column <- .attach_run_result_class(out$list_column, .metadata)
  } else {
    out <- .attach_run_result_class(out, .metadata)
  }

  if (isTRUE(verbose)) {
    .cli_inform_progress(
      c(
        "i" = "Run complete: {.val {run_provenance$n_pairs_valid}} valid / {.val {run_provenance$n_pairs_failed}} failed pair{?s} across {.val {run_provenance$n_sites_input}} site{?s}.",
        "i" = "Output form: {.val {output}}."
      ),
      phase = "run"
    )
  }

  out
}


# Helpers used by cacs_run().


#' Keep the accepted names of one *_args list of cacs_run()
#'
#' Checks one of the lists `iso_args`, `acs_args`, `weight_args`,
#' `moe_args`, and `rate_args` against the names that the step it is passed
#' to accepts (`.CACS_RUN_ARG_KEYS`). Other names give a warning of class
#' `catchmentACS_warning_provenance` and are dropped, so they never reach
#' the step; with no accepted names (`rate_args`), every name is dropped. An
#' element without a name gives an error. cacs_run() has already checked
#' that each argument is a list; the check here repeats it.
#'
#' @param args_list The list given to cacs_run().
#' @param label The start of the argument name, used in the messages as
#'   `<label>_args`: "iso", "acs", "weight", "moe", or "rate".
#' @param sanctioned_keys A character vector of the accepted names.
#'
#' @return The elements of `args_list` with accepted names, in their order
#'   (for a name given twice, the first element only); an empty list when
#'   `args_list` is empty.
#' @keywords internal
#' @noRd
.validate_run_arg_list <- function(args_list, label, sanctioned_keys) {
  if (!is.list(args_list)) {
    .cli_abort_schema(c(
      "{.arg {label}_args} must be a {.cls list}.",
      "x" = "Got {.cls {class(args_list)[[1L]]}}."
    ))
  }
  if (length(args_list) == 0L) {
    return(list())
  }

  arg_names <- names(args_list)
  if (is.null(arg_names) || any(!nzchar(arg_names))) {
    .cli_abort_schema(c(
      "{.arg {label}_args} must be a fully-named {.cls list}.",
      "x" = "At least one element has no name.",
      "i" = "Name each element, as in {.code {label}_args = list(name = value)}."
    ))
  }

  unknown <- setdiff(arg_names, sanctioned_keys)
  if (length(unknown) > 0L) {
    if (length(sanctioned_keys) == 0L) {
      .cli_warn_provenance(
        c(
          "{.arg {label}_args} has {cli::qty(length(unknown))}unknown name{?s}: {.val {unknown}}.",
          "i" = "{.arg {label}_args} accepts no names, so every element is dropped.",
          "x" = "Dropped: {.val {unknown}}."
        ),
        phase = "run"
      )
    } else {
      .cli_warn_provenance(
        c(
          "{.arg {label}_args} has {cli::qty(length(unknown))}unknown name{?s}: {.val {unknown}}.",
          "i" = "Accepted name{?s}: {.val {sanctioned_keys}}.",
          "x" = "Dropped: {.val {unknown}}."
        ),
        phase = "run"
      )
    }
  }

  args_list[intersect(arg_names, sanctioned_keys)]
}


#' Call one step of cacs_run() and record its warnings
#'
#' Calls `fn` with `args` through do.call() inside withCallingHandlers().
#' Each warning that the step gives, from cli or from base R, is added to
#' `warning_log$entries[[phase]]` as a list of `message`, `class`, `phase`,
#' and `captured_at`; cacs_run() returns these lists as the
#' cacs_run_warnings attribute. The warning is not muffled, so it also
#' reaches the user. Errors are not caught.
#'
#' withCallingHandlers() is used rather than tryCatch() because
#' tryCatch(warning = ) would end the step at its first warning and return
#' the handler's value instead of the step's result.
#'
#' @param fn The step, a function such as cacs_isochrone.
#' @param args A named list of arguments for `fn`.
#' @param warning_log The environment made by cacs_run(), with a list
#'   `entries` that has one element for each step.
#' @param phase The name of the element of `warning_log$entries` to add to:
#'   "acs_prefetch", "isochrone", "intersect_weight", "propagate_moe", or
#'   "derive_rates". A misspelled name adds a new element instead of giving
#'   an error.
#'
#' @return The value of `do.call(fn, args)`, attributes included.
#' @keywords internal
#' @noRd
.call_with_capture <- function(fn, args, warning_log, phase) {
  withCallingHandlers(
    do.call(fn, args),
    warning = function(w) {
      warning_log$entries[[phase]] <- c(
        warning_log$entries[[phase]],
        list(list(
          message     = conditionMessage(w),
          class       = class(w),
          phase       = phase,
          captured_at = Sys.time()
        ))
      )
      # No invokeRestart("muffleWarning"), so the warning goes on to the user.
    }
  )
}


#' Build the rows of the pairs that failed at the routing step
#'
#' Makes one row for each failed site and drive-time pair and each variable:
#' the values of `variable` in `template` and the rate names in `rates`. The
#' rows have the columns of `template`, in the same order and of the same
#' types, so that dplyr::bind_rows(template, rows) keeps the columns as they
#' are. The values are NA except:
#'
#'   * `ring_topology = "cumulative"`;
#'   * `failure_origin = "isochrone"`, which marks these rows;
#'   * `moe_fallback_reason = "n/a"`: no margin-of-error formula was tried;
#'   * `estimand_family`, taken from the rows of `template` with the same
#'     `variable`, so rate rows stay "derived_rate" and go to the
#'     `derived_rates` column of the list-column form.
#'
#' `n_tracts` is NA here. A pair that has an area but no tract has instead
#' one row from cacs_intersect_weight(), with `n_tracts = 0` and
#' `failure_origin = "intersection"`.
#'
#' @param failed_pairs A tibble with the columns `site_id` and
#'   `drive_time_min`. Its column `failure_reason` is not used here;
#'   cacs_run() keeps it in the `orchestrator` element of the
#'   cacs_run_warnings attribute.
#' @param rates The named list of rates given to cacs_run(); its names are
#'   added to the variables.
#' @param template The result of cacs_derive_rates() for the pairs that
#'   succeeded.
#'
#' @return A tibble with the columns of `template`, one row for each failed
#'   pair and variable, or no rows when there is no failed pair.
#' @keywords internal
#' @noRd
.build_na_propagation_rows <- function(failed_pairs, rates, template) {
  template_cols <- colnames(template)

  # Variables: the values of `variable` in `template` and the rate names;
  # unique() drops the names that appear in both. `template` also has a row
  # with variable = NA for each pair whose area has no tract, so in a run
  # with such a pair every failed pair gets a row with variable = NA too.
  template_vars <- if ("variable" %in% template_cols) {
    unique(template$variable)
  } else {
    character(0)
  }
  rate_names <- names(rates) %||% character(0)
  all_vars   <- unique(c(template_vars, rate_names))

  # No failed pair (or no variable): zero rows with the columns of `template`.
  if (nrow(failed_pairs) == 0L || length(all_vars) == 0L) {
    empty <- template[integer(0), , drop = FALSE]
    return(empty)
  }

  # tidyr::expand_grid() keeps character columns as they are; expand.grid()
  # would turn them into factors by default.
  na_skeleton <- tidyr::expand_grid(
    site_id        = unique(failed_pairs$site_id),
    drive_time_min = unique(failed_pairs$drive_time_min),
    variable       = all_vars
  )
  # The grid crosses every failed site with every failed drive time; keep
  # only the pairs that failed (a site can fail at one drive time and not at
  # the others).
  na_skeleton <- na_skeleton |>
    dplyr::semi_join(failed_pairs, by = c("site_id", "drive_time_min"))

  # NA of the type each column has in a result (double, integer for
  # n_tracts and acs_year, character, logical).
  na_rows <- na_skeleton |>
    dplyr::mutate(
      estimate                      = NA_real_,
      moe                           = NA_real_,
      se                            = NA_real_,
      ring_topology                 = "cumulative",
      weight_sum                    = NA_real_,
      n_tracts                      = NA_integer_,
      provider                      = NA_character_,
      profile                       = NA_character_,
      osm_snapshot_date             = NA_character_,
      acs_year                      = NA_integer_,
      weight_method                 = NA_character_,
      estimand_family               = NA_character_,
      weight_basis                  = NA_character_,
      moe_formula_requested         = NA_character_,
      moe_formula_effective         = NA_character_,
      moe_fallback                  = NA,
      moe_fallback_reason           = "n/a",
      failure_origin                = "isochrone",
      weight_uncertainty_propagated = NA
    )

  # Take estimand_family from the rows of `template` with the same
  # `variable`, so that rate rows stay "derived_rate".
  if ("estimand_family" %in% template_cols && "variable" %in% template_cols) {
    template_family <- template |>
      dplyr::distinct(.data$variable, .data$estimand_family) |>
      stats::na.omit()
    if (nrow(template_family) > 0L) {
      na_rows <- na_rows |>
        dplyr::select(-"estimand_family") |>
        dplyr::left_join(template_family, by = "variable")
    }
  }

  # Columns of `template` not set above (such as n_tracts_num, n_tracts_den,
  # est_total, var_total_raw, est_mean, and var_mean_raw) are added as NA.
  # The last line drops `se` when `template` has no such column; it has one
  # only with options(cacs.return_se = TRUE).
  missing_in_na <- setdiff(template_cols, colnames(na_rows))
  for (col in missing_in_na) {
    template_col <- template[[col]]
    # NA of the column's type, so that dplyr::bind_rows() keeps the type (an
    # integer column bound to a double NA would become double).
    na_value <- switch(
      typeof(template_col),
      "integer"   = NA_integer_,
      "double"    = NA_real_,
      "logical"   = NA,
      "character" = NA_character_,
      NA
    )
    na_rows[[col]] <- na_value
  }

  na_rows[, template_cols, drop = FALSE]
}


#' Build the list-column form of the result of cacs_run()
#'
#' Turns the long form into a tibble with one row per site and drive time,
#' for `output = "list_column"` and `"both"`. Its columns are `site_id`,
#' `lon`, `lat`, `drive_time_min`, and four list-columns:
#'
#'   * `isochrone`: the drive-time area of the pair, a one-row sf object
#'     from `iso_sf`, or NULL when `iso_sf` is NULL or has no row for the
#'     pair;
#'   * `acs_estimates`: a tibble of the rows of the pair that are not rates
#'     (`estimand_family` other than "derived_rate");
#'   * `derived_rates`: a tibble of the rate rows of the pair;
#'   * `metadata`: a list of `provider`, `profile`, `osm_snapshot_date`,
#'     `acs_year`, `weight_method`, `failure_origin`, and `n_tracts`, taken
#'     from the first row of the pair.
#'
#' `lon` and `lat` are the columns of the same names in `sites_input`, or NA
#' when it has none; the coordinates of an sf object are not used.
#'
#' @param long_final The long form, including the rows of the pairs that
#'   failed at the routing step.
#' @param sites_input The `sites` argument of cacs_run(), a data frame or an
#'   sf object.
#' @param iso_sf The drive-time areas of all pairs, supplied or built, or
#'   NULL.
#'
#' @return A tibble with the eight columns `site_id`, `lon`, `lat`,
#'   `drive_time_min`, `isochrone`, `acs_estimates`, `derived_rates`, and
#'   `metadata`, one row per site and drive time in `long_final`, sorted by
#'   `site_id` and `drive_time_min`.
#' @keywords internal
#' @noRd
.pivot_to_list_column <- function(long_final, sites_input, iso_sf = NULL) {
  # lon and lat of each site, from the first row of the site in `sites`.
  sites_tbl <- if (inherits(sites_input, "sf")) {
    sf::st_drop_geometry(sites_input)
  } else {
    tibble::as_tibble(sites_input)
  }
  # Without `lon` or `lat` columns (an sf object of points often has none),
  # the values are NA: the point coordinates are not used. On a tibble, `$`
  # also gives a warning for each missing column.
  sites_coords <- tibble::tibble(
    site_id = unique(sites_tbl$site_id),
    lon     = if (!is.null(sites_tbl$lon))
                sites_tbl$lon[match(unique(sites_tbl$site_id), sites_tbl$site_id)]
              else NA_real_,
    lat     = if (!is.null(sites_tbl$lat))
                sites_tbl$lat[match(unique(sites_tbl$site_id), sites_tbl$site_id)]
              else NA_real_
  )

  long_tbl <- tibble::as_tibble(long_final)

  # Rate rows (estimand_family "derived_rate") go to `derived_rates` and all
  # other rows to `acs_estimates`. The rows of pairs that failed at the
  # routing step carry the estimand_family of their variable
  # (.build_na_propagation_rows()), so they are split in the same way.
  is_rate_row <- !is.na(long_tbl$estimand_family) &
                  long_tbl$estimand_family == "derived_rate"

  acs_part   <- long_tbl[!is_rate_row, , drop = FALSE]
  rates_part <- long_tbl[ is_rate_row, , drop = FALSE]

  # tidyr::nest() with `.by` nests every other column and takes the key
  # columns as strings, so no tidyselect helper is needed (tidyselect is not
  # in Imports).
  acs_nested <- if (nrow(acs_part) > 0L) {
    tidyr::nest(
      acs_part,
      .by  = c("site_id", "drive_time_min"),
      .key = "acs_estimates"
    )
  } else {
    tibble::tibble(
      site_id        = character(0),
      drive_time_min = integer(0),
      acs_estimates  = list()
    )
  }
  rates_nested <- if (nrow(rates_part) > 0L) {
    tidyr::nest(
      rates_part,
      .by  = c("site_id", "drive_time_min"),
      .key = "derived_rates"
    )
  } else {
    tibble::tibble(
      site_id        = character(0),
      drive_time_min = integer(0),
      derived_rates  = list()
    )
  }

  # metadata: the values of the first row of each pair. provider, profile,
  # osm_snapshot_date, acs_year, and weight_method are the same on every row
  # of a pair (NA for a pair that failed at the routing step).
  # failure_origin and n_tracts are those of the first row: "isochrone" and
  # NA for a pair that failed at the routing step, "intersection" and 0 for
  # a pair whose area has no tract.
  meta_pairs <- long_tbl |>
    dplyr::group_by(.data$site_id, .data$drive_time_min) |>
    dplyr::summarise(
      provider          = dplyr::first(.data$provider),
      profile           = dplyr::first(.data$profile),
      osm_snapshot_date = dplyr::first(.data$osm_snapshot_date),
      acs_year          = dplyr::first(.data$acs_year),
      weight_method     = dplyr::first(.data$weight_method),
      failure_origin    = dplyr::first(.data$failure_origin),
      n_tracts          = dplyr::first(.data$n_tracts),
      .groups = "drop"
    )
  metadata_col <- lapply(seq_len(nrow(meta_pairs)), function(i) {
    list(
      provider          = meta_pairs$provider[[i]],
      profile           = meta_pairs$profile[[i]],
      osm_snapshot_date = meta_pairs$osm_snapshot_date[[i]],
      acs_year          = meta_pairs$acs_year[[i]],
      weight_method     = meta_pairs$weight_method[[i]],
      failure_origin    = meta_pairs$failure_origin[[i]],
      n_tracts          = meta_pairs$n_tracts[[i]]
    )
  })
  meta_tbl <- tibble::tibble(
    site_id        = meta_pairs$site_id,
    drive_time_min = meta_pairs$drive_time_min,
    metadata       = metadata_col
  )

  # One row for each pair found in any of the three tables, sorted by site
  # and drive time.
  pair_keys <- dplyr::bind_rows(
    dplyr::select(acs_nested,   "site_id", "drive_time_min"),
    dplyr::select(rates_nested, "site_id", "drive_time_min"),
    dplyr::select(meta_tbl,     "site_id", "drive_time_min")
  ) |>
    dplyr::distinct() |>
    dplyr::arrange(.data$site_id, .data$drive_time_min)

  out <- pair_keys |>
    dplyr::left_join(sites_coords, by = "site_id") |>
    dplyr::left_join(acs_nested,   by = c("site_id", "drive_time_min")) |>
    dplyr::left_join(rates_nested, by = c("site_id", "drive_time_min")) |>
    dplyr::left_join(meta_tbl,     by = c("site_id", "drive_time_min"))

  # The isochrone cells start as NULL and are filled from `iso_sf` where the
  # site and drive time match.
  out$isochrone <- vector("list", nrow(out))
  if (!is.null(iso_sf)) {
    out <- .cacs_pipe_iso_to_list_column(out, iso_sf = iso_sf)
    if (isTRUE(attr(out, "iso_was_filled"))) {
      # Shown on every call that fills a cell, whatever `verbose` is.
      .cli_inform_listcol_iso_filled()
    }
  }

  # The eight columns, in this order.
  out[, c("site_id", "lon", "lat", "drive_time_min",
          "isochrone", "acs_estimates", "derived_rates", "metadata"),
      drop = FALSE]
}


# Methods for the result of cacs_run(). The result has the class
# cacs_run_result in front of its tibble classes, c("cacs_run_result",
# "tbl_df", "tbl", "data.frame"), so print(), summary(), group_by(), and
# as_tibble() use the methods below while other functions treat it as a
# tibble. The values that print() and summary() show are kept in one
# attribute, cacs_run_result_metadata. Subsetting with `[` and most dplyr
# verbs (filter(), mutate(), arrange(), joins) keep the class and the
# attribute; summarise() returns a plain tibble, and group_by() drops the
# class through its method below.


#' Add the class cacs_run_result and the metadata attribute
#'
#' Puts `cacs_run_result` in front of the existing classes, so that print()
#' and summary() use the methods for it while other functions still see a
#' tibble, and stores `metadata` as the attribute cacs_run_result_metadata.
#'
#' @param out The long or list-column form of the result of cacs_run().
#' @param metadata A list with 11 elements: `generated_at`, `cacs_version`,
#'   `provider`, `profile`, `drive_times`, `n_sites_total`,
#'   `n_sites_success`, `n_sites_failed`, `wall_clock_seconds`,
#'   `skipped_geoids`, and `moe_fallback_summary`.
#'
#' @return `out` with the class and the attribute added.
#' @keywords internal
#' @noRd
.attach_run_result_class <- function(out, metadata) {
  attr(out, "cacs_run_result_metadata") <- metadata
  class(out) <- c("cacs_run_result", class(out))
  out
}


#' Remove the class cacs_run_result, keeping the other classes and attributes
#'
#' The group_by(), print(), and summary() methods call it before they use
#' dplyr, so that dplyr works on an ordinary tibble and does not call the
#' methods of this class again.
#'
#' @keywords internal
#' @noRd
.cacs_run_result_plain <- function(x) {
  class(x) <- setdiff(class(x), "cacs_run_result")
  x
}


#' Group the result of `cacs_run()`
#'
#' Groups a result of [cacs_run()] when it is passed to [dplyr::group_by()].
#' The method removes the class `cacs_run_result` and then groups the rows
#' as those of an ordinary tibble, keeping their order. The grouped table
#' keeps the other attributes of `.data`, and tables computed from it, for
#' example with [dplyr::summarise()], are ordinary tibbles.
#'
#' @param .data A `cacs_run_result` object, as returned by [cacs_run()].
#' @param ... Variables or computations to group by, as in
#'   [dplyr::group_by()].
#' @param .add,.drop Passed to [dplyr::group_by()]. With `.drop = NULL` (the
#'   default), dplyr's default is used ([dplyr::group_by_drop_default()]).
#'
#' @return A grouped tibble, of class
#'   `c("grouped_df", "tbl_df", "tbl", "data.frame")`.
#' @family result summaries
#' @method group_by cacs_run_result
#' @exportS3Method dplyr::group_by cacs_run_result
#' @examples
#' out <- readRDS(system.file("extdata", "visual_walkthrough_fixture.rds",
#'                            package = "catchmentACS"))$run_result
#'
#' # The number of rows for each kind of quantity
#' out |>
#'   dplyr::group_by(estimand_family) |>
#'   dplyr::summarise(rows = dplyr::n())
group_by.cacs_run_result <- function(.data, ..., .add = FALSE, .drop = NULL) {
  data_plain <- .cacs_run_result_plain(.data)
  if (is.null(.drop)) {
    .drop <- dplyr::group_by_drop_default(data_plain)
  }
  dplyr::group_by(data_plain, ..., .add = .add, .drop = .drop)
}


#' Print the result of `cacs_run()`
#'
#' Prints a result of [cacs_run()] as a header about the run, two tables of
#' the five rates, and a preview of the rows.
#'
#' Each part of the output starts with a title line:
#' \describe{
#'   \item{catchmentACS run result}{The time the result was created and the
#'     version of catchmentACS that created it, the routing service and the
#'     drive times given to [cacs_run()], and the routing profile of the
#'     drive-time areas. The drive times are the ones given in the call, so
#'     when the areas come from `precomputed_isochrones` the line can name a
#'     drive time that no row of the result has. The line "Sites" gives the
#'     number of sites with at
#'     least one drive-time area (`success`), the number of sites given to
#'     [cacs_run()] in `sites` (`total`), and `total` minus `success`, but
#'     not below 0 (`failed`). If `precomputed_isochrones` has areas for
#'     sites that are not in `sites`, `success` counts them too, so it can
#'     exceed `total` and `failed` can miss sites without a drive-time area.
#'     When tracts were skipped because they have no area, one more line
#'     gives the length of the `skipped_geoids` attribute, which lists each
#'     such tract once for each of its variables.}
#'   \item{Top 5 rates (cross-site mean +/- sd)}{One row for each of the
#'     five rates, highest mean first. The columns are the unweighted mean
#'     (`mean_estimate`) and the standard deviation (`sd_estimate`) of the
#'     rate over all site and drive-time pairs, leaving out missing values,
#'     and `n`, the number of pairs, including those where the rate is
#'     missing.}
#'   \item{Rates per site}{The estimates of the five rates, with one row for
#'     each site and drive time. The table is shown when the result has at
#'     most `getOption("catchmentACS.summary_per_site_max")` sites (12 by
#'     default). With `0` it is never shown, and with `Inf` always.}
#'   \item{Tibble preview}{The rows, printed as a tibble. Before this title,
#'     the line "Wall clock" gives the time that [cacs_run()] took, in
#'     seconds. The last line names `as_tibble(x)`, which returns a plain
#'     tibble (see [as_tibble.cacs_run_result()]), and `x[]`, which is the
#'     same object and prints in the same way.}
#' }
#'
#' The rate tables need the columns `variable`, `estimate`, and `moe`, so
#' they are not shown for the list-column form of the result. Subsets made
#' with `[` or with dplyr verbs such as [dplyr::filter()], and tables made
#' with [dplyr::count()] or [dplyr::distinct()], keep the class and are
#' printed in the same way, with the header of the whole result. Printing
#' one that has those three columns but no `site_id` column gives an error.
#' Without the `cacs_run_result_metadata` attribute, the result is printed as
#' a plain tibble.
#'
#' @section Capturing the output:
#' The tables are ordinary printed output, and every other line is an R
#' message written with the cli package. These calls keep different parts:
#' ```
#' capture.output(print(x))                    # the tables
#' capture.output(print(x), type = "message")  # the cli output
#' testthat::capture_messages(print(x))        # the cli output, in tests
#' suppressMessages(print(x))                  # prints only the tables
#' ```
#'
#' @param x A `cacs_run_result` object, as returned by [cacs_run()].
#' @param ... Passed to the print method for tibbles ([tibble::print.tbl()])
#'   for the preview, such as `n`, the number of rows to show.
#' @param n_head,n_tail Single numbers that have no effect (`n` in `...`
#'   sets the number of rows).
#'
#' @return `x`, invisibly.
#' @family result summaries
#' @exportS3Method print cacs_run_result
#' @examples
#' # A bundled cacs_run() result for a 10-minute area in Birmingham, Alabama
#' out <- readRDS(system.file("extdata", "visual_walkthrough_fixture.rds",
#'                            package = "catchmentACS"))$run_result
#'
#' # One site and one drive time, so the standard deviations are NA
#' print(out, n = 5)
print.cacs_run_result <- function(x, ..., n_head = 5, n_tail = 3) {
  meta <- attr(x, "cacs_run_result_metadata")
  if (is.null(meta)) {
    # Without the metadata attribute, print as a tibble. dplyr verbs that
    # keep the class also keep the attribute, so this happens when the
    # attribute was removed in some other way.
    return(NextMethod())
  }
  x_plain <- .cacs_run_result_plain(x)

  cli::cli_h1("catchmentACS run result")
  cli::cli_text("Generated at: {format(meta$generated_at)}")
  cli::cli_text("Package version: {as.character(meta$cacs_version)}")
  cli::cli_text("Provider: {meta$provider} / {meta$profile}")
  cli::cli_text("Drive times: {meta$drive_times} min")
  cli::cli_text(paste0(
    "Sites: {meta$n_sites_success} success / ",
    "{meta$n_sites_failed} failed / ",
    "{meta$n_sites_total} total"
  ))

  if (length(meta$skipped_geoids) > 0L) {
    cli::cli_text(
      "Water/degenerate tracts skipped: {length(meta$skipped_geoids)}"
    )
  }

  # Two rate tables: the mean and standard deviation of each rate over all
  # site and drive-time pairs, and the rates of each pair, shown for at most
  # getOption("catchmentACS.summary_per_site_max") sites (12 by default).
  rate_vars <- c("poverty_rate", "snap_rate", "ssi_rate",
                 "unemp_rate", "labor_force_participation")
  if (nrow(x_plain) > 0L &&
      all(c("variable", "estimate", "moe") %in% colnames(x_plain))) {
    rates_summary <- x_plain |>
      dplyr::filter(.data$variable %in% rate_vars) |>
      dplyr::group_by(.data$variable) |>
      dplyr::summarise(
        mean_estimate = mean(.data$estimate, na.rm = TRUE),
        sd_estimate   = stats::sd(.data$estimate, na.rm = TRUE),
        n             = dplyr::n(),
        .groups       = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(.data$mean_estimate)) |>
      utils::head(5L)
    if (nrow(rates_summary) > 0L) {
      cli::cli_h2("Top 5 rates (cross-site mean +/- sd)")
      print(rates_summary)
    }

    per_site_max <- getOption("catchmentACS.summary_per_site_max", 12L)
    n_sites_x <- dplyr::n_distinct(x_plain$site_id)
    if (isTRUE(n_sites_x <= per_site_max) && isTRUE(per_site_max > 0L)) {
      pivot <- .cacs_rates_per_site_pivot(x_plain)
      if (!is.null(pivot$wide) && nrow(pivot$wide) > 0L) {
        cli::cli_h2("Rates per site")
        print(pivot$wide)
      }
    }
  }

  cli::cli_text("Wall clock: {round(meta$wall_clock_seconds, 1)}s")
  cli::cli_h2("Tibble preview")
  NextMethod()

  cli::cli_alert_info(
    "For full output: {.code as_tibble(x)} or {.code x[]}"
  )

  invisible(x)
}


#' Summarize the result of `cacs_run()`
#'
#' Summarizes a result of [cacs_run()] in a list of counts and tables of the
#' five rates, which [print.cacs_run_summary()] prints.
#'
#' @param object A result of [cacs_run()] in the long form
#'   (`output = "long"`, the default). The list-column form gives an error.
#' @param ... Not used.
#' @param breakdown A string choosing the rate tables that
#'   [print.cacs_run_summary()] shows. With `"cross_site"` (the default), it
#'   shows `rates_breakdown`, and also `rates_per_site` when the result has
#'   at most `getOption("catchmentACS.summary_per_site_max")` sites, as in
#'   [print.cacs_run_result()]. `"per_site"` shows only `rates_per_site`,
#'   and `"both"` shows both. The elements of the list do not depend on this
#'   choice.
#'
#' @return A list of class `c("cacs_run_summary", "list")` with the elements
#'   below, and an attribute `breakdown` that keeps the `breakdown` argument
#'   for printing.
#'   \describe{
#'     \item{`metadata`}{The `cacs_run_result_metadata` attribute of
#'       `object`, or `NULL` if it has none: the values shown in the header
#'       of [print.cacs_run_result()], the time that [cacs_run()] took in
#'       seconds (`wall_clock_seconds`), `skipped_geoids`, and counts of rate
#'       rows by `moe_fallback_reason` (`moe_fallback_summary`).}
#'     \item{`n_rows`, `n_sites`, `n_variables`, `n_drive_times`}{The number
#'       of rows, and the numbers of distinct values of `site_id`,
#'       `variable`, and `drive_time_min`.}
#'     \item{`n_tracts_summary`}{The five-number summary of `n_tracts` from
#'       [stats::fivenum()]: minimum, lower hinge, median, upper hinge, and
#'       maximum. Rows where `n_tracts` is `NA`, such as rate rows, are left
#'       out.}
#'     \item{`rates_breakdown`}{A tibble with one row for each of the five
#'       rates (`variable`). The columns are the unweighted mean (`mean`) and
#'       the standard deviation (`sd`) of the rate over all site and
#'       drive-time pairs, leaving out missing values, and `n_NA`, the number
#'       of pairs where the rate is missing. [print.cacs_run_result()] shows
#'       the same means and standard deviations as "Top 5 rates", under the
#'       names `mean_estimate` and `sd_estimate`. The last column of that
#'       table, `n`, counts all the pairs, including those where the rate is
#'       missing.}
#'     \item{`rates_per_site`}{A tibble of the estimates of the five rates,
#'       with one row for each site and drive time (`site_id`,
#'       `drive_time_min`).}
#'     \item{`rates_per_site_moe`}{The same table with each cell a string
#'       giving the estimate and its margin of error (the half-width of its
#'       confidence interval), rounded to three decimals. When `object` has
#'       the attribute `cacs_confidence_level`, the table has it as well, and
#'       [cacs_summary_as_markdown()] names that level in its caption.}
#'     \item{`moe_fallback_rate`}{The number of rows whose `moe_fallback` is
#'       `TRUE` (see [cacs_run()]), divided by the number of all rows.}
#'   }
#' @family result summaries
#' @exportS3Method summary cacs_run_result
#' @examples
#' # A bundled cacs_run() result for a 10-minute area in Birmingham, Alabama
#' out <- readRDS(system.file("extdata", "visual_walkthrough_fixture.rds",
#'                            package = "catchmentACS"))$run_result
#' s <- summary(out)
#' s
#'
#' # The rates with their margins of error
#' s$rates_per_site_moe
summary.cacs_run_result <- function(object, ...,
                                    breakdown = c("cross_site",
                                                  "per_site",
                                                  "both")) {
  breakdown <- match.arg(breakdown)
  meta <- attr(object, "cacs_run_result_metadata")

  rate_vars <- c("poverty_rate", "snap_rate", "ssi_rate",
                 "unemp_rate", "labor_force_participation")

  # Aggregate without the class: dplyr can convert a cacs_run_result with its
  # as_tibble() method; dplyr then works on an ordinary tibble.
  object_plain <- .cacs_run_result_plain(object)

  rates_breakdown <- if (nrow(object_plain) > 0L &&
                         all(c("variable", "estimate") %in% colnames(object_plain))) {
    object_plain |>
      dplyr::filter(.data$variable %in% rate_vars) |>
      dplyr::group_by(.data$variable) |>
      dplyr::summarise(
        mean = mean(.data$estimate, na.rm = TRUE),
        sd   = stats::sd(.data$estimate, na.rm = TRUE),
        n_NA = sum(is.na(.data$estimate)),
        .groups = "drop"
      )
  } else {
    tibble::tibble(
      variable = character(0),
      mean     = numeric(0),
      sd       = numeric(0),
      n_NA     = integer(0)
    )
  }

  # The tables per site are computed whatever `breakdown` is; `breakdown`
  # only chooses what print.cacs_run_summary() shows.
  pivot <- .cacs_rates_per_site_pivot(object_plain)
  rates_per_site     <- pivot$wide
  rates_per_site_moe <- pivot$wide_moe
  # The confidence level of the margins of error goes with the table, so that
  # cacs_summary_as_markdown() can name it in the caption.
  conf_level <- attr(object, "cacs_confidence_level", exact = TRUE)
  if (!is.null(rates_per_site_moe) && !is.null(conf_level)) {
    attr(rates_per_site_moe, "cacs_confidence_level") <- conf_level
  }

  moe_fallback_rate <- if ("moe_fallback" %in% colnames(object_plain)) {
    .fb <- object_plain$moe_fallback
    mean(!is.na(.fb) & .fb, na.rm = FALSE)
  } else {
    NA_real_
  }

  n_tracts_summary <- if ("n_tracts" %in% colnames(object_plain) &&
                          any(!is.na(object_plain$n_tracts))) {
    stats::fivenum(object_plain$n_tracts)
  } else {
    rep(NA_real_, 5L)
  }

  out <- list(
    metadata           = meta,
    n_rows             = nrow(object_plain),
    n_sites            = if ("site_id" %in% colnames(object_plain))
                           dplyr::n_distinct(object_plain$site_id) else 0L,
    n_variables        = if ("variable" %in% colnames(object_plain))
                           dplyr::n_distinct(object_plain$variable) else 0L,
    n_drive_times      = if ("drive_time_min" %in% colnames(object_plain))
                           dplyr::n_distinct(object_plain$drive_time_min) else 0L,
    n_tracts_summary   = n_tracts_summary,
    rates_breakdown    = rates_breakdown,
    rates_per_site     = rates_per_site,
    rates_per_site_moe = rates_per_site_moe,
    moe_fallback_rate  = moe_fallback_rate
  )

  attr(out, "breakdown") <- breakdown  # read by print.cacs_run_summary()
  class(out) <- c("cacs_run_summary", "list")
  out
}


#' Print the summary of a `cacs_run()` result
#'
#' Prints the list returned by [summary.cacs_run_result()]. It starts with a
#' header giving the time the result was created, the routing service and
#' profile, and the numbers of sites, as in [print.cacs_run_result()]. It
#' then shows the numbers of rows, sites, variables, and drive times, the
#' five-number summary `n_tracts_summary` from [stats::fivenum()] (its
#' hinges are labeled `q1` and `q3`), the rate tables chosen by the
#' `breakdown` argument of `summary()`, and `moe_fallback_rate` as a
#' percentage. The table for each site shows the estimates only (the
#' element `rates_per_site`).
#'
#' The tables are ordinary printed output, and every other line is a
#' message, as with [print.cacs_run_result()].
#'
#' @param x A `cacs_run_summary` list, as returned by
#'   [summary.cacs_run_result()].
#' @param ... Not used.
#'
#' @return `x`, invisibly.
#' @family result summaries
#' @exportS3Method print cacs_run_summary
#' @examples
#' out <- readRDS(system.file("extdata", "visual_walkthrough_fixture.rds",
#'                            package = "catchmentACS"))$run_result
#'
#' # With breakdown = "per_site", the only rate table is the one for each site
#' print(summary(out, breakdown = "per_site"))
print.cacs_run_summary <- function(x, ...) {
  cli::cli_h1("catchmentACS run summary")

  meta <- x$metadata
  if (!is.null(meta)) {
    cli::cli_text("Generated at: {format(meta$generated_at)}")
    cli::cli_text("Provider: {meta$provider} / {meta$profile}")
    cli::cli_text(paste0(
      "Sites: {meta$n_sites_success} success / ",
      "{meta$n_sites_failed} failed / ",
      "{meta$n_sites_total} total"
    ))
  }

  cli::cli_h2("Run tallies")
  cli::cli_text("Rows: {x$n_rows}")
  cli::cli_text("Sites: {x$n_sites}")
  cli::cli_text("Variables: {x$n_variables}")
  cli::cli_text("Drive-time bands: {x$n_drive_times}")

  cli::cli_h2("n_tracts five-number summary")
  .fn <- x$n_tracts_summary
  if (length(.fn) == 5L && !all(is.na(.fn))) {
    cli::cli_text(paste0(
      "min={round(.fn[1], 1)}, q1={round(.fn[2], 1)}, ",
      "median={round(.fn[3], 1)}, q3={round(.fn[4], 1)}, ",
      "max={round(.fn[5], 1)}"
    ))
  } else {
    cli::cli_text("(unavailable)")
  }

  # The `breakdown` attribute chooses the rate tables. With "cross_site"
  # (the default), the table per site is also shown when there are at most
  # getOption("catchmentACS.summary_per_site_max") sites, as in
  # print.cacs_run_result().
  breakdown <- attr(x, "breakdown") %||% "cross_site"
  per_site_max <- getOption("catchmentACS.summary_per_site_max", 12L)
  show_cross <- breakdown %in% c("cross_site", "both")
  show_per   <- breakdown %in% c("per_site", "both") ||
                (identical(breakdown, "cross_site") &&
                 isTRUE(x$n_sites <= per_site_max) &&
                 isTRUE(per_site_max > 0L) &&
                 !is.null(x$rates_per_site) &&
                 nrow(x$rates_per_site) > 0L)

  if (show_cross) {
    cli::cli_h2("Rates breakdown (mean / sd / n_NA)")
    if (nrow(x$rates_breakdown) > 0L) {
      print(x$rates_breakdown)
    } else {
      cli::cli_text("(no rate variables found)")
    }
  }
  if (show_per) {
    cli::cli_h2("Rates per site")
    if (!is.null(x$rates_per_site) && nrow(x$rates_per_site) > 0L) {
      print(x$rates_per_site)
    } else {
      cli::cli_text("(no per-site rate data)")
    }
  }

  cli::cli_h2("MOE fallback rate")
  if (is.na(x$moe_fallback_rate)) {
    cli::cli_text("(no moe_fallback column available)")
  } else {
    cli::cli_text("{round(100 * x$moe_fallback_rate, 1)}% of rows fell back")
  }

  invisible(x)
}


#' Format a summary table as a Markdown table
#'
#' Formats a data frame as a Markdown pipe table for Quarto or R Markdown
#' documents and GitHub pages, using [knitr::kable()]. Numbers are rounded to
#' three decimals unless `digits` is given. The knitr package is required.
#'
#' A table with a `site_id` column and a column for at least one of the five
#' rates, such as `rates_per_site` or `rates_per_site_moe`, is changed in two
#' ways. Its columns are put in the order `site_id`, `drive_time_min` (if
#' present), the rates in the order of [`cacs_acs_default_rates`], and any
#' other columns. A caption is added unless `caption` is given. When the
#' cells are text, as in `rates_per_site_moe`, the caption says that they
#' show estimates with margins of error (half-widths of confidence
#' intervals), at the confidence level in the table's attribute
#' `cacs_confidence_level`, which [summary.cacs_run_result()] sets. For a
#' table without that attribute, the caption gives no level.
#'
#' @param summary_tbl A data frame, such as one of the elements
#'   `rates_breakdown`, `rates_per_site`, and `rates_per_site_moe` of a
#'   summary made by [summary.cacs_run_result()].
#' @param ... Arguments passed to [knitr::kable()], such as `digits`,
#'   `caption`, `col.names`, or `align`. The format is always `"pipe"`, so
#'   `format` gives an error.
#'
#' @return A character vector of class `knitr_kable` with the lines of the
#'   table, and of the caption if there is one. Printing it shows the table.
#' @family result summaries
#' @export
#'
#' @examples
#' if (requireNamespace("knitr", quietly = TRUE)) {
#'   # A made-up table shaped like the rates_breakdown element of a summary
#'   summary_tbl <- tibble::tibble(
#'     variable = "poverty_rate",
#'     mean = 0.1234,
#'     sd = 0.0567,
#'     n_NA = 0L
#'   )
#'   cacs_summary_as_markdown(summary_tbl)
#' }
cacs_summary_as_markdown <- function(summary_tbl, ...) {
  if (!inherits(summary_tbl, "data.frame")) {
    .cli_abort_schema(c(
      "{.arg summary_tbl} must be a data frame or tibble.",
      "x" = "Got {.cls {class(summary_tbl)[[1L]]}}."
    ))
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg knitr} is required to render summary Markdown.",
      "i" = "Install it with {.code install.packages(\"knitr\")}."
    ), class = c("catchmentACS_error_missing_suggest",
                 "catchmentACS_error",
                 "catchmentACS_condition"))
  }

  args <- c(list(x = summary_tbl, format = "pipe"), list(...))

  # A table with `site_id` and at least one rate column, such as
  # rates_per_site or rates_per_site_moe of a summary, gets its columns
  # reordered and, unless `caption` is given, a caption.
  cols <- colnames(summary_tbl)
  rate_vars <- names(cacs_acs_default_rates)
  is_per_site <- "site_id" %in% cols && any(rate_vars %in% cols)
  if (is_per_site) {
    # The type of the first rate column present (in the order of
    # cacs_acs_default_rates) chooses the caption: character cells hold
    # "estimate +/- margin" strings, as in rates_per_site_moe. The caption
    # names the confidence level that summary() records on that table, and
    # no level for a table without it.
    first_rate_col <- intersect(rate_vars, cols)[[1L]]
    first_vals <- summary_tbl[[first_rate_col]]
    is_moe_string <- is.character(first_vals)
    level <- attr(summary_tbl, "cacs_confidence_level", exact = TRUE)
    if (is.null(args$caption)) {
      args$caption <- if (!is_moe_string) {
        "Rates per site (estimate)"
      } else if (is.numeric(level) && length(level) == 1L && !is.na(level)) {
        sprintf("Rates per site (estimate \u00b1 %s%% MOE)", format(100 * level, digits = 15))
      } else {
        "Rates per site (estimate \u00b1 MOE)"
      }
    }
    # Columns: site_id, drive_time_min if present, the rates in the order of
    # cacs_acs_default_rates, then the others.
    key_cols <- c("site_id", intersect("drive_time_min", cols))
    cols_ordered <- c(key_cols,
                      intersect(rate_vars, cols),
                      setdiff(cols, c(key_cols, rate_vars)))
    args$x <- summary_tbl[, cols_ordered, drop = FALSE]
  }

  if (is.null(args$digits)) {
    args$digits <- 3
  }
  do.call(knitr::kable, args)
}


#' Format estimates and margins of error as "estimate +/- moe" strings
#'
#' Used by .cacs_rates_per_site_pivot() for its `wide_moe` table. Each
#' number is written with sprintf("%.*f"), so always with `digits` decimals
#' and never in scientific notation, and the two are joined by the
#' plus-minus sign (U+00B1), as in the caption of cacs_summary_as_markdown().
#' A missing value is written as "NA" (for example "NA +/- 0.100"); when both
#' values are missing, the result is NA.
#'
#' @param estimate A numeric vector of estimates.
#' @param moe A numeric vector of margins of error, of the same length.
#' @param digits A single non-negative number of decimals (3 by default),
#'   truncated to a whole number.
#'
#' @return A character vector of the same length as `estimate`.
#' @keywords internal
#' @noRd
.cacs_format_estimate_moe_string <- function(estimate, moe, digits = 3L) {
  # Check the inputs first, so the loop below can index both vectors by
  # position.
  if (length(estimate) != length(moe)) {
    stop(".cacs_format_estimate_moe_string(): length(estimate) must equal length(moe).",
         call. = FALSE)
  }
  if (length(digits) != 1L || !is.numeric(digits) || is.na(digits) ||
      !is.finite(digits) || digits < 0) {
    stop(".cacs_format_estimate_moe_string(): `digits` must be a non-negative scalar integer.",
         call. = FALSE)
  }
  digits <- as.integer(digits)

  n <- length(estimate)
  if (n == 0L) {
    return(character(0))
  }

  na_est <- is.na(estimate)
  na_moe <- is.na(moe)

  # Format each side once, vectorized; the loop only joins the two strings
  # and gives NA when both values are missing.
  est_str <- ifelse(na_est, "NA", sprintf("%.*f", digits, estimate))
  moe_str <- ifelse(na_moe, "NA", sprintf("%.*f", digits, moe))

  vapply(
    seq_len(n),
    function(i) {
      if (na_est[i] && na_moe[i]) NA_character_
      else paste0(est_str[i], " \u00b1 ", moe_str[i])
    },
    character(1)
  )
}


#' Fill the isochrone column of the list-column form
#'
#' For each row of `out`, puts the first row of `iso_sf` with the same
#' `site_id` and `drive_time_min` into the `isochrone` cell, as a one-row sf
#' object; rows without a match keep NULL. `out` is returned unchanged when
#' `iso_sf` is NULL or not an sf object, or when either lacks the columns
#' `site_id` and `drive_time_min`.
#'
#' When a cell is filled, the attribute `iso_was_filled = TRUE` is set on
#' `out`, and .pivot_to_list_column() then shows a message of class
#' `catchmentACS_message_listcol_iso_filled`. The attribute is not removed
#' afterwards, so it stays on the result of cacs_run().
#'
#' @param out The tibble built by .pivot_to_list_column(), with the columns
#'   `site_id`, `drive_time_min`, and `isochrone`.
#' @param iso_sf The drive-time areas, an sf object with the columns
#'   `site_id` and `drive_time_min`, or NULL.
#'
#' @return `out`, with the `isochrone` cells filled where a row of `iso_sf`
#'   matches.
#' @keywords internal
#' @noRd
.cacs_pipe_iso_to_list_column <- function(out, iso_sf = NULL) {
  if (is.null(iso_sf)) {
    return(out)
  }
  if (!all(c("site_id", "drive_time_min") %in% colnames(out))) {
    return(out)
  }
  if (!inherits(iso_sf, "sf")) {
    return(out)
  }
  iso_cols <- colnames(iso_sf)
  if (!all(c("site_id", "drive_time_min") %in% iso_cols)) {
    return(out)
  }

  # drive_time_min is integer in some inputs and double in others, so both
  # sides are compared as integers (values that cannot be converted become
  # NA without a warning), and site_id as character.
  iso_sites <- as.character(iso_sf$site_id)
  iso_dtm   <- suppressWarnings(as.integer(iso_sf$drive_time_min))
  out_sites <- as.character(out$site_id)
  out_dtm   <- suppressWarnings(as.integer(out$drive_time_min))

  any_filled <- FALSE
  isochrone_col <- if (is.list(out$isochrone)) out$isochrone else vector("list", nrow(out))

  for (i in seq_len(nrow(out))) {
    hit <- which(iso_sites == out_sites[[i]] &
                 iso_dtm   == out_dtm[[i]])
    if (length(hit) >= 1L) {
      # The first matching row, kept as a one-row sf object.
      isochrone_col[[i]] <- iso_sf[hit[[1L]], , drop = FALSE]
      any_filled <- TRUE
    }
  }
  out$isochrone <- isochrone_col

  if (any_filled) {
    attr(out, "iso_was_filled") <- TRUE
  }
  out
}


#' Rates per site and drive time, as wide tables
#'
#' Used by print.cacs_run_result() and summary.cacs_run_result(), so the two
#' show the same table. Keeps the rows whose `variable` is one of
#' `rates_names` and writes their estimates (and margins of error) into
#' matrices with one row per key (`site_id`, and `drive_time_min` when the
#' input has it) and one column per rate. Rows follow the first appearance
#' of each key in the input; a key and rate without a row give NA.
#'
#' @param run_result_long A result of cacs_run() in the long form, or a
#'   tibble with at least the columns `site_id`, `variable`, and `estimate`
#'   (`moe` and `drive_time_min` are optional).
#' @param rates_names A character vector of rate names, in the order of the
#'   output columns, or NULL (the default) for the five built-in rates,
#'   `names(.SANCTIONED_RATES_V1)`.
#'
#' @return A list of two tibbles, each with the key columns and one column
#'   per rate: `wide` with the estimates and `wide_moe` with
#'   "estimate +/- moe" strings from .cacs_format_estimate_moe_string().
#'   When the input has no `moe` column, `wide_moe` is NULL and the list has
#'   the attribute `moe_unavailable = TRUE`. Without rate rows, the tibbles
#'   have zero rows. Two rows for the same key and rate give an error.
#' @keywords internal
#' @noRd
.cacs_rates_per_site_pivot <- function(run_result_long, rates_names = NULL) {

  if (is.null(rates_names)) {
    rates_names <- names(.SANCTIONED_RATES_V1)
  }
  if (!is.character(rates_names) || length(rates_names) == 0L ||
      anyNA(rates_names) || any(!nzchar(rates_names))) {
    stop(".cacs_rates_per_site_pivot(): `rates_names` must be a non-empty character vector.",
         call. = FALSE)
  }
  rl_cols <- colnames(run_result_long)
  required_cols <- c("site_id", "variable", "estimate")
  missing_cols <- setdiff(required_cols, rl_cols)
  if (length(missing_cols) > 0L) {
    stop(
      "The result is missing the column(s) that print() and summary() need: ",
      paste(missing_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  has_moe <- "moe" %in% rl_cols
  has_drive_time <- "drive_time_min" %in% rl_cols
  key_cols <- c("site_id", if (has_drive_time) "drive_time_min" else NULL)

  n_rates <- length(rates_names)

  # Zero-row tables with the key columns and one column per rate (numeric in
  # `wide`, character in `wide_moe`), used when there is no rate row, either
  # because the input is empty or because it has no rate rows.
  build_empty <- function() {
    key_empty <- list(site_id = character(0))
    if (has_drive_time) {
      key_empty$drive_time_min <- run_result_long$drive_time_min[0]
    }
    cols <- c(
      key_empty,
      stats::setNames(replicate(n_rates, numeric(0), simplify = FALSE),
                      rates_names)
    )
    tibble::as_tibble(cols)
  }
  build_empty_moe <- function() {
    key_empty <- list(site_id = character(0))
    if (has_drive_time) {
      key_empty$drive_time_min <- run_result_long$drive_time_min[0]
    }
    cols <- c(
      key_empty,
      stats::setNames(replicate(n_rates, character(0), simplify = FALSE),
                      rates_names)
    )
    tibble::as_tibble(cols)
  }

  # Keep the rate rows and only the columns used below.
  keep_cols <- c(key_cols, "variable", "estimate",
                 if (has_moe) "moe" else NULL)
  is_rate_row <- run_result_long$variable %in% rates_names
  filtered <- run_result_long[is_rate_row, keep_cols, drop = FALSE]

  if (nrow(filtered) == 0L) {
    out <- list(
      wide     = build_empty(),
      wide_moe = if (has_moe) build_empty_moe() else NULL
    )
    if (!has_moe) attr(out, "moe_unavailable") <- TRUE
    return(out)
  }

  # Two rows for the same key and rate would write to the same matrix cell,
  # and there is no way to choose between them, so stop with an error.
  dup_data <- filtered[c(key_cols, "variable")]
  dup_key <- do.call(
    paste,
    c(lapply(dup_data, as.character), sep = "\r")
  )
  if (anyDuplicated(dup_key)) {
    stop(
      "The result has more than one row for the same values of ",
      paste(c(key_cols, "variable"), collapse = ", "),
      ". Keep one row for each; combining results, for example with dplyr::bind_rows(), can repeat rows.",
      call. = FALSE
    )
  }

  # Output rows follow the first appearance of each key in the input.
  key_data <- filtered[key_cols]
  key_id <- do.call(
    paste,
    c(lapply(key_data, as.character), sep = "\r")
  )
  key_keep <- !duplicated(key_id)
  key_tbl <- tibble::as_tibble(key_data[key_keep, , drop = FALSE])
  key_ids <- key_id[key_keep]
  n_keys <- length(key_ids)

  # One n_keys x n_rates matrix of estimates (and one of margins of error),
  # filled key by key; a key and rate without a row stay NA.
  est_mat <- matrix(
    NA_real_,
    nrow = n_keys, ncol = n_rates,
    dimnames = list(NULL, rates_names)
  )
  moe_mat <- if (has_moe) {
    matrix(NA_real_, nrow = n_keys, ncol = n_rates,
           dimnames = list(NULL, rates_names))
  } else {
    NULL
  }

  # split() on a factor keeps the order of its levels (first appearance); on
  # a character vector it would sort the keys.
  key_factor <- factor(key_id, levels = key_ids)
  row_idx_by_key <- split(seq_len(nrow(filtered)), key_factor)

  for (i in seq_len(n_keys)) {
    idx <- row_idx_by_key[[i]]
    if (length(idx) == 0L) next  # key has no rate row at all -> stays NA
    col_pos <- match(filtered$variable[idx], rates_names)
    # col_pos cannot be NA, because the rows were kept with
    # `variable %in% rates_names`; the check guards against a change there.
    good <- !is.na(col_pos)
    if (any(good)) {
      est_mat[i, col_pos[good]] <- filtered$estimate[idx][good]
      if (has_moe) {
        moe_mat[i, col_pos[good]] <- filtered$moe[idx][good]
      }
    }
  }

  # The tibbles are built from named columns: the key columns, then the rates
  # in the order of `rates_names`.
  wide_cols <- c(
    as.list(key_tbl),
    stats::setNames(
      lapply(seq_len(n_rates), function(j) est_mat[, j]),
      rates_names
    )
  )
  wide <- tibble::as_tibble(wide_cols)

  # One call of .cacs_format_estimate_moe_string() per rate column rather
  # than per cell.
  if (has_moe) {
    moe_cols_list <- lapply(seq_len(n_rates), function(j) {
      .cacs_format_estimate_moe_string(est_mat[, j], moe_mat[, j], digits = 3L)
    })
    wide_moe_cols <- c(
      as.list(key_tbl),
      stats::setNames(moe_cols_list, rates_names)
    )
    wide_moe <- tibble::as_tibble(wide_moe_cols)
  } else {
    wide_moe <- NULL
  }

  out <- list(wide = wide, wide_moe = wide_moe)
  if (!has_moe) attr(out, "moe_unavailable") <- TRUE
  out
}


#' Convert the result of `cacs_run()` to a plain tibble
#'
#' Removes the class `cacs_run_result` from a result of [cacs_run()], so that
#' the table prints as an ordinary tibble, and keeps the other attributes.
#' The rows keep their order in `x` unless `rate_first` is `TRUE`; the rows
#' are then sorted by `site_id`, `drive_time_min`, and `variable`, with the
#' rows of the five rates before the other rows within each site and drive
#' time.
#'
#' With `rate_first = NULL` (the default), the option
#' `catchmentACS.rate_first_default` decides, but only when `as_tibble()` is
#' called from code run in the global environment, such as the console, a
#' script, a document, or a function written in one of them. A conversion
#' made inside another package, such as dplyr, keeps the order, and so does a
#' call that passes the function itself, as in
#' `lapply(results, tibble::as_tibble)`. The first time the option puts the
#' rate rows first in an R session, a message says so.
#'
#' @param x A `cacs_run_result` object, as returned by [cacs_run()].
#' @param ... Passed to the data frame method of [tibble::as_tibble()], such
#'   as `.name_repair`.
#' @param rate_first A logical value: `TRUE` puts the rate rows first and
#'   `FALSE` keeps the order of the rows. With `NULL` (the default), the
#'   option `catchmentACS.rate_first_default`, `FALSE` unless it has been
#'   set, decides in the calls described above, and other calls keep the
#'   order. A result without a `variable` column, such as the list-column
#'   form, keeps its order either way.
#'
#' @return A tibble of class `c("tbl_df", "tbl", "data.frame")` with the
#'   rows, columns, and attributes of `x`.
#' @family result summaries
#' @method as_tibble cacs_run_result
#' @exportS3Method tibble::as_tibble cacs_run_result
#' @examples
#' out <- readRDS(system.file("extdata", "visual_walkthrough_fixture.rds",
#'                            package = "catchmentACS"))$run_result
#'
#' # By default the rows keep their order in out: the 14 ACS variables, then
#' # the five rates
#' tibble::as_tibble(out)$variable
#'
#' # The rates first
#' tibble::as_tibble(out, rate_first = TRUE)$variable
as_tibble.cacs_run_result <- function(x, ..., rate_first = NULL) {
  # Without rate_first, the option applies only to a call from code run in the
  # global environment (the console, a script, a document). dplyr and other
  # packages convert x internally and then use row positions of the converted
  # table on x itself, so a call from any package namespace keeps the order.
  # parent.frame() of an S3 method is the environment the generic was called
  # from.
  from_option <- is.null(rate_first)
  if (from_option) {
    rate_first <- isTRUE(getOption("catchmentACS.rate_first_default", FALSE)) &&
      identical(topenv(parent.frame()), globalenv())
  }

  # Remove the class first; otherwise tibble::as_tibble(x) would call this
  # method again.
  class(x) <- setdiff(class(x), "cacs_run_result")
  out <- tibble::as_tibble(x, ...)

  if (isTRUE(rate_first) &&
      all(c("site_id", "drive_time_min", "variable") %in% colnames(out)) &&
      nrow(out) > 0L) {
    rate_vars <- names(cacs_acs_default_rates)
    # Within each site and drive time, rate rows (0) come before the other
    # rows (1), then rows are ordered by variable.
    is_rate <- out$variable %in% rate_vars
    .rate_sort <- ifelse(is_rate, 0L, 1L)
    ord <- order(out$site_id,
                 out$drive_time_min,
                 .rate_sort,
                 out$variable)
    out <- out[ord, , drop = FALSE]
    if (from_option) {
      .cli_inform_rate_first_changed()  # message shown once per session
    }
  }

  out
}
