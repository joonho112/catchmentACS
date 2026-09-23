#' catchmentACS: Isochrone-Based Area-Weighted ACS Aggregation
#'
#' @description
#' catchmentACS computes estimates for drive-time areas (isochrones) around
#' sites, such as Pre-K classrooms, by combining the American Community
#' Survey (ACS) 5-year estimates of the census tracts that overlap each area.
#' The tract estimates are weighted by area, and [cacs_intersect_weight()]
#' describes the weights and what they assume. The five rates in
#' [`cacs_acs_default_rates`] are ratios of the weighted counts. Each estimate
#' has a margin of error, the half-width of its confidence interval (90
#' percent by default). [cacs_propagate_moe()] and [cacs_derive_rates()]
#' describe how the margins of error are computed and what they assume. Every
#' result carries a record of the settings and inputs behind it, which
#' [cacs_describe()] prints.
#'
#' @details
#' [cacs_run()] runs five steps in one call, and each step can also be called
#' on its own:
#'
#' 1. [cacs_acs_prefetch()] downloads the ACS data for one state with
#'    tidycensus, which needs a Census API key.
#' 2. [cacs_isochrone()] builds the drive-time areas with the Open Source
#'    Routing Machine (OSRM, the default) or openrouteservice, which needs an
#'    API key.
#' 3. [cacs_intersect_weight()] combines the tract estimates for each area.
#' 4. [cacs_propagate_moe()] computes the margins of error at a chosen
#'    confidence level.
#' 5. [cacs_derive_rates()] computes the rates and their margins of error.
#'
#' [cacs_run()] skips the download when ACS data are supplied through `acs`,
#' and the routing when drive-time areas are supplied through
#' `precomputed_isochrones`. The `provider` values `"mapbox"` and `"r5r"` and
#' the `weight_method` value `"population"` are accepted but not implemented
#' yet. Building drive-time areas with Mapbox or r5r, or weighting by
#' population, gives an error.
#'
#' The articles (vignettes) below are on the package website,
#' <https://joonho112.github.io/catchmentACS/articles/>, and can also be
#' opened with `vignette()` when the package was installed with its
#' vignettes.
#'
#' - `getting-started`: a first run on the bundled example data.
#' - `alabama-tutorial`: the five steps one at a time, then tables for
#'   reports.
#' - `providers`: routing services, API keys, and runs without an internet
#'   connection.
#' - `visual-walkthrough`: maps of each step for one site.
#' - `methodology`: how the estimates and margins of error are computed.
#' - `theory-spatial-aggregation`, `theory-moe-propagation`, and
#'   `theory-derived-rates`: area weighting, margins of error, and rates in
#'   detail.
#' - `porting-v01-to-v03`, `porting-v03-to-v04`, and `porting-v04-to-v05`:
#'   what to change in code written for an earlier version.
#'
#' @seealso
#' - [summary.cacs_run_result()] and [cacs_describe()]: summaries of a result
#'   and of how it was produced.
#' - [cacs_plot_site_pipeline()]: maps of one site's drive-time areas,
#'   tracts, estimates, and rates.
#' - [cacs_set_cache()] and [cacs_cache_dir()]: turning on or off the cache,
#'   where the first three steps save their results, and where it is kept.
#' - [cacs_acs_validate()] and [cacs_validate_iso()]: checks of ACS data and
#'   drive-time areas from other sources.
#' - [`catchmentACS-conditions`]: the classes of the package's messages,
#'   warnings, and errors.
#'
#' @aliases catchmentACS-package
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
