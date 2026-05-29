#' catchmentACS: Isochrone-Based Area-Weighted ACS Aggregation
#'
#' @description
#' catchmentACS turns a set of point locations into drive-time catchment
#' areas and summarizes who lives inside them. It builds drive-time
#' isochrones around each point, intersects them with American Community
#' Survey (ACS) census tracts, and aggregates the tract estimates to each
#' catchment using area weighting. Margins of error are propagated through
#' the aggregation following U.S. Census Bureau guidance, and common
#' derived rates (such as poverty and unemployment) are computed with
#' their own margins of error. The package builds on the `tidycensus`,
#' `sf`, and `tigris` ecosystems.
#'
#' Routing providers are explicit about their status. OSRM and
#' OpenRouteService (ORS) are implemented; Mapbox and r5r are reserved and
#' fail loudly with an informative error until they are supported. Likewise,
#' area weighting is the supported aggregation method, while population
#' weighting is a reserved fail-loud extension.
#'
#' @details
#' The single entry point for most users is [cacs_run()], which orchestrates
#' the full pipeline in one call: prefetch ACS data, build isochrones,
#' intersect and weight, propagate margins of error, and derive rates. The
#' individual steps are also exported so you can run them separately or
#' supply your own intermediate inputs.
#'
#' To get started, see `vignette("getting-started")` and the worked
#' `vignette("alabama-tutorial")`. For routing providers, credentials, and
#' offline workflows, see `vignette("providers")`, and for an end-to-end
#' map-based tour see `vignette("visual-walkthrough")`. The
#' `vignette("methodology")` article documents the underlying theory,
#' including spatial area weighting, margin-of-error propagation, and the
#' derived-rate definitions.
#'
#' @seealso
#' The core pipeline functions:
#' [cacs_run()], [cacs_isochrone()], [cacs_acs_prefetch()],
#' [cacs_intersect_weight()], [cacs_propagate_moe()],
#' [cacs_derive_rates()].
#'
#' @references
#' U.S. Census Bureau. *Understanding and Using American Community Survey
#' Data* (American Community Survey handbook series), which describes the
#' approximation formulas used to propagate margins of error for derived
#' sums, proportions, and ratios.
#'
#' @aliases catchmentACS-package
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
