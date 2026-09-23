#' aaa-globals.R - the constants and column names used across the package.
#'
#' The values that the `match.arg()` calls and the checks of
#' `cacs_isochrone()`, `cacs_intersect_weight()`, `cacs_propagate_moe()`,
#' `cacs_derive_rates()` and `cacs_run()` accept, in one file so that the
#' same string is not typed again in each of them. The last block names the
#' columns that the dplyr, tidyr and sf calls use, which R CMD check would
#' otherwise report as undefined variables.
#'
#' @keywords internal
#' @noRd
NULL

# The value the Census Bureau uses for the 90 percent level. It divides a
# published margin of error to give a standard error, and multiplies a
# standard error to give a margin of error when `level` is 0.90.

.Z_ACS_90 <- 1.645

# The four formula names that the `moe_formula` arguments take and the
# moe_formula_requested and moe_formula_effective columns hold: weighted_sum
# for counts, weighted_mean for medians, per-person values and the other
# proxy kinds, and the proportion and ratio formulas for rates. The ratio
# formula, general_ratio_conservative, is the default for every rate.

.SANCTIONED_MOE_FORMULAS <- c(
  "weighted_sum",
  "weighted_mean",
  "proportion_subset",
  "general_ratio_conservative"
)

# The values `provider` takes. Each one has a helper of its own
# (.iso_via_osrm(), .iso_via_ors(), .iso_via_mapbox(), .iso_via_r5r()), but
# only OSRM and ORS are implemented; the other two give an error.

.SANCTIONED_PROVIDERS <- c("osrm", "ors", "mapbox", "r5r")

# The grid resolution used when `res` is not passed through to OSRM. A value
# the user passes is used as given.

.OSRM_RES_DEFAULT <- 70L

# The resolution used instead when the request goes to the public demo
# server (osrm_mode = "demo") and `res` is not passed through. A coarser grid
# makes fewer requests, so the first call is less likely to run into the
# server's request limit. With
# options(catchmentACS.osrm_demo_budget_protect = FALSE) the 70 above is used
# there too.

.OSRM_RES_DEFAULT_DEMO <- 30L

# The six options whose defaults R/zzz.R sets when the package loads:
#
#   catchmentACS.verbose (TRUE)
#     Read nowhere; the `verbose` argument of each function decides whether
#     it reports progress.
#   cacs.return_se (FALSE)
#     When TRUE, cacs_propagate_moe() adds an `se` column beside `moe`.
#   catchmentACS.summary_per_site_max (12L)
#     print() shows the block of rates per site only when the result has at
#     most this many sites; 0 for never, Inf for always.
#   catchmentACS.osrm_demo_budget_protect (TRUE)
#     See .OSRM_RES_DEFAULT_DEMO above.
#   catchmentACS.capture_return_value ("conditions")
#     The default of the `return_value` argument of
#     cacs_capture_conditions().
#   catchmentACS.rate_first_default (FALSE)
#     With TRUE, as_tibble.cacs_run_result() called from code run in the
#     global environment without `rate_first` puts the rate rows of a site
#     above its ACS rows.
#
# Other options the package reads have no default set at load; each call
# gives its own (catchmentACS.audit_rates, catchmentACS.cache_enabled,
# catchmentACS.cache_dir, catchmentACS.progress and others).

# The two OSRM addresses the package uses when none is given: the public demo
# server, and the local one for osrm_mode = "docker". The two chunk sizes are
# used by .osrm_request_budget() (R/isochrone-osrm.R) to estimate how many
# requests a site costs. They follow osrm::osrmIsochrone(), which asks the
# public demo server for 75 destinations at a time and waits a second after
# each full chunk, and any other server for more at a time with no wait.
# osrm 5.0.0 sends only the grid points within reach of the site, and takes
# 999 of them at a time from any other server, so the estimate is above the
# number of requests actually made on both servers.

.OSRM_PUBLIC_DEMO_SERVER <- "https://routing.openstreetmap.de/"
.OSRM_DOCKER_SERVER_DEFAULT <- "http://0.0.0.0:5000/"
.OSRM_PUBLIC_DEMO_CHUNK_SIZE <- 75L
.OSRM_CUSTOM_SERVER_CHUNK_SIZE <- 450L

# The classes of the error that a drive-time band input gives. The first two
# and the next two are pairs of names for the same error, kept so that
# handlers written for either name still catch it.

.ANNULUS_ERROR_CLASSES <- c(
  "catchmentACS_error_annulus_input",
  "cacs_error_annulus_input",
  "cacs_error_schema",
  "catchmentACS_error_schema",
  "catchmentACS_error",
  "catchmentACS_condition"
)

# The values `weight_method` takes. With "area", a tract's coverage weight is
# the share of its area inside the drive-time area. "population", which would
# weight by block-group population instead, is not implemented:
# cacs_intersect_weight() gives an error before it does any other work.

.SANCTIONED_WEIGHT_METHODS <- c("area", "population")

# The values the estimand_family column takes. It says what kind of estimate
# a row holds, and with it which formula gives the margin of error. The
# classifier in R/moe-helpers.R gives only spatial_total (a count),
# median_proxy, area_weighted_scalar_proxy (a per-person value) and
# metadata_only; derived_rate is written by cacs_derive_rates(), and the
# other two are accepted but never produced.

.ESTIMAND_FAMILIES <- c(
  "spatial_total",
  "derived_rate",
  "area_weighted_scalar_proxy",
  "population_weighted_scalar_proxy",
  "area_weighted_rate_proxy",
  "median_proxy",
  "metadata_only"
)

# The five rates, each with the ACS codes of its numerator and denominator.
# cacs_derive_rates() accepts only a `rates` argument identical to this list,
# so a list with a rate added, left out, renamed or given other codes gives
# an error. R/derive-rates.R exports a copy as cacs_acs_default_rates.

.SANCTIONED_RATES_V1 <- list(
  poverty_rate              = c(num = "B17001_002", den = "B17001_001"),
  snap_rate                 = c(num = "B22003_002", den = "B22003_001"),
  ssi_rate                  = c(num = "B19056_002", den = "B19056_001"),
  unemp_rate                = c(num = "B23025_005", den = "B23025_003"),
  labor_force_participation = c(num = "B23025_002", den = "B23025_001")
)

# The ranges of the check that options(catchmentACS.audit_rates = TRUE) turns
# on. They are rough plausibility bounds, not values a rate has to lie
# between, and a rate outside its range is reported in a warning and left as
# it is.

.SANCTIONED_RATE_BOUNDS_V1 <- list(
  poverty_rate              = c(min = 0.0, max = 0.60),
  snap_rate                 = c(min = 0.0, max = 0.50),
  ssi_rate                  = c(min = 0.0, max = 0.15),
  unemp_rate                = c(min = 0.0, max = 0.30),
  labor_force_participation = c(min = 0.3, max = 0.85)
)

# The formula `cacs_derive_rates()` uses for each rate with
# `formula_dispatch = "auto"`. The numerator of all five rates is part of its
# denominator, so the handbook's proportion formula would apply to all five.
# Only `poverty_rate` and `labor_force_participation` use it here;
# `snap_rate`, `ssi_rate` and `unemp_rate` keep the ratio formula.
# `unemp_rate` keeps it to reproduce the 2025 analysis the package was first
# written for.

.RATE_FORMULA_CATALOGUE <- c(
  poverty_rate              = "proportion_subset",
  snap_rate                 = "general_ratio_conservative",
  ssi_rate                  = "general_ratio_conservative",
  unemp_rate                = "general_ratio_conservative",
  labor_force_participation = "proportion_subset"
)

# The rates that formula_dispatch = "proportion_subset" computes with the
# proportion formula. The other three keep the ratio formula, with a warning
# that names them, and cacs_derive_rates() lists them in the
# formula_downgraded_rates element of cacs_rate_provenance.

.RATE_C1_SUBSET_ELIGIBLE <- c(
  poverty_rate              = TRUE,
  snap_rate                 = FALSE,
  ssi_rate                  = FALSE,
  unemp_rate                = FALSE,
  labor_force_participation = TRUE
)

# The names that cacs_run() passes on to each step through its `iso_args`,
# `acs_args`, `weight_args`, `moe_args` and `rate_args` lists.
# .validate_run_arg_list() (R/run.R) warns about any other name and drops it.
# The `rat` element is empty, so every name in `rate_args` is dropped; the
# two settings of that step have arguments of their own in cacs_run()
# (`formula_dispatch` and, for the step before it, `moe_formula`), and
# giving them inside the lists is an error rather than a dropped name.

.CACS_RUN_ARG_KEYS <- list(
  iso = c("osrm_mode", "ors_api_key", "mapbox_token",
          "r5r_core", "profile", "osm_snapshot_date", "res"),
  acs = c("survey", "geography", "force_refresh", "write_gpkg"),
  wgt = c("min_weight", "keep_tract_audit"),
  moe = c("level", "fallback_chain_max"),
  rat = character(0)
)

# The column names that the dplyr, tidyr and sf calls of the package use
# without quoting them. Naming them here keeps R CMD check from reporting
# "no visible binding for global variable" for each one. The list holds the
# 23 columns of the long table and the names below it.

utils::globalVariables(c(
  # dplyr pronoun
  ".data",
  # the 23 columns of the long table, and se, which
  # options(cacs.return_se = TRUE) adds
  "site_id", "drive_time_min", "variable", "estimate", "moe", "se",
  "weight_sum", "n_tracts", "n_tracts_num", "n_tracts_den",
  "ring_topology",
  "provider", "profile", "osm_snapshot_date",
  "acs_year", "weight_method", "estimand_family", "weight_basis",
  "moe_formula_requested", "moe_formula_effective", "moe_fallback",
  "moe_fallback_reason", "failure_origin", "weight_uncertainty_propagated",
  # columns of the ACS data from tidycensus
  "GEOID", "NAME", "geometry",
  # weights, per tract and per variable
  "coverage_wt", "mean_wt", "area_wt", "weight",
  # columns of the cacs_aggregation_carriers table
  "num_est", "num_var", "den_est", "den_var",
  # weighted totals and averages, and their variances
  "var_total_raw", "var_mean_raw", "est_total", "est_mean",
  # columns that say why a site and drive-time pair has no estimates
  "failure_reason", "isochrone_empty", "pair_invalid"
))
