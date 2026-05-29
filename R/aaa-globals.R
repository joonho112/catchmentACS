#' Internal sanctioned constants and NSE variable bindings
#'
#' Single source of truth for every sanctioned enum referenced from the
#' public surface of catchmentACS. Every downstream call site (e.g. the
#' `match.arg()` invocations inside `cacs_isochrone()`, `cacs_intersect_weight()`,
#' `cacs_propagate_moe()`, `cacs_derive_rates()`, and `cacs_run()`) must
#' reference these constants rather than re-typing string literals, so that
#' an extension at v1.1 only has to touch this file.
#'
#' @keywords internal
#' @noRd
NULL

# ---------------------------------------------------------------------------
# Sec. 22.2 / Sec. 22.8 - ACS Bureau 90% confidence multiplier (rounded value).
# Used as the *input-side* divisor to convert MOE_j -> SE_j and as the
# default output-side z when `level = 0.90` (byte-identical with Sec. 21 output).
# ---------------------------------------------------------------------------

.Z_ACS_90 <- 1.645

# ---------------------------------------------------------------------------
# Sec. 22.2 - 4 sanctioned MOE formula families.
# Family A  = weighted_sum               (spatial_total)
# Family B  = weighted_mean              (*_scalar_proxy + median_proxy)
# Family C1 = proportion_subset          (derived_rate with true subset)
# Family C2 = general_ratio_conservative (derived_rate default fallback)
# ---------------------------------------------------------------------------

.SANCTIONED_MOE_FORMULAS <- c(
  "weighted_sum",
  "weighted_mean",
  "proportion_subset",
  "general_ratio_conservative"
)

# ---------------------------------------------------------------------------
# Sec. 19.4 - 4 sanctioned isochrone providers.
# Dispatches to .iso_via_osrm / .iso_via_ors / .iso_via_mapbox /
# .iso_via_r5r helpers. The current release implements OSRM + ORS; Mapbox/r5r
# are retained as fail-loud future-provider stubs so the enum itself stays
# locked at 4.
# ---------------------------------------------------------------------------

.SANCTIONED_PROVIDERS <- c("osrm", "ors", "mapbox", "r5r")

# ---------------------------------------------------------------------------
# v0.3 UF-2 - effective OSRM default for omitted `res`.
# Explicit `res` values remain user-controlled; this constant is only the
# package default inserted when OSRM is called without a `res` passthrough.
# ---------------------------------------------------------------------------

.OSRM_RES_DEFAULT <- 70L

# ---------------------------------------------------------------------------
# v0.4 Issue 002 - demo-safe OSRM default for omitted `res` when
# `osrm_mode = "demo"`. Keeps the v0.3 70L default for `osrm_mode = "docker"`
# (local OSRM) while down-resolving the public-demo path to a lighter
# request budget that does not exhaust per-minute quota on the first call.
# Gated by option `catchmentACS.osrm_demo_budget_protect` (default TRUE).
# See plan §sec-dec-002 and Issue 002 qmd.
# ---------------------------------------------------------------------------

.OSRM_RES_DEFAULT_DEMO <- 30L

# ---------------------------------------------------------------------------
# v0.4 - package options (registered in R/zzz.R `.onLoad()`).
#
#   catchmentACS.summary_per_site_max (default 12L) — Issue 004.
#     Auto-print gate for the per-site rate block in print.cacs_run_result()
#     and print.cacs_run_summary(). Set to 0L to disable, Inf to always.
#   catchmentACS.osrm_demo_budget_protect (default TRUE) — Issue 002.
#     When TRUE and osrm_mode == "demo" and `res` omitted, downgrade to
#     .OSRM_RES_DEFAULT_DEMO (30L) and emit a once-per-session classed
#     message. When FALSE, omitted res always resolves to 70L (v0.3 behavior).
#   catchmentACS.capture_return_value (default "conditions") — Issue 001.
#     Default for cacs_capture_conditions(return_value =). When "both",
#     returns list(result = <expr value>, conditions = <tibble>).
#   catchmentACS.rate_first_default (default TRUE) — Issue 004.
#     Default for as_tibble.cacs_run_result(rate_first =). When TRUE,
#     rate rows surface above source ACS rows within each site block.
#
# Plus 2 prior v0.3 options (kept):
#   catchmentACS.audit_rates (default FALSE) — Issue #12.
#   catchmentACS.verbose (default TRUE) — Sec. 13.5.
#   cacs.return_se (default FALSE) — Sec. 22.2.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# v0.3 Phase 6 - effective OSRM routing endpoints and request-budget constants.
# The upstream `osrm` package throttles routing.openstreetmap.de isochrone table
# chunks with a 1-second sleep per full 75-destination chunk. Self-hosted/custom
# endpoints use the package's 450-destination chunk path without forced sleep.
# ---------------------------------------------------------------------------

.OSRM_PUBLIC_DEMO_SERVER <- "https://routing.openstreetmap.de/"
.OSRM_DOCKER_SERVER_DEFAULT <- "http://0.0.0.0:5000/"
.OSRM_PUBLIC_DEMO_CHUNK_SIZE <- 75L
.OSRM_CUSTOM_SERVER_CHUNK_SIZE <- 450L

# ---------------------------------------------------------------------------
# v0.3 UF-1 - annulus-input error class chain.
# Keep legacy aliases adjacent to the canonical class names so the
# backward-compat handler contract is visible in one place.
# ---------------------------------------------------------------------------

.ANNULUS_ERROR_CLASSES <- c(
  "catchmentACS_error_annulus_input",
  "cacs_error_annulus_input",
  "cacs_error_schema",
  "catchmentACS_error_schema",
  "catchmentACS_error",
  "catchmentACS_condition"
)

# ---------------------------------------------------------------------------
# Sec. 21.4 / Sec. 21.5 - 2 sanctioned weight methods.
# "area"       -> coverage_wt = int_area / tract_area (universal first product)
# "population" -> block-group population-weighted variant (future-release
#                extension; current release fails loud).
# ---------------------------------------------------------------------------

.SANCTIONED_WEIGHT_METHODS <- c("area", "population")

# ---------------------------------------------------------------------------
# Sec. 7.1 / Sec. 21.5 - sanctioned estimand families.
# ---------------------------------------------------------------------------

.ESTIMAND_FAMILIES <- c(
  "spatial_total",
  "derived_rate",
  "area_weighted_scalar_proxy",
  "population_weighted_scalar_proxy",
  "area_weighted_rate_proxy",
  "median_proxy",
  "metadata_only"
)

# ---------------------------------------------------------------------------
# Sec. 23.2 - v1.0 curated rate catalogue (whitelist, fail-loud).
# Each entry is a 2-element character vector with names "num" and "den"
# carrying the ACS table cell codes that constitute the numerator and
# denominator of the rate. `identical(rates, .SANCTIONED_RATES_V1)` must
# hold inside cacs_derive_rates(); any deviation aborts with E-23-04.
# Custom ratios are deferred to cacs_derive_rates_custom() at v1.1.
# ---------------------------------------------------------------------------

.SANCTIONED_RATES_V1 <- list(
  poverty_rate              = c(num = "B17001_002", den = "B17001_001"),
  snap_rate                 = c(num = "B22003_002", den = "B22003_001"),
  ssi_rate                  = c(num = "B19056_002", den = "B19056_001"),
  unemp_rate                = c(num = "B23025_005", den = "B23025_003"),
  labor_force_participation = c(num = "B23025_002", den = "B23025_001")
)

# ---------------------------------------------------------------------------
# v0.3 Step 4.1 - warning-only bounds for opt-in derived-rate audits.
# These are development guardrails, not hard validity rules. They are used only
# when `getOption("catchmentACS.audit_rates", FALSE)` is TRUE.
# ---------------------------------------------------------------------------

.SANCTIONED_RATE_BOUNDS_V1 <- list(
  poverty_rate              = c(min = 0.0, max = 0.60),
  snap_rate                 = c(min = 0.0, max = 0.50),
  ssi_rate                  = c(min = 0.0, max = 0.15),
  unemp_rate                = c(min = 0.0, max = 0.30),
  labor_force_participation = c(min = 0.3, max = 0.85)
)

# ---------------------------------------------------------------------------
# Sec. 23.4 - rate-family dispatch under `formula_dispatch = "auto"`.
# Maps each sanctioned rate name to the MOE formula family that
# `cacs_propagate_moe()` will apply for that rate. unemp_rate is C2 at
# the 2025 baseline (Q23-1 transition to C1 deferred to v1.1).
# ---------------------------------------------------------------------------

.RATE_FORMULA_CATALOGUE <- c(
  poverty_rate              = "proportion_subset",
  snap_rate                 = "general_ratio_conservative",
  ssi_rate                  = "general_ratio_conservative",
  unemp_rate                = "general_ratio_conservative",
  labor_force_participation = "proportion_subset"
)

.RATE_C1_SUBSET_ELIGIBLE <- c(
  poverty_rate              = TRUE,
  snap_rate                 = FALSE,
  ssi_rate                  = FALSE,
  unemp_rate                = FALSE,
  labor_force_participation = TRUE
)

# ---------------------------------------------------------------------------
# Sec. 24.4 - explicit sub-call argument keys for the cacs_run() orchestrator.
# Top-level public arguments (`moe_formula`, `formula_dispatch`) must be
# used instead of `moe_args$formula` / `rate_args$formula_dispatch`;
# .validate_run_arg_list() enforces this and emits W-24-01 on violations.
# rat is reserved as character(0) so v1.1 can extend the surface without
# a breaking change.
# ---------------------------------------------------------------------------

.CACS_RUN_ARG_KEYS <- list(
  iso = c("osrm_mode", "ors_api_key", "mapbox_token",
          "r5r_core", "profile", "osm_snapshot_date", "res"),
  acs = c("survey", "geography", "force_refresh", "write_gpkg"),
  wgt = c("min_weight", "keep_tract_audit"),
  moe = c("level", "fallback_chain_max"),
  rat = character(0)
)

# ---------------------------------------------------------------------------
# Sec. 12.3.1 + Sec. 19.5 + Sec. 20.5 + Sec. 21.5 + Sec. 21.7 + Sec. 22.3 - NSE column bindings
# for dplyr / tidyr / sf verbs across the package. Silences R CMD check's
# "no visible binding for global variable" NOTE.
#
# Inventory:
#   - 21 canonical long-format columns (Sec. 12.3.1 long schema 20 + se optional)
#   - 4 carrier columns (Sec. 21.7: num_est, num_var, den_est, den_var)
#   - 4 Sec. 22.3 intermediates (var_total_raw, var_mean_raw, est_total, est_mean)
#   - 3 Sec. 21.5 weight basis columns (coverage_wt, mean_wt, area_wt)
#   - 4 spatial/sf helpers (.data, GEOID, NAME, geometry)
#   - 2 Sec. 19/Sec. 24 failure carriers (failure_reason, isochrone_empty, pair_invalid)
# = ~38 elements total covering the Sec. 36.5 superset.
# ---------------------------------------------------------------------------

utils::globalVariables(c(
  # dplyr internals
  ".data",
  # Sec. 12.3.1 canonical long-format columns + v0.2 F4 / v0.3 topology
  "site_id", "drive_time_min", "variable", "estimate", "moe", "se",
  "weight_sum", "n_tracts", "n_tracts_num", "n_tracts_den",
  "ring_topology",
  "provider", "profile", "osm_snapshot_date",
  "acs_year", "weight_method", "estimand_family", "weight_basis",
  "moe_formula_requested", "moe_formula_effective", "moe_fallback",
  "moe_fallback_reason", "failure_origin", "weight_uncertainty_propagated",
  # Sec. 6 / Sec. 20.5 ACS sf helpers
  "GEOID", "NAME", "geometry",
  # Sec. 21.5 weight basis (per-row carriers)
  "coverage_wt", "mean_wt", "area_wt", "weight",
  # Sec. 21.7 carrier attribute columns (joined into batch tibble)
  "num_est", "num_var", "den_est", "den_var",
  # Sec. 22.3 intermediate columns
  "var_total_raw", "var_mean_raw", "est_total", "est_mean",
  # Sec. 19 / Sec. 24 failure carriers
  "failure_reason", "isochrone_empty", "pair_invalid"
))
