# Shared synthetic rate fixtures for Phase 4 formula lock tests.

.phase4_rate_values <- function(...) {
  vals <- c(
    B17001_002 = 150,  B17001_001 = 1000,
    B22003_002 = 100,  B22003_001 = 800,
    B19056_002 = 25,   B19056_001 = 500,
    B11001_001 = 800,
    B23025_005 = 48,   B23025_003 = 600,
    B23025_002 = 650,  B23025_001 = 1000
  )
  overrides <- c(...)
  if (length(overrides) > 0L) {
    vals[names(overrides)] <- overrides
  }
  vals
}

.phase4_rate_vars <- function() {
  c(
    "B17001_002", "B17001_001",
    "B22003_002", "B22003_001",
    "B19056_002", "B19056_001", "B11001_001",
    "B23025_005", "B23025_003",
    "B23025_002", "B23025_001"
  )
}

.phase4_rate_fixture <- function(values = .phase4_rate_values(),
                                 drop = character()) {
  vars <- setdiff(.phase4_rate_vars(), drop)
  est <- unname(values[vars])
  names(est) <- vars
  variance <- pmax(abs(est) / 4, 1)

  carriers <- tibble::tibble(
    site_id         = rep("S01", length(vars)),
    drive_time_min  = rep(15L, length(vars)),
    variable        = vars,
    estimand_family = rep("spatial_total", length(vars)),
    est_total       = as.numeric(est),
    var_total_raw   = as.numeric(variance),
    est_mean        = rep(NA_real_, length(vars)),
    var_mean_raw    = rep(NA_real_, length(vars)),
    weight_sum      = rep(0.9, length(vars)),
    n_tracts        = seq_along(vars)
  )

  long <- tibble::tibble(
    site_id                       = "S01",
    drive_time_min                = 15L,
    ring_topology                 = "cumulative",
    variable                      = "B17001_002",
    estimate                      = as.numeric(values[["B17001_002"]]),
    moe                           = 1.645 * sqrt(abs(values[["B17001_002"]]) / 4),
    weight_sum                    = 0.9,
    n_tracts                      = 1L,
    n_tracts_num                  = NA_integer_,
    n_tracts_den                  = NA_integer_,
    provider                      = "osrm",
    profile                       = "car",
    osm_snapshot_date             = "2026-01-01",
    acs_year                      = 2023L,
    weight_method                 = "area",
    estimand_family               = "spatial_total",
    weight_basis                  = "coverage",
    moe_formula_requested         = "weighted_sum",
    moe_formula_effective         = "weighted_sum",
    moe_fallback                  = FALSE,
    moe_fallback_reason           = "n/a",
    failure_origin                = "none",
    weight_uncertainty_propagated = FALSE
  )

  attr(long, "cacs_aggregation_carriers") <- carriers
  attr(long, "cacs_schema_version") <- "1.0"
  attr(long, "cacs_confidence_level") <- 0.90
  long
}

.phase4_derive <- function(values = .phase4_rate_values(),
                           drop = character(),
                           formula_dispatch = "general_ratio_conservative",
                           audit = FALSE) {
  withr::with_options(
    list(catchmentACS.audit_rates = audit),
    suppressWarnings(
      cacs_derive_rates(
        .phase4_rate_fixture(values = values, drop = drop),
        formula_dispatch = formula_dispatch
      )
    )
  )
}

.phase4_derive_with_warnings <- function(values = .phase4_rate_values(),
                                         drop = character(),
                                         formula_dispatch =
                                           "general_ratio_conservative",
                                         audit = FALSE) {
  warnings <- list()
  out <- withr::with_options(
    list(catchmentACS.audit_rates = audit),
    withCallingHandlers(
      cacs_derive_rates(
        .phase4_rate_fixture(values = values, drop = drop),
        formula_dispatch = formula_dispatch
      ),
      warning = function(w) {
        warnings[[length(warnings) + 1L]] <<- w
        invokeRestart("muffleWarning")
      }
    )
  )
  list(out = out, warnings = warnings)
}

.phase4_rate_row <- function(out, variable) {
  row <- out[out$variable == variable & out$estimand_family == "derived_rate", ]
  testthat::expect_equal(nrow(row), 1L)
  row
}

.phase4_rate_estimates <- function(out) {
  rows <- out[out$variable %in% names(cacs_acs_default_rates) &
                out$estimand_family == "derived_rate", ]
  stats::setNames(rows$estimate, rows$variable)
}

.phase4_required_rate_codes <- function() {
  sort(unique(unname(unlist(cacs_acs_default_rates, use.names = FALSE))))
}

