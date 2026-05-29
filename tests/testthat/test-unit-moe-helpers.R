# ============================================================================
# Unit tests for R/moe-helpers.R primitives (Phase 2.4).
# 15 cases per Sec. 37.6 + 3 robustness extras.
# ============================================================================


# ---- 5 MOE family primitives -----------------------------------------------

test_that("T-MOE-01 .moe_sum_self happy path (Family A, w=1)", {
  # Known answer: moe = [10, 10, 10], z = 1.645
  # Expected: 1.645 * sqrt(3 * (10/1.645)^2) = 10 * sqrt(3) approx 17.3205
  result <- .moe_sum_self(c(10, 10, 10), z = 1.645)
  expect_equal(result, 10 * sqrt(3), tolerance = 1e-9)
})

test_that("T-MOE-02 .moe_wsum_self happy path (Family A weighted)", {
  # moe = [10, 20], w = [0.5, 0.5], z = 1.645
  # Expected: 1.645 * sqrt((0.5*10/1.645)^2 + (0.5*20/1.645)^2)
  result <- .moe_wsum_self(c(10, 20), c(0.5, 0.5), z = 1.645)
  expected <- 1.645 * sqrt((0.5 * 10 / 1.645)^2 + (0.5 * 20 / 1.645)^2)
  expect_equal(result, expected, tolerance = 1e-9)
})

test_that("T-MOE-03 .moe_wmean_self happy path (Family B)", {
  # moe = [10, 20], w = [0.5, 0.5]
  # Expected: .moe_wsum_self / sum(w) = computed / 1
  result <- .moe_wmean_self(c(10, 20), c(0.5, 0.5), z = 1.645)
  expected <- 1.645 * sqrt((0.5 * 10 / 1.645)^2 + (0.5 * 20 / 1.645)^2) / 1
  expect_equal(result, expected, tolerance = 1e-9)
})

test_that("T-MOE-04 .moe_prop_self C1 success (Sec. 22.10 T22-09)", {
  # T22-09: num_est=100, num_moe=10, den_est=200, den_moe=15
  # Var_A = (10/1.645)^2, Var_B = (15/1.645)^2, p = 0.5
  # inside = Var_A - 0.25 * Var_B > 0 -> C1 success
  result <- .moe_prop_self(num_est = 100, num_moe = 10,
                           den_est = 200, den_moe = 15,
                           z = 1.645)
  expect_false(result$inside_negative)
  expect_false(result$zero_denominator)
  expect_false(result$missing_moe)
  Var_A <- (10 / 1.645)^2
  Var_B <- (15 / 1.645)^2
  p     <- 0.5
  inside <- Var_A - p^2 * Var_B
  expected_value <- 1.645 * sqrt(inside) / 200
  expect_equal(result$value, expected_value, tolerance = 1e-9)
})

test_that("T-MOE-05 .moe_ratio_self happy path (Family C2)", {
  # Same inputs as C1 but always works (inside = Var_A + p^2*Var_B)
  result <- .moe_ratio_self(num_est = 100, num_moe = 10,
                            den_est = 200, den_moe = 15,
                            z = 1.645)
  Var_A <- (10 / 1.645)^2
  Var_B <- (15 / 1.645)^2
  p     <- 0.5
  inside <- Var_A + p^2 * Var_B
  expected <- 1.645 * sqrt(inside) / 200
  expect_equal(result, expected, tolerance = 1e-9)
})


# ---- Sentinel signal tests --------------------------------------------------

test_that("T-MOE-06 .moe_prop_self negative variance returns inside_negative = TRUE", {
  # Engineered inputs so Var_A < p^2 * Var_B
  # num_est=100, num_moe=2 (tiny), den_est=200, den_moe=50 (huge), p=0.5
  result <- .moe_prop_self(num_est = 100, num_moe = 2,
                           den_est = 200, den_moe = 50)
  expect_true(result$inside_negative)
  expect_true(is.na(result$value))
})

test_that("T-MOE-07 .moe_prop_self zero denominator returns zero_denominator sentinel", {
  result <- .moe_prop_self(num_est = 100, num_moe = 10,
                           den_est = 0, den_moe = 15)
  expect_true(result$zero_denominator)
  expect_true(is.na(result$value))
})

test_that("T-MOE-08 .moe_prop_self missing moe returns missing_moe sentinel", {
  result <- .moe_prop_self(num_est = 100, num_moe = NA,
                           den_est = 200, den_moe = 15)
  expect_true(result$missing_moe)
  expect_true(is.na(result$value))
})


# ---- NA handling ------------------------------------------------------------

test_that("T-MOE-09 missing MOE propagates to NA uncertainty", {
  expect_true(is.na(.moe_sum_self(c(10, NA, 20), z = 1.645)))
  expect_true(is.na(.moe_wsum_self(c(10, NA, 20), c(0.5, 0.5, 0.5), z = 1.645)))
  expect_true(is.na(.moe_wmean_self(c(10, NA, 20), c(0.5, 0.5, 0.5), z = 1.645)))
})

test_that("T-MOE-09b missing weights and length mismatches are not silently dropped", {
  expect_true(is.na(.moe_wsum_self(c(10, 20), c(0.5, NA), z = 1.645)))
  expect_true(is.na(.moe_wmean_self(c(10, 20), c(0.5, NA), z = 1.645)))
  expect_error(
    .moe_wsum_self(c(10, 20), c(0.5), z = 1.645),
    class = "catchmentACS_error_schema"
  )
})


# ---- Converters (Tier-2 exported) -------------------------------------------

test_that("T-MOE-10 cacs_se_to_moe roundtrip with cacs_moe_to_se", {
  se_in <- c(5, 10, 15)
  moe   <- cacs_se_to_moe(se_in, level = 0.90)
  expect_equal(moe, 1.645 * se_in, tolerance = 1e-9)
  se_back <- cacs_moe_to_se(moe, level = 0.90)
  expect_equal(se_back, se_in, tolerance = 1e-12)
})

test_that("T-MOE-11 cacs_se_to_moe level=0.95 uses qnorm", {
  # z = qnorm(0.975) approx 1.95996
  moe <- cacs_se_to_moe(10, level = 0.95)
  expect_equal(moe, qnorm(0.975) * 10, tolerance = 1e-9)
})


# ---- Classifiers ------------------------------------------------------------

test_that("T-MOE-12 .classify_acs_variable maps count tables to spatial_total", {
  expect_equal(.classify_acs_variable("B17001_002"), "spatial_total")
  expect_equal(.classify_acs_variable("B22003_001"), "spatial_total")
  expect_equal(.classify_acs_variable("B23025_005"), "spatial_total")
})

test_that("T-MOE-13 .classify_acs_variable maps median tables to median_proxy", {
  expect_equal(.classify_acs_variable("B19013_001"), "median_proxy")
  expect_equal(.classify_acs_variable("B25077_001"), "median_proxy")  # regex fallback
})

test_that("T-MOE-14 .classify_acs_variable maps per-capita to area_weighted_scalar_proxy", {
  expect_equal(.classify_acs_variable("B19301_001"), "area_weighted_scalar_proxy")
})

test_that("T-MOE-15 .classify_acs_variable unknown -> metadata_only", {
  expect_equal(.classify_acs_variable("NOT_AN_ACS_CODE"), "metadata_only")
  expect_equal(.classify_acs_variable(NA_character_), "metadata_only")
  expect_equal(.classify_acs_variable(""), "metadata_only")
})

test_that("T-MOE-16 .classify_acs_variable_batch vectorized", {
  result <- .classify_acs_variable_batch(c("B17001_002", "B19013_001", "UNKNOWN"))
  expect_equal(result, c("spatial_total", "median_proxy", "metadata_only"))
})

test_that("T-MOE-17 .classify_acs_variable covers all 14 default vars", {
  # cacs_acs_default_vars from Step 1.4 plus v0.3 SSI numerator.
  default_codes <- c("B01003_001", "B17001_001", "B17001_002", "B19013_001",
                     "B19301_001", "B11001_001", "B22003_001", "B22003_002",
                     "B19056_001", "B19056_002", "B23025_001", "B23025_002",
                     "B23025_003", "B23025_005")
  classified <- .classify_acs_variable_batch(default_codes)
  expect_length(classified, 14L)
  expect_true(all(classified %in% c("spatial_total", "median_proxy",
                                     "area_weighted_scalar_proxy")))
  expect_false(any(classified == "metadata_only"))
})

test_that("T-MOE-18 .moe_wmean_self zero weight sum returns NA", {
  expect_true(is.na(.moe_wmean_self(c(10, 20), c(0, 0), z = 1.645)))
})

test_that("T-MOE-19 .moe_prop_self missing estimates return missing_estimate sentinel", {
  result_num <- .moe_prop_self(num_est = NA_real_, num_moe = 10,
                               den_est = 200, den_moe = 15)
  expect_true(result_num$missing_estimate)
  expect_false(result_num$zero_denominator)
  expect_true(is.na(result_num$value))

  result_den <- .moe_prop_self(num_est = 100, num_moe = 10,
                               den_est = NA_real_, den_moe = 15)
  expect_true(result_den$missing_estimate)
  expect_false(result_den$zero_denominator)
  expect_true(is.na(result_den$value))
})


# ============================================================================
# Phase 6.1 — cacs_propagate_moe() Steps 1-5 (validation + Family A/B
# vectorized). 12 cases T22-01..T22-08 + T22-13..T22-16 per Sec. 22.10.
# ============================================================================


# ---- Fixture builder -------------------------------------------------------
#
# Build a minimal canonical 20-col long tibble + 9-col carrier attribute so
# every Phase 6.1 test can exercise the public entry without needing the
# upstream `cacs_intersect_weight()` pipeline. The fixture covers two
# Family A rows (spatial_total counts) and one Family B row (median proxy)
# at a single (site_id, drive_time_min) pair; tests that exercise C1/C2
# rows add a derived_rate row on top.
#
# Carriers carry the precomputed `var_total_raw` and `var_mean_raw` values
# the propagator multiplies by `z` to produce the output MOE.
.make_phase6_fixture <- function(add_rate_row = FALSE) {

  base_long <- tibble::tibble(
    site_id                       = c("S01", "S01", "S01"),
    drive_time_min                = c(15L,   15L,   15L),
    ring_topology                 = rep("cumulative", 3L),
    variable                      = c("B17001_002", "B17001_001", "B19013_001"),
    estimate                      = c(50,    200,   65000),
    moe                           = c(NA_real_, NA_real_, NA_real_),
    weight_sum                    = c(0.8, 0.8, 0.8),
    n_tracts                      = c(2L, 2L, 2L),
    n_tracts_num                  = rep(NA_integer_, 3L),
    n_tracts_den                  = rep(NA_integer_, 3L),
    provider                      = c("osrm", "osrm", "osrm"),
    profile                       = c("car", "car", "car"),
    osm_snapshot_date             = c("2026-01-01", "2026-01-01", "2026-01-01"),
    acs_year                      = c(2023L, 2023L, 2023L),
    weight_method                 = c("area", "area", "area"),
    estimand_family               = c("spatial_total", "spatial_total", "median_proxy"),
    weight_basis                  = c("coverage", "coverage", "area_mean"),
    moe_formula_requested         = c("weighted_sum", "weighted_sum", "weighted_mean"),
    moe_formula_effective         = c("weighted_sum", "weighted_sum", "weighted_mean"),
    moe_fallback                  = c(FALSE, FALSE, FALSE),
    moe_fallback_reason           = c("n/a", "n/a", "n/a"),
    failure_origin                = c("none", "none", "none"),
    weight_uncertainty_propagated = c(FALSE, FALSE, FALSE)
  )

  base_carrier <- tibble::tibble(
    site_id         = c("S01", "S01", "S01"),
    drive_time_min  = c(15L,   15L,   15L),
    variable        = c("B17001_002", "B17001_001", "B19013_001"),
    estimand_family = c("spatial_total", "spatial_total", "median_proxy"),
    # Known answers: Family A row 1 var_total_raw = 100 -> MOE = 1.645 * 10 = 16.45
    #                Family A row 2 var_total_raw = 400 -> MOE = 1.645 * 20 = 32.90
    #                Family B (median proxy) var_mean_raw = 25 -> MOE = 1.645 * 5 = 8.225
    est_total       = c(50,    200,   NA_real_),
    var_total_raw   = c(100,   400,   NA_real_),
    est_mean        = c(NA_real_, NA_real_, 65000),
    var_mean_raw    = c(NA_real_, NA_real_, 25),
    weight_sum      = c(0.8, 0.8, 0.8),
    n_tracts        = c(2L, 2L, 2L)
  )

  if (isTRUE(add_rate_row)) {
    rate_long <- tibble::tibble(
      site_id                       = "S01",
      drive_time_min                = 15L,
      ring_topology                 = "cumulative",
      variable                      = "poverty_rate",
      estimate                      = NA_real_,
      moe                           = NA_real_,
      weight_sum                    = 0.8,
      n_tracts                      = 2L,
      n_tracts_num                  = 2L,
      n_tracts_den                  = 2L,
      provider                      = "osrm",
      profile                       = "car",
      osm_snapshot_date             = "2026-01-01",
      acs_year                      = 2023L,
      weight_method                 = "area",
      estimand_family               = "derived_rate",
      weight_basis                  = "coverage",
      moe_formula_requested         = "general_ratio_conservative",
      moe_formula_effective         = "general_ratio_conservative",
      moe_fallback                  = FALSE,
      moe_fallback_reason           = "n/a",
      failure_origin                = "none",
      weight_uncertainty_propagated = FALSE
    )
    base_long <- dplyr::bind_rows(base_long, rate_long)
    # Mirror the rate row in the carrier tibble so
    # `.validate_intersect_output()` finds a key match. The actual rate
    # carrier values are computed by Phase 6.3 from num/den lookups; for
    # Phase 6.1 only the key tuple needs to exist.
    rate_carrier <- tibble::tibble(
      site_id         = "S01",
      drive_time_min  = 15L,
      variable        = "poverty_rate",
      estimand_family = "derived_rate",
      est_total       = NA_real_,
      var_total_raw   = NA_real_,
      est_mean        = NA_real_,
      var_mean_raw    = NA_real_,
      weight_sum      = 0.8,
      n_tracts        = NA_integer_
    )
    base_carrier <- dplyr::bind_rows(base_carrier, rate_carrier)
  }

  attr(base_long, "cacs_aggregation_carriers") <- base_carrier
  attr(base_long, "cacs_schema_version")       <- "1.0"
  base_long
}


# ---- T22-01 ----------------------------------------------------------------

test_that("T22-01 cacs_propagate_moe() aborts on non-tbl input", {
  expect_error(
    cacs_propagate_moe(list(site_id = "S01")),
    class = "catchmentACS_error_schema"
  )
  expect_error(
    cacs_propagate_moe(as.data.frame(.make_phase6_fixture())),
    class = "catchmentACS_error_schema"
  )
})


# ---- T22-02 ----------------------------------------------------------------

test_that("T22-02 cacs_propagate_moe() aborts when carrier attribute missing", {
  data <- .make_phase6_fixture()
  attr(data, "cacs_aggregation_carriers") <- NULL
  expect_error(
    cacs_propagate_moe(data),
    class = "catchmentACS_error_schema"
  )
})


test_that("T22-02b cacs_propagate_moe() preserves empty-intersection sentinel rows", {
  data <- tibble::tibble(
    site_id                       = "S_EMPTY",
    drive_time_min                = 5L,
    ring_topology                 = "cumulative",
    variable                      = NA_character_,
    estimate                      = NA_real_,
    moe                           = NA_real_,
    weight_sum                    = NA_real_,
    n_tracts                      = 0L,
    n_tracts_num                  = NA_integer_,
    n_tracts_den                  = NA_integer_,
    provider                      = "osrm",
    profile                       = "car",
    osm_snapshot_date             = "2026-01-01",
    acs_year                      = 2023L,
    weight_method                 = "area",
    estimand_family               = NA_character_,
    weight_basis                  = NA_character_,
    moe_formula_requested         = NA_character_,
    moe_formula_effective         = NA_character_,
    moe_fallback                  = NA,
    moe_fallback_reason           = "n/a",
    failure_origin                = "intersection",
    weight_uncertainty_propagated = FALSE
  )
  attr(data, "cacs_schema_version") <- "1.0"

  out <- cacs_propagate_moe(data)

  expect_equal(nrow(out), 1L)
  expect_true(is.na(out$estimate))
  expect_true(is.na(out$moe))
  expect_equal(out$n_tracts, 0L)
  expect_equal(out$failure_origin, "intersection")
  expect_equal(out$moe_fallback_reason, "n/a")
  expect_true(is.na(out$moe_formula_effective))
})


# ---- T22-03 ----------------------------------------------------------------

test_that("T22-03 cacs_propagate_moe() rejects out-of-range or malformed level", {
  data <- .make_phase6_fixture()
  expect_error(cacs_propagate_moe(data, level = 0),
               class = "catchmentACS_error_schema")
  expect_error(cacs_propagate_moe(data, level = 1),
               class = "catchmentACS_error_schema")
  expect_error(cacs_propagate_moe(data, level = -0.1),
               class = "catchmentACS_error_schema")
  expect_error(cacs_propagate_moe(data, level = 1.5),
               class = "catchmentACS_error_schema")
  expect_error(cacs_propagate_moe(data, level = NA_real_),
               class = "catchmentACS_error_schema")
  expect_error(cacs_propagate_moe(data, level = c(0.9, 0.95)),
               class = "catchmentACS_error_schema")
  expect_error(cacs_propagate_moe(data, level = "0.9"),
               class = "catchmentACS_error_schema")
})


# ---- T22-04 ----------------------------------------------------------------

test_that("T22-04 cacs_propagate_moe() rejects unsanctioned formula arguments", {
  data <- .make_phase6_fixture()
  # Unsanctioned scalar
  expect_error(cacs_propagate_moe(data, formula = "bogus"),
               class = "catchmentACS_error_schema")
  # Unsupported type
  expect_error(cacs_propagate_moe(data, formula = 42),
               class = "catchmentACS_error_schema")
  # Named list with unknown variable
  expect_error(
    cacs_propagate_moe(data, formula = list(unknown_var = "weighted_sum")),
    class = "catchmentACS_error_schema"
  )
  # Named list with unsanctioned formula value
  expect_error(
    cacs_propagate_moe(data, formula = list(B17001_002 = "bogus_formula")),
    class = "catchmentACS_error_schema"
  )
  # Unnamed list
  expect_error(
    cacs_propagate_moe(data, formula = list("weighted_sum")),
    class = "catchmentACS_error_schema"
  )
  # Unknown dots are fail-loud rather than silently ignored.
  expect_error(
    cacs_propagate_moe(data, bogus_arg = 1),
    class = "catchmentACS_error_schema"
  )
  # Currently reserved dots are accepted with a provenance warning.
  expect_warning(
    cacs_propagate_moe(data, fallback_chain_max = 1),
    "ignored reserved",
    class = "catchmentACS_warning_provenance"
  )
})


# ---- T22-05 ----------------------------------------------------------------

test_that("T22-05 Family A weighted_sum vectorized known-answer at eps=1e-9", {
  data <- .make_phase6_fixture()
  out <- cacs_propagate_moe(data, level = 0.90)

  # Family A row 1: B17001_002, var_total_raw = 100 -> MOE = 1.645 * sqrt(100)
  row_a1 <- which(out$variable == "B17001_002")
  expect_length(row_a1, 1L)
  expect_equal(out$moe[row_a1], 1.645 * sqrt(100), tolerance = 1e-9)
  expect_equal(out$moe_formula_effective[row_a1], "weighted_sum")
  expect_false(out$moe_fallback[row_a1])
  expect_equal(out$moe_fallback_reason[row_a1], "n/a")

  # Family A row 2: B17001_001, var_total_raw = 400 -> MOE = 1.645 * sqrt(400)
  row_a2 <- which(out$variable == "B17001_001")
  expect_length(row_a2, 1L)
  expect_equal(out$moe[row_a2], 1.645 * sqrt(400), tolerance = 1e-9)
  expect_equal(out$moe_formula_effective[row_a2], "weighted_sum")
})


# ---- T22-06 ----------------------------------------------------------------

test_that("T22-06 Family B weighted_mean vectorized known-answer at eps=1e-9", {
  data <- .make_phase6_fixture()
  out <- cacs_propagate_moe(data, level = 0.90)

  # Family B row: B19013_001 median proxy, var_mean_raw = 25 -> MOE = 1.645 * 5
  row_b <- which(out$variable == "B19013_001")
  expect_length(row_b, 1L)
  expect_equal(out$moe[row_b], 1.645 * sqrt(25), tolerance = 1e-9)
  expect_equal(out$moe_formula_effective[row_b], "weighted_mean")
  expect_false(out$moe_fallback[row_b])
  expect_equal(out$moe_fallback_reason[row_b], "n/a")
})


# ---- T22-07 ----------------------------------------------------------------

test_that("T22-07 cacs_propagate_moe() formula 3-mode dispatch", {
  data <- .make_phase6_fixture()

  # Mode 1: NULL -> auto-dispatch reproduces input families
  out_null <- cacs_propagate_moe(data, formula = NULL)
  expect_equal(out_null$moe_formula_effective[out_null$variable == "B17001_002"],
               "weighted_sum")
  expect_equal(out_null$moe_formula_effective[out_null$variable == "B19013_001"],
               "weighted_mean")

  # Mode 2: scalar broadcast is allowed only if the formula is compatible
  # with every ordinary row's estimand family. This mixed Family A/B fixture
  # must fail loudly rather than routing the median proxy through Family A.
  expect_error(
    cacs_propagate_moe(data, formula = "weighted_sum"),
    class = "catchmentACS_error_schema"
  )

  # Mode 3: named list per-variable override
  out_list <- cacs_propagate_moe(
    data,
    formula = list(B17001_002 = "weighted_sum",
                   B19013_001 = "weighted_mean")
  )
  expect_equal(
    out_list$moe_formula_effective[out_list$variable == "B17001_002"],
    "weighted_sum"
  )
  expect_equal(
    out_list$moe_formula_effective[out_list$variable == "B19013_001"],
    "weighted_mean"
  )
  # Variables not in the list still receive auto-dispatch (B17001_001 -> A).
  expect_equal(
    out_list$moe_formula_effective[out_list$variable == "B17001_001"],
    "weighted_sum"
  )

  # A named list override that creates a bad family/formula pair also aborts.
  expect_error(
    cacs_propagate_moe(data, formula = list(B19013_001 = "weighted_sum")),
    class = "catchmentACS_error_schema"
  )
})


# ---- T22-08 ----------------------------------------------------------------

test_that("T22-08 cacs_propagate_moe() dispatches ratio rows to C2 default (Step 6.3)", {
  data <- .make_phase6_fixture(add_rate_row = TRUE)
  # The fixture has a derived_rate row with variable = "poverty_rate" ->
  # auto-dispatch produces `general_ratio_conservative` (Sec. 23.4 baseline)
  # and the C2 dispatch reads carriers for B17001_002 (num) + B17001_001
  # (den) via .SANCTIONED_RATES_V1$poverty_rate. With num_est=50,
  # var_n=100, den_est=200, var_d=400, z=1.645:
  #   num_moe = 1.645 * sqrt(100) = 16.45
  #   den_moe = 1.645 * sqrt(400) = 32.90
  #   r       = 0.25
  #   inside  = (16.45/1.645)^2 + 0.25^2 * (32.90/1.645)^2 = 100 + 25 = 125
  #   moe     = 1.645 * sqrt(125) / 200
  out <- cacs_propagate_moe(data)

  row_rate <- which(out$variable == "poverty_rate")
  expect_length(row_rate, 1L)
  expect_equal(out$moe[row_rate], 1.645 * sqrt(125) / 200, tolerance = 1e-9)
  expect_equal(out$estimate[row_rate], 0.25, tolerance = 1e-12)
  expect_equal(out$moe_formula_effective[row_rate],
               "general_ratio_conservative")
  expect_equal(out$moe_formula_requested[row_rate],
               "general_ratio_conservative")
  expect_false(out$moe_fallback[row_rate])
  expect_equal(out$moe_fallback_reason[row_rate], "n/a")
})


# ---- T22-13 ----------------------------------------------------------------

test_that("T22-13 all-NA carrier yields NA MOE without aborting", {
  data <- .make_phase6_fixture()
  carrier <- attr(data, "cacs_aggregation_carriers")
  carrier$var_total_raw <- NA_real_
  carrier$var_mean_raw  <- NA_real_
  attr(data, "cacs_aggregation_carriers") <- carrier

  out <- cacs_propagate_moe(data)
  # No abort; every MOE should be NA but the formula/fallback bookkeeping
  # still updates to the resolved values.
  expect_true(all(is.na(out$moe)))
  expect_true(all(out$moe_formula_effective %in%
                  c("weighted_sum", "weighted_mean")))
  expect_true(all(out$moe_fallback == FALSE))
  expect_true(all(out$moe_fallback_reason == "n/a"))
})


# ---- T22-14 ----------------------------------------------------------------

test_that("T22-14 missing variance carrier yields NA only for affected rows", {
  data <- .make_phase6_fixture()
  carrier <- attr(data, "cacs_aggregation_carriers")
  # Drop the Family A var_total_raw for row 1 only; row 2 + Family B intact.
  carrier$var_total_raw[carrier$variable == "B17001_002"] <- NA_real_
  attr(data, "cacs_aggregation_carriers") <- carrier

  out <- cacs_propagate_moe(data)
  expect_true(is.na(out$moe[out$variable == "B17001_002"]))
  expect_equal(out$moe[out$variable == "B17001_001"],
               1.645 * sqrt(400), tolerance = 1e-9)
  expect_equal(out$moe[out$variable == "B19013_001"],
               1.645 * sqrt(25), tolerance = 1e-9)
})


# ---- T22-15 ----------------------------------------------------------------

test_that("T22-15 mixed Family A + Family B rows all propagate in one pass", {
  data <- .make_phase6_fixture()
  out <- cacs_propagate_moe(data, level = 0.90)

  # All three rows updated, every MOE finite (carriers are all non-NA).
  expect_equal(nrow(out), 3L)
  expect_true(all(is.finite(out$moe)))
  expect_setequal(out$moe_formula_effective,
                  c("weighted_sum", "weighted_mean"))
  # Confidence-level attribute attached.
  expect_equal(attr(out, "cacs_confidence_level"), 0.90)
})


# ---- T22-16 ----------------------------------------------------------------

test_that("T22-16 level=0.95 z-scaling matches qnorm(0.975) within eps=1e-9", {
  data <- .make_phase6_fixture()
  out <- cacs_propagate_moe(data, level = 0.95)

  z95 <- stats::qnorm(0.975)
  expect_equal(out$moe[out$variable == "B17001_002"],
               z95 * sqrt(100), tolerance = 1e-9)
  expect_equal(out$moe[out$variable == "B19013_001"],
               z95 * sqrt(25),  tolerance = 1e-9)
  expect_equal(attr(out, "cacs_confidence_level"), 0.95)
})
