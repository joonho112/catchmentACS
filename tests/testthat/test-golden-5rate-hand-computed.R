# Golden tests for all five sanctioned rates using hand-computed carriers.

test_that("P4-GOLD-01 all five baseline rate estimates match hand math", {
  out <- .phase4_derive()
  est <- .phase4_rate_estimates(out)
  expect_equal(est[["poverty_rate"]], 150 / 1000, tolerance = 1e-12)
  expect_equal(est[["snap_rate"]], 100 / 800, tolerance = 1e-12)
  expect_equal(est[["ssi_rate"]], 25 / 500, tolerance = 1e-12)
  expect_equal(est[["unemp_rate"]], 48 / 600, tolerance = 1e-12)
  expect_equal(est[["labor_force_participation"]], 650 / 1000,
               tolerance = 1e-12)
})

test_that("P4-GOLD-02 poverty_rate baseline equals 0.15", {
  row <- .phase4_rate_row(.phase4_derive(), "poverty_rate")
  expect_equal(row$estimate, 0.15, tolerance = 1e-12)
})

test_that("P4-GOLD-03 poverty_rate zero numerator equals 0", {
  row <- .phase4_rate_row(
    .phase4_derive(.phase4_rate_values(B17001_002 = 0)),
    "poverty_rate"
  )
  expect_equal(row$estimate, 0, tolerance = 1e-12)
})

test_that("P4-GOLD-04 poverty_rate audit upper bound fixture equals 0.60", {
  row <- .phase4_rate_row(
    .phase4_derive(.phase4_rate_values(B17001_002 = 600)),
    "poverty_rate"
  )
  expect_equal(row$estimate, 0.60, tolerance = 1e-12)
})

test_that("P4-GOLD-05 snap_rate baseline equals 0.125", {
  row <- .phase4_rate_row(.phase4_derive(), "snap_rate")
  expect_equal(row$estimate, 0.125, tolerance = 1e-12)
})

test_that("P4-GOLD-06 snap_rate zero numerator equals 0", {
  row <- .phase4_rate_row(
    .phase4_derive(.phase4_rate_values(B22003_002 = 0)),
    "snap_rate"
  )
  expect_equal(row$estimate, 0, tolerance = 1e-12)
})

test_that("P4-GOLD-07 snap_rate audit upper bound fixture equals 0.50", {
  row <- .phase4_rate_row(
    .phase4_derive(.phase4_rate_values(B22003_002 = 400)),
    "snap_rate"
  )
  expect_equal(row$estimate, 0.50, tolerance = 1e-12)
})

test_that("P4-GOLD-08 ssi_rate baseline uses B19056_002 / B19056_001", {
  row <- .phase4_rate_row(.phase4_derive(), "ssi_rate")
  expect_equal(row$estimate, 25 / 500, tolerance = 1e-12)
})

test_that("P4-GOLD-09 ssi_rate differs from the v0.2 total-HH divisor", {
  vals <- .phase4_rate_values()
  row <- .phase4_rate_row(.phase4_derive(vals), "ssi_rate")
  old_formula <- unname(vals[["B19056_001"]] / vals[["B11001_001"]])
  expect_equal(row$estimate, 0.05, tolerance = 1e-12)
  expect_equal(old_formula, 0.625, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(row$estimate, old_formula)))
})

test_that("P4-GOLD-10 ssi_rate zero numerator equals 0", {
  row <- .phase4_rate_row(
    .phase4_derive(.phase4_rate_values(B19056_002 = 0)),
    "ssi_rate"
  )
  expect_equal(row$estimate, 0, tolerance = 1e-12)
})

test_that("P4-GOLD-11 unemp_rate baseline equals 0.08", {
  row <- .phase4_rate_row(.phase4_derive(), "unemp_rate")
  expect_equal(row$estimate, 0.08, tolerance = 1e-12)
})

test_that("P4-GOLD-12 unemp_rate zero numerator equals 0", {
  row <- .phase4_rate_row(
    .phase4_derive(.phase4_rate_values(B23025_005 = 0)),
    "unemp_rate"
  )
  expect_equal(row$estimate, 0, tolerance = 1e-12)
})

test_that("P4-GOLD-13 unemp_rate audit upper bound fixture equals 0.30", {
  row <- .phase4_rate_row(
    .phase4_derive(.phase4_rate_values(B23025_005 = 180)),
    "unemp_rate"
  )
  expect_equal(row$estimate, 0.30, tolerance = 1e-12)
})

test_that("P4-GOLD-14 labor_force_participation baseline equals 0.65", {
  row <- .phase4_rate_row(.phase4_derive(), "labor_force_participation")
  expect_equal(row$estimate, 0.65, tolerance = 1e-12)
})

test_that("P4-GOLD-15 labor_force_participation lower bound fixture equals 0.30", {
  row <- .phase4_rate_row(
    .phase4_derive(.phase4_rate_values(B23025_002 = 300)),
    "labor_force_participation"
  )
  expect_equal(row$estimate, 0.30, tolerance = 1e-12)
})

test_that("P4-GOLD-16 labor_force_participation upper bound fixture equals 0.85", {
  row <- .phase4_rate_row(
    .phase4_derive(.phase4_rate_values(B23025_002 = 850)),
    "labor_force_participation"
  )
  expect_equal(row$estimate, 0.85, tolerance = 1e-12)
})

test_that("P4-GOLD-17 auto dispatch uses C1 only for poverty and LFP", {
  out <- .phase4_derive(formula_dispatch = "auto")
  poverty <- .phase4_rate_row(out, "poverty_rate")
  lfp <- .phase4_rate_row(out, "labor_force_participation")
  snap <- .phase4_rate_row(out, "snap_rate")
  ssi <- .phase4_rate_row(out, "ssi_rate")
  unemp <- .phase4_rate_row(out, "unemp_rate")
  expect_equal(poverty$moe_formula_requested, "proportion_subset")
  expect_equal(lfp$moe_formula_requested, "proportion_subset")
  expect_equal(snap$moe_formula_requested, "general_ratio_conservative")
  expect_equal(ssi$moe_formula_requested, "general_ratio_conservative")
  expect_equal(unemp$moe_formula_requested, "general_ratio_conservative")
})

test_that("P4-GOLD-18 default dispatch uses C2 for all five rates", {
  out <- .phase4_derive()
  rates <- out[out$variable %in% names(cacs_acs_default_rates) &
                 out$estimand_family == "derived_rate", ]
  expect_true(all(rates$moe_formula_requested ==
                    "general_ratio_conservative"))
  expect_true(all(rates$moe_formula_effective ==
                    "general_ratio_conservative"))
})

test_that("P4-GOLD-19 rate rows carry numerator and denominator tract counts", {
  out <- .phase4_derive()
  rates <- out[out$variable %in% names(cacs_acs_default_rates) &
                 out$estimand_family == "derived_rate", ]
  expect_true(all(!is.na(rates$n_tracts_num)))
  expect_true(all(!is.na(rates$n_tracts_den)))
})

test_that("P4-GOLD-20 rate provenance row count equals five", {
  out <- .phase4_derive()
  prov <- attr(out, "cacs_rate_provenance")
  expect_identical(prov$n_rate_rows_appended, 5L)
  expect_identical(prov$n_carrier_missing, 0L)
})

