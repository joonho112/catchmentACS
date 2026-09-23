# Regression locks for #12: v0.3 ssi_rate formula.

test_that("P4-SSI-01 sanctioned ssi_rate formula is B19056_002 / B19056_001", {
  expect_identical(cacs_acs_default_rates$ssi_rate,
                   c(num = "B19056_002", den = "B19056_001"))
})

test_that("P4-SSI-02 internal and exported rate catalogues are identical", {
  expect_identical(cacs_acs_default_rates, catchmentACS:::.SANCTIONED_RATES_V1)
})

test_that("P4-SSI-03 default variables include the SSI numerator and denominator", {
  e <- new.env(parent = emptyenv())
  utils::data("cacs_acs_default_vars", package = "catchmentACS", envir = e)
  expect_true("B19056_002" %in% unname(e$cacs_acs_default_vars))
  expect_true("B19056_001" %in% unname(e$cacs_acs_default_vars))
})

test_that("P4-SSI-04 packaged sample fixture includes the SSI numerator", {
  sample_path <- system.file("extdata", "sample_alabama_subset.rds",
                             package = "catchmentACS")
  acs <- readRDS(sample_path)
  expect_true("B19056_002" %in% unique(acs$variable))
})

test_that("P4-SSI-05 synthetic ssi_rate is plausible and not near one", {
  row <- .phase4_rate_row(.phase4_derive(), "ssi_rate")
  expect_equal(row$estimate, 0.05, tolerance = 1e-12)
  expect_lt(row$estimate, 0.15)
})

test_that("P4-SSI-06 changing B11001_001 cannot change ssi_rate", {
  base <- .phase4_rate_row(.phase4_derive(), "ssi_rate")
  changed <- .phase4_rate_row(
    .phase4_derive(.phase4_rate_values(B11001_001 = 1)),
    "ssi_rate"
  )
  expect_equal(changed$estimate, base$estimate, tolerance = 1e-12)
})

test_that("P4-SSI-07 v0.2 total-HH formula would diverge on the same carriers", {
  vals <- .phase4_rate_values()
  new_rate <- .phase4_rate_row(.phase4_derive(vals), "ssi_rate")$estimate
  old_rate <- vals[["B19056_001"]] / vals[["B11001_001"]]
  expect_gt(old_rate, 10 * new_rate)
})

test_that("P4-SSI-08 rate audit warns on out-of-range synthetic SSI", {
  vals <- .phase4_rate_values(B19056_002 = 300)
  withr::with_options(
    list(catchmentACS.audit_rates = TRUE),
    expect_warning(
      cacs_derive_rates(.phase4_rate_fixture(values = vals)),
      class = "catchmentACS_warning_rate_out_of_range"
    )
  )
})

test_that("P4-SSI-09 rate audit is off by default", {
  vals <- .phase4_rate_values(B19056_002 = 300)
  expect_no_warning(.phase4_derive(vals, audit = FALSE))
})

test_that("P4-SSI-10 ssi_rate bound is locked at 0.15 for the warning audit", {
  expect_equal(catchmentACS:::.SANCTIONED_RATE_BOUNDS_V1$ssi_rate,
               c(min = 0, max = 0.15))
})
