# ===========================================================================
# test-unit-layer0-options.R
#
# v0.4 plan Step 1.3 — Layer 0 unit tests for the 4 new package options
# registered in R/zzz.R `.onLoad()` and documented in R/aaa-globals.R.
#
# Coverage:
#   - 4 tests: default values match plan §sec-step-0-2 lock.
#   - 4 tests: `options(...)` override propagates to `getOption(...)` at use site.
# ===========================================================================

test_that("LAYER0-OPT-01 catchmentACS.summary_per_site_max default = 12L", {
  expect_identical(
    getOption("catchmentACS.summary_per_site_max"),
    12L
  )
})

test_that("LAYER0-OPT-02 catchmentACS.osrm_demo_budget_protect default = TRUE", {
  expect_true(
    getOption("catchmentACS.osrm_demo_budget_protect")
  )
})

test_that("LAYER0-OPT-03 catchmentACS.capture_return_value default = 'conditions'", {
  expect_identical(
    getOption("catchmentACS.capture_return_value"),
    "conditions"
  )
})

test_that("LAYER0-OPT-04 catchmentACS.rate_first_default default = TRUE", {
  expect_true(
    getOption("catchmentACS.rate_first_default")
  )
})

test_that("LAYER0-OPT-05 summary_per_site_max override propagates", {
  withr::local_options(catchmentACS.summary_per_site_max = 50L)
  expect_identical(
    getOption("catchmentACS.summary_per_site_max"),
    50L
  )
})

test_that("LAYER0-OPT-06 osrm_demo_budget_protect override propagates", {
  withr::local_options(catchmentACS.osrm_demo_budget_protect = FALSE)
  expect_false(
    getOption("catchmentACS.osrm_demo_budget_protect")
  )
})

test_that("LAYER0-OPT-07 capture_return_value override propagates", {
  withr::local_options(catchmentACS.capture_return_value = "both")
  expect_identical(
    getOption("catchmentACS.capture_return_value"),
    "both"
  )
})

test_that("LAYER0-OPT-08 rate_first_default override propagates", {
  withr::local_options(catchmentACS.rate_first_default = FALSE)
  expect_false(
    getOption("catchmentACS.rate_first_default")
  )
})

test_that("LAYER0-OPT-09 .OSRM_RES_DEFAULT_DEMO internal constant = 30L", {
  expect_identical(catchmentACS:::.OSRM_RES_DEFAULT_DEMO, 30L)
})

test_that("LAYER0-OPT-10 .OSRM_RES_DEFAULT v0.3 internal constant kept = 70L", {
  expect_identical(catchmentACS:::.OSRM_RES_DEFAULT, 70L)
})
