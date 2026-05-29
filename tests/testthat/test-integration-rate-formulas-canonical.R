# Canonical formula catalogue and audit-helper integration locks.

test_that("P4-CANON-01 sanctioned catalogue has exactly five rates", {
  expect_setequal(names(cacs_acs_default_rates),
                  c("poverty_rate", "snap_rate", "ssi_rate",
                    "unemp_rate", "labor_force_participation"))
})

test_that("P4-CANON-02 every sanctioned rate has named num and den codes", {
  for (rate in cacs_acs_default_rates) {
    expect_identical(names(rate), c("num", "den"))
    expect_true(all(grepl("^B[0-9]{5}_[0-9]{3}$", unname(rate))))
  }
})

test_that("P4-CANON-03 required rate carrier union is covered by default vars", {
  e <- new.env(parent = emptyenv())
  utils::data("cacs_acs_default_vars", package = "catchmentACS", envir = e)
  expect_true(all(.phase4_required_rate_codes() %in%
                    unname(e$cacs_acs_default_vars)))
})

test_that("P4-CANON-04 rate carrier code union has ten unique cells", {
  expect_equal(length(.phase4_required_rate_codes()), 10L)
})

test_that("P4-CANON-05 no sanctioned rate reuses the same numerator and denominator", {
  for (rate in cacs_acs_default_rates) {
    expect_false(identical(unname(rate[["num"]]), unname(rate[["den"]])))
  }
})

test_that("P4-CANON-06 formula catalogue covers the five sanctioned rates", {
  expect_setequal(names(catchmentACS:::.RATE_FORMULA_CATALOGUE),
                  names(cacs_acs_default_rates))
})

test_that("P4-CANON-07 C1 eligibility is locked to poverty and LFP", {
  expect_identical(
    catchmentACS:::.RATE_C1_SUBSET_ELIGIBLE,
    c(poverty_rate = TRUE, snap_rate = FALSE, ssi_rate = FALSE,
      unemp_rate = FALSE, labor_force_participation = TRUE)
  )
})

test_that("P4-CANON-08 sanctioned rate bounds cover all five rates", {
  expect_setequal(names(catchmentACS:::.SANCTIONED_RATE_BOUNDS_V1),
                  names(cacs_acs_default_rates))
})

test_that("P4-CANON-09 every sanctioned bound has increasing min and max", {
  for (bounds in catchmentACS:::.SANCTIONED_RATE_BOUNDS_V1) {
    expect_lt(bounds[["min"]], bounds[["max"]])
  }
})

test_that("P4-CANON-10 in-bound synthetic rates do not warn under audit", {
  expect_no_warning(.phase4_derive(audit = TRUE))
})

test_that("P4-CANON-11 poverty out-of-range value emits audit warning", {
  withr::with_options(
    list(catchmentACS.audit_rates = TRUE),
    expect_warning(
      cacs_derive_rates(
        .phase4_rate_fixture(.phase4_rate_values(B17001_002 = 700))
      ),
      class = "catchmentACS_warning_rate_out_of_range"
    )
  )
})

test_that("P4-CANON-12 snap out-of-range value emits audit warning", {
  withr::with_options(
    list(catchmentACS.audit_rates = TRUE),
    expect_warning(
      cacs_derive_rates(
        .phase4_rate_fixture(.phase4_rate_values(B22003_002 = 500))
      ),
      class = "catchmentACS_warning_rate_out_of_range"
    )
  )
})

test_that("P4-CANON-13 unemp out-of-range value emits audit warning", {
  withr::with_options(
    list(catchmentACS.audit_rates = TRUE),
    expect_warning(
      cacs_derive_rates(
        .phase4_rate_fixture(.phase4_rate_values(B23025_005 = 240))
      ),
      class = "catchmentACS_warning_rate_out_of_range"
    )
  )
})

test_that("P4-CANON-14 LFP below lower bound emits audit warning", {
  withr::with_options(
    list(catchmentACS.audit_rates = TRUE),
    expect_warning(
      cacs_derive_rates(
        .phase4_rate_fixture(.phase4_rate_values(B23025_002 = 200))
      ),
      class = "catchmentACS_warning_rate_out_of_range"
    )
  )
})

test_that("P4-CANON-15 LFP above upper bound emits audit warning", {
  withr::with_options(
    list(catchmentACS.audit_rates = TRUE),
    expect_warning(
      cacs_derive_rates(
        .phase4_rate_fixture(.phase4_rate_values(B23025_002 = 900))
      ),
      class = "catchmentACS_warning_rate_out_of_range"
    )
  )
})
