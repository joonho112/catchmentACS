# Phase 4 rate provenance and audit closeout locks.

test_that("P4-PROV-01 helper required code set matches rate catalogue union", {
  expect_setequal(
    .phase4_required_rate_codes(),
    unique(unname(unlist(cacs_acs_default_rates, use.names = FALSE)))
  )
})

test_that("P4-PROV-02 old SSI divisor B11001_001 is not a required rate carrier", {
  expect_false("B11001_001" %in% .phase4_required_rate_codes())
})

test_that("P4-PROV-03 synthetic fixture carrier table has required carrier columns", {
  fx <- .phase4_rate_fixture()
  car <- attr(fx, "cacs_aggregation_carriers")
  expect_true(all(catchmentACS:::.CARRIER_REQUIRED_COLS %in% names(car)))
  expect_equal(nrow(car), length(.phase4_rate_vars()))
})

test_that("P4-PROV-04 cacs_derive_rates consumes carrier table", {
  fx <- .phase4_rate_fixture()
  expect_false(is.null(attr(fx, "cacs_aggregation_carriers")))
  out <- .phase4_derive()
  expect_null(attr(out, "cacs_aggregation_carriers"))
})

test_that("P4-PROV-05 cacs_derive_rates preserves schema and confidence attributes", {
  out <- .phase4_derive()
  expect_identical(attr(out, "cacs_schema_version"), "1.0")
  expect_identical(attr(out, "cacs_confidence_level"), 0.90)
})

test_that("P4-PROV-06 rate provenance exposes expected field set", {
  prov <- attr(.phase4_derive(), "cacs_rate_provenance")
  expect_setequal(
    names(prov),
    c("rates_computed", "formula_dispatch", "formula_per_rate",
      "n_rate_rows_appended", "n_fallback_c1_to_c2", "n_zero_denominator",
      "n_missing_moe", "n_carrier_missing", "n_rate_audit_out_of_bounds",
      "formula_downgraded_rates", "n_formula_downgraded", "level", "z",
      "generated_at", "cacs_ver")
  )
})

test_that("P4-PROV-07 default formula provenance is C2 for all five rates", {
  prov <- attr(.phase4_derive(), "cacs_rate_provenance")
  expect_identical(prov$formula_dispatch, "general_ratio_conservative")
  expect_true(all(prov$formula_per_rate == "general_ratio_conservative"))
})

test_that("P4-PROV-08 auto formula provenance matches catalogue", {
  prov <- attr(.phase4_derive(formula_dispatch = "auto"),
               "cacs_rate_provenance")
  expect_identical(prov$formula_per_rate,
                   catchmentACS:::.RATE_FORMULA_CATALOGUE)
})

test_that("P4-PROV-09 audit attribute exists but is disabled by default", {
  audit <- attr(.phase4_derive(audit = FALSE), "cacs_rate_audit")
  expect_false(audit$enabled)
  expect_identical(audit$n_out_of_bounds, 0L)
})

test_that("P4-PROV-10 in-bound audit is enabled and clean when requested", {
  audit <- attr(.phase4_derive(audit = TRUE), "cacs_rate_audit")
  expect_true(audit$enabled)
  expect_identical(audit$n_out_of_bounds, 0L)
})

test_that("P4-PROV-11 out-of-bound SSI audit records one row", {
  vals <- .phase4_rate_values(B19056_002 = 300)
  out <- .phase4_derive_with_warnings(vals, audit = TRUE)$out
  audit <- attr(out, "cacs_rate_audit")
  expect_true(audit$enabled)
  expect_identical(audit$n_out_of_bounds, 1L)
  expect_identical(audit$rows$variable[[1L]], "ssi_rate")
})

test_that("P4-PROV-12 rate audit warning class chain is registered", {
  classes <- catchmentACS:::.cacs_cond_classes("warning", "rate_out_of_range")
  expect_true("catchmentACS_warning_rate_out_of_range" %in% classes)
  expect_true("catchmentACS_warning" %in% classes)
  expect_true("catchmentACS_condition" %in% classes)
})

test_that("P4-PROV-13 out-of-bound audit emits rate_out_of_range warning", {
  vals <- .phase4_rate_values(B19056_002 = 300)
  warnings <- .phase4_derive_with_warnings(vals, audit = TRUE)$warnings
  expect_true(any(vapply(warnings, inherits, logical(1),
                         "catchmentACS_warning_rate_out_of_range")))
})

test_that("P4-PROV-14 carrier-missing and audit warnings are distinct classes", {
  carrier_warn <- .phase4_derive_with_warnings(drop = "B19056_002")$warnings
  audit_warn <- .phase4_derive_with_warnings(
    .phase4_rate_values(B19056_002 = 300), audit = TRUE
  )$warnings
  expect_true(inherits(carrier_warn[[1L]],
                       "catchmentACS_warning_carrier_missing"))
  expect_true(any(vapply(audit_warn, inherits, logical(1),
                         "catchmentACS_warning_rate_out_of_range")))
})

test_that("P4-PROV-15 cacs_describe exposes rate provenance attributes", {
  out <- .phase4_derive()
  desc <- suppressMessages(cacs_describe(out))
  expect_true("cacs_rate_provenance" %in% desc$attributes_present)
  expect_false("cacs_aggregation_carriers" %in% desc$attributes_present)
})

test_that("P4-PROV-16 cacs_describe surfaces out-of-range audit counter", {
  vals <- .phase4_rate_values(B19056_002 = 300)
  out <- .phase4_derive_with_warnings(vals, audit = TRUE)$out
  desc <- suppressMessages(cacs_describe(out))
  expect_true(any(grepl("Out-of-range audit rows: 1",
                        desc$sections[["Rate Derivation"]], fixed = TRUE)))
})

test_that("P4-PROV-17 dropping B11001_001 does not create carrier misses", {
  out <- .phase4_derive(drop = "B11001_001")
  prov <- attr(out, "cacs_rate_provenance")
  expect_identical(prov$n_carrier_missing, 0L)
})

test_that("P4-PROV-18 every computed rate row has coverage weight basis", {
  out <- .phase4_derive()
  rates <- out[out$variable %in% names(cacs_acs_default_rates) &
                 out$estimand_family == "derived_rate", ]
  expect_true(all(rates$weight_basis == "coverage"))
})

