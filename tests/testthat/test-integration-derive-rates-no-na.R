# Integration locks for carrier-complete derive-rates behavior.

test_that("P4-NONA-01 carrier-complete C2 derivation returns five finite rates", {
  out <- .phase4_derive()
  rates <- out[out$variable %in% names(cacs_acs_default_rates) &
                 out$estimand_family == "derived_rate", ]
  expect_equal(nrow(rates), 5L)
  expect_true(all(is.finite(rates$estimate)))
  expect_true(all(is.finite(rates$moe)))
})

test_that("P4-NONA-02 carrier-complete auto derivation returns five finite rates", {
  out <- .phase4_derive(formula_dispatch = "auto")
  rates <- out[out$variable %in% names(cacs_acs_default_rates) &
                 out$estimand_family == "derived_rate", ]
  expect_equal(nrow(rates), 5L)
  expect_true(all(is.finite(rates$estimate)))
})

test_that("P4-NONA-03 carrier-complete derivation emits no warnings", {
  res <- .phase4_derive_with_warnings()
  expect_length(res$warnings, 0L)
})

test_that("P4-NONA-04 carrier-complete provenance reports zero carrier misses", {
  out <- .phase4_derive()
  expect_identical(attr(out, "cacs_rate_provenance")$n_carrier_missing, 0L)
})

test_that("P4-NONA-05 carrier-complete failure_origin is none for all rates", {
  out <- .phase4_derive()
  rates <- out[out$variable %in% names(cacs_acs_default_rates) &
                 out$estimand_family == "derived_rate", ]
  expect_true(all(rates$failure_origin == "none"))
})

test_that("P4-NONA-06 carrier-complete rates are bounded within [0, 1]", {
  est <- .phase4_rate_estimates(.phase4_derive())
  expect_true(all(est >= 0 & est <= 1))
})

test_that("P4-NONA-07 variables = 'extended' is carrier-complete", {
  resolved <- catchmentACS:::.resolve_variables("extended")
  expect_true(all(.phase4_required_rate_codes() %in% resolved$vars))
})

test_that("P4-NONA-08 packaged sample fixture is carrier-complete", {
  acs <- readRDS(system.file("extdata", "sample_alabama_subset.rds",
                             package = "catchmentACS"))
  expect_true(all(.phase4_required_rate_codes() %in% unique(acs$variable)))
})

test_that("P4-NONA-09 dropping non-rate B11001_001 keeps all five rates finite", {
  out <- .phase4_derive(drop = "B11001_001")
  rates <- out[out$variable %in% names(cacs_acs_default_rates) &
                 out$estimand_family == "derived_rate", ]
  expect_true(all(is.finite(rates$estimate)))
})

test_that("P4-NONA-10 missing SSI numerator warns and affects only ssi_rate", {
  res <- .phase4_derive_with_warnings(drop = "B19056_002")
  expect_s3_class(res$warnings[[1L]], "catchmentACS_warning_carrier_missing")
  ssi <- .phase4_rate_row(res$out, "ssi_rate")
  other <- res$out[res$out$variable %in%
                     setdiff(names(cacs_acs_default_rates), "ssi_rate") &
                     res$out$estimand_family == "derived_rate", ]
  expect_true(is.na(ssi$estimate))
  expect_true(all(is.finite(other$estimate)))
})

test_that("P4-NONA-11 missing LFP denominator warns and affects only LFP", {
  res <- .phase4_derive_with_warnings(drop = "B23025_001")
  expect_s3_class(res$warnings[[1L]], "catchmentACS_warning_carrier_missing")
  lfp <- .phase4_rate_row(res$out, "labor_force_participation")
  other <- res$out[res$out$variable %in%
                     setdiff(names(cacs_acs_default_rates),
                             "labor_force_participation") &
                     res$out$estimand_family == "derived_rate", ]
  expect_true(is.na(lfp$estimate))
  expect_true(all(is.finite(other$estimate)))
})

test_that("P4-NONA-12 missing carrier rows have NA tract-count slots", {
  res <- .phase4_derive_with_warnings(drop = "B19056_002")
  ssi <- .phase4_rate_row(res$out, "ssi_rate")
  expect_true(is.na(ssi$n_tracts_num))
  expect_true(is.na(ssi$n_tracts_den))
})

test_that("P4-NONA-13 missing carrier warning keeps runtime parent", {
  classes <- catchmentACS:::.cacs_cond_classes("warning", "carrier_missing")
  expect_true("catchmentACS_warning_runtime" %in% classes)
})

test_that("P4-NONA-14 cacs_describe sees zero carrier misses on complete derive", {
  desc <- suppressMessages(cacs_describe(.phase4_derive()))
  expect_true(any(grepl("Carrier-missing rate rows: 0",
                        desc$sections[["Rate Derivation"]], fixed = TRUE)))
})

test_that("P4-NONA-15 cacs_describe sees consumed carriers on derived output", {
  desc <- suppressMessages(cacs_describe(.phase4_derive()))
  expect_true(any(grepl("absent or already consumed",
                        desc$sections[["Carrier Table"]], fixed = TRUE)))
})

