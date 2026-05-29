# ============================================================================
# C-09 lock tests: SSI/LFP carrier-missing closure (v0.3 Step 4.2)
#
# These tests distinguish the fresh/default path from legacy v0.2 replay
# artifacts. The default catalogue must include every sanctioned rate carrier;
# direct or custom-variable pipelines that omit a carrier remain fail-loud via
# a classed carrier-missing warning and NA on only the affected derived rate.
# ============================================================================


.c09_required_rate_codes <- function() {
  sort(unique(unname(unlist(cacs_acs_default_rates, use.names = FALSE))))
}

.c09_default_vars <- function() {
  e <- new.env(parent = emptyenv())
  utils::data("cacs_acs_default_vars", package = "catchmentACS", envir = e)
  sort(unique(unname(e$cacs_acs_default_vars)))
}

.c09_carrier_values <- function() {
  tibble::tibble(
    variable = c(
      "B17001_002", "B17001_001",
      "B22003_002", "B22003_001",
      "B19056_002", "B19056_001",
      "B23025_005", "B23025_003",
      "B23025_002", "B23025_001"
    ),
    est_total = c(50, 200, 20, 100, 5, 80, 25, 200, 300, 600),
    var_total_raw = c(25, 100, 16, 64, 4, 36, 9, 49, 100, 144),
    n_tracts = c(2L, 3L, 2L, 2L, 1L, 3L, 2L, 4L, 5L, 6L)
  )
}

.c09_weighted_fixture <- function(drop = character()) {
  carrier_values <- .c09_carrier_values()
  if (length(drop) > 0L) {
    carrier_values <- carrier_values[!carrier_values$variable %in% drop, ]
  }

  carriers <- tibble::tibble(
    site_id         = rep("S01", nrow(carrier_values)),
    drive_time_min  = rep(15L, nrow(carrier_values)),
    variable        = carrier_values$variable,
    estimand_family = rep("spatial_total", nrow(carrier_values)),
    est_total       = carrier_values$est_total,
    var_total_raw   = carrier_values$var_total_raw,
    est_mean        = rep(NA_real_, nrow(carrier_values)),
    var_mean_raw    = rep(NA_real_, nrow(carrier_values)),
    weight_sum      = rep(0.8, nrow(carrier_values)),
    n_tracts        = carrier_values$n_tracts
  )

  data <- tibble::tibble(
    site_id                       = carriers$site_id,
    drive_time_min                = carriers$drive_time_min,
    ring_topology                 = rep("cumulative", nrow(carriers)),
    variable                      = carriers$variable,
    estimate                      = carriers$est_total,
    moe                           = 1.645 * sqrt(carriers$var_total_raw),
    weight_sum                    = carriers$weight_sum,
    n_tracts                      = carriers$n_tracts,
    n_tracts_num                  = rep(NA_integer_, nrow(carriers)),
    n_tracts_den                  = rep(NA_integer_, nrow(carriers)),
    provider                      = rep("osrm", nrow(carriers)),
    profile                       = rep("car", nrow(carriers)),
    osm_snapshot_date             = rep("2026-01-01", nrow(carriers)),
    acs_year                      = rep(2023L, nrow(carriers)),
    weight_method                 = rep("area", nrow(carriers)),
    estimand_family               = rep("spatial_total", nrow(carriers)),
    weight_basis                  = rep("coverage", nrow(carriers)),
    moe_formula_requested         = rep("weighted_sum", nrow(carriers)),
    moe_formula_effective         = rep("weighted_sum", nrow(carriers)),
    moe_fallback                  = rep(FALSE, nrow(carriers)),
    moe_fallback_reason           = rep("n/a", nrow(carriers)),
    failure_origin                = rep("none", nrow(carriers)),
    weight_uncertainty_propagated = rep(FALSE, nrow(carriers))
  )

  attr(data, "cacs_aggregation_carriers") <- carriers
  attr(data, "cacs_schema_version") <- "1.0"
  attr(data, "cacs_confidence_level") <- 0.90
  data
}

.c09_run_derive <- function(x, ...) {
  warnings <- list()
  out <- withCallingHandlers(
    cacs_derive_rates(x, ...),
    warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  list(out = out, warnings = warnings)
}

.c09_rate_row <- function(out, variable) {
  row <- out[out$variable == variable & out$estimand_family == "derived_rate", ]
  testthat::expect_equal(nrow(row), 1L)
  row
}


test_that("C09-01 sanctioned rate carrier code set is the expected v0.3 set", {
  expect_setequal(
    .c09_required_rate_codes(),
    c(
      "B17001_001", "B17001_002", "B19056_001", "B19056_002",
      "B22003_001", "B22003_002", "B23025_001", "B23025_002",
      "B23025_003", "B23025_005"
    )
  )
})

test_that("C09-02 default ACS variables include every sanctioned rate carrier", {
  expect_true(all(.c09_required_rate_codes() %in% .c09_default_vars()))
  expect_equal(length(.c09_default_vars()), 14L)
})

test_that("C09-03 variables = NULL resolves to a carrier-complete catalogue", {
  resolved <- .resolve_variables(NULL)
  expect_identical(resolved$source, "default_catalogue")
  expect_true(all(.c09_required_rate_codes() %in% resolved$vars))
})

test_that("C09-04 variables = 'core' resolves to a carrier-complete catalogue", {
  resolved <- .resolve_variables("core")
  expect_identical(resolved$source, "core_alias")
  expect_true(all(.c09_required_rate_codes() %in% resolved$vars))
})

test_that("C09-05 refreshed sample Alabama fixture includes every rate carrier", {
  sample_path <- system.file("extdata", "sample_alabama_subset.rds",
                             package = "catchmentACS")
  expect_true(nzchar(sample_path))
  sample_acs <- readRDS(sample_path)
  expect_true(all(.c09_required_rate_codes() %in% unique(sample_acs$variable)))
})

test_that("C09-06 carrier-complete fixture derives all five rates without warning", {
  res <- .c09_run_derive(.c09_weighted_fixture())
  rate_rows <- res$out[res$out$variable %in% names(cacs_acs_default_rates) &
                         res$out$estimand_family == "derived_rate", ]
  expect_length(res$warnings, 0L)
  expect_equal(nrow(rate_rows), 5L)
  expect_true(all(rate_rows$failure_origin == "none"))
  expect_true(all(is.finite(rate_rows$estimate)))
  expect_true(all(is.finite(rate_rows$moe)))
  expect_identical(attr(res$out, "cacs_rate_provenance")$n_carrier_missing, 0L)
})

test_that("C09-07 corrected ssi_rate uses B19056_002 over B19056_001 carriers", {
  res <- .c09_run_derive(.c09_weighted_fixture())
  row <- .c09_rate_row(res$out, "ssi_rate")
  expect_equal(row$estimate, 5 / 80, tolerance = 1e-12)
  expect_equal(row$n_tracts_num, 1L)
  expect_equal(row$n_tracts_den, 3L)
})

test_that("C09-08 labor_force_participation uses B23025_002 over B23025_001", {
  res <- .c09_run_derive(.c09_weighted_fixture(),
                         formula_dispatch = "auto")
  row <- .c09_rate_row(res$out, "labor_force_participation")
  expect_equal(row$estimate, 300 / 600, tolerance = 1e-12)
  expect_equal(row$moe_formula_effective, "proportion_subset")
  expect_equal(row$n_tracts_num, 5L)
  expect_equal(row$n_tracts_den, 6L)
})

test_that("C09-09 missing SSI numerator carrier affects only ssi_rate", {
  res <- .c09_run_derive(.c09_weighted_fixture(drop = "B19056_002"))
  row <- .c09_rate_row(res$out, "ssi_rate")
  other <- res$out[res$out$variable %in%
                     setdiff(names(cacs_acs_default_rates), "ssi_rate") &
                     res$out$estimand_family == "derived_rate", ]
  expect_s3_class(res$warnings[[1L]], "catchmentACS_warning_carrier_missing")
  expect_true(is.na(row$estimate))
  expect_true(is.na(row$moe))
  expect_equal(row$failure_origin, "carrier")
  expect_true(all(other$failure_origin == "none"))
})

test_that("C09-10 missing SSI denominator carrier affects only ssi_rate", {
  res <- .c09_run_derive(.c09_weighted_fixture(drop = "B19056_001"))
  row <- .c09_rate_row(res$out, "ssi_rate")
  other <- res$out[res$out$variable %in%
                     setdiff(names(cacs_acs_default_rates), "ssi_rate") &
                     res$out$estimand_family == "derived_rate", ]
  expect_s3_class(res$warnings[[1L]], "catchmentACS_warning_carrier_missing")
  expect_true(is.na(row$estimate))
  expect_equal(row$failure_origin, "carrier")
  expect_true(all(other$failure_origin == "none"))
})

test_that("C09-11 missing LFP numerator carrier affects only LFP", {
  res <- .c09_run_derive(.c09_weighted_fixture(drop = "B23025_002"))
  row <- .c09_rate_row(res$out, "labor_force_participation")
  other <- res$out[res$out$variable %in%
                     setdiff(names(cacs_acs_default_rates),
                             "labor_force_participation") &
                     res$out$estimand_family == "derived_rate", ]
  expect_s3_class(res$warnings[[1L]], "catchmentACS_warning_carrier_missing")
  expect_true(is.na(row$estimate))
  expect_equal(row$failure_origin, "carrier")
  expect_true(all(other$failure_origin == "none"))
})

test_that("C09-12 missing LFP denominator carrier affects only LFP", {
  res <- .c09_run_derive(.c09_weighted_fixture(drop = "B23025_001"))
  row <- .c09_rate_row(res$out, "labor_force_participation")
  other <- res$out[res$out$variable %in%
                     setdiff(names(cacs_acs_default_rates),
                             "labor_force_participation") &
                     res$out$estimand_family == "derived_rate", ]
  expect_s3_class(res$warnings[[1L]], "catchmentACS_warning_carrier_missing")
  expect_true(is.na(row$estimate))
  expect_equal(row$failure_origin, "carrier")
  expect_true(all(other$failure_origin == "none"))
})

test_that("C09-13 missing SSI carrier leaves non-SSI rate estimates unchanged", {
  complete <- .c09_run_derive(.c09_weighted_fixture())$out
  missing <- .c09_run_derive(.c09_weighted_fixture(drop = "B19056_002"))$out
  keep <- setdiff(names(cacs_acs_default_rates), "ssi_rate")
  complete_rates <- complete[complete$variable %in% keep &
                               complete$estimand_family == "derived_rate",
                             c("variable", "estimate", "moe")]
  missing_rates <- missing[missing$variable %in% keep &
                             missing$estimand_family == "derived_rate",
                           c("variable", "estimate", "moe")]
  complete_rates <- complete_rates[order(complete_rates$variable), ]
  missing_rates <- missing_rates[order(missing_rates$variable), ]
  expect_equal(complete_rates$variable, missing_rates$variable)
  expect_equal(complete_rates$estimate, missing_rates$estimate)
  expect_equal(complete_rates$moe, missing_rates$moe)
})

test_that("C09-14 missing LFP carrier leaves non-LFP rate estimates unchanged", {
  complete <- .c09_run_derive(.c09_weighted_fixture())$out
  missing <- .c09_run_derive(.c09_weighted_fixture(drop = "B23025_001"))$out
  keep <- setdiff(names(cacs_acs_default_rates), "labor_force_participation")
  complete_rates <- complete[complete$variable %in% keep &
                               complete$estimand_family == "derived_rate",
                             c("variable", "estimate", "moe")]
  missing_rates <- missing[missing$variable %in% keep &
                             missing$estimand_family == "derived_rate",
                           c("variable", "estimate", "moe")]
  complete_rates <- complete_rates[order(complete_rates$variable), ]
  missing_rates <- missing_rates[order(missing_rates$variable), ]
  expect_equal(complete_rates$variable, missing_rates$variable)
  expect_equal(complete_rates$estimate, missing_rates$estimate)
  expect_equal(complete_rates$moe, missing_rates$moe)
})

test_that("C09-15 carrier-missing warning keeps runtime parent compatibility", {
  classes <- .cacs_cond_classes("warning", "carrier_missing")
  expect_true("catchmentACS_warning_carrier_missing" %in% classes)
  expect_true("catchmentACS_warning_runtime" %in% classes)
  expect_true("catchmentACS_warning" %in% classes)
})

test_that("C09-16 carrier-missing rows expose provenance and tract-count NA", {
  res <- .c09_run_derive(.c09_weighted_fixture(drop = "B19056_002"))
  row <- .c09_rate_row(res$out, "ssi_rate")
  prov <- attr(res$out, "cacs_rate_provenance")
  expect_identical(prov$n_carrier_missing, 1L)
  expect_true(is.na(row$n_tracts_num))
  expect_true(is.na(row$n_tracts_den))
  expect_match(conditionMessage(res$warnings[[1L]]), "carrier missing")
})
