# ACS data given by the user: a tract repeated for the same variable is an
# error rather than being added twice, and Census Bureau annotation codes
# (such as -666666666) in place of an estimate or a margin of error become NA
# with a warning rather than entering the sums. Made-up tracts and bundled
# data; no network.

.run_intersect <- function(x) {
  withr::with_options(
    list(catchmentACS.cache_enabled = FALSE),
    suppressMessages(cacs_intersect_weight(x$iso, x$acs, verbose = FALSE))
  )
}

test_that("ACS-DUP-01 cacs_acs_validate() stops at a tract repeated for the same variable", {
  x <- helper_acs_two_tracts()
  expect_true(cacs_acs_validate(x$acs))
  err <- tryCatch(cacs_acs_validate(x$acs[c(1, 2, 2), ]), error = function(e) e)
  expect_s3_class(err, "catchmentACS_error_schema")
  expect_match(conditionMessage(err), "01001000002", fixed = TRUE)
})

test_that("ACS-DUP-02 cacs_intersect_weight() stops instead of adding a repeated row twice", {
  x <- helper_acs_two_tracts()
  x$acs <- x$acs[c(1, 2, 2), ]
  expect_error(.run_intersect(x), class = "catchmentACS_error_schema")
})

test_that("ACS-CODE-01 an annotation code in place of an estimate becomes NA with a warning", {
  x <- helper_acs_two_tracts(estimate = c(1000, -666666666))
  expect_warning(out <- .run_intersect(x), class = "catchmentACS_warning_runtime")
  expect_true(is.na(out$estimate))
  expect_true(is.na(out$moe))
})

test_that("ACS-CODE-02 an annotation code in place of a margin of error next to an estimate becomes NA with a warning", {
  base <- .run_intersect(helper_acs_two_tracts())
  x <- helper_acs_two_tracts(moe = c(50, -555555555))
  expect_warning(out <- .run_intersect(x), class = "catchmentACS_warning_runtime")
  expect_equal(out$estimate, base$estimate)
  expect_true(is.na(out$moe))
})

test_that("ACS-CODE-03 a margin-of-error code next to a missing estimate gives no warning", {
  x <- helper_acs_two_tracts(estimate = c(1000, NA), moe = c(50, -555555555))
  expect_no_warning(out <- .run_intersect(x))
  expect_true(is.na(out$estimate))
})

test_that("ACS-CODE-04 the bundled ACS sample gives no warning about codes or repeated rows", {
  loadNamespace("sf")
  iso_path <- system.file("extdata", "legacy_2025_isochrones.rds", package = "catchmentACS")
  acs_path <- system.file("extdata", "sample_alabama_subset.rds", package = "catchmentACS")
  testthat::skip_if(!nzchar(iso_path) || !nzchar(acs_path), "bundled data not installed")
  iso <- readRDS(iso_path)
  iso <- iso[iso$site_id %in% c("AL_SITE_07", "AL_SITE_08") & iso$drive_time_min == 10L, ]
  acs <- readRDS(acs_path)
  expect_true(cacs_acs_validate(acs))
  warnings_seen <- character()
  withCallingHandlers(
    .run_intersect(list(iso = iso, acs = acs)),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("annotation code", warnings_seen, fixed = TRUE)))
})
