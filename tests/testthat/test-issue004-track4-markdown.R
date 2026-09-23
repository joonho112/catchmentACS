# ===========================================================================
# test-issue004-track4-markdown.R
#
# Track 4: cacs_summary_as_markdown() per-site shape
# detection + rendering.
# ===========================================================================

.rr_fixture <- function() {
  readRDS(system.file("testdata", "run_result_3site_demo.rds", package = "catchmentACS"))
}

# --- Per-site (numeric) rendering ------------------------------------------

test_that("ISSUE004-T4-01 rates_per_site (numeric) renders with key columns first", {
  testthat::skip_if_not_installed("knitr")
  rr <- .rr_fixture()
  sm <- summary(rr)
  md <- cacs_summary_as_markdown(sm$rates_per_site)
  expect_s3_class(md, "knitr_kable")
  md_str <- paste(as.character(md), collapse = "\n")
  expect_match(md_str, "\\| ?site_id")
  expect_match(md_str, "drive_time_min")
})

test_that("ISSUE004-T4-02 rates_per_site default caption mentions estimate", {
  testthat::skip_if_not_installed("knitr")
  rr <- .rr_fixture()
  sm <- summary(rr)
  md <- cacs_summary_as_markdown(sm$rates_per_site)
  md_str <- paste(as.character(md), collapse = "\n")
  expect_match(md_str, "Rates per site")
})

# --- Per-site (MOE string) rendering ---------------------------------------

test_that("ISSUE004-T4-03 rates_per_site_moe renders with 'x ± y' strings", {
  testthat::skip_if_not_installed("knitr")
  rr <- .rr_fixture()
  sm <- summary(rr)
  md <- cacs_summary_as_markdown(sm$rates_per_site_moe)
  md_str <- paste(as.character(md), collapse = "\n")
  # "± " or " ± " substring in cell content
  expect_match(md_str, "±")
})

test_that("ISSUE004-T4-04 rates_per_site_moe default caption mentions MOE", {
  testthat::skip_if_not_installed("knitr")
  rr <- .rr_fixture()
  sm <- summary(rr)
  md <- cacs_summary_as_markdown(sm$rates_per_site_moe)
  md_str <- paste(as.character(md), collapse = "\n")
  expect_match(md_str, "MOE")
})

# --- Custom caption override -----------------------------------------------

test_that("ISSUE004-T4-05 custom caption overrides default", {
  testthat::skip_if_not_installed("knitr")
  rr <- .rr_fixture()
  sm <- summary(rr)
  md <- cacs_summary_as_markdown(sm$rates_per_site,
                                  caption = "Custom caption text")
  md_str <- paste(as.character(md), collapse = "\n")
  expect_match(md_str, "Custom caption text")
})

# --- v0.3 backward-compat: rates_breakdown shape unchanged ----------------

test_that("ISSUE004-T4-06 v0.3 rates_breakdown rendering regression", {
  testthat::skip_if_not_installed("knitr")
  rr <- .rr_fixture()
  sm <- summary(rr)
  md_v03 <- cacs_summary_as_markdown(sm$rates_breakdown)
  md_str <- paste(as.character(md_v03), collapse = "\n")
  # The columns of rates_breakdown are shown.
  expect_match(md_str, "variable")
  expect_match(md_str, "mean")
  expect_match(md_str, "sd")
})

# --- Edge: ambiguous tibble (just site_id without rates) -------------------

test_that("ISSUE004-T4-07 site_id present but no rate cols → v0.3 fallback path", {
  testthat::skip_if_not_installed("knitr")
  amb_tbl <- tibble::tibble(site_id = c("S1", "S2"), some_col = 1:2)
  md <- cacs_summary_as_markdown(amb_tbl)
  expect_s3_class(md, "knitr_kable")
  # Should not have 'Rates per site' caption (no rate cols → not per-site shape)
  md_str <- paste(as.character(md), collapse = "\n")
  expect_no_match(md_str, "Rates per site")
})

# --- Confidence level in the caption ---------------------------------------

test_that("ISSUE004-T4-08 the MOE caption names the confidence level of the result", {
  testthat::skip_if_not_installed("knitr")
  rr <- .rr_fixture()
  attr(rr, "cacs_confidence_level") <- 0.95
  md <- cacs_summary_as_markdown(summary(rr)$rates_per_site_moe)
  md_str <- paste(as.character(md), collapse = "\n")
  expect_match(md_str, "95% MOE", fixed = TRUE)
  expect_false(grepl("90% MOE", md_str, fixed = TRUE))
  # The level is not rounded by options(digits).
  attr(rr, "cacs_confidence_level") <- 0.975
  md_975 <- withr::with_options(list(digits = 2), {
    paste(as.character(cacs_summary_as_markdown(summary(rr)$rates_per_site_moe)), collapse = "\n")
  })
  expect_match(md_975, "97.5% MOE", fixed = TRUE)
})

test_that("ISSUE004-T4-09 a table without a recorded level gets a caption without a level", {
  testthat::skip_if_not_installed("knitr")
  rr <- .rr_fixture()
  tbl <- summary(rr)$rates_per_site_moe
  attr(tbl, "cacs_confidence_level") <- NULL
  md_str <- paste(as.character(cacs_summary_as_markdown(tbl)), collapse = "\n")
  expect_match(md_str, "Rates per site (estimate ± MOE)", fixed = TRUE)
})
