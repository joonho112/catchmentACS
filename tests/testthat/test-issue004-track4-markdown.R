# ===========================================================================
# test-issue004-track4-markdown.R
#
# v0.4 plan Step 5.4 — Track 4: cacs_summary_as_markdown() per-site shape
# detection + rendering.
# ===========================================================================

.rr_fixture <- function() {
  readRDS(system.file("testdata", "run_result_3site_demo.rds", package = "catchmentACS"))
}

# --- Per-site (numeric) rendering ------------------------------------------

test_that("ISSUE004-T4-01 rates_per_site (numeric) renders with key columns first", {
  rr <- .rr_fixture()
  sm <- summary(rr)
  md <- cacs_summary_as_markdown(sm$rates_per_site)
  expect_s3_class(md, "knitr_kable")
  md_str <- paste(as.character(md), collapse = "\n")
  expect_match(md_str, "\\| ?site_id")
  expect_match(md_str, "drive_time_min")
})

test_that("ISSUE004-T4-02 rates_per_site default caption mentions estimate", {
  rr <- .rr_fixture()
  sm <- summary(rr)
  md <- cacs_summary_as_markdown(sm$rates_per_site)
  md_str <- paste(as.character(md), collapse = "\n")
  expect_match(md_str, "Rates per site")
})

# --- Per-site (MOE string) rendering ---------------------------------------

test_that("ISSUE004-T4-03 rates_per_site_moe renders with 'x ± y' strings", {
  rr <- .rr_fixture()
  sm <- summary(rr)
  md <- cacs_summary_as_markdown(sm$rates_per_site_moe)
  md_str <- paste(as.character(md), collapse = "\n")
  # "± " or " ± " substring in cell content
  expect_match(md_str, "±")
})

test_that("ISSUE004-T4-04 rates_per_site_moe default caption mentions MOE", {
  rr <- .rr_fixture()
  sm <- summary(rr)
  md <- cacs_summary_as_markdown(sm$rates_per_site_moe)
  md_str <- paste(as.character(md), collapse = "\n")
  expect_match(md_str, "MOE")
})

# --- Custom caption override -----------------------------------------------

test_that("ISSUE004-T4-05 custom caption overrides default", {
  rr <- .rr_fixture()
  sm <- summary(rr)
  md <- cacs_summary_as_markdown(sm$rates_per_site,
                                  caption = "Custom caption text")
  md_str <- paste(as.character(md), collapse = "\n")
  expect_match(md_str, "Custom caption text")
})

# --- v0.3 backward-compat: rates_breakdown shape unchanged ----------------

test_that("ISSUE004-T4-06 v0.3 rates_breakdown rendering regression", {
  rr <- .rr_fixture()
  sm <- summary(rr)
  md_v03 <- cacs_summary_as_markdown(sm$rates_breakdown)
  md_str <- paste(as.character(md_v03), collapse = "\n")
  # rates_breakdown 의 column 들이 보임
  expect_match(md_str, "variable")
  expect_match(md_str, "mean")
  expect_match(md_str, "sd")
})

# --- Edge: ambiguous tibble (just site_id without rates) -------------------

test_that("ISSUE004-T4-07 site_id present but no rate cols → v0.3 fallback path", {
  amb_tbl <- tibble::tibble(site_id = c("S1", "S2"), some_col = 1:2)
  md <- cacs_summary_as_markdown(amb_tbl)
  expect_s3_class(md, "knitr_kable")
  # Should not have 'Rates per site' caption (no rate cols → not per-site shape)
  md_str <- paste(as.character(md), collapse = "\n")
  expect_no_match(md_str, "Rates per site")
})
