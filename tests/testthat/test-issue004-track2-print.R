# ===========================================================================
# test-issue004-track2-print.R
#
# v0.4 plan Step 5.2 — Track 2: print.cacs_run_result() per-site mini-block.
# ===========================================================================

.rr_fixture <- function() {
  readRDS(system.file("testdata", "run_result_3site_demo.rds", package = "catchmentACS"))
}

# --- The cli print method is mostly visual; we test the data path (rates
# preview tibble construction) and the option-gated branch logic. -----------

test_that("ISSUE004-T2-01 default option=12L + 3-site run → per-site block branch active", {
  rr <- .rr_fixture()
  # Mirror the print method's branch logic:
  per_site_max <- getOption("catchmentACS.summary_per_site_max", 12L)
  n_sites <- dplyr::n_distinct(rr$site_id)
  expect_true(n_sites <= per_site_max)
  expect_true(per_site_max > 0L)
  # Pivot exists + non-empty
  pivot <- catchmentACS:::.cacs_rates_per_site_pivot(rr)
  expect_gt(nrow(pivot$wide), 0L)
})

test_that("ISSUE004-T2-02 option=0L disables per-site block branch", {
  rr <- .rr_fixture()
  withr::local_options(catchmentACS.summary_per_site_max = 0L)
  per_site_max <- getOption("catchmentACS.summary_per_site_max", 12L)
  n_sites <- dplyr::n_distinct(rr$site_id)
  # Branch is disabled even though n_sites <= 12: option=0 supersedes.
  expect_false(isTRUE(n_sites <= per_site_max) && isTRUE(per_site_max > 0L))
})

test_that("ISSUE004-T2-03 option=Inf always enables per-site block", {
  rr <- .rr_fixture()
  withr::local_options(catchmentACS.summary_per_site_max = Inf)
  per_site_max <- getOption("catchmentACS.summary_per_site_max", 12L)
  n_sites <- dplyr::n_distinct(rr$site_id)
  expect_true(isTRUE(n_sites <= per_site_max) && isTRUE(per_site_max > 0L))
})

test_that("ISSUE004-T2-04 print method invocation does not throw an error", {
  rr <- .rr_fixture()
  # Direct invocation test: print method runs end-to-end without error.
  # Visual output validated manually; cli/rlang messages route through
  # the message stream, so use capture_messages() for the cli path.
  expect_no_error(invisible(capture.output({
    testthat::capture_messages(print(rr))
  })))
})

test_that("ISSUE004-T2-05 source contains Top 5 rates (cross-site) header", {
  # Source-level regression for the rename Top-3 → Top 5 (cross-site).
  .src <- testthat::test_path("..", "..", "R", "run.R")
  skip_if_not(file.exists(.src))
  src <- readLines(.src)
  combined <- paste(src, collapse = "\n")
  expect_match(combined, "Top 5 rates \\(cross-site")
  expect_match(combined, "Rates per site")
})


test_that("ISSUE004-T2-06 print Rd documents cli message-stream capture", {
  rd_path <- file.path(
    testthat::test_path("..", ".."),
    "man",
    "print.cacs_run_result.Rd"
  )
  skip_if_not(file.exists(rd_path), "print.cacs_run_result.Rd not generated")

  rd_txt <- paste(capture.output(tools::Rd2txt(rd_path)), collapse = "\n")
  rd_txt <- gsub(".\b", "", rd_txt, fixed = FALSE)
  expect_match(rd_txt, "cli output", fixed = TRUE)
  expect_match(rd_txt, "capture_messages", fixed = TRUE)
  expect_match(rd_txt, "capture.output(print(x), type = \"message\")",
               fixed = TRUE)
  expect_match(rd_txt, "Top 5 rates", fixed = TRUE)
  expect_false(grepl("top-3 rates preview", rd_txt, fixed = TRUE))
})
