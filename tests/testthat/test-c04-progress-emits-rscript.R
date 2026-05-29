# ============================================================================
# C-04 lock tests: progress emits as classed conditions under Rscript.
# ============================================================================


.c04_capture_progress <- function(n, verbose = TRUE, option = "auto",
                                  throttle = NULL) {
  opts <- list(catchmentACS.progress = option)
  if (!is.null(throttle)) {
    opts$catchmentACS.progress_throttle <- throttle
  }
  withr::with_envvar(c(CACS_QUIET = ""), {
    withr::with_options(opts, {
      cacs_capture_conditions({
        prog <- .cacs_progress_reporter(n = n, label = "Synthetic",
                                        verbose = verbose)
        for (i in seq_len(n)) {
          prog$tick(detail = paste0("site_", i))
        }
        prog$finish(n_success = n, n_failed = 0L)
      }, classes = "catchmentACS_message_progress")
    })
  })
}


test_that("C04-RSCRIPT-01 bookend mode emits summary but no ticks", {
  out <- .c04_capture_progress(n = 3L, verbose = TRUE, option = "auto")

  expect_equal(sum(out$class == "catchmentACS_message_progress_summary"), 1L)
  expect_equal(sum(out$class == "catchmentACS_message_progress_tick"), 0L)
})


test_that("C04-RSCRIPT-02 forced bar emits ticks and summary", {
  out <- .c04_capture_progress(n = 4L, verbose = TRUE, option = "force")

  expect_equal(sum(out$class == "catchmentACS_message_progress_tick"), 4L)
  expect_equal(sum(out$class == "catchmentACS_message_progress_summary"), 1L)
})


test_that("C04-RSCRIPT-03 silent modes emit zero progress conditions", {
  expect_equal(nrow(.c04_capture_progress(n = 4L, verbose = FALSE,
                                          option = "force")), 0L)
  expect_equal(nrow(.c04_capture_progress(n = 4L, verbose = TRUE,
                                          option = "off")), 0L)
})


test_that("C04-RSCRIPT-04 N > 50 throttle emits sparse ticks plus final tick", {
  out <- .c04_capture_progress(n = 60L, verbose = TRUE, option = "force",
                               throttle = 15L)

  expect_equal(sum(out$class == "catchmentACS_message_progress_tick"), 5L)
  expect_equal(sum(out$class == "catchmentACS_message_progress_summary"), 1L)
})


test_that("C04-RSCRIPT-05 progress emits in Rscript subprocess", {
  script <- tempfile("c04-progress-rscript-", fileext = ".R")
  out_file <- tempfile("c04-progress-rscript-", fileext = ".rds")
  pkg_path <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  writeLines(c(
    "library(cli)",
    "library(rlang)",
    "library(tibble)",
    "library(withr)",
    sprintf("pkg_path <- %s", shQuote(pkg_path)),
    "if (file.exists(file.path(pkg_path, 'R', 'cli.R'))) {",
    "  source(file.path(pkg_path, 'R', 'cli.R'))",
    "  source(file.path(pkg_path, 'R', 'capture-conditions.R'))",
    "  reporter <- .cacs_progress_reporter",
    "} else {",
    "  library(catchmentACS)",
    "  reporter <- getFromNamespace('.cacs_progress_reporter', 'catchmentACS')",
    "}",
    "withr::local_options(list(catchmentACS.progress = 'force'))",
    "out <- cacs_capture_conditions({",
    "  prog <- reporter(n = 2L, label = 'Rscript', verbose = TRUE)",
    "  prog$tick(detail = 'one')",
    "  prog$tick(detail = 'two')",
    "  prog$finish(n_success = 2L, n_failed = 0L)",
    "}, classes = 'catchmentACS_message_progress')",
    sprintf("saveRDS(out, %s)", shQuote(out_file))
  ), script)
  withr::defer(unlink(script))
  withr::defer(unlink(out_file))

  res <- system2(file.path(R.home("bin"), "Rscript"),
                 c("--vanilla", script),
                 stdout = TRUE, stderr = TRUE)
  expect_null(attr(res, "status"), info = paste(res, collapse = "\n"))
  out <- readRDS(out_file)

  expect_equal(sum(out$class == "catchmentACS_message_progress_tick"), 2L)
  expect_equal(sum(out$class == "catchmentACS_message_progress_summary"), 1L)
  expect_false(any(grepl("Rscript [", res, fixed = TRUE)))
})
