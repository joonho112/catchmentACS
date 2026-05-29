# ============================================================================
# Unit tests for cacs_capture_conditions().
# ============================================================================


test_that("C-CAPTURE-01 empty expression returns locked tibble schema", {
  out <- cacs_capture_conditions(1 + 1)

  expect_s3_class(out, "tbl_df")
  expect_identical(names(out),
                   c("class", "message", "phase", "timestamp", "call"))
  expect_identical(nrow(out), 0L)
  expect_type(out$class, "character")
  expect_type(out$message, "character")
  expect_type(out$phase, "character")
  expect_s3_class(out$timestamp, "POSIXct")
  expect_type(out$call, "character")
})


test_that("C-CAPTURE-02 captures messages and warnings with phase metadata", {
  out <- cacs_capture_conditions({
    .cacs_emit("inform", "progress_summary",
               "Capture helper progress", phase = "unit-capture")
    .cacs_emit("warn", "runtime",
               "Capture helper warning", phase = "unit-capture")
  })

  expect_equal(out$class,
               c("catchmentACS_message_progress_summary",
                 "catchmentACS_warning_runtime"))
  expect_match(out$message[[1L]], "Capture helper progress")
  expect_match(out$message[[2L]], "Capture helper warning")
  expect_equal(out$phase, c("unit-capture", "unit-capture"))
  expect_true(all(!is.na(out$timestamp)))
})


test_that("C-CAPTURE-03 short class filters capture only matching leaves", {
  out <- suppressMessages(cacs_capture_conditions({
    .cacs_emit("inform", "water_tract_filter",
               "Water notice", phase = "acs")
    .cacs_emit("inform", "progress_summary",
               "Progress notice", phase = "progress")
  }, classes = "water_tract_filter"))

  expect_equal(nrow(out), 1L)
  expect_equal(out$class, "catchmentACS_message_water_tract_filter")
  expect_equal(out$phase, "acs")
})


test_that("C-CAPTURE-04 parent class filters capture descendants", {
  out <- suppressMessages(cacs_capture_conditions({
    .cacs_emit("inform", "progress_summary",
               "Progress notice", phase = "progress")
    .cacs_emit("warn", "carrier_missing",
               "Carrier warning", phase = "derive_rates")
  }, classes = "catchmentACS_warning_runtime"))

  expect_equal(nrow(out), 1L)
  expect_equal(out$class, "catchmentACS_warning_carrier_missing")
  expect_equal(out$phase, "derive_rates")
})


test_that("C-CAPTURE-05 timestamps are monotone non-decreasing", {
  out <- cacs_capture_conditions({
    .cacs_emit("inform", "progress_tick", "tick 1", phase = "p")
    .cacs_emit("inform", "progress_tick", "tick 2", phase = "p")
    .cacs_emit("inform", "progress_summary", "done", phase = "p")
  })

  expect_equal(nrow(out), 3L)
  expect_true(all(diff(as.numeric(out$timestamp)) >= 0))
})


test_that("C-CAPTURE-06 conditions are captured in non-interactive Rscript", {
  script <- tempfile("capture-conditions-", fileext = ".R")
  out_file <- tempfile("capture-conditions-", fileext = ".rds")
  pkg_path <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  writeLines(c(
    "library(cli)",
    "library(rlang)",
    "library(tibble)",
    sprintf("pkg_path <- %s", shQuote(pkg_path)),
    "if (file.exists(file.path(pkg_path, 'R', 'cli.R'))) {",
    "  source(file.path(pkg_path, 'R', 'cli.R'))",
    "  source(file.path(pkg_path, 'R', 'capture-conditions.R'))",
    "  emit <- .cacs_emit",
    "} else {",
    "  library(catchmentACS)",
    "  emit <- getFromNamespace('.cacs_emit', 'catchmentACS')",
    "}",
    "out <- cacs_capture_conditions({",
    "  emit('inform', 'progress_summary',",
    "    'Rscript capture works', phase = 'rscript')",
    "})",
    sprintf("saveRDS(out, %s)", shQuote(out_file))
  ), script)
  withr::defer(unlink(script))
  withr::defer(unlink(out_file))

  res <- system2(file.path(R.home("bin"), "Rscript"),
                 c("--vanilla", script),
                 stdout = TRUE, stderr = TRUE)
  expect_null(attr(res, "status"), info = paste(res, collapse = "\n"))
  out <- readRDS(out_file)

  expect_true(file.exists(out_file))
  expect_equal(nrow(out), 1L)
  expect_equal(out$class[[1L]], "catchmentACS_message_progress_summary")
  expect_equal(out$phase[[1L]], "rscript")
  expect_false(any(grepl("Rscript capture works", res, fixed = TRUE)))
})


test_that("C-CAPTURE-07 helper does not swallow catchmentACS errors", {
  expect_error(
    cacs_capture_conditions(
      .cli_abort_schema("Capture helper error")
    ),
    class = "catchmentACS_error_schema"
  )
})


test_that("C-CAPTURE-08 invalid class filters abort clearly", {
  expect_error(
    cacs_capture_conditions(1 + 1, classes = NA_character_),
    class = "catchmentACS_error_schema"
  )
})


test_that("C-CAPTURE-09 known class helper covers current public tree", {
  known <- .cacs_known_classes(include_parents = TRUE)

  expect_true("catchmentACS_condition" %in% known)
  expect_true("catchmentACS_message_water_tract_filter" %in% known)
  expect_true("catchmentACS_warning_carrier_missing" %in% known)
  expect_true("catchmentACS_warning_partial" %in% known)
  expect_true("catchmentACS_error_annulus_input" %in% known)
  expect_true("catchmentACS_error_missing_suggest" %in% known)
})


.capture_help_text <- function(topic) {
  rd_path <- file.path("man", paste0(topic, ".Rd"))
  if (!file.exists(rd_path)) {
    rd_path <- testthat::test_path("..", "..", "man", paste0(topic, ".Rd"))
  }
  if (file.exists(rd_path)) {
    return(paste(capture.output(tools::Rd2txt(rd_path)), collapse = "\n"))
  }
  help_ref <- utils::help(topic, package = "catchmentACS")
  if (length(help_ref) == 0L) {
    testthat::skip(paste("help topic unavailable:", topic))
  }
  paste(capture.output(tools::Rd2txt(utils:::.getHelpFile(help_ref))),
        collapse = "\n")
}


test_that("C-CAPTURE-10 export and condition help page are available", {
  expect_true("cacs_capture_conditions" %in%
                getNamespaceExports("catchmentACS"))

  helper_txt <- .capture_help_text("cacs_capture_conditions")
  expect_match(helper_txt, "Evaluates an expression", fixed = TRUE)
  expect_match(helper_txt, "standardized catchmentACS", fixed = TRUE)
  expect_match(helper_txt, "water_tract_filter", fixed = TRUE)

  tree_txt <- .capture_help_text("catchmentACS-conditions")
  expect_match(tree_txt, "catchmentACS_condition", fixed = TRUE)
  expect_match(tree_txt, "catchmentACS_message_water_tract_filter",
               fixed = TRUE)
  expect_match(tree_txt, "catchmentACS_message_progress", fixed = TRUE)
  expect_match(tree_txt, "catchmentACS_message_cache", fixed = TRUE)
  expect_match(tree_txt, "catchmentACS_warning_runtime", fixed = TRUE)
  expect_match(tree_txt, "catchmentACS_error_missing_suggest", fixed = TRUE)
})
