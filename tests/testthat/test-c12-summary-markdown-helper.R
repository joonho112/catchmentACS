# ============================================================================
# C-12 summary Markdown helper tests.
# ============================================================================


.c12_summary_tbl <- function() {
  tibble::tibble(
    variable = c("poverty_rate", "snap_rate"),
    mean     = c(0.123456, 0.987654),
    sd       = c(0.111111, NA_real_),
    n_NA     = c(0L, 2L)
  )
}


test_that("C-12 cacs_summary_as_markdown() emits pipe Markdown with 3 digits", {
  testthat::skip_if_not_installed("knitr")

  summary_tbl <- .c12_summary_tbl()
  md <- cacs_summary_as_markdown(summary_tbl)

  expect_s3_class(md, "knitr_kable")
  expect_identical(attr(md, "format"), "pipe")
  expect_true(any(grepl("|poverty_rate", as.character(md), fixed = TRUE)))
  expect_true(any(grepl("0.123", as.character(md), fixed = TRUE)))
  expect_false(any(grepl("0.123456", as.character(md), fixed = TRUE)))
})


test_that("C-12 cacs_summary_as_markdown() respects dots passed to kable", {
  testthat::skip_if_not_installed("knitr")

  summary_tbl <- .c12_summary_tbl()
  md <- cacs_summary_as_markdown(
    summary_tbl,
    digits = 2,
    caption = "Rate summary",
    col.names = c("Rate", "Mean", "SD", "Missing"),
    align = "lrrr"
  )

  expected <- knitr::kable(
    summary_tbl,
    format = "pipe",
    digits = 2,
    caption = "Rate summary",
    col.names = c("Rate", "Mean", "SD", "Missing"),
    align = "lrrr"
  )

  expect_identical(md, expected)
  expect_true(any(grepl("0.12", as.character(md), fixed = TRUE)))
  expect_false(any(grepl("0.123", as.character(md), fixed = TRUE)))
})


test_that("C-12 cacs_summary_as_markdown() rejects non-table inputs", {
  expect_error(
    cacs_summary_as_markdown(list(variable = "poverty_rate")),
    class = "catchmentACS_error_schema"
  )
})


test_that("C-12 cacs_summary_as_markdown() reports missing knitr as classed error", {
  orig_require_namespace <- get("requireNamespace", envir = baseenv())
  local_mock <- function(package, ..., quietly = TRUE) {
    if (identical(package, "knitr")) return(FALSE)
    orig_require_namespace(package, ..., quietly = quietly)
  }

  testthat::with_mocked_bindings(
    requireNamespace = local_mock,
    .package = "base",
    {
      expect_error(
        cacs_summary_as_markdown(.c12_summary_tbl()),
        class = "catchmentACS_error_missing_suggest"
      )
    }
  )
})
