# as_tibble() on a cacs_run_result keeps the row order unless the caller asks
# for the rate rows first, and the conversions that dplyr makes internally
# always keep it, so joins and row-wise summaries return the right rows even
# with options(catchmentACS.rate_first_default = TRUE). Bundled data; no
# network.

.order_result <- function() {
  path <- system.file("extdata", "visual_walkthrough_fixture.rds", package = "catchmentACS")
  testthat::skip_if(!nzchar(path), "bundled data not installed")
  readRDS(path)$run_result
}

.same_as_plain <- function(res, verb) {
  plain <- tibble::as_tibble(res, rate_first = FALSE)
  on_result <- suppressMessages(verb(res))
  on_plain <- verb(plain)
  cols <- intersect(names(on_result), names(on_plain))
  identical(as.data.frame(on_result)[cols], as.data.frame(on_plain)[cols])
}

.keys <- tibble::tibble(variable = c("poverty_rate", "B17001_002"))
.verbs <- list(
  semi_join = function(x) dplyr::semi_join(x, .keys, by = "variable"),
  anti_join = function(x) dplyr::anti_join(x, .keys, by = "variable"),
  rowwise_summarise = function(x) {
    dplyr::summarise(dplyr::rowwise(x, variable), e = estimate, .groups = "drop")
  }
)

test_that("ROW-ORDER-01 without the option, as_tibble() keeps the rows in their order", {
  res <- .order_result()
  withr::local_options(catchmentACS.rate_first_default = NULL)
  expect_identical(tibble::as_tibble(res)$variable, res$variable)
  expect_identical(tibble::as_tibble(res, rate_first = NULL)$variable, res$variable)
})

test_that("ROW-ORDER-02 the option's default is to keep the order", {
  # The value .onLoad() gives the option when the user has not set it.
  expect_false(isTRUE(getOption("catchmentACS.rate_first_default")))
})

test_that("ROW-ORDER-03 without the option, semi_join(), anti_join(), and rowwise() summaries return the right rows", {
  res <- .order_result()
  withr::local_options(catchmentACS.rate_first_default = NULL)
  for (nm in names(.verbs)) {
    expect_true(.same_as_plain(res, .verbs[[nm]]), label = nm)
  }
})

test_that("ROW-ORDER-04 with the option set to TRUE, dplyr's own conversions still keep the order", {
  res <- .order_result()
  withr::local_options(catchmentACS.rate_first_default = TRUE)
  for (nm in names(.verbs)) {
    expect_true(.same_as_plain(res, .verbs[[nm]]), label = nm)
  }
})

test_that("ROW-ORDER-05 rate_first = TRUE, or the option in code run from the global environment, puts the rate rows first", {
  res <- .order_result()
  rate_vars <- names(cacs_acs_default_rates)
  first_rows <- function(tbl) utils::head(tbl$variable, 5L)
  explicit <- suppressMessages(tibble::as_tibble(res, rate_first = TRUE))
  expect_true(all(first_rows(explicit) %in% rate_vars))

  withr::local_options(catchmentACS.rate_first_default = TRUE)
  user_env <- new.env(parent = globalenv())
  assign("res", res, envir = user_env)
  from_user <- suppressMessages(eval(quote(tibble::as_tibble(res)), envir = user_env))
  expect_true(all(first_rows(from_user) %in% rate_vars))
  expect_setequal(from_user$variable, res$variable)

  # Code whose top environment is not the global environment keeps the order.
  other_env <- new.env(parent = baseenv())
  assign("res", res, envir = other_env)
  from_other <- eval(quote(tibble::as_tibble(res)), envir = other_env)
  expect_identical(from_other$variable, res$variable)
})
