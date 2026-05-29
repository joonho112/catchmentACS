# tests/testthat/test-unit-no-deprecated-args.R
# Regression guard: prevent re-introduction of deprecated tigris args.
# F2 (v0.2.0) — returnclass cleanup for tigris 2.0+.

test_that("no `returnclass =` in R/ source (tigris 2.0+ deprecation)", {
  r_files <- list.files(
    system.file("R", package = "catchmentACS"),
    pattern = "\\.R$",
    full.names = TRUE
  )
  # During devtools::test(), R/*.R isn't in system.file — fall back to
  # devtools::package_file()'s source-tree path:
  if (length(r_files) == 0L) {
    pkg_root <- testthat::test_path("..", "..")
    r_files <- list.files(file.path(pkg_root, "R"), pattern = "\\.R$",
                          full.names = TRUE)
  }
  testthat::skip_if(length(r_files) == 0L,
                    "R/ source not reachable from test context")

  offenders <- character(0)
  for (f in r_files) {
    txt <- readLines(f, warn = FALSE)
    hits <- grep("returnclass\\s*=", txt)
    if (length(hits)) {
      offenders <- c(offenders, sprintf("%s:%s", basename(f),
                                        paste(hits, collapse = ",")))
    }
  }
  testthat::expect_equal(offenders, character(0),
                         info = "F2: tigris::tracts(returnclass=) is deprecated; remove from R/ source")
})


test_that("no `cache = TRUE` in tidycensus::load_variables() calls", {
  r_files <- list.files(
    system.file("R", package = "catchmentACS"),
    pattern = "\\.R$",
    full.names = TRUE
  )
  if (length(r_files) == 0L) {
    pkg_root <- testthat::test_path("..", "..")
    r_files <- list.files(file.path(pkg_root, "R"), pattern = "\\.R$",
                          full.names = TRUE)
  }
  testthat::skip_if(length(r_files) == 0L,
                    "R/ source not reachable from test context")

  offenders <- character(0)
  for (f in r_files) {
    txt <- readLines(f, warn = FALSE)
    hits <- grep("load_variables\\([^\\n]*cache\\s*=\\s*TRUE", txt)
    if (length(hits)) {
      offenders <- c(offenders, sprintf("%s:%s", basename(f),
                                        paste(hits, collapse = ",")))
    }
  }
  testthat::expect_equal(
    offenders,
    character(0),
    info = "tidycensus load_variables cache argument is deprecated and ignored"
  )
})
