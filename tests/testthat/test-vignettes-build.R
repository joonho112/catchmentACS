# ============================================================================
# Vignette build and migration-checklist tests.
# ============================================================================


.vignette_source_path <- function(file) {
  candidates <- c(
    testthat::test_path("..", "..", "vignettes", file),
    file.path(getwd(), "vignettes", file),
    system.file("doc", file, package = "catchmentACS")
  )
  for (candidate in candidates) {
    if (nzchar(candidate) && file.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }
  candidates[[1L]]
}


.porting_vignette_path <- function() {
  .vignette_source_path("porting-v01-to-v03.qmd")
}


.porting_v04_vignette_path <- function() {
  .vignette_source_path("porting-v03-to-v04.qmd")
}


test_that("porting vignette has runnable chunks for S-01 through S-08", {
  path <- .porting_vignette_path()
  if (!file.exists(path)) {
    testthat::skip("porting vignette source is not available in this installed test context")
  }
  expect_true(file.exists(path))
  txt <- readLines(path, warn = FALSE)

  for (sid in sprintf("s%02d", 1:8)) {
    expect_true(
      any(grepl(paste0("label: ", sid, "-"), txt, fixed = TRUE)),
      info = paste("missing chunk for", toupper(sid))
    )
  }
  expect_false(any(grepl("#| eval: false", txt, fixed = TRUE)))
  expect_true(any(grepl("cacs_summary_as_markdown", txt, fixed = TRUE)))
})


test_that("v0.4 porting vignette documents new repair surfaces", {
  path <- .porting_v04_vignette_path()
  if (!file.exists(path)) {
    testthat::skip("v0.4 porting vignette source is not available in this installed test context")
  }
  txt <- readLines(path, warn = FALSE)
  expect_true(any(grepl("return_value = \"both\"", txt, fixed = TRUE)))
  expect_true(any(grepl("cacs_validate_osrm_endpoint", txt, fixed = TRUE)))
  expect_true(any(grepl("drive_time_min", txt, fixed = TRUE)))
  expect_true(any(grepl("rates_per_site_moe", txt, fixed = TRUE)))
  expect_true(any(grepl("group_by(cacs_run_result", txt, fixed = TRUE)))
})


.render_vignette_probe <- function(path) {
  out_dir <- tempfile("cacs-vignette-render-")
  dir.create(out_dir, recursive = TRUE)
  output_file <- paste0(tools::file_path_sans_ext(basename(path)), ".html")
  rendered <- quarto::quarto_render(
    input = path,
    output_file = output_file,
    quarto_args = c("--output-dir", out_dir),
    quiet = TRUE
  )
  if (is.null(rendered)) {
    rendered <- file.path(out_dir, output_file)
  }
  rendered
}


test_that("porting vignettes render to non-empty HTML when Quarto is available", {
  if (nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))) {
    testthat::skip("R CMD check runs vignette code in its vignette checks")
  }
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("quarto")
  if (!nzchar(Sys.which("quarto"))) {
    testthat::skip("Quarto CLI is not installed")
  }
  probe <- suppressWarnings(
    system2(
      file.path(R.home("bin"), "Rscript"),
      c("--vanilla", "-e",
        shQuote("library(catchmentACS); stopifnot(exists('cacs_validate_iso'))")),
      stdout = TRUE,
      stderr = TRUE
    )
  )
  if (!is.null(attr(probe, "status"))) {
    testthat::skip("Installed catchmentACS does not yet expose v0.3 helpers")
  }

  paths <- c(.porting_vignette_path(), .porting_v04_vignette_path())
  paths <- paths[file.exists(paths)]
  if (length(paths) == 0L) {
    testthat::skip("porting vignette sources are not available in this installed test context")
  }

  for (path in paths) {
    rendered <- .render_vignette_probe(path)
    expect_true(file.exists(rendered), info = basename(path))
    expect_gt(file.info(rendered)$size, 1000)
  }
})
