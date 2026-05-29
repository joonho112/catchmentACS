# ============================================================================
# Unit tests for cacs_plot_site_pipeline() Step 6.6 impl (the thin
# orchestrator) + the print.cacs_site_plot_pipeline() S3 method.
#
# 5 testcases per the Step 6.6 plan:
#   T-PLOT-PIPE-01  Wrapper returns a classed list with the 4 named elements
#                   (isochrone, intersection, weighted, rates) in order
#   T-PLOT-PIPE-02  All 4 elements inherit from "leaflet" (+ "htmlwidget")
#   T-PLOT-PIPE-03  Stage-input errors propagate (e.g. acs_sf w/o "variable"
#                   column trips Stage 3's schema abort during orchestration)
#   T-PLOT-PIPE-04  `variable` arg is propagated to Stage 3 (bad variable
#                   aborts via Stage 3's catchmentACS_error_schema family)
#   T-PLOT-PIPE-05  print(x) returns x invisibly + emits the expected
#                   header / 4 bullet lines / accessor patterns
#
# Uses bundled fixtures (legacy_2025_isochrones.rds, sample_alabama_subset.rds,
# cacs_alabama_sites). cacs_run() is invoked once via a small 1-site / 1-ring
# subset to produce a real `run_result` for Stage 4 (mirrors the pattern in
# test-unit-plot-stage4.R). Whole suite stays offline + leaflet-gated.
# ============================================================================

testthat::skip_if_not_installed("leaflet")
testthat::skip_if_not_installed("sf")


.load_iso_pipe <- function() {
  path <- system.file("extdata", "legacy_2025_isochrones.rds",
                      package = "catchmentACS")
  testthat::skip_if(!nzchar(path) || !file.exists(path),
                    "legacy_2025_isochrones.rds fixture not installed")
  readRDS(path)
}

.load_acs_pipe <- function() {
  path <- system.file("extdata", "sample_alabama_subset.rds",
                      package = "catchmentACS")
  testthat::skip_if(!nzchar(path) || !file.exists(path),
                    "sample_alabama_subset.rds fixture not installed")
  readRDS(path)
}

.build_run_result_pipe <- function() {
  iso        <- .load_iso_pipe()
  sites_path <- system.file("extdata", "legacy_2025_sites.rds",
                            package = "catchmentACS")
  testthat::skip_if(!nzchar(sites_path) || !file.exists(sites_path),
                    "legacy_2025_sites.rds fixture not installed")
  sites <- readRDS(sites_path)
  acs   <- .load_acs_pipe()
  iso_subset   <- iso[iso$site_id == "AL_SITE_01" & iso$drive_time_min == 5, ]
  sites_subset <- sites[sites$site_id == "AL_SITE_01", ]
  suppressWarnings(cacs_run(
    sites                  = sites_subset,
    state                  = "AL",
    year                   = 2023,
    drive_times            = 5,
    variables              = unname(cacs_acs_default_vars),
    provider               = "osrm",
    precomputed_isochrones = iso_subset,
    acs                    = acs,
    weight_args            = list(weight_method = "area"),
    output                 = "long",
    verbose                = FALSE
  ))
}


test_that("T-PLOT-PIPE-01 returns a classed list with 4 named elements", {
  iso        <- .load_iso_pipe()
  acs        <- .load_acs_pipe()
  run_result <- .build_run_result_pipe()
  out <- cacs_plot_site_pipeline(
    site_id    = "AL_SITE_01",
    iso_sf     = iso,
    tract_sf   = acs,
    acs_sf     = acs,
    run_result = run_result,
    sites_df   = cacs_alabama_sites
  )
  expect_s3_class(out, "cacs_site_plot_pipeline")
  expect_s3_class(out, "list")
  expect_named(out, c("isochrone", "intersection", "weighted", "rates"))
  # Attributes carry the resolved site identity + Stage 3 selector.
  expect_identical(attr(out, "site_id"),  "AL_SITE_01")
  expect_identical(attr(out, "variable"), "B17001_002")
  expect_true(is.numeric(attr(out, "lat")) && is.finite(attr(out, "lat")))
  expect_true(is.numeric(attr(out, "lon")) && is.finite(attr(out, "lon")))
})


test_that("T-PLOT-PIPE-02 all 4 stage outputs inherit from leaflet", {
  iso        <- .load_iso_pipe()
  acs        <- .load_acs_pipe()
  run_result <- .build_run_result_pipe()
  out <- cacs_plot_site_pipeline(
    site_id    = "AL_SITE_01",
    iso_sf     = iso,
    tract_sf   = acs,
    acs_sf     = acs,
    run_result = run_result,
    sites_df   = cacs_alabama_sites
  )
  for (stage in c("isochrone", "intersection", "weighted", "rates")) {
    expect_s3_class(out[[stage]], "leaflet")
    expect_s3_class(out[[stage]], "htmlwidget")
  }
})


test_that("T-PLOT-PIPE-03 stage-input errors propagate from underlying stage", {
  # Strip the `variable` column from acs_sf so Stage 3 inside the pipeline
  # trips its own schema check. The pipeline does NOT swallow per-stage
  # errors -- it surfaces them straight to the caller.
  iso        <- .load_iso_pipe()
  acs        <- .load_acs_pipe()
  run_result <- .build_run_result_pipe()
  acs_no_var <- acs[, setdiff(names(acs), "variable")]
  expect_error(
    cacs_plot_site_pipeline(
      site_id    = "AL_SITE_01",
      iso_sf     = iso,
      tract_sf   = acs,
      acs_sf     = acs_no_var,
      run_result = run_result,
      sites_df   = cacs_alabama_sites
    ),
    class = "catchmentACS_error_schema"
  )
})


test_that("T-PLOT-PIPE-04 invalid `variable` propagates to Stage 3 abort", {
  iso        <- .load_iso_pipe()
  acs        <- .load_acs_pipe()
  run_result <- .build_run_result_pipe()
  expect_error(
    cacs_plot_site_pipeline(
      site_id    = "AL_SITE_01",
      iso_sf     = iso,
      tract_sf   = acs,
      acs_sf     = acs,
      run_result = run_result,
      sites_df   = cacs_alabama_sites,
      variable   = "B_NOT_A_REAL_VARIABLE"
    ),
    class = "catchmentACS_error_schema"
  )
})


test_that("T-PLOT-PIPE-05 print() returns invisibly and emits expected output", {
  iso        <- .load_iso_pipe()
  acs        <- .load_acs_pipe()
  run_result <- .build_run_result_pipe()
  out <- cacs_plot_site_pipeline(
    site_id    = "AL_SITE_01",
    iso_sf     = iso,
    tract_sf   = acs,
    acs_sf     = acs,
    run_result = run_result,
    sites_df   = cacs_alabama_sites
  )
  # Capture the cli output. cli writes to stderr by default, so wrap with
  # capture.output(type = "message") AND silence the return value too.
  printed <- utils::capture.output(
    ret <- withVisible(print(out)),
    type = "output"
  )
  # cli routes most output to stderr -- grab that too so the header line
  # is captured deterministically across testthat output sinks.
  msgs <- utils::capture.output(print(out), type = "message")
  full <- paste(c(printed, msgs), collapse = "\n")

  # Return value is invisible AND identical to the input list.
  expect_false(ret$visible)
  expect_identical(ret$value, out)

  # Header + accessor patterns must surface in the rendered text.
  expect_match(full, "catchmentACS site plot pipeline", fixed = TRUE)
  expect_match(full, "AL_SITE_01",  fixed = TRUE)
  expect_match(full, "B17001_002",  fixed = TRUE)
  expect_match(full, "x$isochrone", fixed = TRUE)
  expect_match(full, "x$intersection", fixed = TRUE)
  expect_match(full, "x$weighted", fixed = TRUE)
  expect_match(full, "x$rates",    fixed = TRUE)
})
