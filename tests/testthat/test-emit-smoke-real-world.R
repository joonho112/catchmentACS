# ============================================================================
# Env-gated live smoke for Phase 5 condition capture.
# ============================================================================


test_that("P5-SMOKE-01 live water-tract condition capture smoke is gated", {
  testthat::skip_on_cran()
  if (!identical(Sys.getenv("CATCHMENTACS_LIVE_SMOKE"), "true")) {
    testthat::skip("Set CATCHMENTACS_LIVE_SMOKE=true to run Phase 5 live smoke.")
  }
  if (!nzchar(Sys.getenv("CENSUS_API_KEY"))) {
    testthat::skip("CENSUS_API_KEY required for Phase 5 live smoke.")
  }

  cache_dir <- tempfile("phase5-live-smoke-cache-")
  out <- cacs_capture_conditions(
    cacs_acs_prefetch(
      state = "AL",
      year = 2023L,
      variables = "B01003_001",
      cache_dir = cache_dir,
      force_refresh = TRUE,
      drop_water_tracts = TRUE,
      verbose = TRUE
    ),
    classes = "water_tract_filter"
  )

  expect_gte(nrow(out), 1L)
  expect_true(all(out$class == "catchmentACS_message_water_tract_filter"))
})
