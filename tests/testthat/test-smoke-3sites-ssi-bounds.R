skip_live_phase4_ssi_smoke <- function() {
  if (!identical(Sys.getenv("CACS_LIVE_OSRM"), "1") ||
      !identical(Sys.getenv("CACS_LIVE_CENSUS"), "1") ||
      !nzchar(Sys.getenv("CENSUS_API_KEY"))) {
    testthat::skip(
      "set CACS_LIVE_OSRM=1, CACS_LIVE_CENSUS=1, and CENSUS_API_KEY to run Phase 4 SSI smoke"
    )
  }
}

phase4_ssi_smoke_sites <- function() {
  tibble::tibble(
    site_id = c("AL_BHM_01", "AL_MOB_01", "AL_HSV_01"),
    lon = c(-86.8025, -88.0399, -86.5861),
    lat = c(33.5207, 30.6954, 34.7304)
  )
}

phase4_ssi_smoke_run <- function(site_id) {
  skip_live_phase4_ssi_smoke()
  site <- phase4_ssi_smoke_sites()[phase4_ssi_smoke_sites()$site_id == site_id, ]
  out <- cacs_run(
    sites = site,
    state = "AL",
    year = 2023,
    drive_times = 10L,
    variables = NULL,
    provider = "osrm",
    cache_dir = tempfile("cacs_phase4_ssi_smoke_"),
    verbose = FALSE
  )
  rates <- out[out$estimand_family == "derived_rate", ]
  bounds <- catchmentACS:::.SANCTIONED_RATE_BOUNDS_V1
  for (rate in names(bounds)) {
    row <- rates[rates$variable == rate, ]
    testthat::expect_equal(nrow(row), 1L)
    testthat::expect_true(is.finite(row$estimate))
    testthat::expect_gte(row$estimate, bounds[[rate]][["min"]])
    testthat::expect_lte(row$estimate, bounds[[rate]][["max"]])
  }
  ssi <- rates[rates$variable == "ssi_rate", ]
  testthat::expect_lt(ssi$estimate, 1)
  testthat::expect_lt(ssi$estimate, 0.15)
}

test_that("P4-SMOKE-SSI-01 Birmingham SSI rate is bounded in live run", {
  phase4_ssi_smoke_run("AL_BHM_01")
})

test_that("P4-SMOKE-SSI-02 Mobile SSI rate is bounded in live run", {
  phase4_ssi_smoke_run("AL_MOB_01")
})

test_that("P4-SMOKE-SSI-03 Huntsville SSI rate is bounded in live run", {
  phase4_ssi_smoke_run("AL_HSV_01")
})

