skip_live_phase3_smoke <- function() {
  testthat::skip_if_not(
    identical(Sys.getenv("CACS_LIVE_OSRM"), "1") &&
      identical(Sys.getenv("CACS_LIVE_CENSUS"), "1") &&
      nzchar(Sys.getenv("CENSUS_API_KEY")),
    "set CACS_LIVE_OSRM=1, CACS_LIVE_CENSUS=1, and CENSUS_API_KEY to run Phase 3 live smoke"
  )
}

phase3_smoke_sites <- function() {
  fx <- load_replay_fixture("032_3site_fresh")
  fx$sites_sf
}

phase3_smoke_vars <- function() {
  sort(unique(load_replay_fixture("032_3site_fresh")$acs_sf$variable))
}

phase3_smoke_run <- function(site_id, cache_mode) {
  skip_live_phase3_smoke()
  site <- phase3_smoke_sites()[phase3_smoke_sites()$site_id == site_id, ]
  vars <- phase3_smoke_vars()
  with_test_cache(mode = "production", {
    if (identical(cache_mode, "disabled")) {
      cacs_set_cache(FALSE)
    }
    acs <- cacs_acs_prefetch(
      state = "AL",
      year = 2023L,
      variables = vars,
      force_refresh = identical(cache_mode, "force_refresh"),
      verbose = FALSE
    )
    expect_gt(nrow(acs), 1000L)
    expect_false(nrow(acs) == 39L)
    if (identical(cache_mode, "enabled")) {
      acs2 <- cacs_acs_prefetch(
        state = "AL", year = 2023L, variables = vars, verbose = FALSE
      )
      expect_identical(acs, acs2)
      expect_gte(cacs_get_cache_state()$hits[[1]][["acs"]], 1L)
    }
    if (identical(cache_mode, "force_refresh")) {
      expect_equal(cacs_get_cache_state()$hits[[1]][["acs"]], 0L)
    }

    iso <- cacs_isochrone(
      sites = site,
      drive_times = 5L,
      provider = "osrm",
      osrm_mode = "demo",
      verbose = FALSE
    )
    weighted <- cacs_intersect_weight(iso, acs, verbose = FALSE)
    expect_gt(nrow(weighted), 0L)
    if (identical(cache_mode, "disabled")) {
      expect_equal(sum(cacs_cache_status()$n_entries), 0L)
    }
  })
}

test_that("PHASE3-SMOKE-01 Birmingham cache enabled live workflow is gated", {
  phase3_smoke_run("AL_BHM_01", "enabled")
})

test_that("PHASE3-SMOKE-02 Birmingham cache disabled live workflow is gated", {
  phase3_smoke_run("AL_BHM_01", "disabled")
})

test_that("PHASE3-SMOKE-03 Birmingham force_refresh live workflow is gated", {
  phase3_smoke_run("AL_BHM_01", "force_refresh")
})

test_that("PHASE3-SMOKE-04 Mobile cache enabled live workflow is gated", {
  phase3_smoke_run("AL_MOB_01", "enabled")
})

test_that("PHASE3-SMOKE-05 Mobile cache disabled live workflow is gated", {
  phase3_smoke_run("AL_MOB_01", "disabled")
})

test_that("PHASE3-SMOKE-06 Mobile force_refresh live workflow is gated", {
  phase3_smoke_run("AL_MOB_01", "force_refresh")
})

test_that("PHASE3-SMOKE-07 Huntsville cache enabled live workflow is gated", {
  phase3_smoke_run("AL_HSV_01", "enabled")
})

test_that("PHASE3-SMOKE-08 Huntsville cache disabled live workflow is gated", {
  phase3_smoke_run("AL_HSV_01", "disabled")
})

test_that("PHASE3-SMOKE-09 Huntsville force_refresh live workflow is gated", {
  phase3_smoke_run("AL_HSV_01", "force_refresh")
})
