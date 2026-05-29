# ============================================================================
# Env-gated live OSRM smoke for the v0.5 demo-safe omitted-res policy.
# ============================================================================


.p2_live_site <- function(site_id, lon, lat) {
  tibble::tibble(site_id = site_id, lon = lon, lat = lat)
}

.p2_live_osrm_demo_default <- function(site_id, lon, lat) {
  testthat::skip_on_cran()
  testthat::skip_if(Sys.getenv("CACS_LIVE_OSRM") != "1",
                    "set CACS_LIVE_OSRM=1 to run live OSRM smoke")
  testthat::skip_if_not_installed("osrm")

  td <- tempfile("cacs_phase2_live_osrm_")
  on.exit(if (dir.exists(td)) unlink(td, recursive = TRUE), add = TRUE)

  site <- .p2_live_site(site_id, lon, lat)
  iso_default <- suppressMessages(cacs_isochrone(
    sites = site,
    drive_times = 10L,
    provider = "osrm",
    cache_dir = td,
    verbose = FALSE
  ))

  expect_true(.validate_iso_schema(iso_default))
  expect_equal(unique(iso_default$ring_topology), "cumulative")
  expect_equal(unique(iso_default$provider), "osrm")
  prov <- attr(iso_default, "cacs_isochrone_provenance")
  expect_identical(prov$res_param, .OSRM_RES_DEFAULT_DEMO)
  expect_true(isTRUE(prov$osrm_request_budget$public_demo))
  expect_lt(prov$osrm_request_budget$sleep_floor_sec_per_site, 65L)
}


test_that("T-P2-LIVE-OSRM-01 Birmingham omitted demo res=30 live smoke validates", {
  .p2_live_osrm_demo_default("AL_BHM_01", -86.8025, 33.5207)
})

test_that("T-P2-LIVE-OSRM-02 Mobile omitted demo res=30 live smoke validates", {
  .p2_live_osrm_demo_default("AL_MOB_01", -88.0399, 30.6954)
})

test_that("T-P2-LIVE-OSRM-03 Huntsville omitted demo res=30 live smoke validates", {
  .p2_live_osrm_demo_default("AL_HSV_01", -86.5861, 34.7304)
})
