# ============================================================================
# Unit tests for R/isochrone-mapbox.R and R/isochrone-r5r.R — Step 3.3 stubs.
# 2 cases per §38.5 verification 5 + 6:
#   T-STUB-MAPBOX: cacs_isochrone(provider = "mapbox", ...) aborts with
#                  catchmentACS_error_credential + current-release hint
#   T-STUB-R5R:    cacs_isochrone(provider = "r5r", ...) aborts with
#                  catchmentACS_error_credential + current-release hint
#
# These tests verify that the deferred provider stubs (R/isochrone-mapbox.R and
# R/isochrone-r5r.R) route fail-loud rather than silently no-op'ing. If either
# provider becomes live in a future release, replace these checks with
# happy-path integration tests like test-integration-isochrone-ors.R.
# ============================================================================


.mk_stub_sites <- function(id = "S01", lon = -86.80902, lat = 33.52203) {
  tibble::tibble(
    site_id = id,
    lon = lon,
    lat = lat
  )
}


# ============================================================================
# T-STUB-MAPBOX: provider = "mapbox" aborts with current-release hint
# ============================================================================

test_that("T-STUB-MAPBOX provider = 'mapbox' aborts fail-loud with current-release hint", {
  sites <- .mk_stub_sites()

  expect_error(
    cacs_isochrone(
      sites        = sites,
      provider     = "mapbox",
      drive_times  = 5L,
      mapbox_token = "FAKE_TOKEN_FOR_TEST"
    ),
    regexp = "current release",
    class  = "catchmentACS_error_credential"
  )
})


# ============================================================================
# T-STUB-R5R: provider = "r5r" aborts with current-release hint
# ============================================================================

test_that("T-STUB-R5R provider = 'r5r' aborts fail-loud with current-release hint", {
  sites <- .mk_stub_sites()

  # r5r_core: pass a list whose first class is "r5r_core" so the Step 3.1
  # scalar-arg validation (`.validate_iso_scalar_args`) accepts it as a
  # "looks like an r5r core" object. The stub will then abort with the
  # current-release deferral message regardless.
  fake_core <- structure(list(), class = c("r5r_core", "list"))

  expect_error(
    cacs_isochrone(
      sites       = sites,
      provider    = "r5r",
      drive_times = 5L,
      r5r_core    = fake_core
    ),
    regexp = "current release",
    class  = "catchmentACS_error_credential"
  )
})
