# ============================================================================
# Integration tests for R/isochrone-osrm.R — Step 3.2.
# 3 cases per §38.4 + §19.8:
#   T-OSRM-01: happy path single site / single drive_time -> 16-col sf
#   T-OSRM-02: retry-on-5xx — first attempt errors, second succeeds (retry_count = 2)
#   T-OSRM-03: HTTP 429 batch abort -> catchmentACS_error_operator (E-13)
#
# Mocking strategy. `osrm::osrmIsochrone()` is not invoked directly. Instead
# we mock the catchmentACS-internal seam `.osrm_call_isochrone()` via
# testthat::local_mocked_bindings(.package = "catchmentACS"). This lets the
# tests run without requiring an internet connection / running OSRM server
# and keeps the suite hermetic per §16.5.
# ============================================================================


# ---- Local fixture helpers ----- -------------------------------------------

.mk_osrm_site <- function(id = "S01", lon = -86.80902, lat = 33.52203) {
  tib <- tibble::tibble(
    site_id = id,
    lon = lon,
    lat = lat
  )
  sf::st_as_sf(tib, coords = c("lon", "lat"), crs = 4326)
}

# Build a minimal sf with one polygon per break, mimicking osrm::osrmIsochrone()
# return contract (one row per (isomin, isomax) band).
.mk_osrm_response <- function(breaks, center_lon = -86.81, center_lat = 33.52) {
  polys <- lapply(seq_along(breaks), function(j) {
    b <- breaks[[j]]
    # Concentric squares scaled by break minute -> degree halfwidth.
    half <- 0.01 * b
    sf::st_polygon(list(rbind(
      c(center_lon - half, center_lat - half),
      c(center_lon + half, center_lat - half),
      c(center_lon + half, center_lat + half),
      c(center_lon - half, center_lat + half),
      c(center_lon - half, center_lat - half)
    )))
  })
  sfc <- sf::st_sfc(polys, crs = 4326)
  sf::st_sf(
    tibble::tibble(
      id     = seq_along(breaks),
      isomin = c(0L, utils::head(as.integer(breaks), -1L)),
      isomax = as.integer(breaks)
    ),
    geometry = sfc
  )
}


# ============================================================================
# T-OSRM-01: happy path, single site, single drive_time -> 16-col sf
# ============================================================================

test_that("T-OSRM-01 happy path returns 16-col canonical sf (retry_count = 1)", {
  site <- .mk_osrm_site()

  happy_stub <- function(loc, breaks, res = NULL) {
    .mk_osrm_response(breaks)
  }

  testthat::local_mocked_bindings(
    .osrm_call_isochrone = happy_stub,
    .package = "catchmentACS"
  )

  out <- .iso_via_osrm(sites = site, drive_times = 5L, profile = "car",
                       osrm_mode = "demo")

  # sf with 1 row x 16 columns per §19.5
  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 1L)
  expect_equal(ncol(out), 16L)
  expect_named(out, c(
    "site_id", "drive_time_min", "geometry", "provider", "profile",
    "osm_snapshot_date", "routing_engine_version",
    "polygon_simplification_tolerance", "generated_at",
    "isochrone_empty", "provider_requested", "provider_downgrade",
    "osm_snapshot_status", "failure_reason", "retry_count",
    "ring_topology"
  ))

  # Row-level invariants for the happy path
  expect_equal(out$site_id, "S01")
  expect_equal(out$drive_time_min, 5L)
  expect_equal(out$provider, "osrm")
  expect_equal(out$profile, "car")
  expect_true(out$isochrone_empty == FALSE)
  expect_true(is.na(out$failure_reason))
  expect_equal(out$retry_count, 1L)
  expect_equal(out$ring_topology, "cumulative")

  # CRS preserved
  expect_equal(as.integer(sf::st_crs(out)$epsg), 4326L)
})


test_that("T-OSRM-01b annulus response is unioned to cumulative geometry", {
  site <- .mk_osrm_site()

  annulus_stub <- function(loc, breaks, res = NULL) {
    polys <- list(
      sf::st_polygon(list(rbind(
        c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
      ))),
      sf::st_polygon(list(rbind(
        c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0)
      )))
    )
    sf::st_sf(
      tibble::tibble(isomin = c(0L, 5L), isomax = c(5L, 10L)),
      geometry = sf::st_sfc(polys, crs = 4326)
    )
  }

  testthat::local_mocked_bindings(
    .osrm_call_isochrone = annulus_stub,
    .package = "catchmentACS"
  )

  out <- .iso_via_osrm(sites = site, drive_times = c(5L, 10L),
                       profile = "car", osrm_mode = "demo")

  expect_equal(out$ring_topology, rep("cumulative", 2L))
  bbox_5 <- sf::st_bbox(out[1, ])
  bbox_10 <- sf::st_bbox(out[2, ])
  expect_equal(unname(bbox_5[c("xmin", "xmax")]), c(0, 1))
  expect_equal(unname(bbox_10[c("xmin", "xmax")]), c(0, 2))
  expect_gt(as.numeric(sf::st_area(out[2, ])), as.numeric(sf::st_area(out[1, ])))
})


# ============================================================================
# T-OSRM-02: retry-on-5xx — first attempt 503, second 200 (retry_count = 2)
# ============================================================================

test_that("T-OSRM-02 retries on 5xx then succeeds with retry_count = 2", {
  site <- .mk_osrm_site()

  # State counter for the closure across calls within the per-site retry loop.
  call_n <- 0L

  flaky_stub <- function(loc, breaks, res = NULL) {
    call_n <<- call_n + 1L
    if (call_n == 1L) {
      stop("OSRM server returned HTTP 503: Service Unavailable")
    }
    .mk_osrm_response(breaks)
  }

  testthat::local_mocked_bindings(
    .osrm_call_isochrone = flaky_stub,
    .package = "catchmentACS"
  )

  # Stub Sys.sleep so the test runs fast (no real 1s backoff).
  sleep_calls <- numeric(0L)
  testthat::local_mocked_bindings(
    Sys.sleep = function(time) {
      sleep_calls <<- c(sleep_calls, time)
      invisible(NULL)
    },
    .package = "base"
  )

  out <- .iso_via_osrm(sites = site, drive_times = 10L, profile = "car",
                       osrm_mode = "demo")

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 1L)
  expect_equal(out$retry_count, 2L)
  expect_true(is.na(out$failure_reason))
  expect_false(out$isochrone_empty)

  # Verify the 1-second backoff was honored before attempt 2.
  expect_equal(length(sleep_calls), 1L)
  expect_equal(sleep_calls[[1]], 1)

  # And that the stub was actually called twice.
  expect_equal(call_n, 2L)
})


# ============================================================================
# T-OSRM-03: HTTP 429 batch abort -> catchmentACS_error_operator (E-13)
# ============================================================================

test_that("T-OSRM-03 HTTP 429 aborts batch with operator-class error (E-13)", {
  site <- .mk_osrm_site()

  rate_limit_stub <- function(loc, breaks, res = NULL) {
    stop("OSRM client error (HTTP 429): Too Many Requests — quota exceeded")
  }

  testthat::local_mocked_bindings(
    .osrm_call_isochrone = rate_limit_stub,
    .package = "catchmentACS"
  )

  # 429 must short-circuit the retry loop and abort the batch with E-13.
  expect_error(
    .iso_via_osrm(sites = site, drive_times = 5L, profile = "car",
                  osrm_mode = "demo"),
    regexp = "Rate limit \\(HTTP 429\\)",
    class  = "catchmentACS_error_operator"
  )
})
