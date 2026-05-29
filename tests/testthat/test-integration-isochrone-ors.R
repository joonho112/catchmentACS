# ============================================================================
# Integration tests for R/isochrone-ors.R — Step 3.3.
# 3 cases per §38.5 + §19.8:
#   T-ORS-01: happy path single site / single drive_time -> 16-col sf
#   T-ORS-02: retry-on-5xx — first attempt errors, second succeeds (retry_count = 2)
#   T-ORS-03: HTTP 429 batch abort -> catchmentACS_error_operator (E-13)
#
# Mocking strategy. `openrouteservice::ors_isochrones()` is not invoked
# directly. Instead we mock the catchmentACS-internal seam
# `.ors_call_isochrones()` via testthat::local_mocked_bindings(.package =
# "catchmentACS"). This lets the tests run without requiring an internet
# connection / valid ORS API key / the openrouteservice package being
# installed, and keeps the suite hermetic per §16.5.
# ============================================================================


# ---- Local fixture helpers -------------------------------------------------

.mk_ors_site <- function(id = "S01", lon = -86.80902, lat = 33.52203) {
  tib <- tibble::tibble(
    site_id = id,
    lon = lon,
    lat = lat
  )
  sf::st_as_sf(tib, coords = c("lon", "lat"), crs = 4326)
}

# Build an sf mimicking `openrouteservice::ors_isochrones()` output:
# one row per (location, range) pair with `value` (seconds) as a column.
.mk_ors_response <- function(breaks_min, center_lon = -86.81, center_lat = 33.52) {
  polys <- lapply(seq_along(breaks_min), function(j) {
    b <- breaks_min[[j]]
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
      group_index = 0L,
      value       = as.integer(breaks_min) * 60L   # ORS reports seconds
    ),
    geometry = sfc
  )
}


# ============================================================================
# T-ORS-01: happy path, single site, single drive_time -> 16-col sf
# ============================================================================

test_that("T-ORS-01 happy path returns 16-col canonical sf (retry_count = 1)", {
  site <- .mk_ors_site()

  happy_stub <- function(locations, profile, range, api_key, passthrough = list()) {
    # `range` is in seconds; tests pass breaks_min so divide back.
    breaks_min <- as.integer(range / 60L)
    .mk_ors_response(breaks_min)
  }

  # Bypass the openrouteservice install-check so the test runs even when
  # the optional dependency isn't installed. The actual ORS HTTP call is
  # already mocked at the `.ors_call_isochrones()` seam below.
  testthat::local_mocked_bindings(
    check_installed = function(pkg, ...) invisible(TRUE),
    .package = "rlang"
  )
  testthat::local_mocked_bindings(
    .ors_call_isochrones = happy_stub,
    .package = "catchmentACS"
  )

  out <- .iso_via_ors(sites = site, drive_times = 5L, profile = "car",
                      api_key = "FAKE_KEY")

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
  expect_equal(out$drive_time_min, 5L)   # ORS value=300s normalized to 5min
  expect_equal(out$provider, "ors")
  expect_equal(out$profile, "car")
  expect_true(out$isochrone_empty == FALSE)
  expect_true(is.na(out$failure_reason))
  expect_equal(out$retry_count, 1L)
  expect_equal(out$ring_topology, "cumulative")

  # CRS preserved
  expect_equal(as.integer(sf::st_crs(out)$epsg), 4326L)
})


# ============================================================================
# T-ORS-02: retry-on-5xx — first attempt 503, second 200 (retry_count = 2)
# ============================================================================

test_that("T-ORS-02 retries on 5xx then succeeds with retry_count = 2", {
  site <- .mk_ors_site()

  call_n <- 0L
  flaky_stub <- function(locations, profile, range, api_key, passthrough = list()) {
    call_n <<- call_n + 1L
    if (call_n == 1L) {
      stop("ORS server returned HTTP 503: Service Unavailable")
    }
    breaks_min <- as.integer(range / 60L)
    .mk_ors_response(breaks_min)
  }

  # Bypass the openrouteservice install-check (see T-ORS-01 note).
  testthat::local_mocked_bindings(
    check_installed = function(pkg, ...) invisible(TRUE),
    .package = "rlang"
  )
  testthat::local_mocked_bindings(
    .ors_call_isochrones = flaky_stub,
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

  out <- .iso_via_ors(sites = site, drive_times = 10L, profile = "car",
                      api_key = "FAKE_KEY")

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 1L)
  expect_equal(out$retry_count, 2L)
  expect_true(is.na(out$failure_reason))
  expect_false(out$isochrone_empty)

  # 1s backoff after first failure
  expect_equal(length(sleep_calls), 1L)
  expect_equal(sleep_calls[[1]], 1)

  # Stub called twice (one fail, one success)
  expect_equal(call_n, 2L)
})


# ============================================================================
# T-ORS-03: HTTP 429 batch abort -> catchmentACS_error_operator (E-13)
# ============================================================================

test_that("T-ORS-03 HTTP 429 aborts batch with operator-class error (E-13)", {
  site <- .mk_ors_site()

  rate_limit_stub <- function(locations, profile, range, api_key, passthrough = list()) {
    stop("ORS client error (HTTP 429): Too Many Requests — quota exceeded")
  }

  # Bypass the openrouteservice install-check (see T-ORS-01 note).
  testthat::local_mocked_bindings(
    check_installed = function(pkg, ...) invisible(TRUE),
    .package = "rlang"
  )
  testthat::local_mocked_bindings(
    .ors_call_isochrones = rate_limit_stub,
    .package = "catchmentACS"
  )

  # 429 short-circuits the retry loop and aborts the batch with E-13.
  expect_error(
    .iso_via_ors(sites = site, drive_times = 5L, profile = "car",
                 api_key = "FAKE_KEY"),
    regexp = "Rate limit \\(HTTP 429\\)",
    class  = "catchmentACS_error_operator"
  )
})
