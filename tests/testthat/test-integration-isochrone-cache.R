# ============================================================================
# Integration test for R/isochrone-dispatch.R cache integration — Step 3.4.
# T19-12 cache hit per §19.8 + §38.6 verification 5:
#   - First call (with stub backend) returns the canonical 16-col sf and
#     invokes the backend exactly once.
#   - Second identical call returns an `identical()` sf without invoking the
#     backend (call counter stays at 1).
#
# Also covers the `.normalize_iso_schema()` snapshot-merge contract:
# `osm_snapshot_date` and `osm_snapshot_status` in the returned sf reflect
# `.resolve_osm_snapshot()` (not the backend's NA), per §19.5 row 6 + row 13.
# ============================================================================


# ---- Isolated cache dir per test via withr::local_options ------------------

.with_iso_cache_dir <- function(code) {
  td <- tempfile("cacs_iso_test_")
  withr::with_options(
    list(catchmentACS.cache_dir = td, CACS_NO_CONFIRM = "1"),
    {
      Sys.setenv(CACS_NO_CONFIRM = "1")
      on.exit({
        if (dir.exists(td)) unlink(td, recursive = TRUE)
        memoise::forget(.cacs_cache_dir_memo)
      }, add = TRUE)
      memoise::forget(.cacs_cache_dir_memo)
      force(code)
    }
  )
}


.mk_cache_site <- function(id = "S01", lon = -86.80902, lat = 33.52203) {
  tib <- tibble::tibble(
    site_id = id,
    lon = lon,
    lat = lat
  )
  sf::st_as_sf(tib, coords = c("lon", "lat"), crs = 4326)
}


# Build a minimal sf with one polygon per break, mimicking osrm::osrmIsochrone()
# return contract (one row per (isomin, isomax) band).
.mk_cache_osrm_response <- function(breaks, center_lon = -86.81, center_lat = 33.52) {
  polys <- lapply(seq_along(breaks), function(j) {
    b <- breaks[[j]]
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
# T19-12: cache hit returns identical sf without re-invoking backend
# ============================================================================

test_that("T19-12 cache hit returns identical sf without invoking backend", {
  .with_iso_cache_dir({
    site <- .mk_cache_site()

    call_n <- 0L
    counting_stub <- function(loc, breaks, res = NULL) {
      call_n <<- call_n + 1L
      .mk_cache_osrm_response(breaks)
    }

    testthat::local_mocked_bindings(
      .osrm_call_isochrone = counting_stub,
      .package = "catchmentACS"
    )

    # --- First call: cache miss, backend invoked once -----------------------
    out1 <- suppressMessages(
      cacs_isochrone(
        sites       = site,
        drive_times = 5L,
        provider    = "osrm",
        osrm_mode   = "demo"
      )
    )

    expect_s3_class(out1, "sf")
    expect_equal(nrow(out1), 1L)
    expect_equal(ncol(out1), 16L)
    expect_equal(out1$ring_topology, "cumulative")
    expect_equal(call_n, 1L)

    # `.normalize_iso_schema()` must merge `snap$date` / `snap$status`
    # into the snapshot provenance columns (backend leaves them NA).
    expect_false(is.na(out1$osm_snapshot_date))
    expect_equal(out1$osm_snapshot_date, "unknown")
    expect_equal(out1$osm_snapshot_status, "unknown_best_effort")

    # --- Second identical call: cache hit, backend NOT invoked --------------
    out2 <- suppressMessages(
      cacs_isochrone(
        sites       = site,
        drive_times = 5L,
        provider    = "osrm",
        osrm_mode   = "demo"
      )
    )

    expect_s3_class(out2, "sf")
    expect_equal(call_n, 1L)             # still 1 — backend not re-called
    expect_identical(out1, out2)          # round-trip identity
  })
})


# ============================================================================
# T19-12b: cache hit emits the cache-class message
# ============================================================================

test_that("T19-12b cache hit emits catchmentACS_message_cache", {
  .with_iso_cache_dir({
    site <- .mk_cache_site()
    happy_stub <- function(loc, breaks, res = NULL) {
      .mk_cache_osrm_response(breaks)
    }
    testthat::local_mocked_bindings(
      .osrm_call_isochrone = happy_stub,
      .package = "catchmentACS"
    )

    # Prime cache.
    suppressMessages(
      cacs_isochrone(sites = site, drive_times = 5L,
                     provider = "osrm", osrm_mode = "demo")
    )

    expect_message(
      cacs_isochrone(sites = site, drive_times = 5L,
                     provider = "osrm", osrm_mode = "demo"),
      class = "catchmentACS_message_cache"
    )
  })
})


# ============================================================================
# T19-12c: distinct arguments produce a cache miss (different key)
# ============================================================================

test_that("T19-12c distinct drive_times misses the cache", {
  .with_iso_cache_dir({
    site <- .mk_cache_site()

    call_n <- 0L
    counting_stub <- function(loc, breaks, res = NULL) {
      call_n <<- call_n + 1L
      .mk_cache_osrm_response(breaks)
    }
    testthat::local_mocked_bindings(
      .osrm_call_isochrone = counting_stub,
      .package = "catchmentACS"
    )

    suppressMessages(
      cacs_isochrone(sites = site, drive_times = 5L,
                     provider = "osrm", osrm_mode = "demo")
    )
    expect_equal(call_n, 1L)

    # Different drive_times -> different cache key -> backend invoked again.
    suppressMessages(
      cacs_isochrone(sites = site, drive_times = 10L,
                     provider = "osrm", osrm_mode = "demo")
    )
    expect_equal(call_n, 2L)
  })
})


test_that("T19-12d explicit cache_dir writes and reads from requested root", {
  td <- tempfile("cacs_iso_explicit_cache_")
  on.exit(if (dir.exists(td)) unlink(td, recursive = TRUE), add = TRUE)

  site <- .mk_cache_site()
  call_n <- 0L
  counting_stub <- function(loc, breaks, res = NULL) {
    call_n <<- call_n + 1L
    .mk_cache_osrm_response(breaks)
  }
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = counting_stub,
    .package = "catchmentACS"
  )

  out1 <- suppressMessages(
    cacs_isochrone(sites = site, drive_times = 5L,
                   provider = "osrm", osrm_mode = "demo",
                   cache_dir = td)
  )
  expect_s3_class(out1, "sf")
  expect_equal(call_n, 1L)
  expect_length(list.files(file.path(td, "isochrone"), pattern = "\\.rds$"), 1L)

  out2 <- suppressMessages(
    cacs_isochrone(sites = site, drive_times = 5L,
                   provider = "osrm", osrm_mode = "demo",
                   cache_dir = td)
  )
  expect_equal(call_n, 1L)
  expect_identical(out1, out2)
})


test_that("T19-12e sanctioned passthrough participates in cache key", {
  td <- tempfile("cacs_iso_passthrough_cache_")
  on.exit(if (dir.exists(td)) unlink(td, recursive = TRUE), add = TRUE)

  site <- .mk_cache_site()
  call_n <- 0L
  counting_stub <- function(loc, breaks, res = NULL) {
    call_n <<- call_n + 1L
    .mk_cache_osrm_response(breaks)
  }
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = counting_stub,
    .package = "catchmentACS"
  )

  suppressMessages(
    cacs_isochrone(sites = site, drive_times = 5L,
                   provider = "osrm", osrm_mode = "demo",
                   cache_dir = td, res = 12)
  )
  expect_equal(call_n, 1L)

  suppressMessages(
    cacs_isochrone(sites = site, drive_times = 5L,
                   provider = "osrm", osrm_mode = "demo",
                   cache_dir = td, res = 24)
  )
  expect_equal(call_n, 2L)
})
