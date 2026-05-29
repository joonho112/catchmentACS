# ============================================================================
# Unit tests for R/isochrone-dispatch.R — Step 3.1 argument validation surface.
# 9 cases T19-01..T19-08 + T19-11.
# ============================================================================


.mk_iso_sites_local <- function(n = 1L, site_ids = NULL, id_col = "site_id") {
  ids <- if (is.null(site_ids)) paste0("S", sprintf("%02d", seq_len(n))) else site_ids
  out <- tibble::tibble(
    !!id_col := ids,
    lon      = rep(-86.80902, length.out = length(ids)),
    lat      = rep( 33.52203, length.out = length(ids))
  )
  out
}

.mk_iso_stub_sf <- function(site_id = "S01", drive_time_min = 5L,
                            provider = "osrm") {
  geom <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(-86.81, 33.52), c(-86.80, 33.52),
    c(-86.80, 33.53), c(-86.81, 33.53),
    c(-86.81, 33.52)
  ))), crs = 4326)
  # 16-col canonical schema per §19.5 — matches what Step 3.2/3.3 backends
  # produce, so `.normalize_iso_schema()` (Step 3.4) accepts the stub.
  sf::st_sf(
    tibble::tibble(
      site_id                          = site_id,
      drive_time_min                   = as.integer(drive_time_min),
      provider                         = provider,
      profile                          = "car",
      osm_snapshot_date                = NA_character_,
      routing_engine_version           = "stub/0.0.0",
      polygon_simplification_tolerance = NA_real_,
      generated_at                     = Sys.time(),
      isochrone_empty                  = FALSE,
      provider_requested               = provider,
      provider_downgrade               = FALSE,
      osm_snapshot_status              = NA_character_,
      failure_reason                   = NA_character_,
      retry_count                      = 1L,
      ring_topology                    = "cumulative"
    ),
    geometry = geom
  )
}

# Helper: stub all 4 backends so dispatch returns synthetic result
.with_iso_backend_stubs <- function(code) {
  stub <- function(sites, drive_times, profile, ...) .mk_iso_stub_sf()
  testthat::local_mocked_bindings(
    .iso_via_osrm   = stub,
    .iso_via_ors    = stub,
    .iso_via_mapbox = stub,
    .iso_via_r5r    = stub,
    .package = "catchmentACS",
    .env = parent.frame()
  )
  force(code)
}


test_that("T19-01 missing site_id aborts with schema family", {
  sites <- tibble::tibble(lon = -86.81, lat = 33.52)
  expect_error(
    cacs_isochrone(sites = sites, provider = "osrm"),
    class = "catchmentACS_error_schema"
  )
})

test_that("T19-02 duplicate site_id aborts with schema family", {
  sites <- .mk_iso_sites_local(site_ids = c("A", "A", "B"))
  expect_error(
    cacs_isochrone(sites = sites, provider = "osrm"),
    class = "catchmentACS_error_schema"
  )
})

test_that("T19-03 invalid drive_times aborts with schema family", {
  sites <- .mk_iso_sites_local()
  expect_error(
    cacs_isochrone(sites = sites, drive_times = c(-1, 5, 999), provider = "osrm"),
    class = "catchmentACS_error_schema"
  )
})

test_that("T19-04 unsanctioned provider aborts with schema family", {
  sites <- .mk_iso_sites_local()
  expect_error(
    cacs_isochrone(sites = sites, provider = "valhalla"),
    class = "catchmentACS_error_schema"
  )
})

test_that("T19-05 non-character profile aborts with schema family", {
  sites <- .mk_iso_sites_local()
  expect_error(
    cacs_isochrone(sites = sites, provider = "osrm", profile = 42),
    class = "catchmentACS_error_schema"
  )
})

test_that("T19-06 invalid osrm_mode aborts with schema family", {
  sites <- .mk_iso_sites_local()
  expect_error(
    cacs_isochrone(sites = sites, provider = "osrm", osrm_mode = "production"),
    class = "catchmentACS_error_schema"
  )
})

test_that("T19-07 NULL sites aborts with schema family", {
  expect_error(
    cacs_isochrone(sites = NULL, provider = "osrm"),
    class = "catchmentACS_error_schema"
  )
})

test_that("T19-08 legacy point_id rename emits W-01 warning + proceeds", {
  sites <- .mk_iso_sites_local(id_col = "point_id")
  .with_iso_backend_stubs({
    expect_warning(
      out <- cacs_isochrone(sites = sites, provider = "osrm", drive_times = 5),
      class = "catchmentACS_warning_provenance"
    )
    expect_s3_class(out, "sf")
    expect_true("site_id" %in% names(out))
  })
})

test_that("T19-08b base data.frame sites normalize to sf and proceed", {
  sites <- as.data.frame(.mk_iso_sites_local())
  seen_sites <- NULL
  td <- tempfile("cacs_iso_df_sites_")
  on.exit(if (dir.exists(td)) unlink(td, recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    .iso_via_osrm = function(sites, drive_times, profile, ...) {
      seen_sites <<- sites
      .mk_iso_stub_sf(site_id = as.character(sites$site_id[[1L]]),
                      drive_time_min = as.integer(drive_times[[1L]]))
    },
    .package = "catchmentACS"
  )

  out <- suppressMessages(suppressWarnings(
    cacs_isochrone(sites = sites, provider = "osrm",
                   drive_times = 5, cache_dir = td, res = 30,
                   verbose = FALSE)
  ))

  expect_s3_class(seen_sites, "sf")
  expect_equal(sf::st_crs(seen_sites)$epsg, 4326)
  expect_equal(as.character(seen_sites$site_id), sites$site_id)
  expect_s3_class(out, "sf")
  expect_equal(out$site_id, sites$site_id)
})

test_that("T19-11 unsanctioned ... passthrough emits W-01b warning", {
  sites <- .mk_iso_sites_local()
  .with_iso_backend_stubs({
    expect_warning(
      out <- cacs_isochrone(sites = sites, provider = "osrm",
                            drive_times = 5, bogus_arg = 1),
      class = "catchmentACS_warning_provenance"
    )
    expect_s3_class(out, "sf")
  })
})

test_that("T19-11b sanctioned OSRM passthrough reaches backend without warning", {
  sites <- .mk_iso_sites_local()
  seen <- NULL
  stub <- function(sites, drive_times, profile, osrm_mode = "demo", ...) {
    seen <<- list(...)
    .mk_iso_stub_sf()
  }
  td <- tempfile("cacs_iso_passthrough_")
  on.exit(if (dir.exists(td)) unlink(td, recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    .iso_via_osrm = stub,
    .package = "catchmentACS"
  )

  expect_warning(
    out <- cacs_isochrone(sites = sites, provider = "osrm",
                          drive_times = 5, cache_dir = td, res = 12),
    regexp = NA
  )
  expect_s3_class(out, "sf")
  expect_equal(seen$res, 12)
})


test_that("T19-12 .normalize_iso_schema fills and preserves snapshot provenance", {
  iso <- .mk_iso_stub_sf()
  out <- .normalize_iso_schema(
    iso,
    snap = list(date = "2025-04-01", status = "explicit")
  )

  expect_equal(names(out), .ISO_CANONICAL_COLS)
  expect_equal(out$osm_snapshot_date, "2025-04-01")
  expect_equal(out$osm_snapshot_status, "explicit")

  iso_backend <- .mk_iso_stub_sf()
  iso_backend$osm_snapshot_date <- "backend-date"
  iso_backend$osm_snapshot_status <- "backend-status"
  out_backend <- .normalize_iso_schema(
    iso_backend,
    snap = list(date = "snap-date", status = "snap-status")
  )

  expect_equal(out_backend$osm_snapshot_date, "backend-date")
  expect_equal(out_backend$osm_snapshot_status, "backend-status")
})


test_that("T19-13 .normalize_iso_schema fails loud on malformed inputs", {
  expect_error(
    .normalize_iso_schema(tibble::tibble(site_id = "S01"),
                          snap = list(date = "x", status = "y")),
    class = "catchmentACS_error_schema"
  )

  expect_error(
    .normalize_iso_schema(.mk_iso_stub_sf(), snap = list(date = "x")),
    class = "catchmentACS_error_schema"
  )

  iso_missing <- .mk_iso_stub_sf()
  iso_missing$routing_engine_version <- NULL
  expect_error(
    .normalize_iso_schema(
      iso_missing,
      snap = list(date = "2025-04-01", status = "explicit")
    ),
    class = "catchmentACS_error_schema"
  )
})
