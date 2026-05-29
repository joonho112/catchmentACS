# ============================================================================
# Unit tests for cacs_plot_site_isochrone() Stage 1 impl (Step 6.3).
#
# 5 testcases per the Step 6.3 plan:
#   T-PLOT-S1-01  site_id form returns a leaflet htmlwidget
#   T-PLOT-S1-02  (lat, lon) form works (no site_id filtering)
#   T-PLOT-S1-03  site_id not in iso_sf -> catchmentACS_error_schema abort
#   T-PLOT-S1-04  layer count matches expectation (1 tile + N polygons + 1 marker)
#   T-PLOT-S1-05  site marker present (addCircleMarkers call in widget)
#
# Uses bundled fixtures (legacy_2025_isochrones.rds + cacs_alabama_sites)
# so the whole suite runs offline; the leaflet Suggests is required and
# tests gate via testthat::skip_if_not_installed("leaflet").
# ============================================================================

testthat::skip_if_not_installed("leaflet")
testthat::skip_if_not_installed("sf")


.load_iso <- function() {
  path <- system.file("extdata", "legacy_2025_isochrones.rds",
                      package = "catchmentACS")
  testthat::skip_if(!nzchar(path) || !file.exists(path),
                    "legacy_2025_isochrones.rds fixture not installed")
  readRDS(path)
}


test_that("T-PLOT-S1-01 cacs_plot_site_isochrone() returns leaflet widget via site_id", {
  iso <- .load_iso()
  w <- cacs_plot_site_isochrone(
    site_id  = "AL_SITE_01",
    iso_sf   = iso,
    sites_df = cacs_alabama_sites
  )
  expect_s3_class(w, "leaflet")
  expect_s3_class(w, "htmlwidget")
})


test_that("T-PLOT-S1-02 cacs_plot_site_isochrone() works via direct (lat, lon)", {
  iso <- .load_iso()
  # When using the (lat, lon) input form the caller is expected to have
  # pre-subsetted iso_sf to the polygons they want shown; we mimic that
  # here so the layer-count expectations stay deterministic.
  one_site <- iso[iso$site_id == "AL_SITE_01", ]
  # Pluck coords from the bundled cacs_alabama_sites for AL_SITE_01.
  coord <- sf::st_coordinates(
    cacs_alabama_sites[cacs_alabama_sites$site_id == "AL_SITE_01", ]
  )
  w <- cacs_plot_site_isochrone(
    lat    = as.numeric(coord[1L, "Y"]),
    lon    = as.numeric(coord[1L, "X"]),
    iso_sf = one_site
  )
  expect_s3_class(w, "leaflet")
  expect_s3_class(w, "htmlwidget")
})


test_that("T-PLOT-S1-03 unknown site_id in iso_sf aborts with schema class", {
  iso <- .load_iso()
  # Strip every row whose site_id == AL_SITE_01 so the filter inside the
  # function returns 0 rows and triggers the "No isochrone found ..." abort.
  iso_missing <- iso[iso$site_id != "AL_SITE_01", ]
  expect_error(
    cacs_plot_site_isochrone(
      site_id  = "AL_SITE_01",
      iso_sf   = iso_missing,
      sites_df = cacs_alabama_sites
    ),
    class = "catchmentACS_error_schema"
  )
})


test_that("T-PLOT-S1-04 layer count = 1 tile + N drive-time polygons + 1 legend + 1 marker", {
  iso <- .load_iso()
  w <- cacs_plot_site_isochrone(
    site_id  = "AL_SITE_01",
    iso_sf   = iso,
    sites_df = cacs_alabama_sites
  )
  call_methods <- vapply(w$x$calls,
                         function(c) c$method %||% NA_character_,
                         character(1))
  expect_true("addProviderTiles" %in% call_methods)
  expect_true("addPolygons" %in% call_methods)
  expect_true("addCircleMarkers" %in% call_methods)
  expect_true("addLegend" %in% call_methods)

  # AL_SITE_01 in the bundled fixture has 3 drive-time rings (5, 10, 15 min)
  # so we expect 3 addPolygons calls (one per ring).
  n_rings <- sum(call_methods == "addPolygons")
  dt_levels <- length(unique(iso$drive_time_min[iso$site_id == "AL_SITE_01"]))
  expect_equal(n_rings, dt_levels)
})


test_that("T-PLOT-S1-05 site marker (addCircleMarkers) present in widget calls", {
  iso <- .load_iso()
  w <- cacs_plot_site_isochrone(
    site_id  = "AL_SITE_01",
    iso_sf   = iso,
    sites_df = cacs_alabama_sites
  )
  call_methods <- vapply(w$x$calls,
                         function(c) c$method %||% NA_character_,
                         character(1))
  marker_idx <- which(call_methods == "addCircleMarkers")
  expect_length(marker_idx, 1L)

  # Marker coordinates must agree with the resolved site point from
  # cacs_alabama_sites (avoids a regression where the marker drifts away
  # from the actual resolved coord).
  expected_coord <- sf::st_coordinates(
    cacs_alabama_sites[cacs_alabama_sites$site_id == "AL_SITE_01", ]
  )
  marker_args <- w$x$calls[[marker_idx]]$args
  # leaflet packs addCircleMarkers args positionally as
  # (lat, lng, layerId, radius, ...) -- so args[[1]] is lat and
  # args[[2]] is lng.
  expect_equal(unlist(marker_args[[1L]]),
               as.numeric(expected_coord[1L, "Y"]),
               tolerance = 1e-8)
  expect_equal(unlist(marker_args[[2L]]),
               as.numeric(expected_coord[1L, "X"]),
               tolerance = 1e-8)
})
