# The map functions draw a background map that needs no API key by default.
# CARTO's raster tiles, the default before, carry a notice asking for a key
# when they are requested without one, so the default is the OpenStreetMap
# standard map. Bundled data; the map is built offline and no tile is
# requested.

test_that("PLOT-TILES-01 the five map functions default to the OpenStreetMap tiles", {
  fns <- list(
    cacs_plot_site_isochrone = cacs_plot_site_isochrone,
    cacs_plot_site_intersection = cacs_plot_site_intersection,
    cacs_plot_site_weighted = cacs_plot_site_weighted,
    cacs_plot_site_rates = cacs_plot_site_rates,
    cacs_plot_site_pipeline = cacs_plot_site_pipeline
  )
  for (nm in names(fns)) {
    expect_identical(formals(fns[[nm]])$tiles, "OpenStreetMap", info = nm)
  }
  testthat::skip_if_not_installed("leaflet")
  expect_true("OpenStreetMap" %in% names(leaflet::providers))
})

test_that("PLOT-TILES-02 a map built with the defaults adds the OpenStreetMap tiles", {
  testthat::skip_if_not_installed("leaflet")
  path <- system.file("extdata", "legacy_2025_isochrones.rds", package = "catchmentACS")
  testthat::skip_if(!nzchar(path), "bundled data not installed")
  iso <- readRDS(path)
  w <- cacs_plot_site_isochrone(
    site_id  = "AL_SITE_01",
    iso_sf   = iso,
    sites_df = cacs_alabama_sites
  )
  tile_calls <- Filter(function(cl) identical(cl$method, "addProviderTiles"), w$x$calls)
  expect_length(tile_calls, 1L)
  expect_identical(tile_calls[[1L]]$args[[1L]], "OpenStreetMap")
})
