# ============================================================================
# Unit tests for cacs_plot_site_intersection() (Stage 2) and
# cacs_plot_site_weighted() (Stage 3) impls -- Step 6.4 of v0.2.0 plan.
#
# 6 testcases per the Step 6.4 plan:
#   T-PLOT-S2-01  Stage 2 returns a leaflet htmlwidget
#   T-PLOT-S2-02  Stage 2 layer count: tiles + 2x polygons + legend + marker
#   T-PLOT-S2-03  Stage 2 area_wt palette is monotonic on [0, 1]
#   T-PLOT-S3-01  Stage 3 returns a leaflet htmlwidget
#   T-PLOT-S3-02  Stage 3 invalid variable aborts with catchmentACS_error_schema
#   T-PLOT-S3-03  Stage 3 layer count: tiles + 2x polygons + legend + marker
#
# Uses bundled fixtures (legacy_2025_isochrones.rds + sample_alabama_subset.rds
# + cacs_alabama_sites) so the whole suite runs offline; the leaflet Suggests
# is required and tests gate via testthat::skip_if_not_installed("leaflet").
# ============================================================================

testthat::skip_if_not_installed("leaflet")
testthat::skip_if_not_installed("sf")


.load_iso_s23 <- function() {
  path <- system.file("extdata", "legacy_2025_isochrones.rds",
                      package = "catchmentACS")
  testthat::skip_if(!nzchar(path) || !file.exists(path),
                    "legacy_2025_isochrones.rds fixture not installed")
  readRDS(path)
}

.load_acs_s23 <- function() {
  path <- system.file("extdata", "sample_alabama_subset.rds",
                      package = "catchmentACS")
  testthat::skip_if(!nzchar(path) || !file.exists(path),
                    "sample_alabama_subset.rds fixture not installed")
  readRDS(path)
}


# ---------------------------------------------------------------------------
# Stage 2: cacs_plot_site_intersection()
# ---------------------------------------------------------------------------

test_that("T-PLOT-S2-01 cacs_plot_site_intersection() returns a leaflet widget", {
  iso <- .load_iso_s23()
  acs <- .load_acs_s23()
  w <- cacs_plot_site_intersection(
    site_id  = "AL_SITE_01",
    iso_sf   = iso,
    tract_sf = acs,
    sites_df = cacs_alabama_sites
  )
  expect_s3_class(w, "leaflet")
  expect_s3_class(w, "htmlwidget")
})


test_that("T-PLOT-S2-02 Stage 2 layer count: tiles + tract polys + iso outline + legend + marker", {
  iso <- .load_iso_s23()
  acs <- .load_acs_s23()
  w <- cacs_plot_site_intersection(
    site_id  = "AL_SITE_01",
    iso_sf   = iso,
    tract_sf = acs,
    sites_df = cacs_alabama_sites
  )
  call_methods <- vapply(w$x$calls,
                         function(c) c$method %||% NA_character_,
                         character(1))
  expect_true("addProviderTiles" %in% call_methods)
  expect_true("addCircleMarkers" %in% call_methods)
  expect_true("addLegend"        %in% call_methods)
  # Two addPolygons calls: (a) tract chloropleth + (b) isochrone outline.
  expect_equal(sum(call_methods == "addPolygons"), 2L)
})


test_that("T-PLOT-S2-03 area_wt palette closure is monotonic on [0, 1]", {
  # The Stage-2 chloropleth is keyed by .cacs_palette_areawt(), whose
  # domain is the FIXED [0, 1] interval (by design). The
  # mapping is monotonic (lower values -> darker viridis), so two
  # distinct probes at the endpoints must yield two distinct colors.
  pal <- .cacs_palette_areawt(c(0, 0.5, 1))
  cols <- pal(c(0, 0.25, 0.5, 0.75, 1))
  expect_type(cols, "character")
  expect_length(cols, 5L)
  expect_false(any(is.na(cols)))
  # Endpoint colors must differ (palette is non-degenerate).
  expect_false(identical(cols[1L], cols[5L]))
})


# ---------------------------------------------------------------------------
# Stage 3: cacs_plot_site_weighted()
# ---------------------------------------------------------------------------

test_that("T-PLOT-S3-01 cacs_plot_site_weighted() returns a leaflet widget", {
  iso <- .load_iso_s23()
  acs <- .load_acs_s23()
  w <- cacs_plot_site_weighted(
    site_id  = "AL_SITE_01",
    iso_sf   = iso,
    tract_sf = acs,
    acs_sf   = acs,
    variable = "B17001_002",
    sites_df = cacs_alabama_sites
  )
  expect_s3_class(w, "leaflet")
  expect_s3_class(w, "htmlwidget")
})


test_that("T-PLOT-S3-02 unknown variable aborts via schema family", {
  iso <- .load_iso_s23()
  acs <- .load_acs_s23()
  expect_error(
    cacs_plot_site_weighted(
      site_id  = "AL_SITE_01",
      iso_sf   = iso,
      tract_sf = acs,
      acs_sf   = acs,
      variable = "B_NOT_A_VARIABLE",
      sites_df = cacs_alabama_sites
    ),
    class = "catchmentACS_error_schema"
  )
})


test_that("T-PLOT-S3-03 Stage 3 layer count: tiles + tract polys + iso outline + legend + marker", {
  iso <- .load_iso_s23()
  acs <- .load_acs_s23()
  w <- cacs_plot_site_weighted(
    site_id  = "AL_SITE_01",
    iso_sf   = iso,
    tract_sf = acs,
    acs_sf   = acs,
    variable = "B17001_002",
    sites_df = cacs_alabama_sites
  )
  call_methods <- vapply(w$x$calls,
                         function(c) c$method %||% NA_character_,
                         character(1))
  expect_true("addProviderTiles" %in% call_methods)
  expect_true("addCircleMarkers" %in% call_methods)
  expect_true("addLegend"        %in% call_methods)
  expect_equal(sum(call_methods == "addPolygons"), 2L)
})
