# ============================================================================
# Unit tests for R/plot-helpers.R shared helpers (Step 6.1 [CORE]).
#
# 13 testcases per the plot-helper test plan §"Test plan":
#   T-PLOT-H-01..04  .cacs_resolve_site_input() 3 input paths + all-NULL abort
#   T-PLOT-H-05      ambiguous-name abort
#   T-PLOT-H-06      precedence tie (site_id wins over lat/lon AND site_name)
#   T-PLOT-H-07      bad site_id (not in sites_df) abort
#   T-PLOT-H-08..09  invalid coords abort (NA, out-of-range)
#   T-PLOT-H-10..12  .cacs_default_view() shape, lat-shrink, zero-padding abort
#   T-PLOT-H-13      .assert_leaflet_available() both branches (real + mocked)
#
# All input-validation errors route through the existing schema family
# (catchmentACS_error_schema) by design.
# ============================================================================


# Build a tiny 3-row sites sf fixture with one deliberately ambiguous
# site_name to drive T-PLOT-H-05 without depending on
# `cacs_alabama_sites` (which is single-name-per-row by construction).
.mk_sites_local <- function() {
  geom <- sf::st_sfc(
    sf::st_point(c(-86.8, 33.5)),
    sf::st_point(c(-86.5, 32.4)),
    sf::st_point(c(-86.5, 32.4)),
    crs = 4326
  )
  sf::st_sf(
    site_id   = c("S01", "S02", "S03"),
    site_name = c("Alpha", "Beta", "Beta"),
    geometry  = geom
  )
}


test_that("T-PLOT-H-01 .cacs_resolve_site_input resolves via site_id", {
  sites <- .mk_sites_local()
  out <- .cacs_resolve_site_input(site_id = "S01", sites_df = sites)
  expect_type(out, "list")
  expect_named(out, c("site_id", "lat", "lon", "name"), ignore.order = TRUE)
  expect_identical(out$site_id, "S01")
  expect_equal(out$lon, -86.8, tolerance = 1e-9)
  expect_equal(out$lat,  33.5, tolerance = 1e-9)
  expect_identical(out$name, "Alpha")
})


test_that("T-PLOT-H-02 .cacs_resolve_site_input resolves via (lat, lon)", {
  sites <- .mk_sites_local()
  out <- .cacs_resolve_site_input(lat = 32.5, lon = -87.0, sites_df = sites)
  expect_equal(out$lat, 32.5)
  expect_equal(out$lon, -87.0)
  expect_true(is.na(out$site_id))
  expect_true(is.na(out$name))
})


test_that("T-PLOT-H-03 .cacs_resolve_site_input resolves via site_name", {
  sites <- .mk_sites_local()
  out <- .cacs_resolve_site_input(site_name = "Alpha", sites_df = sites)
  expect_identical(out$site_id, "S01")
  expect_identical(out$name,    "Alpha")
  expect_equal(out$lon, -86.8, tolerance = 1e-9)
  expect_equal(out$lat,  33.5, tolerance = 1e-9)
})


test_that("T-PLOT-H-04 all-NULL input aborts via schema family", {
  expect_error(
    .cacs_resolve_site_input(),
    class = "catchmentACS_error_schema"
  )
  err <- tryCatch(.cacs_resolve_site_input(), error = function(e) e)
  expect_match(conditionMessage(err), "No site input provided")
})


test_that("T-PLOT-H-05 ambiguous site_name aborts via schema family", {
  sites <- .mk_sites_local()
  expect_error(
    .cacs_resolve_site_input(site_name = "Beta", sites_df = sites),
    class = "catchmentACS_error_schema"
  )
  err <- tryCatch(
    .cacs_resolve_site_input(site_name = "Beta", sites_df = sites),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "matched 2 rows")
})


test_that("T-PLOT-H-06 precedence: site_id wins over (lat, lon) and site_name", {
  sites <- .mk_sites_local()
  # Pass conflicting (lat, lon) AND site_name pointing at different rows;
  # site_id must win and the resolver must return the S01-row coords.
  out <- .cacs_resolve_site_input(
    site_id   = "S01",
    lat       = 0,
    lon       = 0,
    site_name = "Beta",
    sites_df  = sites
  )
  expect_identical(out$site_id, "S01")
  expect_identical(out$name,    "Alpha")
  expect_equal(out$lon, -86.8, tolerance = 1e-9)
  expect_equal(out$lat,  33.5, tolerance = 1e-9)
})


test_that("T-PLOT-H-07 site_id not in sites_df aborts via schema family", {
  sites <- .mk_sites_local()
  expect_error(
    .cacs_resolve_site_input(site_id = "NOPE", sites_df = sites),
    class = "catchmentACS_error_schema"
  )
  err <- tryCatch(
    .cacs_resolve_site_input(site_id = "NOPE", sites_df = sites),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "not found")
})


test_that("T-PLOT-H-08 NA coords abort via schema family", {
  expect_error(
    .cacs_resolve_site_input(lat = NA_real_, lon = -86),
    class = "catchmentACS_error_schema"
  )
  expect_error(
    .cacs_resolve_site_input(lat = 33,        lon = NA_real_),
    class = "catchmentACS_error_schema"
  )
})


test_that("T-PLOT-H-09 out-of-range coords abort via schema family", {
  expect_error(
    .cacs_resolve_site_input(lat = 95, lon = -86),
    class = "catchmentACS_error_schema"
  )
  expect_error(
    .cacs_resolve_site_input(lat = 33, lon = 200),
    class = "catchmentACS_error_schema"
  )
})


test_that("T-PLOT-H-10 .cacs_default_view returns 4-element bbox list", {
  out <- .cacs_default_view(lat = 33, lon = -86, padding_km = 5)
  expect_type(out, "list")
  expect_named(out, c("lng1", "lat1", "lng2", "lat2"), ignore.order = TRUE)
  # bbox centered on (-86, 33) so lng1 < -86 < lng2 and lat1 < 33 < lat2
  expect_lt(out$lng1, -86); expect_gt(out$lng2, -86)
  expect_lt(out$lat1,  33); expect_gt(out$lat2,  33)
  # symmetric around center (within fp tolerance)
  expect_equal((out$lng1 + out$lng2) / 2, -86, tolerance = 1e-9)
  expect_equal((out$lat1 + out$lat2) / 2,  33, tolerance = 1e-9)
})


test_that("T-PLOT-H-11 .cacs_default_view longitude span grows in degrees at high latitude", {
  low  <- .cacs_default_view(lat = 10, lon = 0, padding_km = 5)
  high <- .cacs_default_view(lat = 60, lon = 0, padding_km = 5)
  span_lon_low  <- low$lng2  - low$lng1
  span_lon_high <- high$lng2 - high$lng1
  span_lat_low  <- low$lat2  - low$lat1
  span_lat_high <- high$lat2 - high$lat1
  # 1deg-of-lon spans fewer km at higher |lat| (cos(lat) shrink). To cover
  # the same `padding_km` distance, the bbox must therefore span MORE
  # degrees of longitude at higher |lat|. So span_lon_high > span_lon_low.
  expect_gt(span_lon_high, span_lon_low)
  # Sanity: the ratio should equal 1/cos(60deg) / 1/cos(10deg) =
  # cos(10deg) / cos(60deg) within fp tolerance.
  expected_ratio <- cos(10 * pi / 180) / cos(60 * pi / 180)
  expect_equal(span_lon_high / span_lon_low, expected_ratio, tolerance = 1e-9)
  # latitude span MUST stay constant across latitudes (uses 111 km/deg flat).
  expect_equal(span_lat_low, span_lat_high, tolerance = 1e-12)
})


test_that("T-PLOT-H-12 .cacs_default_view aborts on non-positive padding_km", {
  expect_error(
    .cacs_default_view(lat = 33, lon = -86, padding_km = 0),
    class = "catchmentACS_error_schema"
  )
  expect_error(
    .cacs_default_view(lat = 33, lon = -86, padding_km = -1),
    class = "catchmentACS_error_schema"
  )
})


# ============================================================================
# Step 6.2 [CORE] additions (T-PLOT-H-14..23) - palette + popup helpers.
# Locked by design.
# All palette tests require leaflet (Suggests-gated); we skip when absent so
# the suite still passes on a leaflet-less CI host.
# ============================================================================


test_that("T-PLOT-H-14 .cacs_palette_areawt monotonic + valid hex on [0,1]", {
  skip_if_not_installed("leaflet")
  pal <- .cacs_palette_areawt(values = c(0, 0.25, 0.5, 0.75, 1))
  expect_type(pal, "closure")
  colors <- pal(c(0, 0.25, 0.5, 0.75, 1))
  # All 5 stops must be valid #RRGGBB hex (with optional alpha) and unique.
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", colors)))
  expect_equal(length(unique(colors)), 5L)
})


test_that("T-PLOT-H-15 .cacs_palette_areawt FIXED-domain (cross-site stability)", {
  skip_if_not_installed("leaflet")
  # Two different sites with different area_wt distributions must hash 0.4 to
  # the same color. The design locks the [0, 1] domain so a value at
  # one site is identical to the same value at any other site.
  pal_a <- .cacs_palette_areawt(values = c(0.10, 0.40, 0.95))
  pal_b <- .cacs_palette_areawt(values = c(0.00, 0.40, 0.60))
  expect_identical(pal_a(0.4), pal_b(0.4))
  expect_identical(pal_a(0.0), pal_b(0.0))
  expect_identical(pal_a(1.0), pal_b(1.0))
})


test_that("T-PLOT-H-16 .cacs_palette_chloropleth family branches (4 cases)", {
  skip_if_not_installed("leaflet")
  vals <- c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90)
  # Verify each family returns DIFFERENT colors for the same value (different
  # ColorBrewer palette names produce different hex output for the same bin).
  c_total  <- .cacs_palette_chloropleth(vals, "spatial_total")(0.5)
  c_rate   <- .cacs_palette_chloropleth(vals, "derived_rate")(0.5)
  c_awsp   <- .cacs_palette_chloropleth(vals, "area_weighted_scalar_proxy")(0.5)
  c_pwsp   <- .cacs_palette_chloropleth(vals, "population_weighted_scalar_proxy")(0.5)
  c_median <- .cacs_palette_chloropleth(vals, "median_proxy")(0.5)
  # spatial_total -> Greens, derived_rate -> RdYlBu, area/pop_weighted -> YlOrRd,
  # median_proxy -> PuOr. The 4 distinct families must yield distinct colors.
  expect_true(length(unique(c(c_total, c_rate, c_awsp, c_median))) == 4L)
  # area and population scalar_proxy share the YlOrRd family (synth table).
  expect_identical(c_awsp, c_pwsp)
})


test_that("T-PLOT-H-17 .cacs_palette_chloropleth metadata_only aborts with schema class", {
  skip_if_not_installed("leaflet")
  expect_error(
    .cacs_palette_chloropleth(values = c(1, 2, 3), variable_family = "metadata_only"),
    class = "catchmentACS_error_schema"
  )
  err <- tryCatch(
    .cacs_palette_chloropleth(values = c(1, 2, 3), variable_family = "metadata_only"),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "metadata_only")
})


test_that("T-PLOT-H-18 .cacs_palette_chloropleth NA-family falls back to YlGnBu", {
  skip_if_not_installed("leaflet")
  vals <- c(0.10, 0.20, 0.30, 0.40, 0.50)
  pal_na    <- .cacs_palette_chloropleth(vals, NA_character_)
  pal_unk   <- .cacs_palette_chloropleth(vals, "not_a_real_family")
  pal_total <- .cacs_palette_chloropleth(vals, "spatial_total")
  # NA-family and unrecognized-family must both route through the YlGnBu
  # fallback by design - same palette -> same color for
  # the same input value.
  expect_identical(pal_na(0.3),  pal_unk(0.3))
  expect_identical(pal_na(0.5),  pal_unk(0.5))
  # The fallback must be DIFFERENT from the spatial_total Greens palette
  # (otherwise we'd silently lose the family-mapping contract).
  expect_false(identical(pal_na(0.3), pal_total(0.3)))
})


test_that("T-PLOT-H-19 .cacs_palette_chloropleth degrades to n<5 for tiny vectors", {
  skip_if_not_installed("leaflet")
  # 3 unique finite values: linear-degrade to n = 3 bins. Color closure must
  # still be callable and return a valid hex (or a hex with alpha).
  pal_small <- .cacs_palette_chloropleth(values = c(1, 2, 3), "spatial_total")
  colors <- pal_small(c(1, 2, 3))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", colors)))
  # And 1 unique value: degenerate single-bin palette still callable.
  pal_one <- .cacs_palette_chloropleth(values = c(7, 7, 7), "spatial_total")
  expect_true(grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", pal_one(7)))
})


test_that("T-PLOT-H-20 .cacs_palette_chloropleth NA inputs render as na_color", {
  skip_if_not_installed("leaflet")
  vals <- c(0.10, 0.20, 0.30, 0.40, 0.50)
  pal  <- .cacs_palette_chloropleth(vals, "spatial_total", na_color = "#CCCCCC")
  # leaflet color closures normalize hex to lowercase by default; compare
  # case-insensitively so the test is robust to upstream cosmetic changes.
  expect_identical(tolower(pal(NA_real_)), tolower("#CCCCCC"))
  # Custom na_color flows through verbatim.
  pal2 <- .cacs_palette_chloropleth(vals, "spatial_total", na_color = "#000000")
  expect_identical(tolower(pal2(NA_real_)), tolower("#000000"))
})


test_that("T-PLOT-H-21 .cacs_palette_areawt aborts when values fall outside [0, 1]", {
  skip_if_not_installed("leaflet")
  expect_error(
    .cacs_palette_areawt(values = c(0.5, 1.5)),
    class = "catchmentACS_error_schema"
  )
  expect_error(
    .cacs_palette_areawt(values = c(-0.1, 0.5)),
    class = "catchmentACS_error_schema"
  )
})


test_that("T-PLOT-H-22 popup builders escape XSS payloads (GEOID + site_id + variable)", {
  skip_if_not_installed("htmltools")
  payload <- "<script>alert(1)</script>"
  # 1) tract popup with malicious GEOID
  html_tract <- .cacs_popup_tract(
    tract_row = list(
      GEOID            = payload,
      area_wt          = 0.42,
      intersection_km2 = 1.5
    ),
    vars = character(0)
  )
  expect_false(grepl("<script>", html_tract, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;", html_tract, fixed = TRUE))

  # 2) site popup with malicious site_id AND site_name
  html_site <- .cacs_popup_site(list(
    site_id   = payload,
    site_name = "<img src=x onerror=alert(1)>",
    lat       = 33.5,
    lon       = -86.8
  ))
  expect_false(grepl("<script>",        html_site, fixed = TRUE))
  expect_false(grepl("<img src=x",      html_site, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;",   html_site, fixed = TRUE))
  expect_true(grepl("&lt;img src=x",    html_site, fixed = TRUE))

  # 3) rate popup with malicious variable name
  html_rate <- .cacs_popup_rate(list(
    variable              = payload,
    estimate              = 0.12,
    moe                   = 0.04,
    n_tracts_num          = 3L,
    n_tracts_den          = 7L,
    moe_fallback          = FALSE,
    moe_formula_effective = "weighted_sum"
  ))
  expect_false(grepl("<script>",      html_rate, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;", html_rate, fixed = TRUE))
})


test_that("T-PLOT-H-23 popup builders deterministically format numerics via formatC", {
  skip_if_not_installed("htmltools")
  # By design: area_wt = 0.123456 must render as the formatC()
  # 4-digit precision string, NOT R's default print width. Test guards against
  # any drift toward generic as.character() / format().
  html <- .cacs_popup_tract(
    tract_row = list(
      GEOID            = "01001020100",
      area_wt          = 0.123456,
      intersection_km2 = 1.234567
    ),
    vars = character(0)
  )
  # Expected formatC(0.123456, digits = 4, format = "f") -> "0.1235"
  expect_true(grepl("0.1235", html, fixed = TRUE))
  # And intersection_km2 = 1.234567 -> "1.2346" (4-digit precision)
  expect_true(grepl("1.2346", html, fixed = TRUE))
  # Sanity: GEOID escaped through htmlEscape (no markup leakage)
  expect_true(grepl("01001020100", html, fixed = TRUE))
})


test_that("T-PLOT-H-13 .assert_leaflet_available both branches", {
  # Real branch: only assert TRUE when leaflet is actually present so
  # the testcase passes under both `install.packages('leaflet')` and
  # leaflet-absent CI configs.
  if (requireNamespace("leaflet", quietly = TRUE)) {
    expect_true(.assert_leaflet_available())
  } else {
    expect_error(
      .assert_leaflet_available(),
      class = "catchmentACS_error_missing_suggest"
    )
  }
  # Mocked-unavailable branch via local requireNamespace stub. We mask
  # base::requireNamespace within the helper's lexical scope so the
  # missing-leaflet path fires deterministically regardless of the
  # ambient package state.
  local_mock <- function(package, ..., quietly = TRUE) {
    if (identical(package, "leaflet")) return(FALSE)
    base::requireNamespace(package, ..., quietly = quietly)
  }
  testthat::with_mocked_bindings(
    requireNamespace = local_mock,
    .package = "base",
    {
      expect_error(
        .assert_leaflet_available(),
        class = "catchmentACS_error_missing_suggest"
      )
    }
  )
})
