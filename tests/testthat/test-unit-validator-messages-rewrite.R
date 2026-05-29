# ============================================================================
# Unit tests for Step 2.3 schema/plot validator message rewrites.
# ============================================================================


.mk_v030_msg_iso <- function(...) {
  geom <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(-87, 33), c(-86, 33), c(-86, 34), c(-87, 34), c(-87, 33)
    ))),
    crs = 4326
  )
  base <- tibble::tibble(
    site_id                          = "S01",
    drive_time_min                   = 15L,
    provider                         = "osrm",
    profile                          = "car",
    osm_snapshot_date                = "unknown",
    routing_engine_version           = "osrm-pkg/0.0.0",
    polygon_simplification_tolerance = NA_real_,
    generated_at                     = as.POSIXct("2026-05-22 00:00:00", tz = "UTC"),
    isochrone_empty                  = FALSE,
    provider_requested               = "osrm",
    provider_downgrade               = FALSE,
    osm_snapshot_status              = "unknown_best_effort",
    failure_reason                   = NA_character_,
    retry_count                      = 1L,
    ring_topology                    = "cumulative"
  )
  out <- sf::st_sf(base, geometry = geom)
  mods <- list(...)
  for (nm in names(mods)) out[[nm]] <- mods[[nm]]
  out
}

.mk_v030_msg_sites <- function() {
  sf::st_sf(
    tibble::tibble(site_id = "S01", site_name = "Example Site"),
    geometry = sf::st_sfc(sf::st_point(c(-86.5, 33.5)), crs = 4326)
  )
}

.mk_v030_msg_tract <- function(...) {
  geom <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(-87, 33), c(-86, 33), c(-86, 34), c(-87, 34), c(-87, 33)
    ))),
    crs = 4326
  )
  base <- tibble::tibble(
    GEOID = "01001000001",
    variable = "B17001_002",
    estimate = 100,
    moe = 10
  )
  out <- sf::st_sf(base, geometry = geom)
  mods <- list(...)
  for (nm in names(mods)) out[[nm]] <- mods[[nm]]
  out
}

.msg_from_error <- function(expr) {
  conditionMessage(rlang::catch_cnd(expr))
}


test_that("T-VALIDATOR-MSG-01 schema template keeps schema condition class", {
  err <- rlang::catch_cnd(.cli_abort_schema_with_template(
    "Example schema failure.",
    list(.schema_issue(
      check = "example_check",
      col = "provider",
      actual = "bad",
      expected = "osrm",
      fix_hint = "Use provider = \"osrm\".",
      example = "iso_sf$provider <- \"osrm\""
    ))
  ))
  expect_s3_class(err, "catchmentACS_error_schema")
})

test_that("T-VALIDATOR-MSG-02 schema template renders expected actual fix example", {
  msg <- .msg_from_error(.cli_abort_schema_with_template(
    "Example schema failure.",
    list(.schema_issue(
      check = "example_check",
      col = "provider",
      actual = "bad",
      expected = "osrm",
      fix_hint = "Use provider = \"osrm\".",
      example = "iso_sf$provider <- \"osrm\""
    ))
  ))
  expect_match(msg, "expected osrm; got bad", fixed = TRUE)
  expect_match(msg, "Fix: Use provider", fixed = TRUE)
  expect_match(msg, "Example: iso_sf$provider <- \"osrm\"", fixed = TRUE)
})

test_that("T-VALIDATOR-MSG-03 schema template examples are parseable", {
  issue <- .schema_issue(
    check = "example_check",
    actual = "bad",
    expected = "good",
    fix_hint = "Repair it.",
    example = "iso_sf$provider <- \"osrm\""
  )
  expect_silent(parse(text = issue$example))
})

test_that("T-VALIDATOR-MSG-04 non-sf validator message includes st_as_sf example", {
  msg <- .msg_from_error(.validate_iso_schema(
    sf::st_drop_geometry(.mk_v030_msg_iso())
  ))
  expect_match(msg, "sf::st_as_sf", fixed = TRUE)
  expect_match(msg, "sf_column_name = \"geometry\"", fixed = TRUE)
})

test_that("T-VALIDATOR-MSG-05 non-sf validator points to cacs_validate_iso", {
  msg <- .msg_from_error(.validate_iso_schema(
    sf::st_drop_geometry(.mk_v030_msg_iso())
  ))
  expect_match(msg, "cacs_validate_iso(iso_sf)", fixed = TRUE)
})

test_that("T-VALIDATOR-MSG-06 missing ring_topology message includes add-column example", {
  iso <- .mk_v030_msg_iso()
  iso$ring_topology <- NULL
  msg <- .msg_from_error(.validate_iso_schema(iso))
  expect_match(msg, "iso_missing_column", fixed = TRUE)
  expect_match(msg, "iso_sf$ring_topology <- \"cumulative\"", fixed = TRUE)
})

test_that("T-VALIDATOR-MSG-07 missing canonical columns points to preflight helper", {
  iso <- .mk_v030_msg_iso()
  iso$ring_topology <- NULL
  msg <- .msg_from_error(.validate_iso_schema(iso))
  expect_match(msg, "Run cacs_validate_iso(iso_sf)", fixed = TRUE)
})

test_that("T-VALIDATOR-MSG-08 CRS message uses iso_sf assignment example", {
  msg <- .msg_from_error(.validate_iso_schema(
    sf::st_transform(.mk_v030_msg_iso(), 4269)
  ))
  expect_match(msg, "EPSG:4269", fixed = TRUE)
  expect_match(msg, "iso_sf <- sf::st_transform(iso_sf, 4326)", fixed = TRUE)
})

test_that("T-VALIDATOR-MSG-09 CRS example is parseable", {
  expect_silent(parse(text = "iso_sf <- sf::st_transform(iso_sf, 4326)"))
})

test_that("T-VALIDATOR-MSG-10 provider_downgrade message includes logical repair", {
  msg <- .msg_from_error(.validate_iso_schema(
    .mk_v030_msg_iso(provider_downgrade = "FALSE")
  ))
  expect_match(msg, "provider_downgrade", fixed = TRUE)
  expect_match(msg, "iso_sf$provider_downgrade <- FALSE", fixed = TRUE)
})

test_that("T-VALIDATOR-MSG-11 isochrone_empty message includes logical repair", {
  msg <- .msg_from_error(.validate_iso_schema(
    .mk_v030_msg_iso(isochrone_empty = "FALSE")
  ))
  expect_match(msg, "isochrone_empty", fixed = TRUE)
  expect_match(msg, "iso_sf$isochrone_empty <- FALSE", fixed = TRUE)
})

test_that("T-VALIDATOR-MSG-12 annulus validator preserves special class", {
  err <- rlang::catch_cnd(.validate_iso_schema(
    .mk_v030_msg_iso(ring_topology = "annulus")
  ))
  expect_s3_class(err, "catchmentACS_error_annulus_input")
  expect_s3_class(err, "cacs_error_annulus_input")
  expect_s3_class(err, "catchmentACS_error_schema")
})

test_that("T-VALIDATOR-MSG-13 annulus message includes conversion and preflight", {
  msg <- .msg_from_error(.validate_iso_schema(
    .mk_v030_msg_iso(ring_topology = "annulus")
  ))
  expect_match(msg, "cacs_rings_to_cumulative", fixed = TRUE)
  expect_match(msg, "cacs_validate_iso(iso_sf)", fixed = TRUE)
})

test_that("T-VALIDATOR-MSG-13B cumulative-labeled isomin message is specific", {
  topology_msg <- .msg_from_error(.validate_iso_schema(
    .mk_v030_msg_iso(ring_topology = "annulus")
  ))
  err <- rlang::catch_cnd(.validate_iso_schema(
    .mk_v030_msg_iso(isomin = 5L, isomax = 10L)
  ))
  expect_s3_class(err, "catchmentACS_error_annulus_input")
  msg <- conditionMessage(err)
  expect_false(identical(topology_msg, msg))
  expect_match(msg, "ring_topology = \"cumulative\"", fixed = TRUE)
  expect_match(msg, "isomin > 0", fixed = TRUE)
  expect_match(msg, "isomin == 0", fixed = TRUE)
  expect_match(msg, "cacs_rings_to_cumulative", fixed = TRUE)
})

test_that("T-VALIDATOR-MSG-14 malformed ring_topology message includes conversion example", {
  msg <- .msg_from_error(.validate_iso_schema(
    .mk_v030_msg_iso(ring_topology = "bad")
  ))
  expect_match(msg, "iso_ring_topology", fixed = TRUE)
  expect_match(msg, "cacs_rings_to_cumulative", fixed = TRUE)
})

test_that("T-VALIDATOR-MSG-15 Stage 2 missing tract GEOID suggests tract_sf <- acs_sf", {
  testthat::local_mocked_bindings(
    .assert_leaflet_available = function() invisible(TRUE),
    .package = "catchmentACS"
  )
  bad_tract <- .mk_v030_msg_tract()
  bad_tract$GEOID <- NULL
  msg <- .msg_from_error(cacs_plot_site_intersection(
    site_id = "S01",
    iso_sf = .mk_v030_msg_iso(),
    tract_sf = bad_tract,
    sites_df = .mk_v030_msg_sites()
  ))
  expect_match(msg, "plot_tract_geoid_missing", fixed = TRUE)
  expect_match(msg, "tract_sf <- acs_sf", fixed = TRUE)
})

test_that("T-VALIDATOR-MSG-16 Stage 3 missing tract GEOID suggests tract_sf <- acs_sf", {
  testthat::local_mocked_bindings(
    .assert_leaflet_available = function() invisible(TRUE),
    .package = "catchmentACS"
  )
  bad_tract <- .mk_v030_msg_tract()
  bad_tract$GEOID <- NULL
  msg <- .msg_from_error(cacs_plot_site_weighted(
    site_id = "S01",
    iso_sf = .mk_v030_msg_iso(),
    tract_sf = bad_tract,
    acs_sf = .mk_v030_msg_tract(),
    variable = "B17001_002",
    sites_df = .mk_v030_msg_sites()
  ))
  expect_match(msg, "plot_tract_geoid_missing", fixed = TRUE)
  expect_match(msg, "tract_sf <- acs_sf", fixed = TRUE)
})

test_that("T-VALIDATOR-MSG-17 Stage 3 missing acs GEOID suggests acs_sf repair", {
  testthat::local_mocked_bindings(
    .assert_leaflet_available = function() invisible(TRUE),
    .package = "catchmentACS"
  )
  bad_acs <- .mk_v030_msg_tract()
  bad_acs$GEOID <- NULL
  msg <- .msg_from_error(cacs_plot_site_weighted(
    site_id = "S01",
    iso_sf = .mk_v030_msg_iso(),
    tract_sf = .mk_v030_msg_tract(),
    acs_sf = bad_acs,
    variable = "B17001_002",
    sites_df = .mk_v030_msg_sites()
  ))
  expect_match(msg, "plot_acs_geoid_missing", fixed = TRUE)
  expect_match(msg, "acs_sf <- tract_sf", fixed = TRUE)
})

test_that("T-VALIDATOR-MSG-18 plot helper all-null input uses templated issue", {
  msg <- .msg_from_error(.cacs_resolve_site_input())
  expect_match(msg, "plot_site_input_missing", fixed = TRUE)
  expect_match(msg, "cacs_plot_site_isochrone", fixed = TRUE)
})

test_that("T-VALIDATOR-MSG-19 template suppresses excess issues predictably", {
  issues <- replicate(
    7,
    .schema_issue(check = "one", actual = "bad", expected = "good"),
    simplify = FALSE
  )
  msg <- .msg_from_error(.cli_abort_schema_with_template(
    "Example schema failure.",
    issues,
    max_issues = 5L
  ))
  expect_match(msg, "and 2 more schema issue", fixed = TRUE)
})

test_that("T-VALIDATOR-MSG-20 top repair examples parse", {
  examples <- c(
    "iso_sf <- sf::st_as_sf(iso_sf, sf_column_name = \"geometry\", crs = 4326)",
    "iso_sf <- cacs_rings_to_cumulative(iso_sf)",
    "iso_sf <- sf::st_transform(iso_sf, 4326)",
    "iso_sf$provider_downgrade <- FALSE",
    "tract_sf <- acs_sf"
  )
  for (example in examples) expect_silent(parse(text = example))
})
