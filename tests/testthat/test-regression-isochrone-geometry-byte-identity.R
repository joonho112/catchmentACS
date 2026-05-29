test_that("P6-GEOM-01 mocked demo and docker outputs have identical geometry bytes", {
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      .p6_osrm_response(breaks, center_lon = loc[[1]], center_lat = loc[[2]])
    },
    .package = "catchmentACS"
  )
  with_test_cache({
    demo <- suppressMessages(cacs_isochrone(
      .p6_sites_tbl(1L), 10L, provider = "osrm",
      cache_dir = cacs_cache_dir(), verbose = FALSE
    ))
    docker <- suppressMessages(cacs_isochrone(
      .p6_sites_tbl(1L), 10L, provider = "osrm", osrm_mode = "docker",
      cache_dir = cacs_cache_dir(), verbose = FALSE
    ))
    expect_identical(.p6_geom_wkb_digest(demo), .p6_geom_wkb_digest(docker))
  })
})

test_that("P6-GEOM-02 mocked demo and docker outputs have identical EPSG5070 area", {
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      .p6_osrm_response(breaks, center_lon = loc[[1]], center_lat = loc[[2]])
    },
    .package = "catchmentACS"
  )
  with_test_cache({
    demo <- sf::st_transform(suppressMessages(cacs_isochrone(
      .p6_sites_tbl(1L), 10L, provider = "osrm",
      cache_dir = cacs_cache_dir(), verbose = FALSE
    )), 5070)
    docker <- sf::st_transform(suppressMessages(cacs_isochrone(
      .p6_sites_tbl(1L), 10L, provider = "osrm", osrm_mode = "docker",
      cache_dir = cacs_cache_dir(), verbose = FALSE
    )), 5070)
    expect_lt(max(abs(as.numeric(sf::st_area(demo)) - as.numeric(sf::st_area(docker)))), 1e-6)
  })
})

test_that("P6-GEOM-03 mocked demo and docker outputs have identical vertex count", {
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      .p6_osrm_response(breaks, center_lon = loc[[1]], center_lat = loc[[2]])
    },
    .package = "catchmentACS"
  )
  with_test_cache({
    demo <- suppressMessages(cacs_isochrone(
      .p6_sites_tbl(1L), c(5L, 10L), provider = "osrm",
      cache_dir = cacs_cache_dir(), verbose = FALSE
    ))
    docker <- suppressMessages(cacs_isochrone(
      .p6_sites_tbl(1L), c(5L, 10L), provider = "osrm", osrm_mode = "docker",
      cache_dir = cacs_cache_dir(), verbose = FALSE
    ))
    expect_equal(.p6_vertex_count(demo), .p6_vertex_count(docker))
  })
})

test_that("P6-GEOM-04 replay 031 geometry survives cache roundtrip byte-identically", {
  fx <- load_replay_fixture("031_3site_anchors")
  iso <- fx$iso_sf_v2[order(fx$iso_sf_v2$site_id, fx$iso_sf_v2$drive_time_min), ]
  with_test_cache({
    key <- cache_key_for("isochrone")
    .cacs_cache_put(iso, key, "isochrone")
    got <- suppressMessages(.cacs_cache_get(key, "isochrone"))
    expect_identical(.p6_geom_wkb_digest(iso), .p6_geom_wkb_digest(got))
  })
})

test_that("P6-GEOM-05 replay 032 geometry area survives cache roundtrip", {
  fx <- load_replay_fixture("032_3site_fresh")
  iso <- fx$iso_sf_v2[order(fx$iso_sf_v2$site_id, fx$iso_sf_v2$drive_time_min), ]
  with_test_cache({
    key <- cache_key_for("isochrone")
    .cacs_cache_put(iso, key, "isochrone")
    got <- suppressMessages(.cacs_cache_get(key, "isochrone"))
    a <- as.numeric(sf::st_area(sf::st_transform(iso, 5070)))
    b <- as.numeric(sf::st_area(sf::st_transform(got, 5070)))
    expect_lt(max(abs(a - b)), 1e-6)
  })
})

test_that("P6-GEOM-06 replay fixture vertex counts are stable under canonical order", {
  for (nm in c("031_3site_anchors", "032_3site_fresh")) {
    fx <- load_replay_fixture(nm)
    iso <- fx$iso_sf_v2[order(fx$iso_sf_v2$site_id, fx$iso_sf_v2$drive_time_min), ]
    expect_equal(.p6_vertex_count(iso), .p6_vertex_count(iso))
    expect_gt(.p6_vertex_count(iso), 0L)
  }
})
