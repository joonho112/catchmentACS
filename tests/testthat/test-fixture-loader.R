# ============================================================================
# Replay fixture loader and shape tests.
# ============================================================================

test_that("replay fixture paths resolve for all bundled fixtures", {
  for (nm in replay_fixture_names()) {
    path <- replay_fixture_path(nm)
    expect_true(nzchar(path), info = nm)
    expect_true(file.exists(path), info = nm)
    expect_match(basename(path), paste0("^fixture_", nm, "\\.rds$"))
  }
})

test_that("load_replay_fixture returns expected object classes", {
  fx031 <- load_replay_fixture("031_3site_anchors")
  fx032 <- load_replay_fixture("032_3site_fresh")
  poison <- load_replay_fixture("cache_poison")
  annulus <- load_replay_fixture("annulus_input")

  expect_type(fx031, "list")
  expect_type(fx032, "list")
  expect_type(poison, "list")
  expect_s3_class(annulus, "sf")
  expect_s3_class(fx031$sites_sf, "sf")
  expect_s3_class(fx032$sites_sf, "sf")
})

test_that("031 replay fixture preserves expected v0.2 shapes", {
  fx <- load_replay_fixture("031_3site_anchors")

  expect_equal(nrow(fx$sites_sf), 3L)
  expect_equal(nrow(fx$iso_sf_v2), 3L)
  expect_equal(ncol(fx$iso_sf_v2), 15L)
  expect_equal(nrow(fx$acs_sf), 20090L)
  expect_equal(ncol(fx$acs_sf), 6L)
  expect_equal(nrow(fx$weighted_seam_v2), 42L)
  expect_equal(ncol(fx$weighted_seam_v2), 22L)
  expect_equal(nrow(fx$final_seam_v2), 57L)
  expect_equal(ncol(fx$final_seam_v2), 26L)
  expect_equal(nrow(fx$cmp1a_isochrone_area), 3L)
  expect_equal(nrow(fx$cmp1b_live_tract_jaccard), 3L)
  expect_equal(nrow(fx$cmp3_seam_jaccard), 3L)
  expect_true(all(c("site_id", "area_2025_km2", "pct_diff") %in%
                    names(fx$cmp1a_isochrone_area)))
  expect_true(all(c("site_id", "jaccard") %in%
                    names(fx$cmp3_seam_jaccard)))
  expect_true(all(c("poverty_rate", "snap_rate", "ssi_rate", "unemp_rate",
                    "labor_force_participation") %in% fx$final_seam_v2$variable))
})

test_that("032 replay fixture preserves fresh-user bug evidence", {
  fx <- load_replay_fixture("032_3site_fresh")

  expect_equal(nrow(fx$sites_sf), 3L)
  expect_setequal(fx$sites_sf$site_id, c("AL_BHM_01", "AL_MOB_01", "AL_HSV_01"))
  expect_equal(nrow(fx$acs_sf), 18655L)
  expect_equal(ncol(fx$acs_sf), 6L)
  expect_equal(nrow(fx$weighted_seam_v2), 39L)
  expect_equal(ncol(fx$weighted_seam_v2), 22L)
  expect_equal(nrow(fx$final_seam_v2), 54L)
  expect_equal(ncol(fx$final_seam_v2), 26L)

  ssi <- fx$final_seam_v2[fx$final_seam_v2$variable == "ssi_rate", ]
  expect_equal(nrow(ssi), 3L)
  expect_true(all(ssi$estimate == 1))
})

test_that("cache poison fixture carries both ACS and weighted poison shapes", {
  poison <- load_replay_fixture("cache_poison")
  fx032 <- load_replay_fixture("032_3site_fresh")

  expect_s3_class(poison$acs_sf, "sf")
  expect_equal(nrow(poison$acs_sf), 39L)
  expect_equal(ncol(poison$acs_sf), 6L)
  expect_equal(poison$acs_sf$GEOID[1:3], c("01001000100", "01002000200", "01003000300"))
  expect_equal(poison$acs_sf$variable[1:3], rep("B01003_001", 3L))
  expect_equal(poison$acs_sf$estimate[1:3], c(1000, 2000, 3000))
  expect_equal(poison$acs_sf$moe[1:3], c(10, 20, 30))
  expect_equal(nrow(poison$weighted_tbl), 15L)
  expect_equal(ncol(poison$weighted_tbl), 22L)
  expect_equal(poison$expected_stale_weighted_rows, 15L)
  expect_equal(poison$expected_real_acs_rows, nrow(fx032$acs_sf))
  expect_equal(table(poison$weighted_tbl$site_id)[["AL_BHM_01"]], 13L)
  expect_equal(table(poison$weighted_tbl$site_id)[["AL_HSV_01"]], 1L)
  expect_equal(table(poison$weighted_tbl$site_id)[["AL_MOB_01"]], 1L)
  expect_match(poison$estimate_digest, "^[0-9a-f]{64}$")
})

test_that("annulus fixture has raw ring evidence for UF-1 validator tests", {
  annulus <- load_replay_fixture("annulus_input")

  expect_s3_class(annulus, "sf")
  expect_equal(nrow(annulus), 3L)
  expect_true(all(c("isomin", "isomax", "ring_topology") %in% names(annulus)))
  expect_true(any(annulus$isomin > 0L))
  expect_true(all(annulus$isomax > annulus$isomin))
  expect_true(all(annulus$ring_topology == "annulus"))
  expect_true(all(sf::st_is_valid(annulus)))
  expect_equal(sf::st_crs(annulus)$epsg, 4326L)
})

test_that("replay fixtures carry provenance metadata", {
  for (nm in replay_fixture_names()) {
    fx <- load_replay_fixture(nm)
    prov <- attr(fx, "cacs_provenance")
    expect_type(prov, "list")
    expect_equal(prov$build_script, "data-raw/build-replay-fixtures.R")
    expect_false(is.null(prov$fixture_name))
    expect_false(isTRUE(prov$network_required))
    expect_true(length(prov$source_paths) >= 1L)
    expect_true(length(prov$input_hashes) >= 1L)
  }
})

test_that("skip guard succeeds when all replay fixtures are installed", {
  expect_invisible(skip_if_no_replay_fixtures())
})

test_that("skip guard can be tested with injected missing path function", {
  expect_condition(
    skip_if_no_replay_fixture("missing", path_fun = function(name) ""),
    class = "skip"
  )
})
