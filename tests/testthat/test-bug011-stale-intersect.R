bug011_pair <- function() {
  fx <- helper_load_synthetic_2tract()
  cx <- -86.80
  cy <- 33.50
  iso <- sf::st_sf(
    tibble::tibble(
      site_id = fx$site$site_id,
      drive_time_min = as.integer(fx$site$drive_time_min),
      provider = "osrm",
      profile = "car",
      osm_snapshot_date = NA_character_,
      routing_engine_version = "stub/0.0.0",
      polygon_simplification_tolerance = NA_real_,
      generated_at = as.POSIXct("2026-05-24 00:00:00", tz = "UTC"),
      isochrone_empty = FALSE,
      provider_requested = "osrm",
      provider_downgrade = FALSE,
      osm_snapshot_status = NA_character_,
      failure_reason = NA_character_,
      retry_count = 1L,
      ring_topology = "cumulative"
    ),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(cx, cy),
        c(cx + 0.008, cy),
        c(cx + 0.008, cy + 0.005),
        c(cx, cy + 0.005),
        c(cx, cy)
      ))),
      crs = 4326
    )
  )
  attr(iso, "cacs_isochrone_provenance") <- list(
    cache_key = "iso-bug011-same",
    res_param = 70L,
    ring_topology = "cumulative"
  )
  acs <- sf::st_sf(
    tibble::tibble(
      GEOID = fx$tracts$GEOID,
      NAME = fx$tracts$NAME,
      variable = fx$tracts$variable,
      estimate = fx$tracts$estimate,
      moe = fx$tracts$moe
    ),
    geometry = sf::st_sfc(
      sf::st_multipolygon(list(list(rbind(
        c(cx - 0.005, cy - 0.005),
        c(cx + 0.020, cy - 0.005),
        c(cx + 0.020, cy + 0.010),
        c(cx - 0.005, cy + 0.010),
        c(cx - 0.005, cy - 0.005)
      )))),
      sf::st_multipolygon(list(list(rbind(
        c(cx + 0.020, cy - 0.005),
        c(cx + 0.040, cy - 0.005),
        c(cx + 0.040, cy + 0.010),
        c(cx + 0.020, cy + 0.010),
        c(cx + 0.020, cy - 0.005)
      )))),
      crs = 4269
    )
  )
  attr(acs, "cacs_provenance") <- list(
    cache_key = "acs-bug011-same",
    state = "AL",
    year = 2023L,
    survey = "acs5",
    geography = "tract",
    variables = unique(acs$variable)
  )
  list(iso = iso, acs = acs)
}

test_that("BUG011-01 same inputs produce an intersect cache hit on second call", {
  pair <- bug011_pair()
  with_test_cache({
    out1 <- suppressMessages(cacs_intersect_weight(pair$iso, pair$acs, verbose = FALSE))
    out2 <- suppressMessages(cacs_intersect_weight(pair$iso, pair$acs, verbose = FALSE))
    state <- cacs_get_cache_state()
    expect_gte(state$hits[[1]][["intersect"]], 1L)
    expect_identical(out1, out2)
  })
})

test_that("BUG011-02 same provenance but changed ACS estimates does not reuse stale intersect", {
  pair1 <- bug011_pair()
  pair2 <- bug011_pair()
  pair2$acs$estimate <- pair2$acs$estimate + 1000
  attr(pair2$acs, "cacs_provenance") <- attr(pair1$acs, "cacs_provenance")
  with_test_cache({
    out1 <- suppressMessages(cacs_intersect_weight(pair1$iso, pair1$acs, verbose = FALSE))
    state_after_first <- cacs_get_cache_state()
    out2 <- suppressMessages(cacs_intersect_weight(pair2$iso, pair2$acs, verbose = FALSE))
    state_after_second <- cacs_get_cache_state()
    expect_false(identical(out1, out2))
    expect_gt(sum(out2$estimate, na.rm = TRUE), sum(out1$estimate, na.rm = TRUE))
    expect_equal(state_after_first$misses[[1]][["intersect"]], 1L)
    expect_equal(state_after_first$hits[[1]][["intersect"]], 0L)
    expect_equal(state_after_second$misses[[1]][["intersect"]], 2L)
    expect_equal(state_after_second$hits[[1]][["intersect"]], 0L)
    row <- cacs_cache_status()[cacs_cache_status()$namespace == "intersect", ]
    expect_equal(row$n_entries, 2L)
    expect_equal(row$n_fingerprints, 2L)
  })
})

test_that("BUG011-03 ACS content-aware key changes while upstream key stays fixed", {
  pair1 <- bug011_pair()
  pair2 <- bug011_pair()
  pair2$acs$estimate[[1L]] <- pair2$acs$estimate[[1L]] + 1
  attr(pair2$acs, "cacs_provenance") <- attr(pair1$acs, "cacs_provenance")
  key1 <- .cache_key_intersect(pair1$iso, pair1$acs, NULL, "area", 1e-6)
  key2 <- .cache_key_intersect(pair2$iso, pair2$acs, NULL, "area", 1e-6)
  expect_false(identical(key1, key2))
})

test_that("BUG011-04 ACS row order does not change intersect key", {
  pair1 <- bug011_pair()
  pair2 <- bug011_pair()
  pair2$acs <- pair2$acs[rev(seq_len(nrow(pair2$acs))), ]
  attr(pair2$acs, "cacs_provenance") <- attr(pair1$acs, "cacs_provenance")
  expect_identical(
    .cache_key_intersect(pair1$iso, pair1$acs, NULL, "area", 1e-6),
    .cache_key_intersect(pair2$iso, pair2$acs, NULL, "area", 1e-6)
  )
})

test_that("BUG011-05 reordered ACS rows reuse the same intersect cache entry", {
  pair1 <- bug011_pair()
  pair2 <- bug011_pair()
  pair2$acs <- pair2$acs[rev(seq_len(nrow(pair2$acs))), ]
  attr(pair2$acs, "cacs_provenance") <- attr(pair1$acs, "cacs_provenance")
  with_test_cache({
    out1 <- suppressMessages(cacs_intersect_weight(pair1$iso, pair1$acs, verbose = FALSE))
    out2 <- suppressMessages(cacs_intersect_weight(pair2$iso, pair2$acs, verbose = FALSE))
    expect_equal(cacs_get_cache_state()$hits[[1]][["intersect"]], 1L)
    expect_identical(out1, out2)
    row <- cacs_cache_status()[cacs_cache_status()$namespace == "intersect", ]
    expect_equal(row$n_entries, 1L)
  })
})

test_that("BUG011-06 isochrone res_param separates intersect cache keys", {
  pair1 <- bug011_pair()
  pair2 <- bug011_pair()
  attr(pair1$iso, "cacs_isochrone_provenance")$res_param <- 50L
  attr(pair2$iso, "cacs_isochrone_provenance")$res_param <- 70L
  expect_false(identical(
    .cache_key_intersect(pair1$iso, pair1$acs, NULL, "area", 1e-6),
    .cache_key_intersect(pair2$iso, pair2$acs, NULL, "area", 1e-6)
  ))
})

test_that("BUG011-07 isochrone ring_topology separates intersect cache keys", {
  pair1 <- bug011_pair()
  pair2 <- bug011_pair()
  pair2$iso$ring_topology <- "annulus"
  attr(pair2$iso, "cacs_isochrone_provenance")$ring_topology <- "annulus"
  expect_false(identical(
    .cache_key_intersect(pair1$iso, pair1$acs, NULL, "area", 1e-6),
    .cache_key_intersect(pair2$iso, pair2$acs, NULL, "area", 1e-6)
  ))
})

test_that("BUG011-08 res_param drift recomputes instead of using stale intersect", {
  pair1 <- bug011_pair()
  pair2 <- bug011_pair()
  attr(pair2$iso, "cacs_isochrone_provenance")$res_param <- 50L
  attr(pair2$iso, "cacs_res_param") <- 50L
  with_test_cache({
    suppressMessages(cacs_intersect_weight(pair1$iso, pair1$acs, verbose = FALSE))
    suppressMessages(cacs_intersect_weight(pair2$iso, pair2$acs, verbose = FALSE))
    state <- cacs_get_cache_state()
    expect_equal(state$hits[[1]][["intersect"]], 0L)
    expect_equal(state$misses[[1]][["intersect"]], 2L)
    row <- cacs_cache_status()[cacs_cache_status()$namespace == "intersect", ]
    expect_equal(row$n_entries, 2L)
  })
})

test_that("BUG011-09 legacy stale intersect cache is invalidated before reuse", {
  pair <- bug011_pair()
  key <- .cache_key_intersect(pair$iso, pair$acs, NULL, "area", 1e-6)
  with_test_cache({
    cache_write_legacy_rds("intersect", key, load_replay_fixture("cache_poison")$weighted_tbl)
    expect_message(
      got <- .cacs_cache_get(key, "intersect"),
      class = "catchmentACS_message_cache_legacy_invalidated"
    )
    expect_null(got)
  })
})

test_that("BUG011-10 mismatched intersect sidecar invalidates stale weighted table", {
  pair <- bug011_pair()
  key <- .cache_key_intersect(pair$iso, pair$acs, NULL, "area", 1e-6)
  with_test_cache({
    .cacs_cache_put(suppressMessages(cacs_intersect_weight(pair$iso, pair$acs, verbose = FALSE)),
                    key, "intersect")
    saveRDS(load_replay_fixture("cache_poison")$weighted_tbl,
            file.path(cacs_cache_dir(), "intersect", paste0(key, ".rds")))
    expect_message(
      got <- .cacs_cache_get(key, "intersect"),
      class = "catchmentACS_message_cache_fingerprint_mismatch"
    )
    expect_null(got)
  })
})

test_that("BUG011-11 full pipeline recomputes when legacy stale intersect exists", {
  pair <- bug011_pair()
  key <- .cache_key_intersect(pair$iso, pair$acs, NULL, "area", 1e-6)
  stale <- load_replay_fixture("cache_poison")$weighted_tbl
  with_test_cache({
    cache_write_legacy_rds("intersect", key, stale)
    out <- suppressMessages(cacs_intersect_weight(pair$iso, pair$acs, verbose = FALSE))
    expect_s3_class(out, "tbl_df")
    expect_false(identical(out, stale))
    expect_equal(cacs_get_cache_state()$misses[[1]][["intersect"]], 1L)
  })
})

test_that("BUG011-12 stale-intersect prevention works without force_refresh", {
  pair1 <- bug011_pair()
  pair2 <- bug011_pair()
  pair2$acs$estimate <- pair2$acs$estimate * 2
  attr(pair2$acs, "cacs_provenance") <- attr(pair1$acs, "cacs_provenance")
  with_test_cache({
    first <- suppressMessages(cacs_intersect_weight(pair1$iso, pair1$acs, verbose = FALSE))
    second <- suppressMessages(cacs_intersect_weight(pair2$iso, pair2$acs, verbose = FALSE))
    expect_false(identical(first, second))
    expect_equal(cacs_get_cache_state()$hits[[1]][["intersect"]], 0L)
    expect_equal(cacs_get_cache_state()$misses[[1]][["intersect"]], 2L)
  })
})
