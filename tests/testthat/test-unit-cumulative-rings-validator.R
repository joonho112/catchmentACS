# ============================================================================
# Unit tests for v0.3 UF-1 cumulative-only isochrone topology validation.
# ============================================================================


.mk_cumulative_iso_schema <- function(ring_topology = "cumulative", ...) {
  geom <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(-87, 33), c(-86, 33), c(-86, 34), c(-87, 34), c(-87, 33)
    ))),
    crs = 4326
  )
  out <- sf::st_sf(
    tibble::tibble(
      site_id                          = "S01",
      drive_time_min                   = 15L,
      provider                         = "osrm",
      profile                          = "car",
      osm_snapshot_date                = "unknown",
      routing_engine_version           = "stub/0.0.0",
      polygon_simplification_tolerance = NA_real_,
      generated_at                     = as.POSIXct("2026-05-24 00:00:00",
                                                     tz = "UTC"),
      isochrone_empty                  = FALSE,
      provider_requested               = "osrm",
      provider_downgrade               = FALSE,
      osm_snapshot_status              = "unknown_best_effort",
      failure_reason                   = NA_character_,
      retry_count                      = 1L,
      ring_topology                    = ring_topology
    ),
    geometry = geom
  )
  mods <- list(...)
  for (nm in names(mods)) out[[nm]] <- mods[[nm]]
  out
}


test_that("T-UF1-VAL-01 annulus replay fixture hard-rejects with v0.3 class chain", {
  fx <- load_replay_fixture("annulus_input")
  err <- rlang::catch_cnd(.validate_iso_schema(fx))

  expect_s3_class(err, "catchmentACS_error_annulus_input")
  expect_s3_class(err, "cacs_error_annulus_input")
  expect_s3_class(err, "cacs_error_schema")
  expect_s3_class(err, "catchmentACS_error_schema")
  expect_s3_class(err, "catchmentACS_error")
  expect_s3_class(err, "catchmentACS_condition")
  expect_match(conditionMessage(err), "annulus")
  expect_match(conditionMessage(err), "cumulative")
  expect_match(conditionMessage(err), "cacs_rings_to_cumulative\\(iso_sf\\)")
  expect_match(conditionMessage(err), "cacs_intersect_weight")
})


test_that("T-UF1-VAL-02 annulus abort=FALSE returns FALSE without throwing", {
  fx <- load_replay_fixture("annulus_input")
  expect_false(.validate_iso_schema(fx, abort = FALSE))
})


test_that("T-UF1-VAL-03 canonical cumulative iso passes validator", {
  iso <- .mk_cumulative_iso_schema()
  expect_true(.validate_iso_schema(iso))
})


test_that("T-UF1-VAL-04 ring_topology annulus fails even without isomin", {
  iso <- .mk_cumulative_iso_schema(ring_topology = "annulus")
  err <- rlang::catch_cnd(.validate_iso_schema(iso))
  expect_s3_class(err, "catchmentACS_error_annulus_input")
  expect_s3_class(err, "cacs_error_annulus_input")
  expect_match(conditionMessage(err), "annulus topology", fixed = TRUE)
})


test_that("T-UF1-VAL-05 isomin > 0 fails even if ring_topology says cumulative", {
  iso <- .mk_cumulative_iso_schema(isomin = 5L, isomax = 10L)
  err <- rlang::catch_cnd(.validate_iso_schema(iso))
  expect_s3_class(err, "catchmentACS_error_annulus_input")
  expect_s3_class(err, "cacs_error_annulus_input")
  expect_match(conditionMessage(err), "ring_topology = \"cumulative\"",
               fixed = TRUE)
  expect_match(conditionMessage(err), "isomin > 0", fixed = TRUE)
  expect_match(conditionMessage(err), "isomin == 0", fixed = TRUE)
})


test_that("T-UF1-VAL-06 isomin = 0 is tolerated as raw evidence", {
  iso <- .mk_cumulative_iso_schema(isomin = 0L, isomax = 15L)
  expect_true(.validate_iso_schema(iso))
})


test_that("T-UF1-VAL-07 missing ring_topology is a generic schema error", {
  iso <- .mk_cumulative_iso_schema()
  iso$ring_topology <- NULL
  err <- rlang::catch_cnd(.validate_iso_schema(iso))
  expect_s3_class(err, "catchmentACS_error_schema")
  expect_false(inherits(err, "catchmentACS_error_annulus_input"))
  expect_false(inherits(err, "cacs_error_annulus_input"))
  expect_match(conditionMessage(err), "ring_topology")
})


test_that("T-UF1-VAL-08 malformed cumulative topology values fail schema", {
  iso_num <- .mk_cumulative_iso_schema(ring_topology = 1L)
  expect_error(.validate_iso_schema(iso_num),
               class = "catchmentACS_error_schema")

  iso_na <- .mk_cumulative_iso_schema(ring_topology = NA_character_)
  expect_error(.validate_iso_schema(iso_na),
               class = "catchmentACS_error_schema")

  iso_bad <- .mk_cumulative_iso_schema(ring_topology = "weird")
  expect_error(.validate_iso_schema(iso_bad),
               class = "catchmentACS_error_schema")
})
