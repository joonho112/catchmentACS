# ============================================================================
# Unit tests for cacs_validate_iso() non-aborting preflight helper.
# ============================================================================


.mk_v030_validate_iso <- function(...) {
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

.expect_validate_examples_parse <- function(issues) {
  examples <- issues$example[!is.na(issues$example) & nzchar(issues$example)]
  for (example in examples) {
    expect_silent(parse(text = example))
  }
}


test_that("T-VALIDATE-ISO-01 valid iso returns zero-row issue tibble", {
  issues <- cacs_validate_iso(.mk_v030_validate_iso())
  expect_s3_class(issues, "tbl_df")
  expect_equal(nrow(issues), 0L)
})

test_that("T-VALIDATE-ISO-02 return columns are stable and ordered", {
  issues <- cacs_validate_iso(.mk_v030_validate_iso())
  expect_equal(names(issues), c(
    "severity", "check", "col", "actual", "expected", "fix_hint", "example"
  ))
})

test_that("T-VALIDATE-ISO-03 exported surface has no abort escape hatch", {
  expect_true("cacs_validate_iso" %in% getNamespaceExports("catchmentACS"))
  expect_false("abort" %in% names(formals(cacs_validate_iso)))
})

test_that("T-VALIDATE-ISO-04 non-sf input returns issue and does not abort", {
  non_sf <- sf::st_drop_geometry(.mk_v030_validate_iso())
  expect_no_error(issues <- cacs_validate_iso(non_sf))
  expect_equal(issues$check, "iso_not_sf")
  expect_match(issues$example, "sf::st_as_sf", fixed = TRUE)
})

test_that("T-VALIDATE-ISO-05 malformed iso returns multiple issues in one pass", {
  iso <- sf::st_transform(.mk_v030_validate_iso(
    provider = "valhalla",
    provider_requested = "valhalla",
    provider_downgrade = "FALSE",
    isochrone_empty = "FALSE",
    retry_count = 1,
    drive_time_min = 0
  ), 4269)
  issues <- cacs_validate_iso(iso)
  expect_true(all(c(
    "iso_crs",
    "iso_provider",
    "iso_provider_requested",
    "iso_provider_downgrade_type",
    "iso_isochrone_empty_type",
    "iso_retry_count_type",
    "iso_drive_time_positive",
    "iso_drive_time_type"
  ) %in% issues$check))
})

test_that("T-VALIDATE-ISO-06 annulus topology reports conversion example", {
  iso <- .mk_v030_validate_iso(ring_topology = "annulus")
  issues <- cacs_validate_iso(iso)
  hit <- issues[issues$check == "iso_annulus_topology", ]
  expect_equal(nrow(hit), 1L)
  expect_equal(hit$example, "iso_sf <- cacs_rings_to_cumulative(iso_sf)")
})

test_that("T-VALIDATE-ISO-07 legacy isomin annulus reports annulus topology", {
  iso <- .mk_v030_validate_iso()
  iso$isomin <- 5
  issues <- cacs_validate_iso(iso)
  hit <- issues[issues$check == "iso_annulus_topology", ]
  expect_equal(hit$col, "isomin")
  expect_match(hit$actual, "isomin > 0", fixed = TRUE)
})

test_that("T-VALIDATE-ISO-08 missing ring_topology has copy-paste example", {
  iso <- .mk_v030_validate_iso()
  iso$ring_topology <- NULL
  issues <- cacs_validate_iso(iso)
  hit <- issues[issues$col == "ring_topology", ]
  expect_equal(hit$check, "iso_missing_column")
  expect_equal(hit$example, "iso_sf$ring_topology <- \"cumulative\"")
})

test_that("T-VALIDATE-ISO-09 missing multiple canonical columns are one row each", {
  iso <- .mk_v030_validate_iso()
  iso$ring_topology <- NULL
  iso$provider_downgrade <- NULL
  iso$retry_count <- NULL
  issues <- cacs_validate_iso(iso)
  expect_equal(sum(issues$check == "iso_missing_column"), 3L)
})

test_that("T-VALIDATE-ISO-10 wrong CRS reports EPSG and transform example", {
  issues <- cacs_validate_iso(sf::st_transform(.mk_v030_validate_iso(), 4269))
  hit <- issues[issues$check == "iso_crs", ]
  expect_equal(hit$actual, "EPSG:4269")
  expect_equal(hit$example, "iso_sf <- sf::st_transform(iso_sf, 4326)")
})

test_that("T-VALIDATE-ISO-11 provider_downgrade character reports logical repair", {
  issues <- cacs_validate_iso(.mk_v030_validate_iso(provider_downgrade = "FALSE"))
  hit <- issues[issues$col == "provider_downgrade", ]
  expect_equal(hit$expected, "logical TRUE/FALSE")
  expect_equal(hit$example, "iso_sf$provider_downgrade <- FALSE")
})

test_that("T-VALIDATE-ISO-12 isochrone_empty character reports logical repair", {
  issues <- cacs_validate_iso(.mk_v030_validate_iso(isochrone_empty = "FALSE"))
  hit <- issues[issues$col == "isochrone_empty", ]
  expect_equal(hit$expected, "logical TRUE/FALSE")
  expect_equal(hit$example, "iso_sf$isochrone_empty <- FALSE")
})

test_that("T-VALIDATE-ISO-13 retry_count double reports integer repair", {
  issues <- cacs_validate_iso(.mk_v030_validate_iso(retry_count = 1))
  hit <- issues[issues$check == "iso_retry_count_type", ]
  expect_equal(hit$example, "iso_sf$retry_count <- as.integer(iso_sf$retry_count)")
})

test_that("T-VALIDATE-ISO-14 drive_time_min zero reports positive-minute issue", {
  issues <- cacs_validate_iso(.mk_v030_validate_iso(drive_time_min = 0L))
  hit <- issues[issues$check == "iso_drive_time_positive", ]
  expect_equal(hit$actual, "0")
})

test_that("T-VALIDATE-ISO-15 drive_time_min double reports integer issue", {
  issues <- cacs_validate_iso(.mk_v030_validate_iso(drive_time_min = 15))
  hit <- issues[issues$check == "iso_drive_time_type", ]
  expect_equal(hit$actual, "<numeric>")
})

test_that("T-VALIDATE-ISO-16 point geometry reports polygon requirement", {
  iso <- .mk_v030_validate_iso()
  sf::st_geometry(iso) <- sf::st_sfc(sf::st_point(c(-86.5, 33.5)), crs = 4326)
  issues <- cacs_validate_iso(iso)
  hit <- issues[issues$check == "iso_geometry_type", ]
  expect_equal(hit$actual, "POINT")
  expect_match(hit$expected, "POLYGON", fixed = TRUE)
})

test_that("T-VALIDATE-ISO-17 provider_requested unsupported reports sanctioned set", {
  issues <- cacs_validate_iso(.mk_v030_validate_iso(provider_requested = "bad"))
  hit <- issues[issues$check == "iso_provider_requested", ]
  expect_match(hit$expected, "osrm", fixed = TRUE)
  expect_equal(hit$actual, "bad")
})

test_that("T-VALIDATE-ISO-18 isomin zero is tolerated", {
  iso <- .mk_v030_validate_iso()
  iso$isomin <- 0
  issues <- cacs_validate_iso(iso)
  expect_false("iso_annulus_topology" %in% issues$check)
})

test_that("T-VALIDATE-ISO-19 malformed examples are parseable", {
  iso <- sf::st_transform(.mk_v030_validate_iso(
    ring_topology = NA_character_,
    provider_downgrade = "FALSE",
    retry_count = 1
  ), 4269)
  .expect_validate_examples_parse(cacs_validate_iso(iso))
})

test_that("T-VALIDATE-ISO-20 valid examples helper handles zero-row output", {
  issues <- cacs_validate_iso(.mk_v030_validate_iso())
  .expect_validate_examples_parse(issues)
  expect_equal(nrow(issues), 0L)
})
