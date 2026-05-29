# ============================================================================
# Unit tests for R/intersect-weight.R - Step 5.1 pre-loop validation surface.
# 11 cases (T21-01..T21-08 + T21-19 + 2 defensive edge cases).
#
# v0.2 Step 3.1 (F3 / BUG-001) note: the degenerate-tract abort previously
# wired in at the post-st_area() check (E-21-16) is now demoted to a
# warn-and-skip path (`catchmentACS_warning_geometry_skip`) when at least
# one healthy tract remains; the abort is preserved only for the
# all-degenerate case. There were no v0.1 testcases in this file asserting
# that abort (the degenerate-tract regression is covered in
# `test-bug001-water-tract.R`), so no `expect_error -> expect_warning`
# flips are needed here. The new test file owns the demotion contract.
# ============================================================================


.mk_iso_sf_local <- function(crs = 4326,
                             site_id = "S01",
                             drive_time_min = 15L,
                             provider = "osrm",
                             provider_requested = "osrm",
                             xmin = -86.85, xmax = -86.75,
                             ymin =  33.48, ymax =  33.58) {
  ring <- rbind(
    c(xmin, ymin), c(xmax, ymin),
    c(xmax, ymax), c(xmin, ymax),
    c(xmin, ymin)
  )
  geom <- sf::st_sfc(sf::st_polygon(list(ring)), crs = crs)
  sf::st_sf(
    tibble::tibble(
      site_id                          = site_id,
      drive_time_min                   = as.integer(drive_time_min),
      provider                         = provider,
      profile                          = "car",
      osm_snapshot_date                = NA_character_,
      routing_engine_version           = "stub/0.0.0",
      polygon_simplification_tolerance = NA_real_,
      generated_at                     = Sys.time(),
      isochrone_empty                  = FALSE,
      provider_requested               = provider_requested,
      provider_downgrade               = FALSE,
      osm_snapshot_status              = NA_character_,
      failure_reason                   = NA_character_,
      retry_count                      = 1L,
      ring_topology                    = "cumulative"
    ),
    geometry = geom
  )
}

.mk_acs_sf_local <- function(crs = 4269,
                             geoid = "01073001100",
                             xmin = -86.95, xmax = -86.65,
                             ymin =  33.40, ymax =  33.65) {
  ring <- rbind(
    c(xmin, ymin), c(xmax, ymin),
    c(xmax, ymax), c(xmin, ymax),
    c(xmin, ymin)
  )
  geom <- sf::st_sfc(sf::st_multipolygon(list(list(ring))), crs = crs)
  sf::st_sf(
    tibble::tibble(
      GEOID    = geoid,
      NAME     = "Tract 11, Jefferson County, Alabama",
      variable = "B01003_001",
      estimate = 4500,
      moe      = 250
    ),
    geometry = geom
  )
}


# T21-01..03 iso_sf schema

test_that("T21-01 iso_sf non-sf aborts with schema family", {
  iso <- tibble::tibble(site_id = "S01", drive_time_min = 15L)
  acs <- .mk_acs_sf_local()
  expect_error(
    cacs_intersect_weight(iso_sf = iso, acs_sf = acs),
    class = "catchmentACS_error_schema"
  )
})

test_that("T21-02 iso_sf missing site_id column aborts with schema family", {
  iso <- .mk_iso_sf_local()
  iso$site_id <- NULL
  acs <- .mk_acs_sf_local()
  expect_error(
    cacs_intersect_weight(iso_sf = iso, acs_sf = acs),
    class = "catchmentACS_error_schema"
  )
})

test_that("T21-03 iso_sf CRS != 4326 aborts (no auto-reproject)", {
  iso <- .mk_iso_sf_local(crs = 4269)
  acs <- .mk_acs_sf_local()
  expect_error(
    cacs_intersect_weight(iso_sf = iso, acs_sf = acs),
    class = "catchmentACS_error_schema"
  )
})


# T21-04..05 acs_sf schema

test_that("T21-04 acs_sf CRS != 4269 aborts (NAD83 invariant)", {
  iso <- .mk_iso_sf_local()
  acs <- .mk_acs_sf_local(crs = 4326)
  expect_error(
    cacs_intersect_weight(iso_sf = iso, acs_sf = acs),
    class = "catchmentACS_error_schema"
  )
})

test_that("T21-05 acs_sf GEOID with 10 digits aborts", {
  iso <- .mk_iso_sf_local()
  acs <- .mk_acs_sf_local(geoid = "0107300110")
  expect_error(
    cacs_intersect_weight(iso_sf = iso, acs_sf = acs),
    class = "catchmentACS_error_schema"
  )
})


# T21-06 weight_method='population' fail-loud (Phase 5.3 Decision #5)
#
# The current beta defers population weighting via a hoisted fail-loud at
# the very top of cacs_intersect_weight() (right after match.arg). The abort
# uses condition class `catchmentACS_error_credential` and fires BEFORE any
# schema validation, so bg_pop_sf state is irrelevant — the credential abort
# always wins regardless of whether bg_pop_sf is NULL or a valid sf.
#
# Pre-v0.1 contract was "schema abort with bg_pop_sf required"; that branch is
# now unreachable in the current release (kept under # nocov for future wire-in).
test_that("T21-06 weight_method = 'population' aborts with credential class (future-release deferral)", {
  iso <- .mk_iso_sf_local()
  acs <- .mk_acs_sf_local()
  expect_error(
    cacs_intersect_weight(iso, acs, bg_pop_sf = NULL,
                          weight_method = "population"),
    class = "catchmentACS_error_credential"
  )
})


# T21-07..08 min_weight range

test_that("T21-07 min_weight = -1 aborts with schema family", {
  iso <- .mk_iso_sf_local()
  acs <- .mk_acs_sf_local()
  expect_error(
    cacs_intersect_weight(iso, acs, min_weight = -1),
    class = "catchmentACS_error_schema"
  )
})

test_that("T21-08 min_weight = 1.5 aborts with schema family", {
  iso <- .mk_iso_sf_local()
  acs <- .mk_acs_sf_local()
  expect_error(
    cacs_intersect_weight(iso, acs, min_weight = 1.5),
    class = "catchmentACS_error_schema"
  )
})


# T21-19 outside v1.0 CONUS+DC scope (operator family abort)

test_that("T21-19 Alaska bbox aborts with operator family", {
  iso <- .mk_iso_sf_local(xmin = -150.1, xmax = -149.9,
                          ymin =   61.1, ymax =   61.3)
  acs <- .mk_acs_sf_local(geoid = "02020000100",
                          xmin = -150.2, xmax = -149.8,
                          ymin =   61.0, ymax =   61.4)
  expect_error(
    cacs_intersect_weight(iso, acs),
    class = "catchmentACS_error_operator"
  )
})


# T21-19b DEFENSIVE - Florida Keys near 24.45N must STILL pass

test_that("T21-19b Florida Keys (lat ~24.55) passes CONUS+DC guard", {
  iso <- .mk_iso_sf_local(xmin = -81.83, xmax = -81.73,
                          ymin =  24.50, ymax =  24.60)
  acs <- .mk_acs_sf_local(geoid = "12087970100",
                          xmin = -81.85, xmax = -81.70,
                          ymin =  24.45, ymax =  24.65)
  # Step 5.4 closes the function: CONUS guard passes -> per-site loop
  # -> family dispatch -> post-loop bind -> canonical long output returned.
  # We assert success (no abort) and a valid output with carrier.
  withr::local_options(catchmentACS.cache_intersect = FALSE)
  out <- suppressMessages(cacs_intersect_weight(iso, acs, verbose = FALSE))
  expect_s3_class(out, "tbl_df")
  expect_setequal(names(out), catchmentACS:::.LONG_REQUIRED_COLS)
  expect_identical(attr(out, "cacs_schema_version"), "1.0")
  expect_false(is.null(attr(out, "cacs_aggregation_carriers")))
})


# T21-EC1 DEFENSIVE - sf_use_s2 must be restored even after full success path

test_that("T21-EC1 sf_use_s2 is restored after a full successful call", {
  iso <- .mk_iso_sf_local()
  acs <- .mk_acs_sf_local()

  prev <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(TRUE))
  on.exit(suppressMessages(sf::sf_use_s2(prev)), add = TRUE)

  withr::local_options(catchmentACS.cache_intersect = FALSE)
  # Step 5.4 closes the function so the call now returns instead of aborting.
  # The s2 restore contract still holds via on.exit() chain.
  out <- suppressMessages(cacs_intersect_weight(iso, acs, verbose = FALSE))
  expect_s3_class(out, "tbl_df")

  # Defensive contract: s2 state restored to TRUE on normal exit
  expect_true(sf::sf_use_s2())
})
