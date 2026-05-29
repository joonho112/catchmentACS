# ============================================================================
# test-bug001-water-tract.R - v0.2.0 Step 3.1 [CORE] regression suite.
#
# Locks the F3 (BUG-001) hybrid fix. Covers BOTH layers:
#
#   Layer (a) `.drop_water_tracts()` inside `cacs_acs_prefetch()`:
#       regex `^[0-9]{2}[0-9]{3}99[0-9]{4}$` (primary) +
#       `st_area() <= 0 | !is.finite()` (defence-in-depth fallback).
#
#   Layer (b) `cacs_intersect_weight()`:
#       degenerate-area abort demoted to warn-and-skip
#       (`catchmentACS_warning_geometry_skip`); fail-loud
#       (`catchmentACS_error_geometry`) preserved when ALL tracts are
#       degenerate; skipped GEOIDs available via
#       `attr(out, "skipped_geoids")`.
#
# Fixture: `inst/testdata/fixture_water_tract_baldwin.rds` (Step 1.4),
# loaded via `tests/testthat/helper-water-fixture.R::load_water_fixture()`.
#
# 10 test cases.
# ============================================================================


# ---- Local helpers --------------------------------------------------------

# `.bug001_with_fake_census_key()` mirrors `.with_fake_census_key` in
# test-unit-acs-prefetch-args.R; duplicated here to avoid cross-file
# dependency on that file's lexical scope.
.bug001_with_fake_census_key <- function(code) {
  withr::with_envvar(c(CENSUS_API_KEY = "FAKE_KEY_FOR_TEST"), code)
}


# `.bug001_mk_iso_4326()` builds a single-site EPSG:4326 isochrone polygon
# covering the Baldwin Co. AL water-tract fixture's bbox (lon roughly
# [-87.98, -87.61], lat [30.52, 31.32]) so `cacs_intersect_weight()` finds
# at least one intersecting land tract. The polygon is wider than the
# fixture bbox so both land tracts overlap. CONUS+DC guard pads ~0.25 deg
# inside .bbox_outside_conus_dc(); we stay well inside that envelope.
.bug001_mk_iso_4326 <- function(site_id = "S01",
                                drive_time_min = 15L,
                                xmin = -88.10, xmax = -87.50,
                                ymin =  30.45, ymax =  31.40) {
  ring <- rbind(
    c(xmin, ymin), c(xmax, ymin),
    c(xmax, ymax), c(xmin, ymax),
    c(xmin, ymin)
  )
  geom <- sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)
  sf::st_sf(
    tibble::tibble(
      site_id                          = site_id,
      drive_time_min                   = as.integer(drive_time_min),
      provider                         = "osrm",
      profile                          = "car",
      osm_snapshot_date                = NA_character_,
      routing_engine_version           = "stub/0.0.0",
      polygon_simplification_tolerance = NA_real_,
      generated_at                     = Sys.time(),
      isochrone_empty                  = FALSE,
      provider_requested               = "osrm",
      provider_downgrade               = FALSE,
      osm_snapshot_status              = NA_character_,
      failure_reason                   = NA_character_,
      retry_count                      = 1L,
      ring_topology                    = "cumulative"
    ),
    geometry = geom
  )
}


# ============================================================================
# Testcase 1 - Prefetch default drops 01003990000 + emits inform message
#              with class `catchmentACS_message_water_tract_filter`.
#
# Strategy: mock `.tidycensus_get_acs_call()` to return the offline fixture
# so we exercise the full prefetch pipeline without a live Census API call.
# The post-fetch `.drop_water_tracts()` layer must (a) remove the water tract
# from the returned sf and (b) emit a message-class condition we can capture.
# ============================================================================

test_that("Testcase 1: prefetch default drops 01003990000 + emits classed inform", {
  skip_if_no_water_fixture()
  fixture <- load_water_fixture()

  .bug001_with_fake_census_key({
    cb <- helper_mock_codebook(known = "B01003_001")
    testthat::local_mocked_bindings(
      load_variables = function(year, dataset, ...) cb,
      .package = "tidycensus"
    )
    testthat::local_mocked_bindings(
      .tidycensus_get_acs_call = function(geography, variables, state, year, survey) fixture,
      .package = "catchmentACS"
    )

    cond <- NULL
    out <- withCallingHandlers(
      cacs_acs_prefetch(
        state             = "AL",
        year              = 2023L,
        variables         = "B01003_001",
        force_refresh     = TRUE,
        drop_water_tracts = TRUE,
        verbose           = TRUE
      ),
      catchmentACS_message_water_tract_filter = function(m) {
        cond <<- m
        invokeRestart("muffleMessage")
      },
      # Silence unrelated cache info messages.
      catchmentACS_message_cache = function(m) invokeRestart("muffleMessage")
    )

    # Layer (a) drop fires: water tract removed.
    expect_false("01003990000" %in% out$GEOID)
    # Healthy land tracts retained.
    expect_true(all(c("01003010100", "01003011103") %in% out$GEOID))
    # Condition was emitted with the locked class.
    expect_s3_class(cond, "catchmentACS_message_water_tract_filter")
    expect_s3_class(cond, "catchmentACS_message")
    expect_s3_class(cond, "catchmentACS_condition")
    # Message body references the dropped GEOID.
    expect_match(conditionMessage(cond), "01003990000", fixed = TRUE)
  })
})


# ============================================================================
# Testcase 2 - `drop_water_tracts = FALSE` retains the water tract
#              (and emits no water-tract message).
# ============================================================================

test_that("Testcase 2: drop_water_tracts = FALSE retains 01003990000", {
  skip_if_no_water_fixture()
  fixture <- load_water_fixture()

  .bug001_with_fake_census_key({
    cb <- helper_mock_codebook(known = "B01003_001")
    testthat::local_mocked_bindings(
      load_variables = function(year, dataset, ...) cb,
      .package = "tidycensus"
    )
    testthat::local_mocked_bindings(
      .tidycensus_get_acs_call = function(geography, variables, state, year, survey) fixture,
      .package = "catchmentACS"
    )

    cond <- NULL
    out <- withCallingHandlers(
      cacs_acs_prefetch(
        state             = "AL",
        year              = 2023L,
        variables         = "B01003_001",
        force_refresh     = TRUE,
        drop_water_tracts = FALSE,
        verbose           = TRUE
      ),
      catchmentACS_message_water_tract_filter = function(m) {
        cond <<- m
        invokeRestart("muffleMessage")
      },
      catchmentACS_message_cache = function(m) invokeRestart("muffleMessage")
    )

    expect_true("01003990000" %in% out$GEOID)
    expect_identical(setequal(out$GEOID, fixture$GEOID), TRUE)
    expect_null(cond)
  })
})


# ============================================================================
# Testcase 3 - Intersect with retained water tract -> warns class
#              `catchmentACS_warning_geometry_skip` + skips (not aborts);
#              `skipped_geoids` attribute populated.
# ============================================================================

test_that("Testcase 3: intersect demotes degenerate abort to warn-and-skip", {
  skip_if_no_water_fixture()
  fixture <- load_water_fixture()
  iso     <- .bug001_mk_iso_4326()

  withr::local_options(catchmentACS.cache_intersect = FALSE)

  cond <- NULL
  out <- withCallingHandlers(
    suppressMessages(
      cacs_intersect_weight(
        iso_sf  = iso,
        acs_sf  = fixture,
        verbose = FALSE
      )
    ),
    catchmentACS_warning_geometry_skip = function(w) {
      cond <<- w
      invokeRestart("muffleWarning")
    },
    # Silence the unrelated cross-state soft hint from §21.3 step 1f
    # (iso bbox extends beyond fixture bbox by design here).
    catchmentACS_warning_provenance = function(w) invokeRestart("muffleWarning")
  )

  # The warning was emitted (not an abort).
  expect_s3_class(cond, "catchmentACS_warning_geometry_skip")
  expect_s3_class(cond, "catchmentACS_warning")
  expect_s3_class(cond, "catchmentACS_condition")
  expect_match(conditionMessage(cond), "01003990000", fixed = TRUE)

  # `skipped_geoids` attribute exists and contains the water tract.
  expect_true("01003990000" %in% attr(out, "skipped_geoids"))

  # Output is a valid tibble with the canonical schema.
  expect_s3_class(out, "tbl_df")
})


# ============================================================================
# Testcase 4 - Canonical 22-mandatory long-output schema (F4) preserved
#              after auto-filter.
# ============================================================================

test_that("Testcase 4: canonical long-output schema preserved after auto-filter", {
  skip_if_no_water_fixture()
  fixture <- load_water_fixture()
  iso     <- .bug001_mk_iso_4326()

  withr::local_options(catchmentACS.cache_intersect = FALSE)

  out <- suppressMessages(suppressWarnings(
    cacs_intersect_weight(
      iso_sf  = iso,
      acs_sf  = fixture,
      verbose = FALSE
    )
  ))

  # 23 mandatory canonical long columns (v0.3 topology schema) all present.
  expect_true(all(catchmentACS:::.LONG_REQUIRED_COLS %in% names(out)))
  # Aggregation carrier attribute present and well-formed.
  carr <- attr(out, "cacs_aggregation_carriers")
  expect_s3_class(carr, "tbl_df")
  expect_true(all(catchmentACS:::.CARRIER_REQUIRED_COLS %in% names(carr)))
  # Provenance attribute also present.
  expect_false(is.null(attr(out, "cacs_aggregation_provenance")))
  expect_identical(attr(out, "cacs_schema_version"), "1.0")
})


# ============================================================================
# Testcase 5 - Layer composition: user-supplied `acs_sf` (no prefetch)
#              still saved by Layer (b) warn-and-skip.
#
# This is the "I built my own ACS sf" path - the user could be reading from
# a custom GPKG or a hand-built fixture that bypassed Layer (a) entirely.
# Layer (b) inside cacs_intersect_weight() must catch the degenerate tract.
# ============================================================================

test_that("Testcase 5: Layer (b) saves the run when user bypasses prefetch", {
  skip_if_no_water_fixture()
  # Mimic an external sf input by stripping every cacs_* attribute the
  # prefetch would have attached.
  acs_user <- load_water_fixture()
  attr(acs_user, "cacs_provenance")      <- NULL
  attr(acs_user, "cacs_acs_provenance")  <- NULL
  attr(acs_user, "cacs_schema_version")  <- NULL

  iso <- .bug001_mk_iso_4326()
  withr::local_options(catchmentACS.cache_intersect = FALSE)

  out <- suppressMessages(suppressWarnings(
    cacs_intersect_weight(
      iso_sf  = iso,
      acs_sf  = acs_user,
      verbose = FALSE
    )
  ))

  # Run reached the post-loop bind without aborting; output is canonical.
  expect_s3_class(out, "tbl_df")
  # Layer (b) caught the bad tract.
  expect_true("01003990000" %in% attr(out, "skipped_geoids"))
  # And the healthy tracts still contributed at least one success row.
  expect_gte(nrow(out), 1L)
})


# ============================================================================
# Testcase 6 - Fail-loud guard: all-degenerate input still errors with
#              class `catchmentACS_error_geometry`.
#
# Build a 2-row sf where BOTH tracts have collapsed (zero-area) geometry.
# We use collinear-vertex multipolygons so the bbox is finite (inside
# CONUS+DC, so the upstream operator-class guard does not preempt) but
# st_area() returns 0 - which is exactly what BUG-001 exhibited. The
# warn-and-skip demotion only fires when at least one healthy tract
# remains; with zero healthy tracts the function must still abort.
# ============================================================================

test_that("Testcase 6: all-degenerate input preserves fail-loud abort", {
  # Build two collapsed (zero-area) MULTIPOLYGON tracts. Each "ring" is
  # 4 colinear points along a single horizontal line so the polygon has
  # well-defined finite vertices (bbox is valid + CONUS-inside) yet
  # st_area() on EPSG:5070 returns 0 - exactly the BUG-001 trigger.
  # Wrap fixture construction in sf_use_s2(FALSE) so the s2 validity
  # check (which rejects duplicate edges) does not preempt our test;
  # cacs_intersect_weight() also disables s2 internally during the
  # area computation, so this matches the in-function CRS regime.
  prev_s2 <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(prev_s2)), add = TRUE)

  # 4-point ring with all vertices coincident at a single lon/lat (point
  # collapse). After EPSG:5070 reprojection inside cacs_intersect_weight()
  # the result still has 0 area, and the polygon is structurally valid
  # under sf_use_s2(FALSE) (PROJ planar).
  flat_ring1 <- rbind(
    c(-87.70, 30.55), c(-87.70, 30.55),
    c(-87.70, 30.55), c(-87.70, 30.55)
  )
  flat_ring2 <- rbind(
    c(-87.50, 30.60), c(-87.50, 30.60),
    c(-87.50, 30.60), c(-87.50, 30.60)
  )
  all_degen <- sf::st_sf(
    GEOID    = c("01003990000", "01003990001"),
    NAME     = c("Flat Tract A", "Flat Tract B"),
    variable = c("B01003_001", "B01003_001"),
    estimate = c(100, 200),
    moe      = c(10, 20),
    geometry = sf::st_sfc(
      sf::st_multipolygon(list(list(flat_ring1))),
      sf::st_multipolygon(list(list(flat_ring2))),
      crs = 4269
    )
  )

  # Sanity: bbox is finite + CONUS-inside, but areas are zero.
  expect_true(all(is.finite(sf::st_bbox(all_degen))))
  expect_true(all(suppressWarnings(as.numeric(sf::st_area(all_degen))) == 0))

  iso <- .bug001_mk_iso_4326()
  withr::local_options(catchmentACS.cache_intersect = FALSE)

  err <- tryCatch(
    suppressMessages(suppressWarnings(
      cacs_intersect_weight(
        iso_sf  = iso,
        acs_sf  = all_degen,
        verbose = FALSE
      )
    )),
    error = function(e) e
  )

  expect_s3_class(err, "catchmentACS_error_geometry")
  expect_s3_class(err, "catchmentACS_error")
  expect_s3_class(err, "catchmentACS_condition")
  # Error hint points back to the prefetch opt-in for the fix path.
  expect_match(
    conditionMessage(err),
    "(drop_water_tracts|cacs_acs_prefetch|degenerate|non-positive)",
    perl = TRUE
  )
})


# ============================================================================
# Testcase 7 - Round-trip: `prefetch %>% intersect_weight` no longer aborts
#              on the Baldwin water tract under v0.2 defaults.
# ============================================================================

test_that("Testcase 7: prefetch -> intersect_weight round-trip no longer aborts", {
  skip_if_no_water_fixture()
  fixture <- load_water_fixture()
  iso     <- .bug001_mk_iso_4326()

  .bug001_with_fake_census_key({
    cb <- helper_mock_codebook(known = "B01003_001")
    testthat::local_mocked_bindings(
      load_variables = function(year, dataset, ...) cb,
      .package = "tidycensus"
    )
    testthat::local_mocked_bindings(
      .tidycensus_get_acs_call = function(geography, variables, state, year, survey) fixture,
      .package = "catchmentACS"
    )

    withr::local_options(catchmentACS.cache_intersect = FALSE)

    acs <- suppressMessages(
      cacs_acs_prefetch(
        state         = "AL",
        year          = 2023L,
        variables     = "B01003_001",
        force_refresh = TRUE,
        verbose       = FALSE
      )
    )

    # Layer (a) already removed the water tract during prefetch.
    expect_false("01003990000" %in% acs$GEOID)

    # Round-trip into intersect_weight: succeeds (no abort).
    out <- suppressMessages(suppressWarnings(
      cacs_intersect_weight(
        iso_sf  = iso,
        acs_sf  = acs,
        verbose = FALSE
      )
    ))

    expect_s3_class(out, "tbl_df")
    # Because Layer (a) already filtered, Layer (b) had nothing to skip.
    expect_identical(attr(out, "skipped_geoids"), character(0))
    # Round-trip yielded at least one success row.
    expect_gte(nrow(out), 1L)
  })
})


# ============================================================================
# Testcase 8 - Hybrid detection: geometry test catches water tract even
#              when GEOID is NOT 99xxxx (mimics a future Census convention
#              drift where the lexical signal stops firing).
# ============================================================================

test_that("Testcase 8: degenerate-area fallback catches non-99 GEOID water tract", {
  skip_if_no_water_fixture()
  fixture <- load_water_fixture()
  # Re-label the water tract with a non-99 GEOID so only the area test fires.
  drifted <- fixture
  drifted$GEOID[drifted$GEOID == "01003990000"] <- "01003011199"

  res <- catchmentACS:::.drop_water_tracts(
    acs_sf            = drifted,
    drop_water_tracts = TRUE,
    verbose           = FALSE
  )

  # Drifted GEOID dropped via area fallback (not regex).
  expect_true("01003011199" %in% res$dropped_geoids)
  expect_true("zero_area" %in% res$dropped_reasons)
  # Healthy land tracts retained.
  expect_setequal(res$kept_sf$GEOID, c("01003010100", "01003011103"))
})


# ============================================================================
# Testcase 9 - Hybrid detection: degenerate (empty) geometry is caught by
#              the `<= 0 | !is.finite()` arm of the area fallback even
#              when the regex would miss.
# ============================================================================

test_that("Testcase 9: empty/degenerate geometry treated as zero-area", {
  skip_if_no_water_fixture()
  fixture <- load_water_fixture()
  # Replace the water tract geometry with an EMPTY polygon
  # (sf::st_area() returns 0 - on the `<= 0` arm of the predicate).
  # Also overwrite GEOID so only the area path can fire (not the regex).
  corrupted <- fixture
  empty_pg  <- sf::st_polygon()
  corrupted$geometry[corrupted$GEOID == "01003990000"] <- sf::st_sfc(
    empty_pg, crs = sf::st_crs(corrupted)
  )
  corrupted$GEOID[corrupted$GEOID == "01003990000"] <- "01003000099"

  res <- catchmentACS:::.drop_water_tracts(
    acs_sf            = corrupted,
    drop_water_tracts = TRUE,
    verbose           = FALSE
  )

  # Corrupted tract is dropped despite a non-99 GEOID.
  expect_true("01003000099" %in% res$dropped_geoids)
  expect_true(any(res$dropped_reasons %in% c("zero_area", "geoid_pattern+zero_area")))
})


# ============================================================================
# Testcase 10 - Hybrid detection: regex hits + geometry FINE -> still drop
#               (regex authoritative for Census convention so we drop
#               special-purpose tracts even when geometry is healthy).
# ============================================================================

test_that("Testcase 10: regex-matched tract dropped even when geometry is healthy", {
  # Build a synthetic 2-tract sf: one regex-match GEOID with a valid 1km^2
  # geometry, plus one normal land tract. Layer (a) must drop the regex-
  # match row even though its area is non-degenerate.
  good_ring <- rbind(
    c(-87.70, 30.55), c(-87.60, 30.55),
    c(-87.60, 30.65), c(-87.70, 30.65),
    c(-87.70, 30.55)
  )
  acs_sf <- sf::st_sf(
    GEOID    = c("01003123400", "01003990001"),   # land + regex-match
    NAME     = c("Land Tract", "Special-Purpose Tract"),
    variable = c("B01003_001", "B01003_001"),
    estimate = c(1000, 2000),
    moe      = c(50, 80),
    geometry = sf::st_sfc(
      sf::st_multipolygon(list(list(good_ring))),
      sf::st_multipolygon(list(list(good_ring))),
      crs = 4269
    )
  )

  # Sanity: both rows have positive, finite area.
  areas <- suppressWarnings(as.numeric(sf::st_area(acs_sf)))
  expect_true(all(areas > 0))

  res <- catchmentACS:::.drop_water_tracts(
    acs_sf            = acs_sf,
    drop_water_tracts = TRUE,
    verbose           = FALSE
  )

  expect_true("01003990001" %in% res$dropped_geoids)
  expect_true("geoid_pattern" %in% res$dropped_reasons)
  expect_identical(res$kept_sf$GEOID, "01003123400")
})
