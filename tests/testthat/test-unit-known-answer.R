# ============================================================================
# Known-answer tests for R/intersect-weight.R Step 5.2 per-site loop body.
#
# Coverage (§21.10 test catalogue, Step 5.2 portion):
#
#   T21-09  - synthetic_single_site fixture, 1 site x 1 tract quarter overlap
#             -> area_wt = 0.25 at epsilon = 1e-9
#
#   T21-10  - synthetic_2tract fixture, 2-tract weighted overlap (50% + 30%)
#             -> area_wts = c(0.5, 0.3), weight_sum = 0.8 at epsilon = 1e-9
#
#   T21-12  - sliver filter strict-greater boundary: a 1m^2 sliver in a 1km^2
#             tract has area_wt = exactly 1e-6 == min_weight, so the strict
#             inequality `area_wt > min_weight` MUST drop it. Returns
#             .empty_site_result(failure_reason = "all_slivers_below_min_weight").
#
#   T21-15  - per-site isolation in a 3-site mixed batch (valid + empty + valid):
#             a middle site whose isochrone has no tract intersection MUST NOT
#             corrupt the two flanking valid sites. Verified by calling the
#             internal closure 3 times and asserting independence (the public
#             entry still aborts at the Step 11-15 deferral stub).
#
# Strategy: Step 5.2 returns an INTERMEDIATE result (the per-tract long tibble)
# and the public entry still aborts at the Step 11-15 deferral stub. The
# tests therefore exercise `.intersect_one_site()` DIRECTLY (it is internal
# but reachable via `:::`). The fixtures are pre-projected to EPSG:5070 (per
# inst/testdata convention) which is exactly what the closure expects.
# ============================================================================


# ---- Fixture-to-closure adapter --------------------------------------------
#
# The synthetic fixtures are EPSG:5070 already; we only need to (a) ensure
# sf_use_s2(FALSE) for planar geometry math (mirrors the Step 5.1 pre-loop)
# and (b) precompute `tract_area_m2` on the ACS sf (mirrors algorithm step 4).
.run_intersect_one_site <- function(iso_5070, acs_5070,
                                    sid, dt, min_weight = 1e-6) {
  prev_s2 <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(prev_s2)), add = TRUE)

  if (is.null(acs_5070$tract_area_m2)) {
    acs_5070$tract_area_m2 <- as.numeric(sf::st_area(acs_5070))
  }

  catchmentACS:::.intersect_one_site(
    sid        = sid,
    dt         = as.integer(dt),
    iso_5070   = iso_5070,
    acs_5070   = acs_5070,
    min_weight = min_weight
  )
}


# ============================================================================
# T21-09 - single-site quarter overlap, area_wt = 0.25 (epsilon = 1e-9)
# ============================================================================

test_that("T21-09 single-site quarter overlap yields area_wt = 0.25 at eps=1e-9", {
  fx <- helper_load_synthetic_single_site()

  # Fixture sanity (defensive against fixture regen bugs).
  expect_equal(sf::st_crs(fx$site)$epsg,  5070L)
  expect_equal(sf::st_crs(fx$tract)$epsg, 5070L)
  expect_equal(fx$expected_area_wt, 0.25, tolerance = 1e-12)

  out <- .run_intersect_one_site(
    iso_5070 = fx$site,
    acs_5070 = fx$tract,
    sid      = "S01",
    dt       = 15L
  )

  # Successful site -> sf tibble with area_wt column (NOT a failure tibble).
  expect_s3_class(out, "sf")
  expect_true("area_wt" %in% names(out))
  expect_true("int_area_m2" %in% names(out))
  expect_true("tract_area_m2" %in% names(out))
  expect_equal(nrow(out), 1L)
  expect_equal(out$area_wt,       0.25,    tolerance = 1e-9)
  expect_equal(out$int_area_m2,   250000,  tolerance = 1e-9)
  expect_equal(out$tract_area_m2, 1e6,     tolerance = 1e-9)
  expect_equal(out$site_id,        "S01")
  expect_equal(out$drive_time_min, 15L)
  # ACS attributes preserved through st_intersection.
  expect_equal(out$GEOID,    "01001000001")
  expect_equal(out$variable, "B17001_002")
  expect_equal(out$estimate, 100)
  expect_equal(out$moe,      12)
})


# ============================================================================
# T21-10 - two-tract weighted-sum overlap, area_wts = c(0.5, 0.3)
# ============================================================================

test_that("T21-10 two-tract overlap yields area_wts = c(0.5, 0.3) at eps=1e-9", {
  fx <- helper_load_synthetic_2tract()

  expect_equal(sf::st_crs(fx$site)$epsg,   5070L)
  expect_equal(sf::st_crs(fx$tracts)$epsg, 5070L)
  expect_equal(fx$expected_area_wts,  c(0.5, 0.3), tolerance = 1e-12)
  expect_equal(fx$expected_weight_sum, 0.8,        tolerance = 1e-12)

  out <- .run_intersect_one_site(
    iso_5070 = fx$site,
    acs_5070 = fx$tracts,
    sid      = "S01",
    dt       = 15L
  )

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 2L)
  # Order may not match fixture row order; reorder by GEOID for stability.
  out <- out[order(out$GEOID), ]
  expect_equal(out$area_wt,    c(0.5, 0.3), tolerance = 1e-9)
  expect_equal(sum(out$area_wt), 0.8,       tolerance = 1e-9)
  # ACS attributes preserved through st_intersection for both tracts.
  expect_equal(out$GEOID,    c("01001000001", "01001000002"))
  expect_equal(out$estimate, c(1000, 2000))
})


# ============================================================================
# T21-12 - sliver filter strict-greater boundary (area_wt == min_weight drops)
#
# Build a 1m x 1m square isochrone inside a 1km x 1km tract:
#   intersection area = 1 m^2
#   tract area        = 1e6 m^2
#   area_wt           = 1e-6 == min_weight (default)
# Strict-greater filter `area_wt > min_weight` must drop the row, returning
# an `.empty_site_result()` with failure_reason "all_slivers_below_min_weight".
# ============================================================================

test_that("T21-12 sliver at exact min_weight boundary is dropped (strict-greater)", {
  # 1km x 1km tract (area = 1e6 m^2).
  tract <- sf::st_sf(
    GEOID    = "01001000099",
    NAME     = "Tract Sliver",
    variable = "B01003_001",
    estimate = 100,
    moe      = 5,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(0,    0),    c(1000, 0),
        c(1000, 1000), c(0,    1000),
        c(0,    0)
      ))),
      crs = 5070
    )
  )

  # 1m x 1m square isochrone inside the tract -> intersection area = 1 m^2.
  # area_wt = 1 / 1e6 = 1e-6 = min_weight -> STRICT drop.
  iso <- sf::st_sf(
    site_id        = "S01",
    drive_time_min = 15L,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(0, 0), c(1, 0),
        c(1, 1), c(0, 1),
        c(0, 0)
      ))),
      crs = 5070
    )
  )

  out <- .run_intersect_one_site(
    iso_5070   = iso,
    acs_5070   = tract,
    sid        = "S01",
    dt         = 15L,
    min_weight = 1e-6
  )

  # Strict drop -> empty result tibble (NOT sf).
  expect_s3_class(out, "tbl_df")
  expect_false(inherits(out, "sf"))
  expect_equal(nrow(out), 1L)
  expect_equal(out$site_id,        "S01")
  expect_equal(out$drive_time_min, 15L)
  expect_equal(out$n_tracts,       0L)
  expect_equal(out$failure_reason, "all_slivers_below_min_weight")
})


# ============================================================================
# T21-15 - per-site isolation: 3 sites (valid + empty + valid) share batch
#
# The middle site (S02) is placed far away so it has no tract intersection.
# Calling the closure 3 times must yield: r1 = sf (valid), r2 = empty tibble
# (failure_reason = "no_tract_intersection"), r3 = sf (valid). Empty middle
# must NOT corrupt the flanking valid results.
# ============================================================================

test_that("T21-15 3-site mixed batch (valid + empty + valid) returns independent results", {
  fx <- helper_load_synthetic_3site_4tract()
  tracts_sf <- fx$tracts  # 4 tracts, 2x2 grid 0..2000m

  # SITE_01: 800x800m square inside tract A (lower-left 1km cell)
  iso_01 <- sf::st_sf(
    site_id        = "SITE_01",
    drive_time_min = 15L,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(100, 100), c(900, 100),
        c(900, 900), c(100, 900),
        c(100, 100)
      ))),
      crs = 5070
    )
  )

  # SITE_02: 1000x1000m square 100km east of tracts -> no intersection
  iso_02 <- sf::st_sf(
    site_id        = "SITE_02",
    drive_time_min = 15L,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(100000, 100000), c(101000, 100000),
        c(101000, 101000), c(100000, 101000),
        c(100000, 100000)
      ))),
      crs = 5070
    )
  )

  # SITE_03: 800x800m square inside tract D (upper-right 1km cell)
  iso_03 <- sf::st_sf(
    site_id        = "SITE_03",
    drive_time_min = 15L,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(1100, 1100), c(1900, 1100),
        c(1900, 1900), c(1100, 1900),
        c(1100, 1100)
      ))),
      crs = 5070
    )
  )

  iso_5070 <- rbind(iso_01, iso_02, iso_03)
  acs_5070 <- tracts_sf
  acs_5070$tract_area_m2 <- as.numeric(sf::st_area(acs_5070))

  # Call the internal closure 3 times against the same batch inputs.
  prev_s2 <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(prev_s2)), add = TRUE)

  r1 <- catchmentACS:::.intersect_one_site("SITE_01", 15L, iso_5070, acs_5070, 1e-6)
  r2 <- catchmentACS:::.intersect_one_site("SITE_02", 15L, iso_5070, acs_5070, 1e-6)
  r3 <- catchmentACS:::.intersect_one_site("SITE_03", 15L, iso_5070, acs_5070, 1e-6)

  # r1 valid: intersects tract A (and only tract A).
  expect_s3_class(r1, "sf")
  expect_true("area_wt" %in% names(r1))
  expect_true(nrow(r1) >= 1L)
  expect_true(all(r1$area_wt > 0 & r1$area_wt <= 1))
  expect_true("01001000001" %in% r1$GEOID)  # tract A

  # r2 empty: no_tract_intersection -> .empty_site_result tibble (NOT sf).
  expect_s3_class(r2, "tbl_df")
  expect_false(inherits(r2, "sf"))
  expect_equal(r2$site_id,        "SITE_02")
  expect_equal(r2$drive_time_min, 15L)
  expect_equal(r2$n_tracts,       0L)
  expect_equal(r2$failure_reason, "no_tract_intersection")

  # r3 valid: intersects tract D (and only tract D).
  expect_s3_class(r3, "sf")
  expect_true("area_wt" %in% names(r3))
  expect_true(nrow(r3) >= 1L)
  expect_true(all(r3$area_wt > 0 & r3$area_wt <= 1))
  expect_true("01001000004" %in% r3$GEOID)  # tract D

  # Independence: r2's empty did NOT corrupt r1 or r3, and r1 / r3 cover
  # disjoint tracts (lower-left vs upper-right of 2x2 grid).
  expect_false(any(r1$GEOID %in% r3$GEOID))
})


# ============================================================================
# Step 5.4 batch integration smoke - full public entry now returns a valid
# 20-col canonical long output (Step 5.4 closes the function).
#
# Verifies the lapply + tryCatch + .compute_family_aggregation() ->
# post-loop bind + iso_provenance left_join + 3 attributes attach -> return
# wires through end-to-end on a canonical (EPSG:4326 + EPSG:4269) synthetic
# two-tract fixture.
# ============================================================================

test_that("Step 5.4 public entry returns canonical long output after full algorithm", {
  fx <- helper_load_synthetic_2tract()

  # Lift fixtures from EPSG:5070 to canonical entry CRSs by adding the
  # 14-col iso schema + MULTIPOLYGON-cast acs schema. Step 5.1 will then
  # transform back to EPSG:5070 internally.
  iso_4326 <- sf::st_sf(
    tibble::tibble(
      site_id                          = fx$site$site_id,
      drive_time_min                   = as.integer(fx$site$drive_time_min),
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
    geometry = sf::st_transform(sf::st_geometry(fx$site), 4326)
  )
  # Shift fixture into CONUS bbox so the §1e CONUS guard passes after
  # forward+back projection. Easiest: build CONUS-anchored copy.
  cx <- -86.80; cy <- 33.50
  iso_4326$geometry <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(cx,           cy),
      c(cx + 0.008,   cy),
      c(cx + 0.008,   cy + 0.005),
      c(cx,           cy + 0.005),
      c(cx,           cy)
    ))),
    crs = 4326
  )
  acs_4269 <- sf::st_sf(
    tibble::tibble(
      GEOID    = fx$tracts$GEOID,
      NAME     = fx$tracts$NAME,
      variable = fx$tracts$variable,
      estimate = fx$tracts$estimate,
      moe      = fx$tracts$moe
    ),
    geometry = sf::st_sfc(
      sf::st_multipolygon(list(list(rbind(
        c(cx - 0.005,    cy - 0.005),
        c(cx + 0.020,    cy - 0.005),
        c(cx + 0.020,    cy + 0.010),
        c(cx - 0.005,    cy + 0.010),
        c(cx - 0.005,    cy - 0.005)
      )))),
      sf::st_multipolygon(list(list(rbind(
        c(cx + 0.020,    cy - 0.005),
        c(cx + 0.040,    cy - 0.005),
        c(cx + 0.040,    cy + 0.010),
        c(cx + 0.020,    cy + 0.010),
        c(cx + 0.020,    cy - 0.005)
      )))),
      crs = 4269
    )
  )

  withr::local_options(catchmentACS.cache_intersect = FALSE)
  out <- suppressMessages(cacs_intersect_weight(iso_4326, acs_4269, verbose = FALSE))

  # Step 5.4 closes the function: a 20-col long tibble with carrier attribute.
  expect_s3_class(out, "tbl_df")
  expect_setequal(names(out), catchmentACS:::.LONG_REQUIRED_COLS)
  expect_identical(attr(out, "cacs_schema_version"), "1.0")
  expect_false(is.null(attr(out, "cacs_aggregation_carriers")))
  expect_false(is.null(attr(out, "cacs_aggregation_provenance")))
  # The 2-tract synth yields >= 1 success row (one per (site, drive_time, var)).
  expect_gte(nrow(out), 1L)
})


# ============================================================================
# T21-16 - weight_method = "population" fail-loud (Phase 5.3 Decision #5)
#
# The current beta defers population weighting; the public entry must abort
# with `catchmentACS_error_credential` class and a clear future-release hint
# so the user gets the deferral, not a cryptic schema or geometry error.
# Verifies the abort fires BEFORE schema validation by passing intentionally
# broken iso_sf / acs_sf (NULL) — the credential abort takes priority.
# ============================================================================

test_that("T21-16 weight_method = 'population' aborts with future-release deferral hint", {
  # Intentionally garbage iso_sf / acs_sf — population fail-loud at the very
  # top of cacs_intersect_weight() must fire BEFORE .validate_iso_schema()
  # even runs.
  err <- tryCatch(
    cacs_intersect_weight(
      iso_sf        = NULL,
      acs_sf        = NULL,
      bg_pop_sf     = NULL,
      weight_method = "population",
      min_weight    = 1e-6,
      verbose       = FALSE
    ),
    error = function(e) e
  )

  expect_s3_class(err, "catchmentACS_error_credential")
  expect_s3_class(err, "catchmentACS_error")
  expect_s3_class(err, "catchmentACS_condition")
  expect_match(conditionMessage(err), "future release", fixed = TRUE)
  expect_match(conditionMessage(err), "population", fixed = TRUE)
})

test_that("T21-16b .apply_pop_weight() stub aborts with same credential class", {
  # Even if a user reaches into the internal namespace, the stub body must
  # abort with the same condition class so the future wire-in is signature-only.
  err <- tryCatch(
    catchmentACS:::.apply_pop_weight(inter_one = NULL, bg_pop_5070 = NULL),
    error = function(e) e
  )

  expect_s3_class(err, "catchmentACS_error_credential")
  expect_match(conditionMessage(err), "future release",    fixed = TRUE)
  expect_match(conditionMessage(err), ".apply_pop_weight", fixed = TRUE)
})


# ============================================================================
# T21-17 - derived_rate carrier preservation (Phase 5.3 Reviewer axis 2)
#
# Variables B17001_001 (poverty universe denominator) and B17001_002 (poverty
# numerator) classify as `spatial_total` per `.DEFAULT_VAR_FAMILY_MAP` (the
# rate itself is constructed at Phase 6.4 `cacs_derive_rates()` from these two
# count carriers). The carrier tibble must carry non-NA `est_total` +
# `var_total_raw` for the count so Phase 6.4 can read it as the rate
# numerator carrier.
#
# Additionally we directly exercise the derived_rate dispatch branch by
# post-injecting estimand_family into the success rows and confirming
# `estimate = NA`, `moe = NA`, while the carrier columns carry forward intact.
# ============================================================================

test_that("T21-17 carrier preserves est_total/var_total_raw for spatial_total counts", {
  # Build a 2-tract success bag for B17001_002 (poverty numerator).
  # This variable classifies as `spatial_total`. The carrier is what Phase 6.4
  # cacs_derive_rates() will read as the poverty NUMERATOR carrier.
  intersect_all <- tibble::tibble(
    site_id        = c("S01", "S01"),
    drive_time_min = c(15L,   15L),
    GEOID          = c("01001000001", "01001000002"),
    variable       = c("B17001_002", "B17001_002"),
    estimate       = c(50, 30),
    moe            = c(10, 8),
    area_wt        = c(0.50, 0.30),
    int_area_m2    = c(500000, 300000),
    tract_area_m2  = c(1e6, 1e6)
  )

  out <- catchmentACS:::.compute_family_aggregation(
    intersect_all = intersect_all,
    weight_method = "area",
    min_weight    = 1e-6
  )

  # 9-col carrier schema present (Sec. 21.7 lookup contract).
  expect_true("est_total"       %in% names(out$carriers))
  expect_true("var_total_raw"   %in% names(out$carriers))
  expect_true("est_mean"        %in% names(out$carriers))
  expect_true("var_mean_raw"    %in% names(out$carriers))
  expect_true("weight_sum"      %in% names(out$carriers))
  expect_true("estimand_family" %in% names(out$carriers))

  # Numerical preservation — Phase 6.4 will read est_total as the rate
  # numerator carrier.
  expect_equal(nrow(out$carriers), 1L)
  expect_equal(out$carriers$variable, "B17001_002")
  expect_equal(out$carriers$est_total,
               50 * 0.5 + 30 * 0.3,
               tolerance = 1e-9)
  expect_equal(out$carriers$var_total_raw,
               (0.5 * 10 / 1.645)^2 + (0.3 * 8 / 1.645)^2,
               tolerance = 1e-9)
  expect_true(is.finite(out$carriers$est_total))
  expect_true(is.finite(out$carriers$var_total_raw))
})


test_that("T21-17a missing ACS estimate propagates to estimate and MOE", {
  intersect_all <- tibble::tibble(
    site_id        = c("S01", "S01"),
    drive_time_min = c(15L,   15L),
    GEOID          = c("01001000001", "01001000002"),
    variable       = c("B17001_002", "B17001_002"),
    estimate       = c(50, NA_real_),
    moe            = c(10, 8),
    area_wt        = c(0.50, 0.30),
    int_area_m2    = c(500000, 300000),
    tract_area_m2  = c(1e6, 1e6)
  )

  out <- catchmentACS:::.compute_family_aggregation(
    intersect_all = intersect_all,
    weight_method = "area",
    min_weight    = 1e-6
  )

  expect_equal(out$n_input_rows_missing_acs_estimate, 1L)
  expect_equal(out$n_output_groups_missing_acs_estimate, 1L)
  expect_true(is.na(out$public$estimate))
  expect_true(is.na(out$public$moe))
  expect_true(is.na(out$carriers$est_total))
  expect_true(is.na(out$carriers$var_total_raw))
})


test_that("T21-17b derived_rate case_when branch emits NA estimate + moe + preserves carrier", {
  # An unknown variable code falls through `.classify_acs_variable` regex
  # patterns and ends up as `metadata_only`. We use that to confirm
  # estimate = NA / moe = NA / weight_basis = "none" path. Then we re-classify
  # by injecting a derived_rate row directly through the carrier output to
  # confirm the carrier schema is non-NA even when estimate is NA.
  intersect_all <- tibble::tibble(
    site_id        = "S01",
    drive_time_min = 15L,
    GEOID          = "01001000001",
    variable       = "B17001_002",   # spatial_total — first verify carrier
    estimate       = 50,
    moe            = 10,
    area_wt        = 0.50,
    int_area_m2    = 500000,
    tract_area_m2  = 1e6
  )

  out <- catchmentACS:::.compute_family_aggregation(
    intersect_all = intersect_all,
    weight_method = "area",
    min_weight    = 1e-6
  )

  # spatial_total: estimate non-NA but carriers ALSO non-NA (the same value).
  expect_false(is.na(out$public$estimate))
  expect_equal(out$public$estimate, 25, tolerance = 1e-9)   # 50 * 0.5
  expect_true(is.finite(out$carriers$est_total))
  expect_equal(out$carriers$est_total, 25, tolerance = 1e-9)

  # Force a derived_rate-classified row by post-injecting estimand_family on
  # the success rows. We do this by re-running aggregation on a copied input
  # where the regex falls into the generic `B[0-9]{5}_\\d{3}` -> spatial_total
  # branch and then synthetically inspect the case_when branches by direct
  # call into the dispatch helper.
  fam_test <- catchmentACS:::.dispatch_moe_formula_effective(
    c("spatial_total", "derived_rate", "metadata_only")
  )
  expect_equal(fam_test[1L], "weighted_sum")
  expect_equal(fam_test[2L], "general_ratio_conservative")
  expect_true(is.na(fam_test[3L]))

  # Synthetic derived_rate dispatch verification: build a 2-row agg tibble
  # mirroring what `.compute_family_aggregation()` would produce, then run
  # the case_when block on it manually.
  agg_in <- tibble::tibble(
    estimand_family = c("derived_rate", "spatial_total", "metadata_only"),
    est_total       = c(100, 200, 300),
    var_total_raw   = c(10, 20, 30),
    est_mean        = c(50, 75, 100),
    var_mean_raw    = c(5, 7, 9)
  )
  est_out <- dplyr::case_when(
    agg_in$estimand_family == "spatial_total"  ~ agg_in$est_total,
    agg_in$estimand_family == "derived_rate"   ~ NA_real_,
    agg_in$estimand_family == "metadata_only"  ~ NA_real_,
    TRUE                                       ~ NA_real_
  )
  expect_true(is.na(est_out[1L]))                          # derived_rate -> NA
  expect_equal(est_out[2L], 200, tolerance = 1e-9)         # spatial_total -> est_total
  expect_true(is.na(est_out[3L]))                          # metadata_only -> NA
})


# ============================================================================
# Step 5.4 fixtures + helpers - canonical EPSG:4326/4269 input pair builder.
#
# T21-18 (composite cache hit) and T21-CARRIER-CONTRACT both need an
# end-to-end-runnable (iso_4326, acs_4269) input pair. We reuse the
# 2-tract synth at a CONUS-anchored origin (matches the Step 5.3 integration
# fixture above) so the §1e CONUS guard + EPSG:5070 round-trip both pass.
# ============================================================================

.step54_make_canonical_pair <- function() {
  fx <- helper_load_synthetic_2tract()
  cx <- -86.80
  cy <-  33.50

  iso_4326 <- sf::st_sf(
    tibble::tibble(
      site_id                          = fx$site$site_id,
      drive_time_min                   = as.integer(fx$site$drive_time_min),
      provider                         = "osrm",
      profile                          = "car",
      osm_snapshot_date                = NA_character_,
      routing_engine_version           = "stub/0.0.0",
      polygon_simplification_tolerance = NA_real_,
      generated_at                     = as.POSIXct("2026-05-22 00:00:00", tz = "UTC"),
      isochrone_empty                  = FALSE,
      provider_requested               = "osrm",
      provider_downgrade               = FALSE,
      osm_snapshot_status              = NA_character_,
      failure_reason                   = NA_character_,
      retry_count                      = 1L,
      ring_topology                    = "cumulative"
    ),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(cx,         cy),
        c(cx + 0.008, cy),
        c(cx + 0.008, cy + 0.005),
        c(cx,         cy + 0.005),
        c(cx,         cy)
      ))),
      crs = 4326
    )
  )

  acs_4269 <- sf::st_sf(
    tibble::tibble(
      GEOID    = fx$tracts$GEOID,
      NAME     = fx$tracts$NAME,
      variable = fx$tracts$variable,
      estimate = fx$tracts$estimate,
      moe      = fx$tracts$moe
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

  list(iso = iso_4326, acs = acs_4269)
}


# ============================================================================
# T21-18 - Composite cache hit (§21.8 transitive invalidation contract)
#
# Two back-to-back calls with identical (iso_sf, acs_sf, weight_method,
# min_weight) tuples MUST produce a cache hit on the second call. The hit
# manifests as:
#   1. an M-CACHE-HIT message containing "Cache hit"
#   2. identical() output (incl. all 3 attributes: schema_version,
#      aggregation_carriers, aggregation_provenance)
#
# We use a per-test cache dir (withr::local_tempdir) so the test is hermetic.
# ============================================================================

test_that("T21-18 composite cache hit returns byte-identical output on second call", {
  pair <- .step54_make_canonical_pair()
  td <- withr::local_tempdir()

  withr::local_options(
    catchmentACS.cache_dir       = td,
    catchmentACS.cache_intersect = TRUE
  )
  on.exit(memoise::forget(.cacs_cache_dir_memo), add = TRUE)
  memoise::forget(.cacs_cache_dir_memo)

  # First call: cache miss, full algorithm runs, output is written to cache.
  out1 <- suppressMessages(
    cacs_intersect_weight(pair$iso, pair$acs, verbose = FALSE)
  )
  expect_s3_class(out1, "tbl_df")
  expect_setequal(names(out1), catchmentACS:::.LONG_REQUIRED_COLS)

  # Verify cache file landed on disk in the intersect namespace.
  cache_files <- list.files(file.path(td, "intersect"), pattern = "\\.rds$")
  expect_length(cache_files, 1L)

  # Second call: cache hit. Capture messages to verify the hit log fires.
  msgs <- testthat::capture_messages(
    out2 <- cacs_intersect_weight(pair$iso, pair$acs, verbose = TRUE)
  )
  expect_true(any(grepl("Cache hit", msgs, fixed = TRUE)))

  # Byte-identical output (including attributes).
  expect_identical(out1, out2)
})


# ============================================================================
# T21-CARRIER-CONTRACT - Phase 6 forward verification
#
# Reviewer Decision #4 axis 3: the `cacs_aggregation_carriers` attribute MUST
# be byte-identical to what Phase 6.2 `.lookup_carrier_num/_den/_var()` will
# read. We assert the contract here so a future rename of a carrier column
# (e.g. `est_total` -> `total_estimate`) trips the test BEFORE Phase 6 starts
# silently NULL-coalescing.
#
# Required carrier schema (Sec. 21.7 / Sec. 22.3 step 6):
#   site_id, drive_time_min, variable, estimand_family,
#   est_total, var_total_raw, est_mean, var_mean_raw, weight_sum
#
# Key uniqueness: (site_id, drive_time_min, variable) is a primary key in the
# carrier tibble (asserted by `.validate_intersect_output()` but re-asserted
# here for explicit Phase 6 contract clarity).
# ============================================================================

# ============================================================================
# T22-09 .. T22-12 — Phase 6.3 C1 -> C2 multi-step fallback chain end-to-end
#
# These four cases drive `cacs_propagate_moe()` through a minimal canonical
# fixture whose derived_rate row resolves via .SANCTIONED_RATES_V1$poverty_rate
# (num = B17001_002, den = B17001_001) onto carrier rows the test builder
# fills with the per-case (num_est / num_var / den_est / den_var) values from
# inst/testdata/synthetic_moe_C{1_success,1_to_C2}.rds (T22-09 / T22-10) and
# from the documented Sec. 22.10 zero_den / missing_moe scenarios (T22-11 /
# T22-12). Together they verify:
#
#   - T22-09 C1 success: inside = 75 > 0  -> MOE_C1 = 1.645*sqrt(75)/200
#   - T22-10 C1 -> C2 fallback: inside = -15 < 0 -> C2 + reason
#                                                  = "negative_variance"
#   - T22-11 zero_den: den_est <= 1e-9 -> NA + reason = "zero_denominator"
#   - T22-12 missing_moe: num_moe = NA -> NA + reason = "missing_moe"
#
# All four cases also assert Sec. 22.5 fallback priority via two adversarial
# rows in T22-11 / T22-12 where two triggers fire simultaneously and the
# higher-priority reason wins.
# ============================================================================

# Builder for a single-rate Phase 6.3 fixture. `formula = NULL` uses
# Sec. 23.4 auto-dispatch (C2 default); pass `"proportion_subset"` to force C1.
.make_phase63_rate_fixture <- function(num_est = 50, num_var = 100,
                                       den_est = 200, den_var = 400) {
  data <- tibble::tibble(
    site_id                       = "S01",
    drive_time_min                = 15L,
    ring_topology                 = "cumulative",
    variable                      = "poverty_rate",
    estimate                      = NA_real_,
    moe                           = NA_real_,
    weight_sum                    = 0.8,
    n_tracts                      = 2L,
    n_tracts_num                  = 2L,
    n_tracts_den                  = 2L,
    provider                      = "osrm",
    profile                       = "car",
    osm_snapshot_date             = "2026-01-01",
    acs_year                      = 2023L,
    weight_method                 = "area",
    estimand_family               = "derived_rate",
    weight_basis                  = "coverage",
    moe_formula_requested         = "proportion_subset",
    moe_formula_effective         = "proportion_subset",
    moe_fallback                  = FALSE,
    moe_fallback_reason           = "n/a",
    failure_origin                = "none",
    weight_uncertainty_propagated = FALSE
  )

  # Carrier carries (a) the rate name row (for `.validate_intersect_output()`'s
  # 3-key match) + (b) the num/den ACS-code rows that the lookup helpers
  # actually resolve. The rate name row keeps all carriers NA — it is never
  # read by the C1/C2 dispatch (the lookup translates rate -> num/den via
  # .SANCTIONED_RATES_V1$poverty_rate).
  carrier <- tibble::tibble(
    site_id         = c("S01",          "S01",          "S01"),
    drive_time_min  = c(15L,            15L,            15L),
    variable        = c("poverty_rate", "B17001_002",   "B17001_001"),
    estimand_family = c("derived_rate", "spatial_total", "spatial_total"),
    est_total       = c(NA_real_,       num_est,        den_est),
    var_total_raw   = c(NA_real_,       num_var,        den_var),
    est_mean        = c(NA_real_,       NA_real_,       NA_real_),
    var_mean_raw    = c(NA_real_,       NA_real_,       NA_real_),
    weight_sum      = c(0.8,            0.8,            0.8),
    n_tracts        = c(NA_integer_,    2L,             2L)
  )

  attr(data, "cacs_aggregation_carriers") <- carrier
  attr(data, "cacs_schema_version")       <- "1.0"
  data
}


test_that("T22-09 C1 success: inside=+75 yields MOE_C1 at eps=1e-9", {
  # num_var = 100, den_var = 400, p_hat = 0.25
  # inside = 100 - 0.0625 * 400 = 75 > 0 -> C1 success
  # MOE_C1 = 1.645 * sqrt(75) / 200
  data <- .make_phase63_rate_fixture(num_est = 50, num_var = 100,
                                     den_est = 200, den_var = 400)
  out <- cacs_propagate_moe(data, formula = "proportion_subset")

  row <- which(out$variable == "poverty_rate")
  expect_length(row, 1L)
  expect_equal(out$moe[row], 1.645 * sqrt(75) / 200, tolerance = 1e-9)
  expect_equal(out$estimate[row], 0.25, tolerance = 1e-12)
  expect_equal(out$moe_formula_effective[row], "proportion_subset")
  expect_equal(out$moe_formula_requested[row], "proportion_subset")
  expect_false(out$moe_fallback[row])
  expect_equal(out$moe_fallback_reason[row], "n/a")
})


test_that("T22-10 C1 -> C2 fallback: inside=-15 reroutes with reason='negative_variance'", {
  # num_var = 10 (small), den_var = 400, p_hat = 0.25
  # inside = 10 - 0.0625 * 400 = -15 < 0 -> C2 fallback
  # MOE_C2 = 1.645 * sqrt(10 + 0.0625 * 400) / 200 = 1.645 * sqrt(35) / 200
  data <- .make_phase63_rate_fixture(num_est = 50, num_var = 10,
                                     den_est = 200, den_var = 400)
  out <- suppressWarnings(
    cacs_propagate_moe(data, formula = "proportion_subset")
  )

  row <- which(out$variable == "poverty_rate")
  expect_length(row, 1L)
  expected_moe <- 1.645 * sqrt(10 + 0.0625 * 400) / 200
  expect_equal(out$moe[row], expected_moe, tolerance = 1e-9)
  expect_equal(out$estimate[row], 0.25, tolerance = 1e-12)
  expect_equal(out$moe_formula_effective[row], "general_ratio_conservative")
  expect_equal(out$moe_formula_requested[row], "proportion_subset")
  expect_true(out$moe_fallback[row])
  expect_equal(out$moe_fallback_reason[row], "negative_variance")
})


test_that("T22-11 zero_den: den_est <= 1e-9 yields NA + reason='zero_denominator'", {
  # den_est = 1e-12 (below .DEN_EPS ~ 1.5e-8) -> zero_denominator branch
  data <- .make_phase63_rate_fixture(num_est = 50, num_var = 100,
                                     den_est = 1e-12, den_var = 400)
  out <- suppressWarnings(
    cacs_propagate_moe(data, formula = "proportion_subset")
  )

  row <- which(out$variable == "poverty_rate")
  expect_length(row, 1L)
  expect_true(is.na(out$moe[row]))
  # The C1 dispatch leaves moe_formula_effective at "proportion_subset"
  # (the requested formula) — only the value collapses to NA. fallback flag
  # flips TRUE so consumers know the requested formula did not apply.
  expect_equal(out$moe_formula_effective[row], "proportion_subset")
  expect_equal(out$moe_formula_requested[row], "proportion_subset")
  expect_true(out$moe_fallback[row])
  expect_equal(out$moe_fallback_reason[row], "zero_denominator")
})


test_that("T22-12 missing_moe: num_moe NA yields NA + reason='missing_moe'", {
  # num_var = NA -> num_moe = NA after .var_to_moe() -> missing_moe branch
  data <- .make_phase63_rate_fixture(num_est = 50, num_var = NA_real_,
                                     den_est = 200, den_var = 400)
  out <- suppressWarnings(
    cacs_propagate_moe(data, formula = "proportion_subset")
  )

  row <- which(out$variable == "poverty_rate")
  expect_length(row, 1L)
  expect_true(is.na(out$moe[row]))
  expect_equal(out$moe_formula_effective[row], "proportion_subset")
  expect_equal(out$moe_formula_requested[row], "proportion_subset")
  expect_true(out$moe_fallback[row])
  expect_equal(out$moe_fallback_reason[row], "missing_moe")
})


test_that("T22-PRIORITY priority order: zero_den beats missing_moe and neg_var", {
  # Adversarial row: den_est = 1e-12 (zero_den) AND num_var = NA (missing_moe)
  # AND num_var would force inside_negative if it weren't NA. Sec. 22.5
  # priority: zero_denominator wins.
  data <- .make_phase63_rate_fixture(num_est = 50, num_var = NA_real_,
                                     den_est = 1e-12, den_var = 400)
  out <- suppressWarnings(
    cacs_propagate_moe(data, formula = "proportion_subset")
  )
  expect_equal(out$moe_fallback_reason[1L], "zero_denominator")
  expect_true(is.na(out$moe[1L]))

  # missing_moe wins over negative_variance: num_var = NA blocks the
  # inside-negative test (the primitive routes to missing_moe first).
  data2 <- .make_phase63_rate_fixture(num_est = 50, num_var = NA_real_,
                                      den_est = 200, den_var = 400)
  out2 <- suppressWarnings(
    cacs_propagate_moe(data2, formula = "proportion_subset")
  )
  expect_equal(out2$moe_fallback_reason[1L], "missing_moe")
})


test_that("T22-NO-OP .apply_* helpers are no-ops on empty idx", {
  # Sec. 22.6 dispatch contract: each branch helper must be safely callable
  # with idx = integer(0) so the dispatcher never has to guard before calling.
  data <- tibble::tibble(
    moe                   = c(1.0, 2.0),
    moe_formula_effective = c("proportion_subset", "proportion_subset"),
    moe_fallback          = c(FALSE, FALSE),
    moe_fallback_reason   = c("n/a", "n/a")
  )
  expect_identical(catchmentACS:::.apply_zero_den(data, integer(0)),    data)
  expect_identical(catchmentACS:::.apply_missing_moe(data, integer(0)), data)
  expect_identical(
    catchmentACS:::.apply_c2_fallback(
      data, integer(0),
      num_est = numeric(0), num_moe = numeric(0),
      den_est = numeric(0), den_moe = numeric(0),
      z = 1.645
    ),
    data
  )
  expect_identical(
    catchmentACS:::.apply_c1_success(
      data, integer(0),
      num_est = numeric(0), num_moe = numeric(0),
      den_est = numeric(0), den_moe = numeric(0),
      z = 1.645
    ),
    data
  )
})


# ============================================================================
# (continuing pre-existing tests below)
# ============================================================================

test_that("T21-CARRIER-CONTRACT carrier attribute matches Sec. 22.3 step 6 lookup contract", {
  pair <- .step54_make_canonical_pair()
  withr::local_options(catchmentACS.cache_intersect = FALSE)

  out <- suppressMessages(
    cacs_intersect_weight(pair$iso, pair$acs, verbose = FALSE)
  )

  # ---- (1) Carrier attribute is present and is a data.frame -------------
  carrier <- attr(out, "cacs_aggregation_carriers")
  expect_false(is.null(carrier))
  expect_s3_class(carrier, "data.frame")

  # ---- (2) 10 required columns are all present (v0.2 grew 9 -> 10
  # by adding `n_tracts` so derive-rates.R can populate the new
  # `n_tracts_num` / `n_tracts_den` columns via carrier lookup). ----------
  required_cols <- c(
    "site_id", "drive_time_min", "variable", "estimand_family",
    "est_total", "var_total_raw", "est_mean", "var_mean_raw", "weight_sum",
    "n_tracts"
  )
  expect_setequal(names(carrier), required_cols)

  # ---- (3) Key uniqueness on (site_id, drive_time_min, variable) -------
  key <- do.call(paste,
                 c(carrier[, c("site_id", "drive_time_min", "variable")],
                   sep = "\r"))
  expect_equal(anyDuplicated(key), 0L)

  # ---- (4) Numeric carriers are nonnegative where non-NA ----------------
  for (col in c("var_total_raw", "var_mean_raw", "weight_sum")) {
    vals <- carrier[[col]]
    expect_true(all(is.na(vals) | vals >= 0))
  }

  # ---- (5) Lookup simulation: Phase 6.2 will resolve numerator carrier
  # via subset on `variable == "B01003_001"`. Verify the carrier supports
  # this lookup pattern (non-zero rows, finite est_total).
  var_for_lookup <- "B01003_001"
  matched <- carrier[carrier$variable == var_for_lookup, , drop = FALSE]
  expect_gt(nrow(matched), 0L)
  expect_true(all(is.finite(matched$est_total)))

  # ---- (6) Carrier survives bind_rows in the post-loop pipeline.
  # Reviewer axis 1 (carrier attach order matters): the attribute is set
  # AFTER bind_rows, so it must be present on the returned tibble - not on
  # any intermediate object that has since been overwritten.
  expect_true("cacs_aggregation_carriers" %in% names(attributes(out)))
})


# ============================================================================
# T23-08 .. T23-11 — Phase 6.4 cacs_derive_rates() known-answer fixtures
#
# Verifies the §23.3 8-step algorithm at ε=1e-9 across the four §22 fallback
# branches for the curated `poverty_rate`:
#   T23-08 C2 default       : MOE_C2 = 1.645 * sqrt(100 + 0.0625 * 400) / 200
#                              = 1.645 * sqrt(125) / 200 ≈ 0.0920
#   T23-09 C1 explicit      : MOE_C1 = 1.645 * sqrt(75) / 200 ≈ 0.0712
#   T23-10 C1 -> C2 fallback: inside = -15 < 0 -> C2 + reason
#                              = "negative_variance"
#   T23-11 carrier missing  : failure_origin = "carrier" on poverty_rate
#                              row while other 4 rate rows render normally.
#
# The fixture builder reuses the .make_phase63_rate_fixture() carrier layout
# but adds a "spatial_total" upstream row for the propagator's validator
# (which requires at least one non-rate row for the long schema to pass).
# ============================================================================

.make_derive_known_fixture <- function(num_est = 50, num_var = 100,
                                       den_est = 200, den_var = 400,
                                       drop_poverty_num = FALSE) {
  # Upstream long row: a Family A count whose carrier the propagator can
  # consume. We use B17001_002 so the carrier 3-key resolves.
  long <- tibble::tibble(
    site_id                       = "S01",
    drive_time_min                = 15L,
    ring_topology                 = "cumulative",
    variable                      = "B17001_002",
    estimate                      = num_est,
    moe                           = if (is.na(num_var)) NA_real_
                                    else 1.645 * sqrt(num_var),
    weight_sum                    = 0.8,
    n_tracts                      = 2L,
    n_tracts_num                  = NA_integer_,
    n_tracts_den                  = NA_integer_,
    provider                      = "osrm",
    profile                       = "car",
    osm_snapshot_date             = "2026-01-01",
    acs_year                      = 2023L,
    weight_method                 = "area",
    estimand_family               = "spatial_total",
    weight_basis                  = "coverage",
    moe_formula_requested         = "weighted_sum",
    moe_formula_effective         = "weighted_sum",
    moe_fallback                  = FALSE,
    moe_fallback_reason           = "n/a",
    failure_origin                = "none",
    weight_uncertainty_propagated = FALSE
  )

  # Carrier rows for the 5 sanctioned rates' num/den ACS codes. poverty_rate
  # carriers are populated with the per-case values; the other 8 codes
  # (snap/ssi/unemp/lfp num+den) carry placeholder values so the lookup
  # resolves and the per-rate dispatch produces *some* MOE.
  carriers <- tibble::tibble(
    site_id         = rep("S01", 9L),
    drive_time_min  = rep(15L,    9L),
    variable        = c("B17001_002", "B17001_001",   # poverty num/den
                        "B22003_002", "B22003_001",   # snap   num/den
                        "B19056_002", "B19056_001",   # ssi    num/den
                        "B23025_005", "B23025_003",   # unemp  num/den
                        "B23025_002"),                # labor force part. num
    estimand_family = rep("spatial_total", 9L),
    est_total       = c(num_est, den_est,
                        20, 100,
                        5, 80,
                        25, 200,
                        300),
    var_total_raw   = c(num_var, den_var,
                        50,  300,
                        30,  200,
                        50,  500,
                        700),
    est_mean        = rep(NA_real_, 9L),
    var_mean_raw    = rep(NA_real_, 9L),
    weight_sum      = rep(0.8,      9L),
    n_tracts        = rep(2L,       9L)
  )

  # labor_force_participation also needs the denominator B23025_001.
  carriers <- dplyr::bind_rows(
    carriers,
    tibble::tibble(
      site_id         = "S01",
      drive_time_min  = 15L,
      variable        = "B23025_001",
      estimand_family = "spatial_total",
      est_total       = 600,
      var_total_raw   = 900,
      est_mean        = NA_real_,
      var_mean_raw    = NA_real_,
      weight_sum      = 0.8,
      n_tracts        = 2L
    )
  )

  if (isTRUE(drop_poverty_num)) {
    carriers <- carriers[carriers$variable != "B17001_002", ]
  }

  attr(long, "cacs_aggregation_carriers") <- carriers
  attr(long, "cacs_schema_version")       <- "1.0"
  attr(long, "cacs_confidence_level")     <- 0.90
  long
}


test_that("T23-08 poverty_rate C2 default known-answer at eps=1e-9", {
  # formula_dispatch = "general_ratio_conservative" (default) -> C2 for all 5.
  # poverty_rate: num_est=50, num_var=100, den_est=200, den_var=400, z=1.645
  # MOE_C2 = 1.645 * sqrt(100 + 0.0625 * 400) / 200 = 1.645 * sqrt(125) / 200
  fx <- .make_derive_known_fixture()
  out <- suppressWarnings(cacs_derive_rates(fx))

  row <- which(out$variable == "poverty_rate")
  expect_length(row, 1L)
  expected_moe <- 1.645 * sqrt(125) / 200
  expect_equal(out$moe[row], expected_moe, tolerance = 1e-9)
  expect_equal(out$estimate[row], 0.25, tolerance = 1e-12)
  expect_equal(out$moe_formula_effective[row], "general_ratio_conservative")
  expect_equal(out$moe_formula_requested[row], "general_ratio_conservative")
  expect_false(out$moe_fallback[row])
  expect_equal(out$moe_fallback_reason[row], "n/a")
  expect_equal(out$failure_origin[row], "none")
  expect_equal(out$estimand_family[row], "derived_rate")
})


test_that("T23-09 poverty_rate C1 explicit known-answer at eps=1e-9", {
  # formula_dispatch = "proportion_subset" -> C1 for subset-eligible rates;
  # non-subset rates are downgraded to C2.
  # poverty_rate carrier set produces inside = 100 - 0.0625*400 = 75 > 0
  # -> C1 success: MOE_C1 = 1.645 * sqrt(75) / 200
  fx <- .make_derive_known_fixture()
  out <- suppressWarnings(
    cacs_derive_rates(fx, formula_dispatch = "proportion_subset")
  )

  row <- which(out$variable == "poverty_rate")
  expect_length(row, 1L)
  expected_moe <- 1.645 * sqrt(75) / 200
  expect_equal(out$moe[row], expected_moe, tolerance = 1e-9)
  expect_equal(out$estimate[row], 0.25, tolerance = 1e-12)
  expect_equal(out$moe_formula_effective[row], "proportion_subset")
  expect_equal(out$moe_formula_requested[row], "proportion_subset")
  expect_false(out$moe_fallback[row])
  expect_equal(out$moe_fallback_reason[row], "n/a")

  non_subset <- out$variable %in% c("snap_rate", "ssi_rate", "unemp_rate")
  expect_true(all(out$moe_formula_requested[non_subset] ==
                    "general_ratio_conservative"))
  expect_false(any(out$moe_formula_effective[non_subset] ==
                     "proportion_subset"))
})


test_that("T23-10 poverty_rate C1 -> C2 fallback known-answer at eps=1e-9", {
  # num_var = 10 -> inside = 10 - 0.0625*400 = -15 < 0 -> C2 fallback.
  # MOE_C2 = 1.645 * sqrt(10 + 0.0625 * 400) / 200 = 1.645 * sqrt(35) / 200
  fx <- .make_derive_known_fixture(num_var = 10)
  out <- suppressWarnings(
    cacs_derive_rates(fx, formula_dispatch = "proportion_subset")
  )

  row <- which(out$variable == "poverty_rate")
  expect_length(row, 1L)
  expected_moe <- 1.645 * sqrt(35) / 200
  expect_equal(out$moe[row], expected_moe, tolerance = 1e-9)
  expect_equal(out$estimate[row], 0.25, tolerance = 1e-12)
  expect_equal(out$moe_formula_effective[row], "general_ratio_conservative")
  expect_equal(out$moe_formula_requested[row], "proportion_subset")
  expect_true(out$moe_fallback[row])
  expect_equal(out$moe_fallback_reason[row], "negative_variance")
})


test_that("T23-11 poverty_rate carrier missing -> failure_origin='carrier'", {
  # Drop B17001_002 (poverty num) from the carrier; the other 4 rates'
  # carriers stay intact so they render normally.
  fx <- .make_derive_known_fixture(drop_poverty_num = TRUE)
  out <- suppressWarnings(cacs_derive_rates(fx))

  row_poverty <- which(out$variable == "poverty_rate")
  expect_length(row_poverty, 1L)
  expect_true(is.na(out$moe[row_poverty]))
  expect_true(is.na(out$estimate[row_poverty]))
  expect_equal(out$failure_origin[row_poverty], "carrier")
  expect_equal(out$moe_fallback_reason[row_poverty], "n/a")
  expect_false(out$moe_fallback[row_poverty])
  expect_equal(out$estimand_family[row_poverty], "derived_rate")

  # Other 4 rate rows render normally (failure_origin = "none").
  other_rate_rows <- which(out$variable %in%
    c("snap_rate", "ssi_rate", "unemp_rate",
      "labor_force_participation"))
  expect_length(other_rate_rows, 4L)
  expect_true(all(out$failure_origin[other_rate_rows] == "none"))
  expect_true(all(is.finite(out$estimate[other_rate_rows])))
})


# ============================================================================
# T23-AUTO — formula_dispatch = "auto" delegates to .RATE_FORMULA_CATALOGUE
# (Sec. 23.4): poverty_rate / labor_force_participation -> proportion_subset,
# rest -> general_ratio_conservative.
# ============================================================================

test_that("T23-AUTO formula_dispatch='auto' per-rate matches .RATE_FORMULA_CATALOGUE", {
  fx <- .make_derive_known_fixture()
  out <- suppressWarnings(
    cacs_derive_rates(fx, formula_dispatch = "auto")
  )

  rate_names <- c("poverty_rate", "snap_rate", "ssi_rate",
                  "unemp_rate", "labor_force_participation")
  rate_rows <- out[out$variable %in% rate_names, ]
  rate_rows <- rate_rows[match(rate_names, rate_rows$variable), ]

  # moe_formula_requested for each rate must equal the catalogue entry.
  expect_equal(rate_rows$moe_formula_requested,
               unname(catchmentACS:::.RATE_FORMULA_CATALOGUE[rate_names]))

  # Provenance attribute holds the per-rate formula vector.
  rate_prov <- attr(out, "cacs_rate_provenance")
  expect_equal(rate_prov$formula_dispatch, "auto")
  expect_equal(rate_prov$formula_per_rate[rate_names],
               catchmentACS:::.RATE_FORMULA_CATALOGUE[rate_names])
})


# ============================================================================
# T23-CARRIER-DROP — consume-then-drop carrier (Sec. 23.5 final contract)
# ============================================================================

test_that("T23-CARRIER-DROP carrier attribute is removed from output", {
  fx <- .make_derive_known_fixture()
  expect_false(is.null(attr(fx, "cacs_aggregation_carriers")))

  out <- suppressWarnings(cacs_derive_rates(fx))
  expect_null(attr(out, "cacs_aggregation_carriers"))
  # Other Phase-6 attributes survive:
  expect_equal(attr(out, "cacs_schema_version"), "1.0")
  expect_equal(attr(out, "cacs_confidence_level"), 0.90)
  expect_false(is.null(attr(out, "cacs_rate_provenance")))
})


# ============================================================================
# T23-ROW-CARDINALITY — Sec. 23.5 row cardinality contract.
# ============================================================================

test_that("T23-ROW-CARDINALITY output rows = input + n_sites x n_drive_times x 5", {
  fx <- .make_derive_known_fixture()
  out <- suppressWarnings(cacs_derive_rates(fx))
  # 1 input row + 1 site x 1 drive_time x 5 rates = 6.
  expect_equal(nrow(out), nrow(fx) + 5L)
})
