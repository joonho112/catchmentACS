# test-regression-2025-alabama.R
#
# Synthetic golden acceptance: 540 area-only rows must match the frozen 2025
# AL Pre-K golden within epsilon = 1e-6 (estimate) / 1e-5 (moe) /
# 1e-9 (weight_sum). Per Sec. 26.5 + Sec. 26.6 + Sec. 43.4.
#
# Activation: this test is gated by `skip_if_not_regression()` and runs
# only when `Sys.getenv("CACS_REGRESSION") == "true"`. PR jobs skip it;
# main-branch push, nightly cron, and manual `gh workflow run regression.yml`
# activate it (Sec. 16.3 CI tier pyramid).
#
# Synthetic-golden note: per Step 8.1 README, the frozen golden
# was produced by running `cacs_run()` end-to-end on the synthetic 20-site
# input + synthetic isochrones + synthetic AL ACS fixture. The regression
# test therefore validates *idempotence + schema invariance* of the
# canonical pipeline -- re-running with the same frozen inputs must
# byte-equivalently reproduce the frozen output. A future production-grade
# regeneration can replace the synthetic baseline with real ground truth,
# at which point the same test gains semantic algorithmic-equivalence value.
#
# v0.5.0 schema-growth note: the frozen golden is a 20-col historical
# synthetic tibble; current production output is 27 cols (mandatory schema grew
# with `n_tracts_num`, `n_tracts_den`, and `ring_topology`, and
# `cacs_propagate_moe()` carries 4 carrier numeric columns for MOE math).
# The composite-key inner_join below is robust to the column
# delta -- non-conflicting columns pass through unsuffixed and are
# simply not asserted against the golden. The schema-delta assertion
# block (Step 5 below) locks the 27 -> 20 ASYMMETRY explicitly so a
# future regression run that re-collapses to an older layout fails loud rather
# than silently passing the per-column tolerance checks.
#
# On failure, dispatch to Sec. 26.7 five-step drift triage:
#   Step 1: classify origin (algorithm bug / toolchain drift / golden
#           bit-rot / spec evolution)
#   Step 2: collect evidence (git log on R/, sf_extSoftVersion(), fixture
#           sha256, recent spec-table commits)
#   Step 3: per-row drill-down on the first violating (site, drive, var)
#   Step 4: resolution branch (fix PR vs epsilon recalibration vs fixture
#           repair vs regeneration PR)
#   Step 5: document the triage outcome + re-run

.c10_urbanicity_reference <- function() {
  tibble::tibble(
    site_id = c("AL_ANCHOR_03", "AL_ANCHOR_02", "AL_ANCHOR_01"),
    site_label = c("Urban anchor site", "Suburban anchor site",
                   "Rural anchor site"),
    urbanicity = c("URBAN", "MID", "RURAL"),
    area_band_pct = c(6, 16, 60)
  )
}


test_that("C10 per-urbanicity area bands preserve tract-membership invariant", {
  fixture <- load_replay_fixture("031_3site_anchors")
  iso <- fixture$iso_sf_v2
  ref <- .c10_urbanicity_reference() |>
    dplyr::left_join(fixture$cmp1a_isochrone_area |>
                       dplyr::select(site_id, area_2025_km2),
                     by = "site_id") |>
    dplyr::left_join(fixture$cmp3_seam_jaccard |>
                       dplyr::select(site_id, seam_jaccard = jaccard),
                     by = "site_id")

  areas <- iso |>
    sf::st_transform(5070)
  areas$area_cacs_km2 <- as.numeric(sf::st_area(areas)) / 1e6
  cmp <- areas |>
    sf::st_drop_geometry() |>
    dplyr::select(site_id, area_cacs_km2) |>
    dplyr::inner_join(ref, by = "site_id") |>
    dplyr::mutate(
      pct_diff = abs(area_cacs_km2 - area_2025_km2) / area_2025_km2 * 100
    )

  testthat::expect_equal(nrow(cmp), 3L)
  testthat::expect_true(all(!is.na(cmp$area_2025_km2)))
  testthat::expect_true(all(!is.na(cmp$seam_jaccard)))
  testthat::expect_true(all(cmp$pct_diff <= cmp$area_band_pct),
                        info = paste(cmp$site_id, round(cmp$pct_diff, 2),
                                     cmp$urbanicity, collapse = "; "))
  testthat::expect_true(all(cmp$seam_jaccard >= 0.95))

  rural_site <- cmp[cmp$site_id == "AL_ANCHOR_01", ]
  testthat::expect_equal(rural_site$urbanicity, "RURAL")
  testthat::expect_gt(rural_site$pct_diff, 30)
  testthat::expect_lte(rural_site$pct_diff, 60)
  testthat::expect_equal(rural_site$seam_jaccard, 1)
})


test_that("cacs_run reproduces frozen 2025 AL Pre-K golden (area, 540 rows)", {
  skip_on_cran()
  skip_if_not_regression()

  # ------------------------------------------------------------------------
  # Step 1: load frozen inputs (network-free, full bypass mode)
  # ------------------------------------------------------------------------
  golden <- readRDS(system.file("extdata", "legacy_2025_golden_output.rds",
                                package = "catchmentACS"))
  iso    <- readRDS(system.file("extdata", "legacy_2025_isochrones.rds",
                                package = "catchmentACS"))
  acs_raw <- readRDS(system.file("extdata", "sample_alabama_subset.rds",
                                 package = "catchmentACS"))
  sites  <- readRDS(system.file("extdata", "legacy_2025_sites.rds",
                                package = "catchmentACS"))

  # Sanity: fixtures loaded with expected shape
  testthat::expect_equal(nrow(sites),  20L)
  testthat::expect_equal(nrow(iso),    60L)
  testthat::expect_equal(nrow(golden), 1080L)  # full per Decision #6
  testthat::expect_true(all(golden$moe_fallback_reason %in%
                              c("n/a", "negative_variance",
                                "zero_denominator", "missing_moe")))
  testthat::expect_true(all(golden$failure_origin %in%
                              c("none", "intersection", "isochrone",
                                "acs", "carrier")))

  prov <- attr(golden, "provenance")
  testthat::expect_type(prov, "list")
  testthat::expect_equal(prov$population_stub_status,
                         "v0.1_population_path_inactive")
  testthat::expect_equal(prov$acs_missing_estimate_policy,
                         "fail_safe_propagate_na")
  testthat::expect_match(prov$script_hash_sha256, "^[0-9a-f]{64}$")
  testthat::expect_true(all(vapply(prov$input_hashes_sha256,
                                   function(x) is.na(x) || grepl("^[0-9a-f]{64}$", x),
                                   logical(1))))

  # v0.1 area-only subset of the full golden (Decision #6 + Sec. 43.4)
  golden_area <- dplyr::filter(golden, weight_method == "area")
  testthat::expect_equal(nrow(golden_area), 540L)

  # The golden preserves the v0.1 fail-safe missing-estimate policy: suppressed
  # ACS estimates remain NA and aggregate outputs propagate NA rather than
  # imputing synthetic medians.
  acs <- acs_raw

  # ------------------------------------------------------------------------
  # Step 2: run pipeline with frozen inputs (bypass mode, output = "long")
  # ------------------------------------------------------------------------
  result <- cacs_run(
    sites                  = sites,
    state                  = "AL",
    year                   = 2023,
    drive_times            = c(5L, 10L, 15L),
    variables              = unname(cacs_acs_default_vars),
    provider               = "osrm",
    weight_method          = "area",
    precomputed_isochrones = iso,
    acs                    = acs,
    rates                  = cacs_acs_default_rates,
    output                 = "long",
    verbose                = FALSE
  )

  # Filter result to the 9 regression output variables (= 540 rows)
  regression_output_vars <- c(
    "B01003_001", "B17001_001", "B17001_002",
    "B19013_001", "B19301_001",
    "B22003_001", "B22003_002",
    "poverty_rate", "snap_rate"
  )
  result_area <- result |>
    dplyr::filter(variable %in% regression_output_vars)
  testthat::expect_equal(nrow(result_area), 540L)

  # ------------------------------------------------------------------------
  # Step 3: align result to golden by composite key (4-tuple)
  # ------------------------------------------------------------------------
  joined <- result_area |>
    dplyr::inner_join(
      golden_area,
      by     = c("site_id", "drive_time_min", "weight_method", "variable"),
      suffix = c("_pkg", "_golden")
    ) |>
    dplyr::arrange(site_id, drive_time_min, variable)

  # ------------------------------------------------------------------------
  # Step 4: per-column tolerance assertion (Sec. 26.6 contract)
  # ------------------------------------------------------------------------
  expect_cacs_close(joined$estimate_pkg,   joined$estimate_golden,   abs_tol = 1e-6)
  expect_cacs_close(joined$moe_pkg,        joined$moe_golden,        abs_tol = 1e-5)
  expect_cacs_close(joined$weight_sum_pkg, joined$weight_sum_golden, abs_tol = 1e-9)

  # n_tracts is integer -- exact equality (no tolerance)
  testthat::expect_identical(joined$n_tracts_pkg, joined$n_tracts_golden)

  # ------------------------------------------------------------------------
  # Step 5: row-count invariance -- pipeline covers all golden_area rows
  # ------------------------------------------------------------------------
  testthat::expect_equal(nrow(joined), nrow(golden_area))
  testthat::expect_equal(nrow(joined), 540L)

  # ------------------------------------------------------------------------
  # Step 6 (v0.2.0 F4 re-baseline): schema growth assertion
  #
  # Lock the v0.5.0 27-col canonical long schema so a future regression cannot
  # silently re-collapse to an older layout. The golden remains a 20-col
  # historical synthetic fixture, so this is an asymmetric assertion: the LHS
  # is the current production schema, the RHS is the regression contract.
  # ------------------------------------------------------------------------
  testthat::expect_equal(ncol(result), 27L)
  testthat::expect_true("ring_topology" %in% names(result))
  testthat::expect_true("n_tracts_num" %in% names(result))
  testthat::expect_true("n_tracts_den" %in% names(result))
  # Rate rows: NA on `n_tracts`, populated on the two F4 cols.
  rate_rows <- result[result$estimand_family == "derived_rate", ]
  if (nrow(rate_rows) > 0L) {
    testthat::expect_true(all(is.na(rate_rows$n_tracts)))
    # F4 columns are populated when carrier resolved successfully
    # (failure_origin == "none"); allow NA when failure_origin signals
    # carrier-missing edge case.
    good <- rate_rows[rate_rows$failure_origin == "none", ]
    if (nrow(good) > 0L) {
      testthat::expect_true(all(!is.na(good$n_tracts_num)))
      testthat::expect_true(all(!is.na(good$n_tracts_den)))
    }
  }
  # Source rows: NA on both F4 cols (preserves the v0.1 invariant that
  # n_tracts is the only tract-count slot used outside of rate rows).
  source_rows <- result[result$estimand_family != "derived_rate", ]
  if (nrow(source_rows) > 0L) {
    testthat::expect_true(all(is.na(source_rows$n_tracts_num)))
    testthat::expect_true(all(is.na(source_rows$n_tracts_den)))
  }
})
