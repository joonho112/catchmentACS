# ===========================================================================
# test-integration-layer1-smoke.R
#
# v0.4 plan Step 2.5 — integration smoke: chain all 5 Layer 1 helpers on a
# REAL v0.3 062 workflow output to verify the helper contracts plug into the
# real schema (not just the synthetic test fixtures).
# ===========================================================================

test_that("STEP25-01 Layer 1 smoke: rates_per_site_pivot on REAL 062 run_result.rds", {
  rr_path <- system.file("testdata", "run_result_3site_demo.rds",
                         package = "catchmentACS")
  skip_if_not(file.exists(rr_path),
              "062 workflow test fixture not available")
  rr <- readRDS(rr_path)

  pivot <- catchmentACS:::.cacs_rates_per_site_pivot(rr)
  expect_s3_class(pivot$wide, "tbl_df")
  expect_identical(nrow(pivot$wide), 3L)
  expect_identical(ncol(pivot$wide), 7L)
  expect_identical(colnames(pivot$wide)[1:2],
                   c("site_id", "drive_time_min"))
  # column order = key columns + .SANCTIONED_RATES_V1 order
  expect_identical(
    colnames(pivot$wide)[-(1:2)],
    names(catchmentACS::cacs_acs_default_rates)
  )
  # wide_moe also populated (moe column present in real run_result)
  expect_s3_class(pivot$wide_moe, "tbl_df")
  expect_identical(nrow(pivot$wide_moe), 3L)
})

test_that("STEP25-02 Layer 1 smoke: pipe_iso fills list-column from REAL iso", {
  iso_path <- system.file("testdata", "iso_3site_demo.rds",
                          package = "catchmentACS")
  skip_if_not(file.exists(iso_path),
              "062 workflow test iso fixture not available")
  iso <- readRDS(iso_path)

  # Synthetic skeleton from iso's pairs
  out <- tibble::tibble(
    site_id        = iso$site_id,
    drive_time_min = as.integer(iso$drive_time_min),
    isochrone      = vector("list", nrow(iso))
  )
  filled <- catchmentACS:::.cacs_pipe_iso_to_list_column(out, iso)
  expect_identical(nrow(filled), 3L)
  expect_true(all(vapply(filled$isochrone,
                          function(x) inherits(x, "sf"),
                          logical(1))))
  expect_true(isTRUE(attr(filled, "iso_was_filled")))
})

test_that("STEP25-03 Layer 1 smoke: osrm_resolve_demo_res 4×2 decision intact", {
  # Already comprehensively tested in test-unit-osrm-resolve-demo-res.R;
  # this smoke just confirms it loads + runs cleanly in the same session
  # as the other Layer 1 helpers.
  out_demo <- catchmentACS:::.cacs_osrm_resolve_demo_res(NULL, "demo", TRUE)
  expect_identical(out_demo$res_effective, 30L)
  expect_true(out_demo$downgraded)
  out_docker <- catchmentACS:::.cacs_osrm_resolve_demo_res(NULL, "docker", TRUE)
  expect_identical(out_docker$res_effective, 70L)
  expect_false(out_docker$downgraded)
})

test_that("STEP25-04 Layer 1 smoke: capture_conditions_with_value round-trip", {
  out <- catchmentACS:::.cacs_capture_conditions_with_value(
    rlang::quo({
      catchmentACS:::.cli_inform_listcol_iso_filled()
      42L
    }),
    targets = NULL
  )
  expect_identical(out$result, 42L)
  expect_identical(nrow(out$conditions), 1L)
})

test_that("STEP25-05 cross-helper consistency: pivot then format same column", {
  # Confirms .cacs_format_estimate_moe_string is what wide_moe uses per-column.
  # Construct a small synthetic input that produces a known $wide_moe cell.
  inp <- tibble::tibble(
    site_id = c("S1", "S1"),
    variable = c("poverty_rate", "snap_rate"),
    estimate = c(0.192, 0.109),
    moe = c(0.024, 0.013)
  )
  pivot <- catchmentACS:::.cacs_rates_per_site_pivot(inp)
  expected_pov_str <- catchmentACS:::.cacs_format_estimate_moe_string(
    0.192, 0.024, digits = 3L
  )
  expect_identical(pivot$wide_moe$poverty_rate[[1L]], expected_pov_str)
})
