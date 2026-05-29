# tests/testthat-shaped scaffold for Variant 2 (base R + vapply style).
# Source `variant-2-base-r-style.R` then `devtools::load_all()` of
# catchmentACS to get `.SANCTIONED_RATES_V1`, then run via
# `testthat::test_file()`.

# ----------------------------------------------------------------------------
# Fixture: 3-site long-format tibble matching the v0.3 workflow test's
# expected per-site rate matrix (Issue 004 "What the user wants" block;
# verified against outputs/09_rates_wide.rds golden cells). Each site has
# all 5 sanctioned rate rows + a couple of source ACS variable rows so the
# `%in%` filter is exercised (rates only, source vars dropped).
# ----------------------------------------------------------------------------

.make_three_site_fixture <- function() {
  rates <- c("poverty_rate", "snap_rate", "ssi_rate", "unemp_rate",
             "labor_force_participation")
  # Golden rates per Issue 004 + v0.3 outputs/09_rates_wide.rds.
  bhm_est <- c(0.1934443, 0.1118386, 0.05038946, 0.04238390, 0.6485219)
  hsv_est <- c(0.2048053, 0.1566358, 0.06195811, 0.06189379, 0.6035788)
  mob_est <- c(0.2680838, 0.2966876, 0.10409112, 0.07483526, 0.5119531)
  bhm_moe <- c(0.024, 0.013, 0.009, 0.009, 0.034)
  hsv_moe <- c(0.020, 0.018, 0.012, 0.012, 0.031)
  mob_moe <- c(0.028, 0.023, 0.015, 0.013, 0.031)

  rate_rows <- tibble::tibble(
    site_id  = rep(c("AL_BHM_01", "AL_HSV_01", "AL_MOB_01"), each = 5L),
    variable = rep(rates, times = 3L),
    estimate = c(bhm_est, hsv_est, mob_est),
    moe      = c(bhm_moe, hsv_moe, mob_moe)
  )

  # A handful of source ACS-variable rows mixed in to verify the rate-row
  # filter does not let source rows leak into the wide pivot.
  noise_rows <- tibble::tibble(
    site_id  = c("AL_BHM_01", "AL_BHM_01", "AL_HSV_01", "AL_MOB_01"),
    variable = c("B01003_001", "B17001_001", "B23025_001", "B22003_002"),
    estimate = c(64226.7, 58947.2, 30000.0, 1234.5),
    moe      = c(2132.8,  2112.0,  900.0,   400.0)
  )
  dplyr::bind_rows(rate_rows, noise_rows)
}


# ----------------------------------------------------------------------------
# Test 1 - Happy path: 3-site fixture -> 3 x 6 wide tibble.
# ----------------------------------------------------------------------------

testthat::test_that("variant 2: 3-site fixture -> 3 x 6 wide tibble", {
  fx  <- .make_three_site_fixture()
  out <- .cacs_rates_per_site_pivot(fx)

  testthat::expect_named(out, c("wide", "wide_moe"))
  testthat::expect_s3_class(out$wide, "tbl_df")
  testthat::expect_equal(nrow(out$wide), 3L)
  testthat::expect_equal(ncol(out$wide), 1L + 5L)
  testthat::expect_equal(out$wide$site_id,
                         c("AL_BHM_01", "AL_HSV_01", "AL_MOB_01"))
  # numeric columns
  for (col in names(.SANCTIONED_RATES_V1)) {
    testthat::expect_true(is.numeric(out$wide[[col]]),
                          info = paste("rate column", col,
                                       "should be numeric"))
  }
})


# ----------------------------------------------------------------------------
# Test 2 - Happy path: wide_moe has 'x ± y' strings.
# ----------------------------------------------------------------------------

testthat::test_that("variant 2: wide_moe is character with 'x ± y' cells", {
  fx  <- .make_three_site_fixture()
  out <- .cacs_rates_per_site_pivot(fx)

  testthat::expect_s3_class(out$wide_moe, "tbl_df")
  testthat::expect_equal(nrow(out$wide_moe), 3L)
  testthat::expect_equal(ncol(out$wide_moe), 1L + 5L)
  testthat::expect_true(is.character(out$wide_moe$poverty_rate))

  # Birmingham poverty_rate cell: "0.193 ± 0.024" at default digits=3.
  bhm_cell <- out$wide_moe$poverty_rate[
    out$wide_moe$site_id == "AL_BHM_01"
  ]
  testthat::expect_equal(bhm_cell, "0.193 ± 0.024")
})


# ----------------------------------------------------------------------------
# Test 3 - Column order matches names(cacs_acs_default_rates).
# ----------------------------------------------------------------------------

testthat::test_that("variant 2: column order matches sanctioned rates v1 order", {
  fx  <- .make_three_site_fixture()
  out <- .cacs_rates_per_site_pivot(fx)

  expected_order <- c("site_id", names(.SANCTIONED_RATES_V1))
  testthat::expect_equal(colnames(out$wide), expected_order)
  testthat::expect_equal(colnames(out$wide_moe), expected_order)
})


# ----------------------------------------------------------------------------
# Test 4 - n_sites = 0 -> 0-row tibble with column names + numeric types.
# ----------------------------------------------------------------------------

testthat::test_that("variant 2: empty input -> 0-row tibble with correct shape", {
  empty <- tibble::tibble(
    site_id  = character(0),
    variable = character(0),
    estimate = numeric(0),
    moe      = numeric(0)
  )
  out <- .cacs_rates_per_site_pivot(empty)

  testthat::expect_s3_class(out$wide, "tbl_df")
  testthat::expect_equal(nrow(out$wide), 0L)
  testthat::expect_equal(ncol(out$wide), 1L + 5L)
  testthat::expect_equal(
    colnames(out$wide),
    c("site_id", names(.SANCTIONED_RATES_V1))
  )
  testthat::expect_true(is.character(out$wide$site_id))
  for (col in names(.SANCTIONED_RATES_V1)) {
    testthat::expect_true(is.numeric(out$wide[[col]]))
  }
  # wide_moe parallel 0-row tibble.
  testthat::expect_s3_class(out$wide_moe, "tbl_df")
  testthat::expect_equal(nrow(out$wide_moe), 0L)
  testthat::expect_equal(ncol(out$wide_moe), 1L + 5L)
})


# ----------------------------------------------------------------------------
# Test 5 - Missing rate row for a site -> NA_real_ cell.
# ----------------------------------------------------------------------------

testthat::test_that("variant 2: missing (site, rate) row -> NA cell", {
  # Drop AL_BHM_01's snap_rate row from the fixture.
  fx <- .make_three_site_fixture()
  fx <- fx[!(fx$site_id == "AL_BHM_01" & fx$variable == "snap_rate"), ]
  out <- .cacs_rates_per_site_pivot(fx)

  bhm_snap <- out$wide$snap_rate[out$wide$site_id == "AL_BHM_01"]
  testthat::expect_true(is.na(bhm_snap))
  # Other Birmingham cells still populated.
  bhm_pov <- out$wide$poverty_rate[out$wide$site_id == "AL_BHM_01"]
  testthat::expect_false(is.na(bhm_pov))
  # wide_moe cell for the dropped row is NA_character_ (both sides NA).
  bhm_snap_moe <- out$wide_moe$snap_rate[out$wide_moe$site_id == "AL_BHM_01"]
  testthat::expect_true(is.na(bhm_snap_moe))
})


# ----------------------------------------------------------------------------
# Test 6 - Missing `moe` column -> wide_moe = NULL + moe_unavailable attr.
# ----------------------------------------------------------------------------

testthat::test_that("variant 2: missing moe column -> wide_moe NULL + attr", {
  fx <- .make_three_site_fixture()
  fx$moe <- NULL
  out <- .cacs_rates_per_site_pivot(fx)

  testthat::expect_null(out$wide_moe)
  testthat::expect_true(isTRUE(attr(out, "moe_unavailable")))
  testthat::expect_s3_class(out$wide, "tbl_df")
  testthat::expect_equal(nrow(out$wide), 3L)
})


# ----------------------------------------------------------------------------
# Test 7 - Golden: Birmingham row matches Issue 004's expected values.
# ----------------------------------------------------------------------------

testthat::test_that("variant 2: Birmingham golden row matches Issue 004 + 09_rates_wide.rds", {
  fx  <- .make_three_site_fixture()
  out <- .cacs_rates_per_site_pivot(fx)

  bhm <- out$wide[out$wide$site_id == "AL_BHM_01", ]
  testthat::expect_equal(bhm$poverty_rate,              0.1934443, tolerance = 1e-6)
  testthat::expect_equal(bhm$snap_rate,                 0.1118386, tolerance = 1e-6)
  testthat::expect_equal(bhm$ssi_rate,                  0.05038946, tolerance = 1e-7)
  testthat::expect_equal(bhm$unemp_rate,                0.04238390, tolerance = 1e-7)
  testthat::expect_equal(bhm$labor_force_participation, 0.6485219, tolerance = 1e-6)
})


# ----------------------------------------------------------------------------
# Test 8 - NA-safe format helper: both NA -> NA_character_; one NA -> "NA"
# token on that side only.
# ----------------------------------------------------------------------------

testthat::test_that("variant 2: .cacs_format_estimate_moe_string is NA-safe", {
  out <- .cacs_format_estimate_moe_string(
    estimate = c(0.192, NA,    0.192, NA),
    moe      = c(0.024, 0.024, NA,    NA)
  )
  testthat::expect_equal(out[1], "0.192 ± 0.024")
  testthat::expect_equal(out[2], "NA ± 0.024")
  testthat::expect_equal(out[3], "0.192 ± NA")
  testthat::expect_true(is.na(out[4]))
})


# ----------------------------------------------------------------------------
# Test 9 - digits argument respected.
# ----------------------------------------------------------------------------

testthat::test_that("variant 2: .cacs_format_estimate_moe_string digits arg respected", {
  s2 <- .cacs_format_estimate_moe_string(0.123456, 0.034567, digits = 2L)
  s4 <- .cacs_format_estimate_moe_string(0.123456, 0.034567, digits = 4L)
  s0 <- .cacs_format_estimate_moe_string(0.6, 0.4, digits = 0L)

  testthat::expect_equal(s2, "0.12 ± 0.03")
  testthat::expect_equal(s4, "0.1235 ± 0.0346")
  testthat::expect_equal(s0, "1 ± 0")  # rounded to whole
})


# ----------------------------------------------------------------------------
# Test 10 - n_sites = 50 synthetic input does not crash.
# ----------------------------------------------------------------------------

testthat::test_that("variant 2: n_sites = 50 perf-ish run does not crash", {
  rates <- names(.SANCTIONED_RATES_V1)
  n     <- 50L
  set.seed(42L)
  big <- tibble::tibble(
    site_id  = rep(sprintf("SITE_%03d", seq_len(n)), each = length(rates)),
    variable = rep(rates, times = n),
    estimate = runif(n * length(rates), 0.05, 0.5),
    moe      = runif(n * length(rates), 0.005, 0.05)
  )

  t0 <- Sys.time()
  out <- .cacs_rates_per_site_pivot(big)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  testthat::expect_equal(nrow(out$wide), n)
  testthat::expect_equal(ncol(out$wide), 1L + length(rates))
  testthat::expect_equal(nrow(out$wide_moe), n)
  # Soft perf budget: 50 sites x 5 rates is < 1 sec on any modern hardware.
  testthat::expect_lt(elapsed, 1.0)
})


# ----------------------------------------------------------------------------
# Test 11 - drive_time_min is part of the key when present.
# ----------------------------------------------------------------------------

testthat::test_that("variant 2: drive_time_min creates separate key rows", {
  rates <- c("poverty_rate", "snap_rate")
  fx <- tibble::tibble(
    site_id = c("S1", "S1", "S1", "S1", "S2", "S2"),
    drive_time_min = c(5L, 5L, 10L, 10L, 5L, 5L),
    variable = rep(rates, times = 3L),
    estimate = c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60),
    moe = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06)
  )

  out <- .cacs_rates_per_site_pivot(fx, rates_names = rates)

  testthat::expect_equal(nrow(out$wide), 3L)
  testthat::expect_equal(
    colnames(out$wide),
    c("site_id", "drive_time_min", rates)
  )
  testthat::expect_equal(out$wide$site_id, c("S1", "S1", "S2"))
  testthat::expect_equal(out$wide$drive_time_min, c(5L, 10L, 5L))
  testthat::expect_equal(out$wide$poverty_rate, c(0.10, 0.30, 0.50))
  testthat::expect_equal(out$wide$snap_rate, c(0.20, 0.40, 0.60))
  testthat::expect_equal(
    colnames(out$wide_moe),
    c("site_id", "drive_time_min", rates)
  )
})


# ----------------------------------------------------------------------------
# Test 12 - duplicate key-rate rows abort instead of overwriting.
# ----------------------------------------------------------------------------

testthat::test_that("variant 2: duplicate key-rate rows abort", {
  fx <- tibble::tibble(
    site_id = c("S1", "S1"),
    drive_time_min = c(5L, 5L),
    variable = c("poverty_rate", "poverty_rate"),
    estimate = c(0.10, 0.99),
    moe = c(0.01, 0.09)
  )

  testthat::expect_error(
    .cacs_rates_per_site_pivot(fx, rates_names = "poverty_rate"),
    "duplicate rate rows"
  )
})
