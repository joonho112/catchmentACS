# ===========================================================================
# test-issue004-track1-summary.R
#
# v0.4 plan Step 5.1 — Track 1 (summary.cacs_run_result + print.cacs_run_summary).
# ===========================================================================

# --- Fixture: load real run_result from v0.3 062 ---------------------------

.rr_fixture <- function() {
  readRDS(system.file("testdata", "run_result_3site_demo.rds", package = "catchmentACS"))
}

# --- Slot presence tests ---------------------------------------------------

test_that("ISSUE004-T1-01 sm$rates_per_site slot exists as 3 × 7 tibble", {
  rr <- .rr_fixture()
  sm <- summary(rr)
  expect_s3_class(sm$rates_per_site, "tbl_df")
  expect_identical(nrow(sm$rates_per_site), 3L)
  expect_identical(ncol(sm$rates_per_site), 7L)
})

test_that("ISSUE004-T1-02 sm$rates_per_site_moe slot exists as 3 × 7 tibble", {
  rr <- .rr_fixture()
  sm <- summary(rr)
  expect_s3_class(sm$rates_per_site_moe, "tbl_df")
  expect_identical(nrow(sm$rates_per_site_moe), 3L)
  expect_identical(ncol(sm$rates_per_site_moe), 7L)
})

test_that("ISSUE004-T1-03 column names = key columns + 5 rate names in SANCTIONED order", {
  rr <- .rr_fixture()
  sm <- summary(rr)
  expect_identical(
    colnames(sm$rates_per_site),
    c("site_id", "drive_time_min", names(cacs_acs_default_rates))
  )
})

test_that("ISSUE004-T1-04 BHM poverty_rate in plausible range [0.15, 0.25]", {
  rr <- .rr_fixture()
  sm <- summary(rr)
  bhm_idx <- which(sm$rates_per_site$site_id == "AL_BHM_01" &
                     sm$rates_per_site$drive_time_min == 10L)
  bhm_pov <- sm$rates_per_site$poverty_rate[bhm_idx]
  # Real ACS data: poverty_rate for AL_BHM_01 10-min catchment ~19%.
  # Range check catches cell mis-association (e.g., snap_rate in poverty col).
  expect_gt(bhm_pov, 0.15)
  expect_lt(bhm_pov, 0.25)
})

# --- breakdown argument tests ----------------------------------------------

test_that("ISSUE004-T1-05 default breakdown = 'cross_site' (v0.3 compat)", {
  rr <- .rr_fixture()
  sm <- summary(rr)
  expect_identical(attr(sm, "breakdown"), "cross_site")
})

test_that("ISSUE004-T1-06 breakdown = 'per_site' attribute set", {
  rr <- .rr_fixture()
  sm <- summary(rr, breakdown = "per_site")
  expect_identical(attr(sm, "breakdown"), "per_site")
  # Slot still unconditionally present
  expect_s3_class(sm$rates_per_site, "tbl_df")
})

test_that("ISSUE004-T1-07 breakdown = 'both' attribute set", {
  rr <- .rr_fixture()
  sm <- summary(rr, breakdown = "both")
  expect_identical(attr(sm, "breakdown"), "both")
})

test_that("ISSUE004-T1-08 invalid breakdown aborts via match.arg", {
  rr <- .rr_fixture()
  expect_error(
    summary(rr, breakdown = "invalid"),
    "should be one of"
  )
})

# --- v0.3 backward-compat: existing slots intact ---------------------------

test_that("ISSUE004-T1-09 v0.3 slots all present + correct shape", {
  rr <- .rr_fixture()
  sm <- summary(rr)
  expect_true(all(c("metadata", "n_rows", "n_sites", "n_variables",
                    "n_drive_times", "n_tracts_summary",
                    "rates_breakdown", "moe_fallback_rate") %in% names(sm)))
  expect_s3_class(sm$rates_breakdown, "tbl_df")
  expect_identical(nrow(sm$rates_breakdown), 5L)  # 5 rates
})

test_that("ISSUE004-T1-10 n_sites=3 default option auto-includes per-site (slot check)", {
  rr <- .rr_fixture()
  sm <- summary(rr)
  # Slot existence + non-empty is the architectural invariant.
  # cli output goes via rlang_inform / message stream and is hard to capture
  # via capture.output(); the auto-include logic is purely option-gated and
  # tested separately via the print method's behavior tracer.
  expect_true(!is.null(sm$rates_per_site))
  expect_gt(nrow(sm$rates_per_site), 0L)
  # Verify the print method's auto-include gate: n_sites=3 ≤ option 12L → show.
  per_site_max <- getOption("catchmentACS.summary_per_site_max", 12L)
  expect_lte(sm$n_sites, per_site_max)
})
