# ===========================================================================
# test-issue004-track3-astibble.R
#
# v0.4 plan Step 5.3 — Track 3: as_tibble.cacs_run_result() rate-first sort.
# ===========================================================================

.rr_fixture <- function() {
  readRDS(system.file("testdata", "run_result_3site_demo.rds", package = "catchmentACS"))
}

# --- Default behavior: rate-first ------------------------------------------

test_that("ISSUE004-T3-01 default as_tibble: first 5 rows = AL_BHM_01 rate rows", {
  rr <- .rr_fixture()
  tbl <- tibble::as_tibble(rr)
  first5 <- tbl[1:5, c("site_id", "variable")]
  expect_true(all(first5$site_id == "AL_BHM_01"))
  rate_vars <- names(cacs_acs_default_rates)
  expect_true(all(first5$variable %in% rate_vars))
})

test_that("ISSUE004-T3-02 default rate-first: rate rows come BEFORE B-codes within site", {
  rr <- .rr_fixture()
  tbl <- tibble::as_tibble(rr)
  # For each site, identify the indices of rate rows and B-code rows.
  rate_vars <- names(cacs_acs_default_rates)
  for (sid in unique(tbl$site_id)) {
    idx_site <- which(tbl$site_id == sid)
    is_rate <- tbl$variable[idx_site] %in% rate_vars
    rate_positions <- idx_site[is_rate]
    bcode_positions <- idx_site[!is_rate]
    # Max rate row index < min B-code row index (rate rows are first)
    if (length(rate_positions) > 0L && length(bcode_positions) > 0L) {
      expect_lt(max(rate_positions), min(bcode_positions))
    }
  }
})

# --- rate_first = FALSE: preserve underlying long row order -----------------

test_that("ISSUE004-T3-03 rate_first=FALSE preserves underlying long row order", {
  rr <- .rr_fixture()
  tbl <- tibble::as_tibble(rr, rate_first = FALSE)

  underlying <- rr
  class(underlying) <- setdiff(class(underlying), "cacs_run_result")
  underlying <- tibble::as_tibble(underlying)

  expect_identical(tbl, underlying)
})

# --- Option override -------------------------------------------------------

test_that("ISSUE004-T3-04 options(rate_first_default=FALSE) becomes default", {
  withr::local_options(catchmentACS.rate_first_default = FALSE)
  rr <- .rr_fixture()
  tbl <- tibble::as_tibble(rr)  # No explicit rate_first arg
  explicit <- tibble::as_tibble(rr, rate_first = FALSE)
  expect_identical(tbl, explicit)
})

# --- Class chain check -----------------------------------------------------

test_that("ISSUE004-T3-05 result is plain tbl_df (cacs_run_result class dropped)", {
  rr <- .rr_fixture()
  tbl <- tibble::as_tibble(rr)
  expect_s3_class(tbl, "tbl_df")
  expect_false(inherits(tbl, "cacs_run_result"))
})

# --- All rows preserved ----------------------------------------------------

test_that("ISSUE004-T3-06 row count and column count preserved", {
  rr <- .rr_fixture()
  tbl <- tibble::as_tibble(rr)
  expect_identical(nrow(tbl), nrow(rr))
  expect_identical(ncol(tbl), ncol(rr))
  # Same variables, just reordered
  expect_setequal(tbl$variable, rr$variable)
})
