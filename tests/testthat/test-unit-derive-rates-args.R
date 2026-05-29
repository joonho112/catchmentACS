# ============================================================================
# Unit tests for cacs_derive_rates() argument validation (Sec. 23.6).
#
# Covers T23-01..T23-07 from Sec. 23.7:
#   - T23-01 non-tbl weighted_acs                          -> E-23-01
#   - T23-02 carrier missing required columns              -> E-23-08
#   - T23-03 unsanctioned custom rate                      -> E-23-04
#   - T23-04 missing sanctioned rate (snap_rate dropped)   -> E-23-04
#   - T23-05 modified num/den code on sanctioned rate      -> E-23-04
#   - T23-06 carrier attribute NULL                        -> E-23-07
#   - T23-07 formula_dispatch unsanctioned enum            -> E-23-09 / E-23-10
#
# Fixture builder mirrors the `cacs_propagate_moe()` output: canonical long
# tibble + `cacs_aggregation_carriers` attribute holding (site, drive_time,
# variable, est_total, var_total_raw, weight_sum) for the two ACS counts
# referenced by .SANCTIONED_RATES_V1$poverty_rate. Only poverty_rate's
# carriers are populated for the args tests — the other 4 rates miss
# carriers and resolve via the failure_origin = "carrier" branch, which
# does *not* abort.
# ============================================================================


# ---- Shared fixture builder ------------------------------------------------

.make_derive_args_fixture <- function() {
  # Minimal canonical long output: one Family A row + matching carrier so
  # `.validate_intersect_output()` finds a key match.
  data <- tibble::tibble(
    site_id                       = "S01",
    drive_time_min                = 15L,
    ring_topology                 = "cumulative",
    variable                      = "B17001_002",
    estimate                      = 50,
    moe                           = 16.45,
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

  carriers <- tibble::tibble(
    site_id         = c("S01", "S01"),
    drive_time_min  = c(15L, 15L),
    variable        = c("B17001_002", "B17001_001"),
    estimand_family = c("spatial_total", "spatial_total"),
    est_total       = c(50, 200),
    var_total_raw   = c(100, 400),
    est_mean        = c(NA_real_, NA_real_),
    var_mean_raw    = c(NA_real_, NA_real_),
    weight_sum      = c(0.8, 0.8),
    n_tracts        = c(2L, 2L)
  )

  attr(data, "cacs_aggregation_carriers") <- carriers
  attr(data, "cacs_schema_version")       <- "1.0"
  attr(data, "cacs_confidence_level")     <- 0.90
  data
}


# ---- T23-01 ----------------------------------------------------------------

test_that("T23-01 cacs_derive_rates aborts on non-tbl weighted_acs (E-23-01)", {
  fx <- .make_derive_args_fixture()
  expect_error(
    cacs_derive_rates(as.data.frame(fx)),
    class = "catchmentACS_error_schema"
  )
  expect_error(
    cacs_derive_rates(list(site_id = "S01")),
    class = "catchmentACS_error_schema"
  )
})


# ---- T23-02 ----------------------------------------------------------------

test_that("T23-02 cacs_derive_rates aborts when carrier missing required col (E-23-08)", {
  fx <- .make_derive_args_fixture()
  car <- attr(fx, "cacs_aggregation_carriers")
  car$var_total_raw <- NULL  # drop required column
  attr(fx, "cacs_aggregation_carriers") <- car

  err <- tryCatch(cacs_derive_rates(fx), error = function(e) e)
  expect_s3_class(err, "catchmentACS_error_schema")
  expect_match(conditionMessage(err), "var_total_raw")
})


test_that("T23-02b cacs_derive_rates uses strict carrier validation before lookup", {
  fx <- .make_derive_args_fixture()

  bad_type <- attr(fx, "cacs_aggregation_carriers")
  bad_type$site_id <- factor(bad_type$site_id)
  attr(fx, "cacs_aggregation_carriers") <- bad_type
  err <- tryCatch(cacs_derive_rates(fx), error = function(e) e)
  expect_s3_class(err, "catchmentACS_error_schema")
  expect_match(conditionMessage(err), "site_id")

  fx <- .make_derive_args_fixture()
  bad_family <- attr(fx, "cacs_aggregation_carriers")
  bad_family$estimand_family[1] <- "bogus_family"
  attr(fx, "cacs_aggregation_carriers") <- bad_family
  err <- tryCatch(cacs_derive_rates(fx), error = function(e) e)
  expect_s3_class(err, "catchmentACS_error_schema")
  expect_match(conditionMessage(err), "estimand_family")
})


# ---- T23-03 ----------------------------------------------------------------

test_that("T23-03 cacs_derive_rates aborts on unsanctioned custom rate (E-23-04)", {
  fx <- .make_derive_args_fixture()
  err <- tryCatch(
    cacs_derive_rates(
      fx,
      rates = list(
        my_rate                   = c(num = "B12345_001", den = "B12345_002"),
        poverty_rate              = c(num = "B17001_002", den = "B17001_001"),
        snap_rate                 = c(num = "B22003_002", den = "B22003_001"),
        ssi_rate                  = c(num = "B19056_002", den = "B19056_001"),
        unemp_rate                = c(num = "B23025_005", den = "B23025_003"),
        labor_force_participation = c(num = "B23025_002", den = "B23025_001")
      )
    ),
    error = function(e) e
  )
  expect_s3_class(err, "catchmentACS_error_schema")
  # E-23-04 message must offer the cacs_derive_rates_custom() migration hint.
  expect_match(conditionMessage(err), "cacs_derive_rates_custom",
               fixed = TRUE)
  expect_match(conditionMessage(err), "my_rate")
})


# ---- T23-04 ----------------------------------------------------------------

test_that("T23-04 cacs_derive_rates aborts when sanctioned rate missing (E-23-04)", {
  fx <- .make_derive_args_fixture()
  # Drop snap_rate.
  err <- tryCatch(
    cacs_derive_rates(
      fx,
      rates = list(
        poverty_rate              = c(num = "B17001_002", den = "B17001_001"),
        ssi_rate                  = c(num = "B19056_002", den = "B19056_001"),
        unemp_rate                = c(num = "B23025_005", den = "B23025_003"),
        labor_force_participation = c(num = "B23025_002", den = "B23025_001")
      )
    ),
    error = function(e) e
  )
  expect_s3_class(err, "catchmentACS_error_schema")
  expect_match(conditionMessage(err), "snap_rate")
})


# ---- T23-05 ----------------------------------------------------------------

test_that("T23-05 cacs_derive_rates aborts when sanctioned rate code modified (E-23-04)", {
  fx <- .make_derive_args_fixture()
  # Swap poverty_rate num from B17001_002 -> B17020_002 (different table).
  err <- tryCatch(
    cacs_derive_rates(
      fx,
      rates = list(
        poverty_rate              = c(num = "B17020_002", den = "B17001_001"),
        snap_rate                 = c(num = "B22003_002", den = "B22003_001"),
        ssi_rate                  = c(num = "B19056_002", den = "B19056_001"),
        unemp_rate                = c(num = "B23025_005", den = "B23025_003"),
        labor_force_participation = c(num = "B23025_002", den = "B23025_001")
      )
    ),
    error = function(e) e
  )
  expect_s3_class(err, "catchmentACS_error_schema")
  expect_match(conditionMessage(err), "curated catalogue")
})


# ---- T23-06 ----------------------------------------------------------------

test_that("T23-06 cacs_derive_rates aborts on missing carrier attribute (E-23-07)", {
  fx <- .make_derive_args_fixture()
  attr(fx, "cacs_aggregation_carriers") <- NULL

  err <- tryCatch(cacs_derive_rates(fx), error = function(e) e)
  expect_s3_class(err, "catchmentACS_error_schema")
  # E-23-07 must offer the upstream call sequence hint.
  expect_match(conditionMessage(err), "cacs_intersect_weight",
               fixed = TRUE)
  expect_match(conditionMessage(err), "cacs_propagate_moe",
               fixed = TRUE)
})


# ---- T23-07 ----------------------------------------------------------------

test_that("T23-07 cacs_derive_rates aborts on unsanctioned formula_dispatch (E-23-09)", {
  fx <- .make_derive_args_fixture()

  err <- tryCatch(
    cacs_derive_rates(fx, formula_dispatch = "bogus"),
    error = function(e) e
  )
  expect_s3_class(err, "catchmentACS_error_schema")
  expect_match(conditionMessage(err), "general_ratio_conservative")
  expect_match(conditionMessage(err), "proportion_subset")
  expect_match(conditionMessage(err), "auto")

  # Length-2 / NA also abort with schema class.
  expect_error(
    cacs_derive_rates(fx, formula_dispatch = c("auto", "auto")),
    class = "catchmentACS_error_schema"
  )
  expect_error(
    cacs_derive_rates(fx, formula_dispatch = NA_character_),
    class = "catchmentACS_error_schema"
  )
})


# ---- T23-07b critical column missing check (E-23-02) -----------------------

test_that("T23-07b cacs_derive_rates aborts when critical column missing (E-23-02)", {
  fx <- .make_derive_args_fixture()
  fx$failure_origin <- NULL  # drop a required Sec. 22 MOE column

  err <- tryCatch(cacs_derive_rates(fx), error = function(e) e)
  expect_s3_class(err, "catchmentACS_error_schema")
  expect_match(conditionMessage(err), "failure_origin")
})


# ---- T23-07c rates non-list -----------------------------------------------

test_that("T23-07c cacs_derive_rates aborts when rates is not a named list (E-23-03)", {
  fx <- .make_derive_args_fixture()
  # Unnamed list
  expect_error(
    cacs_derive_rates(fx, rates = list(c(num = "X", den = "Y"))),
    class = "catchmentACS_error_schema"
  )
  # Atomic vector
  expect_error(
    cacs_derive_rates(fx, rates = c("a", "b")),
    class = "catchmentACS_error_schema"
  )
})
