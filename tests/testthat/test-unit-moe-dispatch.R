# ============================================================================
# Unit tests for Phase 6.2 dispatch + carrier-lookup helpers in
# R/moe-helpers.R. 8 cases per Sec. 41.4 (T-DISP-01..08) covering:
#   - .dispatch_formula_auto() family -> formula mapping
#   - .dispatch_formula_list() per-variable override + auto-dispatch baseline
#   - .check_family_formula_consistency() Sec. 22.2 matrix validation
#   - .lookup_carrier_num() Phase 6.3 C1/C2 fallback building block
# ============================================================================


# ---- Shared fixture builder ------------------------------------------------
#
# Minimal 4-row data tibble + matching carrier tibble. Two Family A rows
# (poverty num + den), one Family B row (median proxy), one derived_rate row
# (poverty_rate). Carrier mirrors the (site_id, drive_time_min, variable)
# 3-key for the two ACS counts referenced by .SANCTIONED_RATES_V1$poverty_rate.

.disp_fixture <- function() {
  data <- tibble::tibble(
    site_id         = c("S01", "S01", "S01", "S01"),
    drive_time_min  = c(15L,   15L,   15L,   15L),
    variable        = c("B17001_002", "B17001_001",
                        "B19013_001", "poverty_rate"),
    estimand_family = c("spatial_total", "spatial_total",
                        "median_proxy", "derived_rate")
  )
  carriers <- tibble::tibble(
    site_id        = c("S01", "S01"),
    drive_time_min = c(15L,   15L),
    variable       = c("B17001_002", "B17001_001"),
    est_total      = c(50,    200),
    var_total_raw  = c(100,   400),
    est_mean       = c(NA_real_, NA_real_),
    var_mean_raw   = c(NA_real_, NA_real_)
  )
  list(data = data, carriers = carriers)
}


# ---- T-DISP-01 -------------------------------------------------------------

test_that("T-DISP-01 .dispatch_formula_auto maps spatial_total -> weighted_sum", {
  out <- .dispatch_formula_auto(c("spatial_total", "spatial_total",
                                  "metadata_only"))
  expect_equal(out, c("weighted_sum", "weighted_sum", NA_character_))
})


# ---- T-DISP-02 -------------------------------------------------------------

test_that("T-DISP-02 .dispatch_formula_auto maps derived_rate -> general_ratio_conservative", {
  out <- .dispatch_formula_auto(c("derived_rate", "median_proxy",
                                  "area_weighted_scalar_proxy"))
  expect_equal(
    out,
    c("general_ratio_conservative", "weighted_mean", "weighted_mean")
  )
})


# ---- T-DISP-03 -------------------------------------------------------------

test_that("T-DISP-03 .dispatch_formula_list overlays named entry onto auto baseline", {
  fx <- .disp_fixture()
  # Override B17001_002 to weighted_mean (illustrative; caller would normally
  # validate matrix consistency separately). Other rows fall back to auto.
  out <- .dispatch_formula_list(
    list(B17001_002 = "weighted_mean"),
    fx$data
  )
  expect_equal(out[fx$data$variable == "B17001_002"], "weighted_mean")
  # B17001_001 was not in the list -> stays on auto-dispatch (spatial_total -> weighted_sum).
  expect_equal(out[fx$data$variable == "B17001_001"], "weighted_sum")
  # The median-proxy row keeps auto-dispatch (weighted_mean).
  expect_equal(out[fx$data$variable == "B19013_001"], "weighted_mean")
  # derived_rate row keeps the Sec. 23.4 C2 default.
  expect_equal(out[fx$data$variable == "poverty_rate"],
               "general_ratio_conservative")
})


# ---- T-DISP-04 -------------------------------------------------------------

test_that("T-DISP-04 .dispatch_formula_list ignores list entries not in data$variable", {
  # Variables not present in `data$variable` are silently skipped — caller
  # (`.resolve_formula()`) is responsible for the unknown-variable abort
  # (E-22-05) so the standalone helper stays no-op-safe for re-use from
  # Phase 6.3 per-rate dispatch.
  fx <- .disp_fixture()
  out <- .dispatch_formula_list(
    list(NON_EXISTENT_VAR = "weighted_sum"),
    fx$data
  )
  # Output matches auto-dispatch baseline byte-identically.
  expect_equal(out, .dispatch_formula_auto(fx$data$estimand_family))
})


# ---- T-DISP-05 -------------------------------------------------------------

test_that("T-DISP-05 .check_family_formula_consistency accepts sanctioned pairs", {
  family  <- c("spatial_total", "median_proxy", "derived_rate",
               "derived_rate", "metadata_only",
               "area_weighted_scalar_proxy",
               "population_weighted_scalar_proxy",
               "area_weighted_rate_proxy")
  formula <- c("weighted_sum", "weighted_mean", "proportion_subset",
               "general_ratio_conservative", NA_character_,
               "weighted_mean", "weighted_mean", "weighted_mean")
  expect_equal(.check_family_formula_consistency(family, formula),
               integer(0))
})


# ---- T-DISP-06 -------------------------------------------------------------

test_that("T-DISP-06 .check_family_formula_consistency flags mismatches (E-22-13)", {
  # Row 1: spatial_total + proportion_subset -> bad
  # Row 2: derived_rate + weighted_sum -> bad
  # Row 3: spatial_total + weighted_sum -> ok
  # Row 4: metadata_only + weighted_mean -> bad (must be NA)
  # Row 5: spatial_total + NA -> bad (non-meta cannot be NA)
  # Row 6: unknown_family + anything -> bad
  family  <- c("spatial_total", "derived_rate", "spatial_total",
               "metadata_only", "spatial_total", "bogus_family")
  formula <- c("proportion_subset", "weighted_sum", "weighted_sum",
               "weighted_mean", NA_character_, "weighted_sum")
  expect_equal(
    .check_family_formula_consistency(family, formula),
    c(1L, 2L, 4L, 5L, 6L)
  )

  # Length mismatch -> abort with schema class
  expect_error(
    .check_family_formula_consistency(c("spatial_total"),
                                      c("weighted_sum", "weighted_mean")),
    class = "catchmentACS_error_schema"
  )

  # Zero-length input -> integer(0) (no abort)
  expect_equal(
    .check_family_formula_consistency(character(0), character(0)),
    integer(0)
  )
})


# ---- T-DISP-07 -------------------------------------------------------------

test_that("T-DISP-07 .lookup_carrier_num returns B17001_002 est_total for poverty_rate", {
  fx <- .disp_fixture()
  # poverty_rate -> num = "B17001_002" per .SANCTIONED_RATES_V1.
  # Carrier has est_total = 50 for that key at (S01, 15).
  out <- .lookup_carrier_num(
    rate_name      = "poverty_rate",
    carriers       = fx$carriers,
    site_id        = "S01",
    drive_time_min = 15L
  )
  expect_equal(out, 50)
  expect_type(out, "double")

  # Same lookup vectorized over two query rows hitting the same key.
  out2 <- .lookup_carrier_num(
    rate_name      = "poverty_rate",
    carriers       = fx$carriers,
    site_id        = c("S01", "S01"),
    drive_time_min = c(15L,   15L)
  )
  expect_equal(out2, c(50, 50))

  # Denominator + variance counterparts share the same dispatch surface.
  expect_equal(
    .lookup_carrier_den(
      rate_name      = "poverty_rate",
      carriers       = fx$carriers,
      site_id        = "S01",
      drive_time_min = 15L
    ),
    200
  )
  expect_equal(
    .lookup_carrier_num_var(
      rate_name      = "poverty_rate",
      carriers       = fx$carriers,
      site_id        = "S01",
      drive_time_min = 15L
    ),
    100
  )
  expect_equal(
    .lookup_carrier_den_var(
      rate_name      = "poverty_rate",
      carriers       = fx$carriers,
      site_id        = "S01",
      drive_time_min = 15L
    ),
    400
  )
})


# ---- T-DISP-08 -------------------------------------------------------------

test_that("T-DISP-08 .lookup_carrier_num returns NA when carrier missing the variable", {
  fx <- .disp_fixture()
  # Drop the num row (B17001_002) entirely -> lookup must return NA_real_,
  # NOT abort — Phase 6.3 will translate the NA into failure_origin="carrier"
  # per Sec. 23.5.
  carriers_missing <- fx$carriers[fx$carriers$variable != "B17001_002", ]
  out <- .lookup_carrier_num(
    rate_name      = "poverty_rate",
    carriers       = carriers_missing,
    site_id        = "S01",
    drive_time_min = 15L
  )
  expect_true(is.na(out))
  expect_type(out, "double")

  # Mismatch on the (site_id, drive_time_min) tuple also returns NA.
  out_wrong_site <- .lookup_carrier_num(
    rate_name      = "poverty_rate",
    carriers       = fx$carriers,
    site_id        = "S99",
    drive_time_min = 15L
  )
  expect_true(is.na(out_wrong_site))

  # Unknown rate_name -> hard abort (schema class), not silent NA.
  expect_error(
    .lookup_carrier_num(
      rate_name      = "bogus_rate",
      carriers       = fx$carriers,
      site_id        = "S01",
      drive_time_min = 15L
    ),
    class = "catchmentACS_error_schema"
  )

  # Carrier missing the value column -> hard abort.
  carriers_no_value <- fx$carriers
  carriers_no_value$est_total <- NULL
  expect_error(
    .lookup_carrier_num(
      rate_name      = "poverty_rate",
      carriers       = carriers_no_value,
      site_id        = "S01",
      drive_time_min = 15L
    ),
    class = "catchmentACS_error_schema"
  )
})
