# ============================================================================
# Integration tests for the §21 -> §22 -> §23 end-to-end chain.
#
# Drives the cacs_intersect_weight() -> cacs_propagate_moe() ->
# cacs_derive_rates() pipeline against a fixture that mirrors the Sec. 23
# 5 curated rates' carrier needs: each rate's numerator and denominator ACS
# code is present as a separate `variable` row in the input acs_sf so the
# Phase 5 aggregation produces a carrier entry for every code referenced
# by .SANCTIONED_RATES_V1.
#
# T23-INT-01 — full pipeline runs without abort and produces 5 rate rows
#              per (site, drive_time) pair.
# T23-INT-02 — attribute lineage: cacs_schema_version + cacs_moe_provenance
#              + cacs_rate_provenance present; cacs_aggregation_carriers
#              dropped (consume-then-drop, Sec. 23.5).
# T23-INT-03 — `formula_dispatch = "auto"` per-rate matches catalogue.
# ============================================================================


.derive_int_acs_codes <- c(
  "B17001_002", "B17001_001",  # poverty num / den
  "B22003_002", "B22003_001",  # snap num / den
  "B19056_002", "B19056_001",  # ssi num / den
  "B23025_005", "B23025_003",  # unemp num / den
  "B23025_002", "B23025_001"   # labor force participation num / den
)


# Build an end-to-end pipeline pair: 16-col iso_sf + tidy acs_sf covering
# the 10 ACS codes referenced by .SANCTIONED_RATES_V1. Anchored at
# CONUS-valid (-86.80, 33.50) so the §1e CONUS guard + EPSG:5070 round-trip
# pass.

.derive_int_make_pair <- function() {
  fx <- helper_load_synthetic_2tract()
  cx <- -86.80; cy <- 33.50

  iso_4326 <- sf::st_sf(
    tibble::tibble(
      site_id                          = fx$site$site_id,
      drive_time_min                   = as.integer(fx$site$drive_time_min),
      provider                         = "osrm",
      profile                          = "car",
      osm_snapshot_date                = NA_character_,
      routing_engine_version           = "stub/0.0.0",
      polygon_simplification_tolerance = NA_real_,
      generated_at                     = as.POSIXct("2026-05-22 00:00:00",
                                                     tz = "UTC"),
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

  # Two tract geometries replicated across all 10 ACS codes so each rate's
  # numerator AND denominator have valid carriers.
  tract_geoms <- sf::st_sfc(
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

  acs_rows <- list()
  for (code in .derive_int_acs_codes) {
    acs_rows[[length(acs_rows) + 1L]] <- tibble::tibble(
      GEOID    = fx$tracts$GEOID,
      NAME     = fx$tracts$NAME,
      variable = code,
      # Deterministic per-code estimate so the carrier math is reproducible.
      estimate = if (endsWith(code, "_001")) c(500, 800) else c(60, 120),
      moe      = if (endsWith(code, "_001")) c(15, 22)  else c(8,  12)
    )
  }
  acs_tbl <- do.call(rbind, acs_rows)
  acs_4269 <- sf::st_sf(
    acs_tbl,
    geometry = sf::st_sfc(rep(tract_geoms,
                              length(.derive_int_acs_codes)),
                          crs = 4269)
  )

  list(iso = iso_4326, acs = acs_4269)
}


test_that("T23-INT-01 21->22->23 end-to-end produces 5 rate rows per (site, drive_time)", {
  pair <- .derive_int_make_pair()
  withr::local_options(catchmentACS.cache_intersect = FALSE)

  inter <- suppressMessages(
    cacs_intersect_weight(pair$iso, pair$acs, verbose = FALSE)
  )
  expect_s3_class(inter, "tbl_df")
  expect_false(is.null(attr(inter, "cacs_aggregation_carriers")))

  prop <- suppressWarnings(cacs_propagate_moe(inter))
  expect_s3_class(prop, "tbl_df")
  expect_false(is.null(attr(prop, "cacs_aggregation_carriers")))
  expect_false(is.null(attr(prop, "cacs_moe_provenance")))

  out <- suppressWarnings(cacs_derive_rates(prop))
  expect_s3_class(out, "tbl_df")

  rate_names <- c("poverty_rate", "snap_rate", "ssi_rate",
                  "unemp_rate", "labor_force_participation")
  rate_rows <- out[out$variable %in% rate_names, ]
  # One (site, drive_time) pair x 5 rates = 5 rate rows.
  expect_equal(nrow(rate_rows), 5L)
  expect_setequal(rate_rows$variable, rate_names)
  expect_true(all(rate_rows$estimand_family == "derived_rate"))
  expect_true(all(rate_rows$weight_basis  == "coverage"))
})


test_that("T23-INT-02 attribute lineage: schema/moe/rate provenance + carrier dropped", {
  pair <- .derive_int_make_pair()
  withr::local_options(catchmentACS.cache_intersect = FALSE)

  out <- suppressWarnings(
    cacs_derive_rates(
      cacs_propagate_moe(
        suppressMessages(
          cacs_intersect_weight(pair$iso, pair$acs, verbose = FALSE)
        )
      )
    )
  )

  expect_equal(attr(out, "cacs_schema_version"), "1.0")
  expect_false(is.null(attr(out, "cacs_moe_provenance")))
  expect_false(is.null(attr(out, "cacs_rate_provenance")))
  expect_null(attr(out, "cacs_aggregation_carriers"))

  rp <- attr(out, "cacs_rate_provenance")
  expect_setequal(
    rp$rates_computed,
    c("poverty_rate", "snap_rate", "ssi_rate",
      "unemp_rate", "labor_force_participation")
  )
  expect_equal(rp$formula_dispatch, "general_ratio_conservative")
})


test_that("T23-INT-03 formula_dispatch='auto' per-rate matches .RATE_FORMULA_CATALOGUE", {
  pair <- .derive_int_make_pair()
  withr::local_options(catchmentACS.cache_intersect = FALSE)

  out <- suppressWarnings(
    cacs_derive_rates(
      cacs_propagate_moe(
        suppressMessages(
          cacs_intersect_weight(pair$iso, pair$acs, verbose = FALSE)
        )
      ),
      formula_dispatch = "auto"
    )
  )

  rate_names <- c("poverty_rate", "snap_rate", "ssi_rate",
                  "unemp_rate", "labor_force_participation")
  rate_rows <- out[out$variable %in% rate_names, ]
  rate_rows <- rate_rows[match(rate_names, rate_rows$variable), ]
  expect_equal(rate_rows$moe_formula_requested,
               unname(catchmentACS:::.RATE_FORMULA_CATALOGUE[rate_names]))
})


test_that("T23-INT-05 canonical 27-col schema snapshot (v0.3 topology column)", {
  # Lock the canonical column order so any future re-arrangement of the long
  # schema lands as a hard test failure rather than a silent drift.
  #
  # Two counting perspectives, both valid:
  #   - Validator constant `.LONG_REQUIRED_COLS`: 23 cols (v0.3 adds
  #     `ring_topology` on top of the v0.2 F4 22-col schema).
  #   - Production long output: 27 cols (the 23 mandatory + 4 carrier numeric
  #     columns that `cacs_propagate_moe()` left-joins for MOE math:
  #     `est_total`, `var_total_raw`, `est_mean`, `var_mean_raw`).
  #
  # v0.2 added `n_tracts_num` + `n_tracts_den` after `n_tracts` per the
  # bivariate schema contract.
  pair <- .derive_int_make_pair()
  withr::local_options(catchmentACS.cache_intersect = FALSE)

  out <- suppressWarnings(
    cacs_derive_rates(
      cacs_propagate_moe(
        suppressMessages(
          cacs_intersect_weight(pair$iso, pair$acs, verbose = FALSE)
        )
      )
    )
  )

  # 23 mandatory cols (validator-locked) come first.
  mandatory <- c(
    "site_id", "drive_time_min", "ring_topology", "variable", "estimate", "moe",
    "weight_sum", "n_tracts", "n_tracts_num", "n_tracts_den",
    "provider", "profile", "osm_snapshot_date",
    "acs_year", "weight_method", "estimand_family", "weight_basis",
    "moe_formula_requested", "moe_formula_effective", "moe_fallback",
    "moe_fallback_reason", "failure_origin", "weight_uncertainty_propagated"
  )
  expect_equal(length(mandatory), 23L)
  expect_true(all(mandatory %in% names(out)),
              info = "every mandatory col must be present")

  # 4 carrier numeric cols are left-joined by `cacs_propagate_moe()` and
  # surface in the long output as well. Total = 27 cols.
  carrier_cols <- c("est_total", "var_total_raw", "est_mean", "var_mean_raw")
  expect_true(all(carrier_cols %in% names(out)),
              info = "every carrier-joined numeric col must be present")

  expect_equal(ncol(out), 27L)
})


test_that("T23-INT-04 explicit C1 downgrades non-C1-eligible rates to C2", {
  pair <- .derive_int_make_pair()
  withr::local_options(catchmentACS.cache_intersect = FALSE)

  out <- NULL
  expect_warning(
    out <- cacs_derive_rates(
      cacs_propagate_moe(
        suppressMessages(
          cacs_intersect_weight(pair$iso, pair$acs, verbose = FALSE)
        )
      ),
      formula_dispatch = "proportion_subset"
    ),
    "Downgraded rates not C1-eligible in this release",
    class = "catchmentACS_warning_runtime"
  )

  rate_names <- c("poverty_rate", "snap_rate", "ssi_rate",
                  "unemp_rate", "labor_force_participation")
  rate_rows <- out[out$variable %in% rate_names, ]
  rate_rows <- rate_rows[match(rate_names, rate_rows$variable), ]

  expect_equal(
    rate_rows$moe_formula_requested,
    c("proportion_subset", "general_ratio_conservative",
      "general_ratio_conservative", "general_ratio_conservative",
      "proportion_subset")
  )
  expect_false(any(
    rate_rows$variable %in% c("snap_rate", "ssi_rate", "unemp_rate") &
      rate_rows$moe_formula_effective == "proportion_subset"
  ))

  rp <- attr(out, "cacs_rate_provenance")
  expect_equal(rp$formula_downgraded_rates,
               c("snap_rate", "ssi_rate", "unemp_rate"))
  expect_equal(rp$n_formula_downgraded, 3L)
})
