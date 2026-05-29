# ============================================================================
# Unit tests for v0.2 F4: n_tracts_num + n_tracts_den columns on derived rate
# rows (per the v0.2 bivariate schema contract).
#
# Contract being tested:
#   - Rate row: both `n_tracts_num` and `n_tracts_den` are populated from the
#     carrier lookup for the rate's num/den ACS codes (per `.SANCTIONED_RATES_V1`).
#   - Source row: both columns are `NA_integer_` (per-variable rows already
#     covered by the existing `n_tracts` column).
#   - Carrier-missing edge case: both new columns collapse to `NA_integer_`
#     and the row carries `failure_origin = "carrier"`.
#   - The existing `n_tracts` column on rate rows remains `NA_integer_`
#     (preserves the v0.1 `is.na(n_tracts)` rate-row filter idiom).
#
# Fixture builder mirrors the §21 → §22 → §23 end-to-end chain used by
# `test-integration-derive-rates.R` so the carrier lookup is reachable
# without a network round-trip.
# ============================================================================


# ---- Shared fixture builder ------------------------------------------------

# Build a tiny end-to-end pipeline pair (iso_sf + acs_sf) populated for every
# ACS code referenced by `.SANCTIONED_RATES_V1` so all 5 rates resolve to
# carrier hits. Anchored at CONUS-valid (-86.80, 33.50).
.n_tracts_int_make_pair <- function() {
  cx <- -86.80; cy <- 33.50

  iso_4326 <- sf::st_sf(
    tibble::tibble(
      site_id                          = "S01",
      drive_time_min                   = 15L,
      provider                         = "osrm",
      profile                          = "car",
      osm_snapshot_date                = NA_character_,
      routing_engine_version           = "stub/0.0.0",
      polygon_simplification_tolerance = NA_real_,
      generated_at                     = as.POSIXct("2026-05-23 00:00:00",
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
        c(cx + 0.020, cy),
        c(cx + 0.020, cy + 0.010),
        c(cx,         cy + 0.010),
        c(cx,         cy)
      ))),
      crs = 4326
    )
  )

  # 3 tract geometries (so n_tracts can plausibly vary if a numerator and
  # denominator come from different ACS universes with different presence).
  tract_geoms <- sf::st_sfc(
    sf::st_multipolygon(list(list(rbind(
      c(cx - 0.005, cy - 0.005),
      c(cx + 0.010, cy - 0.005),
      c(cx + 0.010, cy + 0.015),
      c(cx - 0.005, cy + 0.015),
      c(cx - 0.005, cy - 0.005)
    )))),
    sf::st_multipolygon(list(list(rbind(
      c(cx + 0.010, cy - 0.005),
      c(cx + 0.020, cy - 0.005),
      c(cx + 0.020, cy + 0.015),
      c(cx + 0.010, cy + 0.015),
      c(cx + 0.010, cy - 0.005)
    )))),
    sf::st_multipolygon(list(list(rbind(
      c(cx + 0.020, cy - 0.005),
      c(cx + 0.030, cy - 0.005),
      c(cx + 0.030, cy + 0.015),
      c(cx + 0.020, cy + 0.015),
      c(cx + 0.020, cy - 0.005)
    )))),
    crs = 4269
  )
  tract_ids   <- c("01001000001", "01001000002", "01001000003")
  tract_names <- paste0("Census Tract ", seq_along(tract_ids))

  rate_codes <- c(
    "B17001_002", "B17001_001",  # poverty num / den
    "B22003_002", "B22003_001",  # snap num / den
    "B19056_002", "B19056_001",  # ssi num / den
    "B23025_005", "B23025_003",  # unemp num / den
    "B23025_002", "B23025_001"   # labor force participation num / den
  )

  # Build one tidy ACS row per (code, tract); deterministic numeric values
  # so downstream byte-stable.
  acs_rows <- list()
  for (code in rate_codes) {
    acs_rows[[length(acs_rows) + 1L]] <- tibble::tibble(
      GEOID    = tract_ids,
      NAME     = tract_names,
      variable = code,
      estimate = if (endsWith(code, "_001")) c(500, 800, 1000)
                 else c(60, 120, 100),
      moe      = if (endsWith(code, "_001")) c(15, 22, 30)
                 else c(8,  12, 10)
    )
  }
  acs_tbl  <- do.call(rbind, acs_rows)
  acs_4269 <- sf::st_sf(
    acs_tbl,
    geometry = sf::st_sfc(rep(tract_geoms, length(rate_codes)), crs = 4269)
  )

  list(iso = iso_4326, acs = acs_4269)
}


# ---- T-F4-01: Rate rows have both n_tracts_num + n_tracts_den non-NA -------

test_that("T-F4-01 derived rate rows populate both n_tracts_num and n_tracts_den", {
  pair <- .n_tracts_int_make_pair()
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

  rate_names <- c("poverty_rate", "snap_rate", "ssi_rate",
                  "unemp_rate", "labor_force_participation")
  rate_rows <- out[out$variable %in% rate_names, ]

  expect_equal(nrow(rate_rows), 5L)
  expect_true(all(!is.na(rate_rows$n_tracts_num)),
              info = "every rate row must carry a non-NA n_tracts_num")
  expect_true(all(!is.na(rate_rows$n_tracts_den)),
              info = "every rate row must carry a non-NA n_tracts_den")
  expect_true(is.integer(rate_rows$n_tracts_num))
  expect_true(is.integer(rate_rows$n_tracts_den))

  # Existing n_tracts on rate rows is still NA_integer_ (backward compat
  # for the v0.1 `is.na(n_tracts)` rate-row filter idiom).
  expect_true(all(is.na(rate_rows$n_tracts)))
})


# ---- T-F4-02: Source rows carry NA on both new columns ---------------------

test_that("T-F4-02 source (non-rate) rows have NA on n_tracts_num and n_tracts_den", {
  pair <- .n_tracts_int_make_pair()
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

  source_rows <- out[out$estimand_family != "derived_rate" |
                       is.na(out$estimand_family), ]

  expect_gt(nrow(source_rows), 0L)
  expect_true(all(is.na(source_rows$n_tracts_num)),
              info = "source rows must not populate n_tracts_num (only rate rows do)")
  expect_true(all(is.na(source_rows$n_tracts_den)),
              info = "source rows must not populate n_tracts_den (only rate rows do)")
})


# ---- T-F4-03: num vs den can differ when universes diverge -----------------

test_that("T-F4-03 ssi_rate's n_tracts_num and n_tracts_den come from different ACS universes", {
  pair <- .n_tracts_int_make_pair()
  withr::local_options(catchmentACS.cache_intersect = FALSE)

  inter <- suppressMessages(
    cacs_intersect_weight(pair$iso, pair$acs, verbose = FALSE)
  )
  car <- attr(inter, "cacs_aggregation_carriers")

  # ssi_rate = c(num = "B19056_002", den = "B19056_001"). Both must be
  # present in the carrier so the lookup resolves; the n_tracts values are
  # the per-code aggregation counts (free to diverge in general).
  num_n <- car$n_tracts[car$variable == "B19056_002"]
  den_n <- car$n_tracts[car$variable == "B19056_001"]
  expect_length(num_n, 1L)
  expect_length(den_n, 1L)
  expect_true(is.integer(num_n))
  expect_true(is.integer(den_n))

  # End-to-end: the rate row reflects exactly these two carrier values.
  out <- suppressWarnings(cacs_derive_rates(cacs_propagate_moe(inter)))
  ssi <- out[out$variable == "ssi_rate", ]
  expect_equal(nrow(ssi), 1L)
  expect_equal(ssi$n_tracts_num, num_n)
  expect_equal(ssi$n_tracts_den, den_n)
})


# ---- T-F4-04: source-row consistency - carrier n_tracts matches source rows -

test_that("T-F4-04 carrier n_tracts equals the n_tracts on the matching source row", {
  pair <- .n_tracts_int_make_pair()
  withr::local_options(catchmentACS.cache_intersect = FALSE)

  inter <- suppressMessages(
    cacs_intersect_weight(pair$iso, pair$acs, verbose = FALSE)
  )
  car <- attr(inter, "cacs_aggregation_carriers")

  # Join carrier ↔ source rows on the 3-key (site_id, drive_time_min,
  # variable) and assert byte-equality of n_tracts on every matched row.
  source_rows <- inter[!is.na(inter$variable) &
                         inter$estimand_family != "derived_rate", ]
  for (i in seq_len(nrow(source_rows))) {
    sid <- source_rows$site_id[[i]]
    dt  <- source_rows$drive_time_min[[i]]
    var <- source_rows$variable[[i]]
    car_row <- car[car$site_id == sid &
                    car$drive_time_min == dt &
                    car$variable == var, ]
    expect_equal(nrow(car_row), 1L,
                 info = sprintf("expected 1 carrier match for %s/%s/%s",
                                sid, dt, var))
    expect_equal(car_row$n_tracts, source_rows$n_tracts[[i]],
                 info = sprintf("n_tracts mismatch for %s/%s/%s",
                                sid, dt, var))
  }
})


# ---- T-F4-05: all 5 default rates correctly populate both cols -------------

test_that("T-F4-05 all 5 cacs_acs_default_rates have n_tracts_num + n_tracts_den populated", {
  pair <- .n_tracts_int_make_pair()
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

  for (rate_nm in names(cacs_acs_default_rates)) {
    row <- out[out$variable == rate_nm, ]
    expect_equal(nrow(row), 1L,
                 info = sprintf("rate '%s' should produce exactly 1 row",
                                rate_nm))
    expect_false(is.na(row$n_tracts_num),
                 info = sprintf("'%s' missing n_tracts_num", rate_nm))
    expect_false(is.na(row$n_tracts_den),
                 info = sprintf("'%s' missing n_tracts_den", rate_nm))
    expect_true(is.integer(row$n_tracts_num),
                info = sprintf("'%s' n_tracts_num not integer", rate_nm))
    expect_true(is.integer(row$n_tracts_den),
                info = sprintf("'%s' n_tracts_den not integer", rate_nm))
  }
})


# ---- T-F4-06: carrier-missing edge case propagates both as NA --------------

test_that("T-F4-06 carrier-missing rate row has both n_tracts_num and n_tracts_den as NA", {
  pair <- .n_tracts_int_make_pair()
  withr::local_options(catchmentACS.cache_intersect = FALSE)

  inter <- suppressMessages(
    cacs_intersect_weight(pair$iso, pair$acs, verbose = FALSE)
  )
  prop  <- suppressWarnings(cacs_propagate_moe(inter))

  # Surgically drop poverty's numerator (B17001_002) from the carrier
  # attribute so poverty_rate hits the carrier-missing branch.
  car <- attr(prop, "cacs_aggregation_carriers")
  car <- car[car$variable != "B17001_002", ]
  attr(prop, "cacs_aggregation_carriers") <- car

  out <- suppressWarnings(cacs_derive_rates(prop))
  poverty <- out[out$variable == "poverty_rate", ]

  expect_equal(nrow(poverty), 1L)
  expect_equal(poverty$failure_origin, "carrier")
  expect_true(is.na(poverty$n_tracts_num),
              info = "carrier-missing rate row must have NA n_tracts_num")
  expect_true(is.na(poverty$n_tracts_den),
              info = "carrier-missing rate row must have NA n_tracts_den")
  expect_true(is.integer(poverty$n_tracts_num))   # NA_integer_, not NA
  expect_true(is.integer(poverty$n_tracts_den))
})
