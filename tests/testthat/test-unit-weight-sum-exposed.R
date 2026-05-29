# ============================================================================
# Unit tests for v0.2 F6 audit: lock `weight_sum` as a first-class column in
# the canonical long output (per §21.7 it was already exposed in v0.1; this
# Step 2.3 regression test makes that exposure part of the v0.2.0 contract).
#
# Why we test even though v0.1 already exposes it:
#   - The §21.7 contract is not the same as a public lock. The v0.2.0
#     release-notes call out `weight_sum` under Internal so users can rely
#     on it without grepping the source.
#   - The carrier-attribute join trap (early v0.1 design where downstream
#     callers had to read `attr(out, "cacs_aggregation_carriers")$weight_sum`
#     to recover the value) is the failure mode this test pins.
#
# Assertions:
#   1. `weight_sum` is a column in the long output of the full §21 → §22 →
#      §23 chain.
#   2. `weight_sum` is non-NA on every "good" row (source rows where the
#      upstream intersect succeeded; rate rows where the carrier resolved).
#   3. On source rows, the long-output `weight_sum` byte-equals the
#      carrier-attribute `weight_sum` for the same (site_id, drive_time_min,
#      variable) key.
#   4. Rate rows inherit `weight_sum` from the matching num+den source rows
#      via the Sec. 23.5 `min(num_ws, den_ws)` convention.
# ============================================================================


# ---- Shared fixture --------------------------------------------------------

.ws_int_make_pair <- function() {
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
        c(cx + 0.010, cy),
        c(cx + 0.010, cy + 0.008),
        c(cx,         cy + 0.008),
        c(cx,         cy)
      ))),
      crs = 4326
    )
  )

  tract_geoms <- sf::st_sfc(
    sf::st_multipolygon(list(list(rbind(
      c(cx - 0.005, cy - 0.005),
      c(cx + 0.020, cy - 0.005),
      c(cx + 0.020, cy + 0.012),
      c(cx - 0.005, cy + 0.012),
      c(cx - 0.005, cy - 0.005)
    )))),
    sf::st_multipolygon(list(list(rbind(
      c(cx + 0.020, cy - 0.005),
      c(cx + 0.040, cy - 0.005),
      c(cx + 0.040, cy + 0.012),
      c(cx + 0.020, cy + 0.012),
      c(cx + 0.020, cy - 0.005)
    )))),
    crs = 4269
  )

  rate_codes <- c(
    "B17001_002", "B17001_001",
    "B22003_002", "B22003_001",
    "B19056_002", "B19056_001",
    "B23025_005", "B23025_003",
    "B23025_002", "B23025_001"
  )
  acs_rows <- list()
  for (code in rate_codes) {
    acs_rows[[length(acs_rows) + 1L]] <- tibble::tibble(
      GEOID    = c("01001000001", "01001000002"),
      NAME     = c("Tract 1", "Tract 2"),
      variable = code,
      estimate = if (endsWith(code, "_001")) c(500, 800) else c(60, 120),
      moe      = if (endsWith(code, "_001")) c(15, 22)   else c(8,  12)
    )
  }
  acs_tbl  <- do.call(rbind, acs_rows)
  acs_4269 <- sf::st_sf(
    acs_tbl,
    geometry = sf::st_sfc(rep(tract_geoms, length(rate_codes)), crs = 4269)
  )

  list(iso = iso_4326, acs = acs_4269)
}


# ---- T-WS-01: weight_sum is a column in the canonical long output ----------

test_that("T-WS-01 weight_sum is present in cacs_intersect_weight + cacs_derive_rates outputs", {
  pair <- .ws_int_make_pair()
  withr::local_options(catchmentACS.cache_intersect = FALSE)

  inter <- suppressMessages(
    cacs_intersect_weight(pair$iso, pair$acs, verbose = FALSE)
  )
  expect_true("weight_sum" %in% names(inter))

  prop <- suppressWarnings(cacs_propagate_moe(inter))
  expect_true("weight_sum" %in% names(prop))

  out <- suppressWarnings(cacs_derive_rates(prop))
  expect_true("weight_sum" %in% names(out))
  expect_true(is.numeric(out$weight_sum))
})


# ---- T-WS-02: weight_sum is non-NA on successful rows ----------------------

test_that("T-WS-02 weight_sum is non-NA on every successful long-output row", {
  pair <- .ws_int_make_pair()
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

  # "Good" rows = those without an upstream failure marker. Empty-site /
  # carrier-missing failures legitimately carry NA weight_sum and are
  # explicitly excluded from this assertion.
  good <- out[out$failure_origin == "none" |
                (is.na(out$failure_origin) & !is.na(out$variable)), ]
  expect_gt(nrow(good), 0L)
  expect_true(all(!is.na(good$weight_sum)),
              info = "weight_sum must be non-NA on success rows")
  expect_true(all(good$weight_sum >= 0),
              info = "weight_sum must be nonnegative when populated")
})


# ---- T-WS-03: source-row weight_sum matches the carrier attribute ----------

test_that("T-WS-03 source-row weight_sum byte-equals the carrier attribute value", {
  pair <- .ws_int_make_pair()
  withr::local_options(catchmentACS.cache_intersect = FALSE)

  inter <- suppressMessages(
    cacs_intersect_weight(pair$iso, pair$acs, verbose = FALSE)
  )
  car <- attr(inter, "cacs_aggregation_carriers")
  expect_false(is.null(car))
  expect_true("weight_sum" %in% names(car))

  # Source rows in the long output (non-rate, non-empty) must match the
  # carrier's weight_sum on the 3-key (site_id, drive_time_min, variable).
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
    expect_equal(car_row$weight_sum, source_rows$weight_sum[[i]],
                 tolerance = 1e-12,
                 info = sprintf("weight_sum mismatch for %s/%s/%s",
                                sid, dt, var))
  }
})


# ---- T-WS-04: rate-row weight_sum = min(num_ws, den_ws) per Sec. 23.5 ------

test_that("T-WS-04 rate row inherits weight_sum from min(num, den) of matching source rows", {
  pair <- .ws_int_make_pair()
  withr::local_options(catchmentACS.cache_intersect = FALSE)

  inter <- suppressMessages(
    cacs_intersect_weight(pair$iso, pair$acs, verbose = FALSE)
  )
  out <- suppressWarnings(
    cacs_derive_rates(cacs_propagate_moe(inter))
  )

  for (rate_nm in names(cacs_acs_default_rates)) {
    pair_codes <- cacs_acs_default_rates[[rate_nm]]
    num_ws <- inter$weight_sum[inter$variable == pair_codes[["num"]]]
    den_ws <- inter$weight_sum[inter$variable == pair_codes[["den"]]]
    expect_length(num_ws, 1L)
    expect_length(den_ws, 1L)

    rate_row <- out[out$variable == rate_nm, ]
    expect_equal(nrow(rate_row), 1L)

    expected_ws <- min(num_ws, den_ws, na.rm = TRUE)
    expect_equal(rate_row$weight_sum, expected_ws,
                 tolerance = 1e-12,
                 info = sprintf("rate '%s' weight_sum != min(num,den)",
                                rate_nm))
  }
})
