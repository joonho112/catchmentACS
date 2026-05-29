# ===========================================================================
# test-issue003-listcol-iso-fill.R
#
# v0.4 plan Step 3.2 — public surface test for the Issue 003 fix:
# .pivot_to_list_column() now fills $isochrone from resolved isochrones on
# computed and precomputed paths.
# ===========================================================================

# We don't run cacs_run() end-to-end here (it would require live OSRM/Census
# or a heavy bundled fixture). Instead we exercise `.pivot_to_list_column()`
# directly with a hand-built `long_final` + iso fixture, then verify the
# helper invocation flow.

.mk_synth_long_final <- function(site_ids, drive_times) {
  pairs <- expand.grid(site_id = site_ids,
                       drive_time_min = drive_times,
                       stringsAsFactors = FALSE)
  pairs <- pairs[order(pairs$site_id, pairs$drive_time_min), ]
  n <- nrow(pairs)
  tibble::tibble(
    site_id           = as.character(pairs$site_id),
    drive_time_min    = as.integer(pairs$drive_time_min),
    variable          = "B01003_001",
    estimate          = 1000,
    moe               = 50,
    n_tracts          = 5L,
    n_tracts_num      = NA_integer_,
    n_tracts_den      = NA_integer_,
    provider          = "osrm",
    profile           = "car",
    osm_snapshot_date = "2025-04-01",
    acs_year          = 2023L,
    weight_method     = "area",
    estimand_family   = "spatial_total",
    failure_origin    = "none",
    ring_topology     = "cumulative",
    weight_sum        = 1.0,
    weight_basis      = "area",
    moe_formula_requested = "weighted_sum",
    moe_formula_effective = "weighted_sum",
    moe_fallback      = FALSE,
    moe_fallback_reason = "n/a",
    weight_uncertainty_propagated = FALSE
  )
}

.mk_synth_iso_sf <- function(site_ids, drive_times) {
  pairs <- expand.grid(site_id = site_ids,
                       drive_time_min = drive_times,
                       stringsAsFactors = FALSE)
  geoms <- lapply(seq_len(nrow(pairs)), function(i) {
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  })
  sf::st_sf(
    site_id        = as.character(pairs$site_id),
    drive_time_min = as.integer(pairs$drive_time_min),
    ring_topology  = "cumulative",
    geometry       = sf::st_sfc(geoms, crs = 4326)
  )
}

# --- Tests -----------------------------------------------------------------

test_that("ISSUE003-01 bypass path: 3-site iso → $isochrone populated", {
  long_final <- .mk_synth_long_final(c("S1", "S2", "S3"), 10L)
  iso <- .mk_synth_iso_sf(c("S1", "S2", "S3"), 10L)
  sites <- tibble::tibble(site_id = c("S1", "S2", "S3"),
                          lon = 1:3, lat = 1:3)
  out <- catchmentACS:::.pivot_to_list_column(long_final, sites,
                                              iso_sf = iso)
  expect_identical(nrow(out), 3L)
  expect_true(all(vapply(out$isochrone,
                          function(x) inherits(x, "sf"),
                          logical(1))))
})

test_that("ISSUE003-02 helper null contract: iso_sf = NULL leaves $isochrone NULL", {
  long_final <- .mk_synth_long_final(c("S1", "S2"), 10L)
  sites <- tibble::tibble(site_id = c("S1", "S2"), lon = 1:2, lat = 1:2)
  out <- catchmentACS:::.pivot_to_list_column(long_final, sites,
                                              iso_sf = NULL)
  expect_identical(nrow(out), 2L)
  expect_true(all(vapply(out$isochrone, is.null, logical(1))))
})

test_that("ISSUE003-03 helper default iso_sf = NULL leaves $isochrone NULL", {
  long_final <- .mk_synth_long_final("S1", 10L)
  sites <- tibble::tibble(site_id = "S1", lon = 1, lat = 1)
  # NOT passing iso_sf at all: default = NULL is the helper no-fill contract.
  out <- catchmentACS:::.pivot_to_list_column(long_final, sites)
  expect_null(out$isochrone[[1L]])
})

test_that("ISSUE003-04 multi-band: iso 2 drive_times → keyed correctly", {
  long_final <- .mk_synth_long_final("S1", c(10L, 20L))
  iso <- .mk_synth_iso_sf("S1", c(10L, 20L))
  sites <- tibble::tibble(site_id = "S1", lon = 1, lat = 1)
  out <- catchmentACS:::.pivot_to_list_column(long_final, sites, iso_sf = iso)
  expect_identical(nrow(out), 2L)
  expect_true(all(vapply(out$isochrone,
                          function(x) inherits(x, "sf") && nrow(x) == 1L,
                          logical(1))))
})

test_that("ISSUE003-05 emit captured: resolved-iso fill emits listcol_iso_filled once", {
  long_final <- .mk_synth_long_final(c("S1", "S2"), 10L)
  iso <- .mk_synth_iso_sf(c("S1", "S2"), 10L)
  sites <- tibble::tibble(site_id = c("S1", "S2"), lon = 1:2, lat = 1:2)

  cap <- cacs_capture_conditions(
    out <<- catchmentACS:::.pivot_to_list_column(long_final, sites, iso_sf = iso),
    classes = "listcol_iso_filled"
  )
  # Resolved-iso fill happened: at least 1 emit captured (helper is gated by
  # `if (!is.null(iso_sf))` at caller).
  expect_gte(nrow(cap), 1L)
})

test_that("ISSUE003-06 partial match: iso missing one site → that row NULL", {
  long_final <- .mk_synth_long_final(c("S1", "S2", "S3"), 10L)
  iso <- .mk_synth_iso_sf(c("S1", "S3"), 10L)   # S2 missing
  sites <- tibble::tibble(site_id = c("S1", "S2", "S3"), lon = 1:3, lat = 1:3)
  out <- catchmentACS:::.pivot_to_list_column(long_final, sites, iso_sf = iso)
  s1_idx <- which(out$site_id == "S1")
  s2_idx <- which(out$site_id == "S2")
  s3_idx <- which(out$site_id == "S3")
  expect_true(inherits(out$isochrone[[s1_idx]], "sf"))
  expect_null(out$isochrone[[s2_idx]])
  expect_true(inherits(out$isochrone[[s3_idx]], "sf"))
})
