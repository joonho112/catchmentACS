# ============================================================================
# Integration tests for cacs_run() Steps 7.2 + 7.3 — 5-call sequence +
# output dispatch + attribute carry-through (Sec. 24.3 steps 5-12).
#
# Step 7.2 wired the 5 sub-call orchestrator (steps 5-11) on top of the
# Step 7.1 pre-dispatch (thin validation + execution path + arg list
# validation). Step 7.3 unblocks the Step 12 boundary by adding 3-mode
# output dispatch (`"long"` / `"list_column"` / `"both"`), the
# `.pivot_to_list_column()` helper (Sec. 12.3.2 8-col schema), and the
# 4 run-level attributes (`cacs_schema_version`, `cacs_run_provenance`
# 16 fields per Sec. 24.8, `cacs_run_warnings` phase-keyed list, and
# sub-call provenance preservation).
#
# Tests in this file therefore inspect:
#   - sub-call invocation count (verifies bypass logic per §24.4)
#   - sub-call invocation order (verifies the explicit phase sequence)
#   - warning capture into `warning_log$entries` (verifies §24.7 contract)
#   - partial-success / E-24-15 abort (verifies §24.6 + §24.8 contract)
#   - 3-mode output dispatch + 16-field provenance + warning attribute
#     attach (verifies §24.5 + §24.8 contract for Step 7.3)
#
# Mocking strategy: testthat::local_mocked_bindings(.package = "catchmentACS")
# substitutes each of the 5 exported sub-functions with a counting stub that
# returns a fixture-shaped value. This isolates Step 7.2 / 7.3's orchestration
# layer from §19-§23 sub-function correctness (those are exercised by
# test-integration-acs-prefetch.R, test-integration-isochrone-*.R, and
# test-integration-derive-rates.R).
#
# Cases:
#   T24-01  5-call full path                    — 5 sub-calls in order
#   T24-02  4-call-A bypass (precomputed iso)   — §19 not called, M-24-03
#   T24-03  4-call-B bypass (acs supplied)      — §20 not called, M-24-02
#   T24-04  3-call full bypass                  — §19 + §20 not called
#   T24-05  partial-success NA propagation      — W-24-02 + NA rows
#   T24-06  warning aggregation phase-keyed     — captured per-phase
#   T24-07  catastrophic all-fail E-24-15       — operator-class abort
#   T24-09  output = "long" passthrough         — long tibble + 4 attrs
#   T24-10  output = "list_column" 8-col format — Sec. 12.3.2 schema
#   T24-11  output = "both" named list          — list(long, list_column)
#   T24-11b 4-call-B list_column iso fill       — computed iso backfill
#   T24-11c partial-failure list_column iso     — failed-pair diagnostics
#   T24-12  cacs_run_provenance 16 fields       — §24.8 contract
#   T24-13  cacs_run_warnings phase-keyed       — §24.7 contract
#
# Cross-ref: Sec. 24.3 (12-step algorithm), Sec. 24.4 (bypass paths),
# Sec. 24.5 (output dispatch), Sec. 24.6 (partial-success NA), Sec. 24.7
# (warning aggregation), Sec. 24.8 (error catalog + provenance), Sec. 12.3.2
# (list-column 8-col schema), Sec. 42.4-42.5 (Step 7.2/7.3 impl guide).
# ============================================================================


# ---- Shared fixtures ------------------------------------------------------
#
# `.run_sites_fix(n)` builds a Step 7.1-compliant `sites` tibble (n rows).
# `.run_iso_fix(...)` builds a minimal §19 16-col canonical sf so `cacs_run()`
#   step 7 (`iso_summary` group_by) finds the required columns
#   (`site_id`, `drive_time_min`, `isochrone_empty`, `failure_reason`).
# `.run_acs_fix()` builds a minimal §20 6-col canonical sf (only the columns
#   §21 step 1 validates).
# `.run_weighted_fix()` builds a minimal §21-style tibble with the
#   `cacs_aggregation_carriers` attribute so the §22 / §23 stubs do not
#   need to re-create the carrier contract (they are mocked anyway).

.run_sites_fix <- function(n = 1L,
                           site_ids = paste0("S", sprintf("%02d", seq_len(n)))) {
  tibble::tibble(
    site_id = site_ids,
    lon     = rep(-86.80902, n),
    lat     = rep( 33.52203, n)
  )
}

.run_iso_fix <- function(site_ids        = "S01",
                          drive_times     = c(5L, 10L, 15L),
                          isochrone_empty = FALSE,
                          failure_reason  = NA_character_) {
  rows <- expand.grid(site_id        = site_ids,
                      drive_time_min = drive_times,
                      KEEP.OUT.ATTRS   = FALSE,
                      stringsAsFactors = FALSE)
  n_row <- nrow(rows)
  # Recycle isochrone_empty / failure_reason to row count so callers can
  # mark specific (site, drive_time) pairs as failed.
  isoempty <- rep_len(isochrone_empty, n_row)
  freason  <- rep_len(failure_reason,  n_row)

  geom_list <- lapply(seq_len(n_row), function(i) {
    sf::st_polygon(list(rbind(
      c(-86.81, 33.52), c(-86.80, 33.52),
      c(-86.80, 33.53), c(-86.81, 33.53), c(-86.81, 33.52)
    )))
  })

  sf::st_sf(
    tibble::tibble(
      site_id                          = rows$site_id,
      drive_time_min                   = as.integer(rows$drive_time_min),
      provider                         = "osrm",
      profile                          = "car",
      osm_snapshot_date                = NA_character_,
      routing_engine_version           = "stub/0.0.0",
      polygon_simplification_tolerance = NA_real_,
      generated_at                     = as.POSIXct("2026-05-22 00:00:00",
                                                     tz = "UTC"),
      isochrone_empty                  = isoempty,
      provider_requested               = "osrm",
      provider_downgrade               = FALSE,
      osm_snapshot_status              = NA_character_,
      failure_reason                   = freason,
      retry_count                      = 1L,
      ring_topology                    = "cumulative"
    ),
    geometry = sf::st_sfc(geom_list, crs = 4326)
  )
}

.run_acs_fix <- function(variables = "B17001_002") {
  rows <- expand.grid(GEOID    = c("01001000100", "01001000200"),
                      variable = variables,
                      KEEP.OUT.ATTRS = FALSE,
                      stringsAsFactors = FALSE)
  rows$NAME     <- paste0("Tract ", rows$GEOID)
  rows$estimate <- as.numeric(seq_len(nrow(rows)) * 100L)
  rows$moe      <- as.numeric(seq_len(nrow(rows)) * 10L)
  geom <- sf::st_sfc(
    lapply(seq_len(nrow(rows)), function(i) {
      lon0 <- -86.81 + 0.005 * i
      lat0 <-  33.50 + 0.005 * i
      sf::st_polygon(list(rbind(
        c(lon0,        lat0),
        c(lon0 + 0.01, lat0),
        c(lon0 + 0.01, lat0 + 0.01),
        c(lon0,        lat0 + 0.01),
        c(lon0,        lat0)
      )))
    }),
    crs = 4269
  )
  sf::st_sf(rows, geometry = geom)
}

# Build a Step 5.4-shaped long tibble for the §22 -> §23 stub stack to
# return. Includes the §24.6 byte-identical 18 mandatory columns the
# orchestrator's NA-propagation logic references.
.run_long_fix <- function(site_ids       = "S01",
                          drive_times    = c(5L, 10L, 15L),
                          variables      = "B17001_002",
                          rate_names     = names(catchmentACS:::.SANCTIONED_RATES_V1),
                          include_rates  = TRUE) {
  vars <- if (include_rates) c(variables, rate_names) else variables
  rows <- expand.grid(site_id        = site_ids,
                      drive_time_min = drive_times,
                      variable       = vars,
                      KEEP.OUT.ATTRS = FALSE,
                      stringsAsFactors = FALSE)
  n_row <- nrow(rows)
  fam <- ifelse(rows$variable %in% rate_names,
                "derived_rate", "spatial_total")

  tibble::tibble(
    site_id                       = rows$site_id,
    drive_time_min                = as.integer(rows$drive_time_min),
    ring_topology                 = "cumulative",
    variable                      = rows$variable,
    estimate                      = as.numeric(seq_len(n_row)),
    moe                           = as.numeric(seq_len(n_row)) * 0.1,
    se                            = as.numeric(seq_len(n_row)) * 0.05,
    weight_sum                    = rep(0.95, n_row),
    n_tracts                      = rep(2L,  n_row),
    n_tracts_num                  = ifelse(fam == "derived_rate", 2L, NA_integer_),
    n_tracts_den                  = ifelse(fam == "derived_rate", 2L, NA_integer_),
    provider                      = "osrm",
    profile                       = "car",
    osm_snapshot_date             = NA_character_,
    acs_year                      = 2023L,
    weight_method                 = "area",
    estimand_family               = fam,
    weight_basis                  = "coverage",
    moe_formula_requested         = "auto",
    moe_formula_effective         = ifelse(fam == "derived_rate",
                                           "general_ratio_conservative",
                                           "weighted_sum"),
    moe_fallback                  = FALSE,
    moe_fallback_reason           = NA_character_,
    failure_origin                = NA_character_,
    weight_uncertainty_propagated = FALSE
  )
}


# ===========================================================================
# T24-01  5-call full path — all 5 sub-functions are invoked in order
# ===========================================================================

test_that("T24-01 5-call full path invokes all 5 sub-functions in order", {
  sites <- .run_sites_fix(n = 1L)

  call_log <- character(0)

  stub_acs <- function(state, year, variables, cache_dir = NULL,
                       verbose = TRUE, ...) {
    call_log <<- c(call_log, "acs_prefetch")
    .run_acs_fix(variables = variables %||% "B17001_002")
  }
  stub_iso <- function(sites, drive_times = c(5, 10, 15),
                       provider = "osrm", cache_dir = NULL, ...) {
    call_log <<- c(call_log, "isochrone")
    .run_iso_fix(site_ids = unique(sites$site_id),
                 drive_times = as.integer(drive_times))
  }
  stub_iw  <- function(iso_sf, acs_sf, bg_pop_sf = NULL,
                       weight_method = "area", verbose = TRUE, ...) {
    call_log <<- c(call_log, "intersect_weight")
    out <- .run_long_fix(
      site_ids    = unique(iso_sf$site_id),
      drive_times = sort(unique(iso_sf$drive_time_min)),
      variables   = unique(acs_sf$variable),
      include_rates = FALSE
    )
    # Mimic §21 carrier attribute so §23 stub can pretend the contract held.
    attr(out, "cacs_aggregation_carriers") <- tibble::tibble(
      site_id        = character(0),
      drive_time_min = integer(0)
    )
    out
  }
  stub_moe <- function(data, formula = NULL, level = 0.90, ...) {
    call_log <<- c(call_log, "propagate_moe")
    data
  }
  stub_rates <- function(weighted_acs, rates, formula_dispatch, ...) {
    call_log <<- c(call_log, "derive_rates")
    .run_long_fix(
      site_ids    = unique(weighted_acs$site_id),
      drive_times = sort(unique(weighted_acs$drive_time_min)),
      variables   = setdiff(unique(weighted_acs$variable), names(rates)),
      rate_names  = names(rates),
      include_rates = TRUE
    )
  }

  testthat::local_mocked_bindings(
    cacs_acs_prefetch     = stub_acs,
    cacs_isochrone        = stub_iso,
    cacs_intersect_weight = stub_iw,
    cacs_propagate_moe    = stub_moe,
    cacs_derive_rates     = stub_rates,
    .package = "catchmentACS"
  )

  out <- suppressMessages(suppressWarnings(
    cacs_run(sites = sites, state = "AL", variables = "B17001_002",
             verbose = FALSE)
  ))

  # Step 7.3 returns the long tibble; no operator-class abort.
  expect_s3_class(out, "tbl_df")
  expect_true("variable" %in% colnames(out))

  # All 5 phases ran, in canonical order.
  expect_equal(
    call_log,
    c("acs_prefetch", "isochrone", "intersect_weight",
      "propagate_moe", "derive_rates")
  )

  # execution_path provenance shows the 5-call branch was taken.
  expect_equal(attr(out, "cacs_run_provenance")$execution_path, "5-call")
})


# ===========================================================================
# T24-02  4-call-A bypass — precomputed isochrones supplied, §19 skipped
# ===========================================================================

test_that("T24-02 4-call-A bypass (precomputed iso) skips cacs_isochrone()", {
  sites <- .run_sites_fix(n = 1L)
  iso_in <- .run_iso_fix(site_ids = "S01")

  call_log <- character(0)

  stub_acs <- function(...) {
    call_log <<- c(call_log, "acs_prefetch")
    .run_acs_fix()
  }
  stub_iso <- function(...) {
    call_log <<- c(call_log, "isochrone")  # should NOT be added
    stop("cacs_isochrone() was called despite bypass!")
  }
  stub_iw  <- function(iso_sf, acs_sf, ...) {
    call_log <<- c(call_log, "intersect_weight")
    out <- .run_long_fix(include_rates = FALSE)
    attr(out, "cacs_aggregation_carriers") <- tibble::tibble()
    out
  }
  stub_moe <- function(data, ...) {
    call_log <<- c(call_log, "propagate_moe"); data
  }
  stub_rates <- function(weighted_acs, rates, ...) {
    call_log <<- c(call_log, "derive_rates")
    .run_long_fix(include_rates = TRUE)
  }

  testthat::local_mocked_bindings(
    cacs_acs_prefetch     = stub_acs,
    cacs_isochrone        = stub_iso,
    cacs_intersect_weight = stub_iw,
    cacs_propagate_moe    = stub_moe,
    cacs_derive_rates     = stub_rates,
    .package = "catchmentACS"
  )

  out <- suppressMessages(suppressWarnings(
    cacs_run(sites = sites, state = "AL",
             precomputed_isochrones = iso_in, verbose = FALSE)
  ))
  expect_s3_class(out, "tbl_df")
  expect_equal(attr(out, "cacs_run_provenance")$execution_path, "4-call-A")

  # 4 calls; cacs_isochrone() NOT among them.
  expect_equal(
    call_log,
    c("acs_prefetch", "intersect_weight", "propagate_moe", "derive_rates")
  )

  # M-24-03 fires under verbose = TRUE on bypass.
  expect_message(
    suppressWarnings(
      cacs_run(sites = sites, state = "AL",
               precomputed_isochrones = iso_in, verbose = TRUE)
    ),
    "Isochrone bypass",
    class = "catchmentACS_message_progress"
  )
})


# ===========================================================================
# T24-03  4-call-B bypass — user-supplied acs, §20 skipped
# ===========================================================================

test_that("T24-03 4-call-B bypass (acs supplied) skips cacs_acs_prefetch()", {
  sites  <- .run_sites_fix(n = 1L)
  acs_in <- .run_acs_fix(variables = "B17001_002")

  call_log <- character(0)

  stub_acs <- function(...) {
    call_log <<- c(call_log, "acs_prefetch")  # should NOT be added
    stop("cacs_acs_prefetch() was called despite bypass!")
  }
  stub_iso <- function(sites, ...) {
    call_log <<- c(call_log, "isochrone")
    .run_iso_fix(site_ids = unique(sites$site_id))
  }
  stub_iw  <- function(iso_sf, acs_sf, ...) {
    call_log <<- c(call_log, "intersect_weight")
    out <- .run_long_fix(include_rates = FALSE)
    attr(out, "cacs_aggregation_carriers") <- tibble::tibble()
    out
  }
  stub_moe <- function(data, ...) {
    call_log <<- c(call_log, "propagate_moe"); data
  }
  stub_rates <- function(weighted_acs, rates, ...) {
    call_log <<- c(call_log, "derive_rates")
    .run_long_fix(include_rates = TRUE)
  }

  testthat::local_mocked_bindings(
    cacs_acs_prefetch     = stub_acs,
    cacs_isochrone        = stub_iso,
    cacs_intersect_weight = stub_iw,
    cacs_propagate_moe    = stub_moe,
    cacs_derive_rates     = stub_rates,
    .package = "catchmentACS"
  )

  out <- suppressMessages(suppressWarnings(
    cacs_run(sites = sites, state = "AL", acs = acs_in, verbose = FALSE)
  ))
  expect_s3_class(out, "tbl_df")
  expect_equal(attr(out, "cacs_run_provenance")$execution_path, "4-call-B")

  # 4 calls; cacs_acs_prefetch() NOT among them.
  expect_equal(
    call_log,
    c("isochrone", "intersect_weight", "propagate_moe", "derive_rates")
  )

  # M-24-02 fires under verbose = TRUE on bypass.
  expect_message(
    suppressWarnings(
      cacs_run(sites = sites, state = "AL", acs = acs_in, verbose = TRUE)
    ),
    "ACS bypass",
    class = "catchmentACS_message_progress"
  )
})


# ===========================================================================
# T24-04  3-call full bypass — both precomputed; §19 + §20 skipped
# ===========================================================================

test_that("T24-04 3-call full bypass skips both cacs_acs_prefetch and cacs_isochrone", {
  sites  <- .run_sites_fix(n = 1L)
  iso_in <- .run_iso_fix(site_ids = "S01")
  acs_in <- .run_acs_fix(variables = "B17001_002")

  call_log <- character(0)

  stub_acs <- function(...) {
    call_log <<- c(call_log, "acs_prefetch")
    stop("cacs_acs_prefetch() was called despite full bypass!")
  }
  stub_iso <- function(...) {
    call_log <<- c(call_log, "isochrone")
    stop("cacs_isochrone() was called despite full bypass!")
  }
  stub_iw  <- function(iso_sf, acs_sf, ...) {
    call_log <<- c(call_log, "intersect_weight")
    out <- .run_long_fix(include_rates = FALSE)
    attr(out, "cacs_aggregation_carriers") <- tibble::tibble()
    out
  }
  stub_moe <- function(data, ...) {
    call_log <<- c(call_log, "propagate_moe"); data
  }
  stub_rates <- function(weighted_acs, rates, ...) {
    call_log <<- c(call_log, "derive_rates")
    .run_long_fix(include_rates = TRUE)
  }

  testthat::local_mocked_bindings(
    cacs_acs_prefetch     = stub_acs,
    cacs_isochrone        = stub_iso,
    cacs_intersect_weight = stub_iw,
    cacs_propagate_moe    = stub_moe,
    cacs_derive_rates     = stub_rates,
    .package = "catchmentACS"
  )

  out <- suppressMessages(suppressWarnings(
    cacs_run(sites = sites, state = "AL",
             precomputed_isochrones = iso_in, acs = acs_in,
             verbose = FALSE)
  ))
  expect_s3_class(out, "tbl_df")
  expect_equal(attr(out, "cacs_run_provenance")$execution_path, "3-call")
  expect_true(attr(out, "cacs_run_provenance")$bypass_iso)
  expect_true(attr(out, "cacs_run_provenance")$bypass_acs)

  # Only 3 calls.
  expect_equal(
    call_log,
    c("intersect_weight", "propagate_moe", "derive_rates")
  )
})


# ===========================================================================
# T24-05  Partial-success — 1 of 3 sites has empty isochrone, NA-rows added
# ===========================================================================

test_that("T24-05 partial-success: 1 failed pair triggers W-24-02 + NA propagation", {
  sites <- .run_sites_fix(n = 3L,
                          site_ids = c("S01", "S02", "S03"))

  # Build an iso where S03 / 5 min pair is empty (1 failed pair of 9).
  iso_full <- .run_iso_fix(site_ids        = c("S01", "S02", "S03"),
                            drive_times    = c(5L, 10L, 15L))
  iso_full$isochrone_empty[iso_full$site_id == "S03" &
                              iso_full$drive_time_min == 5L] <- TRUE

  intersect_input <- NULL  # capture what reaches §21
  stub_iso <- function(...) iso_full

  stub_acs <- function(...) .run_acs_fix()
  stub_iw  <- function(iso_sf, acs_sf, ...) {
    intersect_input <<- iso_sf
    pairs <- unique(sf::st_drop_geometry(iso_sf)[, c("site_id", "drive_time_min")])
    out <- dplyr::bind_rows(lapply(seq_len(nrow(pairs)), function(i) {
      .run_long_fix(
        site_ids      = pairs$site_id[[i]],
        drive_times   = pairs$drive_time_min[[i]],
        include_rates = FALSE
      )
    }))
    attr(out, "cacs_aggregation_carriers") <- tibble::tibble()
    out
  }
  stub_moe <- function(data, ...) data
  stub_rates <- function(weighted_acs, rates, ...) {
    pairs <- unique(weighted_acs[, c("site_id", "drive_time_min")])
    dplyr::bind_rows(lapply(seq_len(nrow(pairs)), function(i) {
      .run_long_fix(
        site_ids      = pairs$site_id[[i]],
        drive_times   = pairs$drive_time_min[[i]],
        variables     = setdiff(unique(weighted_acs$variable), names(rates)),
        rate_names    = names(rates),
        include_rates = TRUE
      )
    }))
  }

  testthat::local_mocked_bindings(
    cacs_acs_prefetch     = stub_acs,
    cacs_isochrone        = stub_iso,
    cacs_intersect_weight = stub_iw,
    cacs_propagate_moe    = stub_moe,
    cacs_derive_rates     = stub_rates,
    .package = "catchmentACS"
  )

  # W-24-02 must fire for the partial failure.
  out <- NULL
  expect_warning(
    out <- cacs_run(sites = sites, state = "AL", verbose = FALSE),
    "Partial isochrone failure",
    class = "catchmentACS_warning_partial"
  )

  # The mock captured iso_sf_valid (semi_join restricted to 8 valid pairs).
  expect_false(is.null(intersect_input))
  pair_keys <- unique(paste(intersect_input$site_id,
                            intersect_input$drive_time_min, sep = "/"))
  expect_false("S03/5" %in% pair_keys)
  expect_equal(length(pair_keys), 8L)

  failed <- out[out$site_id == "S03" & out$drive_time_min == 5L, ]
  expect_gt(nrow(failed), 0L)
  expect_setequal(
    failed$variable,
    c("B17001_002", names(catchmentACS:::.SANCTIONED_RATES_V1))
  )
  expect_true(all(is.na(failed$estimate)))
  expect_true(all(is.na(failed$moe)))
  expect_true(all(is.na(failed$weight_sum)))
  expect_true(all(is.na(failed$n_tracts)))
  expect_true(all(failed$failure_origin == "isochrone"))
  expect_true(all(failed$moe_fallback_reason == "n/a"))

  warn_attr <- attr(out, "cacs_run_warnings")
  expect_type(warn_attr, "list")
  expect_equal(warn_attr$orchestrator[[1]]$n_failed, 1L)
})


test_that("T24-05b batch-level isochrone errors abort instead of NA-propagating", {
  sites <- .run_sites_fix(n = 3L,
                          site_ids = c("S01", "S02", "S03"))

  intersect_called <- FALSE
  stub_acs <- function(...) .run_acs_fix()
  stub_iso <- function(...) {
    catchmentACS:::.cli_abort_network(c(
      "Provider batch failure.",
      "x" = "Synthetic T24-05b network abort."
    ))
  }
  stub_iw <- function(...) {
    intersect_called <<- TRUE
    .run_long_fix(include_rates = FALSE)
  }

  testthat::local_mocked_bindings(
    cacs_acs_prefetch     = stub_acs,
    cacs_isochrone        = stub_iso,
    cacs_intersect_weight = stub_iw,
    .package = "catchmentACS"
  )

  err <- tryCatch(
    cacs_run(sites = sites, state = "AL", verbose = FALSE),
    error = function(e) e
  )

  expect_s3_class(err, "catchmentACS_error_network")
  expect_false(intersect_called)
})


# ===========================================================================
# T24-06  Warning aggregation — sub-call cli_warn lands in phase-keyed log
# ===========================================================================
#
# The §22 stub emits a cli_warn; .call_with_capture() must capture it into
# warning_log$entries$propagate_moe without preventing it from reaching the
# console (so user sees it live). We assert the capture by inspecting the
# environment via a bare-bones internal test path: re-call .call_with_capture
# directly with a controlled fn.

test_that("T24-06 .call_with_capture captures cli_warn into phase-keyed log", {
  warning_log <- new.env(parent = emptyenv())
  warning_log$entries <- list(
    isochrone        = list(),
    acs_prefetch     = list(),
    intersect_weight = list(),
    propagate_moe    = list(),
    derive_rates     = list(),
    orchestrator     = list()
  )

  noisy_stub <- function(x) {
    cli::cli_warn("Sub-call test warning: x = {x}",
                  class = c("catchmentACS_warning_runtime",
                            "catchmentACS_warning",
                            "catchmentACS_condition"))
    x * 2
  }

  out <- suppressWarnings(
    catchmentACS:::.call_with_capture(
      fn          = noisy_stub,
      args        = list(x = 5),
      warning_log = warning_log,
      phase       = "propagate_moe"
    )
  )

  expect_equal(out, 10)
  # Captured in propagate_moe phase, no other phases polluted.
  expect_equal(length(warning_log$entries$propagate_moe), 1L)
  expect_equal(length(warning_log$entries$intersect_weight), 0L)
  expect_equal(length(warning_log$entries$derive_rates), 0L)

  entry <- warning_log$entries$propagate_moe[[1L]]
  expect_match(entry$message, "Sub-call test warning")
  expect_true("catchmentACS_warning_runtime" %in% entry$class)
  expect_equal(entry$phase, "propagate_moe")
})


# ===========================================================================
# T24-07  Catastrophic all-fail — every pair fails -> E-24-15 abort
# ===========================================================================

test_that("T24-07 catastrophic all-fail aborts with E-24-15 (operator)", {
  sites <- .run_sites_fix(n = 2L, site_ids = c("S01", "S02"))

  # Build an iso where every (site, drive_time) pair is empty (4/4 failed).
  iso_all_fail <- .run_iso_fix(site_ids        = c("S01", "S02"),
                                drive_times    = c(5L, 10L),
                                isochrone_empty = TRUE)

  stub_acs <- function(...) .run_acs_fix()
  stub_iso <- function(...) iso_all_fail
  # downstream stubs should never be reached after E-24-15
  stub_iw <- function(...) stop("Reached intersect_weight after E-24-15!")
  stub_moe <- function(...) stop("Reached propagate_moe after E-24-15!")
  stub_rates <- function(...) stop("Reached derive_rates after E-24-15!")

  testthat::local_mocked_bindings(
    cacs_acs_prefetch     = stub_acs,
    cacs_isochrone        = stub_iso,
    cacs_intersect_weight = stub_iw,
    cacs_propagate_moe    = stub_moe,
    cacs_derive_rates     = stub_rates,
    .package = "catchmentACS"
  )

  err <- tryCatch(
    suppressMessages(suppressWarnings(
      cacs_run(sites = sites, state = "AL", drive_times = c(5, 10),
               verbose = FALSE)
    )),
    error = function(e) e
  )

  expect_s3_class(err, "catchmentACS_error_operator")
  expect_match(conditionMessage(err), "All ", fixed = TRUE)
  expect_match(conditionMessage(err), "failed isochrone", fixed = TRUE)
})


# ===========================================================================
# T24-NA-01..03  .build_na_propagation_rows() byte-identical schema unit tests
# ===========================================================================

test_that(".build_na_propagation_rows preserves template column order + types", {
  template <- .run_long_fix(site_ids    = "S01",
                            drive_times = c(5L, 10L),
                            variables   = "B17001_002",
                            include_rates = TRUE)
  failed_pairs <- tibble::tibble(
    site_id        = "S02",
    drive_time_min = 5L,
    failure_reason = "isochrone_empty"
  )

  na_rows <- catchmentACS:::.build_na_propagation_rows(
    failed_pairs = failed_pairs,
    rates        = catchmentACS:::.SANCTIONED_RATES_V1,
    template     = template
  )

  # Byte-identical column order vs template.
  expect_identical(colnames(na_rows), colnames(template))

  # §24.6 4-field sentinel contract.
  expect_true(all(na_rows$failure_origin == "isochrone"))
  expect_true(all(is.na(na_rows$n_tracts)))
  expect_true(is.integer(na_rows$n_tracts))   # NA_integer_, not NA_real_
  expect_true(all(na_rows$moe_fallback_reason == "n/a"))
  expect_true(all(is.na(na_rows$moe_formula_effective)))

  # All numeric estimands are NA_real_.
  expect_true(all(is.na(na_rows$estimate)))
  expect_true(all(is.na(na_rows$moe)))
  expect_true(all(is.na(na_rows$weight_sum)))
})


test_that(".build_na_propagation_rows fills estimand_family from template", {
  template <- .run_long_fix(site_ids    = "S01",
                            drive_times = c(5L, 10L),
                            variables   = "B17001_002",
                            include_rates = TRUE)
  failed_pairs <- tibble::tibble(
    site_id        = "S02",
    drive_time_min = 5L,
    failure_reason = "isochrone_empty"
  )

  na_rows <- catchmentACS:::.build_na_propagation_rows(
    failed_pairs = failed_pairs,
    rates        = catchmentACS:::.SANCTIONED_RATES_V1,
    template     = template
  )

  # Rate-name rows should keep "derived_rate" classification.
  rate_rows <- na_rows[na_rows$variable %in%
                          names(catchmentACS:::.SANCTIONED_RATES_V1), ]
  expect_equal(nrow(rate_rows), 5L)
  expect_true(all(rate_rows$estimand_family == "derived_rate"))

  # Source-var rows keep their original family.
  source_rows <- na_rows[na_rows$variable == "B17001_002", ]
  expect_equal(nrow(source_rows), 1L)
  expect_equal(source_rows$estimand_family, "spatial_total")
})


test_that(".build_na_propagation_rows isolates pair-level (drive_time) failure", {
  # Same site_id, two drive_times — only one fails; only the failed pair
  # should appear in the NA rows.
  template <- .run_long_fix(site_ids    = "S01",
                            drive_times = c(5L, 10L),
                            variables   = "B17001_002",
                            include_rates = TRUE)
  failed_pairs <- tibble::tibble(
    site_id        = "S01",
    drive_time_min = 5L,    # only 5-min fails; 10-min succeeds
    failure_reason = "isochrone_empty"
  )

  na_rows <- catchmentACS:::.build_na_propagation_rows(
    failed_pairs = failed_pairs,
    rates        = catchmentACS:::.SANCTIONED_RATES_V1,
    template     = template
  )

  # All NA rows have drive_time_min == 5L (the 10-min pair is NOT NA-marked).
  expect_true(all(na_rows$drive_time_min == 5L))
  expect_true(all(na_rows$site_id        == "S01"))
})


# ===========================================================================
# T24-09..13  Step 7.3 output dispatch + attribute carry-through
# ===========================================================================
#
# These cases share a single mock setup (`.mk_step73_stubs()`) that returns a
# byte-identical sub-call stack across the 5 phases. The output mode varies
# per test so each branch of `switch(output, ...)` is exercised against the
# same long-format input. The 16-field provenance + warning attribute checks
# fold in on top of those.

.mk_step73_stubs <- function(rate_names = names(catchmentACS:::.SANCTIONED_RATES_V1)) {
  list(
    cacs_acs_prefetch = function(state, year, variables, cache_dir = NULL,
                                 verbose = TRUE, ...) {
      out <- .run_acs_fix(variables = variables %||% "B17001_002")
      attr(out, "cacs_acs_provenance") <- list(
        state = state, year = year,
        variables_resolved = variables %||% "B17001_002"
      )
      out
    },
    cacs_isochrone = function(sites, drive_times = c(5, 10, 15),
                              provider = "osrm", cache_dir = NULL, ...) {
      .run_iso_fix(site_ids    = unique(sites$site_id),
                   drive_times = as.integer(drive_times))
    },
    cacs_intersect_weight = function(iso_sf, acs_sf, bg_pop_sf = NULL,
                                     weight_method = "area",
                                     verbose = TRUE, ...) {
      out <- .run_long_fix(
        site_ids      = unique(iso_sf$site_id),
        drive_times   = sort(unique(iso_sf$drive_time_min)),
        variables     = unique(acs_sf$variable),
        include_rates = FALSE
      )
      attr(out, "cacs_aggregation_carriers") <- tibble::tibble(
        site_id        = character(0),
        drive_time_min = integer(0)
      )
      attr(out, "cacs_aggregation_provenance") <- list(
        weight_method = weight_method,
        n_sites       = dplyr::n_distinct(iso_sf$site_id)
      )
      out
    },
    cacs_propagate_moe = function(data, formula = NULL, level = 0.90, ...) {
      attr(data, "cacs_moe_provenance") <- list(
        level                = level,
        formula_requested    = formula %||% "auto"
      )
      attr(data, "cacs_confidence_level") <- level
      data
    },
    cacs_derive_rates = function(weighted_acs, rates, formula_dispatch, ...) {
      out <- .run_long_fix(
        site_ids      = unique(weighted_acs$site_id),
        drive_times   = sort(unique(weighted_acs$drive_time_min)),
        variables     = setdiff(unique(weighted_acs$variable), names(rates)),
        rate_names    = names(rates),
        include_rates = TRUE
      )
      attr(out, "cacs_rate_provenance") <- list(
        rates_emitted    = names(rates),
        formula_dispatch = formula_dispatch
      )
      out
    }
  )
}


test_that("T24-09 output = 'long' returns the long tibble + carries attributes", {
  sites <- .run_sites_fix(n = 1L)
  stubs <- .mk_step73_stubs()

  testthat::local_mocked_bindings(
    cacs_acs_prefetch     = stubs$cacs_acs_prefetch,
    cacs_isochrone        = stubs$cacs_isochrone,
    cacs_intersect_weight = stubs$cacs_intersect_weight,
    cacs_propagate_moe    = stubs$cacs_propagate_moe,
    cacs_derive_rates     = stubs$cacs_derive_rates,
    .package = "catchmentACS"
  )

  out <- suppressMessages(suppressWarnings(
    cacs_run(sites = sites, state = "AL", variables = "B17001_002",
             output = "long", verbose = FALSE)
  ))

  # Default "long" mode returns the §23 long tibble as-is (no nesting).
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("site_id", "drive_time_min", "variable",
                    "estimate", "moe", "estimand_family") %in% colnames(out)))

  # 4 run-level attributes attached.
  expect_equal(attr(out, "cacs_schema_version"), "1.0")
  expect_type(attr(out, "cacs_run_provenance"), "list")
  expect_type(attr(out, "cacs_run_warnings"),   "list")

  # Sub-call provenance preserved through the orchestrator.
  expect_false(is.null(attr(out, "cacs_aggregation_provenance")))
  expect_false(is.null(attr(out, "cacs_moe_provenance")))
  expect_false(is.null(attr(out, "cacs_rate_provenance")))
  expect_equal(attr(out, "cacs_confidence_level"), 0.90)
})


test_that("T24-09b cacs_run(verbose = TRUE) forwards progress to intersect_weight", {
  sites <- .run_sites_fix(n = 1L)
  stubs <- .mk_step73_stubs()
  intersect_verbose <- NULL
  intersect_summaries <- 0L

  stubs$cacs_intersect_weight <- function(iso_sf, acs_sf, bg_pop_sf = NULL,
                                          weight_method = "area",
                                          verbose = TRUE, ...) {
    intersect_verbose <<- verbose
    if (isTRUE(verbose)) {
      .cacs_progress_summary(
        start_time = .cacs_now(),
        n_total    = 1L,
        n_success  = 1L,
        n_failed   = 0L,
        label      = "Intersect+weight",
        verbose    = verbose
      )
    }
    out <- .run_long_fix(
      site_ids      = unique(iso_sf$site_id),
      drive_times   = sort(unique(iso_sf$drive_time_min)),
      variables     = unique(acs_sf$variable),
      include_rates = FALSE
    )
    attr(out, "cacs_aggregation_carriers") <- tibble::tibble(
      site_id        = character(0),
      drive_time_min = integer(0)
    )
    out
  }

  testthat::local_mocked_bindings(
    cacs_acs_prefetch     = stubs$cacs_acs_prefetch,
    cacs_isochrone        = stubs$cacs_isochrone,
    cacs_intersect_weight = stubs$cacs_intersect_weight,
    cacs_propagate_moe    = stubs$cacs_propagate_moe,
    cacs_derive_rates     = stubs$cacs_derive_rates,
    .package = "catchmentACS"
  )

  withr::local_envvar(c(CACS_QUIET = ""))
  withr::local_options(list(catchmentACS.progress = "force"))

  withCallingHandlers(
    suppressWarnings(
      cacs_run(sites = sites, state = "AL", variables = "B17001_002",
               output = "long", verbose = TRUE)
    ),
    catchmentACS_message_progress_summary = function(m) {
      if (identical(m$cacs_phase, "Intersect+weight")) {
        intersect_summaries <<- intersect_summaries + 1L
      }
      invokeRestart("muffleMessage")
    },
    catchmentACS_message_progress_tick = function(m) {
      invokeRestart("muffleMessage")
    },
    catchmentACS_message_progress = function(m) {
      invokeRestart("muffleMessage")
    },
    catchmentACS_message = function(m) {
      invokeRestart("muffleMessage")
    }
  )

  expect_true(intersect_verbose)
  expect_gte(intersect_summaries, 1L)
})

test_that("T24-09c cacs_run(verbose = FALSE) threads quiet flag to subcalls", {
  sites <- .run_sites_fix(n = 1L)
  base_stubs <- .mk_step73_stubs()
  seen <- list()
  iso_summaries <- 0L

  stubs <- base_stubs
  stubs$cacs_acs_prefetch <- function(state, year, variables, cache_dir = NULL,
                                      verbose = TRUE, ...) {
    seen$acs <<- verbose
    base_stubs$cacs_acs_prefetch(
      state = state, year = year, variables = variables,
      cache_dir = cache_dir, verbose = verbose, ...
    )
  }
  stubs$cacs_isochrone <- function(sites, drive_times = c(5, 10, 15),
                                   provider = "osrm", cache_dir = NULL,
                                   verbose = TRUE, ...) {
    seen$iso <<- verbose
    if (isTRUE(verbose)) {
      .cacs_progress_summary(
        start_time = .cacs_now(),
        n_total    = 1L,
        n_success  = 1L,
        n_failed   = 0L,
        label      = "Isochrones",
        verbose    = verbose
      )
    }
    base_stubs$cacs_isochrone(
      sites = sites, drive_times = drive_times, provider = provider,
      cache_dir = cache_dir, ...
    )
  }
  stubs$cacs_intersect_weight <- function(iso_sf, acs_sf, bg_pop_sf = NULL,
                                          weight_method = "area",
                                          verbose = TRUE, ...) {
    seen$wgt <<- verbose
    base_stubs$cacs_intersect_weight(
      iso_sf = iso_sf, acs_sf = acs_sf, bg_pop_sf = bg_pop_sf,
      weight_method = weight_method, verbose = verbose, ...
    )
  }
  stubs$cacs_propagate_moe <- function(data, formula = NULL, level = 0.90,
                                       verbose = TRUE, ...) {
    seen$moe <<- verbose
    base_stubs$cacs_propagate_moe(
      data = data, formula = formula, level = level, ...
    )
  }
  stubs$cacs_derive_rates <- function(weighted_acs, rates, formula_dispatch,
                                      verbose = TRUE, ...) {
    seen$rat <<- verbose
    base_stubs$cacs_derive_rates(
      weighted_acs = weighted_acs, rates = rates,
      formula_dispatch = formula_dispatch, ...
    )
  }

  testthat::local_mocked_bindings(
    cacs_acs_prefetch     = stubs$cacs_acs_prefetch,
    cacs_isochrone        = stubs$cacs_isochrone,
    cacs_intersect_weight = stubs$cacs_intersect_weight,
    cacs_propagate_moe    = stubs$cacs_propagate_moe,
    cacs_derive_rates     = stubs$cacs_derive_rates,
    .package = "catchmentACS"
  )

  withr::local_envvar(c(CACS_QUIET = ""))
  withr::local_options(list(catchmentACS.progress = "force"))

  withCallingHandlers(
    suppressWarnings(
      cacs_run(sites = sites, state = "AL", variables = "B17001_002",
               output = "long", verbose = FALSE)
    ),
    catchmentACS_message_progress_summary = function(m) {
      if (identical(m$cacs_phase, "Isochrones")) {
        iso_summaries <<- iso_summaries + 1L
      }
      invokeRestart("muffleMessage")
    }
  )

  expect_identical(seen, list(
    acs = FALSE, iso = FALSE, wgt = FALSE, moe = FALSE, rat = FALSE
  ))
  expect_identical(iso_summaries, 0L)
})


test_that("T24-10 output = 'list_column' produces the Sec. 12.3.2 8-col schema", {
  sites <- .run_sites_fix(n = 1L)
  stubs <- .mk_step73_stubs()

  testthat::local_mocked_bindings(
    cacs_acs_prefetch     = stubs$cacs_acs_prefetch,
    cacs_isochrone        = stubs$cacs_isochrone,
    cacs_intersect_weight = stubs$cacs_intersect_weight,
    cacs_propagate_moe    = stubs$cacs_propagate_moe,
    cacs_derive_rates     = stubs$cacs_derive_rates,
    .package = "catchmentACS"
  )

  out <- suppressMessages(suppressWarnings(
    cacs_run(sites = sites, state = "AL", variables = "B17001_002",
             output = "list_column", verbose = FALSE)
  ))

  # Sec. 12.3.2 8-col schema (exact column set + order).
  expect_s3_class(out, "tbl_df")
  expect_equal(
    colnames(out),
    c("site_id", "lon", "lat", "drive_time_min",
      "isochrone", "acs_estimates", "derived_rates", "metadata")
  )

  # One row per (site_id, drive_time_min): 1 site x 3 drive_times = 3 rows.
  expect_equal(nrow(out), 3L)

  # list-columns are list-typed. ACS/rate cells carry tibbles, metadata carries
  # a list, and computed isochrone cells carry one-row sf objects.
  expect_type(out$acs_estimates, "list")
  expect_type(out$derived_rates, "list")
  expect_type(out$metadata,      "list")
  expect_type(out$isochrone,     "list")
  expect_true(all(vapply(out$acs_estimates, inherits, logical(1L), "tbl_df")))
  expect_true(all(vapply(out$derived_rates, inherits, logical(1L), "tbl_df")))
  expect_true(all(vapply(
    out$isochrone,
    function(x) inherits(x, "sf") && nrow(x) == 1L,
    logical(1L)
  )))
  expect_identical(
    vapply(out$isochrone, function(x) x$site_id[[1L]], character(1L)),
    out$site_id
  )
  expect_identical(
    vapply(out$isochrone,
           function(x) as.integer(x$drive_time_min[[1L]]),
           integer(1L)),
    as.integer(out$drive_time_min)
  )

  # Run-level + sub-call attributes carry onto the list-column representation.
  expect_equal(attr(out, "cacs_schema_version"), "1.0")
  prov <- attr(out, "cacs_run_provenance")
  expect_type(prov, "list")
  expect_equal(prov$execution_path, "5-call")
  expect_false(prov$bypass_iso)
})


test_that("T24-11 output = 'both' returns a named list with shared attrs", {
  sites <- .run_sites_fix(n = 1L)
  stubs <- .mk_step73_stubs()

  testthat::local_mocked_bindings(
    cacs_acs_prefetch     = stubs$cacs_acs_prefetch,
    cacs_isochrone        = stubs$cacs_isochrone,
    cacs_intersect_weight = stubs$cacs_intersect_weight,
    cacs_propagate_moe    = stubs$cacs_propagate_moe,
    cacs_derive_rates     = stubs$cacs_derive_rates,
    .package = "catchmentACS"
  )

  out <- suppressMessages(suppressWarnings(
    cacs_run(sites = sites, state = "AL", variables = "B17001_002",
             output = "both", verbose = FALSE)
  ))

  # Names locked to c("long", "list_column") in this order.
  expect_type(out, "list")
  expect_equal(names(out), c("long", "list_column"))

  expect_s3_class(out$long,        "tbl_df")
  expect_s3_class(out$list_column, "tbl_df")
  expect_equal(
    colnames(out$list_column),
    c("site_id", "lon", "lat", "drive_time_min",
      "isochrone", "acs_estimates", "derived_rates", "metadata")
  )
  expect_true(all(vapply(
    out$list_column$isochrone,
    function(x) inherits(x, "sf") && nrow(x) == 1L,
    logical(1L)
  )))
  expect_identical(
    vapply(out$list_column$isochrone,
           function(x) x$site_id[[1L]],
           character(1L)),
    out$list_column$site_id
  )
  expect_identical(
    vapply(out$list_column$isochrone,
           function(x) as.integer(x$drive_time_min[[1L]]),
           integer(1L)),
    as.integer(out$list_column$drive_time_min)
  )

  # Both representations carry the same run-level provenance.
  expect_identical(
    attr(out$long,        "cacs_run_provenance"),
    attr(out$list_column, "cacs_run_provenance")
  )
  expect_equal(attr(out$long,        "cacs_schema_version"), "1.0")
  expect_equal(attr(out$list_column, "cacs_schema_version"), "1.0")
  expect_equal(attr(out$list_column, "cacs_run_provenance")$execution_path,
               "5-call")
  expect_false(attr(out$list_column, "cacs_run_provenance")$bypass_iso)
})


test_that("T24-11b output = 'list_column' fills computed isochrones on 4-call-B", {
  sites <- .run_sites_fix(n = 1L)
  acs_in <- .run_acs_fix(variables = "B17001_002")
  stubs <- .mk_step73_stubs()

  testthat::local_mocked_bindings(
    cacs_acs_prefetch     = function(...) stop("ACS prefetch should be bypassed"),
    cacs_isochrone        = stubs$cacs_isochrone,
    cacs_intersect_weight = stubs$cacs_intersect_weight,
    cacs_propagate_moe    = stubs$cacs_propagate_moe,
    cacs_derive_rates     = stubs$cacs_derive_rates,
    .package = "catchmentACS"
  )

  out <- suppressMessages(suppressWarnings(
    cacs_run(sites = sites, state = "AL", variables = "B17001_002",
             acs = acs_in, output = "list_column", verbose = FALSE)
  ))

  prov <- attr(out, "cacs_run_provenance")
  expect_equal(prov$execution_path, "4-call-B")
  expect_false(prov$bypass_iso)
  expect_true(prov$bypass_acs)
  expect_true(all(vapply(
    out$isochrone,
    function(x) inherits(x, "sf") && nrow(x) == 1L,
    logical(1L)
  )))
  expect_identical(
    vapply(out$isochrone, function(x) x$site_id[[1L]], character(1L)),
    out$site_id
  )
  expect_identical(
    vapply(out$isochrone,
           function(x) as.integer(x$drive_time_min[[1L]]),
           integer(1L)),
    as.integer(out$drive_time_min)
  )
})


test_that("T24-11c list_column keeps failed-pair isochrone diagnostics", {
  sites <- .run_sites_fix(n = 1L)
  stubs <- .mk_step73_stubs()
  iso_partial <- .run_iso_fix(
    site_ids        = "S01",
    drive_times     = c(5L, 10L, 15L),
    isochrone_empty = c(TRUE, FALSE, FALSE),
    failure_reason  = c("isochrone_empty", NA_character_, NA_character_)
  )

  testthat::local_mocked_bindings(
    cacs_acs_prefetch     = stubs$cacs_acs_prefetch,
    cacs_isochrone        = function(...) iso_partial,
    cacs_intersect_weight = stubs$cacs_intersect_weight,
    cacs_propagate_moe    = stubs$cacs_propagate_moe,
    cacs_derive_rates     = stubs$cacs_derive_rates,
    .package = "catchmentACS"
  )

  out <- suppressMessages(suppressWarnings(
    cacs_run(sites = sites, state = "AL", variables = "B17001_002",
             output = "list_column", verbose = FALSE)
  ))

  failed_idx <- which(out$site_id == "S01" & out$drive_time_min == 5L)
  expect_length(failed_idx, 1L)
  expect_true(inherits(out$isochrone[[failed_idx]], "sf"))
  expect_identical(nrow(out$isochrone[[failed_idx]]), 1L)
  expect_true(out$isochrone[[failed_idx]]$isochrone_empty[[1L]])
  expect_equal(out$metadata[[failed_idx]]$failure_origin, "isochrone")
  expect_true(all(out$acs_estimates[[failed_idx]]$failure_origin == "isochrone"))

  prov <- attr(out, "cacs_run_provenance")
  expect_equal(prov$n_pairs_failed, 1L)
  expect_equal(prov$n_pairs_valid, 2L)
})


test_that("T24-12 cacs_run_provenance carries the Sec. 24.8 16-field contract", {
  sites <- .run_sites_fix(n = 1L)
  stubs <- .mk_step73_stubs()

  testthat::local_mocked_bindings(
    cacs_acs_prefetch     = stubs$cacs_acs_prefetch,
    cacs_isochrone        = stubs$cacs_isochrone,
    cacs_intersect_weight = stubs$cacs_intersect_weight,
    cacs_propagate_moe    = stubs$cacs_propagate_moe,
    cacs_derive_rates     = stubs$cacs_derive_rates,
    .package = "catchmentACS"
  )

  out <- suppressMessages(suppressWarnings(
    cacs_run(sites = sites, state = "AL", variables = "B17001_002",
             output = "long", verbose = FALSE)
  ))

  prov <- attr(out, "cacs_run_provenance")
  expect_type(prov, "list")
  # ≥ 16 fields per Sec. 24.8.
  expect_gte(length(prov), 16L)

  required <- c(
    "generated_at", "cacs_ver", "R_version", "schema_version",
    "execution_path", "state", "year", "drive_times",
    "provider", "weight_method", "moe_formula", "formula_dispatch",
    "output_format", "level", "min_weight",
    "n_sites_input", "n_pairs_valid", "n_pairs_failed",
    "n_sites_with_any_valid_pair", "n_sites_with_any_failed_pair",
    "bypass_iso", "bypass_acs"
  )
  expect_true(all(required %in% names(prov)))

  # Spot-check a handful of derived quantities.
  expect_equal(prov$execution_path,  "5-call")
  expect_equal(prov$state,           "AL")
  expect_equal(prov$weight_method,   "area")
  expect_equal(prov$output_format,   "long")
  expect_equal(prov$schema_version,  "1.0")
  expect_equal(prov$n_sites_input,   1L)
  expect_equal(prov$n_pairs_failed,  0L)
  expect_equal(prov$drive_times,     c(5L, 10L, 15L))
  expect_false(prov$bypass_iso)
  expect_false(prov$bypass_acs)
})


test_that("T24-13 cacs_run_warnings carries the 6 phase-keyed slots", {
  sites <- .run_sites_fix(n = 1L)
  stubs <- .mk_step73_stubs()
  # Make one stub emit a cli_warn so we can verify it lands in the phase slot.
  noisy_moe <- function(data, formula = NULL, level = 0.90, ...) {
    cli::cli_warn("Step 7.3 test warning: MOE phase",
                  class = c("catchmentACS_warning_runtime",
                            "catchmentACS_warning",
                            "catchmentACS_condition"))
    attr(data, "cacs_moe_provenance") <- list(formula_requested = "auto")
    attr(data, "cacs_confidence_level") <- 0.90
    data
  }

  testthat::local_mocked_bindings(
    cacs_acs_prefetch     = stubs$cacs_acs_prefetch,
    cacs_isochrone        = stubs$cacs_isochrone,
    cacs_intersect_weight = stubs$cacs_intersect_weight,
    cacs_propagate_moe    = noisy_moe,
    cacs_derive_rates     = stubs$cacs_derive_rates,
    .package = "catchmentACS"
  )

  out <- suppressMessages(suppressWarnings(
    cacs_run(sites = sites, state = "AL", variables = "B17001_002",
             output = "long", verbose = FALSE)
  ))

  warn_attr <- attr(out, "cacs_run_warnings")
  expect_type(warn_attr, "list")
  expect_setequal(
    names(warn_attr),
    c("isochrone", "acs_prefetch", "intersect_weight",
      "propagate_moe", "derive_rates", "orchestrator")
  )

  # The noisy_moe warning landed in propagate_moe and nowhere else.
  expect_equal(length(warn_attr$propagate_moe), 1L)
  expect_equal(length(warn_attr$intersect_weight), 0L)
  expect_equal(length(warn_attr$derive_rates), 0L)

  entry <- warn_attr$propagate_moe[[1L]]
  expect_match(entry$message, "Step 7.3 test warning")
  expect_equal(entry$phase, "propagate_moe")
})
