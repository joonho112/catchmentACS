# ============================================================================
# Integration tests for cacs_isochrone() end-to-end — Step 3.5 (Phase 3 exit gate).
# 3 cases per §38.7 + §19.8:
#   T19-09: osm_snapshot_date best-effort resolution path-pair
#           (NULL -> "unknown_best_effort" / "unknown" sentinel vs.
#            Date -> "user_supplied" / "<date>"; distinct cache keys)
#   T19-14: empty polygon -> isochrone_empty = TRUE + W-09 runtime warning
#           (backend returns valid sf but with empty geometry on one row)
#   T19-15: mixed batch (10 sites = 7 success + 2 empty + 1 retry-recovered)
#           dual-tier policy correctness (schema invariance + permissive
#           per-row carry-through), single batch completes (no abort).
#
# T19-12 (cache hit) is already covered in test-integration-isochrone-cache.R
# (Step 3.4). T19-13 (HTTP 429 abort) is covered at the helper level in
# test-integration-isochrone-{osrm,ors}.R (Step 3.2 / 3.3).
#
# Mocking strategy. We mock the catchmentACS-internal seam
# `.osrm_call_isochrone()` via `testthat::local_mocked_bindings(.package =
# "catchmentACS")` so the tests run hermetically (no network, no OSRM server,
# no `osrm` install required at the seam level — `rlang::check_installed()`
# is bypassed where needed). See §16.5 hermetic test policy.
# ============================================================================


# ---- Isolated cache dir per test (re-uses Step 3.4 helper pattern) ---------

.with_iso_cache_dir <- function(code) {
  td <- tempfile("cacs_iso_step35_")
  withr::with_options(
    list(catchmentACS.cache_dir = td, CACS_NO_CONFIRM = "1"),
    {
      Sys.setenv(CACS_NO_CONFIRM = "1")
      on.exit({
        if (dir.exists(td)) unlink(td, recursive = TRUE)
        memoise::forget(.cacs_cache_dir_memo)
      }, add = TRUE)
      memoise::forget(.cacs_cache_dir_memo)
      force(code)
    }
  )
}


# ---- Local fixture helpers -------------------------------------------------

.mk_iso_site <- function(id = "S01", lon = -86.80902, lat = 33.52203) {
  tib <- tibble::tibble(
    site_id = id,
    lon = lon,
    lat = lat
  )
  sf::st_as_sf(tib, coords = c("lon", "lat"), crs = 4326)
}

.mk_iso_sites_n <- function(n = 10L) {
  # Spread sites around Birmingham to keep coords deterministic + distinct.
  base_lon <- -86.80902
  base_lat <-  33.52203
  tib <- tibble::tibble(
    site_id = paste0("S", sprintf("%02d", seq_len(n))),
    lon = base_lon + seq(0, 0.05, length.out = n),
    lat = base_lat + seq(0, 0.05, length.out = n)
  )
  sf::st_as_sf(tib, coords = c("lon", "lat"), crs = 4326)
}

# Mimic `osrm::osrmIsochrone()` valid (non-empty) response: one row per break.
.mk_osrm_happy <- function(breaks, center_lon = -86.81, center_lat = 33.52) {
  polys <- lapply(seq_along(breaks), function(j) {
    b <- breaks[[j]]
    half <- 0.01 * b
    sf::st_polygon(list(rbind(
      c(center_lon - half, center_lat - half),
      c(center_lon + half, center_lat - half),
      c(center_lon + half, center_lat + half),
      c(center_lon - half, center_lat + half),
      c(center_lon - half, center_lat - half)
    )))
  })
  sfc <- sf::st_sfc(polys, crs = 4326)
  sf::st_sf(
    tibble::tibble(
      id     = seq_along(breaks),
      isomin = c(0L, utils::head(as.integer(breaks), -1L)),
      isomax = as.integer(breaks)
    ),
    geometry = sfc
  )
}

# Mimic `osrm::osrmIsochrone()` response where every break is an EMPTY POLYGON
# (e.g. an isolated site with no reachable road network within the breaks).
.mk_osrm_empty <- function(breaks) {
  empty_polys <- lapply(seq_along(breaks), function(j) sf::st_polygon())
  sfc <- sf::st_sfc(empty_polys, crs = 4326)
  sf::st_sf(
    tibble::tibble(
      id     = seq_along(breaks),
      isomin = c(0L, utils::head(as.integer(breaks), -1L)),
      isomax = as.integer(breaks)
    ),
    geometry = sfc
  )
}


# ============================================================================
# T19-09: osm_snapshot_date best-effort resolution (NULL vs explicit Date)
# ============================================================================

test_that("T19-09a osm_snapshot_date = NULL records 'unknown' / 'unknown_best_effort'", {
  .with_iso_cache_dir({
    site <- .mk_iso_site()

    happy_stub <- function(loc, breaks, res = NULL) .mk_osrm_happy(breaks)
    testthat::local_mocked_bindings(
      .osrm_call_isochrone = happy_stub,
      .package = "catchmentACS"
    )

    out <- suppressMessages(suppressWarnings(
      cacs_isochrone(
        sites             = site,
        drive_times       = 5L,
        provider          = "osrm",
        osrm_mode         = "demo",
        osm_snapshot_date = NULL    # triggers best-effort path
      )
    ))

    # §19.5 row 6 + row 13: best-effort lookup failure yields the "unknown"
    # string sentinel (NOT NA) + osm_snapshot_status = "unknown_best_effort".
    expect_equal(out$osm_snapshot_date, "unknown")
    expect_equal(out$osm_snapshot_status, "unknown_best_effort")
    expect_false(is.na(out$osm_snapshot_date))
  })
})


test_that("T19-09b osm_snapshot_date = Date records 'user_supplied' + explicit date", {
  .with_iso_cache_dir({
    site <- .mk_iso_site()

    happy_stub <- function(loc, breaks, res = NULL) .mk_osrm_happy(breaks)
    testthat::local_mocked_bindings(
      .osrm_call_isochrone = happy_stub,
      .package = "catchmentACS"
    )

    pinned_date <- as.Date("2025-04-01")
    out <- suppressMessages(suppressWarnings(
      cacs_isochrone(
        sites             = site,
        drive_times       = 5L,
        provider          = "osrm",
        osrm_mode         = "demo",
        osm_snapshot_date = pinned_date
      )
    ))

    # §19.3 step 3a: explicit Date -> status = "user_supplied", date = ISO chr.
    expect_equal(out$osm_snapshot_date, "2025-04-01")
    expect_equal(out$osm_snapshot_status, "user_supplied")
  })
})


test_that("T19-09c NULL vs Date produce distinct cache keys (separate key spaces)", {
  .with_iso_cache_dir({
    site <- .mk_iso_site()

    # Counting stub: track how many times the backend is actually invoked.
    call_n <- 0L
    counting_stub <- function(loc, breaks, res = NULL) {
      call_n <<- call_n + 1L
      .mk_osrm_happy(breaks)
    }
    testthat::local_mocked_bindings(
      .osrm_call_isochrone = counting_stub,
      .package = "catchmentACS"
    )

    # Call 1: NULL -> "unknown" sentinel.
    out_unknown <- suppressMessages(suppressWarnings(
      cacs_isochrone(
        sites             = site,
        drive_times       = 5L,
        provider          = "osrm",
        osrm_mode         = "demo",
        osm_snapshot_date = NULL
      )
    ))
    expect_equal(call_n, 1L)

    # Call 2: explicit Date -> different cache key, backend invoked again.
    # §19.6 footnote: "unknown" sentinel forms a *separate* key space so a
    # later explicit Date does not collide with prior best-effort entries.
    out_pinned <- suppressMessages(suppressWarnings(
      cacs_isochrone(
        sites             = site,
        drive_times       = 5L,
        provider          = "osrm",
        osrm_mode         = "demo",
        osm_snapshot_date = as.Date("2025-04-01")
      )
    ))
    expect_equal(call_n, 2L)   # backend re-invoked = distinct cache key

    # Both calls share schema but differ in snapshot provenance columns.
    expect_equal(out_unknown$osm_snapshot_status,  "unknown_best_effort")
    expect_equal(out_pinned$osm_snapshot_status,   "user_supplied")
    expect_false(identical(out_unknown$osm_snapshot_date,
                           out_pinned$osm_snapshot_date))
  })
})


# ============================================================================
# T19-14: empty polygon for an isolated site -> isochrone_empty = TRUE + W-09
# ============================================================================

test_that("T19-14 empty polygon yields isochrone_empty = TRUE + W-09 runtime warning", {
  .with_iso_cache_dir({
    site <- .mk_iso_site()

    # Backend returns a valid sf object whose geometry is an empty polygon
    # (simulates an isolated site with no reachable road network).
    empty_stub <- function(loc, breaks, res = NULL) .mk_osrm_empty(breaks)
    testthat::local_mocked_bindings(
      .osrm_call_isochrone = empty_stub,
      .package = "catchmentACS"
    )

    # W-09 runtime warning is emitted by `.iso_via_osrm()` aggregation block.
    # `expect_warning()` returns the warning condition, not the wrapped value,
    # so we capture `out` via withCallingHandlers and assert the warning
    # was emitted via a side-channel.
    warn_seen <- FALSE
    out <- suppressMessages(withCallingHandlers(
      cacs_isochrone(
        sites       = site,
        drive_times = 5L,
        provider    = "osrm",
        osrm_mode   = "demo"
      ),
      catchmentACS_warning_runtime = function(w) {
        if (grepl("empty isochrone", conditionMessage(w))) {
          warn_seen <<- TRUE
        }
        invokeRestart("muffleWarning")
      }
    ))
    expect_true(warn_seen)

    # Row-level invariants: isochrone_empty = TRUE, geometry EMPTY, batch
    # continued (sf returned, not aborted), failure_reason still NA because
    # the call itself succeeded (it just produced empty geometry).
    expect_s3_class(out, "sf")
    expect_equal(nrow(out), 1L)
    expect_true(out$isochrone_empty)
    expect_true(sf::st_is_empty(sf::st_geometry(out)[[1]]))
    expect_true(is.na(out$failure_reason))
    expect_equal(out$retry_count, 1L)
  })
})


# ============================================================================
# T19-15: mixed batch — 7 success + 2 empty + 1 retry-recovered
# ============================================================================
# §19.8 T19-15 spec: 10 sites = 7 정상 + 2 empty + 1 retry-exhaustion.
# We implement a *retry-recovered* variant (transient 5xx then 200) instead of
# retry-exhaustion so the batch returns a 10-row sf without aborting — this
# matches the Step 3.5 task contract ("1 retry-recovered") and exercises the
# same W-09 + retry-count carry-through invariants. The dual-tier policy
# (schema invariance + permissive per-row provenance) is the canonical
# property under test, not the specific failure flavor.
# ============================================================================

test_that("T19-15 mixed batch (7 ok + 2 empty + 1 retry) returns 10 rows with correct provenance", {
  .with_iso_cache_dir({
    sites <- .mk_iso_sites_n(n = 10L)

    # Deterministic site-aware stub: dispatch by site coordinate.
    #   Sites 1-7: happy (valid non-empty polygon)
    #   Sites 8-9: empty polygon
    #   Site 10  : first call errors (HTTP 503), second call succeeds
    # The `.osrm_call_isochrone` seam receives `loc = c(lon, lat)`; we map
    # `loc` -> site index by matching against the input lon vector.
    site_lons <- sf::st_coordinates(sites)[, "X"]
    site_idx_of <- function(loc) {
      which.min(abs(site_lons - loc[[1L]]))
    }

    # Per-site call counters (for retry-recovered site 10).
    site_calls <- integer(10L)

    mixed_stub <- function(loc, breaks, res = NULL) {
      i <- site_idx_of(loc)
      site_calls[[i]] <<- site_calls[[i]] + 1L

      if (i <= 7L) {
        # Happy path — 7 sites with valid geometry.
        .mk_osrm_happy(breaks)
      } else if (i %in% c(8L, 9L)) {
        # Empty geometry on 2 sites.
        .mk_osrm_empty(breaks)
      } else {
        # Site 10 — transient 5xx on first attempt, success on retry.
        if (site_calls[[i]] == 1L) {
          stop("OSRM server returned HTTP 503: Service Unavailable")
        }
        .mk_osrm_happy(breaks)
      }
    }

    testthat::local_mocked_bindings(
      .osrm_call_isochrone = mixed_stub,
      .package = "catchmentACS"
    )

    # Stub Sys.sleep so the 1s retry backoff doesn't slow the suite.
    testthat::local_mocked_bindings(
      Sys.sleep = function(time) invisible(NULL),
      .package = "base"
    )

    # `.iso_via_osrm()` emits a single aggregated W-09 because the empty
    # rows accumulate before the warning fires; W-10 (retry exhaustion) is
    # NOT emitted because site 10 *recovers* on retry (failure_reason = NA).
    # `expect_warning()` returns the warning, not the value, so we capture
    # `out` via withCallingHandlers and verify W-09 was emitted side-channel.
    warn_msgs <- character(0)
    out <- suppressMessages(withCallingHandlers(
      cacs_isochrone(
        sites       = sites,
        drive_times = 5L,
        provider    = "osrm",
        osrm_mode   = "demo"
      ),
      catchmentACS_warning_runtime = function(w) {
        warn_msgs <<- c(warn_msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ))
    expect_true(any(grepl("empty isochrone", warn_msgs)))   # W-09 fired

    # ---- Row-count invariant: 10 sites x 1 drive_time = 10 rows ------------
    expect_s3_class(out, "sf")
    expect_equal(nrow(out), 10L)
    expect_equal(ncol(out), 16L)

    # ---- §19.5 16-col canonical schema invariance (Phase 3 exit criterion) -
    expect_named(out, c(
      "site_id", "drive_time_min", "geometry", "provider", "profile",
      "osm_snapshot_date", "routing_engine_version",
      "polygon_simplification_tolerance", "generated_at",
      "isochrone_empty", "provider_requested", "provider_downgrade",
      "osm_snapshot_status", "failure_reason", "retry_count",
      "ring_topology"
    ))

    # ---- Per-row predicate assertions (dual-tier dual-layer correctness) ---
    # Order preserved: out[1:7] should be non-empty success, out[8:9] empty,
    # out[10] retry-recovered (non-empty, retry_count = 2).
    expect_equal(sum(out$isochrone_empty), 2L)         # exactly 2 empty rows
    expect_equal(sum(!out$isochrone_empty), 8L)        # 7 happy + 1 recovered
    expect_true(all(is.na(out$failure_reason)))        # nobody truly failed
    expect_true(all(out$ring_topology == "cumulative"))
    expect_equal(sum(out$retry_count == 1L), 9L)       # 9 first-try wins
    expect_equal(sum(out$retry_count == 2L), 1L)       # site 10 recovered
    expect_true(all(out$retry_count <= 3L))            # max retry honored

    # ---- Site-level positional checks --------------------------------------
    # Sites 1-7: success, non-empty geometry.
    happy_rows <- out[1:7, ]
    expect_true(all(!happy_rows$isochrone_empty))
    expect_true(all(!sf::st_is_empty(sf::st_geometry(happy_rows))))

    # Sites 8-9: empty polygon.
    empty_rows <- out[8:9, ]
    expect_true(all(empty_rows$isochrone_empty))
    expect_true(all(sf::st_is_empty(sf::st_geometry(empty_rows))))

    # Site 10: retry-recovered (retry_count = 2, geometry non-empty).
    site10 <- out[10, ]
    expect_false(site10$isochrone_empty)
    expect_equal(site10$retry_count, 2L)
    expect_false(sf::st_is_empty(sf::st_geometry(site10)[[1]]))

    # ---- Backend invocation count: 7+2+2 (site 10 called twice) = 11 -------
    expect_equal(sum(site_calls), 11L)
    expect_equal(site_calls[[10L]], 2L)            # exactly 1 retry on site 10
    expect_true(all(site_calls[1:9] == 1L))         # everyone else: one shot
  })
})


# ============================================================================
# T19-16 [v0.2 F1]: verbose=TRUE emits >= n_sites progress tick events
# ============================================================================
# Test-plan extension: assert that verbose = TRUE
# on cacs_isochrone() emits at least one progress_tick condition per site
# under bar mode. We force bar mode via
# `options(catchmentACS.progress = "force")` so the assertion holds even at
# small N (N = 10 would otherwise also pass under "auto" because of the
# N >= 5 threshold, but we force here to keep the test orthogonal to the
# threshold value).
# ============================================================================

test_that("T19-16 verbose=TRUE emits >= n_sites tick events under bar mode", {
  .with_iso_cache_dir({
    sites <- .mk_iso_sites_n(n = 5L)

    happy_stub <- function(loc, breaks, res = NULL) .mk_osrm_happy(breaks)
    testthat::local_mocked_bindings(
      .osrm_call_isochrone = happy_stub,
      .package = "catchmentACS"
    )

    withr::with_envvar(c(CACS_QUIET = ""), {
      withr::with_options(list(catchmentACS.progress = "force"), {
        n_ticks    <- 0L
        n_summary  <- 0L
        out <- suppressWarnings(suppressMessages(withCallingHandlers(
          cacs_isochrone(
            sites       = sites,
            drive_times = 5L,
            provider    = "osrm",
            osrm_mode   = "demo",
            verbose     = TRUE
          ),
          catchmentACS_message_progress_tick = function(m) {
            n_ticks <<- n_ticks + 1L
            invokeRestart("muffleMessage")
          },
          catchmentACS_message_progress_summary = function(m) {
            n_summary <<- n_summary + 1L
            invokeRestart("muffleMessage")
          }
        )))

        expect_s3_class(out, "sf")
        expect_equal(nrow(out), nrow(sites))
        expect_gte(n_ticks, nrow(sites))   # >= n_sites tick events
        expect_equal(n_summary, 1L)        # exactly one end summary
      })
    })
  })
})
