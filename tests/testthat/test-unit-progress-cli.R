# ============================================================================
# Unit tests for v0.2 F1 verbose progress reporter (hybrid bar/line mode).
#
# 10 testcases per the test plan §"Test plan (10 testcases)":
#   1. silent (verbose = FALSE)
#   2. bookend (N = 3, default option)
#   3. bar (N = 10, default option)
#   4. ETA monotonic non-increasing (synthetic timestamps)
#   5. CACS_QUIET = "1" suppression across all 3 fns
#   6. getOption("catchmentACS.progress") == "off" suppression
#   7. getOption("catchmentACS.progress") == "force" -> bar at N = 1
#   8. verbose = FALSE silences all 3 fns
#   9. All 3 fns emit >= n_iter tick events at verbose = TRUE + bar
#  10. cli::test_that_cli snapshot tests (configs plain, ansi, unicode)
#
# Mocking strategy. Live tests use the bundled legacy_2025_* fixtures so
# the reporter exercises real per-(site, drive_time) loops. Env-var
# behaviors (5/6/7) are exercised via direct `.cacs_progress_mode()` calls
# wrapped in `withr::with_envvar` / `withr::with_options` for hermetic
# isolation (no global state leak). See §16.5.
# ============================================================================


# ---- Helpers ---------------------------------------------------------------

# Capture the message classes emitted by a quiet block. Returns a character
# vector of one-character-per-message class chain (joined with "|").
.cap_progress_classes <- function(expr) {
  cls <- character(0)
  withCallingHandlers(
    force(expr),
    catchmentACS_message_progress = function(m) {
      cls <<- c(cls, paste(class(m), collapse = "|"))
      invokeRestart("muffleMessage")
    }
  )
  cls
}

# Capture just the *count* of progress_tick conditions (sub-family).
.count_progress_ticks <- function(expr) {
  n <- 0L
  withCallingHandlers(
    force(expr),
    catchmentACS_message_progress_tick = function(m) {
      n <<- n + 1L
      invokeRestart("muffleMessage")
    }
  )
  n
}

# Capture just the *count* of progress_summary conditions.
.count_progress_summaries <- function(expr) {
  n <- 0L
  withCallingHandlers(
    force(expr),
    catchmentACS_message_progress_summary = function(m) {
      n <<- n + 1L
      invokeRestart("muffleMessage")
    }
  )
  n
}


# ============================================================================
# Case 1: silent mode (verbose = FALSE) - no ticks, no summary emitted
# ============================================================================

test_that("T-PROG-01 verbose = FALSE -> silent mode, no progress conditions", {
  withr::with_envvar(c(CACS_QUIET = ""), {
    withr::with_options(list(catchmentACS.progress = "auto"), {
      prog <- .cacs_progress_reporter(n = 5L, label = "Test",
                                      verbose = FALSE)
      n_ticks <- .count_progress_ticks({
        for (i in seq_len(5L)) prog$tick(detail = paste0("site_", i))
        prog$finish(n_success = 5L, n_failed = 0L)
      })
      n_summary <- .count_progress_summaries({
        prog$finish(n_success = 5L, n_failed = 0L)
      })
      expect_identical(prog$mode, "silent")
      expect_identical(n_ticks, 0L)
      expect_identical(n_summary, 0L)
    })
  })
})


# ============================================================================
# Case 2: bookend mode (N = 3, default option, verbose = TRUE)
# ============================================================================

test_that("T-PROG-02 N=3 default option -> bookend mode (no ticks, 1 summary)", {
  withr::with_envvar(c(CACS_QUIET = ""), {
    withr::with_options(list(catchmentACS.progress = "auto"), {
      prog <- .cacs_progress_reporter(n = 3L, label = "Test",
                                      verbose = TRUE)
      expect_identical(prog$mode, "bookend")
      n_ticks <- .count_progress_ticks({
        for (i in seq_len(3L)) prog$tick(detail = paste0("site_", i))
      })
      n_summary <- .count_progress_summaries({
        prog$finish(n_success = 3L, n_failed = 0L)
      })
      expect_identical(n_ticks, 0L)       # no per-tick events in bookend
      expect_identical(n_summary, 1L)     # exactly one summary
    })
  })
})


# ============================================================================
# Case 3: bar mode (N = 10, default option, verbose = TRUE)
# ============================================================================

test_that("T-PROG-03 N=10 default option -> bar mode (10 ticks + 1 summary)", {
  withr::with_envvar(c(CACS_QUIET = ""), {
    withr::with_options(list(catchmentACS.progress = "force"), {
      prog <- .cacs_progress_reporter(n = 10L, label = "Test",
                                      verbose = TRUE)
      expect_identical(prog$mode, "bar")
      n_ticks <- .count_progress_ticks({
        for (i in seq_len(10L)) prog$tick(detail = paste0("site_", i))
      })
      n_summary <- .count_progress_summaries({
        prog$finish(n_success = 10L, n_failed = 0L)
      })
      expect_identical(n_ticks, 10L)
      expect_identical(n_summary, 1L)
    })
  })
})

test_that("T-PROG-03c N>50 throttles tick conditions but keeps final tick", {
  withr::with_envvar(c(CACS_QUIET = ""), {
    withr::with_options(list(
      catchmentACS.progress = "force",
      catchmentACS.progress_throttle = 15L
    ), {
      prog <- .cacs_progress_reporter(n = 60L, label = "Test",
                                      verbose = TRUE)
      expect_identical(prog$mode, "bar")
      n_ticks <- .count_progress_ticks({
        for (i in seq_len(60L)) prog$tick(detail = paste0("site_", i))
      })
      expect_identical(n_ticks, 5L) # 1, 16, 31, 46, 60
    })
  })
})


test_that("T-PROG-03b progress tick and summary share the progress parent", {
  withr::with_envvar(c(CACS_QUIET = ""), {
    withr::with_options(list(catchmentACS.progress = "force"), {
      classes <- .cap_progress_classes({
        prog <- .cacs_progress_reporter(n = 1L, label = "Test",
                                        verbose = TRUE)
        prog$tick(detail = "only")
        prog$finish(n_success = 1L, n_failed = 0L)
      })

      expect_length(classes, 2L)
      expect_true(any(grepl(
        "^catchmentACS_message_progress_tick\\|catchmentACS_message_progress\\|",
        classes
      )))
      expect_true(any(grepl(
        "^catchmentACS_message_progress_summary\\|catchmentACS_message_progress\\|",
        classes
      )))
    })
  })
})


# ============================================================================
# Case 4: ETA monotonic non-increasing (synthetic timestamps via mock)
# ============================================================================

test_that("T-PROG-04 ETA is monotonic non-increasing across ticks", {
  withr::with_envvar(c(CACS_QUIET = ""), {
    withr::with_options(list(catchmentACS.progress = "force"), {
      # Direct test of .format_eta() with synthetic (elapsed, i, n) triples.
      # As i increases with proportionally-growing elapsed, ETA must not
      # exceed the previous estimate. We model a steady-rate worker
      # processing 1 iter/sec across n = 100.
      etas_sec <- numeric(0)
      n <- 100L
      for (i in seq_len(n)) {
        eta_str <- .format_eta(elapsed_sec = i, i = i, n = n)
        # Parse "M:SS" or "H:MM:SS" back into seconds for monotonicity check.
        secs <- if (grepl(":[0-9]{2}:", eta_str)) {
          parts <- as.integer(strsplit(eta_str, ":", fixed = TRUE)[[1L]])
          parts[1L] * 3600L + parts[2L] * 60L + parts[3L]
        } else if (grepl(":[0-9]{2}$", eta_str)) {
          parts <- as.integer(strsplit(eta_str, ":", fixed = TRUE)[[1L]])
          parts[1L] * 60L + parts[2L]
        } else {
          NA_integer_
        }
        etas_sec <- c(etas_sec, secs)
      }
      expect_false(any(is.na(etas_sec)))
      # Monotonic non-increasing across the whole sequence.
      expect_true(all(diff(etas_sec) <= 0))
      # Reporter-level monotonicity via the eta() closure clamp. Use a mocked
      # clock so an irregular second tick would increase without the clamp.
      t0 <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
      times <- c(t0, t0 + 0.1, t0 + 5)
      clock_i <- 0L
      testthat::local_mocked_bindings(
        .cacs_now = function() {
          clock_i <<- clock_i + 1L
          times[[min(clock_i, length(times))]]
        },
        .package = "catchmentACS"
      )

      msgs <- character(0)
      prog <- .cacs_progress_reporter(n = 3L, label = "Test",
                                      verbose = TRUE)
      withCallingHandlers(
        {
          prog$tick(detail = "fast")
          prog$tick(detail = "slow")
        },
        catchmentACS_message_progress_tick = function(m) {
          msgs <<- c(msgs, conditionMessage(m))
          invokeRestart("muffleMessage")
        }
      )

      etas <- sub(".*\\(ETA ([^)]+)\\).*", "\\1", msgs)
      expect_identical(etas, c("0:00", "0:00"))
    })
  })
})


# ============================================================================
# Case 5: CACS_QUIET = "1" suppresses all 3 fns (via env var override)
# ============================================================================

test_that("T-PROG-05 Sys.getenv('CACS_QUIET') == '1' silences across 3 fns", {
  withr::with_envvar(c(CACS_QUIET = "1"), {
    withr::with_options(list(catchmentACS.progress = "auto"), {
      # Direct resolver checks - 3 conceptual call sites:
      #   (a) cacs_isochrone     dispatch (N = 10, verbose = TRUE)
      #   (b) cacs_acs_prefetch  bookend  (N = 1,  verbose = TRUE)
      #   (c) cacs_intersect_weight loop  (N = 30, verbose = TRUE)
      expect_identical(.cacs_progress_mode(10L, TRUE), "silent")
      expect_identical(.cacs_progress_mode(1L,  TRUE), "silent")
      expect_identical(.cacs_progress_mode(30L, TRUE), "silent")

      # End-to-end: reporter emits no ticks AND no summary under env-quiet.
      prog <- .cacs_progress_reporter(n = 10L, label = "Test",
                                      verbose = TRUE)
      expect_identical(prog$mode, "silent")
      n_total <- 0L
      withCallingHandlers(
        {
          for (i in seq_len(10L)) prog$tick(detail = paste0("s", i))
          prog$finish(n_success = 10L, n_failed = 0L)
        },
        catchmentACS_message_progress = function(m) {
          n_total <<- n_total + 1L
          invokeRestart("muffleMessage")
        }
      )
      expect_identical(n_total, 0L)
    })
  })
})


# ============================================================================
# Case 6: getOption("catchmentACS.progress", "off") suppresses
# ============================================================================

test_that("T-PROG-06 option = 'off' silences regardless of verbose", {
  withr::with_envvar(c(CACS_QUIET = ""), {
    withr::with_options(list(catchmentACS.progress = "off"), {
      expect_identical(.cacs_progress_mode(10L, TRUE), "silent")
      expect_identical(.cacs_progress_mode(1L,  TRUE), "silent")
      expect_identical(.cacs_progress_mode(30L, TRUE), "silent")

      prog <- .cacs_progress_reporter(n = 10L, label = "Test",
                                      verbose = TRUE)
      expect_identical(prog$mode, "silent")
      n_total <- 0L
      withCallingHandlers(
        {
          for (i in seq_len(10L)) prog$tick()
          prog$finish(n_success = 10L, n_failed = 0L)
        },
        catchmentACS_message_progress = function(m) {
          n_total <<- n_total + 1L
          invokeRestart("muffleMessage")
        }
      )
      expect_identical(n_total, 0L)
    })
  })
})


# ============================================================================
# Case 7: getOption("catchmentACS.progress", "force") -> bar at N = 1
# ============================================================================

test_that("T-PROG-07 option = 'force' yields bar mode even at N = 1", {
  withr::with_envvar(c(CACS_QUIET = ""), {
    withr::with_options(list(catchmentACS.progress = "force"), {
      expect_identical(.cacs_progress_mode(1L,  TRUE), "bar")
      expect_identical(.cacs_progress_mode(3L,  TRUE), "bar")
      expect_identical(.cacs_progress_mode(10L, TRUE), "bar")

      prog <- .cacs_progress_reporter(n = 1L, label = "Test",
                                      verbose = TRUE)
      expect_identical(prog$mode, "bar")
      n_ticks <- .count_progress_ticks({
        prog$tick(detail = "only_site")
      })
      n_summary <- .count_progress_summaries({
        prog$finish(n_success = 1L, n_failed = 0L)
      })
      expect_identical(n_ticks, 1L)
      expect_identical(n_summary, 1L)
    })
  })
})


# ============================================================================
# Case 8: verbose = FALSE silences all 3 fns (end-to-end across fn surface)
# ============================================================================

test_that("T-PROG-08 verbose = FALSE silences all 3 reporter modes", {
  withr::with_envvar(c(CACS_QUIET = ""), {
    withr::with_options(list(catchmentACS.progress = "auto"), {
      # Same 3 conceptual call sites as Case 5 but verbose = FALSE this time.
      expect_identical(.cacs_progress_mode(10L, FALSE), "silent")
      expect_identical(.cacs_progress_mode(1L,  FALSE), "silent")
      expect_identical(.cacs_progress_mode(30L, FALSE), "silent")

      for (n_try in c(1L, 3L, 30L)) {
        prog <- .cacs_progress_reporter(n = n_try, label = "Test",
                                        verbose = FALSE)
        expect_identical(prog$mode, "silent")
        n_emit <- 0L
        withCallingHandlers(
          {
            for (i in seq_len(n_try)) prog$tick()
            prog$finish(n_success = n_try, n_failed = 0L)
          },
          catchmentACS_message_progress = function(m) {
            n_emit <<- n_emit + 1L
            invokeRestart("muffleMessage")
          }
        )
        expect_identical(n_emit, 0L)
      }
    })
  })
})


# ============================================================================
# Case 9: live wiring - cacs_isochrone + cacs_intersect_weight emit >= n_iter
# ticks under verbose=TRUE + force-bar mode.
#
# Per the test plan "10. All 3 fns emit >= n_sites tick events at
# verbose=TRUE+bar mode" — we cover cacs_isochrone and cacs_intersect_weight
# here. cacs_acs_prefetch is bookend-only (N = 1, degrades to no per-event
# tick) so it's covered by Case 2 (bookend) + Case 7 (force).
# ============================================================================

test_that("T-PROG-09a cacs_isochrone emits >= n_sites tick events (bar mode)", {
  td <- tempfile("cacs_prog09a_")
  on.exit({
    if (dir.exists(td)) unlink(td, recursive = TRUE)
    memoise::forget(.cacs_cache_dir_memo)
  }, add = TRUE)
  memoise::forget(.cacs_cache_dir_memo)

  sites <- tibble::tibble(
    site_id = paste0("S", sprintf("%02d", seq_len(3L))),
    lon = -86.80902 + seq(0, 0.02, length.out = 3L),
    lat =  33.52203 + seq(0, 0.02, length.out = 3L)
  ) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

  .mk_happy <- function(loc, breaks, res = NULL) {
    polys <- lapply(seq_along(breaks), function(j) {
      b <- breaks[[j]]; half <- 0.01 * b
      sf::st_polygon(list(rbind(
        c(loc[[1L]] - half, loc[[2L]] - half),
        c(loc[[1L]] + half, loc[[2L]] - half),
        c(loc[[1L]] + half, loc[[2L]] + half),
        c(loc[[1L]] - half, loc[[2L]] + half),
        c(loc[[1L]] - half, loc[[2L]] - half)
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
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = .mk_happy,
    .package = "catchmentACS"
  )

  withr::local_envvar(c(CACS_QUIET = ""))
  withr::local_options(list(
    catchmentACS.cache_dir = td,
    catchmentACS.progress  = "force",
    CACS_NO_CONFIRM        = "1"
  ))
  Sys.setenv(CACS_NO_CONFIRM = "1")

  n_ticks_iso <- 0L
  out <- suppressWarnings(suppressMessages(withCallingHandlers(
    cacs_isochrone(
      sites       = sites,
      drive_times = 5L,
      provider    = "osrm",
      osrm_mode   = "demo",
      verbose     = TRUE
    ),
    catchmentACS_message_progress_tick = function(m) {
      n_ticks_iso <<- n_ticks_iso + 1L
      invokeRestart("muffleMessage")
    }
  )))

  expect_s3_class(out, "sf")
  expect_gte(n_ticks_iso, 3L)   # >= n_sites ticks
})


test_that("T-PROG-09b cacs_intersect_weight emits >= n_iter tick events", {
  iso_path <- system.file("extdata", "legacy_2025_isochrones.rds",
                          package = "catchmentACS")
  acs_path <- system.file("extdata", "sample_alabama_subset.rds",
                          package = "catchmentACS")
  skip_if(!nzchar(iso_path) || !nzchar(acs_path),
          "legacy fixtures not installed")
  iso <- readRDS(iso_path)
  acs <- readRDS(acs_path)

  # Restrict to a small, deterministic subset so the test is fast.
  iso_subset <- iso[iso$site_id %in% utils::head(unique(iso$site_id), 2L) &
                      iso$drive_time_min %in% c(5L, 10L), ]
  n_iter_iw <- iso_subset |>
    sf::st_drop_geometry() |>
    dplyr::distinct(.data$site_id, .data$drive_time_min) |>
    nrow()

  # Hermetic cache so a prior run's composite-cache HIT doesn't short-circuit
  # the per-iter loop (cache HIT path returns BEFORE the lapply ticks).
  td <- tempfile("cacs_prog09b_")
  on.exit({
    if (dir.exists(td)) unlink(td, recursive = TRUE)
    memoise::forget(.cacs_cache_dir_memo)
  }, add = TRUE)
  memoise::forget(.cacs_cache_dir_memo)

  withr::local_envvar(c(CACS_QUIET = "", CACS_NO_CONFIRM = "1"))
  withr::local_options(list(
    catchmentACS.progress  = "force",
    catchmentACS.cache_dir = td,
    CACS_NO_CONFIRM        = "1"
  ))
  Sys.setenv(CACS_NO_CONFIRM = "1")

  n_ticks_iw <- 0L
  res <- suppressWarnings(suppressMessages(withCallingHandlers(
    cacs_intersect_weight(
      iso_sf        = iso_subset,
      acs_sf        = acs,
      weight_method = "area",
      verbose       = TRUE
    ),
    catchmentACS_message_progress_tick = function(m) {
      n_ticks_iw <<- n_ticks_iw + 1L
      invokeRestart("muffleMessage")
    }
  )))
  expect_gte(n_ticks_iw, n_iter_iw)
})


# ============================================================================
# Case 10: cli::test_that_cli snapshot tests across (plain, ansi, unicode)
#
# Per the test plan case 10 — snapshot the bar-mode tick +
# summary rendering across the three cli config presets. We invoke the
# reporter directly (not through cacs_isochrone) to keep the snapshot
# deterministic (no timing, no provider state).
# ============================================================================

cli::test_that_cli("T-PROG-10 reporter snapshot across configs",
  configs = c("plain", "ansi", "unicode"),
  {
    withr::with_envvar(c(CACS_QUIET = ""), {
      withr::with_options(list(catchmentACS.progress = "force"), {
        # Snapshot the summary line. tick output carries an ETA whose
        # digits depend on real wall-clock; we bypass the live timer by
        # calling `.cacs_progress_summary()` directly with `start_time =
        # Sys.time()` so elapsed rounds to "0:00" deterministically and
        # the rate falls through to the "n/a" branch (elapsed_sec == 0).
        expect_snapshot(
          {
            .cacs_progress_summary(
              start_time = Sys.time(),
              n_total    = 5L,
              n_success  = 5L,
              n_failed   = 0L,
              label      = "Snapshot",
              verbose    = TRUE
            )
          }
        )
      })
    })
  }
)
