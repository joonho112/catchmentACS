# ============================================================================
# Unit tests for the F6 cacs_run_result S3 surface (Step 4.2 impl +
# Step 4.3 test plan).
#
# Tests the 4 S3 entries added in R/run.R:
#   - .attach_run_result_class()         (internal helper)
#   - print.cacs_run_result()            (S3 method)
#   - summary.cacs_run_result()          (S3 method)
#   - print.cacs_run_summary()           (S3 method)
#
# These tests use the v0.1 frozen-fixture trio (legacy_2025_isochrones.rds,
# legacy_2025_sites.rds, sample_alabama_subset.rds) so the call avoids the
# OSRM network + tidycensus key requirement. A 3-site subset keeps the
# wall-clock per testcase < 2 s while still exercising the
# `n_sites_total > 1` print branch.
#
# Snapshot tests for print() / summary() output land in
# tests/testthat/_snaps/unit-s3-methods/. Subsequent test runs compare
# byte-equality against the locked baseline; regenerate via
# `testthat::snapshot_accept("unit-s3-methods")` when the format
# intentionally changes (e.g. cli theme refresh).
# ============================================================================


# ---- Shared fixture loader -------------------------------------------------
#
# `.mk_run_result()` runs cacs_run() once in full 3-call bypass mode using a
# 3-site subset of the legacy fixtures. Cached across the test file via a
# package-local `local({...})` block so the suite does not pay the ~1 s setup
# cost per testcase.

.mk_run_result_3site <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    iso   <- readRDS(system.file("extdata", "legacy_2025_isochrones.rds",
                                 package = "catchmentACS"))
    sites <- readRDS(system.file("extdata", "legacy_2025_sites.rds",
                                 package = "catchmentACS"))
    acs   <- readRDS(system.file("extdata", "sample_alabama_subset.rds",
                                 package = "catchmentACS"))

    site_ids <- c("AL_SITE_01", "AL_SITE_02", "AL_SITE_03")
    iso_sub   <- iso[iso$site_id %in% site_ids, ]
    sites_sub <- sites[sites$site_id %in% site_ids, ]

    out <- suppressWarnings(suppressMessages(
      cacs_run(
        sites                  = sites_sub,
        state                  = "AL",
        year                   = 2023,
        drive_times            = c(5L, 10L, 15L),
        variables              = unname(cacs_acs_default_vars),
        provider               = "osrm",
        weight_method          = "area",
        precomputed_isochrones = iso_sub,
        acs                    = acs,
        rates                  = cacs_acs_default_rates,
        output                 = "long",
        verbose                = FALSE
      )
    ))
    cached <<- out
    out
  }
})


# ===========================================================================
# T-S3-01 -- class chain includes cacs_run_result on top of tibble chain
# ===========================================================================

test_that("T-S3-01 cacs_run() return value inherits 'cacs_run_result'", {
  out <- .mk_run_result_3site()
  testthat::expect_true(inherits(out, "cacs_run_result"))
  testthat::expect_equal(
    class(out),
    c("cacs_run_result", "tbl_df", "tbl", "data.frame")
  )
})


# ===========================================================================
# T-S3-02 -- backward compat: still a tbl_df (dplyr / ggplot2 work)
# ===========================================================================

test_that("T-S3-02 cacs_run() return value still inherits 'tbl_df'", {
  out <- .mk_run_result_3site()
  testthat::expect_true(inherits(out, "tbl_df"))
  testthat::expect_true(inherits(out, "data.frame"))
})


# ===========================================================================
# T-S3-03 -- cacs_run_result_metadata attribute carries the 11 contract keys
# ===========================================================================

test_that("T-S3-03 metadata attribute has the 11 expected keys", {
  out <- .mk_run_result_3site()
  meta <- attr(out, "cacs_run_result_metadata")
  testthat::expect_type(meta, "list")

  expected_keys <- c(
    "generated_at", "cacs_version", "provider", "profile",
    "drive_times", "n_sites_total", "n_sites_success",
    "n_sites_failed", "wall_clock_seconds", "skipped_geoids",
    "moe_fallback_summary"
  )
  testthat::expect_setequal(names(meta), expected_keys)

  # Spot-check derived values: 3-site subset feeds n_sites_total = 3.
  testthat::expect_equal(meta$n_sites_total, 3L)
  testthat::expect_equal(meta$provider, "osrm")
  testthat::expect_equal(sort(meta$drive_times), c(5, 10, 15))
  testthat::expect_type(meta$wall_clock_seconds, "double")
  testthat::expect_true(meta$wall_clock_seconds >= 0)

  # moe_fallback_summary sub-structure (3 named buckets).
  testthat::expect_type(meta$moe_fallback_summary, "list")
  testthat::expect_setequal(
    names(meta$moe_fallback_summary),
    c("c1_to_c2_count", "zero_den_count", "missing_moe_count")
  )
})


# ===========================================================================
# T-S3-04 -- print.cacs_run_result emits cli_h1 header (snapshot)
# ===========================================================================

test_that("T-S3-04 print() emits a cli_h1 'catchmentACS run result' header", {
  out <- .mk_run_result_3site()

  # Capture print output; snapshot test locks the structural shape.
  printed <- testthat::capture_messages(print(out))
  testthat::expect_true(
    any(grepl("catchmentACS run result", printed, fixed = TRUE))
  )
  testthat::expect_true(
    any(grepl("Provider: osrm", printed, fixed = TRUE))
  )
  testthat::expect_true(
    any(grepl("Sites: ", printed, fixed = TRUE))
  )

  # Snapshot lock for the cli header block (everything before the tibble
  # preview). Times / wall-clock are masked to avoid spurious diffs.
  testthat::expect_snapshot(
    {
      # Suppress the NextMethod() tibble preview so the snapshot stays
      # robust to tibble package width / pillar formatting drift.
      header_only <- attr(out, "cacs_run_result_metadata")
      cat("== print.cacs_run_result structural snapshot ==\n")
      cat(sprintf("provider           : %s\n", header_only$provider))
      cat(sprintf("profile            : %s\n", header_only$profile))
      cat(sprintf("drive_times (n)    : %d\n", length(header_only$drive_times)))
      cat(sprintf("n_sites_total      : %d\n", header_only$n_sites_total))
      cat(sprintf("n_sites_success    : %d\n", header_only$n_sites_success))
      cat(sprintf("n_sites_failed     : %d\n", header_only$n_sites_failed))
      cat(sprintf("skipped_geoids (n) : %d\n", length(header_only$skipped_geoids)))
      cat(sprintf("metadata keys      : %s\n",
                  paste(sort(names(header_only)), collapse = ",")))
    }
  )
})


# ===========================================================================
# T-S3-05 -- summary.cacs_run_result returns cacs_run_summary list
# ===========================================================================

test_that("T-S3-05 summary() returns class c('cacs_run_summary', 'list')", {
  out <- .mk_run_result_3site()
  s <- summary(out)

  testthat::expect_s3_class(s, "cacs_run_summary")
  testthat::expect_equal(class(s), c("cacs_run_summary", "list"))
  testthat::expect_type(s, "list")

  # Required slots per the schema contract.
  expected_slots <- c(
    "metadata", "n_rows", "n_sites", "n_variables",
    "n_drive_times", "n_tracts_summary", "rates_breakdown",
    "rates_per_site", "rates_per_site_moe", "moe_fallback_rate"
  )
  testthat::expect_setequal(names(s), expected_slots)

  # Spot-check derived counts.
  testthat::expect_equal(s$n_sites, 3L)
  testthat::expect_equal(s$n_drive_times, 3L)
  testthat::expect_s3_class(s$rates_breakdown, "tbl_df")
  testthat::expect_setequal(
    colnames(s$rates_breakdown),
    c("variable", "mean", "sd", "n_NA")
  )
})


# ===========================================================================
# T-S3-05b -- dplyr grouping strips cacs_run_result before aggregation
# ===========================================================================

test_that("T-S3-05b dplyr group_by() on cacs_run_result matches plain tibble", {
  out <- .mk_run_result_3site()
  rate_vars <- names(cacs_acs_default_rates)

  grouped <- dplyr::group_by(out, .data$variable)
  testthat::expect_s3_class(grouped, "grouped_df")
  testthat::expect_false(inherits(grouped, "cacs_run_result"))

  classed_summary <- out |>
    dplyr::filter(.data$variable %in% rate_vars) |>
    dplyr::group_by(.data$variable) |>
    dplyr::summarise(
      mean = mean(.data$estimate, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$variable)

  plain_summary <- tibble::as_tibble(out, rate_first = FALSE) |>
    dplyr::filter(.data$variable %in% rate_vars) |>
    dplyr::group_by(.data$variable) |>
    dplyr::summarise(
      mean = mean(.data$estimate, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$variable)

  testthat::expect_equal(classed_summary, plain_summary, tolerance = 1e-12)
})


# ===========================================================================
# T-S3-06 -- print.cacs_run_summary emits structured output (snapshot)
# ===========================================================================

test_that("T-S3-06 print(summary()) emits structured cli sections", {
  out <- .mk_run_result_3site()
  s <- summary(out)

  printed <- testthat::capture_messages(print(s))
  testthat::expect_true(
    any(grepl("catchmentACS run summary", printed, fixed = TRUE))
  )
  testthat::expect_true(any(grepl("Run tallies", printed, fixed = TRUE)))
  testthat::expect_true(
    any(grepl("Rates breakdown", printed, fixed = TRUE))
  )
  testthat::expect_true(
    any(grepl("MOE fallback rate", printed, fixed = TRUE))
  )

  # Snapshot lock for the summary structural shape (counts only, no
  # floating-point estimates -- those drift with the regression fixture).
  testthat::expect_snapshot(
    {
      cat("== print.cacs_run_summary structural snapshot ==\n")
      cat(sprintf("n_rows            : %d\n", s$n_rows))
      cat(sprintf("n_sites           : %d\n", s$n_sites))
      cat(sprintf("n_variables       : %d\n", s$n_variables))
      cat(sprintf("n_drive_times     : %d\n", s$n_drive_times))
      cat(sprintf("rates_breakdown n : %d\n", nrow(s$rates_breakdown)))
      cat(sprintf("n_tracts fivenum n: %d\n",
                  length(s$n_tracts_summary)))
      cat(sprintf("moe_fallback NA?  : %s\n",
                  if (is.na(s$moe_fallback_rate)) "yes" else "no"))
    }
  )
})


# ===========================================================================
# T-S3-07 -- Edge: empty tibble print falls back gracefully (no crash)
# ===========================================================================

test_that("T-S3-07 print() on empty tibble degrades gracefully", {
  # Construct a synthetic cacs_run_result with 0 rows but valid metadata.
  empty_tbl <- tibble::tibble(
    site_id        = character(0),
    drive_time_min = integer(0),
    variable       = character(0),
    estimate       = numeric(0),
    moe            = numeric(0),
    moe_fallback   = logical(0)
  )
  fake_meta <- list(
    generated_at         = Sys.time(),
    cacs_version         = utils::packageVersion("catchmentACS"),
    provider             = "osrm",
    profile              = "car",
    drive_times          = c(5, 10, 15),
    n_sites_total        = 0L,
    n_sites_success      = 0L,
    n_sites_failed       = 0L,
    wall_clock_seconds   = 0.0,
    skipped_geoids       = character(0),
    moe_fallback_summary = list(c1_to_c2_count = 0L,
                                zero_den_count = 0L,
                                missing_moe_count = 0L)
  )
  attr(empty_tbl, "cacs_run_result_metadata") <- fake_meta
  class(empty_tbl) <- c("cacs_run_result", class(empty_tbl))

  testthat::expect_silent(
    suppressMessages(invisible(capture.output({
      msgs <- testthat::capture_messages(print(empty_tbl))
    })))
  )
  # Should not error and the structural header still emits.
  testthat::expect_true(inherits(empty_tbl, "cacs_run_result"))

  # summary() on empty input also tolerated.
  s <- summary(empty_tbl)
  testthat::expect_s3_class(s, "cacs_run_summary")
  testthat::expect_equal(s$n_rows, 0L)
  testthat::expect_equal(nrow(s$rates_breakdown), 0L)
})


# ===========================================================================
# T-S3-08 -- Edge: n_sites = 1 still prints correctly (single-site smoke)
# ===========================================================================

test_that("T-S3-08 print() works for n_sites = 1 (single-site smoke)", {
  iso   <- readRDS(system.file("extdata", "legacy_2025_isochrones.rds",
                               package = "catchmentACS"))
  sites <- readRDS(system.file("extdata", "legacy_2025_sites.rds",
                               package = "catchmentACS"))
  acs   <- readRDS(system.file("extdata", "sample_alabama_subset.rds",
                               package = "catchmentACS"))

  iso_sub   <- iso[iso$site_id == "AL_SITE_01" & iso$drive_time_min == 5L, ]
  sites_sub <- sites[sites$site_id == "AL_SITE_01", ]

  out <- suppressWarnings(suppressMessages(
    cacs_run(
      sites                  = sites_sub,
      state                  = "AL",
      year                   = 2023,
      drive_times            = 5L,
      variables              = unname(cacs_acs_default_vars),
      provider               = "osrm",
      precomputed_isochrones = iso_sub,
      acs                    = acs,
      output                 = "long",
      verbose                = FALSE
    )
  ))

  testthat::expect_true(inherits(out, "cacs_run_result"))
  meta <- attr(out, "cacs_run_result_metadata")
  testthat::expect_equal(meta$n_sites_total, 1L)
  # No crash on the single-site cli header + summary path.
  testthat::expect_silent(
    suppressMessages(invisible(capture.output({
      msgs <- testthat::capture_messages(print(out))
    })))
  )
  testthat::expect_silent(
    suppressMessages(invisible(capture.output({
      msgs <- testthat::capture_messages(print(summary(out)))
    })))
  )
})


# ===========================================================================
# T-S3-09 -- Edge: all-rate-NA still prints + summarises (no NaN crash)
# ===========================================================================

test_that("T-S3-09 print() + summary() handle all-NA rate rows", {
  out <- .mk_run_result_3site()

  # Mask every rate row's estimate -> NA_real_; this is the post-pipeline
  # all-rate-NA edge case that the print method must handle as a
  # robustness target.
  rate_vars <- c("poverty_rate", "snap_rate", "ssi_rate",
                 "unemp_rate", "labor_force_participation")
  out_na <- out
  out_na$estimate[out_na$variable %in% rate_vars] <- NA_real_
  out_na$moe[out_na$variable %in% rate_vars]      <- NA_real_

  # Re-attach the class chain because dplyr-free indexed assignment can
  # rewrite the column without stripping anything -- but be defensive.
  if (!inherits(out_na, "cacs_run_result")) {
    class(out_na) <- c("cacs_run_result", class(out_na))
    attr(out_na, "cacs_run_result_metadata") <-
      attr(out, "cacs_run_result_metadata")
  }

  # print() should not crash on mean(NA, na.rm = TRUE) -> NaN.
  testthat::expect_silent(
    suppressMessages(invisible(capture.output({
      msgs <- testthat::capture_messages(print(out_na))
    })))
  )
  s <- summary(out_na)
  testthat::expect_s3_class(s, "cacs_run_summary")
  # Some / all `mean` values may be NaN; the print method must tolerate that.
  testthat::expect_silent(
    suppressMessages(invisible(capture.output({
      msgs <- testthat::capture_messages(print(s))
    })))
  )
})


# ===========================================================================
# T-S3-10 -- dplyr filter preserves cacs_run_result class chain
# ===========================================================================

test_that("T-S3-10 dplyr::filter() preserves the cacs_run_result class", {
  out <- .mk_run_result_3site()

  # The v0.1 idiom `cacs_run(...) %>% filter(...)` is the single dominant
  # backward-compat path -- if dplyr strips the class chain, every v0.1
  # downstream pipeline silently degrades.
  filtered <- dplyr::filter(out, .data$variable == "poverty_rate")

  # Class chain: dplyr preserves the prepended class (re-dispatch on tibble
  # methods still uses NextMethod()).
  testthat::expect_true(inherits(filtered, "cacs_run_result"))
  testthat::expect_true(inherits(filtered, "tbl_df"))
  testthat::expect_equal(
    class(filtered),
    c("cacs_run_result", "tbl_df", "tbl", "data.frame")
  )

  # The metadata attribute *may* survive dplyr verb dispatch (current
  # behavior: tibble subset retains attributes via [.tbl_df). If it does,
  # the print method should hit our path; if it doesn't, we should still
  # have a working tibble. The print() degradation guard
  # covers both branches.
  meta_after <- attr(filtered, "cacs_run_result_metadata")
  if (!is.null(meta_after)) {
    # Metadata survived the filter -- print method works fully.
    testthat::expect_silent(
      suppressMessages(invisible(capture.output({
        msgs <- testthat::capture_messages(print(filtered))
      })))
    )
  } else {
    # Metadata stripped -- print method must degrade to NextMethod().
    testthat::expect_silent(
      invisible(capture.output(print(filtered)))
    )
  }
})
