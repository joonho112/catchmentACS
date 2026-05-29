# ===========================================================================
# test-issue004-cross-surface-consistency.R
#
# v0.4 plan Step 5.5 — architectural invariant: all 4 tracks expose the
# SAME per-site wide table. This is the lock-test that makes the
# Issue 004 "single internal pivot" promise enforceable.
# ===========================================================================

.rr_fixture <- function() {
  readRDS(system.file("testdata", "run_result_3site_demo.rds", package = "catchmentACS"))
}

# --- INV-01: summary()$rates_per_site == direct pivot helper output --------

test_that("CONSISTENCY-01 summary()$rates_per_site = .cacs_rates_per_site_pivot()$wide", {
  rr <- .rr_fixture()
  sm <- summary(rr)
  direct <- catchmentACS:::.cacs_rates_per_site_pivot(rr)
  expect_identical(sm$rates_per_site, direct$wide)
})

# --- INV-02: summary()$rates_per_site == as_tibble(x) filter+pivot ---------

test_that("CONSISTENCY-02 summary slot == as_tibble + filter + pivot_wider", {
  rr <- .rr_fixture()
  sm <- summary(rr)
  rate_vars <- names(cacs_acs_default_rates)
  manual <- tibble::as_tibble(rr, rate_first = FALSE) |>
    dplyr::filter(.data$variable %in% rate_vars) |>
    dplyr::select("site_id", "drive_time_min", "variable", "estimate") |>
    tidyr::pivot_wider(names_from = "variable",
                       values_from = "estimate") |>
    dplyr::select(dplyr::all_of(c("site_id", "drive_time_min", rate_vars)))

  # Key columns equal (order-aware)
  expect_equal(
    sm$rates_per_site[c("site_id", "drive_time_min")],
    manual[c("site_id", "drive_time_min")]
  )

  # Numeric values equal at byte level for each (site, drive_time, rate) cell
  for (i in seq_len(nrow(sm$rates_per_site))) {
    sm_row <- sm$rates_per_site[i, ]
    man_row <- manual[
      manual$site_id == sm_row$site_id &
        manual$drive_time_min == sm_row$drive_time_min,
    ]
    for (rv in rate_vars) {
      expect_equal(sm_row[[rv]][1L], man_row[[rv]][1L], tolerance = 1e-9,
                   info = paste("site =", sm_row$site_id,
                                "drive_time =", sm_row$drive_time_min,
                                "rate =", rv))
    }
  }
})

# --- INV-03: rates_per_site column order = .SANCTIONED_RATES_V1 ------------

test_that("CONSISTENCY-03 rates_per_site column order = SANCTIONED_RATES_V1", {
  rr <- .rr_fixture()
  sm <- summary(rr)
  expect_identical(
    colnames(sm$rates_per_site),
    c("site_id", "drive_time_min", names(cacs_acs_default_rates))
  )
})

# --- INV-04: cross-key mean(rates_per_site) == rates_breakdown$mean --------

test_that("CONSISTENCY-04 cross-key mean of rates_per_site equals rates_breakdown$mean", {
  rr <- .rr_fixture()
  sm <- summary(rr)
  rate_vars <- names(cacs_acs_default_rates)
  for (rv in rate_vars) {
    direct_mean <- mean(sm$rates_per_site[[rv]], na.rm = TRUE)
    bd_mean <- sm$rates_breakdown$mean[sm$rates_breakdown$variable == rv]
    expect_equal(direct_mean, bd_mean, tolerance = 1e-12,
                 info = paste("rate =", rv))
  }
})

# --- INV-04b: rates_breakdown equals plain-tibble grouped aggregation --------

test_that("CONSISTENCY-04b rates_breakdown matches plain tibble aggregation", {
  rr <- .rr_fixture()
  sm <- summary(rr)
  rate_vars <- names(cacs_acs_default_rates)

  from_summary <- sm$rates_breakdown |>
    dplyr::arrange(.data$variable)

  from_plain <- tibble::as_tibble(rr, rate_first = FALSE) |>
    dplyr::filter(.data$variable %in% rate_vars) |>
    dplyr::group_by(.data$variable) |>
    dplyr::summarise(
      mean = mean(.data$estimate, na.rm = TRUE),
      sd   = stats::sd(.data$estimate, na.rm = TRUE),
      n_NA = sum(is.na(.data$estimate)),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$variable)

  expect_equal(from_summary, from_plain, tolerance = 1e-12)
})

# --- INV-05: rates_per_site_moe "x ± y" → x equals rates_per_site cell ----

test_that("CONSISTENCY-05 rates_per_site_moe parse x = rates_per_site cell", {
  rr <- .rr_fixture()
  sm <- summary(rr)
  rate_vars <- names(cacs_acs_default_rates)
  for (i in seq_len(nrow(sm$rates_per_site))) {
    sid <- sm$rates_per_site$site_id[[i]]
    dt <- sm$rates_per_site$drive_time_min[[i]]
    for (rv in rate_vars) {
      key <- sm$rates_per_site_moe$site_id == sid &
        sm$rates_per_site_moe$drive_time_min == dt
      cell <- sm$rates_per_site_moe[key, ][[rv]]
      val <- sm$rates_per_site[i, ][[rv]]
      if (!is.na(val) && !is.na(cell)) {
        # Parse "0.193 ± 0.024" → 0.193
        parsed <- as.numeric(strsplit(cell, " ± ", fixed = TRUE)[[1L]][[1L]])
        # Tolerance 1e-2 because the wide_moe formats numbers with `digits = 3`
        # which can introduce rounding (e.g., 0.0494 → "0.049"), so the
        # parsed value may differ from the unrounded `val` by up to one digit.
        expect_equal(parsed, val, tolerance = 1e-2,
                     info = paste("site =", sid, "drive_time =", dt,
                                  "rate =", rv))
      }
    }
  }
})

# --- INV-06: pivot helper is the single source of truth -------------------

test_that("CONSISTENCY-06 multiple calls to pivot return identical output", {
  rr <- .rr_fixture()
  out1 <- catchmentACS:::.cacs_rates_per_site_pivot(rr)
  out2 <- catchmentACS:::.cacs_rates_per_site_pivot(rr)
  expect_identical(out1$wide, out2$wide)
  expect_identical(out1$wide_moe, out2$wide_moe)
})

# --- INV-07: Markdown render of rates_per_site is reversible (smoke) ------

test_that("CONSISTENCY-07 markdown render of rates_per_site contains all sites", {
  rr <- .rr_fixture()
  sm <- summary(rr)
  md <- cacs_summary_as_markdown(sm$rates_per_site)
  md_str <- paste(as.character(md), collapse = "\n")
  for (sid in unique(sm$rates_per_site$site_id)) {
    expect_match(md_str, sid, fixed = TRUE)
  }
})
