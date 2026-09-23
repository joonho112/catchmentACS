# helper-regression.R - regression test infrastructure for catchmentACS
#
# Hosts:
#   - expect_cacs_close(actual, expected, abs_tol, rel_tol = 0) --
#     absolute-plus-relative bound, NA-aware, max-diff info on failure.
#
# Note: skip_if_not_regression() already lives in helper-skip.R.
# Splitting that helper across files would create two definitions; we keep
# the skip guard in helper-skip.R and limit this file to the assertion
# helper specific to the regression suite.


#' Expect two numeric vectors close within an absolute (and optionally
#' relative) tolerance, with NA-aware comparison and informative max-diff
#' message.
#'
#' This helper deliberately avoids
#' `testthat::expect_equal(tolerance = ...)` because `all.equal()`'s
#' relative tolerance semantic can become looser than intended on
#' large-count rows (e.g., total_pop ~50000). The current synthetic golden
#' contract is absolute tolerance only (`rel_tol = 0`); `rel_tol > 0` is
#' a documented release valve for empirical GEOS/PROJ drift in future.
#'
#' On failure, the `info` field reports the maximum observed absolute
#' difference, the configured `abs_tol`, the configured `rel_tol`, and
#' the count of violating elements, so a per-row drill-down
#' can start from real numbers.
#'
#' @param actual numeric vector from `cacs_run()` output
#' @param expected numeric vector from frozen golden (same column, joined)
#' @param abs_tol absolute tolerance (e.g., 1e-6 for `estimate`,
#'   1e-5 for `moe`, 1e-9 for `weight_sum`)
#' @param rel_tol relative tolerance (default 0). When non-zero, the
#'   effective per-element tolerance is `abs_tol + rel_tol * max(|expected|, 1)`.
#' @return Invisibly returns the testthat expectation. Both-NA pairs are
#'   considered a match.
#' @keywords internal
#' @noRd
expect_cacs_close <- function(actual, expected, abs_tol, rel_tol = 0) {
  stopifnot(length(actual) == length(expected))

  both_na <- is.na(actual) & is.na(expected)
  diff    <- abs(actual - expected)
  tol     <- abs_tol + rel_tol * pmax(abs(expected), 1)

  # An element matches if BOTH are NA, OR diff is computable AND within tol.
  match <- both_na | (!is.na(diff) & diff <= tol)

  if (any(!match)) {
    bad <- which(!match)
    max_diff <- suppressWarnings(max(diff[!both_na], na.rm = TRUE))
    if (!is.finite(max_diff)) max_diff <- NA_real_
    msg <- sprintf(
      "expect_cacs_close: %d / %d failed (max diff = %.3e, abs_tol = %.3e, rel_tol = %.3e)",
      length(bad), length(actual), max_diff, abs_tol, rel_tol
    )
    testthat::fail(msg)
  } else {
    max_diff <- suppressWarnings(max(diff[!both_na], na.rm = TRUE))
    if (!is.finite(max_diff)) max_diff <- 0
    testthat::expect_true(
      TRUE,
      label = sprintf(
        "all %d values within abs_tol = %.3e (max diff = %.3e)",
        length(actual), abs_tol, max_diff
      )
    )
  }
}
