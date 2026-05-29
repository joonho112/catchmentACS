# ===========================================================================
# test-unit-capture-with-value.R
#
# v0.4 plan Step 2.4 — unit tests for `.cacs_capture_conditions_with_value()`
# (Layer 1 helper for Issue 001).
# ===========================================================================

.call_helper <- function(expr, classes = NULL) {
  quo <- rlang::enquo(expr)
  targets <- catchmentACS:::.cacs_normalize_capture_classes(classes)
  catchmentACS:::.cacs_capture_conditions_with_value(quo, targets)
}

# --- Tests -----------------------------------------------------------------

test_that("STEP24-01 simple arithmetic: result = expr value, conditions empty", {
  out <- .call_helper(1 + 1)
  expect_identical(out$result, 2)
  expect_s3_class(out$conditions, "tbl_df")
  expect_identical(nrow(out$conditions), 0L)
})

test_that("STEP24-02 expr emits message + returns tibble: result captured", {
  emit_helper <- function() {
    catchmentACS:::.cli_inform_listcol_iso_filled()
    tibble::tibble(x = 1:3)
  }
  out <- .call_helper(emit_helper())
  expect_s3_class(out$result, "tbl_df")
  expect_identical(nrow(out$result), 3L)
  # The message helper has no per-session throttle so the condition was captured.
  expect_identical(nrow(out$conditions), 1L)
  expect_identical(out$conditions$class[[1L]],
                   "catchmentACS_message_listcol_iso_filled")
})

test_that("STEP24-03 expr errors → propagates (not swallowed)", {
  expect_error(
    .call_helper(stop("intentional")),
    "intentional"
  )
})

test_that("STEP24-04 in-expr `<-` does NOT propagate to caller env (v0.3 footgun)", {
  # This is the *whole point* of return_value = "both": the result is captured
  # via $result, NOT via in-expr assignment.
  # We assign inside the quoted expr but the assignment is to the
  # eval_tidy frame — it does NOT bleed into this test_that() frame.
  out <- .call_helper(zz_test_var_in_expr <- 42L)
  expect_identical(out$result, 42L)
  # zz_test_var_in_expr should NOT exist in our scope (by design)
  expect_false(exists("zz_test_var_in_expr",
                      envir = environment(), inherits = FALSE))
})

test_that("STEP24-05 NULL return is preserved (not coerced)", {
  out <- .call_helper(invisible(NULL))
  expect_null(out$result)
  expect_identical(nrow(out$conditions), 0L)
})

test_that("STEP24-06 warning + value: warning captured + value returned", {
  warn_helper <- function() {
    rlang::warn(
      "test warning",
      class = c("catchmentACS_warning_runtime",
                "catchmentACS_warning",
                "catchmentACS_condition")
    )
    42L
  }
  out <- .call_helper(warn_helper())
  expect_identical(out$result, 42L)
  expect_identical(nrow(out$conditions), 1L)
})

test_that("STEP24-07 issue 001 reproducer: result = 42, caller env clean", {
  # The exact reproducer from the issue 001 qmd Minimal reproducer.
  out <- .call_helper(z <- 42)
  expect_identical(out$result, 42)
  expect_false(exists("z", envir = environment(), inherits = FALSE))
})

test_that("STEP24-08 targets filter: short suffix capture works", {
  emit_helper <- function() {
    catchmentACS:::.cli_inform_listcol_iso_filled()
    "ok"
  }
  out <- .call_helper(emit_helper(), classes = "listcol_iso_filled")
  expect_identical(out$result, "ok")
  expect_identical(nrow(out$conditions), 1L)
})

test_that("STEP24-09 targets filter: NULL = catch all catchmentACS conditions", {
  emit_helper <- function() {
    catchmentACS:::.cli_inform_listcol_iso_filled()
    "ok"
  }
  out <- .call_helper(emit_helper(), classes = NULL)
  expect_identical(nrow(out$conditions), 1L)
})

test_that("STEP24-10 v0.3 cacs_capture_conditions() public path unchanged", {
  # Regression test: the v0.3 public function (which we have NOT modified yet
  # — that's Step 3.1) should still work exactly as before.
  cap <- catchmentACS::cacs_capture_conditions(
    catchmentACS:::.cli_inform_listcol_iso_filled()
  )
  expect_s3_class(cap, "tbl_df")
  expect_identical(nrow(cap), 1L)
  expect_identical(cap$class[[1L]],
                   "catchmentACS_message_listcol_iso_filled")
})
