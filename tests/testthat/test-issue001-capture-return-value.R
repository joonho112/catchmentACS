# ===========================================================================
# test-issue001-capture-return-value.R
#
# v0.4 plan Step 3.1 — public surface tests for the new
# `cacs_capture_conditions(return_value = ...)` argument.
# ===========================================================================

# --- Default behavior: v0.3 backward-compat --------------------------------

test_that("ISSUE001-01 default return_value = 'conditions' matches v0.3 shape", {
  cap <- cacs_capture_conditions(
    catchmentACS:::.cli_inform_listcol_iso_filled(),
    classes = "listcol_iso_filled"
  )
  expect_s3_class(cap, "tbl_df")
  expect_named(cap, c("class", "message", "phase", "timestamp", "call"))
  expect_identical(nrow(cap), 1L)
})

test_that("ISSUE001-02 explicit return_value = 'conditions' = default", {
  cap <- cacs_capture_conditions(
    catchmentACS:::.cli_inform_listcol_iso_filled(),
    classes = "listcol_iso_filled",
    return_value = "conditions"
  )
  expect_s3_class(cap, "tbl_df")
  expect_identical(nrow(cap), 1L)
})

# --- New behavior: return_value = "both" -----------------------------------

test_that("ISSUE001-03 return_value = 'both' returns list(result, conditions)", {
  emit_then_return <- function() {
    catchmentACS:::.cli_inform_listcol_iso_filled()
    tibble::tibble(x = 1:3)
  }
  out <- cacs_capture_conditions(
    emit_then_return(),
    classes = "listcol_iso_filled",
    return_value = "both"
  )
  expect_type(out, "list")
  expect_named(out, c("result", "conditions"))
  expect_s3_class(out$result, "tbl_df")
  expect_identical(nrow(out$result), 3L)
  expect_identical(nrow(out$conditions), 1L)
})

test_that("ISSUE001-04 'both' with simple expression: result preserved", {
  out <- cacs_capture_conditions(
    1 + 1,
    return_value = "both"
  )
  expect_identical(out$result, 2)
  expect_identical(nrow(out$conditions), 0L)
})

test_that("ISSUE001-05 'both' with NULL return: result is NULL", {
  out <- cacs_capture_conditions(
    invisible(NULL),
    return_value = "both"
  )
  expect_null(out$result)
})

# --- Option-driven default -------------------------------------------------

test_that("ISSUE001-06 catchmentACS.capture_return_value = 'both' changes default", {
  withr::local_options(catchmentACS.capture_return_value = "both")
  out <- cacs_capture_conditions(1 + 1)
  expect_type(out, "list")
  expect_named(out, c("result", "conditions"))
  expect_identical(out$result, 2)
})

# --- Validation ------------------------------------------------------------

test_that("ISSUE001-07 invalid return_value aborts via match.arg", {
  expect_error(
    cacs_capture_conditions(1 + 1, return_value = "invalid"),
    "should be one of"
  )
})

# --- Issue 001 reproducer regression --------------------------------------

test_that("ISSUE001-08 issue 001 reproducer: z <- 42 captured via $result", {
  # Exact pattern from issue 001 §Minimal reproducer + §Where it bit us.
  out <- cacs_capture_conditions(
    z <- 42,
    return_value = "both"
  )
  expect_identical(out$result, 42)
  expect_false(exists("z", envir = environment(), inherits = FALSE))
})

test_that("ISSUE001-09 v0.3 062 workflow Section 6 footgun is healed", {
  # The exact pattern that bit the v0.3 workflow tutorial.
  acs_out <- cacs_capture_conditions(
    {
      catchmentACS:::.cli_inform_listcol_iso_filled()
      list(state = "AL", year = 2023)  # surrogate for cacs_acs_prefetch output
    },
    return_value = "both"
  )
  # The user assigns externally: `acs <- acs_out$result` — this is the
  # whole point. The result is usable, conditions are captured.
  acs <- acs_out$result
  expect_identical(acs$state, "AL")
  expect_identical(acs$year, 2023)
  expect_identical(nrow(acs_out$conditions), 1L)
})

# --- v0.3 backward-compat exhaustive regression ----------------------------

test_that("ISSUE001-BC-01 v0.3 1-arg pattern unchanged", {
  cap <- cacs_capture_conditions(1 + 1)
  expect_s3_class(cap, "tbl_df")
  expect_identical(nrow(cap), 0L)
})

test_that("ISSUE001-BC-02 v0.3 2-arg classes = NULL pattern unchanged", {
  cap <- cacs_capture_conditions(1 + 1, classes = NULL)
  expect_identical(nrow(cap), 0L)
})

test_that("ISSUE001-BC-03 v0.3 expr-with-message pattern unchanged", {
  cap <- cacs_capture_conditions(
    catchmentACS:::.cli_inform_listcol_iso_filled()
  )
  expect_identical(nrow(cap), 1L)
})

test_that("ISSUE001-BC-04 v0.3 short-suffix classes filter unchanged", {
  cap <- cacs_capture_conditions(
    catchmentACS:::.cli_inform_listcol_iso_filled(),
    classes = "listcol_iso_filled"
  )
  expect_identical(nrow(cap), 1L)
})

test_that("ISSUE001-BC-05 v0.3 multi-classes filter unchanged", {
  cap <- cacs_capture_conditions(
    catchmentACS:::.cli_inform_listcol_iso_filled(),
    classes = c("listcol_iso_filled", "rate_first_changed")
  )
  expect_identical(nrow(cap), 1L)
})
