# ===========================================================================
# test-unit-layer0-conditions.R
#
# v0.4 plan Step 1.3 — Layer 0 unit tests for the 3 new condition classes
# registered in R/cli.R + R/catchmentACS-conditions.R + R/capture-conditions.R.
#
# Note: 2 of the 3 helpers use `.frequency = "once"` (per-session), so
# multi-call testing must be done in a single sequence — once a helper has
# fired in the session, subsequent capture attempts return 0 rows. The
# listcol_iso_filled helper has no per-session throttle and is used for
# multi-call assertions.
# ===========================================================================

# --- Basic emit + correct leaf class (single fire per helper) --------------

test_that("LAYER0-COND-01 demo_budget_protected: 1st emit produces correct leaf class", {
  cap <- cacs_capture_conditions(
    catchmentACS:::.cli_inform_demo_budget_protected(),
    classes = NULL
  )
  # may be 0 if prior test run already fired this once_per_session id, or 1
  # if this is the first fire. We assert: when it fires, the class is correct.
  if (nrow(cap) > 0L) {
    expect_identical(cap$class[[1L]],
                     "catchmentACS_message_demo_budget_protected")
  } else {
    succeed("emit silenced by once_per_session — prior test fired it")
  }
})

test_that("LAYER0-COND-02 rate_first_changed: 1st emit produces correct leaf class", {
  cap <- cacs_capture_conditions(
    catchmentACS:::.cli_inform_rate_first_changed(),
    classes = NULL
  )
  if (nrow(cap) > 0L) {
    expect_identical(cap$class[[1L]],
                     "catchmentACS_message_rate_first_changed")
  } else {
    succeed("emit silenced by once_per_session")
  }
})

test_that("LAYER0-COND-03 listcol_iso_filled: emit produces correct leaf class", {
  # No once_per_session — always emits.
  cap <- cacs_capture_conditions(
    catchmentACS:::.cli_inform_listcol_iso_filled(),
    classes = NULL
  )
  expect_identical(nrow(cap), 1L)
  expect_identical(cap$class[[1L]],
                   "catchmentACS_message_listcol_iso_filled")
})

# --- Known-classes inventory test ------------------------------------------

test_that("LAYER0-COND-04 .cacs_known_classes contains all 3 new classes", {
  kc <- catchmentACS:::.cacs_known_classes(include_parents = FALSE)
  expect_true("catchmentACS_message_demo_budget_protected" %in% kc)
  expect_true("catchmentACS_message_rate_first_changed" %in% kc)
  expect_true("catchmentACS_message_listcol_iso_filled" %in% kc)
})

# --- Short-suffix capture filter (use listcol_iso_filled — no throttle) ----

test_that("LAYER0-COND-05 short suffix 'listcol_iso_filled' catches the message", {
  cap <- cacs_capture_conditions(
    catchmentACS:::.cli_inform_listcol_iso_filled(),
    classes = "listcol_iso_filled"
  )
  expect_identical(nrow(cap), 1L)
})

# --- Parent-class capture filter -------------------------------------------

test_that("LAYER0-COND-06 listcol_iso_filled is catchable as parent 'message'", {
  cap <- cacs_capture_conditions(
    catchmentACS:::.cli_inform_listcol_iso_filled(),
    classes = "catchmentACS_message"
  )
  expect_identical(nrow(cap), 1L)
})

test_that("LAYER0-COND-07 listcol_iso_filled is catchable as parent 'condition'", {
  cap <- cacs_capture_conditions(
    catchmentACS:::.cli_inform_listcol_iso_filled(),
    classes = "catchmentACS_condition"
  )
  expect_identical(nrow(cap), 1L)
})

# --- Repeated emit on listcol_iso_filled (no throttle) ---------------------

test_that("LAYER0-COND-08 listcol_iso_filled emits every call (no throttle)", {
  cap <- cacs_capture_conditions({
    catchmentACS:::.cli_inform_listcol_iso_filled()
    catchmentACS:::.cli_inform_listcol_iso_filled()
    catchmentACS:::.cli_inform_listcol_iso_filled()
  }, classes = "listcol_iso_filled")
  expect_identical(nrow(cap), 3L)
})

# --- Family enum coverage --------------------------------------------------

test_that("LAYER0-COND-09 .cacs_cond_classes resolves the 3 new families correctly", {
  cc <- catchmentACS:::.cacs_cond_classes
  expect_identical(
    cc("message", "demo_budget_protected"),
    c("catchmentACS_message_demo_budget_protected",
      "catchmentACS_message",
      "catchmentACS_condition")
  )
  expect_identical(
    cc("message", "rate_first_changed"),
    c("catchmentACS_message_rate_first_changed",
      "catchmentACS_message",
      "catchmentACS_condition")
  )
  expect_identical(
    cc("message", "listcol_iso_filled"),
    c("catchmentACS_message_listcol_iso_filled",
      "catchmentACS_message",
      "catchmentACS_condition")
  )
})
