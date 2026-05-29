# ===========================================================================
# test-unit-osrm-resolve-demo-res.R
#
# v0.4 plan Step 2.3 — unit tests for `.cacs_osrm_resolve_demo_res()`
# (Layer 1 helper for Issue 002). Pure-function 4×2 decision table.
# ===========================================================================

.helper_call <- function(res_param, osrm_mode, options_protect) {
  catchmentACS:::.cacs_osrm_resolve_demo_res(
    res_param       = res_param,
    osrm_mode       = osrm_mode,
    options_protect = options_protect
  )
}

# --- Decision-table tests --------------------------------------------------

test_that("STEP23-01 omitted + demo + protect → 30L + downgraded + emit", {
  out <- .helper_call(NULL, "demo", TRUE)
  expect_identical(out$res_effective, 30L)
  expect_true(out$downgraded)
  expect_true(out$emit_msg)
  expect_type(out$msg_text, "character")
})

test_that("STEP23-02 omitted + demo + !protect → 70L + not downgraded + no emit", {
  out <- .helper_call(NULL, "demo", FALSE)
  expect_identical(out$res_effective, 70L)
  expect_false(out$downgraded)
  expect_false(out$emit_msg)
  expect_null(out$msg_text)
})

test_that("STEP23-03 omitted + docker + protect → 70L (v0.3 default preserved)", {
  out <- .helper_call(NULL, "docker", TRUE)
  expect_identical(out$res_effective, 70L)
  expect_false(out$downgraded)
  expect_false(out$emit_msg)
})

test_that("STEP23-04 omitted + docker + !protect → 70L (same as 23-03)", {
  out <- .helper_call(NULL, "docker", FALSE)
  expect_identical(out$res_effective, 70L)
  expect_false(out$downgraded)
})

test_that("STEP23-05 explicit res=70L + demo + protect → 70L passthrough, no emit", {
  out <- .helper_call(70L, "demo", TRUE)
  expect_identical(out$res_effective, 70L)
  expect_false(out$downgraded)
  expect_false(out$emit_msg)
})

test_that("STEP23-06 explicit res=50L + docker + protect → 50L passthrough", {
  out <- .helper_call(50L, "docker", TRUE)
  expect_identical(out$res_effective, 50L)
  expect_false(out$downgraded)
})

test_that("STEP23-07 explicit res=30L + demo + protect → 30L passthrough (no emit)", {
  # 30L 명시 사용자는 ALREADY demo-aware — emit 부재.
  out <- .helper_call(30L, "demo", TRUE)
  expect_identical(out$res_effective, 30L)
  expect_false(out$downgraded)
  expect_false(out$emit_msg)
})

test_that("STEP23-08 numeric res coerces to integer", {
  out <- .helper_call(50, "docker", TRUE)
  expect_identical(out$res_effective, 50L)
  expect_type(out$res_effective, "integer")
})

test_that("STEP23-09 invalid options_protect aborts", {
  expect_error(.helper_call(NULL, "demo", NA),
               "non-NA logical scalar")
  expect_error(.helper_call(NULL, "demo", "TRUE"),
               "non-NA logical scalar")
})
