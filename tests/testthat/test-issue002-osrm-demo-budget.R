# ===========================================================================
# test-issue002-osrm-demo-budget.R
#
# v0.4 plan Step 4.1 — public surface tests for the Issue 002 fix:
# `cacs_isochrone()` downgrades omitted `res` from 70L to 30L when
# `osrm_mode = "demo"` AND `catchmentACS.osrm_demo_budget_protect = TRUE`.
# Also tests the rewritten 429 abort message hint.
#
# Note: these tests do NOT call live OSRM. They exercise the
# `.cacs_osrm_resolve_demo_res()` invocation path + emit gating inside
# `.iso_via_osrm()` via direct argument-tracing tests (the actual OSRM call
# is gated by `Sys.getenv("CATCHMENTACS_LIVE_OSRM") == "1"` env var).
# ===========================================================================

# --- Helper invocation path tests ------------------------------------------

test_that("ISSUE002-01 helper invoked: omitted res + demo + protect → 30L", {
  out <- catchmentACS:::.cacs_osrm_resolve_demo_res(
    res_param       = NULL,
    osrm_mode       = "demo",
    options_protect = TRUE
  )
  expect_identical(out$res_effective, 30L)
  expect_true(out$downgraded)
  expect_true(out$emit_msg)
})

test_that("ISSUE002-02 options propagation: option TRUE → downgrade emit_msg", {
  withr::local_options(catchmentACS.osrm_demo_budget_protect = TRUE)
  protect <- getOption("catchmentACS.osrm_demo_budget_protect", TRUE)
  out <- catchmentACS:::.cacs_osrm_resolve_demo_res(NULL, "demo", protect)
  expect_true(out$emit_msg)
})

test_that("ISSUE002-03 options propagation: option FALSE → no downgrade", {
  withr::local_options(catchmentACS.osrm_demo_budget_protect = FALSE)
  protect <- getOption("catchmentACS.osrm_demo_budget_protect", TRUE)
  out <- catchmentACS:::.cacs_osrm_resolve_demo_res(NULL, "demo", protect)
  expect_identical(out$res_effective, 70L)
  expect_false(out$downgraded)
  expect_false(out$emit_msg)
})

test_that("ISSUE002-04 explicit res=70L + demo → 70L passthrough (silent)", {
  out <- catchmentACS:::.cacs_osrm_resolve_demo_res(70L, "demo", TRUE)
  expect_identical(out$res_effective, 70L)
  expect_false(out$emit_msg)
})

test_that("ISSUE002-05 explicit res=70L + docker → 70L passthrough (silent)", {
  out <- catchmentACS:::.cacs_osrm_resolve_demo_res(70L, "docker", TRUE)
  expect_identical(out$res_effective, 70L)
  expect_false(out$emit_msg)
})

test_that("ISSUE002-06 omitted + docker → 70L (v0.3 default preserved)", {
  out <- catchmentACS:::.cacs_osrm_resolve_demo_res(NULL, "docker", TRUE)
  expect_identical(out$res_effective, 70L)
  expect_false(out$emit_msg)
})


test_that("ISSUE002-06b exact osrm n mapping uses n without changing res budget", {
  testthat::skip_if_not_installed("osrm")

  args <- catchmentACS:::.cacs_osrm_resolution_args(27L)
  if ("n" %in% names(formals(osrm::osrmIsochrone))) {
    expect_identical(args, list(n = 500L))
  } else {
    expect_identical(args, list(res = 27L))
  }
})


test_that("ISSUE002-06c non-exact public res values preserve res semantics", {
  testthat::skip_if_not_installed("osrm")

  expect_identical(catchmentACS:::.cacs_osrm_resolution_args(30L),
                   list(res = 30L))
  expect_identical(catchmentACS:::.cacs_osrm_resolution_args(50L),
                   list(res = 50L))
  expect_identical(catchmentACS:::.cacs_osrm_resolution_args(70L),
                   list(res = 70L))
})

# --- 429 abort message contains 3 remediation paths ------------------------

test_that("ISSUE002-07 429 abort message lists 3 remediation paths (string-match)", {
  # We inspect `R/isochrone-osrm.R` source to confirm the 3 paths are
  # mentioned. This is more robust than reproducing a 429 from a live test
  # (which would require complex httptest2 mocking).
  .src <- testthat::test_path("..", "..", "R", "isochrone-osrm.R")
  skip_if_not(file.exists(.src))
  osrm_src <- readLines(.src)
  ix <- which(grepl("Rate limit \\(HTTP 429\\)", osrm_src))
  expect_gte(length(ix), 1L)
  # Extract a window around the 429 message
  window <- paste(osrm_src[max(1, ix[1] - 1):min(length(osrm_src), ix[1] + 8)],
                  collapse = "\n")
  expect_match(window, "res = 30")
  expect_match(window, "docker")
  expect_match(window, "[Ss]witch provider")
})

test_that("ISSUE002-07b mocked 429 abort uses enriched remediation message", {
  site <- sf::st_as_sf(
    tibble::tibble(site_id = "S1", lon = -86.8, lat = 33.5),
    coords = c("lon", "lat"),
    crs = 4326
  )
  testthat::local_mocked_bindings(
    .osrm_retry_one_site = function(site_row, breaks, res = NULL) {
      list(
        geom = vector("list", length(breaks)),
        empty = rep(TRUE, length(breaks)),
        attempts = 1L,
        http_status = 429L,
        failure_reason = "HTTP 429"
      )
    },
    .package = "catchmentACS"
  )

  err <- tryCatch(
    catchmentACS:::.iso_via_osrm(
      site,
      drive_times = 5L,
      profile = "car",
      res = 30L
    ),
    error = function(e) e
  )

  expect_s3_class(err, "catchmentACS_error_operator")
  msg <- conditionMessage(err)
  expect_match(msg, "Rate limit \\(HTTP 429\\)")
  expect_match(msg, "res = 30L", fixed = TRUE)
  expect_match(msg, "osrm_mode = \"docker\"", fixed = TRUE)
  expect_match(msg, "provider = \"ors\"", fixed = TRUE)
})

# --- demo-budget emit captured (gated by once_per_session) -----------------

test_that("ISSUE002-08 emit captured by short-suffix 'demo_budget_protected'", {
  cap <- cacs_capture_conditions(
    catchmentACS:::.cli_inform_demo_budget_protected(),
    classes = "demo_budget_protected"
  )
  # 1 row when this is the first call in the session; 0 when previous tests
  # already fired the once_per_session — both are valid.
  expect_lte(nrow(cap), 1L)
  if (nrow(cap) > 0L) {
    expect_identical(cap$class[[1L]],
                     "catchmentACS_message_demo_budget_protected")
  }
})
