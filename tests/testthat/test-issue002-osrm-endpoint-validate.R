# ===========================================================================
# test-issue002-osrm-endpoint-validate.R
#
# v0.4 plan Step 4.2 — public surface tests for cacs_validate_osrm_endpoint().
# Offline tests use a known-unreachable URL (port 9 = discard) to exercise
# the connection-failure branch. Live tests are env-gated.
# ===========================================================================

# --- Offline behavior tests ------------------------------------------------

test_that("STEP42-01 function exists + is exported", {
  expect_true(exists("cacs_validate_osrm_endpoint",
                     envir = asNamespace("catchmentACS")))
  expect_true("cacs_validate_osrm_endpoint" %in%
              ls("package:catchmentACS"))
})

test_that("STEP42-02 unreachable URL returns 1-row tibble with quota_ok=FALSE", {
  out <- cacs_validate_osrm_endpoint(server = "http://localhost:9",
                                     timeout = 1)
  expect_s3_class(out, "tbl_df")
  expect_identical(nrow(out), 1L)
  expect_named(out, c("endpoint", "quota_ok", "response_ms", "http_status"))
  expect_false(out$quota_ok)
  expect_identical(out$endpoint, "http://localhost:9")
})

test_that("STEP42-03 unreachable URL: http_status is NA + response_ms numeric", {
  out <- cacs_validate_osrm_endpoint(server = "http://localhost:9",
                                     timeout = 1)
  expect_true(is.na(out$http_status))
  expect_type(out$response_ms, "double")
  expect_true(out$response_ms >= 0)
})

test_that("STEP42-04 invalid timeout aborts", {
  expect_error(
    cacs_validate_osrm_endpoint(server = "http://localhost:9", timeout = -1),
    "positive numeric scalar"
  )
  expect_error(
    cacs_validate_osrm_endpoint(server = "http://localhost:9", timeout = NA),
    "positive numeric scalar"
  )
})

test_that("STEP42-05 default server falls back to options/demo", {
  withr::local_options(osrm.server = "http://localhost:9")
  out <- cacs_validate_osrm_endpoint(timeout = 1)
  expect_identical(out$endpoint, "http://localhost:9")
})

test_that("STEP42-05b public demo probe URL uses routed-car prefix", {
  url <- catchmentACS:::.cacs_osrm_probe_url(
    server = "https://routing.openstreetmap.de/"
  )
  expect_identical(
    url,
    paste0(
      "https://routing.openstreetmap.de/routed-car/",
      "route/v1/driving/-86.8,33.5;-86.7,33.4"
    )
  )
})

test_that("STEP42-05c self-hosted probe URL keeps standard OSRM route path", {
  url <- catchmentACS:::.cacs_osrm_probe_url(
    server = "http://localhost:5000/"
  )
  expect_identical(
    url,
    "http://localhost:5000/route/v1/driving/-86.8,33.5;-86.7,33.4"
  )
})

test_that("STEP42-05d public demo probe URL respects routed profile prefixes", {
  bike_url <- catchmentACS:::.cacs_osrm_probe_url(
    server = "https://routing.openstreetmap.de/",
    profile = "bike"
  )
  foot_url <- catchmentACS:::.cacs_osrm_probe_url(
    server = "https://routing.openstreetmap.de/",
    profile = "foot"
  )
  already_prefixed <- catchmentACS:::.cacs_osrm_probe_url(
    server = "https://routing.openstreetmap.de/routed-car/"
  )

  expect_identical(
    bike_url,
    "https://routing.openstreetmap.de/routed-bike/route/v1/driving/-86.8,33.5;-86.7,33.4"
  )
  expect_identical(
    foot_url,
    "https://routing.openstreetmap.de/routed-foot/route/v1/driving/-86.8,33.5;-86.7,33.4"
  )
  expect_identical(
    already_prefixed,
    "https://routing.openstreetmap.de/routed-car/route/v1/driving/-86.8,33.5;-86.7,33.4"
  )
})

# --- New warning class registered ------------------------------------------

test_that("STEP42-06 catchmentACS_warning_provider_quota_exhausted is known", {
  kc <- catchmentACS:::.cacs_known_classes(include_parents = FALSE)
  expect_true("catchmentACS_warning_provider_quota_exhausted" %in% kc)
})

test_that("STEP42-07 emit helper produces correct class chain", {
  cap <- cacs_capture_conditions(
    catchmentACS:::.cli_warn_provider_quota_exhausted("test message"),
    classes = "provider_quota_exhausted"
  )
  expect_identical(nrow(cap), 1L)
  expect_identical(cap$class[[1L]],
                   "catchmentACS_warning_provider_quota_exhausted")
})

# --- Env-gated live test ---------------------------------------------------

test_that("STEP42-08 [LIVE] real OSRM demo endpoint", {
  skip_if_not(
    identical(Sys.getenv("CATCHMENTACS_LIVE_OSRM"), "1"),
    "Set CATCHMENTACS_LIVE_OSRM=1 to enable live OSRM endpoint test"
  )
  out <- cacs_validate_osrm_endpoint()  # uses .OSRM_PUBLIC_DEMO_SERVER default
  expect_s3_class(out, "tbl_df")
  expect_identical(nrow(out), 1L)
  # quota_ok may be TRUE or FALSE depending on current quota state;
  # we only assert the function returns valid shape.
  expect_type(out$quota_ok, "logical")
  expect_false(is.na(out$quota_ok))
})
