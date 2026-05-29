test_that("P6-PERF-EMIT-01 perf-fix condition class chain is registered", {
  classes <- .cacs_cond_classes("message", "perf_fix_applied")
  expect_identical(classes[[1L]], "catchmentACS_message_perf_fix_applied")
  expect_true("catchmentACS_message" %in% classes)
  expect_true("catchmentACS_condition" %in% classes)
})

test_that("P6-PERF-EMIT-02 docker cacs_isochrone emits subscribable perf-fix condition", {
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      .p6_osrm_response(breaks, center_lon = loc[[1]], center_lat = loc[[2]])
    },
    .package = "catchmentACS"
  )

  with_test_cache({
    conds <- cacs_capture_conditions(
      cacs_isochrone(
        sites = .p6_sites_tbl(2L),
        drive_times = 10L,
        provider = "osrm",
        osrm_mode = "docker",
        cache_dir = cacs_cache_dir(),
        verbose = TRUE
      ),
      classes = "perf_fix_applied"
    )
    expect_equal(nrow(conds), 1L)
    expect_equal(conds$class, "catchmentACS_message_perf_fix_applied")
    expect_equal(conds$phase, "isochrone")
  })
})

test_that("P6-PERF-EMIT-03 public demo mode does not emit fast-path condition", {
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      .p6_osrm_response(breaks, center_lon = loc[[1]], center_lat = loc[[2]])
    },
    .package = "catchmentACS"
  )

  with_test_cache({
    conds <- cacs_capture_conditions(
      cacs_isochrone(
        sites = .p6_sites_tbl(1L),
        drive_times = 10L,
        provider = "osrm",
        cache_dir = cacs_cache_dir(),
        verbose = TRUE
      ),
      classes = "perf_fix_applied"
    )
    expect_equal(nrow(conds), 0L)
  })
})

test_that("P6-PERF-EMIT-04 perf-fix condition fires once per invocation", {
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      .p6_osrm_response(breaks, center_lon = loc[[1]], center_lat = loc[[2]])
    },
    .package = "catchmentACS"
  )

  with_test_cache({
    conds <- cacs_capture_conditions(
      cacs_isochrone(
        sites = .p6_sites_tbl(3L),
        drive_times = c(5L, 10L),
        provider = "osrm",
        osrm_mode = "docker",
        cache_dir = cacs_cache_dir(),
        verbose = TRUE
      ),
      classes = "perf_fix_applied"
    )
    expect_equal(nrow(conds), 1L)
  })
})
