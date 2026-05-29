test_that("P6-LOCK-ISO-01 mocked docker path stays below 15 seconds per site", {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      calls$n <- calls$n + 1L
      .p6_osrm_response(breaks, center_lon = loc[[1]], center_lat = loc[[2]])
    },
    .package = "catchmentACS"
  )

  with_test_cache({
    elapsed <- unname(system.time(
      out <- suppressMessages(cacs_isochrone(
        sites = .p6_sites_tbl(3L),
        drive_times = c(5L, 10L),
        provider = "osrm",
        osrm_mode = "docker",
        cache_dir = cacs_cache_dir(),
        verbose = FALSE
      ))
    )[["elapsed"]])
    expect_lt(elapsed, 15 * 3 + 2)
    expect_equal(calls$n, 3L)
    expect_false(any(out$isochrone_empty))
  })
})

test_that("P6-LOCK-ISO-02 explicit res=70 remains inside mocked 15s/site lock", {
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      expect_equal(res, 70L)
      .p6_osrm_response(breaks, center_lon = loc[[1]], center_lat = loc[[2]])
    },
    .package = "catchmentACS"
  )

  with_test_cache({
    elapsed <- unname(system.time(
      out <- suppressMessages(cacs_isochrone(
        sites = .p6_sites_tbl(2L),
        drive_times = 10L,
        provider = "osrm",
        osrm_mode = "docker",
        res = 70L,
        cache_dir = cacs_cache_dir(),
        verbose = FALSE
      ))
    )[["elapsed"]])
    expect_lt(elapsed, 15 * 2 + 2)
    expect_equal(attr(out, "cacs_isochrone_provenance")$res_param, 70L)
  })
})

test_that("P6-LOCK-ISO-03 fast-path provenance carries zero forced sleep", {
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      .p6_osrm_response(breaks, center_lon = loc[[1]], center_lat = loc[[2]])
    },
    .package = "catchmentACS"
  )

  with_test_cache({
    out <- suppressMessages(cacs_isochrone(
      sites = .p6_sites_tbl(1L),
      drive_times = 10L,
      provider = "osrm",
      osrm_mode = "docker",
      cache_dir = cacs_cache_dir(),
      verbose = FALSE
    ))
    budget <- attr(out, "cacs_isochrone_provenance")$osrm_request_budget
    expect_false(budget$public_demo)
    expect_equal(budget$sleep_floor_sec_per_site, 0L)
    expect_equal(attr(out, "cacs_isochrone_provenance")$osrm_server,
                 .OSRM_DOCKER_SERVER_DEFAULT)
  })
})

test_that("P6-LOCK-ISO-04 public-demo path is documented as outside 15s/site", {
  budget <- .osrm_request_budget(
    res = 70L,
    breaks = c(5L, 10L),
    osrm_server = .OSRM_PUBLIC_DEMO_SERVER
  )
  expect_true(budget$public_demo)
  expect_gt(budget$sleep_floor_sec_per_site, 15L)
  expect_equal(budget$table_calls, 66L)
})

test_that("P6-LOCK-ISO-05 retry sleep budget is bounded below 15s/site", {
  sleep_calls <- numeric(0L)
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      stop("OSRM server returned HTTP 503: Service Unavailable")
    },
    .package = "catchmentACS"
  )
  testthat::local_mocked_bindings(
    Sys.sleep = function(time) {
      sleep_calls <<- c(sleep_calls, time)
      invisible(NULL)
    },
    .package = "base"
  )

  out <- suppressWarnings(.iso_via_osrm(
    sites = .p6_sites_sf(1L),
    drive_times = 10L,
    profile = "car",
    osrm_mode = "demo"
  ))
  expect_equal(sleep_calls, c(1, 2))
  expect_lt(sum(sleep_calls), 15)
  expect_equal(out$retry_count, 3L)
})

test_that("P6-LOCK-ISO-06 cache-hit path is below 15s/site and has no seam call", {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      calls$n <- calls$n + 1L
      .p6_osrm_response(breaks, center_lon = loc[[1]], center_lat = loc[[2]])
    },
    .package = "catchmentACS"
  )

  with_test_cache({
    args <- list(
      sites = .p6_sites_tbl(1L),
      drive_times = 10L,
      provider = "osrm",
      osrm_mode = "docker",
      cache_dir = cacs_cache_dir(),
      verbose = FALSE
    )
    suppressMessages(do.call(cacs_isochrone, args))
    before <- calls$n
    elapsed <- unname(system.time(suppressMessages(do.call(cacs_isochrone, args)))[["elapsed"]])
    expect_equal(calls$n, before)
    expect_lt(elapsed, 15)
    expect_equal(.p6_counter_row("isochrone")$hit, 1L)
  })
})
