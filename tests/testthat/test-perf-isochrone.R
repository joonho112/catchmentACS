# ============================================================================
# Phase 6 Step 6.1 performance-root tests for OSRM isochrone dispatch.
# ============================================================================


.mk_perf_site <- function(n = 1L) {
  tibble::tibble(
    site_id = paste0("S", seq_len(n)),
    lon = -86.80 + seq_len(n) * 0.001,
    lat = 33.50 + seq_len(n) * 0.001
  ) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
}

.mk_perf_osrm_response <- function(breaks, center_lon = -86.80,
                                   center_lat = 33.50) {
  polys <- lapply(seq_along(breaks), function(j) {
    b <- breaks[[j]]
    half <- 0.001 * b
    sf::st_polygon(list(rbind(
      c(center_lon - half, center_lat - half),
      c(center_lon + half, center_lat - half),
      c(center_lon + half, center_lat + half),
      c(center_lon - half, center_lat + half),
      c(center_lon - half, center_lat - half)
    )))
  })
  sf::st_sf(
    tibble::tibble(
      id = seq_along(breaks),
      isomin = c(0L, utils::head(as.integer(breaks), -1L)),
      isomax = as.integer(breaks)
    ),
    geometry = sf::st_sfc(polys, crs = 4326)
  )
}


test_that("P6-PERF-ISO-01 OSRM public-demo budget exposes hidden sleep floor", {
  budget <- .osrm_request_budget(
    res = 70L,
    breaks = 10L,
    osrm_server = .OSRM_PUBLIC_DEMO_SERVER
  )

  expect_true(budget$public_demo)
  expect_equal(budget$grid_points, 4900L)
  expect_equal(budget$chunk_size, 75L)
  expect_equal(budget$table_calls, 66L)
  expect_equal(budget$sleep_floor_sec_per_site, 65L)
})


test_that("P6-PERF-ISO-02 docker mode forwards explicit fast endpoint and profile", {
  td <- tempfile("cacs_p6_perf_iso_")
  on.exit(if (dir.exists(td)) unlink(td, recursive = TRUE), add = TRUE)

  calls <- list()
  stub <- function(loc, breaks, res = NULL) {
    calls[[length(calls) + 1L]] <<- list(
      loc = loc,
      breaks = breaks,
      res = res,
      server = getOption("osrm.server"),
      profile = getOption("osrm.profile")
    )
    .mk_perf_osrm_response(breaks, center_lon = loc[[1]], center_lat = loc[[2]])
  }

  testthat::local_mocked_bindings(
    .osrm_call_isochrone = stub,
    .package = "catchmentACS"
  )

  conds <- cacs_capture_conditions(
    cacs_isochrone(
      sites = .mk_perf_site(3L),
      drive_times = c(5L, 10L),
      provider = "osrm",
      osrm_mode = "docker",
      cache_dir = td,
      verbose = TRUE
    ),
    classes = "perf_fix_applied"
  )

  expect_equal(nrow(conds), 1L)
  expect_equal(conds$class, "catchmentACS_message_perf_fix_applied")
  expect_equal(length(calls), 3L)
  expect_true(all(vapply(calls, function(x) identical(x$breaks, c(5L, 10L)),
                         logical(1))))
  expect_true(all(vapply(calls, function(x) identical(x$res, .OSRM_RES_DEFAULT),
                         logical(1))))
  expect_true(all(vapply(calls, function(x) identical(x$server, .OSRM_DOCKER_SERVER_DEFAULT),
                         logical(1))))
  expect_true(all(vapply(calls, function(x) identical(x$profile, "car"),
                         logical(1))))
})


test_that("P6-PERF-ISO-03 server and profile participate in isochrone cache key", {
  site <- .mk_perf_site(1L)
  make_key <- function(server, profile = "car") {
    td <- tempfile("cacs_p6_key_")
    on.exit(if (dir.exists(td)) unlink(td, recursive = TRUE), add = TRUE)
    testthat::local_mocked_bindings(
      .osrm_call_isochrone = function(loc, breaks, res = NULL) {
        .mk_perf_osrm_response(breaks)
      },
      .package = "catchmentACS"
    )
    out <- suppressMessages(cacs_isochrone(
      sites = site,
      drive_times = 10L,
      provider = "osrm",
      cache_dir = td,
      verbose = FALSE,
      profile = profile,
      osrm.server = server
    ))
    attr(out, "cacs_isochrone_provenance")$cache_key
  }

  key_a <- make_key("http://127.0.0.1:5000/")
  key_b <- make_key("http://127.0.0.1:5100/")
  key_c <- make_key("http://127.0.0.1:5000/", profile = "bike")

  expect_false(identical(key_a, key_b))
  expect_false(identical(key_a, key_c))
})


test_that("P6-PERF-ISO-04 exhausted transient failures sleep only before retry attempts", {
  site <- .mk_perf_site(1L)
  call_n <- 0L
  sleep_calls <- numeric(0L)

  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      call_n <<- call_n + 1L
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
    sites = site,
    drive_times = 10L,
    profile = "car",
    osrm_mode = "demo"
  ))

  expect_equal(call_n, 3L)
  expect_equal(sleep_calls, c(1, 2))
  expect_equal(out$retry_count, 3L)
  expect_false(is.na(out$failure_reason))
})


test_that("P6-PERF-ISO-05 omitted res=70 is forwarded to OSRM once per site", {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  calls$res <- integer()
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      calls$n <- calls$n + 1L
      calls$res <- c(calls$res, res)
      .p6_osrm_response(breaks, center_lon = loc[[1]], center_lat = loc[[2]])
    },
    .package = "catchmentACS"
  )

  with_test_cache({
    out <- suppressMessages(cacs_isochrone(
      sites = .p6_sites_tbl(3L),
      drive_times = 10L,
      provider = "osrm",
      osrm_mode = "docker",
      cache_dir = cacs_cache_dir(),
      verbose = FALSE
    ))
    expect_equal(calls$n, 3L)
    expect_true(all(calls$res == .OSRM_RES_DEFAULT))
    expect_false(any(out$isochrone_empty))
  })
})


test_that("P6-PERF-ISO-06 warm isochrone cache avoids OSRM seam and reports counters", {
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
    suppressMessages(do.call(cacs_isochrone, args))
    row <- .p6_counter_row("isochrone")
    expect_equal(calls$n, 1L)
    expect_equal(row$hit, 1L)
    expect_equal(row$miss, 1L)
    expect_equal(row$hit_rate, 0.5)
  })
})


test_that("P6-PERF-ISO-07 explicit res separates isochrone cache identity", {
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      .p6_osrm_response(breaks, center_lon = loc[[1]], center_lat = loc[[2]])
    },
    .package = "catchmentACS"
  )

  with_test_cache({
    key_for_res <- function(res) {
      out <- suppressMessages(cacs_isochrone(
        sites = .p6_sites_tbl(1L),
        drive_times = 10L,
        provider = "osrm",
        osrm_mode = "docker",
        res = res,
        cache_dir = cacs_cache_dir(),
        verbose = FALSE
      ))
      attr(out, "cacs_isochrone_provenance")$cache_key
    }
    expect_false(identical(key_for_res(50L), key_for_res(70L)))
  })
})


test_that("P6-PERF-ISO-08 docker request budget is fast-path eligible", {
  budget <- .osrm_request_budget(
    res = .OSRM_RES_DEFAULT,
    breaks = c(5L, 10L, 15L),
    osrm_server = .OSRM_DOCKER_SERVER_DEFAULT
  )
  expect_false(budget$public_demo)
  expect_equal(budget$sleep_floor_sec_per_site, 0L)
  expect_lte(budget$table_calls, 11L)
  expect_lt(budget$table_calls, budget$public_demo_table_calls_per_site)
})


test_that("P6-PERF-ISO-09 public demo res=70 is classified over 15s/site", {
  budget <- .osrm_request_budget(
    res = .OSRM_RES_DEFAULT,
    breaks = c(5L, 10L, 15L),
    osrm_server = .OSRM_PUBLIC_DEMO_SERVER
  )
  expect_true(budget$public_demo)
  expect_gt(budget$sleep_floor_sec_per_site, 15L)
  expect_equal(budget$public_demo_sleep_floor_sec_per_site, 65L)
})
