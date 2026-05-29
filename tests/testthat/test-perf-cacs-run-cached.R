test_that("P6-RUN-CACHE-01 cold mocked full cacs_run primes all cache namespaces", {
  with_test_cache({
    acs_calls <- new.env(parent = emptyenv())
    osrm_calls <- new.env(parent = emptyenv())
    acs_calls$n <- 0L
    osrm_calls$n <- 0L
    osrm_calls$res <- integer()
    .p6_install_network_mocks(acs_calls, osrm_calls)

    elapsed <- unname(system.time(
      out <- suppressWarnings(suppressMessages(cacs_run(
        sites = .p6_sites_tbl(3L),
        state = "AL",
        variables = .p6_default_vars(),
        provider = "osrm",
        drive_times = 10L,
        cache_dir = cacs_cache_dir(),
        iso_args = list(osrm_mode = "docker"),
        verbose = FALSE
      )))
    )[["elapsed"]])

    expect_s3_class(out, "tbl_df")
    expect_true(any(out$variable %in% names(cacs_acs_default_rates)))
    expect_equal(acs_calls$n, 1L)
    expect_equal(osrm_calls$n, 3L)
    expect_true(all(osrm_calls$res == .OSRM_RES_DEFAULT))
    expect_lt(elapsed, 10)
    expect_gte(.p6_counter_row("acs")$miss, 1L)
    expect_gte(.p6_counter_row("isochrone")$miss, 1L)
    expect_gte(.p6_counter_row("intersect")$miss, 1L)
  })
})

test_that("P6-RUN-CACHE-02 warm full cacs_run avoids ACS and OSRM seams", {
  with_test_cache({
    acs_calls <- new.env(parent = emptyenv())
    osrm_calls <- new.env(parent = emptyenv())
    acs_calls$n <- 0L
    osrm_calls$n <- 0L
    osrm_calls$res <- integer()
    .p6_install_network_mocks(acs_calls, osrm_calls)

    args <- list(
      sites = .p6_sites_tbl(3L),
      state = "AL",
      variables = .p6_default_vars(),
      provider = "osrm",
      drive_times = 10L,
      cache_dir = cacs_cache_dir(),
      iso_args = list(osrm_mode = "docker"),
      verbose = FALSE
    )
    suppressWarnings(suppressMessages(do.call(cacs_run, args)))
    first_counts <- c(acs = acs_calls$n, osrm = osrm_calls$n)
    elapsed <- unname(system.time(
      out <- suppressWarnings(suppressMessages(do.call(cacs_run, args)))
    )[["elapsed"]])

    expect_s3_class(out, "tbl_df")
    expect_equal(c(acs = acs_calls$n, osrm = osrm_calls$n), first_counts)
    expect_lt(elapsed, 5)
    expect_gte(.p6_counter_row("acs")$hit, 1L)
    expect_gte(.p6_counter_row("isochrone")$hit, 1L)
    expect_gte(.p6_counter_row("intersect")$hit, 1L)
  })
})

test_that("P6-RUN-CACHE-03 disabled cache runs mocked path without writes", {
  with_test_cache({
    acs_calls <- new.env(parent = emptyenv())
    osrm_calls <- new.env(parent = emptyenv())
    acs_calls$n <- 0L
    osrm_calls$n <- 0L
    osrm_calls$res <- integer()
    .p6_install_network_mocks(acs_calls, osrm_calls)
    cacs_set_cache(FALSE)

    out <- suppressWarnings(suppressMessages(cacs_run(
      sites = .p6_sites_tbl(2L),
      state = "AL",
      variables = .p6_default_vars(),
      provider = "osrm",
      drive_times = 10L,
      cache_dir = cacs_cache_dir(),
      iso_args = list(osrm_mode = "docker"),
      verbose = FALSE
    )))

    expect_s3_class(out, "tbl_df")
    expect_equal(acs_calls$n, 1L)
    expect_equal(osrm_calls$n, 2L)
    expect_equal(sum(cacs_cache_status()$n_entries), 0L)
  })
})

test_that("P6-RUN-CACHE-04 precomputed iso and ACS bypass network phases", {
  with_test_cache({
    testthat::local_mocked_bindings(
      cacs_acs_prefetch = function(...) stop("ACS should be bypassed"),
      cacs_isochrone = function(...) stop("isochrone should be bypassed"),
      .package = "catchmentACS"
    )

    out <- suppressWarnings(suppressMessages(cacs_run(
      sites = .p6_sites_tbl(3L),
      state = "AL",
      variables = .p6_default_vars(),
      drive_times = 10L,
      precomputed_isochrones = .p6_iso_sf(drive_times = 10L),
      acs = .p6_acs_sf(),
      cache_dir = cacs_cache_dir(),
      verbose = FALSE
    )))

    expect_s3_class(out, "tbl_df")
    expect_true(any(out$variable %in% names(cacs_acs_default_rates)))
  })
})

test_that("P6-RUN-CACHE-05 output both preserves run metadata in cached path", {
  with_test_cache({
    out <- suppressWarnings(suppressMessages(cacs_run(
      sites = .p6_sites_tbl(2L),
      state = "AL",
      variables = .p6_default_vars(),
      drive_times = 10L,
      precomputed_isochrones = .p6_iso_sf(sites = .p6_sites_tbl(2L), drive_times = 10L),
      acs = .p6_acs_sf(),
      cache_dir = cacs_cache_dir(),
      output = "both",
      verbose = FALSE
    )))

    expect_named(out, c("long", "list_column"))
    expect_s3_class(out$long, "cacs_run_result")
    expect_false(is.null(attr(out$long, "cacs_run_provenance")))
    expect_false(is.null(attr(out$long, "cacs_run_result_metadata")))
  })
})

test_that("P6-RUN-CACHE-06 explicit cache_dir contains intersect entries", {
  with_test_cache({
    out <- suppressWarnings(suppressMessages(cacs_run(
      sites = .p6_sites_tbl(1L),
      state = "AL",
      variables = .p6_default_vars(),
      drive_times = 10L,
      precomputed_isochrones = .p6_iso_sf(sites = .p6_sites_tbl(1L), drive_times = 10L),
      acs = .p6_acs_sf(),
      cache_dir = cacs_cache_dir(),
      verbose = FALSE
    )))
    status <- cacs_cache_status()
    expect_s3_class(out, "tbl_df")
    expect_gte(status$n_entries[status$namespace == "intersect"], 1L)
  })
})

test_that("P6-RUN-CACHE-07 warm cacs_run produces nonzero public hit rates", {
  with_test_cache({
    acs_calls <- new.env(parent = emptyenv())
    osrm_calls <- new.env(parent = emptyenv())
    acs_calls$n <- 0L
    osrm_calls$n <- 0L
    osrm_calls$res <- integer()
    .p6_install_network_mocks(acs_calls, osrm_calls)

    args <- list(
      sites = .p6_sites_tbl(1L),
      state = "AL",
      variables = .p6_default_vars(),
      provider = "osrm",
      drive_times = 10L,
      cache_dir = cacs_cache_dir(),
      iso_args = list(osrm_mode = "docker"),
      verbose = FALSE
    )
    suppressWarnings(suppressMessages(do.call(cacs_run, args)))
    suppressWarnings(suppressMessages(do.call(cacs_run, args)))

    counters <- cacs_get_cache_state()$counters[[1]]
    expect_true(all(counters$hit > 0L))
    expect_true(all(counters$miss > 0L))
    expect_true(all(counters$hit_rate > 0))
  })
})
