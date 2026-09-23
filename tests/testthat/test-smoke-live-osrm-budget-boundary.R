test_that("P6-LIVE-OSRM-01 live OSRM smoke records budget boundary", {
  skip_on_cran()
  skip_if_not(
    identical(Sys.getenv("CACS_LIVE_OSRM"), "1") ||
      identical(Sys.getenv("CATCHMENTACS_LIVE_SMOKE"), "true"),
    "set CACS_LIVE_OSRM=1 or CATCHMENTACS_LIVE_SMOKE=true to run live OSRM smoke"
  )

  with_test_cache({
    elapsed <- unname(system.time(
      out <- suppressMessages(cacs_isochrone(
        sites = .p6_sites_tbl(1L),
        drive_times = 10L,
        provider = "osrm",
        cache_dir = cacs_cache_dir(),
        verbose = FALSE
      ))
    )[["elapsed"]])
    prov <- attr(out, "cacs_isochrone_provenance")
    trace <- c(
      paste0("elapsed_sec=", elapsed),
      paste0("server=", prov$osrm_server),
      paste0("res_param=", prov$res_param),
      paste0("public_demo=", prov$osrm_request_budget$public_demo),
      paste0("sleep_floor_sec_per_site=", prov$osrm_request_budget$sleep_floor_sec_per_site)
    )
    trace_path <- file.path(tempdir(), "catchmentACS-live-osrm-budget-boundary-trace.txt")
    writeLines(trace, trace_path)

    expect_s3_class(out, "sf")
    expect_false(is.null(prov$osrm_request_budget))
    if (isTRUE(prov$osrm_request_budget$public_demo)) {
      expect_identical(prov$res_param, .OSRM_RES_DEFAULT_DEMO)
      expect_lt(prov$osrm_request_budget$sleep_floor_sec_per_site, 65L)
    } else {
      expect_lt(elapsed, 30)
    }
  })
})
