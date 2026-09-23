visual_walkthrough_fixture <- function() {
  readRDS(system.file("extdata", "visual_walkthrough_fixture.rds",
                      package = "catchmentACS", mustWork = TRUE))
}

visual_proxy_rows <- function(x) {
  keep <- x$variable %in% c("B19013_001", "B19301_001")
  out <- data.frame(
    site_id = x$site_id[keep],
    drive_time_min = x$drive_time_min[keep],
    variable = x$variable[keep],
    estimate = x$estimate[keep],
    moe = x$moe[keep],
    stringsAsFactors = FALSE
  )
  out[order(out$site_id, out$drive_time_min, out$variable), ]
}

test_that("visual walkthrough fixture is schema-current", {
  fx <- visual_walkthrough_fixture()

  expect_type(fx, "list")
  expect_setequal(
    names(fx),
    c("iso_sf", "tract_sf", "acs_sf", "run_result", "sites_df", "anchor_site")
  )
  expect_true("ring_topology" %in% names(fx$iso_sf))
  expect_true(all(fx$iso_sf$ring_topology == "cumulative"))
  expect_equal(nrow(cacs_validate_iso(fx$iso_sf)), 0L)

  expect_true("ring_topology" %in% names(fx$run_result))
  expect_true(all(fx$run_result$ring_topology == "cumulative"))

  meta <- attr(fx$run_result, "cacs_run_result_metadata")
  prov <- attr(fx$run_result, "cacs_run_provenance")
  expect_equal(as.character(meta$cacs_version), "0.5.0")
  expect_equal(prov$cacs_ver, "0.5.0")
  expect_equal(prov$execution_path, "3-call")
  expect_true(isTRUE(prov$bypass_iso))
  expect_true(isTRUE(prov$bypass_acs))
})

test_that("visual walkthrough proxy rows match a current offline rerun", {
  fx <- visual_walkthrough_fixture()

  old_cache_enabled <- getOption("catchmentACS.cache_enabled", NULL)
  withr::local_options(catchmentACS.cache_enabled = old_cache_enabled)
  cacs_set_cache(FALSE, scope = "session")

  fresh <- suppressWarnings(suppressMessages(cacs_run(
    sites                  = fx$sites_df,
    state                  = "AL",
    year                   = 2023,
    drive_times            = c(5L, 10L, 15L),
    variables              = unname(cacs_acs_default_vars),
    provider               = "osrm",
    weight_method          = "area",
    precomputed_isochrones = fx$iso_sf,
    acs                    = fx$acs_sf,
    rates                  = cacs_acs_default_rates,
    output                 = "long",
    verbose                = FALSE
  )))

  stored_proxy <- visual_proxy_rows(fx$run_result)
  fresh_proxy <- visual_proxy_rows(fresh)

  expect_equal(stored_proxy[, c("site_id", "drive_time_min", "variable")],
               fresh_proxy[, c("site_id", "drive_time_min", "variable")])
  expect_equal(stored_proxy$estimate, fresh_proxy$estimate, tolerance = 1e-8)
  expect_equal(stored_proxy$moe, fresh_proxy$moe, tolerance = 1e-8)
  expect_gt(min(stored_proxy$estimate, na.rm = TRUE), 10000)
})
