# ============================================================================
# C-05 lock tests: cacs_run(verbose=) propagates to all five subcalls.
# ============================================================================


.c05_sites <- function() {
  tibble::tibble(site_id = "S01", lon = -86.80, lat = 33.50)
}


.c05_iso <- function(sites, drive_times = 5L) {
  geom <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(-86.81, 33.50), c(-86.79, 33.50),
    c(-86.79, 33.52), c(-86.81, 33.52),
    c(-86.81, 33.50)
  ))), crs = 4326)
  sf::st_sf(
    tibble::tibble(
      site_id = sites$site_id[[1L]],
      drive_time_min = as.integer(drive_times[[1L]]),
      provider = "osrm",
      profile = "car",
      osm_snapshot_date = NA_character_,
      routing_engine_version = "stub/0.0.0",
      polygon_simplification_tolerance = NA_real_,
      generated_at = as.POSIXct("2026-05-24 00:00:00", tz = "UTC"),
      isochrone_empty = FALSE,
      provider_requested = "osrm",
      provider_downgrade = FALSE,
      osm_snapshot_status = NA_character_,
      failure_reason = NA_character_,
      retry_count = 1L,
      ring_topology = "cumulative"
    ),
    geometry = geom
  )
}


.c05_acs <- function() {
  sf::st_sf(
    tibble::tibble(GEOID = "01001000100", NAME = "Tract",
                   variable = "B17001_002", estimate = 100, moe = 10),
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(-86.82, 33.49), c(-86.78, 33.49),
      c(-86.78, 33.53), c(-86.82, 33.53),
      c(-86.82, 33.49)
    ))), crs = 4269)
  )
}


.c05_long <- function() {
  tibble::tibble(
    site_id = "S01",
    drive_time_min = 5L,
    ring_topology = "cumulative",
    variable = "B17001_002",
    estimate = 100,
    moe = 10,
    weight_sum = 1,
    n_tracts = 1L,
    n_tracts_num = NA_integer_,
    n_tracts_den = NA_integer_,
    provider = "osrm",
    profile = "car",
    osm_snapshot_date = NA_character_,
    acs_year = 2023L,
    weight_method = "area",
    estimand_family = "spatial_total",
    weight_basis = "coverage",
    moe_formula_requested = "auto",
    moe_formula_effective = "weighted_sum",
    moe_fallback = FALSE,
    moe_fallback_reason = NA_character_,
    failure_origin = NA_character_,
    weight_uncertainty_propagated = FALSE
  )
}


.c05_emit_summary <- function(label, verbose) {
  if (isTRUE(verbose)) {
    .cacs_progress_summary(.cacs_now(), 1L, 1L, 0L, label, verbose = TRUE)
  }
}


.c05_with_run_stubs <- function(seen, code) {
  testthat::local_mocked_bindings(
    cacs_acs_prefetch = function(..., verbose = TRUE) {
      seen$acs <- verbose
      .c05_emit_summary("ACS prefetch", verbose)
      .c05_acs()
    },
    cacs_isochrone = function(sites, drive_times = 5L, ..., verbose = TRUE) {
      seen$iso <- verbose
      .c05_emit_summary("Isochrones", verbose)
      .c05_iso(sites, drive_times)
    },
    cacs_intersect_weight = function(iso_sf, acs_sf, ..., verbose = TRUE) {
      seen$wgt <- verbose
      .c05_emit_summary("Intersect+weight", verbose)
      out <- .c05_long()
      attr(out, "cacs_aggregation_carriers") <- tibble::tibble()
      out
    },
    cacs_propagate_moe = function(data, ..., verbose = TRUE) {
      seen$moe <- verbose
      .c05_emit_summary("MOE propagation", verbose)
      data
    },
    cacs_derive_rates = function(weighted_acs, ..., verbose = TRUE) {
      seen$rat <- verbose
      .c05_emit_summary("Rate derivation", verbose)
      weighted_acs
    },
    .package = "catchmentACS"
  )
  force(code)
}


.c05_seen_list <- function(seen) {
  list(acs = seen$acs, iso = seen$iso, wgt = seen$wgt,
       moe = seen$moe, rat = seen$rat)
}


test_that("C05-RUN-01 cacs_run(verbose=TRUE) forwards TRUE to all subcalls", {
  seen <- new.env(parent = emptyenv())
  .c05_with_run_stubs(seen, {
    out <- suppressWarnings(cacs_run(
      sites = .c05_sites(), state = "AL", drive_times = 5L,
      variables = "B17001_002", verbose = TRUE
    ))
    expect_s3_class(out, "tbl_df")
  })

  expect_identical(.c05_seen_list(seen), list(
    acs = TRUE, iso = TRUE, wgt = TRUE, moe = TRUE, rat = TRUE
  ))
})


test_that("C05-RUN-02 cacs_run(verbose=TRUE) captures five subcall summaries", {
  seen <- new.env(parent = emptyenv())
  .c05_with_run_stubs(seen, {
    out <- cacs_capture_conditions(
      suppressWarnings(cacs_run(
        sites = .c05_sites(), state = "AL", drive_times = 5L,
        variables = "B17001_002", verbose = TRUE
      )),
      classes = "catchmentACS_message_progress_summary"
    )
  })

  expect_setequal(out$phase,
                  c("ACS prefetch", "Isochrones", "Intersect+weight",
                    "MOE propagation", "Rate derivation"))
  expect_equal(nrow(out), 5L)
})


test_that("C05-RUN-03 cacs_run(verbose=FALSE) forwards FALSE to all subcalls", {
  seen <- new.env(parent = emptyenv())
  .c05_with_run_stubs(seen, {
    out <- suppressWarnings(cacs_run(
      sites = .c05_sites(), state = "AL", drive_times = 5L,
      variables = "B17001_002", verbose = FALSE
    ))
    expect_s3_class(out, "tbl_df")
  })

  expect_identical(.c05_seen_list(seen), list(
    acs = FALSE, iso = FALSE, wgt = FALSE, moe = FALSE, rat = FALSE
  ))
})


test_that("C05-RUN-04 cacs_run(verbose=FALSE) emits zero progress summaries", {
  seen <- new.env(parent = emptyenv())
  .c05_with_run_stubs(seen, {
    out <- cacs_capture_conditions(
      suppressWarnings(cacs_run(
        sites = .c05_sites(), state = "AL", drive_times = 5L,
        variables = "B17001_002", verbose = FALSE
      )),
      classes = "catchmentACS_message_progress_summary"
    )
  })

  expect_equal(nrow(out), 0L)
})


test_that("C05-RUN-05 propagation is stable with cache option enabled", {
  seen <- new.env(parent = emptyenv())
  with_test_cache({
    .c05_with_run_stubs(seen, {
      out <- cacs_capture_conditions(
        suppressWarnings(cacs_run(
          sites = .c05_sites(), state = "AL", drive_times = 5L,
          variables = "B17001_002", verbose = TRUE
        )),
        classes = "catchmentACS_message_progress_summary"
      )
      expect_equal(nrow(out), 5L)
    })
  })
})


test_that("C05-RUN-06 propagation is stable with cache option disabled", {
  seen <- new.env(parent = emptyenv())
  withr::local_options(catchmentACS.cache_isochrone = FALSE,
                       catchmentACS.cache_acs = FALSE,
                       catchmentACS.cache_intersect = FALSE)
  .c05_with_run_stubs(seen, {
    out <- cacs_capture_conditions(
      suppressWarnings(cacs_run(
        sites = .c05_sites(), state = "AL", drive_times = 5L,
        variables = "B17001_002", verbose = TRUE
      )),
      classes = "catchmentACS_message_progress_summary"
    )
    expect_equal(nrow(out), 5L)
  })
})


test_that("C05-RUN-07 MOE and rate stages emit real summary conditions", {
  weighted <- .c05_long()
  attr(weighted, "cacs_schema_version") <- "1.0"
  attr(weighted, "cacs_aggregation_carriers") <- tibble::tibble(
    site_id = "S01",
    drive_time_min = 5L,
    variable = "B17001_002",
    estimand_family = "spatial_total",
    est_total = 100,
    var_total_raw = (10 / .Z_ACS_90)^2,
    est_mean = NA_real_,
    var_mean_raw = NA_real_,
    weight_sum = 1,
    n_tracts = 1L
  )

  moe_conds <- cacs_capture_conditions(
    cacs_propagate_moe(weighted, verbose = TRUE),
    classes = "catchmentACS_message_progress_summary"
  )
  expect_equal(moe_conds$phase, "MOE propagation")

  propagated <- suppressMessages(
    cacs_propagate_moe(weighted, verbose = FALSE)
  )
  rate_conds <- cacs_capture_conditions(
    suppressWarnings(cacs_derive_rates(propagated, verbose = TRUE)),
    classes = "catchmentACS_message_progress_summary"
  )
  expect_equal(rate_conds$phase, "Rate derivation")
})
