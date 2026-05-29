# Unit tests for v0.3 UF-2 OSRM res default and once-per-session notice.

.RES_NOTICE_ID <- "catchmentACS_res_default_changed"
.RES_NOTICE_CLASS <- "catchmentACS_message_res_default_changed"
.DEMO_NOTICE_ID <- "catchmentACS_demo_budget_protected"
.DEMO_NOTICE_CLASS <- "catchmentACS_message_demo_budget_protected"

.with_fresh_res_notice_once <- function(code) {
  env <- get("message_freq_env", asNamespace("rlang"))
  had <- exists(.RES_NOTICE_ID, envir = env, inherits = FALSE)
  old <- if (had) get(.RES_NOTICE_ID, envir = env, inherits = FALSE) else NULL

  if (had) rm(list = .RES_NOTICE_ID, envir = env)
  on.exit({
    if (exists(.RES_NOTICE_ID, envir = env, inherits = FALSE)) {
      rm(list = .RES_NOTICE_ID, envir = env)
    }
    if (had) assign(.RES_NOTICE_ID, old, envir = env)
  }, add = TRUE)

  withr::local_options(list(
    "rlang:::message_always" = NULL,
    rlib_message_verbosity = "default"
  ))

  force(code)
}

.seed_res_notice_once <- function() {
  env <- get("message_freq_env", asNamespace("rlang"))
  assign(.RES_NOTICE_ID, Sys.time(), envir = env)
}

.capture_res_notice <- function(expr) {
  msgs <- list()
  value <- suppressMessages(withCallingHandlers(
    force(expr),
    catchmentACS_message_res_default_changed = function(m) {
      msgs <<- c(msgs, list(m))
      invokeRestart("muffleMessage")
    }
  ))
  list(value = value, messages = msgs)
}

.with_fresh_demo_notice_once <- function(code) {
  env <- get("message_freq_env", asNamespace("rlang"))
  had <- exists(.DEMO_NOTICE_ID, envir = env, inherits = FALSE)
  old <- if (had) get(.DEMO_NOTICE_ID, envir = env, inherits = FALSE) else NULL

  if (had) rm(list = .DEMO_NOTICE_ID, envir = env)
  on.exit({
    if (exists(.DEMO_NOTICE_ID, envir = env, inherits = FALSE)) {
      rm(list = .DEMO_NOTICE_ID, envir = env)
    }
    if (had) assign(.DEMO_NOTICE_ID, old, envir = env)
  }, add = TRUE)

  withr::local_options(list(
    "rlang:::message_always" = NULL,
    rlib_message_verbosity = "default"
  ))

  force(code)
}

.capture_demo_notice <- function(expr) {
  msgs <- list()
  value <- suppressMessages(withCallingHandlers(
    force(expr),
    catchmentACS_message_demo_budget_protected = function(m) {
      msgs <<- c(msgs, list(m))
      invokeRestart("muffleMessage")
    }
  ))
  list(value = value, messages = msgs)
}

.mk_res_notice_sites <- function(n = 1L) {
  tibble::tibble(
    site_id = paste0("S", sprintf("%02d", seq_len(n))),
    lon = rep(-86.80902, n),
    lat = rep(33.52203, n)
  )
}

.mk_res_osrm_response <- function(breaks,
                                  center_lon = -86.81,
                                  center_lat = 33.52) {
  breaks <- as.integer(breaks)
  polys <- lapply(breaks, function(b) {
    half <- 0.01 * b
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
      isomin = c(0L, utils::head(breaks, -1L)),
      isomax = breaks
    ),
    geometry = sf::st_sfc(polys, crs = 4326)
  )
}

.mk_res_run_iso <- function(site_ids = "S01", drive_times = 5L,
                            provider = "osrm") {
  rows <- expand.grid(
    site_id = site_ids,
    drive_time_min = as.integer(drive_times),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  geom <- sf::st_sfc(lapply(seq_len(nrow(rows)), function(i) {
    sf::st_polygon(list(rbind(
      c(-86.81, 33.52), c(-86.80, 33.52),
      c(-86.80, 33.53), c(-86.81, 33.53),
      c(-86.81, 33.52)
    )))
  }), crs = 4326)
  sf::st_sf(
    tibble::tibble(
      site_id = rows$site_id,
      drive_time_min = rows$drive_time_min,
      provider = provider,
      profile = "car",
      osm_snapshot_date = "unknown",
      routing_engine_version = "stub/0.0.0",
      polygon_simplification_tolerance = NA_real_,
      generated_at = Sys.time(),
      isochrone_empty = FALSE,
      provider_requested = provider,
      provider_downgrade = FALSE,
      osm_snapshot_status = "unknown_best_effort",
      failure_reason = NA_character_,
      retry_count = 1L,
      ring_topology = "cumulative"
    ),
    geometry = geom
  )
}

.mk_res_run_acs <- function() {
  sf::st_sf(
    tibble::tibble(
      GEOID = "01001000100",
      NAME = "Synthetic tract",
      variable = "B17001_002",
      estimate = 100,
      moe = 10
    ),
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(-86.82, 33.51), c(-86.79, 33.51),
      c(-86.79, 33.54), c(-86.82, 33.54),
      c(-86.82, 33.51)
    ))), crs = 4269)
  )
}

.mk_res_run_long <- function(site_ids = "S01", drive_times = 5L) {
  tibble::tibble(
    site_id = site_ids[[1L]],
    drive_time_min = as.integer(drive_times[[1L]]),
    ring_topology = "cumulative",
    variable = "poverty_rate",
    estimate = 0.25,
    moe = 0.05,
    weight_sum = 1,
    n_tracts = 1L,
    n_tracts_num = 1L,
    n_tracts_den = 1L,
    provider = "osrm",
    profile = "car",
    osm_snapshot_date = "unknown",
    acs_year = 2023L,
    weight_method = "area",
    estimand_family = "derived_rate",
    weight_basis = "coverage",
    moe_formula_requested = "general_ratio_conservative",
    moe_formula_effective = "general_ratio_conservative",
    moe_fallback = FALSE,
    moe_fallback_reason = "n/a",
    failure_origin = "none",
    weight_uncertainty_propagated = FALSE,
    est_total = NA_real_,
    var_total_raw = NA_real_,
    est_mean = NA_real_,
    var_mean_raw = NA_real_
  )
}

.with_res_run_stubs <- function(record_env, code) {
  testthat::local_mocked_bindings(
    cacs_acs_prefetch = function(...) .mk_res_run_acs(),
    cacs_isochrone = function(sites, drive_times, provider, cache_dir = NULL, ...) {
      record_env$iso_dots <- list(...)
      .mk_res_run_iso(unique(sites$site_id), drive_times, provider = provider)
    },
    cacs_intersect_weight = function(iso_sf, acs_sf, ...) {
      out <- .mk_res_run_long(
        site_ids = unique(iso_sf$site_id),
        drive_times = sort(unique(iso_sf$drive_time_min))
      )
      attr(out, "cacs_aggregation_carriers") <- tibble::tibble()
      out
    },
    cacs_propagate_moe = function(data, ...) data,
    cacs_derive_rates = function(weighted_acs, ...) weighted_acs,
    .package = "catchmentACS",
    .env = parent.frame()
  )
  force(code)
}

.res_help_text <- function(topic) {
  rd_path <- file.path("man", paste0(topic, ".Rd"))
  if (!file.exists(rd_path)) {
    rd_path <- testthat::test_path("..", "..", "man", paste0(topic, ".Rd"))
  }
  if (file.exists(rd_path)) {
    return(paste(capture.output(tools::Rd2txt(rd_path)), collapse = "\n"))
  }

  help_ref <- utils::help(topic, package = "catchmentACS")
  if (length(help_ref) == 0L) {
    testthat::skip("Installed help topic is not available in this context")
  }
  paste(capture.output(tools::Rd2txt(utils:::.getHelpFile(help_ref))),
        collapse = "\n")
}

test_that("res default notice class is registered", {
  classes <- .cacs_cond_classes("message", "res_default_changed")
  expect_equal(classes[[1L]], .RES_NOTICE_CLASS)
  expect_true("catchmentACS_message" %in% classes)
  expect_true("catchmentACS_condition" %in% classes)
})

test_that("OSRM omitted-res default constant is locked", {
  expect_identical(.OSRM_RES_DEFAULT, 70L)
})

test_that("res default notice helper uses rlang once metadata", {
  body_txt <- paste(deparse(body(.cli_inform_res_default_changed)), collapse = "\n")
  expect_match(body_txt, '.frequency = "once"', fixed = TRUE)
  expect_match(
    body_txt,
    '.frequency_id = "catchmentACS_res_default_changed"',
    fixed = TRUE
  )
})

test_that("res default notice helper emits classed message with recovery text", {
  .with_fresh_res_notice_once({
    cap <- .capture_res_notice(.cli_inform_res_default_changed())
    expect_length(cap$messages, 1L)
    msg <- cap$messages[[1L]]
    expect_s3_class(msg, .RES_NOTICE_CLASS)
    expect_s3_class(msg, "catchmentACS_message")
    expect_s3_class(msg, "catchmentACS_condition")
    expect_match(conditionMessage(msg), "OSRM")
    expect_match(conditionMessage(msg), "res")
    expect_match(conditionMessage(msg), "70")
    expect_match(conditionMessage(msg), "res = 50L", fixed = TRUE)
  })
})

test_that("res default notice fires once for repeated omitted-res calls", {
  .with_fresh_res_notice_once({
    cap <- .capture_res_notice({
      .cli_inform_res_default_changed()
      .cli_inform_res_default_changed()
    })
    expect_length(cap$messages, 1L)
  })
})

test_that("res default notice respects existing rlang once sentinel", {
  .with_fresh_res_notice_once({
    .seed_res_notice_once()
    cap <- .capture_res_notice(.cli_inform_res_default_changed())
    expect_length(cap$messages, 0L)
  })
})

test_that("internal OSRM omitted demo res defaults to 30L with demo-budget notice", {
  site <- sf::st_as_sf(.mk_res_notice_sites(), coords = c("lon", "lat"),
                       crs = 4326)
  seen <- NULL
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      seen <<- res
      .mk_res_osrm_response(breaks)
    },
    .package = "catchmentACS"
  )
  .with_fresh_demo_notice_once({
    cap <- .capture_demo_notice(
      .iso_via_osrm(site, drive_times = 5L, profile = "car")
    )
    expect_identical(seen, .OSRM_RES_DEFAULT_DEMO)
    expect_length(cap$messages, 1L)
  })
})

test_that("internal OSRM explicit res = 50L remains silent and forwarded", {
  site <- sf::st_as_sf(.mk_res_notice_sites(), coords = c("lon", "lat"),
                       crs = 4326)
  seen <- NULL
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      seen <<- res
      .mk_res_osrm_response(breaks)
    },
    .package = "catchmentACS"
  )
  .with_fresh_res_notice_once({
    cap <- .capture_res_notice(
      .iso_via_osrm(site, drive_times = 5L, profile = "car", res = 50L)
    )
    expect_identical(seen, 50L)
    expect_length(cap$messages, 0L)
  })
})

test_that("internal OSRM explicit res = 70L remains silent and forwarded", {
  site <- sf::st_as_sf(.mk_res_notice_sites(), coords = c("lon", "lat"),
                       crs = 4326)
  seen <- NULL
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      seen <<- res
      .mk_res_osrm_response(breaks)
    },
    .package = "catchmentACS"
  )
  .with_fresh_res_notice_once({
    cap <- .capture_res_notice(
      .iso_via_osrm(site, drive_times = 5L, profile = "car", res = 70L)
    )
    expect_identical(seen, 70L)
    expect_length(cap$messages, 0L)
  })
})

test_that("direct cacs_isochrone omitted demo OSRM res emits demo-budget notice and forwards 30L", {
  td <- tempfile("cacs_res_default_")
  on.exit(if (dir.exists(td)) unlink(td, recursive = TRUE), add = TRUE)
  seen <- NULL
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      seen <<- res
      .mk_res_osrm_response(breaks)
    },
    .package = "catchmentACS"
  )
  .with_fresh_demo_notice_once({
    cap <- .capture_demo_notice(
      cacs_isochrone(
        sites = .mk_res_notice_sites(),
        drive_times = 5L,
        provider = "osrm",
        cache_dir = td,
        verbose = FALSE
      )
    )
    expect_identical(seen, .OSRM_RES_DEFAULT_DEMO)
    expect_length(cap$messages, 1L)
  })
})

test_that("direct cacs_isochrone omitted docker OSRM res emits res-default notice and forwards 70L", {
  td <- tempfile("cacs_res_default_docker_")
  on.exit(if (dir.exists(td)) unlink(td, recursive = TRUE), add = TRUE)
  seen <- NULL
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      seen <<- res
      .mk_res_osrm_response(breaks)
    },
    .package = "catchmentACS"
  )
  .with_fresh_res_notice_once({
    cap <- .capture_res_notice(
      cacs_isochrone(
        sites = .mk_res_notice_sites(),
        drive_times = 5L,
        provider = "osrm",
        osrm_mode = "docker",
        cache_dir = td,
        verbose = FALSE
      )
    )
    expect_identical(seen, .OSRM_RES_DEFAULT)
    expect_length(cap$messages, 1L)
  })
})

test_that("direct cacs_isochrone omitted demo OSRM res can opt out to 70L", {
  td <- tempfile("cacs_res_default_demo_optout_")
  on.exit(if (dir.exists(td)) unlink(td, recursive = TRUE), add = TRUE)
  seen <- NULL
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      seen <<- res
      .mk_res_osrm_response(breaks)
    },
    .package = "catchmentACS"
  )
  withr::local_options(catchmentACS.osrm_demo_budget_protect = FALSE)
  .with_fresh_res_notice_once({
    cap <- .capture_res_notice(
      cacs_isochrone(
        sites = .mk_res_notice_sites(),
        drive_times = 5L,
        provider = "osrm",
        cache_dir = td,
        verbose = FALSE
      )
    )
    expect_identical(seen, .OSRM_RES_DEFAULT)
    expect_length(cap$messages, 1L)
  })
})

test_that("direct cacs_isochrone repeated omitted demo OSRM res emits one notice", {
  td <- tempfile("cacs_res_default_once_")
  on.exit(if (dir.exists(td)) unlink(td, recursive = TRUE), add = TRUE)
  call_n <- 0L
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      call_n <<- call_n + 1L
      .mk_res_osrm_response(breaks)
    },
    .package = "catchmentACS"
  )
  .with_fresh_demo_notice_once({
    cap <- .capture_demo_notice({
      cacs_isochrone(
        sites = .mk_res_notice_sites(), drive_times = 5L,
        provider = "osrm", cache_dir = td, verbose = FALSE
      )
      cacs_isochrone(
        sites = .mk_res_notice_sites(), drive_times = 5L,
        provider = "osrm", cache_dir = td, verbose = FALSE
      )
    })
    expect_length(cap$messages, 1L)
    expect_equal(call_n, 1L)
  })
})

test_that("direct cacs_isochrone explicit res = 50L is silent and forwarded", {
  td <- tempfile("cacs_res_explicit_50_")
  on.exit(if (dir.exists(td)) unlink(td, recursive = TRUE), add = TRUE)
  seen <- NULL
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      seen <<- res
      .mk_res_osrm_response(breaks)
    },
    .package = "catchmentACS"
  )
  .with_fresh_res_notice_once({
    cap <- .capture_res_notice(
      cacs_isochrone(
        sites = .mk_res_notice_sites(), drive_times = 5L,
        provider = "osrm", cache_dir = td, verbose = FALSE,
        res = 50L
      )
    )
    expect_identical(seen, 50L)
    expect_length(cap$messages, 0L)
  })
})

test_that("direct cacs_isochrone explicit res = 70L is silent and forwarded", {
  td <- tempfile("cacs_res_explicit_70_")
  on.exit(if (dir.exists(td)) unlink(td, recursive = TRUE), add = TRUE)
  seen <- NULL
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      seen <<- res
      .mk_res_osrm_response(breaks)
    },
    .package = "catchmentACS"
  )
  .with_fresh_res_notice_once({
    cap <- .capture_res_notice(
      cacs_isochrone(
        sites = .mk_res_notice_sites(), drive_times = 5L,
        provider = "osrm", cache_dir = td, verbose = FALSE,
        res = .OSRM_RES_DEFAULT
      )
    )
    expect_identical(seen, 70L)
    expect_length(cap$messages, 0L)
  })
})

test_that("non-OSRM omitted res path does not emit OSRM res notice", {
  td <- tempfile("cacs_res_non_osrm_")
  on.exit(if (dir.exists(td)) unlink(td, recursive = TRUE), add = TRUE)
  seen_dots <- NULL
  testthat::local_mocked_bindings(
    .iso_via_ors = function(sites, drive_times, profile, api_key, ...) {
      seen_dots <<- list(...)
      .mk_res_run_iso(unique(sites$site_id), drive_times, provider = "ors")
    },
    .package = "catchmentACS"
  )
  .with_fresh_res_notice_once({
    cap <- .capture_res_notice(
      cacs_isochrone(
        sites = .mk_res_notice_sites(), drive_times = 5L,
        provider = "ors", ors_api_key = "FAKE_KEY",
        cache_dir = td, verbose = FALSE
      )
    )
    expect_length(cap$messages, 0L)
    expect_false("res" %in% names(seen_dots))
  })
})

test_that("effective res participates in isochrone cache identity", {
  td <- tempfile("cacs_res_cache_identity_")
  on.exit(if (dir.exists(td)) unlink(td, recursive = TRUE), add = TRUE)
  seen <- integer(0)
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      seen <<- c(seen, res)
      .mk_res_osrm_response(breaks)
    },
    .package = "catchmentACS"
  )
  .with_fresh_demo_notice_once({
    .capture_demo_notice(
      cacs_isochrone(
        sites = .mk_res_notice_sites(), drive_times = 5L,
        provider = "osrm", cache_dir = td, verbose = FALSE
      )
    )
    .capture_demo_notice(
      cacs_isochrone(
        sites = .mk_res_notice_sites(), drive_times = 5L,
        provider = "osrm", cache_dir = td, verbose = FALSE,
        res = 70L
      )
    )
    .capture_demo_notice(
      cacs_isochrone(
        sites = .mk_res_notice_sites(), drive_times = 5L,
        provider = "osrm", cache_dir = td, verbose = FALSE,
        res = 50L
      )
    )
  })
  expect_identical(seen, c(.OSRM_RES_DEFAULT_DEMO, .OSRM_RES_DEFAULT, 50L))
})

test_that("cacs_run iso_args forwards explicit res without unknown-key warning", {
  seen <- new.env(parent = emptyenv())
  warnings <- list()
  .with_res_run_stubs(seen, {
    out <- withCallingHandlers(
      cacs_run(
        sites = .mk_res_notice_sites(),
        state = "AL",
        drive_times = 5L,
        variables = "B17001_002",
        iso_args = list(res = 50L),
        verbose = FALSE
      ),
      catchmentACS_warning_provenance = function(w) {
        warnings <<- c(warnings, list(w))
        invokeRestart("muffleWarning")
      }
    )
    expect_s3_class(out, "tbl_df")
  })
  expect_identical(seen$iso_dots$res, 50L)
  expect_length(warnings, 0L)
})

test_that("cacs_run iso_args forwards explicit res = 70L silently", {
  seen <- new.env(parent = emptyenv())
  warnings <- list()
  .with_res_run_stubs(seen, {
    out <- withCallingHandlers(
      cacs_run(
        sites = .mk_res_notice_sites(),
        state = "AL",
        drive_times = 5L,
        variables = "B17001_002",
        iso_args = list(res = 70L),
        verbose = FALSE
      ),
      catchmentACS_warning_provenance = function(w) {
        warnings <<- c(warnings, list(w))
        invokeRestart("muffleWarning")
      }
    )
    expect_s3_class(out, "tbl_df")
  })
  expect_identical(seen$iso_dots$res, 70L)
  expect_length(warnings, 0L)
})

test_that("OSRM res defaults and demo-budget protection are documented in generated help", {
  rd_txt <- .res_help_text("cacs_isochrone")
  expect_match(rd_txt, "protected by resolving", fixed = TRUE)
  expect_match(rd_txt, "30L", fixed = TRUE)
  expect_match(rd_txt, "70L", fixed = TRUE)
  expect_match(rd_txt, "catchmentACS_message_demo_budget_protected",
               fixed = TRUE)
  expect_match(rd_txt, "catchmentACS_message_res_default_changed",
               fixed = TRUE)
  expect_match(rd_txt, "res = 50L", fixed = TRUE)
})
