# ============================================================================
# C-06 Stage 4 + pipeline input forms.
#
# Locks the Phase 7.1 behavior: Stage 4 and the 4-stage plot pipeline accept
# direct `(lat, lon)` input by resolving it to the nearest run-result site, and
# `site_name` continues to work through the exact-match resolver.
# ============================================================================

testthat::skip_if_not_installed("leaflet")
testthat::skip_if_not_installed("sf")


.c06_load_iso <- function() {
  path <- system.file("extdata", "legacy_2025_isochrones.rds",
                      package = "catchmentACS")
  testthat::skip_if(!nzchar(path) || !file.exists(path),
                    "legacy_2025_isochrones.rds fixture not installed")
  readRDS(path)
}

.c06_load_sites <- function() {
  path <- system.file("extdata", "legacy_2025_sites.rds",
                      package = "catchmentACS")
  testthat::skip_if(!nzchar(path) || !file.exists(path),
                    "legacy_2025_sites.rds fixture not installed")
  readRDS(path)
}

.c06_run_result <- function(site_id = "AL_SITE_01") {
  tibble::tibble(
    site_id = site_id,
    drive_time_min = 5L,
    variable = c("poverty_rate", "snap_rate", "ssi_rate",
                 "unemp_rate", "labor_force_participation"),
    estimate = c(0.20, 0.10, 0.05, 0.07, 0.65),
    moe = c(0.03, 0.02, 0.01, 0.02, 0.08),
    n_tracts_num = 3L,
    n_tracts_den = 3L,
    moe_fallback = FALSE,
    moe_formula_effective = "proportion_subset"
  )
}

.c06_fixtures <- function() {
  sites <- .c06_load_sites()
  sites <- sites[match(c("AL_SITE_02", "AL_SITE_01"), sites$site_id), ]
  site <- sites[sites$site_id == "AL_SITE_01", ]
  coord <- sf::st_coordinates(site)[1L, ]
  list(
    iso = .c06_load_iso(),
    sites = sites,
    site = site,
    lon = as.numeric(coord[["X"]]),
    lat = as.numeric(coord[["Y"]]),
    rr = .c06_run_result("AL_SITE_01")
  )
}

.c06_capture_plot_conditions <- function(expr) {
  messages <- list()
  warnings <- list()
  value <- withCallingHandlers(
    force(expr),
    catchmentACS_message_resolve_site = function(m) {
      messages[[length(messages) + 1L]] <<- m
      invokeRestart("muffleMessage")
    },
    catchmentACS_warning_resolve_site_distant = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, messages = messages, warnings = warnings)
}


test_that("C06-S4-01 Stage 4 lat/lon resolves to nearest run-result site", {
  fx <- .c06_fixtures()

  cap <- .c06_capture_plot_conditions(cacs_plot_site_rates(
    lat = fx$lat,
    lon = fx$lon,
    iso_sf = fx$iso,
    run_result = fx$rr,
    sites_df = fx$sites
  ))

  expect_s3_class(cap$value, "leaflet")
  expect_s3_class(cap$value, "htmlwidget")
  expect_length(cap$messages, 1L)
  expect_s3_class(cap$messages[[1L]], "catchmentACS_message_resolve_site")
  expect_match(conditionMessage(cap$messages[[1L]]), "AL_SITE_01",
               fixed = TRUE)
  expect_length(cap$warnings, 0L)
})


test_that("C06-COND-01 resolve-site condition classes are capturable", {
  msg <- cacs_capture_conditions(
    .cli_inform_resolve_site("Synthetic resolve-site notice"),
    classes = "resolve_site"
  )
  warn <- cacs_capture_conditions(
    .cli_warn_resolve_site_distant("Synthetic distant-site warning"),
    classes = "resolve_site_distant"
  )

  expect_equal(msg$class, "catchmentACS_message_resolve_site")
  expect_equal(warn$class, "catchmentACS_warning_resolve_site_distant")
})


test_that("C06-S4-02 Stage 4 site_name resolves without nearest-site notice", {
  fx <- .c06_fixtures()

  cap <- .c06_capture_plot_conditions(cacs_plot_site_rates(
    site_name = fx$site$site_name[[1L]],
    iso_sf = fx$iso,
    run_result = fx$rr,
    sites_df = fx$sites
  ))

  expect_s3_class(cap$value, "leaflet")
  expect_length(cap$messages, 0L)
  expect_length(cap$warnings, 0L)
  flat <- paste(unlist(cap$value$x$calls), collapse = "\n")
  expect_true(grepl("cacs-popup-rates", flat, fixed = TRUE))
  expect_true(grepl(fx$site$site_name[[1L]], flat, fixed = TRUE))
})


test_that("C06-S4-03 distant Stage 4 lat/lon emits notice and warning", {
  fx <- .c06_fixtures()

  cap <- .c06_capture_plot_conditions(cacs_plot_site_rates(
    lat = fx$lat - 3,
    lon = fx$lon,
    iso_sf = fx$iso,
    run_result = fx$rr,
    sites_df = fx$sites
  ))

  expect_s3_class(cap$value, "leaflet")
  expect_length(cap$messages, 1L)
  expect_s3_class(cap$messages[[1L]], "catchmentACS_message_resolve_site")
  expect_length(cap$warnings, 1L)
  expect_s3_class(cap$warnings[[1L]],
                  "catchmentACS_warning_resolve_site_distant")
  expect_match(conditionMessage(cap$warnings[[1L]]), "AL_SITE_01",
               fixed = TRUE)
})


test_that("C06-S4-04 missing sites_df gives clear early lat/lon error", {
  fx <- .c06_fixtures()
  rr <- .c06_run_result("CUSTOM_ONLY")

  err <- tryCatch(
    cacs_plot_site_rates(
      lat = fx$lat,
      lon = fx$lon,
      iso_sf = fx$iso,
      run_result = rr
    ),
    error = identity
  )

  expect_s3_class(err, "catchmentACS_error_schema")
  expect_match(conditionMessage(err), "sites_df", fixed = TRUE)
  expect_match(conditionMessage(err), "site_id", fixed = TRUE)
  expect_match(conditionMessage(err), "cacs_alabama_sites", fixed = TRUE)
})


test_that("C06-PIPE-01 pipeline lat/lon resolves exactly once up front", {
  fx <- .c06_fixtures()
  calls <- list()
  fake_stage <- function(stage) {
    force(stage)
    function(site_id = NULL, lat = NULL, lon = NULL, ...) {
      calls[[stage]] <<- list(site_id = site_id, lat = lat, lon = lon)
      structure(list(), class = c("leaflet", "htmlwidget"))
    }
  }

  testthat::local_mocked_bindings(
    cacs_plot_site_isochrone = fake_stage("isochrone"),
    cacs_plot_site_intersection = fake_stage("intersection"),
    cacs_plot_site_weighted = fake_stage("weighted"),
    cacs_plot_site_rates = fake_stage("rates"),
    .package = "catchmentACS"
  )

  cap <- .c06_capture_plot_conditions(cacs_plot_site_pipeline(
    lat = fx$lat,
    lon = fx$lon,
    iso_sf = NULL,
    tract_sf = NULL,
    acs_sf = NULL,
    run_result = tibble::tibble(site_id = "AL_SITE_01"),
    sites_df = fx$sites
  ))

  expect_s3_class(cap$value, "cacs_site_plot_pipeline")
  expect_length(cap$messages, 1L)
  expect_length(cap$warnings, 0L)
  expect_identical(attr(cap$value, "site_id"), "AL_SITE_01")
  expect_named(calls, c("isochrone", "intersection", "weighted", "rates"))
  expect_identical(
    vapply(calls, `[[`, character(1), "site_id"),
    c(isochrone = "AL_SITE_01", intersection = "AL_SITE_01",
      weighted = "AL_SITE_01", rates = "AL_SITE_01")
  )
  expect_true(all(vapply(calls, function(x) is.null(x$lat) && is.null(x$lon),
                         logical(1))))
})
