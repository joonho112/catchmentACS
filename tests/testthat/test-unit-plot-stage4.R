# ============================================================================
# Unit tests for cacs_plot_site_rates() Stage 4 impl (Step 6.5).
#
# 5 testcases per the Step 6.5 plan:
#   T-PLOT-S4-01  Widget class check (leaflet + htmlwidget)
#   T-PLOT-S4-02  Popup contains all 5 rate names
#   T-PLOT-S4-03  n_tracts_num / n_tracts_den surface in popup
#   T-PLOT-S4-04  moe_fallback = TRUE row visually flagged
#   T-PLOT-S4-05  site_id not in run_result aborts catchmentACS_error_schema
#
# Uses bundled fixtures + a small cacs_run() invocation (Tier 1 1-site
# smoke pattern). Falls back to a hand-constructed run_result tibble for
# tests 04/05 to keep them hermetic from cacs_run() internals (fallback
# flag, missing-site).
# ============================================================================

testthat::skip_if_not_installed("leaflet")
testthat::skip_if_not_installed("sf")


.load_iso_s4 <- function() {
  path <- system.file("extdata", "legacy_2025_isochrones.rds",
                      package = "catchmentACS")
  testthat::skip_if(!nzchar(path) || !file.exists(path),
                    "legacy_2025_isochrones.rds fixture not installed")
  readRDS(path)
}

.build_run_result_s4 <- function() {
  iso   <- .load_iso_s4()
  sites_path <- system.file("extdata", "legacy_2025_sites.rds",
                            package = "catchmentACS")
  acs_path   <- system.file("extdata", "sample_alabama_subset.rds",
                            package = "catchmentACS")
  testthat::skip_if(!nzchar(sites_path) || !file.exists(sites_path),
                    "legacy_2025_sites.rds fixture not installed")
  testthat::skip_if(!nzchar(acs_path)   || !file.exists(acs_path),
                    "sample_alabama_subset.rds fixture not installed")
  sites <- readRDS(sites_path)
  acs   <- readRDS(acs_path)
  iso_subset   <- iso[iso$site_id == "AL_SITE_01" & iso$drive_time_min == 5, ]
  sites_subset <- sites[sites$site_id == "AL_SITE_01", ]
  suppressWarnings(cacs_run(
    sites                  = sites_subset,
    state                  = "AL",
    year                   = 2023,
    drive_times            = 5,
    variables              = unname(cacs_acs_default_vars),
    provider               = "osrm",
    precomputed_isochrones = iso_subset,
    acs                    = acs,
    weight_args            = list(weight_method = "area"),
    output                 = "long",
    verbose                = FALSE
  ))
}


test_that("T-PLOT-S4-01 cacs_plot_site_rates() returns leaflet widget", {
  iso        <- .load_iso_s4()
  run_result <- .build_run_result_s4()
  w <- cacs_plot_site_rates(
    site_id    = "AL_SITE_01",
    iso_sf     = iso,
    run_result = run_result,
    sites_df   = cacs_alabama_sites
  )
  expect_s3_class(w, "leaflet")
  expect_s3_class(w, "htmlwidget")

  call_methods <- vapply(w$x$calls,
                         function(c) c$method %||% NA_character_,
                         character(1))
  expect_true("addProviderTiles" %in% call_methods)
  expect_true("addCircleMarkers" %in% call_methods)
  expect_true("addPolygons"      %in% call_methods)
})


test_that("T-PLOT-S4-02 popup contains all 5 rate names", {
  iso        <- .load_iso_s4()
  run_result <- .build_run_result_s4()
  w <- cacs_plot_site_rates(
    site_id    = "AL_SITE_01",
    iso_sf     = iso,
    run_result = run_result,
    sites_df   = cacs_alabama_sites
  )
  call_methods <- vapply(w$x$calls,
                         function(c) c$method %||% NA_character_,
                         character(1))
  marker_idx <- which(call_methods == "addCircleMarkers")
  expect_length(marker_idx, 1L)
  # leaflet packs addCircleMarkers args positionally; popup is one of the
  # named members. Inspect the JSON-friendly args list and grab whatever
  # element carries our wrapper div as a substring.
  marker_args <- w$x$calls[[marker_idx]]$args
  flat <- paste(unlist(marker_args), collapse = "\n")
  expect_true(grepl("cacs-popup-rates", flat, fixed = TRUE))
  for (rate in c("poverty_rate", "snap_rate", "ssi_rate",
                 "unemp_rate", "labor_force_participation")) {
    expect_true(
      grepl(rate, flat, fixed = TRUE),
      info = paste0("rate ", rate, " missing from popup HTML")
    )
  }
})


test_that("T-PLOT-S4-03 popup carries n_tracts_num and n_tracts_den labels", {
  iso        <- .load_iso_s4()
  run_result <- .build_run_result_s4()
  w <- cacs_plot_site_rates(
    site_id    = "AL_SITE_01",
    iso_sf     = iso,
    run_result = run_result,
    sites_df   = cacs_alabama_sites
  )
  call_methods <- vapply(w$x$calls,
                         function(c) c$method %||% NA_character_,
                         character(1))
  marker_args <- w$x$calls[[which(call_methods == "addCircleMarkers")]]$args
  flat <- paste(unlist(marker_args), collapse = "\n")
  # The shared rate popup template surfaces both n_tracts counts under a
  # single "n_tracts (num/den):" label per inst/templates/popup.html.
  expect_true(grepl("n_tracts (num/den)", flat, fixed = TRUE))

  # Snapshot the rate popup HTML structure so future template edits get
  # surfaced loudly. The popup is multi-line so use `(?s)` to make `.`
  # match newlines. We grab everything from the wrapper div opener through
  # the final closing div block.
  popup_html <- regmatches(
    flat,
    regexpr("(?s)<div class=\"cacs-popup-rates\">.*?</div>\\s*</div>",
            flat, perl = TRUE)
  )
  expect_true(length(popup_html) == 1L && nzchar(popup_html))
  # Substitute numeric runs so the snapshot is stable across ACS vintages.
  popup_norm <- gsub("[0-9]+\\.[0-9]+", "<num>", popup_html, perl = TRUE)
  popup_norm <- gsub("\\b[0-9]+\\b",      "<int>", popup_norm, perl = TRUE)
  testthat::expect_snapshot(cat(popup_norm))
})


test_that("T-PLOT-S4-04 moe_fallback = TRUE row gets a visual flag", {
  iso <- .load_iso_s4()
  # Hand-construct a 1-site run_result with one fallback row. Keeps the
  # test hermetic from cacs_run() fallback-trigger internals.
  rr <- tibble::tibble(
    site_id        = "AL_SITE_01",
    drive_time_min = 5L,
    variable       = c("poverty_rate", "snap_rate", "ssi_rate",
                       "unemp_rate", "labor_force_participation"),
    estimate       = c(0.20, 0.10, 0.05, 0.07, 0.65),
    moe            = c(0.03, 0.02, 0.01, 0.02, 0.08),
    n_tracts_num   = c(3L, 3L, 3L, 3L, 3L),
    n_tracts_den   = c(3L, 3L, 3L, 3L, 3L),
    moe_fallback   = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    moe_formula_effective = c("proportion_subset",
                              "general_ratio_conservative",
                              "general_ratio_conservative",
                              "general_ratio_conservative",
                              "general_ratio_conservative")
  )
  w <- cacs_plot_site_rates(
    site_id    = "AL_SITE_01",
    iso_sf     = iso,
    run_result = rr,
    sites_df   = cacs_alabama_sites
  )
  call_methods <- vapply(w$x$calls,
                         function(c) c$method %||% NA_character_,
                         character(1))
  marker_args <- w$x$calls[[which(call_methods == "addCircleMarkers")]]$args
  flat <- paste(unlist(marker_args), collapse = "\n")
  # The fallback-flagged variable gets an asterisk prefix in the rendered
  # popup variable cell ("* snap_rate"), and the template's "MOE fallback"
  # row reads "fallback applied".
  expect_true(grepl("* snap_rate", flat, fixed = TRUE))
  expect_true(grepl("fallback applied", flat, fixed = TRUE))
  # Non-fallback rows must not carry the asterisk prefix.
  expect_false(grepl("* poverty_rate", flat, fixed = TRUE))
})


test_that("T-PLOT-S4-05 unknown site_id in run_result aborts via schema family", {
  iso <- .load_iso_s4()
  # Build a run_result that only carries AL_SITE_02 so the AL_SITE_01
  # filter inside the function returns 0 rows.
  rr <- tibble::tibble(
    site_id        = "AL_SITE_02",
    drive_time_min = 5L,
    variable       = "poverty_rate",
    estimate       = 0.20,
    moe            = 0.03,
    n_tracts_num   = 3L,
    n_tracts_den   = 3L,
    moe_fallback   = FALSE,
    moe_formula_effective = "proportion_subset"
  )
  expect_error(
    cacs_plot_site_rates(
      site_id    = "AL_SITE_01",
      iso_sf     = iso,
      run_result = rr,
      sites_df   = cacs_alabama_sites
    ),
    class = "catchmentACS_error_schema"
  )
})
