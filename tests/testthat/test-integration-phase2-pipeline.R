# ============================================================================
# Phase 2 offline integration locks across run/intersect/MOE/rate layers.
# ============================================================================


.P2_PIPE_ENV <- new.env(parent = emptyenv())

.p2_pipe_read_extdata <- function(name) {
  path <- system.file("extdata", name, package = "catchmentACS")
  testthat::skip_if(!nzchar(path) || !file.exists(path),
                    paste("extdata fixture unavailable:", name))
  readRDS(path)
}

.p2_pipe_legacy_inputs <- function(n_sites = 3L) {
  sites <- .p2_pipe_read_extdata("legacy_2025_sites.rds")
  iso <- .p2_pipe_read_extdata("legacy_2025_isochrones.rds")
  acs <- .p2_pipe_read_extdata("sample_alabama_subset.rds")
  site_ids <- sites$site_id[seq_len(n_sites)]
  iso_sub <- iso[
    iso$site_id %in% site_ids & iso$drive_time_min == 10L,
  ]
  list(sites = sites[seq_len(n_sites), ], iso = iso_sub, acs = acs)
}

.p2_pipe_run <- function() {
  if (!exists("run", envir = .P2_PIPE_ENV, inherits = FALSE)) {
    inputs <- .p2_pipe_legacy_inputs(3L)
    out <- suppressWarnings(cacs_run(
      sites = inputs$sites,
      state = "AL",
      drive_times = 10L,
      precomputed_isochrones = inputs$iso,
      acs = inputs$acs,
      verbose = FALSE
    ))
    assign("run", out, envir = .P2_PIPE_ENV)
  }
  get("run", envir = .P2_PIPE_ENV, inherits = FALSE)
}

.p2_pipe_run_both <- function() {
  if (!exists("run_both", envir = .P2_PIPE_ENV, inherits = FALSE)) {
    inputs <- .p2_pipe_legacy_inputs(1L)
    out <- suppressWarnings(cacs_run(
      sites = inputs$sites,
      state = "AL",
      drive_times = 10L,
      precomputed_isochrones = inputs$iso,
      acs = inputs$acs,
      output = "both",
      verbose = FALSE
    ))
    assign("run_both", out, envir = .P2_PIPE_ENV)
  }
  get("run_both", envir = .P2_PIPE_ENV, inherits = FALSE)
}

.p2_pipe_fixture032_weighted <- function() {
  if (!exists("weighted032", envir = .P2_PIPE_ENV, inherits = FALSE)) {
    fx <- load_replay_fixture("032_3site_fresh")
    iso <- fx$iso_sf_v2
    iso$ring_topology <- "cumulative"
    weighted <- suppressWarnings(cacs_intersect_weight(
      iso_sf = iso,
      acs_sf = fx$acs_sf,
      verbose = FALSE
    ))
    propagated <- suppressWarnings(cacs_propagate_moe(weighted))
    derived <- suppressWarnings(cacs_derive_rates(propagated))
    assign("fixture032", fx, envir = .P2_PIPE_ENV)
    assign("weighted032", weighted, envir = .P2_PIPE_ENV)
    assign("propagated032", propagated, envir = .P2_PIPE_ENV)
    assign("derived032", derived, envir = .P2_PIPE_ENV)
  }
  get("weighted032", envir = .P2_PIPE_ENV, inherits = FALSE)
}

.p2_pipe_propagated032 <- function() {
  .p2_pipe_fixture032_weighted()
  get("propagated032", envir = .P2_PIPE_ENV, inherits = FALSE)
}

.p2_pipe_derived032 <- function() {
  .p2_pipe_fixture032_weighted()
  get("derived032", envir = .P2_PIPE_ENV, inherits = FALSE)
}

.p2_pipe_error <- function(expr) {
  tryCatch(force(expr), error = function(e) e)
}

.p2_pipe_msg <- function(expr) {
  conditionMessage(.p2_pipe_error(expr))
}


test_that("T-P2-PIPE-01 cacs_run bypass returns cacs_run_result", {
  out <- .p2_pipe_run()
  expect_s3_class(out, "cacs_run_result")
  expect_s3_class(out, "tbl_df")
})

test_that("T-P2-PIPE-02 cacs_run bypass output includes required schema", {
  out <- .p2_pipe_run()
  expect_true(all(.LONG_REQUIRED_COLS %in% names(out)))
  expect_equal(ncol(tibble::as_tibble(out)[.LONG_REQUIRED_COLS]), 23L)
})

test_that("T-P2-PIPE-03 cacs_run bypass output includes ring_topology", {
  out <- .p2_pipe_run()
  expect_true("ring_topology" %in% names(out))
  expect_equal(unique(out$ring_topology), "cumulative")
})

test_that("T-P2-PIPE-04 source rows carry cumulative topology", {
  out <- .p2_pipe_run()
  source_rows <- out[!is.na(out$n_tracts), ]
  expect_gt(nrow(source_rows), 0L)
  expect_equal(unique(source_rows$ring_topology), "cumulative")
})

test_that("T-P2-PIPE-05 derived rows carry cumulative topology", {
  out <- .p2_pipe_run()
  rate_rows <- out[is.na(out$n_tracts), ]
  expect_gt(nrow(rate_rows), 0L)
  expect_equal(unique(rate_rows$ring_topology), "cumulative")
})

test_that("T-P2-PIPE-06 run provenance records ISO and ACS bypass", {
  prov <- attr(.p2_pipe_run(), "cacs_run_provenance")
  expect_true(prov$bypass_iso)
  expect_true(prov$bypass_acs)
})

test_that("T-P2-PIPE-07 output='both' preserves long ring_topology", {
  out <- .p2_pipe_run_both()
  expect_true(all(c("long", "list_column") %in% names(out)))
  expect_true("ring_topology" %in% names(out$long))
  expect_equal(unique(out$long$ring_topology), "cumulative")
})

test_that("T-P2-PIPE-08 list-column output is produced from Phase 2 long output", {
  out <- .p2_pipe_run_both()
  expect_s3_class(out$list_column, "tbl_df")
  expect_gt(ncol(out$list_column), 1L)
  expect_equal(attr(out$list_column, "cacs_schema_version"), "1.0")
})

test_that("T-P2-PIPE-09 cacs_intersect_weight carries ring_topology", {
  weighted <- .p2_pipe_fixture032_weighted()
  expect_true("ring_topology" %in% names(weighted))
  expect_equal(unique(weighted$ring_topology), "cumulative")
})

test_that("T-P2-PIPE-10 cacs_propagate_moe preserves ring_topology", {
  propagated <- .p2_pipe_propagated032()
  expect_true("ring_topology" %in% names(propagated))
  expect_equal(unique(propagated$ring_topology), "cumulative")
})

test_that("T-P2-PIPE-11 cacs_derive_rates preserves ring_topology on rate rows", {
  derived <- .p2_pipe_derived032()
  rate_rows <- derived[is.na(derived$n_tracts), ]
  expect_gt(nrow(rate_rows), 0L)
  expect_equal(unique(rate_rows$ring_topology), "cumulative")
})

test_that("T-P2-PIPE-12 final derived output satisfies long schema validator", {
  expect_true(.validate_intersect_output(.p2_pipe_derived032(),
                                         require_carrier = FALSE))
})

test_that("T-P2-PIPE-13 precomputed ISO missing ring_topology aborts helpfully", {
  inputs <- .p2_pipe_legacy_inputs(1L)
  inputs$iso$ring_topology <- NULL
  msg <- .p2_pipe_msg(cacs_run(
    sites = inputs$sites,
    state = "AL",
    drive_times = 10L,
    precomputed_isochrones = inputs$iso,
    acs = inputs$acs,
    verbose = FALSE
  ))
  expect_match(msg, "ring_topology", fixed = TRUE)
  expect_match(msg, "cacs_validate_iso(iso_sf)", fixed = TRUE)
})

test_that("T-P2-PIPE-14 annulus ISO aborts before weighting", {
  inputs <- .p2_pipe_legacy_inputs(1L)
  annulus <- load_replay_fixture("annulus_input")
  err <- .p2_pipe_error(cacs_run(
    sites = inputs$sites,
    state = "AL",
    drive_times = 10L,
    precomputed_isochrones = annulus,
    acs = inputs$acs,
    verbose = FALSE
  ))
  expect_s3_class(err, "catchmentACS_error_annulus_input")
  expect_s3_class(err, "cacs_error_annulus_input")
})

test_that("T-P2-PIPE-15 perturbed provider_downgrade surfaces rewrite", {
  inputs <- .p2_pipe_legacy_inputs(1L)
  inputs$iso$provider_downgrade <- "FALSE"
  msg <- .p2_pipe_msg(cacs_intersect_weight(
    iso_sf = inputs$iso,
    acs_sf = inputs$acs,
    verbose = FALSE
  ))
  expect_match(msg, "provider_downgrade", fixed = TRUE)
  expect_match(msg, "iso_sf$provider_downgrade <- FALSE", fixed = TRUE)
})
