# ============================================================================
# Unit tests for cacs_run() Step 7.1 / 7.2 pre-dispatch + boundary surface.
#
# Step 7.1 implements Sec. 24.3 steps 1-4 (thin arg validation + execution
# path determination + sub-call arg-list validation + warning collector
# init). Step 7.2 wires steps 5-11 (5-call orchestrator + partial-success
# NA propagation) on top. The only remaining boundary abort is Step 12
# (output dispatch, deferred to Step 7.3) — successful traversal of
# steps 1-11 lands on the long-format/list-column output surface. These
# tests exercise that boundary plus narrow argument-forwarding regressions:
#
#   T24-08 - thin validation success path reaches the Step 12 boundary
#            (catchmentACS_error_operator) via mocked sub-calls.
#   T24-08b - `weight_args` forwarding to intersection.
#   T24-08c - base data.frame `sites` traverses cacs_run -> cacs_isochrone.
#   T24-12 - invalid `output` enum -> match.arg() abort.
#   T24-14 - unknown key in `moe_args` -> W-24-01 warn + drop (validation
#            still succeeds and the call falls through to the boundary).
#   T24-15 - `moe_args$formula` reserved -> E-24-13 abort
#            (catchmentACS_error_schema), *no* warning emitted.
#
# Cross-ref: Sec. 24.2 (21-arg signature), Sec. 24.3 (12-step algorithm),
# Sec. 24.4 (.CACS_RUN_ARG_KEYS), Sec. 24.9 (error / warning catalog),
# Sec. 42.3 (Step 7.1 spec), Sec. 42.4 (Step 7.2 spec).
# ============================================================================


# ---- Shared fixtures ------------------------------------------------------
#
# `.mk_run_sites_local()` returns a minimal 1-row tibble matching the
# `sites` contract (`site_id`, `lon`, `lat`). The point coords here do not
# matter for Step 7.1 because the function aborts at the pre-dispatch
# boundary before provider geometry is processed.

.mk_run_sites_local <- function(n = 1L) {
  tibble::tibble(
    site_id = paste0("S", sprintf("%02d", seq_len(n))),
    lon     = rep(-86.80902, n),
    lat     = rep( 33.52203, n)
  )
}


# ---- T24-08 thin validation success ---------------------------------------
#
# When every argument is well-typed and the 5 sub-calls produce well-shaped
# fixture data, `cacs_run()` should traverse pre-dispatch + steps 5-11
# cleanly and abort with the Step 7.2 boundary message ("step 12"). That
# abort carries `catchmentACS_error_operator` (not schema), so we can
# distinguish "validation + orchestration passed" from "validation failed"
# by condition class. We also expect the M-24-01 execution-path message to
# fire as a side effect.
#
# Sub-call stubs share shape with test-integration-run.R T24-01..T24-07 so
# we are exercising the same orchestrator surface but at the unit-test
# layer (filter = "run-args" still uses these stubs).

.mk_run_iso_stub_local <- function(site_ids = "S01") {
  rows <- expand.grid(site_id        = site_ids,
                      drive_time_min = c(5L, 10L, 15L),
                      KEEP.OUT.ATTRS   = FALSE,
                      stringsAsFactors = FALSE)
  geom_list <- lapply(seq_len(nrow(rows)), function(i) {
    sf::st_polygon(list(rbind(
      c(-86.81, 33.52), c(-86.80, 33.52),
      c(-86.80, 33.53), c(-86.81, 33.53), c(-86.81, 33.52)
    )))
  })
  sf::st_sf(
    tibble::tibble(
      site_id                          = rows$site_id,
      drive_time_min                   = as.integer(rows$drive_time_min),
      provider                         = "osrm",
      profile                          = "car",
      osm_snapshot_date                = NA_character_,
      routing_engine_version           = "stub/0.0.0",
      polygon_simplification_tolerance = NA_real_,
      generated_at                     = as.POSIXct("2026-05-22 00:00:00",
                                                     tz = "UTC"),
      isochrone_empty                  = FALSE,
      provider_requested               = "osrm",
      provider_downgrade               = FALSE,
      osm_snapshot_status              = NA_character_,
      failure_reason                   = NA_character_,
      retry_count                      = 1L,
      ring_topology                    = "cumulative"
    ),
    geometry = sf::st_sfc(geom_list, crs = 4326)
  )
}

.mk_run_long_stub_local <- function(site_ids    = "S01",
                                     drive_times = c(5L, 10L, 15L)) {
  rate_names <- names(catchmentACS:::.SANCTIONED_RATES_V1)
  vars <- c("B17001_002", rate_names)
  rows <- expand.grid(site_id        = site_ids,
                      drive_time_min = drive_times,
                      variable       = vars,
                      KEEP.OUT.ATTRS   = FALSE,
                      stringsAsFactors = FALSE)
  n <- nrow(rows)
  tibble::tibble(
    site_id                       = rows$site_id,
    drive_time_min                = as.integer(rows$drive_time_min),
    ring_topology                 = "cumulative",
    variable                      = rows$variable,
    estimate                      = as.numeric(seq_len(n)),
    moe                           = as.numeric(seq_len(n)) * 0.1,
    se                            = as.numeric(seq_len(n)) * 0.05,
    weight_sum                    = rep(0.95, n),
    n_tracts                      = rep(2L,   n),
    n_tracts_num                  = ifelse(rows$variable %in% rate_names,
                                           2L, NA_integer_),
    n_tracts_den                  = ifelse(rows$variable %in% rate_names,
                                           2L, NA_integer_),
    provider                      = "osrm",
    profile                       = "car",
    osm_snapshot_date             = NA_character_,
    acs_year                      = 2023L,
    weight_method                 = "area",
    estimand_family               = ifelse(rows$variable %in% rate_names,
                                           "derived_rate", "spatial_total"),
    weight_basis                  = "coverage",
    moe_formula_requested         = "auto",
    moe_formula_effective         = "weighted_sum",
    moe_fallback                  = FALSE,
    moe_fallback_reason           = NA_character_,
    failure_origin                = NA_character_,
    weight_uncertainty_propagated = FALSE
  )
}

.with_run_stubs_local <- function(code, weight_arg_recorder = NULL) {
  acs_stub_long <- sf::st_sf(
    tibble::tibble(GEOID = "01001000100", NAME = "Tract", variable = "B17001_002",
                   estimate = 100, moe = 10),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-86.85, 33.50), c(-86.75, 33.50),
        c(-86.75, 33.55), c(-86.85, 33.55), c(-86.85, 33.50)
      ))),
      crs = 4269
    )
  )

  testthat::local_mocked_bindings(
    cacs_acs_prefetch     = function(...) acs_stub_long,
    cacs_isochrone        = function(sites, ...) {
      .mk_run_iso_stub_local(site_ids = unique(sites$site_id))
    },
    cacs_intersect_weight = function(iso_sf, ...) {
      if (!is.null(weight_arg_recorder)) {
        weight_arg_recorder$args <- list(...)
      }
      out <- .mk_run_long_stub_local(
        site_ids = unique(iso_sf$site_id),
        drive_times = sort(unique(iso_sf$drive_time_min))
      )
      attr(out, "cacs_aggregation_carriers") <- tibble::tibble()
      out
    },
    cacs_propagate_moe    = function(data, ...) data,
    cacs_derive_rates     = function(weighted_acs, ...) {
      .mk_run_long_stub_local(
        site_ids = unique(weighted_acs$site_id),
        drive_times = sort(unique(weighted_acs$drive_time_min))
      )
    },
    .package = "catchmentACS",
    .env = parent.frame()
  )
  force(code)
}

test_that("T24-08 thin validation succeeds and returns a long-format tibble", {
  sites <- .mk_run_sites_local()

  .with_run_stubs_local({
    out <- suppressMessages(suppressWarnings(
      cacs_run(sites = sites, state = "AL", verbose = FALSE)
    ))

    # Step 7.3: the 5-call path now returns a long tibble carrying the
    # 4 run-level attributes — no more operator-class boundary abort.
    expect_s3_class(out, "tbl_df")
    expect_equal(attr(out, "cacs_schema_version"), "1.0")
    expect_equal(attr(out, "cacs_run_provenance")$execution_path, "5-call")

    # M-24-01 execution-path message lands when verbose = TRUE.
    expect_message(
      suppressWarnings(
        cacs_run(sites = sites, state = "AL", verbose = TRUE)
      ),
      "5-call",
      class = "catchmentACS_message_progress"
    )

    # 3-call path with bypass args supplied also succeeds (and does NOT
    # trip E-24-15 because the bypass iso is a valid pair).
    iso_in <- .mk_run_iso_stub_local()
    acs_in <- sf::st_sf(
      tibble::tibble(GEOID = "01001000100", NAME = "Tract",
                     variable = "B17001_002", estimate = 100, moe = 10),
      geometry = sf::st_sfc(
        sf::st_polygon(list(rbind(
          c(-86.85, 33.50), c(-86.75, 33.50),
          c(-86.75, 33.55), c(-86.85, 33.55), c(-86.85, 33.50)
        ))),
        crs = 4269
      )
    )
    out3 <- suppressMessages(suppressWarnings(
      cacs_run(sites = sites, state = "AL",
               precomputed_isochrones = iso_in, acs = acs_in,
               verbose = FALSE)
    ))
    expect_s3_class(out3, "tbl_df")
    expect_equal(attr(out3, "cacs_run_provenance")$execution_path, "3-call")
  })
})


test_that("T24-08b weight_args forwards keep_tract_audit to intersection", {
  sites <- .mk_run_sites_local()
  recorder <- new.env(parent = emptyenv())

  .with_run_stubs_local({
    out <- suppressMessages(suppressWarnings(
      cacs_run(
        sites = sites,
        state = "AL",
        weight_args = list(min_weight = 0.02, keep_tract_audit = TRUE),
        verbose = FALSE
      )
    ))

    expect_s3_class(out, "tbl_df")
    expect_true(isTRUE(recorder$args$keep_tract_audit))
    expect_equal(recorder$args$min_weight, 0.02)
    expect_false(isTRUE(recorder$args$verbose))
  }, weight_arg_recorder = recorder)
})

test_that("T24-08c base data.frame sites traverse cacs_run 5-call path", {
  sites <- as.data.frame(.mk_run_sites_local())
  seen_sites <- NULL
  td <- tempfile("cacs_run_df_sites_")
  on.exit(if (dir.exists(td)) unlink(td, recursive = TRUE), add = TRUE)
  acs_stub_long <- sf::st_sf(
    tibble::tibble(GEOID = "01001000100", NAME = "Tract",
                   variable = "B17001_002", estimate = 100, moe = 10),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-86.85, 33.50), c(-86.75, 33.50),
        c(-86.75, 33.55), c(-86.85, 33.55), c(-86.85, 33.50)
      ))),
      crs = 4269
    )
  )

  testthat::local_mocked_bindings(
    cacs_acs_prefetch = function(...) acs_stub_long,
    .iso_via_osrm = function(sites, drive_times, profile, ...) {
      seen_sites <<- sites
      .mk_run_iso_stub_local(site_ids = as.character(sites$site_id))
    },
    cacs_intersect_weight = function(iso_sf, ...) {
      out <- .mk_run_long_stub_local(
        site_ids = unique(iso_sf$site_id),
        drive_times = sort(unique(iso_sf$drive_time_min))
      )
      attr(out, "cacs_aggregation_carriers") <- tibble::tibble()
      out
    },
    cacs_propagate_moe = function(data, ...) data,
    cacs_derive_rates = function(weighted_acs, ...) {
      .mk_run_long_stub_local(
        site_ids = unique(weighted_acs$site_id),
        drive_times = sort(unique(weighted_acs$drive_time_min))
      )
    },
    .package = "catchmentACS"
  )

  out <- suppressMessages(suppressWarnings(
    cacs_run(sites = sites, state = "AL", cache_dir = td,
             iso_args = list(res = 30), verbose = FALSE)
  ))

  expect_s3_class(seen_sites, "sf")
  expect_equal(as.character(seen_sites$site_id), sites$site_id)
  expect_s3_class(out, "tbl_df")
  expect_equal(attr(out, "cacs_run_provenance")$execution_path, "5-call")
})


# ---- T24-12 invalid output enum -------------------------------------------
#
# `match.arg(output)` rejects any string outside the 3-element enum
# `c("long", "list_column", "both")` with a base-R `simpleError`. The
# message lists the sanctioned values so the user can recover. (This abort
# class is NOT one of the catchmentACS_error_* families — match.arg's
# stock behavior is sufficient and matches the convention used by
# `cacs_isochrone(provider = ...)` and `cacs_intersect_weight(weight_method
# = ...)`.)

test_that("T24-12 invalid `output` enum aborts via match.arg", {
  sites <- .mk_run_sites_local()

  err <- tryCatch(
    cacs_run(sites = sites, state = "AL", output = "bogus", verbose = FALSE),
    error = function(e) e
  )

  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "'arg'", fixed = TRUE)
  expect_match(conditionMessage(err), "long")
  expect_match(conditionMessage(err), "list_column")
  expect_match(conditionMessage(err), "both")

  # Mirror checks for the other two `match.arg()`-guarded enums so a
  # future refactor does not silently break them.
  expect_error(
    cacs_run(sites = sites, state = "AL", provider = "bogus",
             verbose = FALSE),
    class = "error"
  )
  expect_error(
    cacs_run(sites = sites, state = "AL", weight_method = "bogus",
             verbose = FALSE),
    class = "error"
  )
})


# ---- T24-13 population weighting fail-loud --------------------------------
#
# Population weighting is a reserved future-release surface. It should abort
# before live ACS/provider work regardless of whether the caller supplies a
# non-NULL `bg_pop_sf` placeholder.

test_that("T24-13 weight_method = 'population' aborts before provider work", {
  sites <- .mk_run_sites_local()

  err <- tryCatch(
    cacs_run(
      sites = sites,
      state = "AL",
      weight_method = "population",
      bg_pop_sf = structure(list(), class = c("sf", "data.frame")),
      verbose = FALSE
    ),
    error = function(e) e
  )

  expect_s3_class(err, "catchmentACS_error_credential")
  expect_match(conditionMessage(err), "population", fixed = TRUE)
  expect_match(conditionMessage(err), "future release", fixed = TRUE)
  expect_match(conditionMessage(err), "no provider or ACS request", fixed = TRUE)
})


# ---- T24-14 unknown key in moe_args ---------------------------------------
#
# `cacs_run(..., moe_args = list(bogus_arg = 1))` triggers a single
# W-24-01 `catchmentACS_warning_provenance` warning that lists the unknown
# key, advises on the sanctioned set, and is dropped before forward. After
# the warning fires, validation continues and the call returns the Step 7.3
# long tibble via the mocked sub-calls.

test_that("T24-14 unknown moe_args key warns (W-24-01) and is dropped", {
  sites <- .mk_run_sites_local()

  .with_run_stubs_local({
    # withCallingHandlers so we observe both the warning AND the eventual
    # abort. testthat::expect_warning() with `class=` filters the warning by
    # condition class; the trailing `error = function(e) NULL` swallows the
    # Step 7.2 boundary abort so it does not surface as a test failure.
    expect_warning(
      tryCatch(
        cacs_run(sites = sites, state = "AL",
                 moe_args = list(bogus_arg = 1), verbose = FALSE),
        error = function(e) NULL
      ),
      "bogus_arg",
      class = "catchmentACS_warning_provenance"
    )
  })

  # The same call wrapped in tryCatch(warning=) returns the warning
  # condition so we can inspect its message + classes. The W-24-01 fires
  # in Step 3 (before any sub-call), so no stubs are needed for the
  # message-inspection branch — tryCatch(warning=) captures the *first*
  # warning and short-circuits the rest of the call.
  w <- tryCatch(
    cacs_run(sites = sites, state = "AL",
             moe_args = list(bogus_arg = 1), verbose = FALSE),
    warning = function(w) w
  )
  expect_s3_class(w, "catchmentACS_warning_provenance")
  expect_match(conditionMessage(w), "bogus_arg")
  # Sanctioned keys for moe (`.CACS_RUN_ARG_KEYS$moe`) are listed so the
  # user can recover.
  expect_match(conditionMessage(w), "level")
  expect_match(conditionMessage(w), "fallback_chain_max")
})


# ---- T24-15 reserved formula key in moe_args ------------------------------
#
# Per the Sec. 24.4 design decision: `moe_args$formula` and
# `rate_args$formula_dispatch` are the 2 reserved keys that *abort* rather
# than warn-and-drop, because forwarding them would let two competing
# values exist in the same call (the user-supplied `moe_args$formula` and
# the top-level `moe_formula`). The abort carries
# `catchmentACS_error_schema` (E-24-13) and points the user at the
# top-level public surface.

test_that("T24-15 moe_args$formula aborts with E-24-13 (schema)", {
  sites <- .mk_run_sites_local()

  err <- tryCatch(
    cacs_run(sites = sites, state = "AL",
             moe_args = list(formula = "weighted_sum"), verbose = FALSE),
    error = function(e) e
  )

  expect_s3_class(err, "catchmentACS_error_schema")
  expect_match(conditionMessage(err), "moe_args$formula", fixed = TRUE)
  expect_match(conditionMessage(err), "moe_formula", fixed = TRUE)

  # The companion rate_args$formula_dispatch reservation aborts the same
  # way (mirror check so a future signature drift does not silently break
  # the lock).
  err2 <- tryCatch(
    cacs_run(sites = sites, state = "AL",
             rate_args = list(formula_dispatch = "auto"), verbose = FALSE),
    error = function(e) e
  )
  expect_s3_class(err2, "catchmentACS_error_schema")
  expect_match(conditionMessage(err2), "rate_args$formula_dispatch",
               fixed = TRUE)
  expect_match(conditionMessage(err2), "formula_dispatch", fixed = TRUE)
})
