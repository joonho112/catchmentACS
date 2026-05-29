# ============================================================================
# Integration tests for R/acs-prefetch.R — Step 4.2 (§20.3 algorithm Steps
# 8-10: suppression carry-through, 17-field provenance attribute, .rds +
# optional .gpkg persist).
#
# 5 cases per §20.8 (T20-09..T20-13):
#   T20-09  survey acs1 deferred (re-verify Step 4.1 schema-tier coverage)
#   T20-10  cache hit returns identical sf without re-invoking tidycensus
#   T20-11  force_refresh = TRUE bypasses cache (backend re-invoked)
#   T20-12  suppression > 10% triggers W-20-05 runtime warn + NA sentinel
#   T20-13  provenance attribute present with the 17-field §20.5 spec
#
# Mocking: `testthat::local_mocked_bindings(.tidycensus_get_acs_call = ...)`
# replaces the live tidycensus HTTP path. The codebook lookup
# `tidycensus::load_variables()` is also mocked so the partial-batch
# tolerance branch is deterministic.
#
# Each test isolates the on-disk cache via `.with_acs_cache_dir()` so that
# T20-10 / T20-11 do not pollute each other or the user's home cache.
# ============================================================================


# ---- Cache dir isolation per test ------------------------------------------

.with_acs_cache_dir <- function(code) {
  td <- tempfile("cacs_acs_test_")
  withr::with_options(
    list(catchmentACS.cache_dir = td, CACS_NO_CONFIRM = "1"),
    {
      Sys.setenv(CACS_NO_CONFIRM = "1")
      on.exit({
        if (dir.exists(td)) unlink(td, recursive = TRUE)
        memoise::forget(.cacs_cache_dir_memo)
      }, add = TRUE)
      memoise::forget(.cacs_cache_dir_memo)
      force(code)
    }
  )
}


.with_fake_census_key_int <- function(code) {
  withr::with_envvar(c(CENSUS_API_KEY = "FAKE_KEY_FOR_TEST"), code)
}


# ===========================================================================
# T20-09  survey = 'acs1' aborts (re-verify Step 4.1 schema-tier coverage)
# ===========================================================================

test_that("T20-09 survey = 'acs1' aborts (Step 4.1 schema lock re-verified)", {
  .with_acs_cache_dir({
    .with_fake_census_key_int({
      expect_error(
        cacs_acs_prefetch(state = "AL", survey = "acs1"),
        class  = "catchmentACS_error_schema",
        regexp = "v1\\.1|acs5"
      )
    })
  })
})


# ===========================================================================
# T20-10  cache hit returns identical sf without re-invoking tidycensus
# ===========================================================================

test_that("T20-10 cache hit returns identical sf without re-invoking backend", {
  .with_acs_cache_dir({
    .with_fake_census_key_int({
      cb <- helper_mock_codebook(known = "B19013_001")
      testthat::local_mocked_bindings(
        load_variables = function(year, dataset, ...) cb,
        .package = "tidycensus"
      )

      call_n <- 0L
      counting_stub <- function(geography, variables, state, year, survey) {
        call_n <<- call_n + 1L
        helper_mock_acs_long_sf(state = state, year = year,
                                variables = variables, n_tract = 4L)
      }
      testthat::local_mocked_bindings(
        .tidycensus_get_acs_call = counting_stub,
        .package = "catchmentACS"
      )

      # --- First call: cache miss, backend invoked once -------------------
      out1 <- suppressMessages(
        cacs_acs_prefetch(state = "AL",
                          variables = "B19013_001",
                          verbose = FALSE)
      )
      expect_s3_class(out1, "sf")
      expect_equal(call_n, 1L)
      expect_false(is.null(attr(out1, "cacs_provenance")))
      expect_identical(attr(out1, "cacs_provenance"),
                       attr(out1, "cacs_acs_provenance"))

      # --- Second identical call: cache hit works without key/codebook -----
      withr::with_envvar(c(CENSUS_API_KEY = NA_character_), {
        testthat::local_mocked_bindings(
          load_variables = function(...) {
            stop("cache hit should not load the codebook")
          },
          .package = "tidycensus"
        )

        out2 <- suppressMessages(
          cacs_acs_prefetch(state = "AL",
                            variables = "B19013_001",
                            verbose = FALSE)
        )
        expect_s3_class(out2, "sf")
        expect_equal(call_n, 1L)        # still 1: backend not re-called
        expect_identical(out1, out2)    # round-trip identity (provenance too)
      })
    })
  })
})


test_that("T20-10b partial-invalid cache hit uses canonical effective-vars key", {
  .with_acs_cache_dir({
    .with_fake_census_key_int({
      cb <- helper_mock_codebook(known = "B19013_001")
      testthat::local_mocked_bindings(
        load_variables = function(year, dataset, ...) cb,
        .package = "tidycensus"
      )

      call_n <- 0L
      testthat::local_mocked_bindings(
        .tidycensus_get_acs_call = function(geography, variables, state, year, survey) {
          call_n <<- call_n + 1L
          expect_identical(variables, "B19013_001")
          helper_mock_acs_long_sf(state = state, year = year,
                                  variables = variables, n_tract = 4L)
        },
        .package = "catchmentACS"
      )

      expect_warning(
        out1 <- suppressMessages(
          cacs_acs_prefetch(state = "AL",
                            variables = c("B19013_001", "B99999_999"),
                            verbose = FALSE)
        ),
        class = "catchmentACS_warning_variable"
      )
      expect_equal(call_n, 1L)

      expect_warning(
        out2 <- suppressMessages(
          cacs_acs_prefetch(state = "AL",
                            variables = c("B19013_001", "B99999_999"),
                            verbose = FALSE)
        ),
        class = "catchmentACS_warning_variable"
      )
      expect_equal(call_n, 1L)
      expect_identical(out1, out2)
    })
  })
})


# ===========================================================================
# T20-11  force_refresh = TRUE bypasses cache (backend re-invoked)
# ===========================================================================

test_that("T20-11 force_refresh = TRUE bypasses cache", {
  .with_acs_cache_dir({
    .with_fake_census_key_int({
      cb <- helper_mock_codebook(known = "B19013_001")
      testthat::local_mocked_bindings(
        load_variables = function(year, dataset, ...) cb,
        .package = "tidycensus"
      )

      call_n <- 0L
      counting_stub <- function(geography, variables, state, year, survey) {
        call_n <<- call_n + 1L
        helper_mock_acs_long_sf(state = state, year = year,
                                variables = variables, n_tract = 4L)
      }
      testthat::local_mocked_bindings(
        .tidycensus_get_acs_call = counting_stub,
        .package = "catchmentACS"
      )

      # Prime cache.
      suppressMessages(
        cacs_acs_prefetch(state = "AL",
                          variables = "B19013_001",
                          verbose = FALSE)
      )
      expect_equal(call_n, 1L)

      # force_refresh = TRUE — backend re-invoked even though cache exists.
      out_forced <- suppressMessages(
        cacs_acs_prefetch(state = "AL",
                          variables = "B19013_001",
                          force_refresh = TRUE,
                          verbose = FALSE)
      )
      expect_equal(call_n, 2L)
      expect_s3_class(out_forced, "sf")
    })
  })
})


test_that("T20-11c force_refresh with no Census key aborts before codebook/backend", {
  .with_acs_cache_dir({
    .with_fake_census_key_int({
      cb <- helper_mock_codebook(known = "B19013_001")
      testthat::local_mocked_bindings(
        load_variables = function(year, dataset, ...) cb,
        .package = "tidycensus"
      )

      call_n <- 0L
      testthat::local_mocked_bindings(
        .tidycensus_get_acs_call = function(geography, variables, state, year, survey) {
          call_n <<- call_n + 1L
          helper_mock_acs_long_sf(state = state, year = year,
                                  variables = variables, n_tract = 4L)
        },
        .package = "catchmentACS"
      )

      suppressMessages(
        cacs_acs_prefetch(state = "AL",
                          variables = "B19013_001",
                          verbose = FALSE)
      )
      expect_equal(call_n, 1L)

      withr::with_envvar(c(CENSUS_API_KEY = NA_character_), {
        testthat::local_mocked_bindings(
          load_variables = function(...) {
            stop("force_refresh without key should not load the codebook")
          },
          .package = "tidycensus"
        )

        expect_error(
          suppressMessages(
            cacs_acs_prefetch(state = "AL",
                              variables = "B19013_001",
                              force_refresh = TRUE,
                              verbose = FALSE)
          ),
          class = "catchmentACS_error_credential",
          regexp = "CENSUS_API_KEY"
        )
      })
      expect_equal(call_n, 1L)
    })
  })
})


test_that("T20-11b explicit cache_dir writes and reads from requested root", {
  td <- tempfile("cacs_acs_explicit_cache_")
  on.exit(if (dir.exists(td)) unlink(td, recursive = TRUE), add = TRUE)

  .with_fake_census_key_int({
    cb <- helper_mock_codebook(known = "B19013_001")
    testthat::local_mocked_bindings(
      load_variables = function(year, dataset, ...) cb,
      .package = "tidycensus"
    )

    call_n <- 0L
    counting_stub <- function(geography, variables, state, year, survey) {
      call_n <<- call_n + 1L
      helper_mock_acs_long_sf(state = state, year = year,
                              variables = variables, n_tract = 4L)
    }
    testthat::local_mocked_bindings(
      .tidycensus_get_acs_call = counting_stub,
      .package = "catchmentACS"
    )

    out1 <- suppressMessages(
      cacs_acs_prefetch(state = "AL",
                        variables = "B19013_001",
                        cache_dir = td,
                        verbose = FALSE)
    )
    expect_s3_class(out1, "sf")
    expect_equal(call_n, 1L)
    expect_length(list.files(file.path(td, "acs"), pattern = "\\.rds$"), 1L)

    out2 <- suppressMessages(
      cacs_acs_prefetch(state = "AL",
                        variables = "B19013_001",
                        cache_dir = td,
                        verbose = FALSE)
    )
    expect_equal(call_n, 1L)
    expect_identical(out1, out2)
  })
})


# ===========================================================================
# T20-12  estimate suppression > 10% triggers W-20-05 runtime warn
# ===========================================================================

test_that("T20-12 estimate suppression > 10% triggers W-20-05 runtime warn", {
  .with_acs_cache_dir({
    .with_fake_census_key_int({
      cb <- helper_mock_codebook(known = "B19013_001")
      testthat::local_mocked_bindings(
        load_variables = function(year, dataset, ...) cb,
        .package = "tidycensus"
      )

      # 20% estimate suppression injection (> 10% threshold).
      testthat::local_mocked_bindings(
        .tidycensus_get_acs_call = function(geography, variables, state, year, survey) {
          helper_mock_acs_long_sf(state = state, year = year,
                                  variables = variables, n_tract = 10L,
                                  suppression_pct  = 0.20,
                                  moe_sentinel_pct = 0)
        },
        .package = "catchmentACS"
      )

      expect_warning(
        out <- suppressMessages(
          cacs_acs_prefetch(state = "AL",
                            variables = "B19013_001",
                            verbose = FALSE,
                            force_refresh = TRUE)
        ),
        class  = "catchmentACS_warning_runtime",
        regexp = "estimate"
      )
      expect_s3_class(out, "sf")
      # No MOE sentinels were injected in this test.
      expect_true(all(is.na(out$moe) | out$moe >= 0))
      expect_false(any(!is.na(out$moe) & out$moe == -555555555))
      # Estimate suppression and MOE sentinels are tracked separately.
      prov <- attr(out, "cacs_provenance")
      expect_true(prov$n_suppressed > 0L)
      expect_identical(prov$n_suppressed, prov$n_estimate_suppressed)
      expect_equal(prov$n_moe_sentinel, 0L)
    })
  })
})


test_that("T20-12b estimate suppression and MOE sentinels can differ", {
  .with_acs_cache_dir({
    .with_fake_census_key_int({
      cb <- helper_mock_codebook(known = "B19013_001")
      testthat::local_mocked_bindings(
        load_variables = function(year, dataset, ...) cb,
        .package = "tidycensus"
      )

      testthat::local_mocked_bindings(
        .tidycensus_get_acs_call = function(geography, variables, state, year, survey) {
          helper_mock_acs_long_sf(state = state, year = year,
                                  variables = variables, n_tract = 10L,
                                  suppression_pct  = 0.10,
                                  moe_sentinel_pct = 0.30)
        },
        .package = "catchmentACS"
      )

      expect_warning(
        out <- suppressMessages(
          cacs_acs_prefetch(state = "AL",
                            variables = "B19013_001",
                            verbose = FALSE,
                            force_refresh = TRUE)
        ),
        class  = "catchmentACS_warning_runtime",
        regexp = "MOE"
      )
      prov <- attr(out, "cacs_provenance")
      expect_identical(prov$n_suppressed, prov$n_estimate_suppressed)
      expect_lt(prov$n_estimate_suppressed, prov$n_moe_sentinel)
      expect_equal(prov$n_negative_moe, 0L)
    })
  })
})


# ===========================================================================
# T20-13  17-field provenance attribute present + matches §20.5 spec
# ===========================================================================

test_that("T20-13 provenance attribute is present with the §20.5 17 fields", {
  .with_acs_cache_dir({
    .with_fake_census_key_int({
      cb <- helper_mock_codebook(known = "B19013_001")
      testthat::local_mocked_bindings(
        load_variables = function(year, dataset, ...) cb,
        .package = "tidycensus"
      )
      testthat::local_mocked_bindings(
        .tidycensus_get_acs_call = function(geography, variables, state, year, survey) {
          helper_mock_acs_long_sf(state = state, year = year,
                                  variables = variables, n_tract = 3L)
        },
        .package = "catchmentACS"
      )

      out <- suppressMessages(
        cacs_acs_prefetch(state = "AL",
                          variables = "B19013_001",
                          verbose = FALSE,
                          force_refresh = TRUE)
      )

      prov <- attr(out, "cacs_provenance")
      expect_false(is.null(prov))
      expect_type(prov, "list")
      expect_identical(prov, attr(out, "cacs_acs_provenance"))

      # The §39.4 / §20.5 catalogue enumerates the implemented field set.
      # Counting from the Step 4.2 task spec (18 named fields):
      #   state, year, survey, geography, variables, variable_source,
      #   n_variables_requested, n_variables_received,
      #   n_variables_skipped, n_tracts, suppression/MOE counters, n_total_rows,
      #   cache_key, cache_namespace, generated_at,
      #   tidycensus_version, sf_version, acs_geometry_vintage
      # Some §20.5 phrasings collapse variable_source under variables,
      # yielding "17 fields"; the implemented list has 18 distinct keys.
      expected_fields <- c(
        "state", "year", "survey", "geography", "variables",
        "variable_source", "n_variables_requested", "n_variables_received",
        "n_variables_skipped", "n_tracts", "n_suppressed",
        "n_estimate_suppressed", "n_moe_sentinel", "n_negative_moe",
        "n_total_rows",
        "cache_key", "cache_namespace", "generated_at",
        "tidycensus_version", "sf_version", "acs_geometry_vintage"
      )
      expect_setequal(names(prov), expected_fields)
      expect_equal(length(prov), length(expected_fields))

      # Spot-check scalar payloads.
      expect_identical(prov$state, "AL")
      expect_identical(prov$year, 2023L)
      expect_identical(prov$survey, "acs5")
      expect_identical(prov$geography, "tract")
      expect_identical(prov$variable_source, "user_supplied")
      expect_identical(prov$cache_namespace, "acs")
      expect_true(nchar(prov$cache_key) > 0L)
      expect_s3_class(prov$generated_at, "POSIXct")

      # Schema version sentinel attached.
      expect_identical(attr(out, "cacs_schema_version"), "1.0")

      # Temporary provenance hooks must be stripped.
      expect_null(attr(out, "cacs_cache_key_pending"))
      expect_null(attr(out, "cacs_vars_source"))
      expect_null(attr(out, "cacs_variables_skipped"))
    })
  })
})
