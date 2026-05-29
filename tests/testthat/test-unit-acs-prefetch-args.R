# ============================================================================
# Unit tests for R/acs-prefetch.R — Step 4.1 (§20.3 algorithm Steps 1-7).
#
# Core cases per §20.8 (T20-01..T20-08 + T20-10):
#   T20-01 state lowercase            → E-20-04 schema abort + "uppercase" msg
#   T20-02 state vector (scalar lock)  → E-20-02 schema abort + "cacs_run" hint
#   T20-03 state unknown FIPS         → E-20-05 schema abort
#   T20-04 survey acs1                → schema abort + v1.1 hint
#   T20-05 geography "block group"    → schema abort + v0.2 hint
#   T20-06 variables NULL             → uses cacs_acs_default_vars (14 vars)
#   T20-07 variables malformed codes  → E-20-06 schema abort
#   T20-08 variables all invalid      → E-20-07 variable abort
#   T20-10 variables partial invalid  → W-20-04 variable warn + skip
#
# A 10th sanity test confirms the §20.2 frozen 9-arg signature so a future
# refactor cannot silently widen / reorder the public surface.
#
# Mocking: the tidycensus HTTP path is replaced via
# `testthat::local_mocked_bindings(.tidycensus_get_acs_call = ...)`. The
# codebook lookup `tidycensus::load_variables()` may be reached when an
# internet codebook cache is present; tests that exercise variable
# resolution stub `tidycensus::load_variables` to a deterministic value.
# ============================================================================


# ---------------------------------------------------------------------------
# Helpers (helper_mock_acs_long_sf / helper_mock_codebook) live in
# tests/testthat/helper-mocks.R so they are sourced automatically by testthat.
# This file only adds a CENSUS_API_KEY shim.
# ---------------------------------------------------------------------------

.with_fake_census_key <- function(code) {
  withr::with_envvar(c(CENSUS_API_KEY = "FAKE_KEY_FOR_TEST"), code)
}


# ===========================================================================
# T20-00 Signature: 10 args in canonical order (§20.2 frozen; v0.2 widened
# from 9 to 10 to add `drop_water_tracts` for BUG-001 (F3) hybrid filter
# per the validation contract).
# ===========================================================================

test_that("cacs_acs_prefetch() formal signature is frozen at 10 args (v0.2)", {
  expect_identical(
    names(formals(cacs_acs_prefetch)),
    c("state", "year", "variables", "survey", "geography",
      "cache_dir", "write_gpkg", "force_refresh",
      "drop_water_tracts", "verbose")
  )
  # `drop_water_tracts` defaults to TRUE (v0.2 BUG-001 (F3) auto-filter on
  # by default; users can opt out by passing FALSE).
  expect_identical(
    eval(formals(cacs_acs_prefetch)$drop_water_tracts),
    TRUE
  )
})


# ===========================================================================
# T20-01 state lowercase → schema abort, message mentions "uppercase"
# ===========================================================================

test_that("T20-01 state = 'al' aborts with schema family + 'uppercase' msg", {
  .with_fake_census_key({
    expect_error(
      cacs_acs_prefetch(state = "al"),
      class  = "catchmentACS_error_schema",
      regexp = "uppercase"
    )
  })
})


# ===========================================================================
# T20-02 state vector → schema abort + scalar-lock orchestrator hint
# ===========================================================================

test_that("T20-02 state = c('AL','MS') aborts with scalar lock + cacs_run hint", {
  .with_fake_census_key({
    expect_error(
      cacs_acs_prefetch(state = c("AL", "MS")),
      class  = "catchmentACS_error_schema",
      regexp = "cacs_run"
    )
  })
})


# ===========================================================================
# T20-03 unknown state ('XX') → schema abort
# ===========================================================================

test_that("T20-03 state = 'XX' (unknown FIPS) aborts with schema family", {
  .with_fake_census_key({
    expect_error(
      cacs_acs_prefetch(state = "XX"),
      class = "catchmentACS_error_schema"
    )
  })
})


# ===========================================================================
# T20-04 survey = 'acs1' → schema abort + v1.1 deferral hint
# ===========================================================================

test_that("T20-04 survey = 'acs1' aborts with v1.1 deferral hint", {
  .with_fake_census_key({
    expect_error(
      cacs_acs_prefetch(state = "AL", survey = "acs1"),
      class  = "catchmentACS_error_schema",
      regexp = "v1\\.1|acs5"
    )
  })
})


# ===========================================================================
# T20-05 geography = 'block group' → schema abort + v0.2 deferral hint
# ===========================================================================

test_that("T20-05 geography = 'block group' aborts with deferral hint", {
  .with_fake_census_key({
    expect_error(
      cacs_acs_prefetch(state = "AL", geography = "block group"),
      class  = "catchmentACS_error_schema",
      regexp = "v0\\.2|tract"
    )
  })
})


# ===========================================================================
# T20-05b..e year policy → release-pinned ACS5 range + integer guard
# ===========================================================================

test_that("T20-05b ACS5 year range helper is pinned for v0.5.0", {
  expect_identical(.cacs_acs5_year_range(), c(min = 2009L, max = 2024L))
})


test_that("T20-05c years outside release-verified ACS5 range abort", {
  for (bad_year in c(2008L, 2025L, 2026L, 2027L)) {
    expect_error(
      cacs_acs_prefetch(state = "AL", year = bad_year),
      class = "catchmentACS_error_schema",
      regexp = "2009.*2024"
    )
  }
})


test_that("T20-05d fractional ACS year aborts before integer truncation", {
  expect_error(
    cacs_acs_prefetch(state = "AL", year = 2023.5),
    class = "catchmentACS_error_schema",
    regexp = "integer"
  )
})


test_that("T20-05e ACS5 boundary years 2009 and 2024 succeed under deterministic mocks", {
  .with_fake_census_key({
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

    for (yr in c(2009L, 2024L)) {
      out <- suppressMessages(
        cacs_acs_prefetch(state = "AL", year = yr,
                          variables = "B19013_001",
                          verbose = FALSE, force_refresh = TRUE)
      )
      expect_s3_class(out, "sf")
      expect_identical(attr(out, "cacs_provenance")$year, yr)
    }
  })
})


# ===========================================================================
# T20-06 variables = NULL → uses cacs_acs_default_vars (14 vars),
#                            cache_payload vars sorted/unique, success path
# ===========================================================================

test_that("T20-06 variables = NULL falls back to cacs_acs_default_vars (14)", {
  .with_fake_census_key({
    # Stub the codebook to accept all default vars
    e <- new.env(parent = emptyenv())
    utils::data("cacs_acs_default_vars", package = "catchmentACS", envir = e)
    cb <- helper_mock_codebook(known = unname(e$cacs_acs_default_vars))

    testthat::local_mocked_bindings(
      .tidycensus_get_acs_call = function(geography, variables, state, year, survey) {
        # The mock must echo the default variable request size.
        helper_mock_acs_long_sf(state = state, year = year,
                                variables = variables, n_tract = 3L)
      },
      .package = "catchmentACS"
    )
    testthat::local_mocked_bindings(
      load_variables = function(year, dataset, ...) cb,
      .package = "tidycensus"
    )

    out <- suppressMessages(
      cacs_acs_prefetch(state = "AL", variables = NULL,
                        verbose = FALSE, force_refresh = TRUE)
    )
    expect_s3_class(out, "sf")
    # Step 4.2 folds the provenance hooks into canonical provenance.
    prov <- attr(out, "cacs_provenance")
    expect_false(is.null(prov))
    expect_identical(prov, attr(out, "cacs_acs_provenance"))
    expect_identical(prov$variable_source, "default_catalogue")
    # 13 default vars all valid → n_variables_skipped == 0
    expect_identical(prov$n_variables_skipped, 0L)
    expect_equal(sf::st_crs(out)$epsg, 4269L)
  })
})


test_that("T20-06b codebook lookup does not pass deprecated load_variables cache arg", {
  .with_fake_census_key({
    cb <- helper_mock_codebook(known = "B19013_001")

    testthat::local_mocked_bindings(
      load_variables = function(year, dataset, ...) {
        expect_identical(list(...), list())
        cb
      },
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
      cacs_acs_prefetch(state = "AL", variables = "B19013_001",
                        verbose = FALSE, force_refresh = TRUE)
    )
    expect_s3_class(out, "sf")
  })
})


test_that("T20-06c cache miss without Census key aborts before codebook lookup", {
  td <- tempfile("cacs_acs_no_key_miss_")
  on.exit(if (dir.exists(td)) unlink(td, recursive = TRUE), add = TRUE)

  withr::with_envvar(c(CENSUS_API_KEY = NA_character_), {
    load_called <- FALSE
    get_called <- FALSE
    testthat::local_mocked_bindings(
      load_variables = function(...) {
        load_called <<- TRUE
        stop("codebook should not be called without a key")
      },
      .package = "tidycensus"
    )
    testthat::local_mocked_bindings(
      .tidycensus_get_acs_call = function(...) {
        get_called <<- TRUE
        stop("get_acs should not be called without a key")
      },
      .package = "catchmentACS"
    )

    expect_error(
      suppressMessages(
        cacs_acs_prefetch(state = "AL", variables = "B19013_001",
                          cache_dir = td, verbose = FALSE)
      ),
      class = "catchmentACS_error_credential",
      regexp = "CENSUS_API_KEY"
    )
    expect_false(load_called)
    expect_false(get_called)
  })
})


test_that("T20-06d codebook endpoint-not-found is surfaced as schema tier", {
  .with_fake_census_key({
    testthat::local_mocked_bindings(
      load_variables = function(...) {
        stop("API endpoint not found. Does this data set exist?")
      },
      .package = "tidycensus"
    )
    testthat::local_mocked_bindings(
      .tidycensus_get_acs_call = function(...) {
        stop("get_acs should not run after codebook endpoint failure")
      },
      .package = "catchmentACS"
    )

    expect_error(
      suppressMessages(
        cacs_acs_prefetch(state = "AL", year = 2024L,
                          variables = "B19013_001",
                          verbose = FALSE, force_refresh = TRUE)
      ),
      class = "catchmentACS_error_schema",
      regexp = "verified.*2024|codebook"
    )
  })
})


# ===========================================================================
# T20-07 variables = malformed codes → schema abort
# ===========================================================================

test_that("T20-07 malformed variables abort with schema family (E-20-06)", {
  .with_fake_census_key({
    expect_error(
      cacs_acs_prefetch(state = "AL",
                         variables = c("BOGUS_001", "B!BAD_002")),
      class = "catchmentACS_error_schema"
    )
  })
})


# ===========================================================================
# T20-08 variables = all-invalid-but-pattern-correct codes → variable abort
# ===========================================================================

test_that("T20-08 all-invalid variables abort with variable family (E-20-07)", {
  .with_fake_census_key({
    # Stub the codebook so the requested codes (pattern-valid but absent
    # in the codebook) trigger the all-invalid branch.
    cb <- helper_mock_codebook(known = "B19013_001")
    testthat::local_mocked_bindings(
      load_variables = function(year, dataset, ...) cb,
      .package = "tidycensus"
    )
    testthat::local_mocked_bindings(
      .tidycensus_get_acs_call = function(geography, variables, state, year, survey) {
        stop("Should never reach the API on all-invalid variables")
      },
      .package = "catchmentACS"
    )

    expect_error(
      cacs_acs_prefetch(state = "AL",
                         variables = c("B99999_999", "B88888_888")),
      class  = "catchmentACS_error_variable",
      regexp = "0 codes valid"
    )
  })
})


# ===========================================================================
# T20-10 partial invalid → warn (W-20-04) + skip + proceed
# ===========================================================================

test_that("T20-10 partial-invalid variables warn (W-20-04) + skip + proceed", {
  .with_fake_census_key({
    cb <- helper_mock_codebook(known = "B19013_001")
    testthat::local_mocked_bindings(
      load_variables = function(year, dataset, ...) cb,
      .package = "tidycensus"
    )
    testthat::local_mocked_bindings(
      .tidycensus_get_acs_call = function(geography, variables, state, year, survey) {
        # The mock should be called with variables_effective = "B19013_001" only.
        expect_identical(variables, "B19013_001")
        helper_mock_acs_long_sf(state = state, year = year,
                                variables = variables, n_tract = 4L)
      },
      .package = "catchmentACS"
    )

    expect_warning(
      out <- suppressMessages(
        cacs_acs_prefetch(state = "AL",
                          variables = c("B19013_001", "B99999_999"),
                          verbose = FALSE, force_refresh = TRUE)
      ),
      class = "catchmentACS_warning_variable"
    )
    expect_s3_class(out, "sf")
    # Step 4.2 folds provenance hooks into canonical provenance.
    prov <- attr(out, "cacs_provenance")
    expect_false(is.null(prov))
    expect_identical(prov, attr(out, "cacs_acs_provenance"))
    expect_identical(prov$n_variables_requested, 2L)
    expect_identical(prov$n_variables_received, 1L)
    expect_identical(prov$n_variables_skipped, 1L)
    expect_identical(prov$variable_source, "user_supplied")
    expect_identical(unname(prov$variables), "B19013_001")
    expect_identical(prov$cache_key,
                     bug010_acs_key(variables = "B19013_001"))
    expect_false(identical(
      prov$cache_key,
      bug010_acs_key(variables = c("B19013_001", "B99999_999"))
    ))
    expect_true(nrow(out) > 0L)
  })
})
