bug010_vars <- function(fx) {
  sort(unique(fx$acs_sf$variable))
}

bug010_acs_key <- function(state = "AL",
                           year = 2023L,
                           variables = bug010_vars(load_replay_fixture("cache_poison"))) {
  payload <- list(
    state = state,
    year = as.integer(year),
    survey = "acs5",
    geography = "tract",
    variables = sort(unique(variables)),
    geometry_vintage = .acs_geometry_vintage(year, "tract"),
    tidycensus_version = as.character(utils::packageVersion("tidycensus")),
    tigris_version = as.character(utils::packageVersion("tigris")),
    sf_version = as.character(utils::packageVersion("sf")),
    schema_version = "1.0",
    package_version = as.character(utils::packageVersion("catchmentACS")),
    r_version = paste(R.version$major, R.version$minor, sep = ".")
  )
  .cacs_cache_key(payload, "acs")
}

bug010_mock_codebook <- function(vars) {
  tibble::tibble(name = vars, label = vars, concept = "cache poison regression")
}

bug010_expect_acs_call <- function(geography, variables, state, year, survey, vars) {
  expect_identical(geography, "tract")
  expect_setequal(variables, vars)
  expect_identical(state, "AL")
  expect_identical(as.integer(year), 2023L)
  expect_identical(survey, "acs5")
}

test_that("BUG010-01 replay fixture locks the 39-row poison shape", {
  fx <- load_replay_fixture("cache_poison")
  expect_s3_class(fx$acs_sf, "sf")
  expect_equal(nrow(fx$acs_sf), fx$expected_poison_acs_rows)
  expect_equal(length(unique(fx$acs_sf$GEOID)), 3L)
  expect_equal(length(unique(fx$acs_sf$variable)), 13L)
  expect_identical(
    digest::digest(fx$acs_sf$estimate, algo = "sha256"),
    fx$estimate_digest
  )
})

test_that("BUG010-02 acs_test poison is invisible to production cache get", {
  fx <- load_replay_fixture("cache_poison")
  key <- bug010_acs_key(variables = bug010_vars(fx))
  td <- tempfile("bug010_ns_")
  with_test_cache(mode = "test", cache_dir = td, cleanup = FALSE, {
    expect_true(.cacs_cache_put(fx$acs_sf, key, "acs"))
    expect_true(file.exists(file.path(cacs_cache_dir(), "acs_test", paste0(key, ".rds"))))
  })
  with_test_cache(mode = "production", cache_dir = td, {
    expect_null(suppressMessages(.cacs_cache_get(key, "acs")))
  })
})

test_that("BUG010-03 cacs_acs_prefetch ignores poison written during test mode", {
  fx <- load_replay_fixture("cache_poison")
  vars <- bug010_vars(fx)
  td <- tempfile("bug010_prefetch_")
  key <- bug010_acs_key(variables = vars)

  with_test_cache(mode = "test", cache_dir = td, cleanup = FALSE, {
    withr::local_envvar(c(CENSUS_API_KEY = "FAKE"))
    testthat::local_mocked_bindings(
      load_variables = function(year, dataset, ...) bug010_mock_codebook(vars),
      .package = "tidycensus"
    )
    testthat::local_mocked_bindings(
      .tidycensus_get_acs_call = function(geography, variables, state, year, survey) {
        bug010_expect_acs_call(geography, variables, state, year, survey, vars)
        fx$acs_sf
      },
      .package = "catchmentACS"
    )
    test_out <- suppressWarnings(suppressMessages(
      cacs_acs_prefetch(state = "AL", year = 2023L, variables = vars, verbose = FALSE)
    ))
    test_key <- attr(test_out, "cacs_provenance")$cache_key
    expect_identical(test_key, key)
    expect_equal(nrow(test_out), fx$expected_poison_acs_rows)
    expect_true(file.exists(file.path(cacs_cache_dir(), "acs_test", paste0(key, ".rds"))))
    expect_true(file.exists(file.path(cacs_cache_dir(), "acs_test", paste0(key, ".fingerprint"))))
    expect_false(file.exists(file.path(cacs_cache_dir(), "acs", paste0(key, ".rds"))))
  })

  with_test_cache(mode = "production", cache_dir = td, {
    withr::local_envvar(c(CENSUS_API_KEY = "FAKE"))
    testthat::local_mocked_bindings(
      load_variables = function(year, dataset, ...) bug010_mock_codebook(vars),
      .package = "tidycensus"
    )
    call_n <- 0L
    testthat::local_mocked_bindings(
      .tidycensus_get_acs_call = function(geography, variables, state, year, survey) {
        bug010_expect_acs_call(geography, variables, state, year, survey, vars)
        call_n <<- call_n + 1L
        helper_mock_acs_long_sf(
          state = state,
          year = year,
          variables = variables,
          n_tract = 4L
        )
      },
      .package = "catchmentACS"
    )
    out <- suppressMessages(
      cacs_acs_prefetch(state = "AL", year = 2023L, variables = vars, verbose = FALSE)
    )
    expect_equal(call_n, 1L)
    expect_s3_class(out, "sf")
    expect_identical(attr(out, "cacs_provenance")$cache_key, key)
    expect_equal(nrow(out), length(vars) * 4L)
    expect_equal(length(unique(out$GEOID)), 4L)
    expect_false(identical(digest::digest(out$estimate, algo = "sha256"), fx$estimate_digest))
    expect_true(file.exists(file.path(cacs_cache_dir(), "acs_test", paste0(key, ".rds"))))
    expect_true(file.exists(file.path(cacs_cache_dir(), "acs", paste0(key, ".rds"))))
  })
})

test_that("BUG010-04 production poison cache hit emits stale warning", {
  fx <- load_replay_fixture("cache_poison")
  vars <- bug010_vars(fx)
  key <- bug010_acs_key(variables = vars)
  with_test_cache(mode = "production", {
    value <- fx$acs_sf
    attr(value, "cacs_provenance") <- list(
      state = "AL",
      year = 2023L,
      survey = "acs5",
      geography = "tract",
      n_variables_received = length(vars),
      cache_key = key
    )
    expect_true(.cacs_cache_put(value, key, "acs"))
    expect_warning(
      got <- suppressMessages(.cacs_cache_get(key, "acs")),
      class = "catchmentACS_warning_cache_stale_suspect"
    )
    expect_equal(nrow(got), fx$expected_poison_acs_rows)
    expect_identical(got$estimate, value$estimate)
  })
})

test_that("BUG010-05 clearing acs namespace removes production poison", {
  fx <- load_replay_fixture("cache_poison")
  key <- bug010_acs_key(variables = bug010_vars(fx))
  with_test_cache(mode = "production", {
    .cacs_cache_put(fx$acs_sf, key, "acs")
    expect_equal(cacs_cache_status()$n_entries[cacs_cache_status()$namespace == "acs"], 1L)
    expect_equal(cacs_cache_status()$n_fingerprints[cacs_cache_status()$namespace == "acs"], 1L)
    suppressMessages(cacs_clear_cache("acs", confirm = FALSE))
    expect_equal(cacs_cache_status()$n_entries[cacs_cache_status()$namespace == "acs"], 0L)
    expect_equal(cacs_cache_status()$n_fingerprints[cacs_cache_status()$namespace == "acs"], 0L)
    expect_false(file.exists(file.path(cacs_cache_dir(), "acs", paste0(key, ".rds"))))
    expect_false(file.exists(file.path(cacs_cache_dir(), "acs", paste0(key, ".fingerprint"))))
  })
})

test_that("BUG010-06 sidecar mismatch blocks poisoned production hit", {
  fx <- load_replay_fixture("cache_poison")
  key <- bug010_acs_key(variables = bug010_vars(fx))
  with_test_cache(mode = "production", {
    .cacs_cache_put(helper_mock_acs_long_sf(n_tract = 4L), key, "acs")
    saveRDS(fx$acs_sf, file.path(cacs_cache_dir(), "acs", paste0(key, ".rds")))
    expect_message(
      got <- .cacs_cache_get(key, "acs"),
      class = "catchmentACS_message_cache_fingerprint_mismatch"
    )
    expect_null(got)
  })
})

test_that("BUG010-07 force_refresh bypasses stale production ACS cache", {
  fx <- load_replay_fixture("cache_poison")
  vars <- bug010_vars(fx)
  key <- bug010_acs_key(variables = vars)
  with_test_cache(mode = "production", {
    .cacs_cache_put(fx$acs_sf, key, "acs")
    withr::local_envvar(c(CENSUS_API_KEY = "FAKE"))
    testthat::local_mocked_bindings(
      load_variables = function(year, dataset, ...) bug010_mock_codebook(vars),
      .package = "tidycensus"
    )
    call_n <- 0L
    testthat::local_mocked_bindings(
      .tidycensus_get_acs_call = function(geography, variables, state, year, survey) {
        bug010_expect_acs_call(geography, variables, state, year, survey, vars)
        call_n <<- call_n + 1L
        helper_mock_acs_long_sf(state = state, year = year,
                                variables = variables, n_tract = 4L)
      },
      .package = "catchmentACS"
    )
    out <- suppressMessages(
      cacs_acs_prefetch(state = "AL", year = 2023L, variables = vars,
                        force_refresh = TRUE, verbose = FALSE)
    )
    expect_equal(call_n, 1L)
    expect_identical(attr(out, "cacs_provenance")$cache_key, key)
    expect_equal(nrow(out), length(vars) * 4L)
    expect_false(identical(digest::digest(out$estimate, algo = "sha256"), fx$estimate_digest))
    fresh <- suppressWarnings(suppressMessages(.cacs_cache_get(key, "acs")))
    expect_equal(nrow(fresh), length(vars) * 4L)
    expect_false(identical(digest::digest(fresh$estimate, algo = "sha256"), fx$estimate_digest))
  })
})

test_that("BUG010-08 legacy poison without sidecar invalidates", {
  fx <- load_replay_fixture("cache_poison")
  key <- bug010_acs_key(variables = bug010_vars(fx))
  with_test_cache(mode = "production", {
    cache_write_legacy_rds("acs", key, fx$acs_sf)
    expect_message(
      got <- .cacs_cache_get(key, "acs"),
      class = "catchmentACS_message_cache_legacy_invalidated"
    )
    expect_null(got)
  })
})

test_that("BUG010-09 public prefetch invalidates legacy production poison and fetches fresh", {
  fx <- load_replay_fixture("cache_poison")
  vars <- bug010_vars(fx)
  key <- bug010_acs_key(variables = vars)
  with_test_cache(mode = "production", {
    cache_write_legacy_rds("acs", key, fx$acs_sf)
    withr::local_envvar(c(CENSUS_API_KEY = "FAKE"))
    testthat::local_mocked_bindings(
      load_variables = function(year, dataset, ...) bug010_mock_codebook(vars),
      .package = "tidycensus"
    )
    call_n <- 0L
    testthat::local_mocked_bindings(
      .tidycensus_get_acs_call = function(geography, variables, state, year, survey) {
        bug010_expect_acs_call(geography, variables, state, year, survey, vars)
        call_n <<- call_n + 1L
        helper_mock_acs_long_sf(state = state, year = year,
                                variables = variables, n_tract = 4L)
      },
      .package = "catchmentACS"
    )
    out <- suppressMessages(
      cacs_acs_prefetch(state = "AL", year = 2023L, variables = vars, verbose = FALSE)
    )
    expect_equal(call_n, 1L)
    expect_equal(nrow(out), length(vars) * 4L)
    expect_identical(attr(out, "cacs_provenance")$cache_key, key)
    expect_equal(cacs_cache_status()$n_entries[cacs_cache_status()$namespace == "acs"], 1L)
    expect_equal(cacs_cache_status()$n_fingerprints[cacs_cache_status()$namespace == "acs"], 1L)
    fresh <- suppressWarnings(suppressMessages(.cacs_cache_get(key, "acs")))
    expect_equal(nrow(fresh), length(vars) * 4L)
  })
})
