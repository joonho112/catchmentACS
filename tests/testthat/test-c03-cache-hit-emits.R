# ============================================================================
# C-03 cache-hit condition replay regression tests.
#
# Step 5.2 ensures conditions produced during a live cache-writing call can be
# replayed on cache hit. The motivating case is the BUG-001 water-tract filter:
# a live ACS prefetch writes the post-filter sf object, so the subsequent cache
# hit has no rows left to drop and must replay the original classed condition.
# ============================================================================


.c03_fake_census_key <- function(code) {
  withr::with_envvar(c(CENSUS_API_KEY = "FAKE_KEY_FOR_TEST"), code)
}

.c03_capture_water_condition <- function(geoids = "01003990000") {
  captured <- NULL
  withCallingHandlers(
    .cli_inform_water_tract_filter(geoids),
    catchmentACS_message_water_tract_filter = function(m) {
      captured <<- m
      invokeRestart("muffleMessage")
    }
  )
  captured
}


test_that("C03-CACHE-01 cache put stores and get replays classed conditions", {
  with_test_cache({
    key <- cache_key_for("acs")
    value <- cache_acs_tbl(5)
    cond <- .c03_capture_water_condition("01003990000")

    expect_true(.cacs_cache_put(value, key, "acs", conditions = list(cond)))

    replay <- NULL
    got <- withCallingHandlers(
      .cacs_cache_get(key, "acs"),
      catchmentACS_message_water_tract_filter = function(m) {
        replay <<- m
        invokeRestart("muffleMessage")
      },
      catchmentACS_message_cache = function(m) {
        invokeRestart("muffleMessage")
      }
    )

    expect_identical(got, value)
    expect_s3_class(replay, "catchmentACS_message_water_tract_filter")
    expect_s3_class(replay, "catchmentACS_message")
    expect_equal(replay$cacs_phase, "acs")
    expect_match(conditionMessage(replay), "01003990000", fixed = TRUE)
  })
})


test_that("C03-CACHE-02 cache hit without condition metadata is graceful", {
  with_test_cache({
    key <- cache_key_for("acs")
    value <- cache_acs_tbl(5)

    expect_true(.cacs_cache_put(value, key, "acs"))

    n_replayed <- 0L
    got <- withCallingHandlers(
      .cacs_cache_get(key, "acs"),
      catchmentACS_message_water_tract_filter = function(m) {
        n_replayed <<- n_replayed + 1L
        invokeRestart("muffleMessage")
      },
      catchmentACS_message_cache = function(m) {
        invokeRestart("muffleMessage")
      }
    )

    expect_identical(got, value)
    expect_identical(n_replayed, 0L)
  })
})


test_that("C03-CACHE-03 ACS prefetch cache hit replays water-tract condition", {
  # The replayed water-tract condition depends on sf/GEOS-version-coupled
  # water-tract detection; pin this replay assertion to a local dev stack.
  skip_on_ci()
  skip_on_covr()
  skip_if_no_water_fixture()
  fixture <- load_water_fixture()

  with_test_cache({
    .c03_fake_census_key({
      cb <- helper_mock_codebook(known = "B01003_001")
      testthat::local_mocked_bindings(
        load_variables = function(year, dataset, ...) cb,
        .package = "tidycensus"
      )

      call_n <- 0L
      testthat::local_mocked_bindings(
        .tidycensus_get_acs_call = function(geography, variables, state,
                                            year, survey) {
          call_n <<- call_n + 1L
          fixture
        },
        .package = "catchmentACS"
      )

      water_messages <- list()
      withCallingHandlers(
        {
          out_live <- cacs_acs_prefetch(
            state = "AL", year = 2023L, variables = "B01003_001",
            drop_water_tracts = TRUE, verbose = TRUE
          )
          out_hit <- cacs_acs_prefetch(
            state = "AL", year = 2023L, variables = "B01003_001",
            drop_water_tracts = TRUE, verbose = TRUE
          )
        },
        catchmentACS_message_water_tract_filter = function(m) {
          water_messages[[length(water_messages) + 1L]] <<- m
          invokeRestart("muffleMessage")
        },
        catchmentACS_message_cache = function(m) {
          invokeRestart("muffleMessage")
        },
        catchmentACS_message_progress = function(m) {
          invokeRestart("muffleMessage")
        }
      )

      expect_equal(call_n, 1L)
      expect_false("01003990000" %in% out_live$GEOID)
      expect_false("01003990000" %in% out_hit$GEOID)
      expect_identical(out_live, out_hit)
      expect_length(water_messages, 2L)
      expect_true(all(vapply(
        water_messages,
        inherits,
        logical(1L),
        what = "catchmentACS_message_water_tract_filter"
      )))
      expect_true(all(grepl(
        "01003990000",
        vapply(water_messages, conditionMessage, character(1L)),
        fixed = TRUE
      )))
    })
  })
})


test_that("C03-CACHE-04 ACS prefetch valid cache without metadata is graceful", {
  skip_if_no_water_fixture()
  fixture <- load_water_fixture()

  with_test_cache({
    .c03_fake_census_key({
      cb <- helper_mock_codebook(known = "B01003_001")
      testthat::local_mocked_bindings(
        load_variables = function(year, dataset, ...) cb,
        .package = "tidycensus"
      )

      call_n <- 0L
      testthat::local_mocked_bindings(
        .tidycensus_get_acs_call = function(geography, variables, state,
                                            year, survey) {
          call_n <<- call_n + 1L
          fixture
        },
        .package = "catchmentACS"
      )

      out_live <- suppressMessages(cacs_acs_prefetch(
        state = "AL", year = 2023L, variables = "B01003_001",
        drop_water_tracts = TRUE, verbose = FALSE
      ))
      expect_equal(call_n, 1L)

      # Rewrite the same key without condition metadata but with a valid
      # fingerprint, simulating a v0.3 cache entry created before Step 5.2.
      cache_key <- attr(out_live, "cacs_provenance")$cache_key
      attr(out_live, .CACS_CACHE_CONDITIONS_ATTR) <- NULL
      expect_true(.cacs_cache_put(out_live, cache_key, "acs"))

      testthat::local_mocked_bindings(
        .tidycensus_get_acs_call = function(...) {
          stop("backend should not be invoked on valid cache hit")
        },
        .package = "catchmentACS"
      )

      n_replayed <- 0L
      out_hit <- withCallingHandlers(
        cacs_acs_prefetch(
          state = "AL", year = 2023L, variables = "B01003_001",
          drop_water_tracts = TRUE, verbose = TRUE
        ),
        catchmentACS_message_water_tract_filter = function(m) {
          n_replayed <<- n_replayed + 1L
          invokeRestart("muffleMessage")
        },
        catchmentACS_message_cache = function(m) {
          invokeRestart("muffleMessage")
        },
        catchmentACS_message_progress = function(m) {
          invokeRestart("muffleMessage")
        }
      )

      expect_identical(out_hit, out_live)
      expect_identical(n_replayed, 0L)
    })
  })
})


test_that("C03-CACHE-05 ACS replay honors current verbose and drop-water flags", {
  skip_if_no_water_fixture()
  fixture <- load_water_fixture()

  with_test_cache({
    .c03_fake_census_key({
      cb <- helper_mock_codebook(known = "B01003_001")
      testthat::local_mocked_bindings(
        load_variables = function(year, dataset, ...) cb,
        .package = "tidycensus"
      )

      testthat::local_mocked_bindings(
        .tidycensus_get_acs_call = function(geography, variables, state,
                                            year, survey) fixture,
        .package = "catchmentACS"
      )

      suppressMessages(cacs_acs_prefetch(
        state = "AL", year = 2023L, variables = "B01003_001",
        drop_water_tracts = TRUE, verbose = TRUE
      ))

      n_replayed <- 0L
      withCallingHandlers(
        cacs_acs_prefetch(
          state = "AL", year = 2023L, variables = "B01003_001",
          drop_water_tracts = TRUE, verbose = FALSE
        ),
        catchmentACS_message_water_tract_filter = function(m) {
          n_replayed <<- n_replayed + 1L
          invokeRestart("muffleMessage")
        },
        catchmentACS_message_cache = function(m) {
          invokeRestart("muffleMessage")
        },
        catchmentACS_message_progress = function(m) {
          invokeRestart("muffleMessage")
        }
      )
      expect_identical(n_replayed, 0L)

      withCallingHandlers(
        cacs_acs_prefetch(
          state = "AL", year = 2023L, variables = "B01003_001",
          drop_water_tracts = FALSE, verbose = TRUE
        ),
        catchmentACS_message_water_tract_filter = function(m) {
          n_replayed <<- n_replayed + 1L
          invokeRestart("muffleMessage")
        },
        catchmentACS_message_cache = function(m) {
          invokeRestart("muffleMessage")
        },
        catchmentACS_message_progress = function(m) {
          invokeRestart("muffleMessage")
        }
      )
      expect_identical(n_replayed, 0L)
    })
  })
})
