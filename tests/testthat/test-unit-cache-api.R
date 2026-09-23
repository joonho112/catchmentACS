test_that("CACHE-API-01 cacs_set_cache session disable blocks reads and writes", {
  with_test_cache({
    key <- cache_key_for("isochrone")
    expect_true(.cacs_cache_put(list(x = 1), key, "isochrone"))
    cacs_set_cache(FALSE, scope = "session")
    expect_false(.cacs_cache_put(list(x = 2), key, "isochrone"))
    expect_null(.cacs_cache_get(key, "isochrone"))
    expect_false(cacs_get_cache_state()$enabled)
  })
})

test_that("CACHE-API-02 cacs_get_cache_state exposes physical namespaces", {
  with_test_cache(mode = "test", {
    state <- cacs_get_cache_state()
    expect_identical(state$fingerprint_algorithm, "sha256")
    expect_identical(state$namespace_mode, "test")
    expect_setequal(
      state$status[[1]]$namespace,
      c("isochrone", "acs", "acs_test", "intersect")
    )
  })
})

test_that("CACHE-API-03 global cache setting changes no startup file in any combination", {
  with_test_cache({
    withr::local_options(width = 200, cli.width = 200)
    home_rprofile <- path.expand("~/.Rprofile")
    home_before <- if (file.exists(home_rprofile)) tools::md5sum(home_rprofile) else NA_character_
    rp <- tempfile("Rprofile_")
    writeLines(c("# keep me", "options(existing_option = TRUE)"), rp)
    rp_before <- tools::md5sum(rp)
    rp_missing <- tempfile("Rprofile_missing_")
    combos <- expand.grid(
      enabled = c(TRUE, FALSE), confirm = c(TRUE, FALSE),
      no_confirm = c(NA, "1"), profile_env = c(NA, "file"),
      old_option = c(FALSE, TRUE), stringsAsFactors = FALSE
    )
    for (i in seq_len(nrow(combos))) {
      cb <- combos[i, ]
      withr::with_envvar(
        c(CACS_NO_CONFIRM = cb$no_confirm,
          R_PROFILE_USER = if (is.na(cb$profile_env)) NA else rp),
        withr::with_options(
          list(catchmentACS.rprofile_path = if (cb$old_option) rp_missing else NULL),
          expect_message(
            expect_true(cacs_set_cache(cb$enabled, scope = "global", confirm = cb$confirm)),
            class = "catchmentACS_message_cache"
          )
        )
      )
      expect_identical(getOption("catchmentACS.cache_enabled"), cb$enabled)
    }
    expect_identical(tools::md5sum(rp), rp_before)
    expect_false(file.exists(rp_missing))
    home_after <- if (file.exists(home_rprofile)) tools::md5sum(home_rprofile) else NA_character_
    expect_identical(unname(home_after), unname(home_before))
  })
})

test_that("CACHE-API-04 global cache setting needs no startup file folder", {
  with_test_cache({
    missing_parent <- file.path(tempfile("missing_parent_"), ".Rprofile")
    withr::local_options(catchmentACS.rprofile_path = missing_parent)
    withr::local_envvar(R_PROFILE_USER = missing_parent)
    expect_true(suppressMessages(cacs_set_cache(FALSE, scope = "global", confirm = FALSE)))
    expect_false(dir.exists(dirname(missing_parent)))
    expect_false(cacs_get_cache_state()$enabled)
  })
})

test_that("CACHE-API-05 invalid namespace clear request fails before deletion", {
  with_test_cache({
    key <- cache_key_for("isochrone")
    .cacs_cache_put(list(x = 1), key, "isochrone")
    expect_error(cacs_clear_cache("bogus", confirm = FALSE))
    expect_equal(cacs_cache_status()$n_entries[
      cacs_cache_status()$namespace == "isochrone"
    ], 1L)
  })
})

test_that("CACHE-API-06 state counters distinguish disabled misses from hits", {
  with_test_cache({
    key <- cache_key_for("isochrone")
    .cacs_cache_put(list(x = 1), key, "isochrone")
    suppressMessages(.cacs_cache_get(key, "isochrone"))
    cacs_set_cache(FALSE)
    expect_null(.cacs_cache_get(key, "isochrone"))

    state <- cacs_get_cache_state()
    expect_equal(state$hits[[1]][["isochrone"]], 1L)
    expect_equal(state$misses[[1]][["isochrone"]], 1L)
  })
})

test_that("CACHE-API-06 global cache setting says when an environment variable decides", {
  with_test_cache({
    withr::local_options(width = 200, cli.width = 200)
    withr::local_envvar(CACS_CACHE_ENABLED = "true")
    expect_message(
      cacs_set_cache(FALSE, scope = "global"),
      "environment variable `?CACS_CACHE_ENABLED`? keeps the cache on"
    )
    expect_true(cacs_get_cache_state()$enabled)

    withr::local_envvar(CACS_CACHE_ENABLED = NA, CACS_NO_CACHE = "1")
    expect_message(
      cacs_set_cache(TRUE, scope = "global"),
      "environment variable `?CACS_NO_CACHE`? keeps the cache off"
    )
    expect_false(cacs_get_cache_state()$enabled)
  })
})

test_that("CACHE-API-07 turning the cache on with a lasting folder names that folder", {
  with_test_cache({
    withr::local_options(width = 200, cli.width = 200)
    testthat::local_mocked_bindings(
      .cacs_cache_in_tempdir = function(path) FALSE,
      .package = "catchmentACS"
    )
    msg <- tryCatch(
      cacs_set_cache(TRUE, scope = "global"),
      catchmentACS_message_cache = function(m) conditionMessage(m)
    )
    expect_match(msg, "which lasts between sessions", fixed = TRUE)
    expect_match(msg, "remove it", fixed = TRUE)
    expect_false(grepl("R_user_dir", msg, fixed = TRUE))
  })
})

test_that("CACHE-API-08 confirm = TRUE in a session that cannot ask removes nothing and says why", {
  skip_if(interactive())
  with_test_cache({
    withr::local_envvar(CACS_NO_CONFIRM = NA)
    withr::local_options(width = 200, cli.width = 200)
    key <- cache_key_for("isochrone")
    expect_true(.cacs_cache_put(list(x = 1), key, "isochrone"))
    msgs <- character()
    keep_message <- function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
    utils::capture.output(
      out <- withCallingHandlers(
        cacs_clear_cache("isochrone", confirm = TRUE),
        message = keep_message
      )
    )
    expect_equal(nrow(out), 0L)
    expect_true(any(grepl("cannot ask for confirmation", msgs, fixed = TRUE)))
    expect_false(any(grepl("Aborted by user", msgs, fixed = TRUE)))
    status <- cacs_cache_status()
    expect_equal(status$n_entries[status$namespace == "isochrone"], 1L)

    msgs <- character()
    out <- withCallingHandlers(
      cacs_clear_cache("isochrone", confirm = FALSE),
      message = keep_message
    )
    expect_equal(sum(out$n_removed), 2L)
    expect_true(any(grepl("Removed 2 files, freeing ", msgs, fixed = TRUE)))
    expect_false(any(grepl("freeing \"", msgs, fixed = TRUE)))
  })
})
