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

test_that("CACHE-API-03 global cache setting preserves unrelated Rprofile lines", {
  with_test_cache({
    rp <- tempfile("Rprofile_")
    writeLines(c("# keep me", "options(existing_option = TRUE)"), rp)
    withr::local_options(catchmentACS.rprofile_path = rp)

    expect_true(cacs_set_cache(FALSE, scope = "global", confirm = FALSE))
    expect_true(cacs_set_cache(TRUE, scope = "global", confirm = FALSE))

    txt <- readLines(rp, warn = FALSE)
    expect_true("# keep me" %in% txt)
    expect_true("options(existing_option = TRUE)" %in% txt)
    expect_equal(sum(txt == "# catchmentACS v0.3 cache control"), 1L)
    expect_true(any(grepl("catchmentACS.cache_enabled = TRUE", txt, fixed = TRUE)))
  })
})

test_that("CACHE-API-04 global cache setting errors when parent is missing", {
  with_test_cache({
    missing_parent <- file.path(tempfile("missing_parent_"), ".Rprofile")
    withr::local_options(catchmentACS.rprofile_path = missing_parent)
    expect_error(
      cacs_set_cache(FALSE, scope = "global", confirm = FALSE),
      class = "catchmentACS_error_operator"
    )
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
