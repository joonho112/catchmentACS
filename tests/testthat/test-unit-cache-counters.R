test_that("P6-CACHE-CTR-01 fresh session exposes three zeroed public counters", {
  with_test_cache({
    counters <- cache_counter_state()

    expect_s3_class(counters, "tbl_df")
    expect_equal(nrow(counters), 3L)
    expect_identical(counters$namespace, c("isochrone", "acs", "intersect"))
    expect_true(all(counters$hit == 0L))
    expect_true(all(counters$miss == 0L))
    expect_true(all(is.nan(counters$hit_rate)))
  })
})

test_that("P6-CACHE-CTR-02 successful cache get increments only its hit counter", {
  with_test_cache({
    key <- cache_key_for("isochrone")
    .cacs_cache_put(list(x = "iso"), key, "isochrone")
    suppressMessages(.cacs_cache_get(key, "isochrone"))

    iso <- cache_counter_row("isochrone")
    acs <- cache_counter_row("acs")
    expect_equal(iso$hit, 1L)
    expect_equal(iso$miss, 0L)
    expect_equal(iso$hit_rate, 1)
    expect_equal(acs$hit, 0L)
    expect_equal(acs$miss, 0L)
  })
})

test_that("P6-CACHE-CTR-03 nonexistent cache get increments only its miss counter", {
  with_test_cache({
    key <- cache_key_for("acs")
    expect_null(.cacs_cache_get(key, "acs"))

    acs <- cache_counter_row("acs")
    iso <- cache_counter_row("isochrone")
    expect_equal(acs$hit, 0L)
    expect_equal(acs$miss, 1L)
    expect_equal(acs$hit_rate, 0)
    expect_equal(iso$hit, 0L)
    expect_equal(iso$miss, 0L)
  })
})

test_that("P6-CACHE-CTR-04 cacs_clear_cache resets only cleared namespace counters", {
  with_test_cache({
    iso_key <- cache_key_for("isochrone")
    acs_key <- cache_key_for("acs")
    .cacs_cache_put(list(x = "iso"), iso_key, "isochrone")
    .cacs_cache_put(list(x = "acs"), acs_key, "acs")
    suppressMessages(.cacs_cache_get(iso_key, "isochrone"))
    suppressMessages(.cacs_cache_get(acs_key, "acs"))

    suppressMessages(cacs_clear_cache(namespace = "isochrone", confirm = FALSE))

    iso <- cache_counter_row("isochrone")
    acs <- cache_counter_row("acs")
    expect_equal(iso$hit, 0L)
    expect_equal(iso$miss, 0L)
    expect_true(is.nan(iso$hit_rate))
    expect_equal(acs$hit, 1L)
    expect_equal(acs$miss, 0L)
  })
})

test_that("P6-CACHE-CTR-05 fingerprint mismatch invalidates and counts as a miss", {
  with_test_cache({
    key <- cache_key_for("intersect")
    .cacs_cache_put(list(x = "valid"), key, "intersect")

    path <- file.path(cacs_cache_dir(), "intersect", paste0(key, ".rds"))
    saveRDS(list(x = "tampered"), path)

    expect_message(
      got <- .cacs_cache_get(key, "intersect"),
      class = "catchmentACS_message_cache_fingerprint_mismatch"
    )
    expect_null(got)

    counters <- cache_counter_row("intersect")
    expect_equal(counters$hit, 0L)
    expect_equal(counters$miss, 1L)
    expect_equal(counters$hit_rate, 0)
  })
})

test_that("P6-CACHE-CTR-06 ACS public counter aggregates acs_test reads", {
  with_test_cache(mode = "test", {
    key <- cache_key_for("acs")
    .cacs_cache_put(list(x = "acs-test"), key, "acs")
    suppressMessages(.cacs_cache_get(key, "acs"))

    counters <- cache_counter_row("acs")
    state <- cacs_get_cache_state()
    expect_equal(counters$hit, 1L)
    expect_equal(counters$miss, 0L)
    expect_equal(state$hits[[1]][["acs_test"]], 1L)
    expect_equal(state$hits[[1]][["acs"]], 0L)
  })
})

test_that("P6-CACHE-CTR-07 disabled cache get counts as public miss", {
  with_test_cache({
    key <- cache_key_for("isochrone")
    .cacs_cache_put(list(x = "iso"), key, "isochrone")
    cacs_set_cache(FALSE)
    expect_null(.cacs_cache_get(key, "isochrone"))
    row <- cache_counter_row("isochrone")
    expect_equal(row$hit, 0L)
    expect_equal(row$miss, 1L)
    expect_equal(row$hit_rate, 0)
  })
})

test_that("P6-CACHE-CTR-08 clear all resets public counters across namespaces", {
  with_test_cache({
    for (ns in c("isochrone", "acs", "intersect")) {
      key <- cache_key_for(ns)
      .cacs_cache_put(list(ns = ns), key, ns)
      suppressMessages(.cacs_cache_get(key, ns))
    }
    suppressMessages(cacs_clear_cache("all", confirm = FALSE))
    counters <- cache_counter_state()
    expect_true(all(counters$hit == 0L))
    expect_true(all(counters$miss == 0L))
    expect_true(all(is.nan(counters$hit_rate)))
  })
})

test_that("P6-CACHE-CTR-09 replayed cached conditions do not double-count hits", {
  with_test_cache({
    key <- cache_key_for("acs")
    cond <- structure(
      list(message = "cached condition", call = NULL),
      class = c("catchmentACS_message_cache", "catchmentACS_message",
                "catchmentACS_condition", "message", "condition")
    )
    .cacs_cache_put(cache_acs_tbl(5), key, "acs", conditions = list(cond))
    suppressMessages(.cacs_cache_get(key, "acs"))
    row <- cache_counter_row("acs")
    expect_equal(row$hit, 1L)
    expect_equal(row$miss, 0L)
  })
})
