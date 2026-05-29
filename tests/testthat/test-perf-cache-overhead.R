perf_large_acs <- function(n = 18655L) {
  cache_acs_tbl(n)
}

test_that("PERF-CACHE-01 sha256 value fingerprint stays below local budget", {
  value <- perf_large_acs()
  elapsed <- unname(system.time(.cacs_cache_value_digest(value))[["elapsed"]])
  expect_lt(elapsed, 0.05)
})

test_that("PERF-CACHE-02 cache hit on large ACS fixture stays below local budget", {
  value <- perf_large_acs()
  with_test_cache({
    key <- cache_key_for("acs")
    .cacs_cache_put(value, key, "acs")
    elapsed <- unname(system.time(
      suppressWarnings(suppressMessages(.cacs_cache_get(key, "acs")))
    )[["elapsed"]])
    expect_lt(elapsed, 0.25)
  })
})

test_that("PERF-CACHE-03 cache status across namespaces stays below local budget", {
  with_test_cache({
    .cacs_cache_put(list(x = 1), cache_key_for("isochrone"), "isochrone")
    .cacs_cache_put(cache_acs_tbl(100), cache_key_for("acs"), "acs")
    .cacs_cache_put(tibble::tibble(x = 1), cache_key_for("intersect"), "intersect")
    elapsed <- unname(system.time(cacs_cache_status())[["elapsed"]])
    expect_lt(elapsed, 0.10)
    expect_equal(sum(cacs_cache_status()$n_entries), 3L)
  })
})

test_that("PERF-CACHE-04 counter bump loop stays below 100us per bump", {
  with_test_cache({
    elapsed <- unname(system.time({
      for (i in seq_len(10000L)) {
        .cacs_cache_bump("hits", "isochrone")
      }
    })[["elapsed"]])
    expect_lt(elapsed / 10000, 0.0001)
  })
})

test_that("PERF-CACHE-05 public counter tibble construction stays cheap", {
  with_test_cache({
    elapsed <- unname(system.time({
      for (i in seq_len(1000L)) {
        .cacs_cache_counter_tibble()
      }
    })[["elapsed"]])
    expect_lt(elapsed / 1000, 0.001)
  })
})

test_that("PERF-CACHE-06 clear reset keeps counters zero without scanning values", {
  with_test_cache({
    .cacs_cache_bump("hits", "isochrone")
    .cacs_cache_bump("misses", "acs")
    elapsed <- unname(system.time(.cacs_cache_reset_counters("all"))[["elapsed"]])
    expect_lt(elapsed, 0.01)
    expect_true(all(cacs_get_cache_state()$counters[[1]]$hit == 0L))
    expect_true(all(cacs_get_cache_state()$counters[[1]]$miss == 0L))
  })
})

test_that("PERF-CACHE-07 cache hit counter overhead preserves large-hit budget", {
  value <- perf_large_acs()
  with_test_cache({
    key <- cache_key_for("acs")
    .cacs_cache_put(value, key, "acs")
    elapsed <- unname(system.time(
      suppressWarnings(suppressMessages(.cacs_cache_get(key, "acs")))
    )[["elapsed"]])
    expect_lt(elapsed, 0.25)
    expect_equal(.p6_counter_row("acs")$hit, 1L)
  })
})
