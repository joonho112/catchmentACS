test_that("T-P3-CACHE-ISO-01 test namespace mode writes ACS to acs_test", {
  with_test_cache(mode = "test", {
    key <- .cacs_cache_key(cache_payload(12), "acs")
    expect_true(.cacs_cache_put(list(x = "test"), key, "acs"))
    expect_true(file.exists(file.path(cacs_cache_dir(), "acs_test", paste0(key, ".rds"))))
    expect_false(file.exists(file.path(cacs_cache_dir(), "acs", paste0(key, ".rds"))))
  })
})

test_that("T-P3-CACHE-ISO-02 production mode ignores acs_test entries", {
  td <- tempfile("cacs_isolation_")
  with_test_cache(mode = "test", cache_dir = td, cleanup = FALSE, {
    key <- .cacs_cache_key(cache_payload(12), "acs")
    .cacs_cache_put(list(x = "poison"), key, "acs")
  })
  with_test_cache(mode = "production", cache_dir = td, {
    key <- .cacs_cache_key(cache_payload(12), "acs")
    expect_null(suppressMessages(.cacs_cache_get(key, "acs")))
  })
  if (dir.exists(td)) unlink(td, recursive = TRUE)
})

test_that("T-P3-CACHE-ISO-03 production namespace writes ACS to acs", {
  with_test_cache(mode = "production", {
    key <- .cacs_cache_key(cache_payload(12), "acs")
    .cacs_cache_put(list(x = "prod"), key, "acs")
    expect_true(file.exists(file.path(cacs_cache_dir(), "acs", paste0(key, ".rds"))))
    expect_false(file.exists(file.path(cacs_cache_dir(), "acs_test", paste0(key, ".rds"))))
  })
})

test_that("T-P3-CACHE-ISO-04 path heuristic routes tests path to acs_test", {
  td <- file.path(tempfile("root_"), "tests", "cache")
  withr::local_options(
    catchmentACS.cache_namespace_mode = NULL,
    catchmentACS.cache_test_namespace = NULL
  )
  key <- .cacs_cache_key(cache_payload(12), "acs")
  on.exit(if (dir.exists(dirname(dirname(td)))) unlink(dirname(dirname(td)), recursive = TRUE), add = TRUE)
  expect_true(.cacs_cache_put(list(x = 1), key, "acs", cache_dir = td))
  expect_true(file.exists(file.path(td, "acs_test", paste0(key, ".rds"))))
})

test_that("T-P3-CACHE-ISO-05 clear all removes acs_test too", {
  with_test_cache(mode = "test", {
    key <- .cacs_cache_key(cache_payload(12), "acs")
    .cacs_cache_put(list(x = "test"), key, "acs")
    suppressMessages(cacs_clear_cache("all", confirm = FALSE))
    expect_equal(sum(cacs_cache_status()$n_entries), 0L)
  })
})

test_that("T-P3-CACHE-ISO-06 env test mode routes ACS to acs_test", {
  with_test_cache(mode = "production", {
    withr::local_options(catchmentACS.cache_namespace_mode = NULL)
    withr::local_envvar(c(CACS_CACHE_TEST_MODE = "1"))
    key <- .cacs_cache_key(cache_payload(12), "acs")
    .cacs_cache_put(list(x = "env-test"), key, "acs")
    expect_true(file.exists(file.path(cacs_cache_dir(), "acs_test", paste0(key, ".rds"))))
  })
})

test_that("T-P3-CACHE-ISO-07 explicit non-test cache_dir wins over TESTTHAT env", {
  td <- tempfile("cacs_prod_cache_")
  on.exit(if (dir.exists(td)) unlink(td, recursive = TRUE), add = TRUE)
  with_test_cache(mode = "production", {
    withr::local_options(
      catchmentACS.cache_namespace_mode = NULL,
      catchmentACS.cache_test_namespace = NULL,
      catchmentACS.test_cache = NULL,
      catchmentACS.cache_dir = NULL
    )
    withr::local_envvar(c(TESTTHAT = "true"))
    key <- .cacs_cache_key(cache_payload(12), "acs")
    .cacs_cache_put(list(x = "explicit-dir"), key, "acs", cache_dir = td)
    expect_true(file.exists(file.path(td, "acs", paste0(key, ".rds"))))
    expect_false(file.exists(file.path(td, "acs_test", paste0(key, ".rds"))))
  })
})
