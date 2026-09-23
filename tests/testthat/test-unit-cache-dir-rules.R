# The cache folder: the setting that decides it, the default inside the
# temporary folder of the R session, and a change of setting in the middle of
# a session.

.cache_dir_path <- function(path) {
  normalizePath(path, mustWork = FALSE, winslash = "/")
}

.cache_dir_other_key <- function(namespace, value) {
  payload <- cache_payload(namespace)
  payload$k1 <- value
  .cacs_cache_key(payload, namespace)
}

test_that("CACHE-DIR-01 the option takes precedence over CACS_CACHE_DIR", {
  opt_dir <- tempfile("cacs_dir_option_")
  env_dir <- tempfile("cacs_dir_envvar_")
  withr::local_options(catchmentACS.cache_dir = opt_dir)
  withr::local_envvar(CACS_CACHE_DIR = env_dir)
  expect_identical(cacs_cache_dir(create = FALSE), .cache_dir_path(opt_dir))
})

test_that("CACHE-DIR-02 CACS_CACHE_DIR is used when the option is not set", {
  env_dir <- tempfile("cacs_dir_envvar_")
  withr::local_options(catchmentACS.cache_dir = NULL)
  withr::local_envvar(CACS_CACHE_DIR = env_dir)
  expect_identical(cacs_cache_dir(create = FALSE), .cache_dir_path(env_dir))
})

test_that("CACHE-DIR-03 without either setting the folder is inside tempdir()", {
  withr::local_options(catchmentACS.cache_dir = NULL)
  withr::local_envvar(CACS_CACHE_DIR = NA)
  path <- cacs_cache_dir(create = FALSE)
  expect_identical(basename(path), "catchmentACS")
  expect_identical(.cache_dir_path(dirname(path)), .cache_dir_path(tempdir()))
})

test_that("CACHE-DIR-04 without either setting a saved result goes inside tempdir()", {
  default_dir <- file.path(tempdir(), "catchmentACS")
  existed <- dir.exists(default_dir)
  withr::defer(if (!existed) unlink(default_dir, recursive = TRUE))
  withr::local_options(
    catchmentACS.cache_dir = NULL,
    catchmentACS.cache_enabled = TRUE,
    catchmentACS.cache_namespace_mode = "production"
  )
  withr::local_envvar(
    CACS_CACHE_DIR = NA, CACS_NO_CACHE = NA, CACS_CACHE_ENABLED = NA,
    CACS_CACHE_ISOCHRONE = NA
  )
  key <- .cache_dir_other_key("isochrone", 1001L)
  expect_true(.cacs_cache_put(list(x = 1), key, "isochrone"))
  expect_true(file.exists(file.path(default_dir, "isochrone", paste0(key, ".rds"))))
  expect_identical(suppressMessages(.cacs_cache_get(key, "isochrone")), list(x = 1))
})

test_that("CACHE-DIR-05 after the option changes, results are saved, read, and deleted in the new folder", {
  first <- tempfile("cacs_dir_first_")
  second <- tempfile("cacs_dir_second_")
  withr::defer(unlink(c(first, second), recursive = TRUE))
  with_test_cache(cache_dir = first, cleanup = FALSE, {
    key1 <- .cache_dir_other_key("isochrone", 2001L)
    key2 <- .cache_dir_other_key("isochrone", 2002L)
    expect_true(.cacs_cache_put(list(x = 1), key1, "isochrone"))
    expect_identical(cacs_cache_dir(create = FALSE), .cache_dir_path(first))

    withr::with_options(list(catchmentACS.cache_dir = second), {
      expect_identical(cacs_cache_dir(create = FALSE), .cache_dir_path(second))
      expect_null(.cacs_cache_get(key1, "isochrone"))
      expect_true(.cacs_cache_put(list(x = 2), key2, "isochrone"))
      expect_true(file.exists(cache_paths(second, "isochrone", key2)$rds))
      expect_false(file.exists(cache_paths(first, "isochrone", key2)$rds))
      expect_equal(sum(cacs_cache_status()$n_entries), 1L)
      suppressMessages(cacs_clear_cache("all", confirm = FALSE))
      expect_false(file.exists(cache_paths(second, "isochrone", key2)$rds))
    })

    expect_true(file.exists(cache_paths(first, "isochrone", key1)$rds))
    expect_identical(suppressMessages(.cacs_cache_get(key1, "isochrone")), list(x = 1))
  })
})

test_that("CACHE-DIR-06 a change of CACS_CACHE_DIR takes effect at once", {
  first <- tempfile("cacs_dir_env_first_")
  second <- tempfile("cacs_dir_env_second_")
  withr::local_options(catchmentACS.cache_dir = NULL)
  withr::local_envvar(CACS_CACHE_DIR = first)
  expect_identical(cacs_cache_dir(create = FALSE), .cache_dir_path(first))
  withr::local_envvar(CACS_CACHE_DIR = second)
  expect_identical(cacs_cache_dir(create = FALSE), .cache_dir_path(second))
})
