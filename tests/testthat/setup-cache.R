# Results that the tests save go to a temporary folder that is deleted when
# the tests end, not to the cache folder of the person running them. The
# option catchmentACS.cache_dir takes precedence over the environment variable
# CACS_CACHE_DIR and the default folder (see ?cacs_cache_dir); the variable is
# set as well, so R processes started by a test use the same folder. Tests
# that need a folder of their own still set it with with_test_cache() or their
# own options.

local({
  cache_dir <- tempfile("catchmentACS-tests-cache-")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  old_option <- options(catchmentACS.cache_dir = cache_dir)
  old_envvar <- Sys.getenv("CACS_CACHE_DIR", unset = NA)
  Sys.setenv(CACS_CACHE_DIR = cache_dir)

  withr::defer({
    options(old_option)
    if (is.na(old_envvar)) {
      Sys.unsetenv("CACS_CACHE_DIR")
    } else {
      Sys.setenv(CACS_CACHE_DIR = old_envvar)
    }
    unlink(cache_dir, recursive = TRUE)
  }, envir = testthat::teardown_env())
})
