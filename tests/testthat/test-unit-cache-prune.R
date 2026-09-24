# Tidying of a cache folder that lasts between sessions, and GeoPackage files
# in the cache tools. Every folder here is a temporary folder; the tidying of
# a lasting folder is reached with force = TRUE or by treating a temporary
# folder as lasting.

.prune_key <- function(i) {
  sprintf("%064d", as.integer(i))
}

.prune_file <- function(root, namespace, name, days_old = 0) {
  path <- file.path(root, namespace, name)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines("x", path)
  Sys.setFileTime(path, Sys.time() - days_old * 86400)
  path
}

test_that("PRUNE-01 old results and leftovers are deleted, recent files and other files are kept", {
  root <- tempfile("cacs_prune_")
  withr::defer(unlink(root, recursive = TRUE))
  withr::local_options(catchmentACS.cache_max_age_days = NULL)
  k <- vapply(1:11, .prune_key, character(1))

  keep <- c(
    .prune_file(root, "isochrone", paste0(k[1], ".rds")),
    .prune_file(root, "isochrone", paste0(k[1], ".fingerprint")),
    .prune_file(root, "isochrone", paste0(k[4], ".rds.tmp"), 0.5),
    .prune_file(root, "intersect", paste0(k[6], ".rds"), 0.2),
    .prune_file(root, "acs", paste0(k[9], ".gpkg"), 5),
    .prune_file(root, "acs", paste0(k[11], ".rds"), 10),
    .prune_file(root, "acs", paste0(k[11], ".fingerprint"), 40),
    .prune_file(root, "isochrone", "notes.txt", 100),
    .prune_file(root, "isochrone/other", paste0(k[2], ".rds"), 100),
    .prune_file(root, ".", paste0(k[3], ".rds"), 100)
  )
  drop <- c(
    .prune_file(root, "acs", paste0(k[2], ".rds"), 40),
    .prune_file(root, "acs", paste0(k[2], ".fingerprint"), 40),
    .prune_file(root, "acs", paste0(k[2], ".gpkg"), 40),
    .prune_file(root, "isochrone", paste0(k[3], ".rds.tmp"), 2),
    .prune_file(root, "intersect", paste0(k[5], ".rds"), 2),
    .prune_file(root, "acs_test", paste0(k[7], ".fingerprint"), 2),
    .prune_file(root, "acs", paste0(k[8], ".gpkg"), 40),
    .prune_file(root, "isochrone", paste0(k[10], ".fingerprint.tmp"), 3)
  )

  expect_equal(.cacs_cache_prune(root, force = TRUE), length(drop))
  expect_true(all(file.exists(keep)))
  expect_false(any(file.exists(drop)))
})

test_that("PRUNE-02 cache_max_age_days sets the age limit, Inf keeps old results, and a bad value counts as 30", {
  root <- tempfile("cacs_prune_age_")
  withr::defer(unlink(root, recursive = TRUE))
  k <- .prune_key(21)

  withr::local_options(catchmentACS.cache_max_age_days = Inf)
  old <- c(
    .prune_file(root, "isochrone", paste0(k, ".rds"), 400),
    .prune_file(root, "isochrone", paste0(k, ".fingerprint"), 400)
  )
  leftover <- .prune_file(root, "isochrone", paste0(.prune_key(22), ".rds.tmp"), 2)
  expect_equal(.cacs_cache_prune(root, force = TRUE), 1L)
  expect_true(all(file.exists(old)))
  expect_false(file.exists(leftover))

  withr::local_options(catchmentACS.cache_max_age_days = 500)
  expect_equal(.cacs_cache_prune(root, force = TRUE), 0L)
  expect_true(all(file.exists(old)))

  withr::local_options(catchmentACS.cache_max_age_days = "a month")
  expect_equal(.cacs_cache_prune(root, force = TRUE), 2L)
  expect_false(any(file.exists(old)))
})

test_that("PRUNE-03 a folder inside tempdir() is not tidied, and a lasting folder is tidied once per session", {
  root <- tempfile("cacs_prune_session_")
  withr::defer(unlink(root, recursive = TRUE))
  old <- c(
    .prune_file(root, "isochrone", paste0(.prune_key(31), ".rds"), 40),
    .prune_file(root, "isochrone", paste0(.prune_key(31), ".fingerprint"), 40)
  )

  with_test_cache(cache_dir = root, cleanup = FALSE, {
    expect_true(.cacs_cache_put(list(x = 1), .prune_key(32), "isochrone"))
    expect_true(all(file.exists(old)))
  })

  testthat::local_mocked_bindings(
    .cacs_cache_in_tempdir = function(path) FALSE,
    .package = "catchmentACS"
  )
  with_test_cache(cache_dir = root, cleanup = FALSE, {
    expect_true(.cacs_cache_put(list(x = 2), .prune_key(33), "isochrone"))
    expect_false(any(file.exists(old)))
    later <- c(
      .prune_file(root, "isochrone", paste0(.prune_key(34), ".rds"), 40),
      .prune_file(root, "isochrone", paste0(.prune_key(34), ".fingerprint"), 40)
    )
    expect_null(.cacs_cache_get(.prune_key(35), "isochrone"))
    expect_true(.cacs_cache_put(list(x = 3), .prune_key(36), "isochrone"))
    expect_true(all(file.exists(later)))
  })
})

test_that("PRUNE-04 reading a saved result resets its age", {
  root <- tempfile("cacs_prune_read_")
  withr::defer(unlink(root, recursive = TRUE))
  with_test_cache(cache_dir = root, cleanup = FALSE, {
    key <- cache_key_for("isochrone")
    expect_true(.cacs_cache_put(list(x = 1), key, "isochrone"))
    paths <- cache_paths(root, "isochrone", key)
    Sys.setFileTime(c(paths$rds, paths$fingerprint), Sys.time() - 40 * 86400)
    expect_identical(suppressMessages(.cacs_cache_get(key, "isochrone")), list(x = 1))
    expect_lt(as.numeric(difftime(Sys.time(), file.info(paths$rds)$mtime, units = "days")), 1)
    expect_equal(.cacs_cache_prune(root, force = TRUE), 0L)
    expect_true(all(file.exists(c(paths$rds, paths$fingerprint))))
  })
})

test_that("PRUNE-05 cacs_clear_cache() deletes GeoPackage files and cacs_cache_status() counts their size", {
  root <- tempfile("cacs_prune_gpkg_")
  withr::defer(unlink(root, recursive = TRUE))
  with_test_cache(mode = "production", cache_dir = root, cleanup = FALSE, {
    key <- cache_key_for("acs")
    expect_true(.cacs_cache_put(cache_acs_tbl(5), key, "acs"))
    gpkg <- file.path(root, "acs", paste0(key, ".gpkg"))
    writeBin(as.raw(seq_len(2048) %% 256), gpkg)
    paths <- cache_paths(root, "acs", key)
    sizes <- file.info(c(paths$rds, paths$fingerprint, gpkg))$size
    status <- cacs_cache_status()
    expect_equal(status$total_size_mb[status$namespace == "acs"], sum(sizes) / 1024^2)
    cleared <- suppressMessages(cacs_clear_cache("acs", confirm = FALSE))
    expect_equal(cleared$n_removed, 3L)
    expect_equal(cleared$bytes_freed, sum(sizes))
    expect_false(file.exists(gpkg))
  })
})

test_that("PRUNE-06 with the cache off and no cache folder set, write_gpkg = TRUE writes inside tempdir()", {
  skip_if_not("GPKG" %in% sf::st_drivers()$name, "GDAL has no GPKG driver")
  default_dir <- file.path(tempdir(), "catchmentACS")
  existed <- dir.exists(default_dir)
  withr::defer(if (!existed) unlink(default_dir, recursive = TRUE))
  before <- list.files(default_dir, pattern = "[.]gpkg$", recursive = TRUE)
  rds_before <- list.files(default_dir, pattern = "[.]rds$", recursive = TRUE)

  withr::local_options(catchmentACS.cache_dir = NULL, catchmentACS.cache_enabled = FALSE)
  withr::local_envvar(CACS_CACHE_DIR = NA, CENSUS_API_KEY = "FAKE_KEY_FOR_TEST")
  cb <- helper_mock_codebook(known = "B19013_001")
  testthat::local_mocked_bindings(
    load_variables = function(year, dataset, ...) cb,
    .package = "tidycensus"
  )
  testthat::local_mocked_bindings(
    .tidycensus_get_acs_call = function(geography, variables, state, year, survey) {
      helper_mock_acs_long_sf(state = state, year = year, variables = variables, n_tract = 4L)
    },
    .package = "catchmentACS"
  )

  out <- suppressMessages(cacs_acs_prefetch(
    state = "AL", variables = "B19013_001", write_gpkg = TRUE, verbose = FALSE
  ))
  expect_s3_class(out, "sf")
  after <- list.files(default_dir, pattern = "[.]gpkg$", recursive = TRUE)
  expect_length(setdiff(after, before), 1L)
  expect_identical(list.files(default_dir, pattern = "[.]rds$", recursive = TRUE), rds_before)
})

test_that("PRUNE-07 cacs_clear_cache() and cacs_cache_status() leave files the package did not make", {
  root <- tempfile("cacs_prune_other_")
  withr::defer(unlink(root, recursive = TRUE))
  with_test_cache(mode = "production", cache_dir = root, cleanup = FALSE, {
    key <- cache_key_for("acs")
    expect_true(.cacs_cache_put(cache_acs_tbl(5), key, "acs"))
    others <- c(
      .prune_file(root, "acs", "my_alabama_tracts.rds"),
      .prune_file(root, "acs", "my_tracts.gpkg"),
      .prune_file(root, "acs", "README.txt"),
      .prune_file(root, "isochrone", "notes.fingerprint")
    )
    paths <- cache_paths(root, "acs", key)
    own_bytes <- sum(file.info(c(paths$rds, paths$fingerprint))$size)

    status <- cacs_cache_status()
    expect_equal(status$n_entries[status$namespace == "acs"], 1L)
    expect_equal(status$legacy_entries[status$namespace == "acs"], 0L)
    expect_equal(status$n_fingerprints[status$namespace == "isochrone"], 0L)
    expect_equal(sum(status$total_size_mb), own_bytes / 1024^2)

    cleared <- suppressMessages(cacs_clear_cache("all", confirm = FALSE))
    expect_equal(sum(cleared$n_removed), 2L)
    expect_equal(sum(cleared$bytes_freed), own_bytes)
    expect_false(any(file.exists(c(paths$rds, paths$fingerprint))))
    expect_true(all(file.exists(others)))
  })
})

test_that("PRUNE-08 .cacs_cache_in_tempdir() tells the session's temporary folder from other folders", {
  expect_true(.cacs_cache_in_tempdir(tempdir()))
  expect_true(.cacs_cache_in_tempdir(file.path(tempdir(), "catchmentACS")))
  expect_true(.cacs_cache_in_tempdir(
    file.path(normalizePath(tempdir(), winslash = "/"), "not-created", "cache")
  ))
  # tempfile() writes the path as R does on the platform: on Windows, with
  # backslashes, and here for a folder that does not exist.
  expect_true(.cacs_cache_in_tempdir(tempfile("not_created_")))
  expect_true(.cacs_cache_in_tempdir(file.path(tempfile("not_created_"), "cache")))
  expect_false(.cacs_cache_in_tempdir(paste0(tempdir(), "x")))
  expect_false(.cacs_cache_in_tempdir(dirname(tempdir())))
  expect_false(.cacs_cache_in_tempdir(tools::R_user_dir("catchmentACS", "cache")))
})

test_that("PRUNE-09 a lasting folder is tidied before the first read of a session", {
  root <- tempfile("cacs_prune_first_read_")
  withr::defer(unlink(root, recursive = TRUE))
  key <- .prune_key(91)
  with_test_cache(cache_dir = root, cleanup = FALSE, {
    expect_true(.cacs_cache_put(list(x = 1), key, "isochrone"))
  })
  paths <- cache_paths(root, "isochrone", key)
  Sys.setFileTime(c(paths$rds, paths$fingerprint), Sys.time() - 40 * 86400)

  testthat::local_mocked_bindings(
    .cacs_cache_in_tempdir = function(path) FALSE,
    .package = "catchmentACS"
  )
  with_test_cache(cache_dir = root, cleanup = FALSE, {
    expect_null(.cacs_cache_get(key, "isochrone"))
    expect_false(any(file.exists(c(paths$rds, paths$fingerprint))))
  })
})
