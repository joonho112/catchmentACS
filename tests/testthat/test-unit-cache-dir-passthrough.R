# The cache_dir argument of cacs_intersect_weight(), and cacs_run(cache_dir = )
# reaching all three steps. Two bundled made-up sites keep the runs short.

.passthrough_inputs <- function() {
  loadNamespace("sf")
  ext <- function(f) {
    readRDS(system.file("extdata", f, package = "catchmentACS", mustWork = TRUE))
  }
  sites <- ext("legacy_2025_sites.rds")
  sites <- sites[sites$site_id %in% c("AL_SITE_07", "AL_SITE_08"), ]
  iso <- ext("legacy_2025_isochrones.rds")
  iso <- iso[iso$site_id %in% sites$site_id & iso$drive_time_min == 10L, ]
  list(sites = sites, iso = iso, acs = ext("sample_alabama_subset.rds"))
}

.cache_files <- function(root) {
  if (!dir.exists(root)) return(character(0))
  list.files(root, recursive = TRUE)
}

test_that("CACHE-PASS-01 cacs_intersect_weight saves and reads its result in cache_dir", {
  x <- .passthrough_inputs()
  default_root <- tempfile("cacs_pass_default_")
  given_root <- tempfile("cacs_pass_given_")
  withr::defer(unlink(given_root, recursive = TRUE))
  with_test_cache(cache_dir = default_root, {
    first <- suppressWarnings(suppressMessages(
      cacs_intersect_weight(x$iso, x$acs, verbose = FALSE, cache_dir = given_root)
    ))
    expect_length(list.files(file.path(given_root, "intersect"), pattern = "[.]rds$"), 1L)
    expect_length(list.files(file.path(given_root, "intersect"), pattern = "[.]fingerprint$"), 1L)
    expect_length(.cache_files(default_root), 0L)

    second <- suppressWarnings(suppressMessages(
      cacs_intersect_weight(x$iso, x$acs, verbose = FALSE, cache_dir = given_root)
    ))
    expect_identical(cacs_get_cache_state()$hits[[1]][["intersect"]], 1L)
    expect_identical(second$estimate, first$estimate)
    expect_length(.cache_files(default_root), 0L)
  })
})

test_that("CACHE-PASS-02 cacs_intersect_weight rejects a cache_dir that is not one string", {
  x <- .passthrough_inputs()
  with_test_cache({
    expect_error(
      cacs_intersect_weight(x$iso, x$acs, verbose = FALSE, cache_dir = 1),
      class = "catchmentACS_error_schema"
    )
    expect_error(
      cacs_intersect_weight(x$iso, x$acs, verbose = FALSE, cache_dir = c("a", "b")),
      class = "catchmentACS_error_schema"
    )
  })
})

test_that("CACHE-PASS-03 cacs_run(cache_dir = ) keeps the intersection results in that folder", {
  x <- .passthrough_inputs()
  default_root <- tempfile("cacs_pass_run_default_")
  given_root <- tempfile("cacs_pass_run_given_")
  withr::defer(unlink(given_root, recursive = TRUE))
  with_test_cache(cache_dir = default_root, {
    run <- function() {
      suppressWarnings(suppressMessages(cacs_run(
        sites = x$sites, state = "AL", year = 2023, drive_times = 10L,
        variables = unname(cacs_acs_default_vars), provider = "osrm",
        weight_method = "area", precomputed_isochrones = x$iso, acs = x$acs,
        rates = cacs_acs_default_rates, output = "long", verbose = FALSE,
        cache_dir = given_root
      )))
    }
    first <- run()
    expect_gt(nrow(first), 0L)
    expect_length(.cache_files(default_root), 0L)
    expect_length(list.files(file.path(given_root, "intersect"), pattern = "[.]rds$"), 1L)

    second <- run()
    expect_identical(cacs_get_cache_state()$hits[[1]][["intersect"]], 1L)
    expect_identical(second$estimate, first$estimate)
    expect_length(.cache_files(default_root), 0L)
  })
})

test_that("CACHE-PASS-04 a cache folder that cannot be created costs only the saved copy", {
  x <- .passthrough_inputs()
  blocker <- tempfile("cacs_pass_blocker_")
  writeLines("not a folder", blocker)
  withr::defer(unlink(blocker))
  with_test_cache({
    expect_warning(
      out <- suppressMessages(
        cacs_intersect_weight(x$iso, x$acs, verbose = FALSE,
                              cache_dir = file.path(blocker, "cache"))
      ),
      class = "catchmentACS_warning_runtime"
    )
    expect_gt(nrow(out), 0L)
    expect_false(dir.exists(file.path(blocker, "cache")))

    default_root <- cacs_cache_dir()
    suppressWarnings(suppressMessages(
      cacs_intersect_weight(x$iso, x$acs, verbose = FALSE, cache_dir = "")
    ))
    expect_length(list.files(file.path(default_root, "intersect"), pattern = "[.]rds$"), 1L)
  })
})
