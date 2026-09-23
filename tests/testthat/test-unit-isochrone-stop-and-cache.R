# A routing service that answers with a request limit (HTTP 429) or a refused
# key (401, 403) gets no further requests in the same call. A result in which
# a site failed in a way that may be temporary is not saved in the cache, and
# such a result saved by an earlier version is not used; a site that the
# service rejected with another HTTP 4xx status is saved and reported when
# the saved result is used. The HTTP status is read only where an error
# message gives one, so a connection failure that names a port, such as 443,
# is not taken for a rejection. The routing calls are mocked, so no request
# leaves the machine.

.stop_sites <- function() {
  sf::st_as_sf(
    data.frame(site_id = c("S1", "S2", "S3"),
               lon = c(-86.80, -86.70, -86.60), lat = 33.5),
    coords = c("lon", "lat"), crs = 4326
  )
}

.stop_response <- function(loc, breaks) {
  polys <- lapply(breaks, function(b) {
    h <- 0.01 * b
    sf::st_polygon(list(rbind(
      c(loc[1] - h, loc[2] - h), c(loc[1] + h, loc[2] - h),
      c(loc[1] + h, loc[2] + h), c(loc[1] - h, loc[2] + h),
      c(loc[1] - h, loc[2] - h)
    )))
  })
  sf::st_sf(
    data.frame(id = seq_along(breaks), isomin = 0L,
               isomax = as.integer(breaks)),
    geometry = sf::st_sfc(polys, crs = 4326)
  )
}

test_that("ISO-STOP-01 OSRM: no request follows a 429, 401, or 403", {
  testthat::skip_if_not_installed("osrm")
  testthat::local_mocked_bindings(
    Sys.sleep = function(time) invisible(NULL),
    .package = "base"
  )
  expected_class <- c("429" = "catchmentACS_error_operator",
                      "401" = "catchmentACS_error_credential",
                      "403" = "catchmentACS_error_credential")
  for (status in names(expected_class)) {
    n_calls <- 0L
    testthat::local_mocked_bindings(
      .osrm_call_isochrone = function(loc, breaks, res = NULL) {
        n_calls <<- n_calls + 1L
        stop(sprintf("OSRM client error (HTTP %s)", status))
      },
      .package = "catchmentACS"
    )
    expect_error(
      .iso_via_osrm(.stop_sites(), drive_times = 5L, profile = "car",
                    osrm_mode = "docker", res = 30L),
      class = expected_class[[status]]
    )
    expect_identical(n_calls, 1L, label = paste("routing calls after HTTP", status))
  }
})

test_that("ISO-STOP-02 openrouteservice: no request follows a 429, 401, or 403", {
  testthat::local_mocked_bindings(
    check_installed = function(pkg, ...) invisible(TRUE),
    .package = "rlang"
  )
  expected_class <- c("429" = "catchmentACS_error_operator",
                      "401" = "catchmentACS_error_credential",
                      "403" = "catchmentACS_error_credential")
  for (status in names(expected_class)) {
    n_calls <- 0L
    testthat::local_mocked_bindings(
      .ors_call_isochrones = function(locations, profile, range, api_key,
                                      passthrough = list()) {
        n_calls <<- n_calls + 1L
        stop(sprintf("ORS client error (HTTP %s)", status))
      },
      .package = "catchmentACS"
    )
    expect_error(
      .iso_via_ors(.stop_sites(), drive_times = 5L, profile = "car",
                   api_key = "FAKE_KEY"),
      class = expected_class[[status]]
    )
    expect_identical(n_calls, 1L, label = paste("routing calls after HTTP", status))
  }
})

test_that("ISO-CACHE-01 a result in which a site failed with HTTP 503 is not saved, so the next call routes again", {
  testthat::skip_if_not_installed("osrm")
  root <- tempfile("cacs_iso_failed_")
  withr::defer(unlink(root, recursive = TRUE))
  testthat::local_mocked_bindings(
    Sys.sleep = function(time) invisible(NULL),
    .package = "base"
  )
  n_calls <- 0L
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      n_calls <<- n_calls + 1L
      if (isTRUE(all.equal(loc[[1]], -86.70))) {
        stop("OSRM server error (HTTP 503)")
      }
      .stop_response(loc, breaks)
    },
    .package = "catchmentACS"
  )
  with_test_cache(cache_dir = root, cleanup = FALSE, {
    run <- function() {
      suppressWarnings(suppressMessages(cacs_isochrone(
        .stop_sites(), drive_times = 5L, provider = "osrm",
        osrm_mode = "docker", res = 30L, cache_dir = root, verbose = FALSE
      )))
    }
    first <- run()
    expect_identical(sum(!is.na(first$failure_reason)), 1L)
    calls_first <- n_calls
    expect_length(list.files(root, pattern = "[.](rds|fingerprint)$", recursive = TRUE), 0L)
    second <- run()
    expect_gt(n_calls, calls_first)
    expect_identical(sum(!is.na(second$failure_reason)), 1L)
  })
})

test_that("ISO-CACHE-02 a result in which every site succeeded is still saved and read back", {
  testthat::skip_if_not_installed("osrm")
  root <- tempfile("cacs_iso_ok_")
  withr::defer(unlink(root, recursive = TRUE))
  n_calls <- 0L
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      n_calls <<- n_calls + 1L
      .stop_response(loc, breaks)
    },
    .package = "catchmentACS"
  )
  with_test_cache(cache_dir = root, cleanup = FALSE, {
    run <- function() {
      suppressWarnings(suppressMessages(cacs_isochrone(
        .stop_sites(), drive_times = 5L, provider = "osrm",
        osrm_mode = "docker", res = 30L, cache_dir = root, verbose = FALSE
      )))
    }
    first <- run()
    expect_true(all(is.na(first$failure_reason)))
    expect_identical(n_calls, 3L)
    expect_length(list.files(root, pattern = "[.]rds$", recursive = TRUE), 1L)
    second <- run()
    expect_identical(n_calls, 3L)
    expect_equal(sf::st_drop_geometry(second)$site_id, sf::st_drop_geometry(first)$site_id)
  })
})

test_that("ISO-CACHE-03 a site rejected with HTTP 400 is saved with the result, and the saved copy says so", {
  testthat::skip_if_not_installed("osrm")
  root <- tempfile("cacs_iso_rejected_")
  withr::defer(unlink(root, recursive = TRUE))
  n_calls <- 0L
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      n_calls <<- n_calls + 1L
      if (isTRUE(all.equal(loc[[1]], -86.70))) {
        stop("OSRM client error (HTTP 400)")
      }
      .stop_response(loc, breaks)
    },
    .package = "catchmentACS"
  )
  with_test_cache(cache_dir = root, cleanup = FALSE, {
    run <- function() {
      suppressMessages(cacs_isochrone(
        .stop_sites(), drive_times = 5L, provider = "osrm",
        osrm_mode = "docker", res = 30L, cache_dir = root, verbose = FALSE
      ))
    }
    first <- suppressWarnings(run())
    expect_identical(sum(!is.na(first$failure_reason)), 1L)
    expect_identical(n_calls, 3L)
    expect_length(list.files(root, pattern = "[.]rds$", recursive = TRUE), 1L)
    expect_warning(second <- run(), "rejected the request",
                   class = "catchmentACS_warning_runtime")
    expect_identical(n_calls, 3L)
    expect_identical(sum(!is.na(second$failure_reason)), 1L)
  })
})

test_that("ISO-CACHE-04 a saved result with a failure that may be temporary, as earlier versions saved, is routed again", {
  testthat::skip_if_not_installed("osrm")
  root <- tempfile("cacs_iso_old_failed_")
  withr::defer(unlink(root, recursive = TRUE))
  n_calls <- 0L
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      n_calls <<- n_calls + 1L
      .stop_response(loc, breaks)
    },
    .package = "catchmentACS"
  )
  with_test_cache(cache_dir = root, cleanup = FALSE, {
    run <- function() {
      suppressWarnings(suppressMessages(cacs_isochrone(
        .stop_sites(), drive_times = 5L, provider = "osrm",
        osrm_mode = "docker", res = 30L, cache_dir = root, verbose = FALSE
      )))
    }
    first <- run()
    expect_identical(n_calls, 3L)
    key <- attr(first, "cacs_isochrone_provenance")$cache_key
    expect_true(is.character(key) && nzchar(key))
    # The same result with one failed site, saved under the same key.
    old <- first
    old$failure_reason[[2L]] <- "OSRM server error (HTTP 503)"
    expect_true(isTRUE(.cacs_cache_put(old, key, "isochrone", cache_dir = root)))
    second <- run()
    expect_identical(n_calls, 6L)
    expect_true(all(is.na(second$failure_reason)))
    third <- run()
    expect_identical(n_calls, 6L)
  })
})

test_that("ISO-STATUS-01 the HTTP status is read only where the message gives one", {
  given <- c(
    "OSRM API request failed [503]" = 503L,
    "OSRM API request failed [400]\nInvalidQuery\nQuery string malformed" = 400L,
    "OSRM client error (HTTP 429)" = 429L,
    "OSRM server returned HTTP 503: Service Unavailable" = 503L,
    "Too Many Requests (HTTP 429)." = 429L,
    "client error (429)" = 429L,
    "status 401" = 401L,
    "Openrouteservice API request failed\n[403] Access to this API has been disallowed" = 403L
  )
  for (msg in names(given)) {
    expect_identical(.extract_http_status(msg), given[[msg]], label = msg)
  }
  none <- c(
    "Failed to connect to routing.openstreetmap.de port 443 after 3 ms: Couldn't connect to server",
    "Timeout was reached: [routing.openstreetmap.de:443] Connection timed out after 10002 milliseconds",
    "Could not resolve host: routing.openstreetmap.de",
    "could not reach 200 sites"
  )
  for (msg in none) {
    expect_identical(.extract_http_status(msg), NA_integer_, label = msg)
  }
  expect_identical(.extract_http_status(NA_character_), NA_integer_)
  expect_identical(.extract_http_status(character()), NA_integer_)
  expect_identical(.extract_http_status(NULL), NA_integer_)
})

test_that("ISO-CACHE-05 a connection failure that names port 443 is tried again and the result is not saved", {
  testthat::skip_if_not_installed("osrm")
  root <- tempfile("cacs_iso_connect_")
  withr::defer(unlink(root, recursive = TRUE))
  testthat::local_mocked_bindings(
    Sys.sleep = function(time) invisible(NULL),
    .package = "base"
  )
  n_calls <- 0L
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      n_calls <<- n_calls + 1L
      if (isTRUE(all.equal(loc[[1]], -86.70))) {
        stop("Failed to connect to routing.openstreetmap.de port 443 after 3 ms: Couldn't connect to server")
      }
      .stop_response(loc, breaks)
    },
    .package = "catchmentACS"
  )
  with_test_cache(cache_dir = root, cleanup = FALSE, {
    first <- suppressWarnings(suppressMessages(cacs_isochrone(
      .stop_sites(), drive_times = 5L, provider = "osrm",
      osrm_mode = "docker", res = 30L, cache_dir = root, verbose = FALSE
    )))
    failed <- !is.na(first$failure_reason)
    expect_identical(sum(failed), 1L)
    # three attempts for the failing site, one for each of the other two
    expect_identical(n_calls, 5L)
    expect_identical(first$retry_count[failed], 3L)
    expect_length(list.files(root, pattern = "[.](rds|fingerprint)$", recursive = TRUE), 0L)
  })
})
