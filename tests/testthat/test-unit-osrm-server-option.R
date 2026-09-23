# The osrm package sets the options osrm.server and osrm.profile to its own
# defaults when it is loaded, which can happen during the first
# cacs_isochrone() call of a session. These tests imitate that load inside
# the call and check that a server set by the user stays in use over repeated
# calls and is recorded as the server of each result. The routing call is
# mocked and records the server osrm would use; no request is sent.

.srv_site <- function(lon) {
  sf::st_as_sf(data.frame(site_id = "S1", lon = lon, lat = 33.5),
               coords = c("lon", "lat"), crs = 4326)
}

.srv_response <- function(loc, breaks) {
  h <- 0.01 * breaks[[1]]
  sf::st_sf(
    data.frame(id = 1L, isomin = 0L, isomax = as.integer(breaks[[1]])),
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(loc[1] - h, loc[2] - h), c(loc[1] + h, loc[2] - h),
      c(loc[1] + h, loc[2] + h), c(loc[1] - h, loc[2] + h),
      c(loc[1] - h, loc[2] - h)
    ))), crs = 4326)
  )
}

.srv_mock <- function(env, targets) {
  testthat::local_mocked_bindings(
    check_installed = function(pkg, ...) {
      if (identical(pkg, "osrm")) {
        # What osrm's .onLoad() does.
        options(osrm.server = "https://routing.openstreetmap.de/",
                osrm.profile = "car")
      }
      invisible(TRUE)
    },
    .package = "rlang",
    .env = env
  )
  testthat::local_mocked_bindings(
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      targets$server <- c(targets$server, getOption("osrm.server"))
      .srv_response(loc, breaks)
    },
    .package = "catchmentACS",
    .env = env
  )
}

.srv_run <- function(lon, ...) {
  suppressMessages(cacs_isochrone(
    .srv_site(lon), drive_times = 5L, provider = "osrm", osrm_mode = "demo",
    res = 30L, verbose = FALSE, ...
  ))
}

test_that("OSRM-SERVER-01 a server set in options(osrm.server = ) stays in use over repeated calls", {
  withr::local_options(catchmentACS.cache_enabled = FALSE,
                       osrm.server = "http://127.0.0.1:5999/")
  targets <- new.env()
  .srv_mock(environment(), targets)
  servers <- vapply(1:3, function(k) {
    attr(.srv_run(-86.8 + k / 100), "cacs_isochrone_provenance")$osrm_server
  }, character(1))
  expect_identical(targets$server, rep("http://127.0.0.1:5999/", 3L))
  expect_identical(servers, rep("http://127.0.0.1:5999/", 3L))
  expect_identical(getOption("osrm.server"), "http://127.0.0.1:5999/")
})

test_that("OSRM-SERVER-02 without a user server, the demo server is used and osrm's own default is left in place", {
  withr::local_options(catchmentACS.cache_enabled = FALSE, osrm.server = NULL)
  targets <- new.env()
  .srv_mock(environment(), targets)
  out <- .srv_run(-86.8)
  expect_identical(targets$server, "https://routing.openstreetmap.de/")
  expect_identical(attr(out, "cacs_isochrone_provenance")$osrm_server,
                   "https://routing.openstreetmap.de/")
  expect_identical(getOption("osrm.server"), "https://routing.openstreetmap.de/")
})

test_that("OSRM-SERVER-03 a server given to one call is used for it and the option keeps its value", {
  withr::local_options(catchmentACS.cache_enabled = FALSE,
                       osrm.server = "http://127.0.0.1:5999/")
  targets <- new.env()
  .srv_mock(environment(), targets)
  out <- .srv_run(-86.8, osrm.server = "http://127.0.0.1:6001/")
  expect_identical(targets$server, "http://127.0.0.1:6001/")
  expect_identical(attr(out, "cacs_isochrone_provenance")$osrm_server,
                   "http://127.0.0.1:6001/")
  expect_identical(getOption("osrm.server"), "http://127.0.0.1:5999/")
  .srv_run(-86.7)
  expect_identical(targets$server[[2]], "http://127.0.0.1:5999/")
})
