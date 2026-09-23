# ============================================================================
# C-11 planar CRS noise suppression tests.
# ============================================================================


.c11_mk_iso_4326 <- function() {
  ring <- rbind(
    c(-87.75, 30.60), c(-87.55, 30.60),
    c(-87.55, 30.80), c(-87.75, 30.80),
    c(-87.75, 30.60)
  )
  sf::st_sf(
    tibble::tibble(
      site_id                          = "C11_SITE",
      drive_time_min                   = 15L,
      provider                         = "osrm",
      profile                          = "car",
      osm_snapshot_date                = NA_character_,
      routing_engine_version           = "stub/0.0.0",
      polygon_simplification_tolerance = NA_real_,
      generated_at                     = Sys.time(),
      isochrone_empty                  = FALSE,
      provider_requested               = "osrm",
      provider_downgrade               = FALSE,
      osm_snapshot_status              = NA_character_,
      failure_reason                   = NA_character_,
      retry_count                      = 0L,
      ring_topology                    = "cumulative"
    ),
    geometry = sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)
  )
}


.c11_mk_acs_4326_for_internal_call <- function() {
  ring <- rbind(
    c(-87.72, 30.62), c(-87.58, 30.62),
    c(-87.58, 30.78), c(-87.72, 30.78),
    c(-87.72, 30.62)
  )
  acs <- sf::st_sf(
    tibble::tibble(
      GEOID = "01003999999",
      NAME = "C11 synthetic tract",
      variable = "B01003_001",
      estimate = 100,
      moe = 10
    ),
    geometry = sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)
  )
  acs$tract_area_m2 <- suppressWarnings(
    as.numeric(sf::st_area(sf::st_transform(acs, 5070)))
  ) * 1.0001
  acs
}


test_that("C-11 internal per-site intersections muffle redundant planar sf messages", {
  # The inputs are longitude/latitude with s2 off, so that st_intersection()
  # gives its planar notice. The overlap areas are then measured after a
  # projection to EPSG:5070, where .intersect_one_site() expects its inputs to
  # be: with s2 off, sf measures longitude/latitude only with the lwgeom
  # package, which is not installed with sf. The notice is first looked for
  # without muffling: if this version of sf gives none for these inputs, or
  # words it another way, there is nothing to check and the test skips.
  prev_s2 <- suppressWarnings(sf::sf_use_s2())
  suppressMessages(sf::sf_use_s2(FALSE))
  withr::defer(suppressMessages(sf::sf_use_s2(prev_s2)))
  is_planar_notice <- function(msg) {
    grepl("longitude/latitude", msg, ignore.case = TRUE) &&
      grepl("planar", msg, ignore.case = TRUE)
  }
  unmuffled <- character()
  withCallingHandlers(
    suppressWarnings(sf::st_intersection(
      .c11_mk_iso_4326(),
      .c11_mk_acs_4326_for_internal_call()
    )),
    message = function(m) {
      if (is_planar_notice(conditionMessage(m))) {
        unmuffled <<- c(unmuffled, conditionMessage(m))
      }
      invokeRestart("muffleMessage")
    }
  )
  if (length(unmuffled) == 0L) {
    testthat::skip("sf gives no planar notice for these inputs")
  }
  sf_area <- sf::st_area
  testthat::local_mocked_bindings(
    st_area = function(x, ...) {
      if (isTRUE(sf::st_is_longlat(x))) {
        x <- sf::st_transform(x, 5070)
      }
      sf_area(x, ...)
    },
    .package = "sf"
  )

  planar_messages <- character()
  out <- withCallingHandlers(
    .intersect_one_site(
      sid = "C11_SITE",
      dt = 15L,
      iso_5070 = .c11_mk_iso_4326(),
      acs_5070 = .c11_mk_acs_4326_for_internal_call(),
      min_weight = 1e-6
    ),
    message = function(m) {
      msg <- conditionMessage(m)
      if (is_planar_notice(msg)) {
        planar_messages <<- c(planar_messages, msg)
      }
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      if (grepl("package 'sf' was built", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  expect_equal(planar_messages, character(0))
  expect_s3_class(out, "sf")
  expect_gt(nrow(out), 0L)
})


test_that("C-11 public catchmentACS warnings are not over-suppressed", {
  skip_if_no_water_fixture()
  fixture <- load_water_fixture()
  iso <- .c11_mk_iso_4326()

  withr::local_options(catchmentACS.cache_intersect = FALSE)

  geometry_skip_seen <- FALSE
  out <- withCallingHandlers(
    suppressMessages(
      cacs_intersect_weight(
        iso_sf = iso,
        acs_sf = fixture,
        verbose = FALSE
      )
    ),
    catchmentACS_warning_geometry_skip = function(w) {
      geometry_skip_seen <<- TRUE
      invokeRestart("muffleWarning")
    },
    catchmentACS_warning_provenance = function(w) invokeRestart("muffleWarning")
  )

  expect_true(geometry_skip_seen)
  expect_s3_class(out, "tbl_df")
  expect_true("01003990000" %in% attr(out, "skipped_geoids"))
})


test_that("C-11 internal sf noise helper is narrowly targeted", {
  expect_silent(
    .cacs_muffle_internal_sf_planar_noise(
      warning("although coordinates are longitude/latitude, st_intersection assumes that they are planar")
    )
  )
  expect_silent(
    .cacs_muffle_internal_sf_planar_noise(
      message("although coordinates are longitude/latitude, st_intersects assumes that they are planar")
    )
  )
  expect_silent(
    .cacs_muffle_internal_sf_planar_noise(
      warning("attribute variables are assumed to be spatially constant throughout all geometries")
    )
  )
  expect_message(
    .cacs_muffle_internal_sf_planar_noise(message("ordinary sf message")),
    "ordinary sf message"
  )
  expect_warning(
    .cacs_muffle_internal_sf_planar_noise(warning("ordinary GEOS warning")),
    "ordinary GEOS warning"
  )
})
