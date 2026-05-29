# ============================================================================
# Unit tests for R/checks.R preflight + cacs_acs_validate() (8 cases).
# ============================================================================


.mk_acs_sf_local <- function(...) {
  geom <- sf::st_sfc(
    sf::st_multipolygon(list(list(rbind(
      c(-87, 33), c(-86, 33), c(-86, 34), c(-87, 34), c(-87, 33)
    )))),
    crs = 4269
  )
  base <- tibble::tibble(
    GEOID    = "01001000001",
    NAME     = "Census Tract 1, Autauga County, Alabama",
    variable = "B01003_001",
    estimate = 1000,
    moe      = 50
  )
  out <- sf::st_sf(base, geometry = geom)
  mods <- list(...)
  for (nm in names(mods)) out[[nm]] <- mods[[nm]]
  out
}


test_that("T-CHECKS-01 .check_osrm_reachable returns TRUE when osrm installed", {
  skip_if_not_installed("osrm")
  expect_true(.check_osrm_reachable())
})

test_that("T-CHECKS-02 .check_ors_token returns TRUE with non-empty key", {
  skip_if_not_installed("openrouteservice")
  expect_true(.check_ors_token(api_key = "fake_test_token"))
})

test_that("T-CHECKS-03 .check_mapbox_token aborts with current-release hint", {
  expect_error(.check_mapbox_token(),
               class = "catchmentACS_error_credential")
  err <- tryCatch(.check_mapbox_token(), error = function(e) e)
  expect_match(conditionMessage(err), "current release", ignore.case = FALSE)
})

test_that("T-CHECKS-04 .check_r5r_core aborts with current-release hint", {
  expect_error(.check_r5r_core(),
               class = "catchmentACS_error_credential")
  err <- tryCatch(.check_r5r_core(), error = function(e) e)
  expect_match(conditionMessage(err), "current release", ignore.case = FALSE)
})

test_that("T-CHECKS-05 cacs_acs_validate returns TRUE on valid Path B sf", {
  acs <- .mk_acs_sf_local()
  expect_true(cacs_acs_validate(acs))
})

test_that("T-CHECKS-06 cacs_acs_validate aborts on bad GEOID", {
  acs <- .mk_acs_sf_local()
  acs$GEOID <- "BAD_GEOID"
  expect_error(cacs_acs_validate(acs),
               class = "catchmentACS_error_schema")
})

test_that("T-CHECKS-07 cacs_acs_validate has no abort=FALSE escape on exported surface", {
  expect_false("abort" %in% names(formals(cacs_acs_validate)))
})

test_that("T-CHECKS-08 cacs_acs_validate aborts on bad CRS (Mapbox-irrelevant)", {
  acs <- sf::st_transform(.mk_acs_sf_local(), 4326)
  expect_error(cacs_acs_validate(acs),
               class = "catchmentACS_error_schema")
})
