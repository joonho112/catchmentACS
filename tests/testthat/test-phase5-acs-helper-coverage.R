# ============================================================================
# Phase 5 coverage helpers for ACS prefetch support functions.
# ============================================================================


test_that("P5-COV-ACS-01 variable resolver covers all public modes", {
  default <- .resolve_variables(NULL)
  core <- .resolve_variables("core")
  extended <- .resolve_variables("extended")
  explicit <- .resolve_variables(c("B01003_001", "B01003_001", "B17001_002"))

  expect_equal(default$source, "default_catalogue")
  expect_equal(core$source, "core_alias")
  expect_equal(extended$source, "extended_alias")
  expect_equal(default$vars, core$vars)
  expect_equal(core$vars, extended$vars)
  expect_equal(explicit$source, "user_supplied")
  expect_equal(explicit$vars, c("B01003_001", "B17001_002"))
})


test_that("P5-COV-ACS-02 variable resolver fails loudly on bad inputs", {
  expect_error(
    .resolve_variables(c("B01003_001", NA_character_)),
    class = "catchmentACS_error_schema"
  )
  expect_error(
    .resolve_variables("not_an_acs_code"),
    class = "catchmentACS_error_schema"
  )
  expect_error(
    .resolve_variables(123),
    class = "catchmentACS_error_schema"
  )
})


test_that("P5-COV-ACS-03 tidycensus error classifier buckets messages", {
  c5 <- .classify_tidycensus_error(simpleError("503 server error"))
  c4 <- .classify_tidycensus_error(simpleError("404 not found"))
  cu <- .classify_tidycensus_error(simpleError("unexpected parse failure"))

  expect_s3_class(c5, "tidycensus_5xx")
  expect_s3_class(c4, "tidycensus_4xx")
  expect_s3_class(cu, "tidycensus_unknown")
  expect_equal(attr(c5, "cacs_msg"), "503 server error")
})


test_that("P5-COV-ACS-04 state and cache-dir helpers cover success/failure", {
  expect_true(.validate_state_in_fips("AL"))
  expect_true(.validate_state_in_fips("01"))
  expect_error(
    .validate_state_in_fips("ZZ"),
    class = "catchmentACS_error_schema"
  )

  parent <- tempfile("phase5-cache-parent-")
  dir.create(parent)
  cache_dir <- file.path(parent, "acs-cache")
  expect_match(.resolve_cache_dir(cache_dir), "acs-cache")
  expect_true(dir.exists(cache_dir))
  expect_error(
    .resolve_cache_dir(file.path(parent, "missing-parent", "cache")),
    class = "catchmentACS_error_operator"
  )
})


test_that("P5-COV-ACS-05 drop-water helper handles empty and opt-out paths", {
  empty <- sf::st_sf(
    tibble::tibble(GEOID = character(), NAME = character(),
                   variable = character(), estimate = numeric(),
                   moe = numeric()),
    geometry = sf::st_sfc(crs = 4269)
  )
  empty_out <- .drop_water_tracts(empty, drop_water_tracts = TRUE,
                                  verbose = TRUE)
  expect_equal(nrow(empty_out$kept_sf), 0L)
  expect_equal(empty_out$dropped_geoids, character(0))

  acs <- sf::st_sf(
    tibble::tibble(
      GEOID = c("01003990000", "01003010100"),
      NAME = c("Water tract", "Land tract"),
      variable = "B01003_001",
      estimate = c(0, 100),
      moe = c(0, 10)
    ),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-87.80, 30.60), c(-87.79, 30.60),
        c(-87.79, 30.61), c(-87.80, 30.61),
        c(-87.80, 30.60)
      ))),
      sf::st_polygon(list(rbind(
        c(-87.78, 30.60), c(-87.77, 30.60),
        c(-87.77, 30.61), c(-87.78, 30.61),
        c(-87.78, 30.60)
      ))),
      crs = 4269
    )
  )
  opt_out <- .drop_water_tracts(acs, drop_water_tracts = FALSE,
                                verbose = TRUE)
  expect_equal(nrow(opt_out$kept_sf), nrow(acs))
  expect_equal(opt_out$dropped_reasons, character(0))
})
