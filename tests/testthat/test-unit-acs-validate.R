# ============================================================================
# Unit tests for cacs_acs_validate() (Phase 4.4 + already-existing Phase 2.3)
# 4 cases verifying Path B exported wrapper works against sample_alabama_subset.
# ============================================================================


test_that("T-VALIDATE-01 sample_alabama_subset.rds passes cacs_acs_validate", {
  path <- system.file("extdata", "sample_alabama_subset.rds",
                      package = "catchmentACS")
  skip_if(!nzchar(path) || !file.exists(path),
          "sample_alabama_subset.rds not yet bundled in inst/extdata/")
  acs <- readRDS(path)
  expect_true(cacs_acs_validate(acs))
})

test_that("T-VALIDATE-02 sample_alabama_subset has expected schema", {
  path <- system.file("extdata", "sample_alabama_subset.rds",
                      package = "catchmentACS")
  skip_if(!nzchar(path) || !file.exists(path),
          "sample_alabama_subset.rds not yet bundled")
  acs <- readRDS(path)
  expect_s3_class(acs, "sf")
  expect_equal(sf::st_crs(acs)$epsg, 4269L)
  expect_true(all(c("GEOID", "NAME", "variable", "estimate", "moe", "geometry") %in% names(acs)))
  expect_gt(nrow(acs), 1000L)
})

test_that("T-VALIDATE-03 cacs_acs_validate has correct exported signature", {
  expect_true(exists("cacs_acs_validate", envir = asNamespace("catchmentACS")))
  expect_false("abort" %in% names(formals(cacs_acs_validate)))
})

test_that("T-VALIDATE-04 live tidycensus smoke (gated by CENSUS_API_KEY)", {
  skip_if_no_census_api()
  skip_if_not_live_provider()
  # Live smoke test — Phase 9 advisor review only
  result <- cacs_acs_prefetch(
    state = "AL", year = 2023, survey = "acs5", geography = "tract",
    variables = "B19013_001", verbose = FALSE
  )
  expect_true(cacs_acs_validate(result))
})
