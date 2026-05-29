# ============================================================================
# C-03 lock tests: water-tract filter emits under Rscript-visible conditions.
# ============================================================================


.c03_water_sf <- function() {
  sf::st_sf(
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
}


test_that("C03-RSCRIPT-01 .drop_water_tracts emits water condition live", {
  out <- cacs_capture_conditions(
    .drop_water_tracts(.c03_water_sf(), drop_water_tracts = TRUE,
                       verbose = TRUE),
    classes = "water_tract_filter"
  )

  expect_equal(nrow(out), 1L)
  expect_equal(out$class, "catchmentACS_message_water_tract_filter")
  expect_equal(out$phase, "acs")
  expect_match(out$message, "01003990000", fixed = TRUE)
})


test_that("C03-RSCRIPT-02 drop_water_tracts = FALSE emits no water condition", {
  out <- cacs_capture_conditions(
    .drop_water_tracts(.c03_water_sf(), drop_water_tracts = FALSE,
                       verbose = TRUE),
    classes = "water_tract_filter"
  )

  expect_equal(nrow(out), 0L)
})


test_that("C03-RSCRIPT-03 water condition is visible in Rscript subprocess", {
  script <- tempfile("c03-water-rscript-", fileext = ".R")
  out_file <- tempfile("c03-water-rscript-", fileext = ".rds")
  pkg_path <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  writeLines(c(
    "library(cli)",
    "library(rlang)",
    "library(sf)",
    "library(tibble)",
    sprintf("pkg_path <- %s", shQuote(pkg_path)),
    "if (file.exists(file.path(pkg_path, 'R', 'cli.R'))) {",
    "  source(file.path(pkg_path, 'R', 'cli.R'))",
    "  source(file.path(pkg_path, 'R', 'capture-conditions.R'))",
    "  source(file.path(pkg_path, 'R', 'acs-prefetch.R'))",
    "  drop_fun <- .drop_water_tracts",
    "} else {",
    "  library(catchmentACS)",
    "  drop_fun <- getFromNamespace('.drop_water_tracts', 'catchmentACS')",
    "}",
    "acs <- sf::st_sf(",
    "  tibble::tibble(GEOID = c('01003990000', '01003010100'),",
    "                 NAME = c('Water tract', 'Land tract'),",
    "                 variable = 'B01003_001',",
    "                 estimate = c(0, 100), moe = c(0, 10)),",
    "  geometry = sf::st_sfc(",
    "    sf::st_polygon(list(rbind(c(-87.80,30.60), c(-87.79,30.60), c(-87.79,30.61), c(-87.80,30.61), c(-87.80,30.60)))),",
    "    sf::st_polygon(list(rbind(c(-87.78,30.60), c(-87.77,30.60), c(-87.77,30.61), c(-87.78,30.61), c(-87.78,30.60)))),",
    "    crs = 4269))",
    "out <- cacs_capture_conditions(",
    "  drop_fun(acs, TRUE, TRUE),",
    "  classes = 'water_tract_filter')",
    sprintf("saveRDS(out, %s)", shQuote(out_file))
  ), script)
  withr::defer(unlink(script))
  withr::defer(unlink(out_file))

  res <- system2(file.path(R.home("bin"), "Rscript"),
                 c("--vanilla", script),
                 stdout = TRUE, stderr = TRUE)
  expect_null(attr(res, "status"), info = paste(res, collapse = "\n"))
  out <- readRDS(out_file)

  expect_equal(nrow(out), 1L)
  expect_equal(out$class[[1L]], "catchmentACS_message_water_tract_filter")
  expect_equal(out$phase[[1L]], "acs")
  expect_false(any(grepl("Dropped 1 water", res, fixed = TRUE)))
})


test_that("C03-RSCRIPT-04 cache-hit replay remains locked by Step 5.2 tests", {
  expect_true(file.exists(testthat::test_path("test-c03-cache-hit-emits.R")))
  expect_true("catchmentACS_message_water_tract_filter" %in%
                .cacs_known_classes(include_parents = TRUE))
})
