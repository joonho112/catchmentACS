# The map functions use the bundled cacs_alabama_sites table when sites_df is
# NULL. The table must be found when the package is loaded but not attached
# (a script that calls catchmentACS:: without library()), and an object with
# the same name in the global environment must not take its place. Both
# situations need a new R process, so these tests run one with the installed
# package and skip when the package under test is not an installed copy.

.plot_sites_run <- local({
  result <- NULL
  function() {
    if (!is.null(result)) {
      return(result)
    }
    pkg_path <- getNamespaceInfo("catchmentACS", "path")
    testthat::skip_if_not(
      basename(pkg_path) == "catchmentACS" &&
        file.exists(file.path(pkg_path, "Meta", "package.rds")),
      "needs the installed package"
    )
    script <- tempfile("plot-bundled-sites-", fileext = ".R")
    out <- tempfile("plot-bundled-sites-", fileext = ".rds")
    on.exit(unlink(c(script, out)), add = TRUE)
    writeLines(c(
      sprintf(".libPaths(c(%s, .libPaths()))", deparse(dirname(pkg_path))),
      "options(catchmentACS.cache_enabled = FALSE)",
      "try_value <- function(expr) tryCatch(expr, error = function(e) paste('error:', conditionMessage(e)))",
      "res <- list()",
      "loadNamespace('catchmentACS')",
      "res$attached <- 'package:catchmentACS' %in% search()",
      "sites <- catchmentACS::cacs_alabama_sites",
      "res$bundled <- as.numeric(sf::st_coordinates(sites[sites$site_id == 'AL_SITE_01', ])[1, c('Y', 'X')])",
      "res$by_id <- try_value({",
      "  r <- catchmentACS:::.cacs_resolve_site_input(site_id = 'AL_SITE_01')",
      "  c(r$lat, r$lon)",
      "})",
      "res$by_latlon <- try_value(catchmentACS:::.resolve_site_from_latlon(",
      "  lat = res$bundled[[1]] + 0.001, lon = res$bundled[[2]], sites_df = NULL,",
      "  run_result = data.frame(site_id = 'AL_SITE_01'))$site_id)",
      "res$map <- if (requireNamespace('leaflet', quietly = TRUE)) try_value({",
      "  iso <- readRDS(system.file('extdata', 'legacy_2025_isochrones.rds', package = 'catchmentACS'))",
      "  class(catchmentACS::cacs_plot_site_isochrone(site_id = 'AL_SITE_01', iso_sf = iso))[[1]]",
      "}) else NA_character_",
      "assign('cacs_alabama_sites', sf::st_sf(site_id = 'AL_SITE_01',",
      "  geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)), envir = globalenv())",
      "suppressPackageStartupMessages(library(catchmentACS))",
      "res$shadowed <- try_value({",
      "  r <- catchmentACS:::.cacs_resolve_site_input(site_id = 'AL_SITE_01')",
      "  c(r$lat, r$lon)",
      "})",
      sprintf("saveRDS(res, %s)", deparse(out))
    ), script)
    log <- suppressWarnings(system2(
      file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(script)),
      stdout = TRUE, stderr = TRUE
    ))
    if (!file.exists(out)) {
      testthat::fail(paste(c("The R process gave no result:", log), collapse = "\n"))
      return(NULL)
    }
    result <<- readRDS(out)
    result
  }
})

test_that("PLOT-SITES-01 the bundled site table is found when the package is not attached", {
  res <- .plot_sites_run()
  expect_false(res$attached)
  expect_type(res$by_id, "double")
  expect_equal(res$by_id, res$bundled)
  expect_identical(res$by_latlon, "AL_SITE_01")
  if (!is.na(res$map)) {
    expect_identical(res$map, "leaflet")
  }
})

test_that("PLOT-SITES-02 an object named cacs_alabama_sites in the global environment does not replace the bundled table", {
  res <- .plot_sites_run()
  expect_type(res$shadowed, "double")
  expect_equal(res$shadowed, res$bundled)
})
