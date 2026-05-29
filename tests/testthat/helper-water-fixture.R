# helper-water-fixture.R — loader + skip guard for BUG-001 water-tract fixture
#
# v0.2.0 Step 1.4. Packaged offline fixture
# `inst/testdata/fixture_water_tract_baldwin.rds` reproduces BUG-001:
# water tract GEOID 01003990000 (Baldwin Co., AL) carries ALAND = 0 and a
# degenerate geometry that v0.1 aborted on inside `cacs_intersect_weight()`.
# v0.2.0 Step 3.1 closed BUG-001 via a 2-layer fix:
#   * Layer (a) `.drop_water_tracts()` in `cacs_acs_prefetch()`: regex
#     + zero-area drop, ON by default, opt-out via `drop_water_tracts = FALSE`.
#   * Layer (b) `cacs_intersect_weight()`: warn-and-skip on partial
#     degeneracy (preserves fail-loud when ALL tracts are degenerate).
# The fixture remains the canonical regression input: tests use it to
# exercise both layers and to verify the v0.1 abort no longer fires.
# It contains the offending water tract plus 2 real Baldwin land tracts
# for realistic intersection-context (3 rows × 6 cols per §20.5 schema).
#
# Provenance: the packaged fixture was built from a live AL 2023 ACS pull, then
# frozen for offline tests. Regenerate via
# `Rscript data-raw/build-water-fixture.R` with `CENSUS_API_KEY` set.
#
# Cross-ref: v020-plan Step 1.4; §16.1 (helper convention); §21 (intersect
# weight); §25.2 (fixture catalogue).


#' Load the BUG-001 water-tract fixture
#'
#' Returns the installed `sf` tibble (3 rows × GEOID/NAME/variable/estimate/
#' moe/geometry) for use in `cacs_intersect_weight()` regression tests.
#'
#' @return An `sf` tibble in EPSG:4269 with 3 Baldwin Co. tracts (2 land
#'   + 1 water).
#' @keywords internal
#' @noRd
load_water_fixture <- function() {
  path <- system.file(
    "testdata", "fixture_water_tract_baldwin.rds",
    package = "catchmentACS"
  )
  if (!nzchar(path)) {
    stop(
      "fixture_water_tract_baldwin.rds not installed; ",
      "run `Rscript data-raw/build-water-fixture.R` and reinstall.",
      call. = FALSE
    )
  }
  readRDS(path)
}


#' Skip the calling test if the water-tract fixture is not installed
#'
#' Defensive for CI runners (`devtools::test()` without prior install) and
#' for source-tree test executions where `inst/testdata/` may not yet be
#' on the installed package path.
#'
#' @keywords internal
#' @noRd
skip_if_no_water_fixture <- function() {
  path <- system.file(
    "testdata", "fixture_water_tract_baldwin.rds",
    package = "catchmentACS"
  )
  if (!nzchar(path)) {
    testthat::skip(
      "fixture_water_tract_baldwin.rds not installed; skipping BUG-001 test."
    )
  }
  invisible(TRUE)
}
