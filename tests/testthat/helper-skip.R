# helper-skip.R — opt-in/skip guards for test-*.R files
#
# Hosts:
#   - skip_if_not_live_provider() — skip live API tests unless CACS_LIVE_PROVIDER=true
#   - skip_if_not_regression() — skip regression suite unless CACS_REGRESSION=true
#   - skip_if_no_census_api() — skip live Census API tests unless CENSUS_API_KEY set
#
# Cross-ref: §16.1 (helper convention), §16.3 (CI tier pyramid), §26.4.


#' Skip if CACS_LIVE_PROVIDER environment variable is not set to "true"
#'
#' Use this to gate any test that hits a live external provider (OSRM,
#' ORS, Mapbox, r5r) without mocking. Default is skip — live tests are
#' opt-in only.
#'
#' @keywords internal
#' @noRd
skip_if_not_live_provider <- function() {
  if (identical(tolower(Sys.getenv("CACS_LIVE_PROVIDER", "")), "true")) {
    return(invisible(TRUE))
  }
  testthat::skip("CACS_LIVE_PROVIDER not set; skipping live-provider test.")
}


#' Skip if CACS_REGRESSION environment variable is not "true"
#'
#' Use this to gate the §26 regression test suite (heavy fixtures,
#' ε=1e-6/1e-5/1e-9 tolerance). Runs on main-branch push CI tier 2 only.
#'
#' @keywords internal
#' @noRd
skip_if_not_regression <- function() {
  if (identical(tolower(Sys.getenv("CACS_REGRESSION", "")), "true")) {
    return(invisible(TRUE))
  }
  testthat::skip("CACS_REGRESSION not set; skipping regression test.")
}


#' Skip if no CENSUS_API_KEY environment variable
#'
#' Use this to gate live `tidycensus::get_acs()` calls. Production user
#' must set their key via `Sys.setenv(CENSUS_API_KEY = ...)`.
#'
#' @keywords internal
#' @noRd
skip_if_no_census_api <- function() {
  if (nzchar(Sys.getenv("CENSUS_API_KEY"))) {
    return(invisible(TRUE))
  }
  testthat::skip("CENSUS_API_KEY not set; skipping live Census API test.")
}
