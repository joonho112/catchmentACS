# helper-mocks.R — local_mocked_bindings() helpers + httptest2 fixture loaders
#
# Hosts:
#   - helper_mock_acs_long_sf() — tidycensus::get_acs() return value mock
#       (synthetic AL state long-format sf, EPSG:4269, 6 required columns)
#   - helper_mock_codebook()    — tidycensus::load_variables() return mock
#   - helper_mock_osrm_response() — osrm::osrmIsochrone() mock (Step 3.2)
#   - helper_mock_ors_response()  — openrouteservice::ors_isochrones() mock
#   - helper_with_mock_dir()      — httptest2::with_mock_dir() wrapper
#
# Cross-ref: §16.1 + §16.5 (mocking strategy), §19.8 + §20.7 (test cases).


#' Build a deterministic long-format ACS sf for use with
#' \code{testthat::local_mocked_bindings(.tidycensus_get_acs_call = ...)}
#'
#' The returned sf carries the 6 canonical columns required by
#' \code{.validate_acs_schema()} (\code{GEOID}, \code{NAME}, \code{variable},
#' \code{estimate}, \code{moe}, \code{geometry}) in EPSG:4269 (NAD83) so it
#' passes \code{cacs_acs_prefetch()} Step 7 schema validation. The mock is
#' state-aware (defaults to \code{"AL"}), variable-aware, and tract-count
#' parameterized so individual tests can shape rows without touching the
#' tidycensus HTTP path.
#'
#' @param state Character(1) USPS code — included in the synthetic
#'   \code{NAME} column. The 2-digit FIPS prefix is hardcoded to
#'   \code{"01"} (Alabama) because the synthetic GEOIDs in v0.1 only
#'   need to be well-formed 11-digit strings; tests that need other
#'   states can override via the \code{geoid_prefix} argument.
#' @param year Integer ACS terminal year (passed through unchanged).
#' @param variables Character vector of ACS codes (\code{NULL} → a single
#'   \code{"B19013_001"}).
#' @param n_tract Integer tract count.
#' @param geoid_prefix Character(2) 2-digit state FIPS to use when
#'   constructing GEOIDs; defaults to \code{"01"} (Alabama).
#' @param suppression_pct Numeric in [0, 1]; fraction of rows with
#'   \code{estimate = NA} to simulate Bureau suppression (used by Step
#'   4.2 integration tests).
#' @param moe_sentinel_pct Numeric in [0, 1]; fraction of rows with
#'   \code{moe = -555555555} to simulate Bureau MOE sentinels.
#' @return An sf tibble with the 6 §20.5 canonical columns + EPSG:4269.
helper_mock_acs_long_sf <- function(state            = "AL",
                                    year             = 2023L,
                                    variables        = NULL,
                                    n_tract          = 5L,
                                    geoid_prefix     = "01",
                                    suppression_pct  = 0,
                                    moe_sentinel_pct = 0) {
  if (is.null(variables) || length(variables) == 0L) {
    variables <- "B19013_001"
  }
  geoid <- sprintf("%s%03d%06d", geoid_prefix,
                   seq_len(n_tract),
                   seq_len(n_tract) * 100L)

  # Per-tract polygons (small, non-overlapping squares)
  geom_list <- lapply(seq_len(n_tract), function(i) {
    lon0 <- -86.80 + 0.02 * i
    lat0 <-  33.50 + 0.02 * i
    sf::st_polygon(list(rbind(
      c(lon0,        lat0),
      c(lon0 + 0.01, lat0),
      c(lon0 + 0.01, lat0 + 0.01),
      c(lon0,        lat0 + 0.01),
      c(lon0,        lat0)
    )))
  })
  geom <- sf::st_sfc(geom_list, crs = 4269)

  rows <- expand.grid(
    GEOID    = geoid,
    variable = variables,
    KEEP.OUT.ATTRS   = FALSE,
    stringsAsFactors = FALSE
  )
  rows$NAME     <- paste0("Tract ", rows$GEOID, ", ", state)
  rows$estimate <- as.numeric(seq_len(nrow(rows)) * 1000L)
  rows$moe      <- as.numeric(seq_len(nrow(rows)) * 10L)

  # Inject suppression / sentinels per Step 4.2 fixtures
  if (suppression_pct > 0) {
    n_supp <- ceiling(nrow(rows) * suppression_pct)
    rows$estimate[seq_len(n_supp)] <- NA_real_
  }
  if (moe_sentinel_pct > 0) {
    n_sent <- ceiling(nrow(rows) * moe_sentinel_pct)
    idx <- seq.int(length.out = n_sent, from = nrow(rows) - n_sent + 1L)
    rows$moe[idx] <- -555555555
  }

  geom_per_row <- geom[match(rows$GEOID, geoid)]
  sf::st_sf(rows, geometry = geom_per_row, crs = 4269)
}


# helper_mock_codebook(): deterministic tidycensus::load_variables() return.
helper_mock_codebook <- function(known = "B19013_001") {
  tibble::tibble(name = unique(known), label = "Mock", concept = "Mock")
}


# Cross-step placeholders (filled in later phases).
# helper_mock_osrm_response()   — Phase 3 OSRM mock fixture
# helper_mock_ors_response()    — Phase 3 ORS mock fixture
# helper_with_mock_dir()        — Phase 3+ httptest2::with_mock_dir() wrapper
