# Shared deterministic fixtures for Phase 6 performance-boundary tests.

.p6_default_vars <- function() {
  env <- new.env(parent = emptyenv())
  utils::data("cacs_acs_default_vars", package = "catchmentACS", envir = env)
  sort(unique(unname(env$cacs_acs_default_vars)))
}

.p6_rate_values <- function() {
  c(
    B01003_001 = 1500, B17001_001 = 1000, B17001_002 = 150,
    B19013_001 = 55000, B19301_001 = 28000, B11001_001 = 800,
    B22003_001 = 800, B22003_002 = 100,
    B19056_001 = 500, B19056_002 = 25,
    B23025_001 = 1000, B23025_002 = 650,
    B23025_003 = 600, B23025_005 = 48
  )
}

.p6_sites_tbl <- function(n = 3L) {
  base <- tibble::tibble(
    site_id = c("AL_P6_BHM", "AL_P6_MOB", "AL_P6_HSV"),
    lon = c(-86.80902, -88.03989, -86.58610),
    lat = c(33.52203, 30.69537, 34.73037)
  )
  base[seq_len(n), , drop = FALSE]
}

.p6_sites_sf <- function(n = 3L) {
  sf::st_as_sf(.p6_sites_tbl(n), coords = c("lon", "lat"), crs = 4326,
               remove = FALSE)
}

.p6_osrm_response <- function(breaks, center_lon = -86.80902,
                              center_lat = 33.52203) {
  polys <- lapply(seq_along(breaks), function(j) {
    half <- 0.001 * as.numeric(breaks[[j]])
    sf::st_polygon(list(rbind(
      c(center_lon - half, center_lat - half),
      c(center_lon + half, center_lat - half),
      c(center_lon + half, center_lat + half),
      c(center_lon - half, center_lat + half),
      c(center_lon - half, center_lat - half)
    )))
  })
  sf::st_sf(
    tibble::tibble(
      id = seq_along(breaks),
      isomin = c(0L, utils::head(as.integer(breaks), -1L)),
      isomax = as.integer(breaks)
    ),
    geometry = sf::st_sfc(polys, crs = 4326)
  )
}

.p6_iso_sf <- function(sites = .p6_sites_tbl(3L), drive_times = 10L) {
  rows <- expand.grid(
    site_id = sites$site_id,
    drive_time_min = as.integer(drive_times),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  coords <- sites[match(rows$site_id, sites$site_id), c("lon", "lat")]
  geom <- lapply(seq_len(nrow(rows)), function(i) {
    half <- 0.001 * rows$drive_time_min[[i]]
    lon <- coords$lon[[i]]
    lat <- coords$lat[[i]]
    sf::st_polygon(list(rbind(
      c(lon - half, lat - half),
      c(lon + half, lat - half),
      c(lon + half, lat + half),
      c(lon - half, lat + half),
      c(lon - half, lat - half)
    )))
  })
  out <- sf::st_sf(
    tibble::tibble(
      site_id = rows$site_id,
      drive_time_min = rows$drive_time_min,
      provider = "osrm",
      profile = "car",
      osm_snapshot_date = "unknown",
      routing_engine_version = "phase6-stub",
      polygon_simplification_tolerance = NA_real_,
      generated_at = as.POSIXct("2026-05-25 00:00:00", tz = "UTC"),
      isochrone_empty = FALSE,
      provider_requested = "osrm",
      provider_downgrade = FALSE,
      osm_snapshot_status = "unknown_best_effort",
      failure_reason = NA_character_,
      retry_count = 1L,
      ring_topology = "cumulative"
    ),
    geometry = sf::st_sfc(geom, crs = 4326)
  )
  attr(out, "cacs_isochrone_provenance") <- list(
    cache_key = "phase6-iso-fixture",
    cache_namespace = "isochrone",
    res_param = .OSRM_RES_DEFAULT,
    ring_topology = "cumulative"
  )
  attr(out, "cacs_provenance") <- attr(out, "cacs_isochrone_provenance")
  out
}

.p6_acs_sf <- function(variables = .p6_default_vars(),
                       geoids = sprintf("0107300%04d", 1:4)) {
  rows <- expand.grid(
    GEOID = geoids,
    variable = variables,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  vals <- .p6_rate_values()
  rows$NAME <- paste("Phase 6 tract", rows$GEOID)
  rows$estimate <- as.numeric(vals[rows$variable])
  rows$estimate[is.na(rows$estimate)] <- 100
  rows$moe <- pmax(sqrt(abs(rows$estimate)), 1)

  shells <- list(
    rbind(c(-87.00, 33.30), c(-86.60, 33.30), c(-86.60, 33.75),
          c(-87.00, 33.75), c(-87.00, 33.30)),
    rbind(c(-88.25, 30.50), c(-87.85, 30.50), c(-87.85, 30.90),
          c(-88.25, 30.90), c(-88.25, 30.50)),
    rbind(c(-86.80, 34.55), c(-86.35, 34.55), c(-86.35, 34.95),
          c(-86.80, 34.95), c(-86.80, 34.55)),
    rbind(c(-89.00, 32.00), c(-88.50, 32.00), c(-88.50, 32.50),
          c(-89.00, 32.50), c(-89.00, 32.00))
  )
  geom <- sf::st_sfc(lapply(shells[seq_along(geoids)], function(x) {
    sf::st_polygon(list(x))
  }), crs = 4269)
  out <- sf::st_sf(rows, geometry = geom[match(rows$GEOID, geoids)], crs = 4269)

  vars_named <- variables
  names(vars_named) <- variables
  prov <- list(
    state = "AL",
    year = 2023L,
    survey = "acs5",
    geography = "tract",
    variables = vars_named,
    variable_source = "phase6_perf_fixture",
    n_variables_requested = length(variables),
    n_variables_received = length(variables),
    n_variables_skipped = 0L,
    n_tracts = length(geoids),
    n_suppressed = 0L,
    n_estimate_suppressed = 0L,
    n_moe_sentinel = 0L,
    n_negative_moe = 0L,
    n_total_rows = nrow(out),
    cache_key = "phase6-acs-fixture",
    cache_namespace = "acs",
    generated_at = as.POSIXct("2026-05-25 00:00:00", tz = "UTC"),
    tidycensus_version = as.character(utils::packageVersion("tidycensus")),
    sf_version = as.character(utils::packageVersion("sf")),
    acs_geometry_vintage = "2023_tract"
  )
  attr(out, "cacs_provenance") <- prov
  attr(out, "cacs_acs_provenance") <- prov
  attr(out, "cacs_schema_version") <- "1.0"
  out
}

.p6_mock_codebook <- function(variables = .p6_default_vars()) {
  tibble::tibble(name = variables, label = variables, concept = "Phase 6")
}

.p6_install_network_mocks <- function(acs_calls = NULL, osrm_calls = NULL,
                                      variables = .p6_default_vars()) {
  local_env <- parent.frame()
  withr::local_envvar(c(CENSUS_API_KEY = "phase6-test-key"),
                      .local_envir = local_env)
  testthat::local_mocked_bindings(
    load_variables = function(year, dataset, ...) {
      .p6_mock_codebook(variables)
    },
    .package = "tidycensus",
    .env = local_env
  )
  testthat::local_mocked_bindings(
    .tidycensus_get_acs_call = function(geography, variables, state, year,
                                        survey) {
      if (!is.null(acs_calls)) acs_calls$n <- acs_calls$n + 1L
      .p6_acs_sf(variables)
    },
    .osrm_call_isochrone = function(loc, breaks, res = NULL) {
      if (!is.null(osrm_calls)) {
        osrm_calls$n <- osrm_calls$n + 1L
        osrm_calls$res <- c(osrm_calls$res, res)
      }
      .p6_osrm_response(breaks, center_lon = loc[[1]], center_lat = loc[[2]])
    },
    .package = "catchmentACS",
    .env = local_env
  )
  invisible(TRUE)
}

.p6_counter_row <- function(namespace) {
  rows <- cacs_get_cache_state()$counters[[1]]
  rows[rows$namespace == namespace, , drop = FALSE]
}

.p6_geom_wkb_digest <- function(x) {
  digest::digest(sf::st_as_binary(sf::st_geometry(x), EWKB = TRUE),
                 algo = "sha256", serialize = TRUE)
}

.p6_vertex_count <- function(geom) {
  sum(vapply(sf::st_geometry(geom), function(g) nrow(sf::st_coordinates(g)),
             integer(1)))
}
