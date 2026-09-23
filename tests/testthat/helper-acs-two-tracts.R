# A drive-time area over two made-up tracts, in the coordinate reference
# systems cacs_intersect_weight() expects (EPSG:4326 for the area, EPSG:4269
# for the tracts). Used by the checks of repeated rows and annotation codes.
helper_acs_two_tracts <- function(estimate = c(1000, 2000), moe = c(50, 80)) {
  cx <- -86.80
  cy <- 33.50
  iso <- sf::st_sf(
    tibble::tibble(
      site_id = "S01", drive_time_min = 15L, provider = "osrm", profile = "car",
      osm_snapshot_date = NA_character_, routing_engine_version = "stub/0.0.0",
      polygon_simplification_tolerance = NA_real_,
      generated_at = as.POSIXct("2026-05-24 00:00:00", tz = "UTC"),
      isochrone_empty = FALSE, provider_requested = "osrm",
      provider_downgrade = FALSE, osm_snapshot_status = NA_character_,
      failure_reason = NA_character_, retry_count = 1L,
      ring_topology = "cumulative"
    ),
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(cx, cy), c(cx + 0.030, cy), c(cx + 0.030, cy + 0.005),
      c(cx, cy + 0.005), c(cx, cy)
    ))), crs = 4326)
  )
  square <- function(x0) {
    sf::st_multipolygon(list(list(rbind(
      c(x0, cy - 0.005), c(x0 + 0.020, cy - 0.005), c(x0 + 0.020, cy + 0.010),
      c(x0, cy + 0.010), c(x0, cy - 0.005)
    ))))
  }
  acs <- sf::st_sf(
    tibble::tibble(
      GEOID = c("01001000001", "01001000002"),
      NAME = c("Tract 1", "Tract 2"),
      variable = "B01003_001",
      estimate = estimate,
      moe = moe
    ),
    geometry = sf::st_sfc(square(cx - 0.005), square(cx + 0.015), crs = 4269)
  )
  list(iso = iso, acs = acs)
}
