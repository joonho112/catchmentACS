# Build v0.3 replay fixtures from frozen v0.2 evidence.
#
# This script is intentionally offline. It packages the 031 replication
# walkthrough outputs and the 032 fresh-user workflow outputs into compact
# fixtures consumed by v0.3 regression tests.
#
# Run from package root:
#   Rscript data-raw/build-replay-fixtures.R

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(tibble)
})

devtools::load_all(".", quiet = TRUE)

out_dir <- "inst/testdata"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stamp <- function(source_name, source_paths) {
  list(
    build_script = "data-raw/build-replay-fixtures.R",
    built_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    fixture_name = source_name,
    source = source_name,
    source_paths = source_paths,
    input_hashes = vapply(source_paths, function(path) {
      if (file.exists(path)) {
        digest::digest(file = path, algo = "sha256")
      } else {
        NA_character_
      }
    }, character(1)),
    network_required = FALSE,
    package_version = read.dcf("DESCRIPTION")[1, "Version"]
  )
}

attach_provenance <- function(x, source_name, source_paths) {
  attr(x, "cacs_provenance") <- stamp(source_name, source_paths)
  x
}

read_source <- function(path) {
  if (!file.exists(path)) {
    stop("Missing source artifact: ", path, call. = FALSE)
  }
  readRDS(path)
}

site_points_from_iso <- function(iso_sf) {
  iso_projected <- sf::st_transform(iso_sf, 5070)
  points <- sf::st_point_on_surface(sf::st_geometry(iso_projected))
  points <- sf::st_transform(sf::st_sfc(points, crs = 5070), sf::st_crs(iso_sf))
  sf::st_sf(
    tibble::tibble(
      site_id = iso_sf$site_id,
      site_name = paste0("031 anchor ", iso_sf$site_id),
      source = "isochrone_point_on_surface_proxy"
    ),
    geometry = points,
    crs = sf::st_crs(iso_sf)
  )
}

source_031 <- c(
  iso = "replay-sources/031/iso_cacs.rds",
  acs = "replay-sources/031/acs_cacs.rds",
  weighted = "replay-sources/031/weighted_seam.rds",
  propagated = "replay-sources/031/propagated_seam.rds",
  final = "replay-sources/031/final_seam.rds",
  result = "replay-sources/031/result_run.rds",
  site_coords = "replay-sources/031/cmp0-site-coords.rds",
  cmp1a_area = "replay-sources/031/cmp1a-isochrone-area.rds",
  cmp1b_live_jaccard = "replay-sources/031/cmp1b-tract-jaccard.rds",
  cmp3_seam_jaccard = "replay-sources/031/cmp3-seam-jaccard.rds"
)

iso_031 <- read_source(source_031[["iso"]])
fixture_031 <- list(
  sites_sf = site_points_from_iso(iso_031),
  site_coord_audit = read_source(source_031[["site_coords"]]),
  iso_sf_v2 = iso_031,
  acs_sf = read_source(source_031[["acs"]]),
  weighted_seam_v2 = read_source(source_031[["weighted"]]),
  propagated_seam_v2 = read_source(source_031[["propagated"]]),
  final_seam_v2 = read_source(source_031[["final"]]),
  result_run_v2 = read_source(source_031[["result"]]),
  cmp1a_isochrone_area = read_source(source_031[["cmp1a_area"]]),
  cmp1b_live_tract_jaccard = read_source(source_031[["cmp1b_live_jaccard"]]),
  cmp3_seam_jaccard = read_source(source_031[["cmp3_seam_jaccard"]])
)
fixture_031$derived_rates_v2 <- fixture_031$final_seam_v2 |>
  dplyr::filter(.data$estimand_family == "derived_rate")
fixture_031 <- attach_provenance(fixture_031, "v020 031 replication walkthrough", source_031)
saveRDS(fixture_031, file.path(out_dir, "fixture_031_3site_anchors.rds"), version = 3)

source_032 <- c(
  sites = "replay-sources/032/02_sites_sf.rds",
  iso = "replay-sources/032/03_iso.rds",
  acs = "replay-sources/032/04_acs.rds",
  weighted = "replay-sources/032/05_weighted.rds",
  propagated = "replay-sources/032/06_propagated.rds",
  final = "replay-sources/032/07_final.rds",
  rates_labeled = "replay-sources/032/07_rates_labeled.rds",
  rates_wide = "replay-sources/032/07_rates_wide.rds",
  result = "replay-sources/032/08_run_result.rds"
)

fixture_032 <- list(
  sites_sf = read_source(source_032[["sites"]]),
  iso_sf_v2 = read_source(source_032[["iso"]]),
  acs_sf = read_source(source_032[["acs"]]),
  weighted_seam_v2 = read_source(source_032[["weighted"]]),
  propagated_seam_v2 = read_source(source_032[["propagated"]]),
  final_seam_v2 = read_source(source_032[["final"]]),
  rates_labeled_v2 = read_source(source_032[["rates_labeled"]]),
  rates_wide_v2 = read_source(source_032[["rates_wide"]]),
  result_run_v2 = read_source(source_032[["result"]])
)
fixture_032$derived_rates_v2 <- fixture_032$final_seam_v2 |>
  dplyr::filter(.data$estimand_family == "derived_rate")
fixture_032 <- attach_provenance(fixture_032, "v020 032 fresh-user workflow", source_032)
saveRDS(fixture_032, file.path(out_dir, "fixture_032_3site_fresh.rds"), version = 3)

make_poison_acs <- function(variables) {
  geoid <- c("01001000100", "01002000200", "01003000300")
  geom_list <- lapply(seq_along(geoid), function(i) {
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
  rows <- expand.grid(
    GEOID = geoid,
    variable = variables,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  rows$NAME <- paste0("Tract ", rows$GEOID, ", AL")
  rows$estimate <- as.numeric(seq_len(nrow(rows)) * 1000L)
  rows$moe <- as.numeric(seq_len(nrow(rows)) * 10L)
  geom <- sf::st_sfc(geom_list, crs = 4269)
  sf::st_sf(rows, geometry = geom[match(rows$GEOID, geoid)], crs = 4269)
}

poison_vars <- unique(fixture_032$acs_sf$variable)
poison_acs <- make_poison_acs(poison_vars)

poison_weighted <- suppressWarnings(
  {
    old_intersect_cache <- getOption("catchmentACS.cache_intersect", NULL)
    on.exit(options(catchmentACS.cache_intersect = old_intersect_cache), add = TRUE)
    options(catchmentACS.cache_intersect = FALSE)
    cacs_intersect_weight(
      iso_sf = fixture_032$iso_sf_v2,
      acs_sf = poison_acs,
      weight_method = "area",
      verbose = FALSE
    )
  }
)

poison_propagated <- suppressWarnings(
  cacs_propagate_moe(poison_weighted)
)

poison_final <- suppressWarnings(
  cacs_derive_rates(poison_propagated)
)

fixture_cache_poison <- list(
  acs_sf = poison_acs,
  weighted_tbl = poison_weighted,
  propagated_tbl = poison_propagated,
  final_tbl = poison_final,
  bug_context = "Exact 39-row fake ACS pattern from v020 032 bug #10; stale intersect has 15 rows.",
  expected_real_acs_rows = nrow(fixture_032$acs_sf),
  expected_poison_acs_rows = nrow(poison_acs),
  expected_stale_weighted_rows = nrow(poison_weighted),
  estimate_digest = digest::digest(poison_acs$estimate, algo = "sha256")
)
fixture_cache_poison <- attach_provenance(
  fixture_cache_poison,
  "v020 032 cache poison regression seed",
  source_032[c("acs", "weighted")]
)
saveRDS(fixture_cache_poison, file.path(out_dir, "fixture_cache_poison.rds"), version = 3)

make_annulus <- function(sites_sf, isomin, isomax) {
  sites_5070 <- sf::st_transform(sites_sf, 5070)
  geom <- lapply(seq_len(nrow(sites_5070)), function(i) {
    inner <- sf::st_buffer(sf::st_geometry(sites_5070[i, ]), dist = isomin[i] * 1609.344)
    outer <- sf::st_buffer(sf::st_geometry(sites_5070[i, ]), dist = isomax[i] * 1609.344)
    sf::st_geometry(sf::st_difference(outer, inner))[[1]]
  })
  ann <- sf::st_sf(
    tibble::tibble(
      site_id = sites_sf$site_id,
      drive_time_min = as.integer(isomax),
      isomin = as.integer(isomin),
      isomax = as.integer(isomax),
      ring_topology = "annulus",
      provider = "fixture",
      profile = "car",
      generated_at = as.POSIXct("2026-05-24 00:00:00", tz = "UTC")
    ),
    geometry = sf::st_sfc(geom, crs = 5070)
  )
  sf::st_transform(ann, 4326)
}

fixture_annulus <- make_annulus(
  fixture_032$sites_sf,
  isomin = c(5L, 5L, 10L),
  isomax = c(10L, 15L, 15L)
)
fixture_annulus <- attach_provenance(
  fixture_annulus,
  "synthetic annulus fixture for v0.3 UF-1",
  source_032[["sites"]]
)
saveRDS(fixture_annulus, file.path(out_dir, "fixture_annulus_input.rds"), version = 3)

message("Wrote replay fixtures to ", normalizePath(out_dir))
