# data-raw/build-visual-walkthrough-fixture.R - writes
# inst/extdata/visual_walkthrough_fixture.rds, the data behind the maps of the
# visual walkthrough article.
#
# The parts come from an earlier run of the package on one real site,
# AL_BHM_01 ("Birmingham_Vulcan"): a drive-time area from the OSRM routing
# service, Census tract boundaries, and ACS estimates. Keeping them in one file
# lets the article draw the four maps of a real place while it is built without
# a network, as a package on CRAN must be.
#
# What goes in the file:
#   * one 10-minute drive-time area, as the earlier run computed it
#     (res = 30L, a ring that runs from the site);
#   * the ACS rows of the tracts that meet that area, plus a small ring of
#     tracts around it, which keeps the file small;
#   * a run_result computed here with the current version of the package from
#     that area and those ACS rows, so that the numbers in the article are the
#     ones the package gives now;
#   * tract_sf and acs_sf are the same object, because the long ACS table
#     carries the tract geometry as well as the estimates, and the map
#     functions take both.
#
# Build with:  Rscript data-raw/build-visual-walkthrough-fixture.R
# The article reads it with:
#   readRDS(system.file("extdata", "visual_walkthrough_fixture.rds",
#                       package = "catchmentACS"))

suppressMessages({
  library(catchmentACS)
  library(sf)
  library(dplyr)
})

ANCHOR_SITE <- "AL_BHM_01"

# The outputs of the earlier run are in a working folder that is not part of
# the package; only the file written at the end of this script is.
V030 <- Sys.getenv("CACS_WORKFLOW_OUTPUTS", unset = "workflow-test/outputs")
rd <- function(f) readRDS(file.path(V030, f))

iso_all   <- rd("03_iso_osrm_res30.rds")  # sf: 3 sites x 1 (10-min) ring
acs_all   <- rd("06_acs.rds")             # sf: full AL ACS long (~20k rows)
sites_all <- rd("02_sites_df.rds")        # df: site_id, site_name, lat, lon

stopifnot(
  ANCHOR_SITE %in% iso_all$site_id,
  ANCHOR_SITE %in% sites_all$site_id,
  all(c("GEOID", "variable", "estimate", "moe") %in% names(acs_all))
)

# The drive-time area of the site, and a one-row sf POINT (WGS 84) for the
# marker on the maps.
iso_filtered <- iso_all |> dplyr::filter(.data$site_id == ANCHOR_SITE)
stopifnot(
  nrow(iso_filtered) >= 1L,
  "ring_topology" %in% names(iso_filtered),
  all(iso_filtered$ring_topology == "cumulative")
)

site_row <- sites_all |> dplyr::filter(.data$site_id == ANCHOR_SITE)
stopifnot(nrow(site_row) == 1L)
sites_sf <- sf::st_as_sf(site_row, coords = c("lon", "lat"),
                         crs = 4326, remove = FALSE)

# Keep the ACS rows of the tracts that meet the area, and of those within
# about 2 km of it, so that the maps show some ground around it. The tracts are
# picked from one geometry for each GEOID, because the long table repeats the
# geometry once for each variable.
tracts_unique <- acs_all |> dplyr::distinct(.data$GEOID, .keep_all = TRUE)
iso_geom <- sf::st_union(sf::st_geometry(iso_filtered))
iso_buff <- iso_geom |>
  sf::st_transform(5070) |>
  sf::st_buffer(2000) |>
  sf::st_transform(sf::st_crs(tracts_unique))

hit <- lengths(sf::st_intersects(tracts_unique, iso_buff)) > 0L
keep_geoids <- tracts_unique$GEOID[hit]
acs_bhm <- acs_all |> dplyr::filter(.data$GEOID %in% keep_geoids)

stopifnot(length(keep_geoids) >= 10L, nrow(acs_bhm) > 0L)

# Compute run_result with the current version of the package. Giving both the
# drive-time area and the ACS rows means no routing service and no Census
# download are used, so the numbers are the same each time.
run_result <- suppressWarnings(suppressMessages(
  cacs_run(
    sites                  = site_row,
    state                  = "AL",
    year                   = 2023,
    drive_times            = 10L,
    variables              = unique(acs_bhm$variable),
    provider               = "osrm",
    weight_method          = "area",
    precomputed_isochrones = iso_filtered,
    acs                    = acs_bhm,
    rates                  = cacs_acs_default_rates,
    output                 = "long",
    verbose                = FALSE
  )
))

stopifnot(
  inherits(run_result, "tbl_df"),
  all(run_result$site_id == ANCHOR_SITE),
  all(c("variable", "estimate", "moe", "n_tracts_num", "n_tracts_den",
        "moe_fallback") %in% names(run_result)),
  # the five rates, which the fourth map shows
  all(c("poverty_rate", "snap_rate", "ssi_rate", "unemp_rate",
        "labor_force_participation") %in% run_result$variable)
)

# The six names the article reads.
fixture <- list(
  iso_sf      = iso_filtered,
  tract_sf    = acs_bhm,
  acs_sf      = acs_bhm,
  run_result  = run_result,
  sites_df    = sites_sf,
  anchor_site = ANCHOR_SITE
)

# Check that the four maps and the function that builds all four do come out
# of this file, when leaflet is installed.
if (requireNamespace("leaflet", quietly = TRUE)) {
  p1 <- cacs_plot_site_isochrone(site_id = ANCHOR_SITE,
                                 iso_sf = fixture$iso_sf,
                                 sites_df = fixture$sites_df)
  p2 <- cacs_plot_site_intersection(site_id = ANCHOR_SITE,
                                    iso_sf = fixture$iso_sf,
                                    tract_sf = fixture$tract_sf,
                                    sites_df = fixture$sites_df)
  p3 <- cacs_plot_site_weighted(site_id = ANCHOR_SITE,
                                iso_sf = fixture$iso_sf,
                                tract_sf = fixture$tract_sf,
                                acs_sf = fixture$acs_sf,
                                variable = "B17001_002",
                                sites_df = fixture$sites_df)
  p4 <- cacs_plot_site_rates(site_id = ANCHOR_SITE,
                             iso_sf = fixture$iso_sf,
                             run_result = fixture$run_result,
                             sites_df = fixture$sites_df)
  pipe <- cacs_plot_site_pipeline(site_id = ANCHOR_SITE,
                                  iso_sf = fixture$iso_sf,
                                  tract_sf = fixture$tract_sf,
                                  acs_sf = fixture$acs_sf,
                                  run_result = fixture$run_result,
                                  sites_df = fixture$sites_df,
                                  variable = "B17001_002")
  stopifnot(
    inherits(p1, "leaflet"), inherits(p2, "leaflet"),
    inherits(p3, "leaflet"), inherits(p4, "leaflet"),
    inherits(pipe, "cacs_site_plot_pipeline")
  )
  cat("Smoke test: all 4 stage plots + pipeline render OK.\n")
}

# Write the file and report its size.
out_path <- "inst/extdata/visual_walkthrough_fixture.rds"
saveRDS(fixture, out_path, version = 2, compress = "xz")

sz <- file.info(out_path)$size
cat(sprintf(
  "Saved fixture: %s (%.2f MB) | anchor %s | %d tracts | %d ACS rows | %d run rows\n",
  out_path, sz / 1024 / 1024, ANCHOR_SITE,
  length(keep_geoids), nrow(acs_bhm), nrow(run_result)
))
