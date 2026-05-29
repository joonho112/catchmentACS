# data-raw/build-visual-walkthrough-fixture.R
# ---------------------------------------------------------------------------
# v0.5.0 -- single source of truth for
#   inst/extdata/visual_walkthrough_fixture.rds
#
# REWRITTEN to use the REAL single-point workflow from the verified v0.3.0
# verified v0.3.0 workflow test, so the Visual
# Walkthrough vignette reproduces the actual Stage 1-4 leaflet maps for a
# recognizable Alabama site -- AL_BHM_01, "Birmingham_Vulcan" -- using real
# OSRM isochrone geometry, real Census tracts, and real ACS estimates, while
# still rendering entirely offline (CRAN policy: no network at vignette build).
#
# Design choices:
#   * Anchor = AL_BHM_01 (Birmingham, The Vulcan), a concrete landmark a
#     reader can place on a map -- replacing the abstract synthetic site.
#   * Single 10-minute drive-time catchment, exactly as the v0.3.0 workflow
#     test plotted it (res = 30L cumulative ring).
#   * ACS is subset to the tracts that intersect the catchment (+ a small
#     context buffer), keeping the fixture small.
#   * run_result is REGENERATED with the CURRENT package via the 3-call
#     (precomputed-isochrone) path, so every estimand -- including the
#     area_weighted_scalar_proxy / median_proxy families -- reflects the
#     v0.5.0 mean-weight proxy fix. (The 5 sanctioned rates were never
#     affected by that bug, but regenerating keeps the whole table correct.)
#   * tract_sf and acs_sf are the SAME object (the long ACS sf carries both
#     tract geometry and the variable/estimate/moe panel), mirroring the
#     plot-function signatures and the v0.3.0 test calls verbatim.
#
# Build via:   Rscript data-raw/build-visual-walkthrough-fixture.R
# Vignette consumes via:
#   readRDS(system.file("extdata", "visual_walkthrough_fixture.rds",
#                       package = "catchmentACS"))
# ---------------------------------------------------------------------------

suppressMessages({
  library(catchmentACS)
  library(sf)
  library(dplyr)
})

ANCHOR_SITE <- "AL_BHM_01"

# ---------------------------------------------------------------------------
# 1. Load the REAL v0.3.0 workflow-test outputs (real OSRM iso + Census ACS).
#    These live in a local working tree (not shipped); only the
#    compact fixture we write below ships with the package.
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# 2. Anchor isochrone + a sites_df (sf POINT, WGS84) for the site marker.
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# 3. Subset ACS to the tracts intersecting the catchment (+ ~2 km context
#    buffer). One distinct geometry per GEOID first (avoid 14x duplication).
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# 4. Regenerate run_result with the CURRENT package (3-call path: precomputed
#    isochrone + the Birmingham ACS subset). Deterministic, no network.
# ---------------------------------------------------------------------------
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
  # the five sanctioned rates are present (Stage 4)
  all(c("poverty_rate", "snap_rate", "ssi_rate", "unemp_rate",
        "labor_force_participation") %in% run_result$variable)
)

# ---------------------------------------------------------------------------
# 5. Assemble the fixture (same 6 slot names the vignette already consumes).
# ---------------------------------------------------------------------------
fixture <- list(
  iso_sf      = iso_filtered,
  tract_sf    = acs_bhm,
  acs_sf      = acs_bhm,
  run_result  = run_result,
  sites_df    = sites_sf,
  anchor_site = ANCHOR_SITE
)

# ---------------------------------------------------------------------------
# 6. Smoke-test the four Stage plots + the pipeline wrapper actually render
#    from this fixture (only if leaflet is available).
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# 7. Persist + report.
# ---------------------------------------------------------------------------
out_path <- "inst/extdata/visual_walkthrough_fixture.rds"
saveRDS(fixture, out_path, version = 2)

sz <- file.info(out_path)$size
cat(sprintf(
  "Saved fixture: %s (%.2f MB) | anchor %s | %d tracts | %d ACS rows | %d run rows\n",
  out_path, sz / 1024 / 1024, ANCHOR_SITE,
  length(keep_geoids), nrow(acs_bhm), nrow(run_result)
))
