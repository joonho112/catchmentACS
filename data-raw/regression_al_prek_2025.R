# ============================================================================
# data-raw/regression_al_prek_2025.R
#
# Generates 4 regression fixtures in `inst/extdata/` for the §26 regression
# test (Step 8.1 of Phase 8):
#
#   1. legacy_2025_sites.rds         — 20-row sf POINT (regression input)
#   2. legacy_2025_isochrones.rds    — 60 sf MULTIPOLYGON (20 sites × 3 drives)
#   3. legacy_2025_golden_output.rds — 1,080-row golden (area 540 + pop 540 stub)
#   4. osm_snapshot_2025-04-01.txt   — routing provenance text
#
# Plus:
#   - inst/extdata/README.md         — 6-file inventory + schema + provenance
#
# SYNTHETIC golden via canonical pipeline run
# -------------------------------------------------------------
# A set of private 2025 baseline `.rds` artifacts exists in a restricted local
# directory (outside the package ship boundary). Their schema does not match
# the canonical 20-col long contract used by `cacs_run()`:
#
#   - Baseline used annular rings (0-5, 5-10, 10-15 minutes), NOT cumulative
#     drive_time isochrones (5, 10, 15).
#   - Baseline output is WIDE (`total_pop_E`, `total_pop_M`, ...), NOT long
#     melted with a `variable` column.
#   - Baseline lacks `weight_method`, `weight_sum`, `n_tracts`, dispatch
#     metadata, and the canonical 20-col schema (§12.3.1).
#   - Baseline used 902 sites; only 20 are needed for the regression fixture.
#
# Re-deriving the 1,080-row canonical golden from baseline tibbles would
# require re-running the 2025 baseline scripts with new aggregation logic
# (cumulative isochrones instead of rings) — that is *itself* a §26.7 golden
# regeneration PR scope, not Step 8.1's task.
#
# Therefore the synthetic-fixture approach (consistent with `data-raw/al_acs_fetch.R`
# and `data-raw/cacs_alabama_sites.R` synthetic precedent) is to:
#
#   (a) Use `cacs_alabama_sites` (10 sample sites) as the seed, doubled to
#       a 20-site regression input by generating 10 additional synthetic
#       sites at deterministic offsets (preserves Alabama geographic
#       spread, distinct from the 10-row user-facing sample fixture).
#   (b) Generate 60 synthetic isochrones (20 × 3 drive_times) as buffered
#       circles around each site at radii proportional to drive_time
#       (5min = 5km, 10min = 10km, 15min = 15km — rough but deterministic).
#   (c) Run `cacs_run()` end-to-end with `weight_method = "area"` against
#       `sample_alabama_subset.rds`, producing the 540-row area golden.
#   (d) Append a 540-row "population" stub block with `estimate = NA`,
#       `weight_method = "population"`, all other canonical columns
#       populated, fulfilling Decision #6 (full 1,080-row preservation
#       without bg_pop_sf fixture. A future population-weighting regeneration
#       may overwrite those rows with real values.
#
# This makes the regression test a *frozen output of the canonical pipeline*
# rather than a 1:1 reproduction of the 2025 ad-hoc scripts. A future
# production-grade regeneration with live facility coordinates and a population
# weighting fixture will supersede this synthetic baseline.
#
# Run: Rscript data-raw/regression_al_prek_2025.R
# ============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(tibble)
  library(dplyr)
  library(devtools)
})

# Load the package — for cacs_run(), cacs_alabama_sites, cacs_acs_default_vars
devtools::load_all(quiet = TRUE)

.sha256_file <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  digest::digest(path, algo = "sha256", file = TRUE)
}

.git_metadata <- function() {
  inside <- suppressWarnings(
    system2("git", c("rev-parse", "--is-inside-work-tree"),
            stdout = TRUE, stderr = TRUE)
  )
  if (!identical(inside[[1]], "true")) {
    return(list(status = "not_git_worktree",
                commit = NA_character_,
                dirty = NA))
  }
  commit <- suppressWarnings(
    system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = TRUE)
  )
  status <- suppressWarnings(
    system2("git", c("status", "--porcelain"), stdout = TRUE, stderr = TRUE)
  )
  commit_value <- if (length(commit) > 0L) commit[[1]] else NA_character_
  list(status = "git_worktree",
       commit = commit_value,
       dirty = length(status) > 0L)
}

# Clear any prior on-disk caches so this script's output is purely a function
# of the inputs in this file (not stale entries from earlier debug runs).
suppressMessages(
  catchmentACS::cacs_clear_cache(namespace = "all", confirm = FALSE)
)

set.seed(20260522L)  # Blueprint v1.0.1 sign-off date — reproducibility seed.

# ---------------------------------------------------------------------------
# Step 1: Baseline existence audit (Q26-3 evidence capture)
# ---------------------------------------------------------------------------
# Even though we are NOT reading the 5 baseline .rds files into the canonical
# golden, we still audit their existence so the README provenance entry can
# document the Q26-3 verified status with bit-stable evidence.

# The private baseline directory (if available) is supplied via an env var; it
# is never hardcoded and is not required, since the golden is fully synthetic.
source_dir <- Sys.getenv("CACS_BASELINE_DIR", unset = NA_character_)
baseline_files <- c(
  "baseline_acs_tracts.rds",
  "baseline_isochrones.rds",
  "baseline_weighted.rds",
  "baseline_weighted_tibble.rds",
  "baseline_needs_estimates.rds"
)
baseline_status <- if (is.na(source_dir)) {
  stats::setNames(rep(NA, length(baseline_files)), baseline_files)
} else {
  vapply(baseline_files, function(f) file.exists(file.path(source_dir, f)),
         logical(1))
}

# ---------------------------------------------------------------------------
# Step 2: Build the 20-site regression input
# ---------------------------------------------------------------------------
# Strategy: pick 20 centroids of distinct tracts in the synthetic
# sample_alabama_subset.rds fixture, spread across the ~911 tracts. This
# guarantees that every site's 5-min isochrone (5km radius) overlaps at
# least the tract it sits in, so cacs_intersect_weight does not emit
# "empty-site sentinel" rows (n_tracts = 0 / estimand_family = NA) that
# would then trip .validate_propagate_output() in cacs_propagate_moe().
#
# The 10 cacs_alabama_sites are user-facing city-centroid samples that
# may fall *between* synthetic tract polygons; this fixture uses a
# different, denser, pipeline-bound site set tuned to the synthetic ACS.
# A future production regeneration would use real facility
# coordinates and the corresponding full ACS coverage.

acs_for_seed <- readRDS("inst/extdata/sample_alabama_subset.rds")
acs_tracts <- acs_for_seed |> dplyr::distinct(GEOID, .keep_all = TRUE)
acs_centroids <- suppressWarnings(sf::st_centroid(acs_tracts))

# Pick 20 indices spread across the 911 tract sequence
n_tr <- nrow(acs_centroids)
sel_idx <- round(seq.int(from = 50L, to = n_tr - 50L, length.out = 20L))
sel_tracts <- acs_centroids[sel_idx, ]

# AL region labels (rough, by index quintile)
region_lookup <- c(
  rep("South Coast",      4L),
  rep("Southeast",        4L),
  rep("Central",          4L),
  rep("North Central",    4L),
  rep("North",            4L)
)

legacy_2025_sites <- tibble::tibble(
  site_id      = sprintf("AL_SITE_%02d", seq_len(20L)),
  site_name    = sprintf("Regression Site %d", seq_len(20L)),
  county_fips  = substr(sel_tracts$GEOID, 1L, 5L),
  region_label = region_lookup
) |>
  dplyr::bind_cols(geometry = sf::st_geometry(sel_tracts)) |>
  sf::st_as_sf(sf_column_name = "geometry", crs = 4326)

stopifnot(nrow(legacy_2025_sites) == 20L)
stopifnot(inherits(legacy_2025_sites, "sf"))
stopifnot(sf::st_crs(legacy_2025_sites)$epsg == 4326L)

saveRDS(legacy_2025_sites, "inst/extdata/legacy_2025_sites.rds",
        version = 3, compress = "xz")
message("Wrote inst/extdata/legacy_2025_sites.rds (", nrow(legacy_2025_sites), " sites)")

# ---------------------------------------------------------------------------
# Step 3: Build 60 synthetic isochrones (20 sites × 3 drive_times)
# ---------------------------------------------------------------------------
# Each isochrone is a buffered circle around the site at the projected radius
# (1km per minute, rough but deterministic). EPSG:5070 (Alabama area-preserving
# Albers) → buffer → re-project to EPSG:4326.

drive_times <- c(5L, 10L, 15L)

sites_proj <- sf::st_transform(legacy_2025_sites, 5070)
iso_rows <- vector("list", 20L * 3L)
idx <- 1L
for (s in seq_len(20L)) {
  pt <- sites_proj[s, ]
  for (dt in drive_times) {
    radius_m <- dt * 1000  # 1 km per minute
    buf <- sf::st_buffer(pt, dist = radius_m)
    geom <- sf::st_transform(buf, 4326) |> sf::st_geometry()
    iso_rows[[idx]] <- tibble::tibble(
      site_id        = legacy_2025_sites$site_id[s],
      drive_time_min = dt,
      geometry       = geom
    )
    idx <- idx + 1L
  }
}

legacy_2025_isochrones <- do.call(rbind, lapply(iso_rows, function(x) {
  sf::st_sf(x, crs = 4326)
}))

# Attach §19.5 canonical 16-col schema metadata
legacy_2025_isochrones$provider                         <- "osrm"
legacy_2025_isochrones$profile                          <- "car"
legacy_2025_isochrones$osm_snapshot_date                <- "2025-04-01"
legacy_2025_isochrones$routing_engine_version           <- "OSRM 5.27.1"
legacy_2025_isochrones$polygon_simplification_tolerance <- 0.0
legacy_2025_isochrones$generated_at                     <- as.POSIXct(
  "2025-04-15 12:00:00", tz = "UTC"
)
legacy_2025_isochrones$isochrone_empty                  <- FALSE
legacy_2025_isochrones$provider_requested               <- "osrm"
legacy_2025_isochrones$provider_downgrade               <- FALSE
legacy_2025_isochrones$osm_snapshot_status              <- "explicit"
legacy_2025_isochrones$failure_reason                   <- NA_character_
legacy_2025_isochrones$retry_count                      <- 0L
legacy_2025_isochrones$ring_topology                    <- "cumulative"

# Reorder to canonical column sequence (geometry last by sf convention)
canonical_iso_cols <- c(
  "site_id", "drive_time_min", "provider", "profile",
  "osm_snapshot_date", "routing_engine_version",
  "polygon_simplification_tolerance", "generated_at",
  "isochrone_empty", "provider_requested", "provider_downgrade",
  "osm_snapshot_status", "failure_reason", "retry_count",
  "ring_topology", "geometry"
)
legacy_2025_isochrones <- legacy_2025_isochrones[, canonical_iso_cols]

stopifnot(nrow(legacy_2025_isochrones) == 60L)
stopifnot(inherits(legacy_2025_isochrones, "sf"))

saveRDS(legacy_2025_isochrones, "inst/extdata/legacy_2025_isochrones.rds",
        version = 3, compress = "xz")
message("Wrote inst/extdata/legacy_2025_isochrones.rds (",
        nrow(legacy_2025_isochrones), " iso polygons)")

# ---------------------------------------------------------------------------
# Step 4: Build the 1,080-row golden via cacs_run() end-to-end + pop stub
# ---------------------------------------------------------------------------
# (a) Real area-weighted run on the 20-site fixture
acs_raw <- readRDS("inst/extdata/sample_alabama_subset.rds")

# Fail-safe missing-estimate policy: the synthetic ACS fixture intentionally
# preserves suppressed/missing estimates. The aggregation layer now propagates
# those missing values to catchment-level estimates/MOEs instead of imputing.
acs <- acs_raw
message("ACS missing-estimate policy: fail_safe_propagate_na; input NA estimates = ",
        sum(is.na(acs_raw$estimate)))

# Restrict to a 9-variable subset for the golden (matches §43.3 spec
# `cacs_regression_output_vars` shape — 9 distinct outputs).
# Use the 7 source vars from the spec + 2 derived rates (poverty/snap).
# Note: cacs_run() validates `rates` against the full curated catalogue, so we
# must pass `cacs_acs_default_rates` (all 5) and filter to the 2 chosen rate
# names AFTER the run.
regression_source_vars <- c(
  "B01003_001",   # total_pop
  "B17001_001",   # pov_denom
  "B17001_002",   # pov_below
  "B19013_001",   # med_hh_inc
  "B19301_001",   # per_cap_inc
  "B22003_001",   # snap_denom
  "B22003_002"    # snap_recv
)
regression_rate_names <- c("poverty_rate", "snap_rate")
# 7 source + 2 rates = 9 output variables (= §43.3 "9 distinct output vars")

result_full <- cacs_run(
  sites                  = legacy_2025_sites,
  state                  = "AL",
  year                   = 2023,
  drive_times            = drive_times,
  variables              = unname(cacs_acs_default_vars),
  provider               = "osrm",
  weight_method          = "area",
  precomputed_isochrones = legacy_2025_isochrones,
  acs                    = acs,
  rates                  = cacs_acs_default_rates,  # full curated catalogue
  output                 = "long",
  verbose                = FALSE
)
message("cacs_run() area pass produced ", nrow(result_full), " rows")

# Filter to the 9 regression output variables = 540 area rows
regression_output_vars <- c(regression_source_vars, regression_rate_names)
result_area <- result_full |>
  dplyr::filter(variable %in% regression_output_vars)

# Sanity check: 20 sites × 3 drive_times × 9 vars = 540
stopifnot(nrow(result_area) == 540L)
stopifnot(all(c("site_id", "drive_time_min", "variable", "estimate", "moe",
                "weight_sum", "n_tracts", "weight_method") %in% names(result_area)))

# Canonical 20-col selection (§12.3.1 long schema)
canonical_long_cols <- c(
  "site_id", "drive_time_min", "variable", "estimate", "moe",
  "weight_sum", "n_tracts",
  "provider", "profile", "osm_snapshot_date",
  "acs_year", "weight_method",
  "estimand_family", "weight_basis",
  "moe_formula_requested", "moe_formula_effective",
  "moe_fallback", "moe_fallback_reason",
  "failure_origin", "weight_uncertainty_propagated"
)
# Add acs_year column if cacs_run did not emit it
if (!"acs_year" %in% names(result_area)) {
  result_area$acs_year <- 2023L
}
missing_cols <- setdiff(canonical_long_cols, names(result_area))
if (length(missing_cols) > 0L) {
  stop("Canonical column(s) missing from cacs_run output: ",
       paste(missing_cols, collapse = ", "))
}
result_area_canonical <- result_area[, canonical_long_cols]

# (b) Build the 540-row "population" stub block — Decision #6 preservation
#     The current release keeps population weighting reserved/fail-loud, so the
#     population path is a historical placeholder (estimate = NA). A future
#     population-weighting release may overwrite this block through a dedicated
#     regeneration step.
result_pop_stub <- result_area_canonical
result_pop_stub$weight_method        <- "population"
result_pop_stub$weight_basis         <- "block_group_pop"
result_pop_stub$estimate             <- NA_real_
result_pop_stub$moe                  <- NA_real_
result_pop_stub$weight_sum           <- NA_real_
result_pop_stub$n_tracts             <- NA_integer_
result_pop_stub$moe_fallback         <- FALSE
result_pop_stub$moe_fallback_reason  <- "n/a"
result_pop_stub$failure_origin       <- "none"
result_pop_stub$weight_uncertainty_propagated <- FALSE

# (c) Bind area + population, arrange to canonical sort order
legacy_2025_golden_output <- dplyr::bind_rows(
  result_area_canonical,
  result_pop_stub
) |>
  dplyr::arrange(site_id, drive_time_min, weight_method, variable)

# Sanity checks: 1,080 rows, 540/540 area/population split, 20 cols
stopifnot(nrow(legacy_2025_golden_output) == 1080L)
stopifnot(ncol(legacy_2025_golden_output) == 20L)
split_tbl <- table(legacy_2025_golden_output$weight_method)
stopifnot(split_tbl["area"] == 540L)
stopifnot(split_tbl["population"] == 540L)
# 9 distinct variables
stopifnot(length(unique(legacy_2025_golden_output$variable)) == 9L)

# Provenance attribute — Q26-3 audit status pinned to the fixture
attr(legacy_2025_golden_output, "provenance") <- list(
  source_scripts    = "private 2025 baseline analysis (not shipped)",
  baseline_artifacts = baseline_files,
  baseline_status    = baseline_status,
  acs_year           = 2023L,
  osm_snapshot       = "2025-04-01",
  geos_version       = as.character(sf::sf_extSoftVersion()["GEOS"]),
  proj_version       = as.character(sf::sf_extSoftVersion()["PROJ"]),
	  generated_on       = Sys.Date(),
	  generation_seed    = 20260522L,
	  acs_missing_estimate_policy = "fail_safe_propagate_na",
	  acs_input_missing_estimate_count = as.integer(sum(is.na(acs_raw$estimate))),
	  population_stub_status = "v0.1_population_path_inactive",
	  population_stub_rows = 540L,
	  script_hash_sha256 = .sha256_file("data-raw/regression_al_prek_2025.R"),
	  input_hashes_sha256 = list(
	    sample_alabama_subset = .sha256_file("inst/extdata/sample_alabama_subset.rds"),
	    legacy_2025_sites = .sha256_file("inst/extdata/legacy_2025_sites.rds"),
	    legacy_2025_isochrones = .sha256_file("inst/extdata/legacy_2025_isochrones.rds")
	  ),
	  package_git = .git_metadata(),
	  baseline_audit     = "n/a (synthetic golden; no private baseline read)",
	  v01_synthetic_note = paste(
    "synthetic golden: produced by cacs_run() end-to-end on",
    "20 synthetic sites + buffered-circle isochrones + sample_alabama_subset",
    "ACS fixture. Population path (540 rows) is a historical STUB (estimate",
    "= NA) per Decision #6 until a future population-weighting regeneration."
  )
)

saveRDS(legacy_2025_golden_output, "inst/extdata/legacy_2025_golden_output.rds",
        version = 3, compress = "xz")
message("Wrote inst/extdata/legacy_2025_golden_output.rds (",
        nrow(legacy_2025_golden_output), " rows; area + pop-stub)")

# ---------------------------------------------------------------------------
# Step 5: osm_snapshot provenance text
# ---------------------------------------------------------------------------
osm_snapshot_lines <- c(
  "OSM Snapshot Provenance - catchmentACS legacy_2025 regression fixture",
  "",
  "snapshot_date:     2025-04-01",
  "provider:          OSRM 5.27.1 (public demo server)",
  "server_url:        https://router.project-osrm.org/",
  "retrieval_dates:   2025-05-24 (per baseline script timestamp)",
  "profile:           car",
  "region_bbox:       AL (lon -88.5 to -84.9, lat 30.2 to 35.0)",
  "notes:             Public demo server; production should use",
  "                   self-hosted OSRM with pinned PBF.",
  "",
  "synthetic fixture note:",
  "  The legacy_2025_isochrones.rds fixture is synthetic (buffered circles",
  "  at 1km/minute radius around each site, EPSG:5070 buffer reprojected to",
  "  EPSG:4326). This metadata describes the OSM snapshot that the v1.0",
  "  production regeneration would use; it is preserved here so the",
  "  regression-test provenance attribute carries a stable historical value."
)
writeLines(osm_snapshot_lines, "inst/extdata/osm_snapshot_2025-04-01.txt")
message("Wrote inst/extdata/osm_snapshot_2025-04-01.txt")

# ---------------------------------------------------------------------------
# Step 6: Size + budget audit (post-write)
# ---------------------------------------------------------------------------
files_written <- c(
  "inst/extdata/legacy_2025_sites.rds",
  "inst/extdata/legacy_2025_isochrones.rds",
  "inst/extdata/legacy_2025_golden_output.rds",
  "inst/extdata/osm_snapshot_2025-04-01.txt"
)
sizes <- vapply(files_written, function(f) file.size(f), numeric(1))
message("")
message("=== File size audit ===")
for (i in seq_along(files_written)) {
  message(sprintf("  %-50s %8.1f KB",
                  basename(files_written[i]), sizes[i] / 1024))
}
message(sprintf("  %-50s %8.1f KB", "TOTAL", sum(sizes) / 1024))
message("")
message("Step 8.1 fixtures generated. README written separately by Write tool.")
