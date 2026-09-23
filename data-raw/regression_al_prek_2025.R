# data-raw/regression_al_prek_2025.R - writes the four files in inst/extdata/
# that the regression test uses (inst/extdata/README.md describes them):
#
#   1. legacy_2025_sites.rds         20-row sf POINT, the input of the run
#   2. legacy_2025_isochrones.rds    60 sf MULTIPOLYGON (20 sites, 3 drive
#                                    times)
#   3. legacy_2025_golden_output.rds 1,080 rows: 540 from the run with area
#                                    weighting, and 540 rows of the same shape
#                                    for population weighting with no estimates
#   4. osm_snapshot_2025-04-01.txt   a note on the fixed routing values in
#                                    legacy_2025_isochrones.rds
#
# The data are made up. What the script does:
#
#   (a) Twenty sites: the center points of 20 tracts of
#       inst/extdata/sample_alabama_subset.rds, spread over its 911 tracts, so
#       that the smallest area around each site holds at least the tract it
#       sits in (see the comment at the step below).
#   (b) Sixty areas: a circle around each site with a radius of 1 km for each
#       minute, so 5 km, 10 km, and 15 km. They are rough, but they are the
#       same every time.
#   (c) A cacs_run() with `weight_method = "area"` against
#       `sample_alabama_subset.rds`, which gives the 540 rows.
#   (d) A second block of 540 rows for `weight_method = "population"` with
#       `estimate = NA` and the other columns filled, so that the file has
#       1,080 rows. Population weighting is not implemented, so those rows hold
#       no estimates.
#
# The regression test therefore compares a run with the output of this
# script, not with an earlier analysis.
#
# Run: Rscript data-raw/regression_al_prek_2025.R

suppressPackageStartupMessages({
  library(sf)
  library(tibble)
  library(dplyr)
  library(devtools)
})

# The package itself: cacs_run(), cacs_alabama_sites, cacs_acs_default_vars
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

set.seed(20260522L)  # Fixed, so that the files are the same each time

# Step 1: Build the 20-site regression input
# The sites are the center points of 20 tracts of sample_alabama_subset.rds,
# spread over its 911. A site at the center of a tract means that even the
# 5-minute area (5 km) holds the tract it sits in, so cacs_intersect_weight()
# finds at least one tract for every pair and returns no rows with
# n_tracts = 0, which cacs_propagate_moe() would then stop on.
#
# The 10 points of cacs_alabama_sites are city center points and can fall
# between the made-up tracts of that file, which is why they are not used
# here.

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

# Step 2: the 60 areas (20 sites, 3 drive times)
# Each isochrone is a buffered circle around the site at the projected radius
# (1 km per minute, rough but the same every time). The site is projected to
# EPSG:5070 (NAD83 / Conus Albers, an equal-area projection), buffered there,
# and the circle is re-projected to EPSG:4326.

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

# The other 14 columns that cacs_isochrone() returns
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

# The column order of cacs_isochrone(), with geometry last as sf expects
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

# Step 3: the 1,080 rows, from a cacs_run() plus the population block
# (a) Real area-weighted run on the 20-site fixture
acs_raw <- readRDS("inst/extdata/sample_alabama_subset.rds")

# Fail-safe missing-estimate policy: the synthetic ACS fixture intentionally
# preserves suppressed/missing estimates. The aggregation layer now propagates
# those missing values to catchment-level estimates/MOEs instead of imputing.
acs <- acs_raw
message("ACS missing-estimate policy: fail_safe_propagate_na; input NA estimates = ",
        sum(is.na(acs_raw$estimate)))

# The comparison file keeps nine variables: seven ACS codes and two rates.
# cacs_run() takes only the whole list of five rates, so all five are computed
# and the two are picked out afterwards.
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
# 7 ACS variables + 2 rates = 9

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
  rates                  = cacs_acs_default_rates,  # all five
  output                 = "long",
  verbose                = FALSE
)
message("cacs_run() area pass produced ", nrow(result_full), " rows")

# Filter to the 9 regression output variables = 540 area rows
regression_output_vars <- c(regression_source_vars, regression_rate_names)
result_area <- result_full |>
  dplyr::filter(variable %in% regression_output_vars)

# Sanity check: 20 sites x 3 drive_times x 9 vars = 540
stopifnot(nrow(result_area) == 540L)
stopifnot(all(c("site_id", "drive_time_min", "variable", "estimate", "moe",
                "weight_sum", "n_tracts", "weight_method") %in% names(result_area)))

# The columns of the long table, in order
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

# (b) The 540 rows for population weighting, which have no estimates
#     Population weighting is not implemented and gives an error, so these
#     rows copy the area rows with estimate = NA.
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

# (c) Put the two blocks together and sort them
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

# A record of what this file was built from, kept with it
attr(legacy_2025_golden_output, "provenance") <- list(
  source_scripts     = "data-raw/regression_al_prek_2025.R",
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
  v01_synthetic_note = paste(
    "Made-up inputs: cacs_run() with weight_method = \"area\" on 20 made-up",
    "sites, circles of 1 km for each minute of drive time around them, and",
    "sample_alabama_subset.rds. The 540 rows for weight_method = \"population\"",
    "are placeholders with estimate = NA; population weighting is not",
    "implemented."
  )
)

saveRDS(legacy_2025_golden_output, "inst/extdata/legacy_2025_golden_output.rds",
        version = 3, compress = "xz")
message("Wrote inst/extdata/legacy_2025_golden_output.rds (",
        nrow(legacy_2025_golden_output), " rows; area + pop-stub)")

# Step 4: the note on the routing values
osm_snapshot_lines <- c(
  "Routing values in legacy_2025_isochrones.rds",
  "",
  "The 60 drive-time areas in legacy_2025_isochrones.rds are made up. They are",
  "circles, not routes: data-raw/regression_al_prek_2025.R projects each of the",
  "20 made-up sites to EPSG:5070, buffers it by 1 km for each minute of drive",
  "time (5, 10, and 15 minutes), and projects the circle back to EPSG:4326. No",
  "routing server was contacted.",
  "",
  "The script writes the routing columns of that file as fixed values:",
  "",
  "  provider                 osrm",
  "  profile                  car",
  "  routing_engine_version   OSRM 5.27.1",
  "  osm_snapshot_date        2025-04-01",
  "  osm_snapshot_status      explicit",
  "  generated_at             2025-04-15 12:00:00 UTC",
  "",
  "They describe no routing run. The same date is the osm_snapshot field of the",
  "provenance attribute of legacy_2025_golden_output.rds."
)
writeLines(osm_snapshot_lines, "inst/extdata/osm_snapshot_2025-04-01.txt")
message("Wrote inst/extdata/osm_snapshot_2025-04-01.txt")

# Step 5: report the size of each file written
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
message("Fixtures written. inst/extdata/README.md is kept up to date separately.")
