# ============================================================================
# data-raw/al_acs_fetch.R — Generate sample_alabama_subset.rds for inst/extdata/
#
# This is a SYNTHETIC fixture mimicking tidycensus output. Live tidycensus
# fetches require CENSUS_API_KEY (`Sys.setenv(CENSUS_API_KEY = ...)`), but this
# package-check fixture is intentionally generated offline with the same schema,
# realistic tract count (~911 AL tracts), and the cacs_acs_default_vars
# catalogue.
#
# A future live-regeneration workflow may replace this with a fetched fixture from
# tidycensus::get_acs(geography = "tract", state = "AL", year = 2023,
#                     variables = cacs_acs_default_vars, geometry = TRUE)
#
# Run: Rscript data-raw/al_acs_fetch.R
# ============================================================================

library(sf)
library(tibble)
library(dplyr)

set.seed(20260522)  # Blueprint v1.0.1 sign-off date for reproducibility

# Load the default ACS variables from data/cacs_acs_default_vars.rda
load("data/cacs_acs_default_vars.rda")
acs_vars <- cacs_acs_default_vars

# Alabama has ~1,182 census tracts as of 2020 census; older ACS surveys
# used ~911 tracts. For v0.1 synthetic fixture, use 911 tracts to match
# §17.3 spec.
n_tracts <- 911L

# Generate synthetic GEOID: state(01) + county(001-067 cycling) + tract(6-digit)
counties <- sprintf("%03d", c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23,
                              25, 27, 29, 31, 33, 35, 37, 39, 41, 43,
                              45, 47, 49, 51, 53, 55, 57, 59, 61, 63,
                              65, 67, 69, 71, 73, 75, 77, 79, 81, 83,
                              85, 87, 89, 91, 93, 95, 97, 99, 101, 103,
                              105, 107, 109, 111, 113, 115, 117, 119,
                              121, 123, 125, 127, 129, 131, 133))

geoid_list <- character(n_tracts)
name_list  <- character(n_tracts)
for (i in seq_len(n_tracts)) {
  county_fips <- counties[((i - 1L) %% length(counties)) + 1L]
  tract_num <- sprintf("%06d", (i - 1L) %/% length(counties) * 100 + 1L)
  geoid_list[i] <- paste0("01", county_fips, tract_num)
  name_list[i]  <- sprintf("Census Tract %s.%s, County %s, Alabama",
                           substr(tract_num, 1, 4),
                           substr(tract_num, 5, 6),
                           county_fips)
}

# Generate synthetic geometries (small non-overlapping polygons across AL)
# AL roughly bounded by lon [-88.5, -84.9], lat [30.2, 35.0]
grid_n <- ceiling(sqrt(n_tracts))
lon_grid <- seq(-88.5, -84.9, length.out = grid_n)
lat_grid <- seq( 30.2,  35.0, length.out = grid_n)

geom_list <- vector("list", n_tracts)
for (i in seq_len(n_tracts)) {
  row_idx <- ((i - 1L) %/% grid_n) + 1L
  col_idx <- ((i - 1L) %%  grid_n) + 1L
  lon0 <- lon_grid[col_idx]
  lat0 <- lat_grid[row_idx]
  dx <- 0.03
  dy <- 0.03
  geom_list[[i]] <- sf::st_polygon(list(rbind(
    c(lon0,      lat0),
    c(lon0 + dx, lat0),
    c(lon0 + dx, lat0 + dy),
    c(lon0,      lat0 + dy),
    c(lon0,      lat0)
  )))
}
geom_sfc <- sf::st_sfc(geom_list, crs = 4269)

# Expand to long format: n_tracts × length(cacs_acs_default_vars)
sample_alabama_subset <- expand.grid(
  GEOID = geoid_list,
  variable = unname(acs_vars),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

# Realistic synthetic estimates per variable type
estimate_ranges <- list(
  B01003_001 = c(2000, 5000),      # total_pop
  B17001_001 = c(1800, 4800),      # pov_denom
  B17001_002 = c(200, 1200),       # pov_below
  B19013_001 = c(35000, 75000),    # med_hh_inc
  B19301_001 = c(15000, 40000),    # per_cap_inc
  B11001_001 = c(800, 2000),       # total_hh
  B22003_001 = c(800, 2000),       # snap_denom
  B22003_002 = c(50, 400),         # snap_recv
  B19056_001 = c(800, 2000),       # ssi_total
  B19056_002 = c(20, 200),         # ssi_hh
  B23025_001 = c(1500, 3500),      # emp_denom
  B23025_002 = c(900, 2500),       # labor_force
  B23025_003 = c(850, 2400),       # civ_labor
  B23025_005 = c(50, 300)          # unemployed
)

sample_alabama_subset$estimate <- vapply(seq_len(nrow(sample_alabama_subset)),
  function(i) {
    v <- sample_alabama_subset$variable[i]
    r <- estimate_ranges[[v]]
    if (is.null(r)) c(100, 1000) else round(runif(1L, r[1], r[2]))
  },
  numeric(1L)
)
sample_alabama_subset$moe <- round(sample_alabama_subset$estimate * runif(nrow(sample_alabama_subset), 0.05, 0.25))

# Inject realistic suppression (~5% rate)
n_supp <- ceiling(nrow(sample_alabama_subset) * 0.05)
supp_idx <- sample(seq_len(nrow(sample_alabama_subset)), n_supp)
sample_alabama_subset$estimate[supp_idx] <- NA_real_
sample_alabama_subset$moe[supp_idx] <- -555555555  # MOE sentinel for suppressed

# Build NAME column for each row
sample_alabama_subset$NAME <- name_list[match(sample_alabama_subset$GEOID, geoid_list)]

# Final column order per §20.5 tidy ACS schema
sample_alabama_subset <- sample_alabama_subset[, c("GEOID", "NAME", "variable", "estimate", "moe")]

# Attach geometry (each GEOID maps to one geometry)
geom_per_row <- geom_sfc[match(sample_alabama_subset$GEOID, geoid_list)]
sample_alabama_subset_sf <- sf::st_sf(sample_alabama_subset, geometry = geom_per_row, crs = 4269)

# Final validation
stopifnot(
  inherits(sample_alabama_subset_sf, "sf"),
  nrow(sample_alabama_subset_sf) == n_tracts * length(acs_vars),
  sf::st_crs(sample_alabama_subset_sf)$epsg == 4269L,
  all(grepl("^\\d{11}$", sample_alabama_subset_sf$GEOID)),
  all(c("GEOID", "NAME", "variable", "estimate", "moe", "geometry") %in% names(sample_alabama_subset_sf))
)

# Save to inst/extdata/
saveRDS(sample_alabama_subset_sf,
        "inst/extdata/sample_alabama_subset.rds",
        compress = "xz")

cat(sprintf("Saved sample_alabama_subset.rds:\n  - %d rows (%d tracts × %d vars)\n  - file size: %.1f KB\n",
            nrow(sample_alabama_subset_sf),
            n_tracts,
            length(acs_vars),
            file.info("inst/extdata/sample_alabama_subset.rds")$size / 1024))
