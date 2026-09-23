# data-raw/build-water-fixture.R - writes
# inst/testdata/fixture_water_tract_baldwin.rds, the tract data that tests use
# for a water tract.
#
# Tract 01003990000 in Baldwin County is water: it has no land area and its
# geometry has an area of zero, which is the case cacs_intersect_weight() has
# to leave out of a drive-time area instead of dividing by it. The fixture
# holds that tract and two ordinary tracts next to it, so the tests need no
# network.
#
# Build it again when the Census Bureau changes the 2020 tract boundaries of
# 01003990000, when a new tidycensus or tigris version returns geometry of
# another year, or when the ACS year below changes.
#
# Run from the package root, with CENSUS_API_KEY set:
#   Rscript data-raw/build-water-fixture.R
#
# It writes an sf tibble of 3 rows and 6 columns (GEOID, NAME, variable,
# estimate, moe, geometry) for one variable (B01003_001, total population) in
# EPSG:4269, about 3 KB.

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(catchmentACS)
})

# The Census API returns the same rows for a given state, year, and list of
# variables, so nothing here is random and no seed is needed.

# Pull the full Alabama 2023 ACS for the chosen compact variable.
# B01003_001 = total population (single variable keeps the fixture small).
message("Fetching live AL 2023 ACS (B01003_001 only)...")
al_full <- cacs_acs_prefetch(
  state     = "AL",
  year      = 2023L,
  variables = "B01003_001",
  survey    = "acs5",
  geography = "tract",
  verbose   = FALSE
)

# Three Baldwin County (FIPS 003) tracts:
#   - 01003020100, 01003020200 = land tracts
#   - 01003990000              = the water tract, with no land area
target_geoids <- c("01003020100", "01003020200", "01003990000")

# Tracts are sometimes renumbered between years. If one of the two land tracts
# is missing from the download, the first two Baldwin County tracts whose code
# does not start with 99 and whose geometry has an area take its place. The
# water tract itself cannot be replaced, so a download without it stops the
# script.
have_water <- "01003990000" %in% al_full$GEOID
if (!have_water) {
  stop("Water tract 01003990000 not in live ACS pull — cannot reproduce BUG-001.")
}

baldwin_mask <- substr(al_full$GEOID, 1L, 5L) == "01003"
land_mask    <- substr(al_full$GEOID, 6L, 7L) != "99"
baldwin_land_geoids <- unique(al_full$GEOID[baldwin_mask & land_mask])

primary_land <- intersect(c("01003020100", "01003020200"), al_full$GEOID)
if (length(primary_land) < 2L) {
  fallback_needed <- 2L - length(primary_land)
  candidates <- setdiff(baldwin_land_geoids, primary_land)
  # Restrict to non-zero-area land tracts only
  cand_sf <- al_full[al_full$GEOID %in% candidates, ]
  cand_areas <- as.numeric(sf::st_area(cand_sf))
  ok_candidates <- unique(cand_sf$GEOID[cand_areas > 0])
  fallback <- head(ok_candidates, fallback_needed)
  primary_land <- c(primary_land, fallback)
  message(sprintf("Substituted %d Baldwin land tract(s): %s",
                  fallback_needed,
                  paste(fallback, collapse = ", ")))
}
target_geoids <- c(primary_land, "01003990000")
stopifnot(length(target_geoids) == 3L)

fixture <- al_full[al_full$GEOID %in% target_geoids, , drop = FALSE]
# Drop the attributes of the downloaded table, which the fixture does not need
attr(fixture, "cacs_provenance") <- NULL
attr(fixture, "cacs_acs_provenance") <- NULL
attr(fixture, "cacs_schema_version") <- NULL

stopifnot(
  inherits(fixture, "sf"),
  nrow(fixture) == 3L,
  all(c("GEOID", "NAME", "variable", "estimate", "moe", "geometry")
      %in% names(fixture)),
  setequal(fixture$GEOID, target_geoids)
)

# The point of the fixture: the water tract has an area of zero or none.
areas <- as.numeric(sf::st_area(fixture))
water_idx <- which(fixture$GEOID == "01003990000")
stopifnot(length(water_idx) == 1L)
stopifnot(areas[water_idx] <= 0 || !is.finite(areas[water_idx]))

out_path <- "inst/testdata/fixture_water_tract_baldwin.rds"
saveRDS(fixture, file = out_path, compress = "xz")

cat(sprintf(
  "Wrote %s (%d rows, %.1f KB) — BUG-001 water tract %s area = %.1f m^2.\n",
  out_path,
  nrow(fixture),
  file.info(out_path)$size / 1024,
  fixture$GEOID[water_idx],
  areas[water_idx]
))
