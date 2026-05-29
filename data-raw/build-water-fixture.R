# ============================================================================
# data-raw/build-water-fixture.R — Generate fixture_water_tract_baldwin.rds
#                                  for BUG-001 reproduction (water tract
#                                  01003990000 with ALAND = 0 / degenerate
#                                  geometry aborting cacs_intersect_weight()).
#
# v0.2.0 Step 1.4 PROVENANCE SCRIPT — single source of truth for the
# water-tract offline regression fixture built once from live ACS and used by
# Phase 5+ regression tests once the BUG-001 fix lands.
#
# Re-run conditions (any of):
#   * Census Bureau revises 2020 tract boundaries and 01003990000 changes
#   * tidycensus / tigris version bump shifts geometry vintage
#   * Bumping the canonical ACS year (currently 2023L)
#
# Re-run command (from package root, with CENSUS_API_KEY in env):
#   Rscript data-raw/build-water-fixture.R
#
# Output: inst/testdata/fixture_water_tract_baldwin.rds (~3 KB)
#   sf tibble, 3 rows × 6 cols (GEOID, NAME, variable, estimate, moe,
#   geometry), 1 variable (B01003_001 total population), EPSG:4269.
#
# Cross-ref: v020-plan Step 1.4; §20.5 (cacs_acs_prefetch 6-col schema);
# §21 (cacs_intersect_weight area_wt = 0 degenerate trigger = BUG-001).
# ============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(catchmentACS)
})

# Census API is deterministic for a given (state, year, variables) tuple —
# no randomness involved; no seed needed.

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

# Target 3 Baldwin Co. (county FIPS 003) GEOIDs:
#   - 01003020100, 01003020200 = real land tracts
#   - 01003990000               = WATER tract (BUG-001 offender, ALAND = 0)
target_geoids <- c("01003020100", "01003020200", "01003990000")

# Defensive fallback: if either land tract is missing in the live ACS pull
# (Census tract renames between vintages can happen), substitute the first
# 2 valid Baldwin (county 003) GEOIDs whose tract code does NOT start with
# 99 and whose geometry has non-zero area. The water tract 01003990000
# itself is required — BUG-001 cannot be reproduced without it.
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
# Strip provenance attrs from the parent (slim fixture down)
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

# Verify BUG-001 reproduction trigger: water tract has degenerate area.
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
