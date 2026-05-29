# ----------------------------------------------------------------------
# Generate cacs_alabama_sites
#
# §17.2 — 10-row sf POINT, sample input for cacs_run() and the
# getting-started vignette.
#
# ------------------------------------------------------------------
# DE-IDENTIFICATION AUDIT TRAIL  (Step 8.3, Phase 8 formalization)
# ------------------------------------------------------------------
# The coordinates produced by this script are PUBLIC Alabama city
# centroids (US Census Gazetteer / GNIS) perturbed with a reproducible
# uniform jitter. They are NOT real Pre-K facility coordinates, and this
# script does NOT read, transform, or surface any administrative record.
# Any real administrative coordinates are held privately and are outside
# the package ship boundary.
#
# This design satisfies §17.8 Q17-1 by anchoring jitter on public
# centroids (so that any re-identification attempt must necessarily
# begin from public data) and applying a ≈ 0.5 mile uniform offset.
#
# Reproducibility parameters (commit-pinned):
#   set.seed(20260522L)            # blueprint v1.0.1 sign-off date
#   jitter_radius_deg = 0.007      # ≈ 654 m E-W, ≈ 774 m N-S at lat 33°N
#                                  # = 0.41–0.48 mile uniform half-width
#                                  # (within §17.8 Q17-1 ≈ 0.5 mile envelope)
#   distribution      = runif(-radius, +radius)  per axis, independent
#   crs               = EPSG:4326 anchor; jitter applied directly in
#                       degrees (matches Step 1.4 stub bit-for-bit so
#                       that data/cacs_alabama_sites.rda is stable
#                       across the Step 1.4 → Step 8.3 transition)
#
# Reproducibility token (digest::digest of the parameter list above):
#   9bd2bdddfacdebcf3656a7d127bb7828
# Any change to seed / radius / distribution / crs MUST regenerate this
# token and be accompanied by a versioned justification in the commit
# message (e.g. "Step X.Y: re-jitter — token = <new>").
#
# A future de-identification audit may revisit the radius per §17.8 Q17-1
# empirical recalibration; that work would deterministically re-jitter via the
# same seed plus a new radius value and update the token above.
#
# Run: Rscript data-raw/cacs_alabama_sites.R
# ----------------------------------------------------------------------

library(sf)
library(tibble)
library(usethis)

# Reproducible jitter — change this seed only with a versioned justification.
# 20260522 = blueprint v1.0.1 sign-off date.
set.seed(20260522L)

# Public Alabama city centroids (US Census Gazetteer / GNIS).
# These are city centroids, NOT real Pre-K facility coordinates.
al_city_centroids <- tibble::tibble(
  city         = c("Birmingham", "Mobile", "Huntsville", "Montgomery",
                   "Tuscaloosa", "Auburn", "Decatur", "Florence",
                   "Dothan", "Gadsden"),
  county_fips  = c("01073", "01097", "01089", "01101",
                   "01125", "01081", "01103", "01077",
                   "01069", "01055"),
  region_label = c("Birmingham Metro", "South Coast", "North", "Central",
                   "West Central", "East Central", "North Central", "Northwest",
                   "Southeast", "Northeast"),
  lon_anchor   = c(-86.8104, -88.0399, -86.5861, -86.3000,
                   -87.5692, -85.4808, -86.9833, -87.6773,
                   -85.3905, -86.0066),
  lat_anchor   = c( 33.5186,  30.6954,  34.7304,  32.3668,
                    33.2098,  32.6099,  34.6059,  34.7998,
                    31.2232,  34.0143)
)

# Reproducible jitter ~0.5 mile uniform half-width
# (≈ 0.007 degrees ≈ 654 m E-W, 774 m N-S at AL latitude).
jitter_radius_deg <- 0.007
n_sites <- nrow(al_city_centroids)
lon_jit <- runif(n_sites, -jitter_radius_deg, jitter_radius_deg)
lat_jit <- runif(n_sites, -jitter_radius_deg, jitter_radius_deg)

cacs_alabama_sites <- sf::st_sf(
  site_id      = sprintf("AL_SITE_%02d", seq_len(n_sites)),
  site_name    = sprintf("Sample Site %d", seq_len(n_sites)),
  county_fips  = al_city_centroids$county_fips,
  region_label = al_city_centroids$region_label,
  geometry     = sf::st_sfc(
    Map(function(x, y) sf::st_point(c(x, y)),
        al_city_centroids$lon_anchor + lon_jit,
        al_city_centroids$lat_anchor + lat_jit),
    crs = 4326
  )
)

# Generation-time invariants (fail-loud — must hold for package ship).
stopifnot(
  nrow(cacs_alabama_sites) == 10L,
  inherits(cacs_alabama_sites, "sf"),
  sf::st_crs(cacs_alabama_sites)$epsg == 4326L,
  all(c("site_id", "site_name", "county_fips", "region_label", "geometry")
      %in% names(cacs_alabama_sites)),
  all(grepl("^AL_SITE_\\d{2}$", cacs_alabama_sites$site_id)),
  !any(duplicated(cacs_alabama_sites$site_id)),
  all(sf::st_is_valid(cacs_alabama_sites))
)

# §17.6 — xz compression for tarball size budget.
usethis::use_data(cacs_alabama_sites, compress = "xz", overwrite = TRUE)
