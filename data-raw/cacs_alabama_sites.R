# data-raw/cacs_alabama_sites.R - writes data/cacs_alabama_sites.rda, the
# 10-row sf POINT object used in the examples and in the getting-started
# article.
#
# Where the points come from:
#   The coordinates are public Alabama city center points (US Census Gazetteer
#   and GNIS) moved by a fixed random offset. They are not the coordinates of
#   any Pre-K site, and this script reads no record of any such site. Anyone
#   trying to work back from these points therefore starts from public data.
#
# How the offset is made:
#   set.seed(20260522L)
#   jitter_radius_deg = 0.007, about 654 m east-west and 774 m north-south at
#     latitude 33 degrees North, so a half-width of 0.41 to 0.48 mile
#   one draw from runif(-radius, +radius) for each axis, independently
#   EPSG:4326 throughout, with the offset added in degrees
#
# The digest of that list of settings is
# 9bd2bdddfacdebcf3656a7d127bb7828. A change to the seed, the radius, the
# distribution, or the coordinate system changes the points, so it needs a new
# digest here and a reason in the commit message.
#
# Run: Rscript data-raw/cacs_alabama_sites.R

library(sf)
library(tibble)
library(usethis)

# The seed of the offset. Changing it moves every point, so change it only
# with a reason in the commit message and a new digest at the top.
set.seed(20260522L)

# Public Alabama city center points (US Census Gazetteer and GNIS), not the
# coordinates of any Pre-K site.
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

# The offset: a half-width of about 0.5 mile, that is 0.007 degrees, about
# 654 m east-west and 774 m north-south at this latitude.
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

# Checks before the data are written.
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

# xz compression, which keeps the installed package small.
usethis::use_data(cacs_alabama_sites, compress = "xz", overwrite = TRUE)
