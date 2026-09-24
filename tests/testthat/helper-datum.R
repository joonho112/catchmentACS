# Tolerance for comparing weighted values with values stored in the package.
#
# The drive-time areas are in WGS 84 (EPSG:4326) and the tracts in NAD83
# (EPSG:4269); both are transformed to EPSG:5070 before areas are measured.
# The change from WGS 84 to NAD83 is the one that PROJ chooses. Without datum
# grid files, PROJ treats the two as the same, as on the computer where the
# stored values were made. With grid files, it moves WGS 84 coordinates by a
# meter or two, which changes the overlap areas and so the weighted values in
# the fourth or fifth significant digit.

# How far apart, in meters, the same longitude and latitude end up in
# EPSG:5070 when read as WGS 84 and as NAD83, at a point in Alabama.
datum_shift_m <- function() {
  wgs84 <- sf::st_sfc(sf::st_point(c(-86.8, 33.5)), crs = 4326)
  nad83 <- sf::st_sfc(sf::st_point(c(-86.8, 33.5)), crs = 4269)
  xy_wgs84 <- sf::st_coordinates(sf::st_transform(wgs84, 5070))
  xy_nad83 <- sf::st_coordinates(sf::st_transform(nad83, 5070))
  sqrt(sum((xy_wgs84 - xy_nad83)^2))
}

# 1e-8 (the values must be the same) when PROJ does not shift WGS 84 against
# NAD83, and 0.1 percent when it does.
stored_value_tolerance <- function() {
  if (datum_shift_m() < 1e-6) 1e-8 else 1e-3
}
