# ----------------------------------------------------------------------
# Generate 7 synthetic fixtures for Phase 5-6 unit tests (Step 2.5)
#
# §25.2 + §37.7 — known-answer ground truth at epsilon = 1e-9 exact.
# Pure deterministic geometry (sf POINT/POLYGON in EPSG:5070, except
# `al_10_site_sample` which preserves EPSG:4326 to match
# `cacs_alabama_sites`).
#
# Run from package root:
#   Rscript data-raw/synthetic_fixtures.R
#
# ---- Spec resolution -------------------------------------------------
# §22.10 (truth source) specifies for T22-10: A=50, A_var=10, B=200,
# B_var=400 -> inside = A_var - p^2 * B_var = 10 - 0.0625*400 = -15.
# §37.7's literal `num_moe = 10` is internally inconsistent with that
# (it would yield A_var = (10/1.645)^2 ~ 36.95, inside ~ +11.95, not
# the required -15 needed to exercise the C1->C2 fallback). We honor
# §22.10's MATH by deriving num_moe / den_moe from A_var / B_var via
# num_moe = z * sqrt(A_var), den_moe = z * sqrt(B_var). For T22-09
# this still yields num_moe = 16.45, den_moe = 32.90 (matching §37.7
# literal). For T22-10 the resolved num_moe is ~5.20 instead of 10.
# This is the only spec ambiguity resolved by this script.
# ----------------------------------------------------------------------

library(sf)
library(tibble)

target_dir <- "inst/testdata"
if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)

# ---- helpers ---------------------------------------------------------

# Axis-aligned square polygon (closed ring) in EPSG:5070 by default.
.mk_sq <- function(xmin, ymin, side = 1000, crs = 5070) {
  sf::st_polygon(list(rbind(
    c(xmin, ymin),
    c(xmin + side, ymin),
    c(xmin + side, ymin + side),
    c(xmin, ymin + side),
    c(xmin, ymin)
  )))
}

# Axis-aligned rectangle by explicit bounds in EPSG:5070.
.mk_rect <- function(xmin, ymin, xmax, ymax, crs = 5070) {
  sf::st_polygon(list(rbind(
    c(xmin, ymin),
    c(xmax, ymin),
    c(xmax, ymax),
    c(xmin, ymax),
    c(xmin, ymin)
  )))
}

# ----------------------------------------------------------------------
# Fixture 1: Master 3-site x 4-tract grid (EPSG:5070, 1km squares)
# §25.2 / §37.7 — estimate=c(100,200,300,400), MOE=c(12,18,24,30).
# ----------------------------------------------------------------------
tracts_4 <- sf::st_sf(
  GEOID    = sprintf("0100100000%d", 1:4),
  NAME     = sprintf("Tract %s", LETTERS[1:4]),
  variable = "B17001_002",
  estimate = c(100, 200, 300, 400),
  moe      = c( 12,  18,  24,  30),
  geometry = sf::st_sfc(
    .mk_sq(0,    0),       # Tract A (0-1km, 0-1km)
    .mk_sq(1000, 0),       # Tract B (1-2km, 0-1km)
    .mk_sq(0,    1000),    # Tract C (0-1km, 1-2km)
    .mk_sq(1000, 1000),    # Tract D (1-2km, 1-2km)
    crs = 5070
  )
)

sites_3 <- sf::st_sf(
  site_id        = sprintf("SITE_%02d", 1:3),
  drive_time_min = 15L,
  geometry = sf::st_sfc(
    sf::st_point(c( 500,  500)),
    sf::st_point(c(1500,  500)),
    sf::st_point(c(1000, 1000)),
    crs = 5070
  )
)

saveRDS(
  list(sites = sites_3, tracts = tracts_4),
  file.path(target_dir, "synthetic_3site_4tract.rds"),
  compress = "xz"
)

# ----------------------------------------------------------------------
# Fixture 2: T21-09 single-site quarter overlap (area_wt = 0.25)
# Tract: 1km x 1km square (area = 1,000,000 m^2).
# Isochrone: 500m x 500m square at lower-left corner (area = 250,000 m^2).
# Known answers: area_wt = 0.25, est_total = 25, moe_total = 3.0.
# ----------------------------------------------------------------------
tract_1 <- sf::st_sf(
  GEOID    = "01001000001",
  NAME     = "Tract A",
  variable = "B17001_002",
  estimate = 100,
  moe      = 12,
  geometry = sf::st_sfc(.mk_sq(0, 0, side = 1000), crs = 5070)
)

iso_quarter <- sf::st_sf(
  site_id        = "S01",
  drive_time_min = 15L,
  geometry       = sf::st_sfc(.mk_sq(0, 0, side = 500), crs = 5070)
)

saveRDS(
  list(
    site              = iso_quarter,
    tract             = tract_1,
    expected_area_wt  = 0.25,
    expected_estimate = 25,
    expected_moe      = 3.0
  ),
  file.path(target_dir, "synthetic_single_site.rds"),
  compress = "xz"
)

# ----------------------------------------------------------------------
# Fixture 3: T21-10 two-tract weighted-sum (50% / 30%)
# Tract1: 1km x 1km at (0,0)-(1000,1000), est=1000, MOE=50.
# Tract2: 1km x 1km at (1000,0)-(2000,1000), est=2000, MOE=80.
# Isochrone: 1500m x 500m rectangle covering
#   - half of tract1: (500,0)-(1000,1000) = 500,000 m^2 / 1,000,000 = 0.5
#   - 30% of tract2: (1000,0)-(1300,1000) = 300,000 m^2 / 1,000,000 = 0.3
# Known answers: area_wt = c(0.5, 0.3), weight_sum = 0.8,
#                est_total = 0.5*1000 + 0.3*2000 = 1100.
# ----------------------------------------------------------------------
tracts_2 <- sf::st_sf(
  GEOID    = c("01001000001", "01001000002"),
  NAME     = c("Tract 1", "Tract 2"),
  variable = "B01003_001",
  estimate = c(1000, 2000),
  moe      = c(  50,   80),
  geometry = sf::st_sfc(
    .mk_sq(0,    0, side = 1000),
    .mk_sq(1000, 0, side = 1000),
    crs = 5070
  )
)

iso_2tract <- sf::st_sf(
  site_id        = "S01",
  drive_time_min = 15L,
  geometry       = sf::st_sfc(.mk_rect(500, 0, 1300, 1000), crs = 5070)
)

saveRDS(
  list(
    site               = iso_2tract,
    tracts             = tracts_2,
    expected_area_wts  = c(0.5, 0.3),
    expected_weight_sum = 0.8,
    expected_est_total = 1100
  ),
  file.path(target_dir, "synthetic_2tract.rds"),
  compress = "xz"
)

# ----------------------------------------------------------------------
# Fixture 4: T22-09/10/11/12 known-answer MOE carrier table (4 x 8)
# §22.10 fallback chain coverage:
#   row 1 -> T22-09: C1 success         (inside = +75,  moe ~ 0.07122)
#   row 2 -> T22-10: C1 -> C2 fallback  (inside = -15,  moe ~ 0.04866, reason = negative_variance)
#   row 3 -> T22-11: zero denominator   (moe = NA, reason = zero_denominator)
#   row 4 -> T22-12: missing input MOE  (moe = NA, reason = missing_moe)
#
# z = 1.645 (90% CI per Bureau convention).
# num_moe / den_moe derived as z * sqrt(A_var | B_var) to keep §22.10
# math self-consistent — see "Spec resolution" note at file top.
# ----------------------------------------------------------------------
z <- 1.645

# Row 1 (T22-09): A_var = 100, B_var = 400 -> inside = 100 - 0.0625*400 = +75
num_moe_T09 <- z * sqrt(100)   # 16.45
den_moe_all <- z * sqrt(400)   # 32.90

# Row 2 (T22-10): A_var = 10,  B_var = 400 -> inside = 10  - 0.0625*400 = -15
num_moe_T10 <- z * sqrt(10)    # 5.201947...

moe_carriers <- tibble::tibble(
  case_id  = c("T22-09_C1_success",
               "T22-10_C1_to_C2",
               "T22-11_zero_den",
               "T22-12_missing_moe"),
  variable = "poverty_rate",
  num_est  = c(50,            50,            50,            50),
  num_moe  = c(num_moe_T09,   num_moe_T10,   num_moe_T09,   NA_real_),
  den_est  = c(200,           200,           1e-12,         200),
  den_moe  = c(den_moe_all,   den_moe_all,   den_moe_all,   den_moe_all),
  expected_moe             = c(0.07122, 0.04813, NA_real_, NA_real_),
  expected_fallback_reason = c(NA_character_,
                               "negative_variance",
                               "zero_denominator",
                               "missing_moe")
)

saveRDS(moe_carriers,
        file.path(target_dir, "synthetic_moe_carriers.rds"),
        compress = "xz")

# ----------------------------------------------------------------------
# Fixture 5: T22-09 dedicated single-row (C1 success, inside = +75)
# ----------------------------------------------------------------------
saveRDS(
  list(
    inputs = list(
      num_est = 50,
      num_moe = num_moe_T09,    # 16.45
      den_est = 200,
      den_moe = den_moe_all,    # 32.90
      z       = z
    ),
    expected_inside          = 75,
    expected_inside_positive = TRUE,
    expected_moe             = 0.07122,
    expected_formula         = "proportion_subset",
    expected_fallback        = FALSE
  ),
  file.path(target_dir, "synthetic_moe_C1_success.rds"),
  compress = "xz"
)

# ----------------------------------------------------------------------
# Fixture 6: T22-10 dedicated single-row (C1 -> C2 fallback, inside = -15)
# ----------------------------------------------------------------------
saveRDS(
  list(
    inputs = list(
      num_est = 50,
      num_moe = num_moe_T10,    # ~5.202 (resolved from spec ambiguity)
      den_est = 200,
      den_moe = den_moe_all,    # 32.90
      z       = z
    ),
    expected_inside          = -15,
    expected_inside_negative = TRUE,
    expected_moe             = 0.04813,
    expected_formula         = "general_ratio_conservative",
    expected_fallback        = TRUE,
    expected_fallback_reason = "negative_variance"
  ),
  file.path(target_dir, "synthetic_moe_C1_to_C2.rds"),
  compress = "xz"
)

# ----------------------------------------------------------------------
# Fixture 7: Alabama 10-site sample (EPSG:4326)
# Re-export the Step 1.4 cacs_alabama_sites lazy-load dataset into
# inst/testdata/ so Phase 3 integration tests can readRDS() directly
# without loading the package namespace.
# ----------------------------------------------------------------------
sites_rda <- "data/cacs_alabama_sites.rda"
if (!file.exists(sites_rda)) {
  stop("Missing prerequisite: ", sites_rda,
       " (run data-raw/cacs_alabama_sites.R first).")
}
load(sites_rda)  # introduces `cacs_alabama_sites` into this env
stopifnot(
  inherits(cacs_alabama_sites, "sf"),
  nrow(cacs_alabama_sites) == 10,
  sf::st_crs(cacs_alabama_sites)$epsg == 4326
)
saveRDS(cacs_alabama_sites,
        file.path(target_dir, "al_10_site_sample.rds"),
        compress = "xz")

# ----------------------------------------------------------------------
# Verification (geometry validity + file presence + ground-truth math)
# This script owns the seven files listed below. `inst/testdata/` also contains
# replay and bug-regression fixtures generated by other data-raw scripts, so do
# not assert on the directory-wide `.rds` count.
# ----------------------------------------------------------------------
expected_files <- file.path(target_dir, c(
  "synthetic_3site_4tract.rds",
  "synthetic_single_site.rds",
  "synthetic_2tract.rds",
  "synthetic_moe_carriers.rds",
  "synthetic_moe_C1_success.rds",
  "synthetic_moe_C1_to_C2.rds",
  "al_10_site_sample.rds"
))
generated <- expected_files[file.exists(expected_files)]
message(sprintf("Generated %d fixtures in %s:", length(generated), target_dir))
for (f in generated) {
  message(sprintf("  %s (%d bytes)", basename(f), file.size(f)))
}

stopifnot(length(generated) == 7L)
stopifnot(all(sf::st_is_valid(tracts_4)))
stopifnot(all(sf::st_is_valid(sites_3)))
stopifnot(sf::st_is_valid(tract_1))
stopifnot(sf::st_is_valid(iso_quarter))
stopifnot(all(sf::st_is_valid(tracts_2)))
stopifnot(sf::st_is_valid(iso_2tract))

# Ground-truth math invariants (epsilon = 1e-9)
stopifnot(abs(as.numeric(sf::st_area(iso_quarter)) /
              as.numeric(sf::st_area(tract_1)) - 0.25) < 1e-9)
inter_2 <- sf::st_intersection(iso_2tract, tracts_2)
overlap_areas <- as.numeric(sf::st_area(inter_2))
tract_areas   <- as.numeric(sf::st_area(tracts_2))
area_wts      <- overlap_areas / tract_areas
stopifnot(all(abs(area_wts - c(0.5, 0.3)) < 1e-9))

# C1 inside invariant
p <- 50 / 200
inside_T09 <- (num_moe_T09 / z)^2 - p^2 * (den_moe_all / z)^2
inside_T10 <- (num_moe_T10 / z)^2 - p^2 * (den_moe_all / z)^2
stopifnot(abs(inside_T09 - 75)  < 1e-9)
stopifnot(abs(inside_T10 + 15)  < 1e-9)

message("All 7 synthetic-fixture script outputs generated and verified.")
