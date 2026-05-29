# ============================================================================
# Unit tests for R/checks.R 6 internal schema validators (12 cases).
# ============================================================================


.mk_iso_sf <- function(...) {
  geom <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(-87, 33), c(-86, 33), c(-86, 34), c(-87, 34), c(-87, 33)
    ))),
    crs = 4326
  )
  base <- tibble::tibble(
    site_id                          = "S01",
    drive_time_min                   = 15L,
    provider                         = "osrm",
    profile                          = "car",
    osm_snapshot_date                = "unknown",
    routing_engine_version           = "osrm-pkg/0.0.0",
    polygon_simplification_tolerance = NA_real_,
    generated_at                     = as.POSIXct("2026-05-22 00:00:00", tz = "UTC"),
    isochrone_empty                  = FALSE,
    provider_requested               = "osrm",
    provider_downgrade               = FALSE,
    osm_snapshot_status              = "unknown_best_effort",
    failure_reason                   = NA_character_,
    retry_count                      = 1L,
    ring_topology                    = "cumulative"
  )
  out <- sf::st_sf(base, geometry = geom)
  mods <- list(...)
  for (nm in names(mods)) out[[nm]] <- mods[[nm]]
  out
}

.mk_bg_pop_sf <- function(...) {
  geom <- sf::st_sfc(
    sf::st_multipolygon(list(list(rbind(
      c(-87, 33), c(-86, 33), c(-86, 34), c(-87, 34), c(-87, 33)
    )))),
    crs = 4269
  )
  base <- tibble::tibble(
    GEOID          = "010010000011",
    tract_geoid    = "01001000001",
    population_est = 1500
  )
  out <- sf::st_sf(base, geometry = geom)
  mods <- list(...)
  for (nm in names(mods)) out[[nm]] <- mods[[nm]]
  out
}

.mk_acs_sf <- function(...) {
  geom <- sf::st_sfc(
    sf::st_multipolygon(list(list(rbind(
      c(-87, 33), c(-86, 33), c(-86, 34), c(-87, 34), c(-87, 33)
    )))),
    crs = 4269
  )
  base <- tibble::tibble(
    GEOID    = "01001000001",
    NAME     = "Census Tract 1, Autauga County, Alabama",
    variable = "B01003_001",
    estimate = 1000,
    moe      = 50
  )
  out <- sf::st_sf(base, geometry = geom)
  mods <- list(...)
  for (nm in names(mods)) out[[nm]] <- mods[[nm]]
  out
}

.mk_long_tbl <- function(...) {
  base <- tibble::tibble(
    site_id        = "S01",
    drive_time_min = 15L,
    ring_topology  = "cumulative",
    variable       = "B17001_002",
    estimate       = 25,
    moe            = 3.0,
    weight_sum     = 0.25,
    n_tracts       = 1L,
    n_tracts_num   = NA_integer_,
    n_tracts_den   = NA_integer_,
    provider       = "osrm",
    profile        = "car",
    osm_snapshot_date = "unknown",
    acs_year       = 2023L,
    weight_method  = "area",
    estimand_family = "spatial_total",
    weight_basis   = "coverage",
    moe_formula_requested = "weighted_sum",
    moe_formula_effective = "weighted_sum",
    moe_fallback   = FALSE,
    moe_fallback_reason = "n/a",
    failure_origin = "none",
    weight_uncertainty_propagated = FALSE
  )
  carriers <- tibble::tibble(
    site_id        = "S01",
    drive_time_min = 15L,
    variable       = "B17001_002",
    estimand_family = "spatial_total",
    est_total      = 25,
    var_total_raw  = 3.32,
    est_mean       = NA_real_,
    var_mean_raw   = NA_real_,
    weight_sum     = 0.25,
    n_tracts       = 1L
  )
  out <- base
  attr(out, "cacs_aggregation_carriers") <- carriers
  attr(out, "cacs_schema_version") <- "1.0"
  mods <- list(...)
  for (nm in names(mods)) out[[nm]] <- mods[[nm]]
  out
}


test_that("T-SCHEMA-01 .validate_iso_schema happy path returns TRUE", {
  iso <- .mk_iso_sf()
  expect_true(.validate_iso_schema(iso))
  expect_false(.validate_iso_schema(iso[, "site_id"], abort = FALSE))
})

test_that("T-SCHEMA-02 .validate_iso_schema sad — wrong CRS aborts", {
  iso_4269 <- sf::st_transform(.mk_iso_sf(), 4269)
  expect_error(.validate_iso_schema(iso_4269),
               class = "catchmentACS_error_schema")
  expect_false(.validate_iso_schema(iso_4269, abort = FALSE))
})

test_that("T-SCHEMA-03 .validate_acs_schema happy path returns TRUE", {
  acs <- .mk_acs_sf()
  expect_true(.validate_acs_schema(acs))
})

test_that("T-SCHEMA-04 .validate_acs_schema sad — missing column aborts", {
  acs <- .mk_acs_sf()
  acs$moe <- NULL
  expect_error(.validate_acs_schema(acs),
               class = "catchmentACS_error_schema")
})

test_that("T-SCHEMA-05 .validate_bg_pop_schema happy path returns TRUE", {
  bg <- .mk_bg_pop_sf()
  expect_true(.validate_bg_pop_schema(bg))
})

test_that("T-SCHEMA-06 .validate_bg_pop_schema sad — missing canonical columns aborts", {
  bg <- .mk_bg_pop_sf()
  bg$tract_geoid <- NULL
  expect_error(.validate_bg_pop_schema(bg),
               class = "catchmentACS_error_schema")
})

test_that("T-SCHEMA-07 .validate_intersect_output happy path returns TRUE", {
  out <- .mk_long_tbl()
  expect_true(.validate_intersect_output(out))
})

test_that("T-SCHEMA-08 .validate_intersect_output sad — missing carrier aborts", {
  out <- .mk_long_tbl()
  attr(out, "cacs_aggregation_carriers") <- NULL
  expect_error(.validate_intersect_output(out),
               class = "catchmentACS_error_schema")
})

test_that("T-SCHEMA-08b .validate_intersect_output sad — duplicate carrier keys abort", {
  out <- .mk_long_tbl()
  car <- attr(out, "cacs_aggregation_carriers")
  attr(out, "cacs_aggregation_carriers") <- rbind(car, car)
  expect_error(.validate_intersect_output(out),
               class = "catchmentACS_error_schema")
})

test_that("T-SCHEMA-08c .validate_intersect_output sad — negative raw variance aborts", {
  out <- .mk_long_tbl()
  car <- attr(out, "cacs_aggregation_carriers")
  car$var_total_raw <- -1
  attr(out, "cacs_aggregation_carriers") <- car
  expect_error(.validate_intersect_output(out),
               class = "catchmentACS_error_schema")
})

test_that("T-SCHEMA-08d carrier validator accepts population scalar family", {
  out <- .mk_long_tbl(
    estimand_family = "population_weighted_scalar_proxy",
    weight_basis = "population_mean",
    moe_formula_requested = "weighted_mean",
    moe_formula_effective = "weighted_mean"
  )
  car <- attr(out, "cacs_aggregation_carriers")
  car$estimand_family <- "population_weighted_scalar_proxy"
  car$est_total <- NA_real_
  car$var_total_raw <- NA_real_
  car$est_mean <- 25
  car$var_mean_raw <- 3.32
  attr(out, "cacs_aggregation_carriers") <- car

  expect_true(.validate_intersect_output(out))
})

test_that("T-SCHEMA-08e carrier validator rejects malformed key columns", {
  out <- .mk_long_tbl()

  car_site <- attr(out, "cacs_aggregation_carriers")
  car_site$site_id <- factor(car_site$site_id)
  attr(out, "cacs_aggregation_carriers") <- car_site
  expect_error(.validate_intersect_output(out),
               class = "catchmentACS_error_schema")

  out <- .mk_long_tbl()
  car_time <- attr(out, "cacs_aggregation_carriers")
  car_time$drive_time_min <- 15.5
  attr(out, "cacs_aggregation_carriers") <- car_time
  expect_error(.validate_intersect_output(out),
               class = "catchmentACS_error_schema")
})

test_that("T-SCHEMA-08f symmetric carrier validation rejects extra keys", {
  out <- .mk_long_tbl()
  car <- attr(out, "cacs_aggregation_carriers")
  extra <- car
  extra$variable <- "B17001_001"
  attr(out, "cacs_aggregation_carriers") <- rbind(car, extra)

  expect_true(.validate_intersect_output(out))
  expect_error(
    .validate_intersect_output(out, require_symmetric_keys = TRUE),
    class = "catchmentACS_error_schema"
  )
})

test_that("T-SCHEMA-09 .validate_propagate_output happy path returns TRUE", {
  out <- .mk_long_tbl()
  expect_true(.validate_propagate_output(out))
})

test_that("T-SCHEMA-10 .validate_propagate_output sad — NA in moe_formula_effective", {
  out <- .mk_long_tbl()
  out$moe_formula_effective <- NA_character_
  expect_error(.validate_propagate_output(out),
               class = "catchmentACS_error_schema")
})

test_that("T-SCHEMA-11 .validate_derive_rates_output happy — sanctioned rate row", {
  out <- .mk_long_tbl(variable = "poverty_rate")
  attr(out, "cacs_aggregation_carriers") <- NULL
  expect_true(.validate_derive_rates_output(out))
})

test_that("T-SCHEMA-12 .validate_derive_rates_output sad — no sanctioned rate row", {
  out <- .mk_long_tbl(variable = "unsanctioned_custom_rate")
  attr(out, "cacs_aggregation_carriers") <- NULL
  expect_error(.validate_derive_rates_output(out),
               class = "catchmentACS_error_schema")
})

test_that("T-SCHEMA-13 .validate_bg_pop_schema sad — alias-only population aborts", {
  bg <- .mk_bg_pop_sf()
  bg$population_est <- NULL
  bg$pop <- 1500
  expect_error(.validate_bg_pop_schema(bg),
               class = "catchmentACS_error_schema")
})

test_that("T-SCHEMA-14 .validate_bg_pop_schema sad — malformed GEOID aborts", {
  bg <- .mk_bg_pop_sf(GEOID = "01001000001")
  expect_error(.validate_bg_pop_schema(bg),
               class = "catchmentACS_error_schema")
})

test_that("T-SCHEMA-15 .validate_bg_pop_schema sad — negative population aborts", {
  bg <- .mk_bg_pop_sf(population_est = -1)
  expect_error(.validate_bg_pop_schema(bg),
               class = "catchmentACS_error_schema")
})
