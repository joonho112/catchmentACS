# ============================================================================
# C-07 tract-audit attribute tests.
# ============================================================================


.c07_pair_2tract <- function() {
  fx <- helper_load_synthetic_2tract()
  cx <- -86.80
  cy <-  33.50

  iso_4326 <- sf::st_sf(
    tibble::tibble(
      site_id                          = fx$site$site_id,
      drive_time_min                   = as.integer(fx$site$drive_time_min),
      provider                         = "osrm",
      profile                          = "car",
      osm_snapshot_date                = NA_character_,
      routing_engine_version           = "stub/0.0.0",
      polygon_simplification_tolerance = NA_real_,
      generated_at                     = as.POSIXct("2026-05-22 00:00:00",
                                                    tz = "UTC"),
      isochrone_empty                  = FALSE,
      provider_requested               = "osrm",
      provider_downgrade               = FALSE,
      osm_snapshot_status              = NA_character_,
      failure_reason                   = NA_character_,
      retry_count                      = 1L,
      ring_topology                    = "cumulative"
    ),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(cx,         cy),
        c(cx + 0.030, cy),
        c(cx + 0.030, cy + 0.005),
        c(cx,         cy + 0.005),
        c(cx,         cy)
      ))),
      crs = 4326
    )
  )

  acs_4269 <- sf::st_sf(
    tibble::tibble(
      GEOID    = fx$tracts$GEOID,
      NAME     = fx$tracts$NAME,
      variable = fx$tracts$variable,
      estimate = fx$tracts$estimate,
      moe      = fx$tracts$moe
    ),
    geometry = sf::st_sfc(
      sf::st_multipolygon(list(list(rbind(
        c(cx - 0.005, cy - 0.005),
        c(cx + 0.020, cy - 0.005),
        c(cx + 0.020, cy + 0.010),
        c(cx - 0.005, cy + 0.010),
        c(cx - 0.005, cy - 0.005)
      )))),
      sf::st_multipolygon(list(list(rbind(
        c(cx + 0.020, cy - 0.005),
        c(cx + 0.040, cy - 0.005),
        c(cx + 0.040, cy + 0.010),
        c(cx + 0.020, cy + 0.010),
        c(cx + 0.020, cy - 0.005)
      )))),
      crs = 4269
    )
  )

  list(
    iso = iso_4326,
    acs = acs_4269,
    fx = fx
  )
}

.c07_pair_empty <- function() {
  pair <- .c07_pair_2tract()
  iso <- pair$iso
  cx <- -85.80
  cy <-  33.50
  sf::st_geometry(iso) <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(cx,         cy),
      c(cx + 0.030, cy),
      c(cx + 0.030, cy + 0.005),
      c(cx,         cy + 0.005),
      c(cx,         cy)
    ))),
    crs = 4326
  )
  pair$iso <- iso
  pair
}


test_that("C07-AUDIT-01 default cacs_intersect_weight has no tract audit", {
  pair <- .c07_pair_2tract()
  withr::local_options(catchmentACS.cache_intersect = FALSE)

  out <- suppressMessages(
    cacs_intersect_weight(pair$iso, pair$acs, verbose = FALSE)
  )

  expect_s3_class(out, "tbl_df")
  expect_null(attr(out, "cacs_tract_audit", exact = TRUE))
})


test_that("C07-AUDIT-02 opt-in tract audit has locked schema and values", {
  pair <- .c07_pair_2tract()
  withr::local_options(catchmentACS.cache_intersect = FALSE)

  out <- suppressMessages(
    cacs_intersect_weight(
      pair$iso, pair$acs,
      keep_tract_audit = TRUE,
      verbose = FALSE
    )
  )
  audit <- attr(out, "cacs_tract_audit", exact = TRUE)

  expect_s3_class(audit, "tbl_df")
  expect_identical(
    names(audit),
    c("site_id", "drive_time_min", "GEOID", "area_wt",
      "int_area_m2", "tract_area_m2")
  )
  expect_type(audit$site_id, "character")
  expect_type(audit$drive_time_min, "integer")
  expect_type(audit$GEOID, "character")
  expect_type(audit$area_wt, "double")
  expect_type(audit$int_area_m2, "double")
  expect_type(audit$tract_area_m2, "double")

  audit <- audit[order(audit$GEOID), ]
  expect_equal(nrow(audit), 2L)
  expect_true(all(audit$area_wt > 0 & audit$area_wt <= 1))
  expect_true(all(audit$int_area_m2 >= 0))
  expect_true(all(audit$tract_area_m2 > 0))
})


test_that("C07-AUDIT-03 audit area weights agree with public weight_sum", {
  pair <- .c07_pair_2tract()
  withr::local_options(catchmentACS.cache_intersect = FALSE)

  out <- suppressMessages(
    cacs_intersect_weight(
      pair$iso, pair$acs,
      keep_tract_audit = TRUE,
      verbose = FALSE
    )
  )
  audit <- attr(out, "cacs_tract_audit", exact = TRUE)
  public <- out[out$variable == "B01003_001", , drop = FALSE]

  expect_equal(nrow(public), 1L)
  expect_equal(sum(audit$area_wt), public$weight_sum, tolerance = 1e-6)
})


test_that("C07-AUDIT-04 optional audit attribute is validated when present", {
  pair <- .c07_pair_2tract()
  withr::local_options(catchmentACS.cache_intersect = FALSE)

  out <- suppressMessages(
    cacs_intersect_weight(
      pair$iso, pair$acs,
      keep_tract_audit = TRUE,
      verbose = FALSE
    )
  )
  expect_true(.validate_intersect_output(out, abort = FALSE))

  bad <- out
  audit <- attr(bad, "cacs_tract_audit", exact = TRUE)
  audit$area_wt[[1L]] <- 2
  attr(bad, "cacs_tract_audit") <- audit
  expect_false(.validate_intersect_output(bad, abort = FALSE))
  expect_error(
    .validate_intersect_output(bad, abort = TRUE),
    class = "catchmentACS_error_schema"
  )

  bad_na <- out
  audit_na <- attr(bad_na, "cacs_tract_audit", exact = TRUE)
  audit_na$int_area_m2[[1L]] <- NA_real_
  attr(bad_na, "cacs_tract_audit") <- audit_na
  expect_false(.validate_intersect_output(bad_na, abort = FALSE))

  bad_extra <- out
  audit_extra <- attr(bad_extra, "cacs_tract_audit", exact = TRUE)
  audit_extra$unexpected <- TRUE
  attr(bad_extra, "cacs_tract_audit") <- audit_extra
  expect_false(.validate_intersect_output(bad_extra, abort = FALSE))
})


test_that("C07-AUDIT-05 audit attr works for no-intersection empty sites", {
  pair <- .c07_pair_empty()
  withr::local_options(catchmentACS.cache_intersect = FALSE)

  out <- suppressWarnings(suppressMessages(
    cacs_intersect_weight(
      pair$iso, pair$acs,
      keep_tract_audit = TRUE,
      verbose = FALSE
    )
  ))
  audit <- attr(out, "cacs_tract_audit", exact = TRUE)

  expect_s3_class(audit, "tbl_df")
  expect_identical(nrow(audit), 0L)
  expect_identical(
    names(audit),
    c("site_id", "drive_time_min", "GEOID", "area_wt",
      "int_area_m2", "tract_area_m2")
  )
})


test_that("C07-AUDIT-06 cache handles default and opt-in ordering", {
  pair <- .c07_pair_2tract()

  with_test_cache({
    # FALSE -> FALSE: default output remains byte-identical and audit-free.
    out_default <- suppressMessages(
      cacs_intersect_weight(pair$iso, pair$acs, verbose = FALSE)
    )
    expect_null(attr(out_default, "cacs_tract_audit", exact = TRUE))

    out_hit_default <- suppressMessages(
      cacs_intersect_weight(pair$iso, pair$acs, verbose = FALSE)
    )
    expect_identical(out_default, out_hit_default)
    expect_null(attr(out_hit_default, "cacs_tract_audit", exact = TRUE))

    # FALSE -> TRUE: a prior no-audit cache must not prevent opt-in audit.
    out_hit_audit <- suppressMessages(
      cacs_intersect_weight(
        pair$iso, pair$acs,
        keep_tract_audit = TRUE,
        verbose = FALSE
      )
    )
    audit <- attr(out_hit_audit, "cacs_tract_audit", exact = TRUE)
    expect_s3_class(audit, "tbl_df")
    expect_equal(nrow(audit), 2L)

    # TRUE -> FALSE: a cached audited object must not leak the attr to callers
    # who did not request it.
    out_after_audit_default <- suppressMessages(
      cacs_intersect_weight(pair$iso, pair$acs, verbose = FALSE)
    )
    expect_null(attr(out_after_audit_default, "cacs_tract_audit", exact = TRUE))
    expected_default <- out_hit_audit
    attr(expected_default, "cacs_tract_audit") <- NULL
    expect_identical(out_after_audit_default, expected_default)

    # TRUE -> TRUE: the opt-in cache path keeps the audit attr available.
    out_after_audit_true <- suppressMessages(
      cacs_intersect_weight(
        pair$iso, pair$acs,
        keep_tract_audit = TRUE,
        verbose = FALSE
      )
    )
    expect_s3_class(attr(out_after_audit_true, "cacs_tract_audit", exact = TRUE),
                    "tbl_df")
  })
})


test_that("C07-AUDIT-07 cacs_run accepts keep_tract_audit in weight_args", {
  expect_true("keep_tract_audit" %in% .CACS_RUN_ARG_KEYS$wgt)
  expect_true("keep_tract_audit" %in% names(formals(cacs_intersect_weight)))
})
