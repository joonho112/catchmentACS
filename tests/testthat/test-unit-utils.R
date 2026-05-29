# ============================================================================
# Unit tests for R/utils.R — .repair_geometry, .safe_geom, .empty_site_result,
# pretty-printers. 13 cases.
# ============================================================================

test_that("T-UTIL-01 valid passthrough is a no-op", {
  square <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    crs = 5070
  )
  x <- sf::st_sf(id = 1L, geometry = square)
  out <- .repair_geometry(x)
  expect_identical(sf::st_geometry(out), sf::st_geometry(x))
})

test_that("T-UTIL-02 NULL passthrough returns NULL", {
  expect_null(.repair_geometry(NULL))
})

test_that("T-UTIL-03 empty sfc returns as-is, no abort", {
  empty <- sf::st_sfc(crs = 5070)
  out <- .repair_geometry(empty)
  expect_s3_class(out, "sfc")
  expect_length(out, 0)
})

test_that("T-UTIL-04 wrong class aborts with schema condition", {
  expect_error(
    .repair_geometry(data.frame(x = 1)),
    class = "catchmentACS_error_schema"
  )
})

test_that("T-UTIL-05 self-intersection is repaired via st_make_valid", {
  # Bowtie polygon (self-intersecting at origin)
  bow <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(2, 2), c(0, 2), c(2, 0), c(0, 0)))),
    crs = 5070
  )
  x <- sf::st_sf(id = 1L, geometry = bow)
  expect_warning(out <- .repair_geometry(x),
                 class = "catchmentACS_warning_runtime")
  expect_true(all(sf::st_is_valid(out)))
})

test_that("T-UTIL-06 GEOMETRYCOLLECTION is collapsed to POLYGON union", {
  poly <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  pt   <- sf::st_point(c(5, 5))
  gc   <- sf::st_geometrycollection(list(poly, pt))
  x    <- sf::st_sf(id = 1L,
                    geometry = sf::st_sfc(gc, crs = 5070))
  expect_warning(out <- .repair_geometry(x),
                 class = "catchmentACS_warning_runtime")
  expect_false(any(vapply(sf::st_geometry(out), inherits, logical(1),
                          what = "GEOMETRYCOLLECTION")))
})

test_that("T-UTIL-07 mixed POLYGON + MULTIPOLYGON: no abort, preserves rows", {
  p  <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  mp <- sf::st_multipolygon(list(list(rbind(
    c(2, 2), c(3, 2), c(3, 3), c(2, 3), c(2, 2)
  ))))
  x  <- sf::st_sf(
    id = c(1L, 2L),
    geometry = sf::st_sfc(p, mp, crs = 5070)
  )
  # Best-effort: do not abort heterogeneous batch. Downstream st_intersection
  # handles mixed POLYGON/MULTIPOLYGON natively. Harmonization attempted but
  # not guaranteed (sfc_GEOMETRY casting is sf-version dependent).
  out <- .repair_geometry(x)
  expect_equal(nrow(out), 2L)
  expect_true(all(sf::st_is_valid(out)))
})

test_that("T-UTIL-08 mixed batch: 1 valid + 1 invalid -> per-row degrade, no batch abort", {
  good <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  bad  <- sf::st_polygon(list(rbind(c(0, 0), c(2, 2), c(0, 2), c(2, 0), c(0, 0))))
  x <- sf::st_sf(
    id = c("good", "bad"),
    geometry = sf::st_sfc(good, bad, crs = 5070)
  )
  out <- suppressWarnings(.repair_geometry(x))
  expect_equal(nrow(out), 2L)
  expect_true(sf::st_is_valid(out)[1L])
})

test_that("T-UTIL-09 .safe_geom isolates per-site error + preserves call chain", {
  result_ok  <- .safe_geom(1 + 1, site_id = "S1", drive_time_min = 5L)
  expect_equal(result_ok, 2)

  result_err <- .safe_geom(stop("simulated"), site_id = "S9",
                           drive_time_min = 10L)
  expect_true(is.list(result_err))
  expect_true(result_err$.failure)
  expect_equal(result_err$site_id, "S9")
  expect_equal(result_err$drive_time_min, 10L)
  expect_match(result_err$reason, "simulated")
  expect_s3_class(result_err$cond, "condition")

  result_cacs_err <- .safe_geom(
    .cli_abort_schema("schema simulated"),
    site_id = "S10",
    drive_time_min = 15L
  )
  expect_true(result_cacs_err$.failure)
  expect_equal(result_cacs_err$site_id, "S10")
  expect_equal(result_cacs_err$drive_time_min, 15L)
  expect_match(result_cacs_err$reason, "schema simulated")
  expect_true("catchmentACS_error_schema" %in% result_cacs_err$class_chain)
})

test_that("T-UTIL-10 .safe_geom re-signals catchmentACS warnings to outer handler", {
  captured <- list()
  withCallingHandlers(
    .safe_geom(
      .cli_warn_runtime("inner warning"),
      site_id = "S1", drive_time_min = 5L
    ),
    catchmentACS_warning = function(w) {
      captured[[length(captured) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_length(captured, 1L)
  expect_s3_class(captured[[1L]], "catchmentACS_warning_runtime")
})

test_that("T-UTIL-10b .safe_geom re-signals catchmentACS messages to outer handler", {
  captured <- list()
  withCallingHandlers(
    .safe_geom(
      .cacs_emit("inform", "progress_summary", "inner message",
                 phase = "utils-test"),
      site_id = "S1", drive_time_min = 5L
    ),
    catchmentACS_message = function(m) {
      captured[[length(captured) + 1L]] <<- m
      invokeRestart("muffleMessage")
    }
  )
  expect_length(captured, 1L)
  expect_s3_class(captured[[1L]], "catchmentACS_message_progress_summary")
  expect_equal(captured[[1L]]$cacs_phase, "utils-test")
})

test_that("T-UTIL-11 .empty_site_result schema is bind_rows-compatible", {
  r1 <- .empty_site_result("S1", 5L, n_tracts = 0L,
                           failure_reason = "no_tract_intersection")
  r2 <- .empty_site_result("S2", 10L, n_tracts = 0L,
                           failure_reason = "all_slivers_below_min_weight")
  expect_no_error(out <- dplyr::bind_rows(r1, r2))
  expect_equal(nrow(out), 2L)
  expect_equal(out$failure_origin, c("intersection", "intersection"))
  expect_equal(out$n_tracts, c(0L, 0L))
  expect_true(all(is.na(out$estimate)))
})

test_that("T-UTIL-12 .empty_site_result has correct column types", {
  r <- .empty_site_result("S1", 5L)
  expect_type(r$site_id, "character")
  expect_type(r$drive_time_min, "integer")
  expect_type(r$estimate, "double")
  expect_type(r$n_tracts, "integer")
  expect_equal(r$moe_fallback_reason, "n/a")
  expect_equal(r$failure_origin, "intersection")
})

test_that("T-UTIL-13 pretty_ms / pretty_bytes / pretty_n", {
  expect_equal(pretty_ms(500),  "500 ms")
  expect_equal(pretty_ms(1500), "1.50 s")
  expect_equal(pretty_ms(NA),   "n/a")
  expect_equal(pretty_bytes(1024),      "1.0 KB")
  expect_equal(pretty_bytes(1024 * 1024), "1.0 MB")
  expect_equal(pretty_n(1234567), "1,234,567")
})

test_that("T-UTIL-14 pretty helpers cover boundary branches", {
  expect_equal(pretty_ms(60000), "1.0 min")
  expect_equal(pretty_ms(-1), "n/a")
  expect_equal(pretty_bytes(0), "0.0 B")
  expect_equal(pretty_bytes(-1), "n/a")
  expect_equal(pretty_bytes(1024^3), "1.0 GB")
  expect_equal(pretty_n(NA_real_), "n/a")
})
