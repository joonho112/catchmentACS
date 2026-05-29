# ===========================================================================
# test-unit-pipe-iso-listcol.R
#
# v0.4 plan Step 2.2 — unit tests for `.cacs_pipe_iso_to_list_column()`
# (Layer 1 helper for Issue 003).
# ===========================================================================

# --- Fixture builders ------------------------------------------------------

.mk_out_skeleton <- function(site_ids, drive_times) {
  pairs <- expand.grid(site_id = site_ids,
                       drive_time_min = drive_times,
                       stringsAsFactors = FALSE)
  pairs <- pairs[order(pairs$site_id, pairs$drive_time_min), ]
  tibble::tibble(
    site_id        = as.character(pairs$site_id),
    drive_time_min = as.integer(pairs$drive_time_min),
    isochrone      = vector("list", nrow(pairs))
  )
}

.mk_iso_sf <- function(site_ids, drive_times, extra_cols = TRUE) {
  pairs <- expand.grid(site_id = site_ids,
                       drive_time_min = drive_times,
                       stringsAsFactors = FALSE)
  geoms <- lapply(seq_len(nrow(pairs)), function(i) {
    sf::st_polygon(list(rbind(
      c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
    )))
  })
  out <- sf::st_sf(
    site_id        = as.character(pairs$site_id),
    drive_time_min = as.integer(pairs$drive_time_min),
    geometry       = sf::st_sfc(geoms, crs = 4326)
  )
  if (extra_cols) {
    out$ring_topology <- "cumulative"
    out$provider <- "osrm"
  }
  out
}

# --- Tests -----------------------------------------------------------------

test_that("STEP22-01 happy path: 3-site iso sf filled into 3 rows", {
  out <- .mk_out_skeleton(c("S1", "S2", "S3"), 10L)
  iso <- .mk_iso_sf(c("S1", "S2", "S3"), 10L)
  filled <- catchmentACS:::.cacs_pipe_iso_to_list_column(out, iso)
  expect_identical(nrow(filled), 3L)
  expect_true(all(vapply(filled$isochrone,
                          function(x) inherits(x, "sf") && nrow(x) == 1L,
                          logical(1))))
  expect_true(isTRUE(attr(filled, "iso_was_filled")))
})

test_that("STEP22-02 defensive NULL path: iso_sf = NULL leaves $isochrone unchanged", {
  out <- .mk_out_skeleton("S1", 10L)
  filled <- catchmentACS:::.cacs_pipe_iso_to_list_column(out, iso_sf = NULL)
  expect_identical(filled, out)
  expect_null(attr(filled, "iso_was_filled"))
})

test_that("STEP22-03 schema mismatch: iso_sf with extra cols still joins", {
  out <- .mk_out_skeleton(c("S1", "S2"), 10L)
  iso <- .mk_iso_sf(c("S1", "S2"), 10L, extra_cols = TRUE)
  iso$extra_metadata <- c("alpha", "beta")
  filled <- catchmentACS:::.cacs_pipe_iso_to_list_column(out, iso)
  expect_identical(nrow(filled), 2L)
  # Filled cell carries all original columns including extras
  expect_true("extra_metadata" %in% colnames(filled$isochrone[[1L]]))
  expect_true(isTRUE(attr(filled, "iso_was_filled")))
})

test_that("STEP22-04 partial match: 2-of-3 sites in iso → 2 filled + 1 NULL", {
  out <- .mk_out_skeleton(c("S1", "S2", "S3"), 10L)
  iso <- .mk_iso_sf(c("S1", "S3"), 10L)  # S2 missing
  filled <- catchmentACS:::.cacs_pipe_iso_to_list_column(out, iso)
  expect_identical(nrow(filled), 3L)
  s1_idx <- which(filled$site_id == "S1")
  s2_idx <- which(filled$site_id == "S2")
  s3_idx <- which(filled$site_id == "S3")
  expect_true(inherits(filled$isochrone[[s1_idx]], "sf"))
  expect_null(filled$isochrone[[s2_idx]])
  expect_true(inherits(filled$isochrone[[s3_idx]], "sf"))
  expect_true(isTRUE(attr(filled, "iso_was_filled")))
})

test_that("STEP22-05 multi-band: 2 drive times keyed correctly", {
  out <- .mk_out_skeleton("S1", c(10L, 20L))
  iso <- .mk_iso_sf("S1", c(10L, 20L))
  filled <- catchmentACS:::.cacs_pipe_iso_to_list_column(out, iso)
  expect_identical(nrow(filled), 2L)
  expect_true(all(vapply(filled$isochrone,
                          function(x) inherits(x, "sf") && nrow(x) == 1L,
                          logical(1))))
  # Each cell carries its own drive_time_min row
  cell_dtm <- vapply(filled$isochrone,
                     function(x) x$drive_time_min[[1L]], integer(1))
  expect_identical(cell_dtm, c(10L, 20L))
})

test_that("STEP22-06 each filled cell is a 1-row sf (not multi-row)", {
  out <- .mk_out_skeleton(c("S1", "S2"), 10L)
  iso <- .mk_iso_sf(c("S1", "S2"), 10L)
  filled <- catchmentACS:::.cacs_pipe_iso_to_list_column(out, iso)
  for (cell in filled$isochrone) {
    expect_identical(nrow(cell), 1L)
  }
})

test_that("STEP22-07 attr(iso_was_filled) only set when fill happens", {
  out <- .mk_out_skeleton("S1", 10L)
  # iso_sf with no matching key → no fill
  iso_no_match <- .mk_iso_sf("OTHER", 10L)
  filled <- catchmentACS:::.cacs_pipe_iso_to_list_column(out, iso_no_match)
  expect_null(attr(filled, "iso_was_filled"))
  expect_null(filled$isochrone[[1L]])
})

test_that("STEP22-08 CRS preservation: EPSG:4326 retained in filled cell", {
  out <- .mk_out_skeleton("S1", 10L)
  iso <- .mk_iso_sf("S1", 10L)
  filled <- catchmentACS:::.cacs_pipe_iso_to_list_column(out, iso)
  expect_identical(sf::st_crs(filled$isochrone[[1L]])$epsg, 4326L)
})

test_that("STEP22-09 robust to defensive bad inputs (early return)", {
  out <- .mk_out_skeleton("S1", 10L)
  # Not an sf
  bad_input <- tibble::tibble(site_id = "S1", drive_time_min = 10L)
  filled <- catchmentACS:::.cacs_pipe_iso_to_list_column(out, bad_input)
  expect_identical(filled, out)
  # Missing required columns
  empty_sf <- sf::st_sf(other = "x",
                       geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326))
  filled2 <- catchmentACS:::.cacs_pipe_iso_to_list_column(out, empty_sf)
  expect_identical(filled2, out)
})
