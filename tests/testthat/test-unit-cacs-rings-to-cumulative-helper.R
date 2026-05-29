# ============================================================================
# Unit tests for cacs_rings_to_cumulative().
# ============================================================================


.mk_ring_poly <- function(xmin, xmax, ymin, ymax) {
  sf::st_polygon(list(rbind(
    c(xmin, ymin), c(xmax, ymin), c(xmax, ymax),
    c(xmin, ymax), c(xmin, ymin)
  )))
}


.mk_annulus_rings <- function(site_ids = c("A", "B")) {
  bands <- tibble::tibble(
    isomin = c(0L, 5L, 10L),
    isomax = c(5L, 10L, 15L),
    xmin = c(0, 1, 2),
    xmax = c(1, 2, 3)
  )

  rows <- list()
  geoms <- list()
  for (s in seq_along(site_ids)) {
    y0 <- (s - 1L) * 10
    for (i in seq_len(nrow(bands))) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        site_id = site_ids[[s]],
        isomin = bands$isomin[[i]],
        isomax = bands$isomax[[i]],
        provider = "osrm",
        profile = "car",
        ring_topology = "annulus"
      )
      geoms[[length(geoms) + 1L]] <- .mk_ring_poly(
        bands$xmin[[i]], bands$xmax[[i]], y0, y0 + 1
      )
    }
  }

  sf::st_sf(
    dplyr::bind_rows(rows),
    geometry = sf::st_sfc(geoms, crs = 4326)
  )
}


test_that("T-UF1-RINGS-01 converts annulus bands to cumulative rows", {
  out <- cacs_rings_to_cumulative(.mk_annulus_rings("A"))

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 3L)
  expect_equal(out$site_id, rep("A", 3L))
  expect_equal(out$drive_time_min, c(5L, 10L, 15L))
  expect_equal(out$isomin, c(0L, 0L, 0L))
  expect_equal(out$isomax, c(5L, 10L, 15L))
  expect_equal(out$ring_topology, rep("cumulative", 3L))
  expect_equal(as.integer(sf::st_crs(out)$epsg), 4326L)
})


test_that("T-UF1-RINGS-02 cumulative geometry grows by unioning prior bands", {
  out <- cacs_rings_to_cumulative(.mk_annulus_rings("A"))
  bboxes <- lapply(seq_len(nrow(out)), function(i) sf::st_bbox(out[i, ]))

  expect_equal(unname(bboxes[[1]][c("xmin", "xmax")]), c(0, 1))
  expect_equal(unname(bboxes[[2]][c("xmin", "xmax")]), c(0, 2))
  expect_equal(unname(bboxes[[3]][c("xmin", "xmax")]), c(0, 3))
  expect_true(all(diff(as.numeric(sf::st_area(out))) > 0))
})


test_that("T-UF1-RINGS-03 drive_times NULL returns all sorted isomax values", {
  out <- cacs_rings_to_cumulative(.mk_annulus_rings("A"), drive_times = NULL)
  expect_equal(out$drive_time_min, c(5L, 10L, 15L))
})


test_that("T-UF1-RINGS-04 explicit drive_times are filtered and sorted", {
  out <- cacs_rings_to_cumulative(.mk_annulus_rings("A"),
                                  drive_times = c(15, 5, 5))
  expect_equal(out$drive_time_min, c(5L, 15L))
  expect_equal(out$isomax, c(5L, 15L))
})


test_that("T-UF1-RINGS-05 requested missing drive_time fails loud", {
  expect_error(
    cacs_rings_to_cumulative(.mk_annulus_rings("A"), drive_times = c(5, 20)),
    class = "catchmentACS_error_schema"
  )
})


test_that("T-UF1-RINGS-06 multi-site conversion does not cross site boundaries", {
  out <- cacs_rings_to_cumulative(.mk_annulus_rings(c("A", "B")),
                                  drive_times = c(10, 15))

  expect_equal(nrow(out), 4L)
  expect_equal(out$site_id, c("A", "A", "B", "B"))
  a_bbox <- sf::st_bbox(out[out$site_id == "A" & out$drive_time_min == 15L, ])
  b_bbox <- sf::st_bbox(out[out$site_id == "B" & out$drive_time_min == 15L, ])
  expect_equal(unname(a_bbox[c("ymin", "ymax")]), c(0, 1))
  expect_equal(unname(b_bbox[c("ymin", "ymax")]), c(10, 11))
})


test_that("T-UF1-RINGS-07 conversion is idempotent for its own output", {
  out1 <- cacs_rings_to_cumulative(.mk_annulus_rings("A"))
  out2 <- cacs_rings_to_cumulative(out1)

  expect_equal(out2$drive_time_min, out1$drive_time_min)
  expect_equal(out2$ring_topology, out1$ring_topology)
  expect_equal(as.numeric(sf::st_area(out2)),
               as.numeric(sf::st_area(out1)),
               tolerance = 1e-8)
  expect_equal(
    lapply(seq_len(nrow(out2)), function(i) sf::st_bbox(out2[i, ])),
    lapply(seq_len(nrow(out1)), function(i) sf::st_bbox(out1[i, ]))
  )
})


test_that("T-UF1-RINGS-08 invalid object and missing required columns abort", {
  expect_error(cacs_rings_to_cumulative(tibble::tibble()),
               class = "catchmentACS_error_schema")

  x <- .mk_annulus_rings("A")
  x$isomin <- NULL
  expect_error(cacs_rings_to_cumulative(x),
               class = "catchmentACS_error_schema")
})


test_that("T-UF1-RINGS-09 malformed ring bounds abort", {
  x_na <- .mk_annulus_rings("A")
  x_na$isomax[[1]] <- NA_integer_
  expect_error(cacs_rings_to_cumulative(x_na),
               class = "catchmentACS_error_schema")

  x_neg <- .mk_annulus_rings("A")
  x_neg$isomin[[1]] <- -1L
  expect_error(cacs_rings_to_cumulative(x_neg),
               class = "catchmentACS_error_schema")

  x_frac <- .mk_annulus_rings("A")
  x_frac$isomax <- as.numeric(x_frac$isomax)
  x_frac$isomax[[1]] <- 5.5
  expect_error(cacs_rings_to_cumulative(x_frac),
               class = "catchmentACS_error_schema")
})


test_that("T-UF1-RINGS-10 unsupported topology values abort", {
  x <- .mk_annulus_rings("A")
  x$ring_topology[[1]] <- "weird"
  expect_error(cacs_rings_to_cumulative(x),
               class = "catchmentACS_error_schema")

  x_na <- .mk_annulus_rings("A")
  x_na$ring_topology[[1]] <- NA_character_
  expect_error(cacs_rings_to_cumulative(x_na),
               class = "catchmentACS_error_schema")
})


test_that("T-UF1-RINGS-11 CRS and geometry-type guards fail loud", {
  x_crs <- sf::st_transform(.mk_annulus_rings("A"), 5070)
  expect_error(cacs_rings_to_cumulative(x_crs),
               class = "catchmentACS_error_schema")

  x_pt <- .mk_annulus_rings("A")
  sf::st_geometry(x_pt) <- sf::st_sfc(
    rep(list(sf::st_point(c(-86.8, 33.5))), nrow(x_pt)),
    crs = 4326
  )
  expect_error(cacs_rings_to_cumulative(x_pt),
               class = "catchmentACS_error_schema")
})


test_that("T-UF1-RINGS-12 incomplete annulus chains abort", {
  x <- .mk_annulus_rings("A")
  x <- x[x$isomin != 5L, ]
  expect_error(cacs_rings_to_cumulative(x, drive_times = 15),
               class = "catchmentACS_error_schema")
})
