.cache_poly_sf <- function(n = 2L) {
  geom <- sf::st_sfc(lapply(seq_len(n), function(i) {
    sf::st_polygon(list(rbind(
      c(i, i), c(i + 0.1, i), c(i + 0.1, i + 0.1),
      c(i, i + 0.1), c(i, i)
    )))
  }), crs = 4269)
  sf::st_sf(geometry = geom)
}

.cache_acs_sf <- function(est_shift = 0) {
  x <- helper_mock_acs_long_sf(
    variables = c("B17001_001", "B17001_002"),
    n_tract = 3L
  )
  x$estimate <- x$estimate + est_shift
  attr(x, "cacs_provenance") <- list(
    cache_key = "same-upstream-key",
    state = "AL",
    year = 2023L,
    survey = "acs5",
    geography = "tract",
    variables = c("B17001_001", "B17001_002")
  )
  x
}

.cache_iso_sf <- function(res = 70L, topology = "cumulative") {
  x <- .cache_poly_sf(2L)
  x$site_id <- c("A", "A")
  x$drive_time_min <- c(5L, 10L)
  x$ring_topology <- topology
  attr(x, "cacs_isochrone_provenance") <- list(
    cache_key = "iso-upstream-key",
    res_param = res,
    ring_topology = topology
  )
  x
}

test_that("T-P3-CACHE-KEY-01 ACS content hash is stable across row order", {
  acs <- .cache_acs_sf()
  shuffled <- acs[rev(seq_len(nrow(acs))), ]
  expect_identical(.extract_acs_content_hash(acs),
                   .extract_acs_content_hash(shuffled))
})

test_that("T-P3-CACHE-KEY-02 ACS content hash changes when estimates change", {
  expect_false(identical(
    .extract_acs_content_hash(.cache_acs_sf(est_shift = 0)),
    .extract_acs_content_hash(.cache_acs_sf(est_shift = 1))
  ))
})

test_that("T-P3-CACHE-KEY-03 intersect key changes despite same ACS provenance key", {
  iso <- .cache_iso_sf()
  key1 <- .cache_key_intersect(iso, .cache_acs_sf(0), NULL, "area", 0.001)
  key2 <- .cache_key_intersect(iso, .cache_acs_sf(1), NULL, "area", 0.001)
  expect_false(identical(key1, key2))
})

test_that("T-P3-CACHE-KEY-04 intersect key changes with iso res_param", {
  acs <- .cache_acs_sf()
  key1 <- .cache_key_intersect(.cache_iso_sf(res = 50L), acs, NULL, "area", 0.001)
  key2 <- .cache_key_intersect(.cache_iso_sf(res = 70L), acs, NULL, "area", 0.001)
  expect_false(identical(key1, key2))
})

test_that("T-P3-CACHE-KEY-05 intersect key changes with ring topology", {
  acs <- .cache_acs_sf()
  key1 <- .cache_key_intersect(.cache_iso_sf(topology = "cumulative"), acs, NULL, "area", 0.001)
  key2 <- .cache_key_intersect(.cache_iso_sf(topology = "annulus"), acs, NULL, "area", 0.001)
  expect_false(identical(key1, key2))
})

test_that("T-P3-CACHE-KEY-06 namespace dimension locks reflect v0.3 contract", {
  expect_error(.cacs_cache_key(cache_payload(11), "acs"),
               class = "catchmentACS_error_schema")
  expect_match(.cacs_cache_key(cache_payload(12), "acs"), "^[0-9a-f]{64}$")
  expect_error(.cacs_cache_key(cache_payload(13), "acs"),
               class = "catchmentACS_error_schema")
  expect_error(.cacs_cache_key(cache_payload(15), "isochrone"),
               class = "catchmentACS_error_schema")
  expect_match(.cacs_cache_key(cache_payload(16), "isochrone"), "^[0-9a-f]{64}$")
  expect_error(.cacs_cache_key(cache_payload(13), "intersect"),
               class = "catchmentACS_error_schema")
  expect_match(.cacs_cache_key(cache_payload(14), "intersect"), "^[0-9a-f]{64}$")
})

test_that("T-P3-CACHE-KEY-07 ACS content hash changes when geometry changes", {
  acs1 <- .cache_acs_sf()
  acs2 <- acs1
  sf::st_geometry(acs2) <- sf::st_buffer(sf::st_geometry(acs2), dist = 0.001)
  expect_false(identical(.extract_acs_content_hash(acs1),
                         .extract_acs_content_hash(acs2)))
})

test_that("T-P3-CACHE-KEY-08 ACS content hash changes when CRS metadata changes", {
  acs1 <- .cache_acs_sf()
  acs2 <- suppressWarnings(sf::st_set_crs(acs1, 3857))
  expect_false(identical(.extract_acs_content_hash(acs1),
                         .extract_acs_content_hash(acs2)))
})
