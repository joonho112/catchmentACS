# The key under which cacs_intersect_weight() saves a result must change when
# the drive-time areas change, even when they carry the cache_key that
# cacs_isochrone() recorded for the whole result (rows selected with `[` or
# geometry replaced with st_geometry<- keep that attribute), and must not
# change with the way the row names of the areas are stored. Bundled data;
# no network.

.key_inputs <- function() {
  # Rows of an sf object are selected correctly only once sf is loaded.
  loadNamespace("sf")
  iso_path <- system.file("extdata", "legacy_2025_isochrones.rds", package = "catchmentACS")
  acs_path <- system.file("extdata", "sample_alabama_subset.rds", package = "catchmentACS")
  testthat::skip_if(!nzchar(iso_path) || !nzchar(acs_path), "bundled data not installed")
  iso <- readRDS(iso_path)
  # The attribute that cacs_isochrone() attaches to its result.
  attr(iso, "cacs_isochrone_provenance") <- list(
    cache_key = paste(rep("ab", 32L), collapse = ""),
    ring_topology = "cumulative"
  )
  list(iso = iso, acs = readRDS(acs_path))
}

.key_of <- function(iso, acs) {
  .cache_key_intersect(iso, acs, NULL, "area", 1e-6)
}

test_that("INT-KEY-01 two subsets of one isochrone result get different keys and their own rows", {
  x <- .key_inputs()
  iso07 <- x$iso[x$iso$site_id == "AL_SITE_07" & x$iso$drive_time_min == 10L, ]
  iso08 <- x$iso[x$iso$site_id == "AL_SITE_08" & x$iso$drive_time_min == 10L, ]
  expect_false(identical(.key_of(iso07, x$acs), .key_of(iso08, x$acs)))
  with_test_cache({
    r07 <- suppressMessages(cacs_intersect_weight(iso07, x$acs, verbose = FALSE))
    r08 <- suppressMessages(cacs_intersect_weight(iso08, x$acs, verbose = FALSE))
    expect_identical(unique(r07$site_id), "AL_SITE_07")
    expect_identical(unique(r08$site_id), "AL_SITE_08")
  })
})

test_that("INT-KEY-02 an area whose geometry was replaced gets a new key and a newly computed result", {
  x <- .key_inputs()
  iso07 <- x$iso[x$iso$site_id == "AL_SITE_07" & x$iso$drive_time_min == 10L, ]
  smaller <- iso07
  g <- sf::st_buffer(sf::st_transform(sf::st_geometry(smaller), 5070), -1500)
  sf::st_geometry(smaller) <- sf::st_transform(g, 4326)
  expect_false(identical(.key_of(iso07, x$acs), .key_of(smaller, x$acs)))
  fresh <- withr::with_options(
    list(catchmentACS.cache_enabled = FALSE),
    suppressMessages(cacs_intersect_weight(smaller, x$acs, verbose = FALSE))
  )
  with_test_cache({
    suppressMessages(cacs_intersect_weight(iso07, x$acs, verbose = FALSE))
    second <- suppressMessages(cacs_intersect_weight(smaller, x$acs, verbose = FALSE))
    expect_equal(second$estimate, fresh$estimate)
  })
})

test_that("INT-KEY-03 the same areas, also in another row order, still find the saved result", {
  x <- .key_inputs()
  iso2 <- x$iso[x$iso$site_id %in% c("AL_SITE_07", "AL_SITE_08") & x$iso$drive_time_min == 10L, ]
  reversed <- iso2[rev(seq_len(nrow(iso2))), ]
  expect_identical(.key_of(iso2, x$acs), .key_of(reversed, x$acs))
  with_test_cache({
    first <- suppressMessages(cacs_intersect_weight(iso2, x$acs, verbose = FALSE))
    hits_before <- cacs_get_cache_state()$hits[[1L]][["intersect"]]
    again <- suppressMessages(cacs_intersect_weight(reversed, x$acs, verbose = FALSE))
    expect_identical(cacs_get_cache_state()$hits[[1L]][["intersect"]], hits_before + 1L)
    expect_equal(again$estimate, first$estimate)
  })
})

test_that("INT-KEY-04 the key does not depend on how the row names are stored", {
  x <- .key_inputs()
  sub <- x$iso[x$iso$site_id %in% c("AL_SITE_07", "AL_SITE_08") & x$iso$drive_time_min == 10L, ]
  # A data frame keeps row names as they are; a subset of a tibble made
  # before tibble is loaded keeps the original row numbers in the same way.
  plain <- sf::st_as_sf(as.data.frame(sub))
  attr(plain, "cacs_isochrone_provenance") <- attr(sub, "cacs_isochrone_provenance")
  attr(plain, "row.names") <- seq_len(nrow(plain))
  kept <- plain
  attr(kept, "row.names") <- c(20L, 23L)
  expect_identical(.key_of(plain, x$acs), .key_of(kept, x$acs))
  no_key <- plain
  attr(no_key, "cacs_isochrone_provenance") <- NULL
  no_key_kept <- no_key
  attr(no_key_kept, "row.names") <- c(20L, 23L)
  expect_identical(.key_of(no_key, x$acs), .key_of(no_key_kept, x$acs))
  # The same for ACS data without a recorded key.
  acs_sub <- sf::st_as_sf(as.data.frame(x$acs[x$acs$variable == "B01003_001", ][1:4, ]))
  attr(acs_sub, "cacs_provenance") <- NULL
  attr(acs_sub, "cacs_acs_provenance") <- NULL
  acs_kept <- acs_sub
  attr(acs_kept, "row.names") <- c(11L, 12L, 13L, 14L)
  expect_identical(.key_of(plain, acs_sub), .key_of(plain, acs_kept))
})

test_that("INT-KEY-05 cacs_run() with a subset of precomputed areas returns the rows of that subset", {
  x <- .key_inputs()
  iso07 <- x$iso[x$iso$site_id == "AL_SITE_07" & x$iso$drive_time_min == 10L, ]
  iso08 <- x$iso[x$iso$site_id == "AL_SITE_08" & x$iso$drive_time_min == 10L, ]
  sites <- catchmentACS::cacs_alabama_sites
  run_sub <- function(iso) {
    suppressWarnings(suppressMessages(cacs_run(
      sites[sites$site_id %in% unique(iso$site_id), ], state = "AL",
      drive_times = 10L, precomputed_isochrones = iso, acs = x$acs,
      verbose = FALSE
    )))
  }
  with_test_cache({
    run_sub(iso07)
    out08 <- run_sub(iso08)
    expect_identical(unique(out08$site_id), "AL_SITE_08")
  })
})
