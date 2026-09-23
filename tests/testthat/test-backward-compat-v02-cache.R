test_that("V02-CACHE-01 legacy ACS RDS without sidecar is a graceful miss", {
  fx <- load_replay_fixture("cache_poison")
  key <- bug010_acs_key(variables = bug010_vars(fx))
  with_test_cache({
    cache_write_legacy_rds("acs", key, fx$acs_sf)
    expect_message(
      got <- .cacs_cache_get(key, "acs"),
      class = "catchmentACS_message_cache_legacy_invalidated"
    )
    expect_null(got)
  })
})

test_that("V02-CACHE-02 legacy isochrone RDS without sidecar is a graceful miss", {
  key <- cache_key_for("isochrone")
  with_test_cache({
    cache_write_legacy_rds("isochrone", key, list(old = TRUE))
    expect_message(
      got <- .cacs_cache_get(key, "isochrone"),
      class = "catchmentACS_message_cache_legacy_invalidated"
    )
    expect_null(got)
  })
})

test_that("V02-CACHE-03 legacy intersect RDS without sidecar is a graceful miss", {
  key <- cache_key_for("intersect")
  with_test_cache({
    cache_write_legacy_rds("intersect", key, tibble::tibble(old = TRUE))
    expect_message(
      got <- .cacs_cache_get(key, "intersect"),
      class = "catchmentACS_message_cache_legacy_invalidated"
    )
    expect_null(got)
  })
})

test_that("V02-CACHE-04 legacy invalidation removes the old file", {
  key <- cache_key_for("acs")
  with_test_cache({
    cache_write_legacy_rds("acs", key, data.frame(x = 1))
    suppressMessages(.cacs_cache_get(key, "acs"))
    expect_false(file.exists(file.path(cacs_cache_dir(), "acs", paste0(key, ".rds"))))
  })
})

test_that("V02-CACHE-05 legacy invalidation removes old isochrone and intersect files", {
  with_test_cache({
    iso_key <- cache_key_for("isochrone")
    int_key <- cache_key_for("intersect")
    cache_write_legacy_rds("isochrone", iso_key, list(old = TRUE))
    cache_write_legacy_rds("intersect", int_key, tibble::tibble(old = TRUE))

    suppressMessages(.cacs_cache_get(iso_key, "isochrone"))
    suppressMessages(.cacs_cache_get(int_key, "intersect"))

    expect_false(file.exists(file.path(cacs_cache_dir(), "isochrone", paste0(iso_key, ".rds"))))
    expect_false(file.exists(file.path(cacs_cache_dir(), "intersect", paste0(int_key, ".rds"))))
  })
})

test_that("V02-CACHE-06 legacy ACS miss can be followed by fresh v0.3 write", {
  fx <- load_replay_fixture("cache_poison")
  key <- bug010_acs_key(variables = bug010_vars(fx))
  with_test_cache({
    cache_write_legacy_rds("acs", key, fx$acs_sf)
    suppressMessages(expect_null(.cacs_cache_get(key, "acs")))
    fresh <- helper_mock_acs_long_sf(variables = bug010_vars(fx), n_tract = 4L)
    expect_true(.cacs_cache_put(fresh, key, "acs"))
    got <- suppressWarnings(suppressMessages(.cacs_cache_get(key, "acs")))
    expect_equal(nrow(got), length(bug010_vars(fx)) * 4L)
  })
})
