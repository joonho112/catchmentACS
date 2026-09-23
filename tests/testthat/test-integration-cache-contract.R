cache_contract_key <- function(namespace, offset = 0L) {
  payload <- cache_payload(namespace)
  payload[[1L]] <- payload[[1L]] + offset
  .cacs_cache_key(payload, namespace)
}

test_that("CACHE-CONTRACT-01 all cache namespaces write paired sidecars", {
  with_test_cache({
    keys <- c(
      isochrone = cache_contract_key("isochrone"),
      acs = cache_contract_key("acs"),
      intersect = cache_contract_key("intersect")
    )
    expect_true(.cacs_cache_put(list(ns = "isochrone"), keys[["isochrone"]], "isochrone"))
    expect_true(.cacs_cache_put(cache_acs_tbl(5), keys[["acs"]], "acs"))
    expect_true(.cacs_cache_put(tibble::tibble(ns = "intersect"), keys[["intersect"]], "intersect"))

    st <- cacs_cache_status()
    expect_true(all(st$n_entries[match(names(keys), st$namespace)] == 1L))
    expect_true(all(st$n_fingerprints[match(names(keys), st$namespace)] == 1L))
  })
})

test_that("CACHE-CONTRACT-02 production and test ACS namespaces can coexist", {
  td <- tempfile("cache_contract_")
  key <- cache_contract_key("acs")
  with_test_cache(mode = "test", cache_dir = td, cleanup = FALSE, {
    .cacs_cache_put(list(value = "test"), key, "acs")
  })
  with_test_cache(mode = "production", cache_dir = td, cleanup = FALSE, {
    .cacs_cache_put(list(value = "production"), key, "acs")
    expect_identical(
      suppressMessages(.cacs_cache_get(key, "acs")),
      list(value = "production")
    )
  })
  with_test_cache(mode = "test", cache_dir = td, {
    expect_identical(
      suppressMessages(.cacs_cache_get(key, "acs")),
      list(value = "test")
    )
  })
})

test_that("CACHE-CONTRACT-03 clear all removes sidecars across physical namespaces", {
  with_test_cache(mode = "test", {
    .cacs_cache_put(list(x = 1), cache_contract_key("isochrone"), "isochrone")
    .cacs_cache_put(list(x = 2), cache_contract_key("acs"), "acs")
    .cacs_cache_put(list(x = 3), cache_contract_key("intersect"), "intersect")
    suppressMessages(out <- cacs_clear_cache("all", confirm = FALSE))
    expect_gte(sum(out$n_removed), 6L)
    expect_equal(sum(cacs_cache_status()$n_entries), 0L)
    expect_equal(sum(cacs_cache_status()$n_fingerprints), 0L)
  })
})

test_that("CACHE-CONTRACT-04 disabled cache preserves existing entries for re-enable", {
  with_test_cache({
    key <- cache_contract_key("isochrone")
    .cacs_cache_put(list(x = 1), key, "isochrone")
    cacs_set_cache(FALSE)
    expect_null(.cacs_cache_get(key, "isochrone"))
    cacs_set_cache(TRUE)
    expect_identical(suppressMessages(.cacs_cache_get(key, "isochrone")), list(x = 1))
  })
})

test_that("CACHE-CONTRACT-05 legacy invalidation in one namespace does not clear others", {
  with_test_cache({
    iso_key <- cache_contract_key("isochrone")
    acs_key <- cache_contract_key("acs")
    .cacs_cache_put(list(x = "iso"), iso_key, "isochrone")
    cache_write_legacy_rds("acs", acs_key, data.frame(x = 1))

    expect_message(
      got <- .cacs_cache_get(acs_key, "acs"),
      class = "catchmentACS_message_cache_legacy_invalidated"
    )
    expect_null(got)
    expect_identical(suppressMessages(.cacs_cache_get(iso_key, "isochrone")),
                     list(x = "iso"))
  })
})

test_that("CACHE-CONTRACT-06 cache state records cross-namespace integration hits", {
  with_test_cache(mode = "test", {
    iso_key <- cache_contract_key("isochrone")
    acs_key <- cache_contract_key("acs")
    int_key <- cache_contract_key("intersect")
    .cacs_cache_put(list(x = "iso"), iso_key, "isochrone")
    .cacs_cache_put(cache_acs_tbl(5), acs_key, "acs")
    .cacs_cache_put(tibble::tibble(x = "int"), int_key, "intersect")

    suppressMessages(.cacs_cache_get(iso_key, "isochrone"))
    suppressWarnings(suppressMessages(.cacs_cache_get(acs_key, "acs")))
    suppressMessages(.cacs_cache_get(int_key, "intersect"))

    hits <- cacs_get_cache_state()$hits[[1]]
    expect_equal(hits[["isochrone"]], 1L)
    expect_equal(hits[["acs_test"]], 1L)
    expect_equal(hits[["intersect"]], 1L)
  })
})
