test_that("T-CACHE-OPT-01 cacs_set_cache(FALSE) disables writes", {
  with_test_cache({
    cacs_set_cache(FALSE)
    key <- cache_key_for("isochrone")
    expect_false(.cacs_cache_put(list(x = 1), key, "isochrone"))
  })
})

test_that("T-CACHE-OPT-02 cacs_set_cache(TRUE) re-enables writes", {
  with_test_cache({
    cacs_set_cache(FALSE)
    cacs_set_cache(TRUE)
    key <- cache_key_for("isochrone")
    expect_true(.cacs_cache_put(list(x = 1), key, "isochrone"))
  })
})

test_that("T-CACHE-OPT-03 cacs_get_cache_state returns stable schema", {
  with_test_cache({
    state <- cacs_get_cache_state()
    expect_s3_class(state, "tbl_df")
    expect_true(all(c("enabled", "cache_dir", "namespace_mode",
                      "fingerprint_algorithm", "hits", "misses",
                      "status", "session_started_at") %in% names(state)))
  })
})

test_that("T-CACHE-OPT-04 CACS_NO_CACHE disables cache", {
  with_test_cache({
    withr::local_envvar(c(CACS_NO_CACHE = "1"))
    key <- cache_key_for("isochrone")
    expect_false(.cacs_cache_put(list(x = 1), key, "isochrone"))
  })
})

test_that("T-CACHE-OPT-05 CACS_CACHE_ENABLED=false disables cache", {
  with_test_cache({
    withr::local_options(catchmentACS.cache_enabled = NULL)
    withr::local_envvar(c(CACS_CACHE_ENABLED = "false"))
    key <- cache_key_for("isochrone")
    expect_false(.cacs_cache_put(list(x = 1), key, "isochrone"))
  })
})

test_that("T-CACHE-OPT-06 CACS_CACHE_ENABLED=true enables cache", {
  with_test_cache({
    withr::local_options(catchmentACS.cache_enabled = NULL)
    withr::local_envvar(c(CACS_CACHE_ENABLED = "true"))
    key <- cache_key_for("isochrone")
    expect_true(.cacs_cache_put(list(x = 1), key, "isochrone"))
  })
})

test_that("T-CACHE-OPT-07 namespace option disables intersect", {
  with_test_cache({
    withr::local_options(catchmentACS.cache_intersect = FALSE)
    key <- cache_key_for("intersect")
    expect_false(.cacs_cache_put(list(x = 1), key, "intersect"))
  })
})

test_that("T-CACHE-OPT-08 namespace env disables intersect", {
  with_test_cache({
    withr::local_envvar(c(CACS_CACHE_INTERSECT = "0"))
    key <- cache_key_for("intersect")
    expect_false(.cacs_cache_put(list(x = 1), key, "intersect"))
  })
})

test_that("T-CACHE-OPT-09 global scope writes sentinel block", {
  with_test_cache({
    rp <- tempfile("Rprofile_")
    withr::local_options(catchmentACS.rprofile_path = rp)
    expect_true(cacs_set_cache(FALSE, scope = "global", confirm = FALSE))
    txt <- readLines(rp, warn = FALSE)
    expect_true(any(grepl("catchmentACS.cache_enabled = FALSE", txt, fixed = TRUE)))
  })
})

test_that("T-CACHE-OPT-10 global scope is idempotent", {
  with_test_cache({
    rp <- tempfile("Rprofile_")
    withr::local_options(catchmentACS.rprofile_path = rp)
    cacs_set_cache(FALSE, scope = "global", confirm = FALSE)
    cacs_set_cache(FALSE, scope = "global", confirm = FALSE)
    txt <- readLines(rp, warn = FALSE)
    expect_equal(sum(txt == "# catchmentACS v0.3 cache control"), 1L)
  })
})

test_that("T-CACHE-OPT-11 global scope updates existing block", {
  with_test_cache({
    rp <- tempfile("Rprofile_")
    withr::local_options(catchmentACS.rprofile_path = rp)
    cacs_set_cache(FALSE, scope = "global", confirm = FALSE)
    cacs_set_cache(TRUE, scope = "global", confirm = FALSE)
    txt <- readLines(rp, warn = FALSE)
    expect_true(any(grepl("catchmentACS.cache_enabled = TRUE", txt, fixed = TRUE)))
    expect_false(any(grepl("catchmentACS.cache_enabled = FALSE", txt, fixed = TRUE)))
  })
})

test_that("T-CACHE-OPT-12 invalid enabled errors as schema", {
  expect_error(cacs_set_cache(NA), class = "catchmentACS_error_schema")
})

test_that("T-CACHE-OPT-13 invalid scope errors", {
  expect_error(cacs_set_cache(TRUE, scope = "forever"))
})

test_that("T-CACHE-OPT-14 session_started_at is POSIXct", {
  with_test_cache({
    expect_s3_class(cacs_get_cache_state()$session_started_at, "POSIXct")
  })
})

test_that("T-CACHE-OPT-15 state status list contains cache_status tibble", {
  with_test_cache({
    state <- cacs_get_cache_state()
    expect_s3_class(state$status[[1]], "tbl_df")
    expect_true("acs_test" %in% state$status[[1]]$namespace)
  })
})

test_that("T-CACHE-OPT-16 reset state zeros counters", {
  with_test_cache({
    key <- cache_key_for("isochrone")
    expect_null(.cacs_cache_get(key, "isochrone"))
    .cacs_cache_reset_state()
    expect_equal(cacs_get_cache_state()$misses[[1]][["isochrone"]], 0L)
  })
})

test_that("T-CACHE-OPT-17 namespace option disables ACS only", {
  with_test_cache({
    withr::local_options(catchmentACS.cache_acs = FALSE)
    acs_key <- cache_key_for("acs")
    iso_key <- cache_key_for("isochrone")
    expect_false(.cacs_cache_put(list(x = "acs"), acs_key, "acs"))
    expect_true(.cacs_cache_put(list(x = "iso"), iso_key, "isochrone"))
  })
})

test_that("T-CACHE-OPT-18 namespace env disables ACS only", {
  with_test_cache({
    withr::local_envvar(c(CACS_CACHE_ACS = "0"))
    acs_key <- cache_key_for("acs")
    int_key <- cache_key_for("intersect")
    expect_false(.cacs_cache_put(list(x = "acs"), acs_key, "acs"))
    expect_true(.cacs_cache_put(list(x = "intersect"), int_key, "intersect"))
  })
})

test_that("T-CACHE-OPT-19 cache state counters track hits and misses by namespace", {
  with_test_cache({
    iso_key <- cache_key_for("isochrone")
    acs_key <- cache_key_for("acs")
    int_key <- cache_key_for("intersect")

    expect_null(.cacs_cache_get(iso_key, "isochrone"))
    .cacs_cache_put(list(x = "iso"), iso_key, "isochrone")
    .cacs_cache_put(list(x = "acs"), acs_key, "acs")
    .cacs_cache_put(list(x = "intersect"), int_key, "intersect")
    suppressMessages(.cacs_cache_get(iso_key, "isochrone"))
    suppressMessages(.cacs_cache_get(acs_key, "acs"))
    suppressMessages(.cacs_cache_get(int_key, "intersect"))

    state <- cacs_get_cache_state()
    expect_equal(state$misses[[1]][["isochrone"]], 1L)
    expect_equal(state$hits[[1]][["isochrone"]], 1L)
    expect_equal(state$hits[[1]][["acs"]], 1L)
    expect_equal(state$hits[[1]][["intersect"]], 1L)
  })
})

test_that("T-CACHE-OPT-20 ACS counters land in acs_test under test namespace", {
  with_test_cache(mode = "test", {
    key <- cache_key_for("acs")
    .cacs_cache_put(list(x = "acs-test"), key, "acs")
    suppressMessages(.cacs_cache_get(key, "acs"))
    state <- cacs_get_cache_state()
    expect_equal(state$hits[[1]][["acs_test"]], 1L)
    expect_equal(state$hits[[1]][["acs"]], 0L)
  })
})

test_that("T-CACHE-OPT-21 cache enabled announcement is opt-in and once per session", {
  with_test_cache(announce = TRUE, {
    key1 <- cache_key_for("isochrone")
    payload2 <- cache_payload("isochrone")
    payload2$k1 <- 99L
    key2 <- .cacs_cache_key(payload2, "isochrone")
    expect_message(.cacs_cache_put(list(x = 1), key1, "isochrone"),
                   class = "catchmentACS_message_cache_enabled_announce")
    expect_silent(.cacs_cache_put(list(x = 2), key2, "isochrone"))
  })
})
