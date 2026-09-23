small_acs_value <- function(n = 5L, state = "AL") {
  out <- data.frame(GEOID = sprintf("0100100%04d", seq_len(n)),
                    variable = "B01003_001",
                    estimate = seq_len(n),
                    moe = 1)
  prov <- list(state = state, year = 2023L, geography = "tract",
               n_variables_received = 13L)
  attr(out, "cacs_provenance") <- prov
  attr(out, "cacs_acs_provenance") <- prov
  out
}

test_that("T-CACHE-STALE-01 warns below default threshold", {
  with_test_cache({
    key <- cache_key_for("acs")
    .cacs_cache_put(small_acs_value(5), key, "acs")
    expect_warning(
      suppressMessages(.cacs_cache_get(key, "acs")),
      class = "catchmentACS_warning_cache_stale_suspect"
    )
  })
})

test_that("T-CACHE-STALE-02 does not warn above threshold", {
  with_test_cache({
    key <- cache_key_for("acs")
    .cacs_cache_put(small_acs_value(1001), key, "acs")
    expect_no_warning(suppressMessages(.cacs_cache_get(key, "acs")))
  })
})

test_that("T-CACHE-STALE-03 threshold zero disables", {
  with_test_cache({
    withr::local_options(catchmentACS.stale_threshold_rows = 0L)
    key <- cache_key_for("acs")
    .cacs_cache_put(small_acs_value(5), key, "acs")
    expect_no_warning(suppressMessages(.cacs_cache_get(key, "acs")))
  })
})

test_that("T-CACHE-STALE-04 state override disables WY false positive", {
  with_test_cache({
    withr::local_options(catchmentACS.stale_threshold_WY = 3L)
    key <- cache_key_for("acs")
    .cacs_cache_put(small_acs_value(5, "WY"), key, "acs")
    expect_no_warning(suppressMessages(.cacs_cache_get(key, "acs")))
  })
})

test_that("T-CACHE-STALE-05 state override can tighten threshold", {
  with_test_cache({
    withr::local_options(catchmentACS.stale_threshold_AL = 10L)
    key <- cache_key_for("acs")
    .cacs_cache_put(small_acs_value(5, "AL"), key, "acs")
    expect_warning(
      suppressMessages(.cacs_cache_get(key, "acs")),
      class = "catchmentACS_warning_cache_stale_suspect"
    )
  })
})

test_that("T-CACHE-STALE-06 stale warning fires once per key", {
  with_test_cache({
    key <- cache_key_for("acs")
    .cacs_cache_put(small_acs_value(5), key, "acs")
    expect_warning(suppressMessages(.cacs_cache_get(key, "acs")))
    expect_no_warning(suppressMessages(.cacs_cache_get(key, "acs")))
  })
})

test_that("T-CACHE-STALE-07 different stale key warns separately", {
  with_test_cache({
    key1 <- cache_key_for("acs")
    key2 <- digest::digest("acs2", algo = "sha256")
    .cacs_cache_put(small_acs_value(5), key1, "acs")
    .cacs_cache_put(small_acs_value(5), key2, "acs")
    expect_warning(suppressMessages(.cacs_cache_get(key1, "acs")))
    expect_warning(suppressMessages(.cacs_cache_get(key2, "acs")))
  })
})

test_that("T-CACHE-STALE-08 negative threshold disables", {
  with_test_cache({
    withr::local_options(catchmentACS.stale_threshold_rows = -1L)
    key <- cache_key_for("acs")
    .cacs_cache_put(small_acs_value(5), key, "acs")
    expect_no_warning(suppressMessages(.cacs_cache_get(key, "acs")))
  })
})

test_that("T-CACHE-STALE-09 warning message includes cleanup recipe", {
  with_test_cache({
    key <- cache_key_for("acs")
    .cacs_cache_put(small_acs_value(5), key, "acs")
    expect_warning(
      suppressMessages(.cacs_cache_get(key, "acs")),
      regexp = "cacs_clear_cache"
    )
  })
})

test_that("T-CACHE-STALE-10 missing provenance still warns by row count", {
  with_test_cache({
    key <- cache_key_for("acs")
    .cacs_cache_put(data.frame(x = 1:5), key, "acs")
    expect_warning(suppressMessages(.cacs_cache_get(key, "acs")),
                   class = "catchmentACS_warning_cache_stale_suspect")
  })
})

test_that("T-CACHE-STALE-11 acs_test stale hit warns in test mode", {
  with_test_cache(namespace_mode = "test", {
    key <- cache_key_for("acs")
    .cacs_cache_put(small_acs_value(5), key, "acs")
    expect_warning(suppressMessages(.cacs_cache_get(key, "acs")),
                   class = "catchmentACS_warning_cache_stale_suspect")
  })
})

test_that("T-CACHE-STALE-12 acs_test stale warning names acs_test cleanup namespace", {
  with_test_cache(namespace_mode = "test", {
    key <- cache_key_for("acs")
    .cacs_cache_put(small_acs_value(5), key, "acs")
    expect_warning(
      suppressMessages(.cacs_cache_get(key, "acs")),
      regexp = "namespace = \"acs_test\""
    )
  })
})

test_that("T-CACHE-STALE-13 isochrone namespace never stale-warns", {
  with_test_cache({
    key <- cache_key_for("isochrone")
    .cacs_cache_put(data.frame(x = 1:5), key, "isochrone")
    expect_no_warning(suppressMessages(.cacs_cache_get(key, "isochrone")))
  })
})

test_that("T-CACHE-STALE-14 list value in acs namespace does not stale-warn", {
  with_test_cache({
    key <- cache_key_for("acs")
    .cacs_cache_put(list(x = 1:5), key, "acs")
    expect_no_warning(suppressMessages(.cacs_cache_get(key, "acs")))
  })
})

test_that("T-CACHE-STALE-15 custom threshold 3 does not warn at 4 rows", {
  with_test_cache({
    withr::local_options(catchmentACS.stale_threshold_rows = 3L)
    key <- cache_key_for("acs")
    .cacs_cache_put(small_acs_value(4), key, "acs")
    expect_no_warning(suppressMessages(.cacs_cache_get(key, "acs")))
  })
})

test_that("T-CACHE-STALE-16 custom threshold 3 warns at 2 rows", {
  with_test_cache({
    withr::local_options(catchmentACS.stale_threshold_rows = 3L)
    key <- cache_key_for("acs")
    .cacs_cache_put(small_acs_value(2), key, "acs")
    expect_warning(suppressMessages(.cacs_cache_get(key, "acs")),
                   class = "catchmentACS_warning_cache_stale_suspect")
  })
})

test_that("T-CACHE-STALE-17 invalid state override falls back to default", {
  with_test_cache({
    withr::local_options(catchmentACS.stale_threshold_AL = "tiny")
    key <- cache_key_for("acs")
    .cacs_cache_put(small_acs_value(5, "AL"), key, "acs")
    expect_warning(suppressMessages(.cacs_cache_get(key, "acs")),
                   class = "catchmentACS_warning_cache_stale_suspect")
  })
})
