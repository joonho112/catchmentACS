# ============================================================================
# Unit tests for R/cache.R - SHA-256 dispatch + memoise L1 + atomic writes.
# 10 cases.
# ============================================================================

# Isolated cache dir per test via withr::local_options
.with_cache_dir <- function(code) {
  td <- tempfile("cacs_test_")
  withr::with_options(
    list(catchmentACS.cache_dir = td, CACS_NO_CONFIRM = "1"),
    {
      Sys.setenv(CACS_NO_CONFIRM = "1")
      on.exit({
        if (dir.exists(td)) unlink(td, recursive = TRUE)
        memoise::forget(.cacs_cache_dir_memo)
      }, add = TRUE)
      memoise::forget(.cacs_cache_dir_memo)
      force(code)
    }
  )
}

test_that("T-CACHE-01 cacs_cache_dir creates directory", {
  .with_cache_dir({
    p <- cacs_cache_dir(create = TRUE)
    expect_true(dir.exists(p))
  })
})

test_that("T-CACHE-02 cacs_cache_dir(create=FALSE) returns path without creating", {
  .with_cache_dir({
    p <- cacs_cache_dir(create = FALSE)
    expect_type(p, "character")
    expect_length(p, 1L)
  })
})

test_that("T-CACHE-03 .cacs_cache_key SHA-256 stability - same payload, same key", {
  pl <- as.list(setNames(1:16, paste0("k", 1:16)))
  k1 <- .cacs_cache_key(pl, "isochrone")
  k2 <- .cacs_cache_key(pl, "isochrone")
  expect_identical(k1, k2)
  expect_match(k1, "^[0-9a-f]{64}$")  # SHA-256 = 64 hex chars (per Sec. 9.5 patch)
})

test_that("T-CACHE-04 .cacs_cache_key payload ordering invariance", {
  pl_a <- as.list(setNames(1:16, paste0("k", 1:16)))
  pl_b <- pl_a[order(names(pl_a), decreasing = TRUE)]
  k1 <- .cacs_cache_key(pl_a, "isochrone")
  k2 <- .cacs_cache_key(pl_b, "isochrone")
  expect_identical(k1, k2)
})

test_that("T-CACHE-05 .cacs_cache_key namespace dispatch - different ns -> different key", {
  pl12 <- as.list(setNames(1:12, paste0("k", 1:12)))
  pl14 <- as.list(setNames(1:14, paste0("k", 1:14)))
  k_acs <- .cacs_cache_key(pl12, "acs")
  k_int <- .cacs_cache_key(pl14, "intersect")
  expect_match(k_acs, "^[0-9a-f]+$")
  expect_match(k_int, "^[0-9a-f]+$")
  expect_false(identical(k_acs, k_int))
})

test_that("T-CACHE-06 .cacs_cache_put + .cacs_cache_get roundtrip", {
  .with_cache_dir({
    pl <- as.list(setNames(1:16, paste0("k", 1:16)))
    key <- .cacs_cache_key(pl, "isochrone")
    val <- list(message = "hello", n = 42L)
    expect_true(.cacs_cache_put(val, key, "isochrone"))
    got <- suppressMessages(.cacs_cache_get(key, "isochrone"))
    expect_identical(got, val)
  })
})

test_that("T-CACHE-07 cacs_cache_status enumerates files", {
  .with_cache_dir({
    pl <- as.list(setNames(1:16, paste0("k", 1:16)))
    key <- .cacs_cache_key(pl, "isochrone")
    .cacs_cache_put(list(x = 1), key, "isochrone")
    status <- cacs_cache_status()
    expect_s3_class(status, "tbl_df")
    expect_equal(nrow(status), 4L)
    expect_true(all(c("namespace", "n_entries", "orphan_tmp",
                      "n_fingerprints", "legacy_entries",
                      "total_size_mb", "oldest", "newest") %in% names(status)))
    iso_row <- status[status$namespace == "isochrone", ]
    expect_equal(iso_row$n_entries, 1L)
  })
})

test_that("T-CACHE-08 cacs_clear_cache removes files", {
  .with_cache_dir({
    pl <- as.list(setNames(1:16, paste0("k", 1:16)))
    key <- .cacs_cache_key(pl, "isochrone")
    .cacs_cache_put(list(x = 1), key, "isochrone")
    expect_equal(cacs_cache_status()$n_entries[1], 1L)
    suppressMessages(out <- cacs_clear_cache("isochrone", confirm = FALSE))
    expect_s3_class(out, "tbl_df")
    expect_equal(sum(out$n_removed), 2L)
    expect_equal(cacs_cache_status()$n_entries[1], 0L)
  })
})

test_that("T-CACHE-09 schema validation - wrong dims aborts", {
  pl_wrong <- as.list(setNames(1:5, paste0("k", 1:5)))
  expect_error(
    .cacs_cache_key(pl_wrong, "isochrone"),
    class = "catchmentACS_error_schema"
  )
})

test_that("T-CACHE-10 atomic write - orphan .tmp ignored on read", {
  .with_cache_dir({
    pl <- as.list(setNames(1:16, paste0("k", 1:16)))
    key <- .cacs_cache_key(pl, "isochrone")
    base <- cacs_cache_dir(create = TRUE)
    iso_dir <- file.path(base, "isochrone")
    dir.create(iso_dir, recursive = TRUE, showWarnings = FALSE)
    # Create orphan .tmp (simulating killed mid-write)
    writeLines("partial", file.path(iso_dir, paste0(key, ".rds.tmp")))
    # .cacs_cache_get should treat it as miss (no .rds present)
    got <- .cacs_cache_get(key, "isochrone")
    expect_null(got)
    # cacs_cache_status should report orphan_tmp = 1
    status <- cacs_cache_status()
    iso_row <- status[status$namespace == "isochrone", ]
    expect_equal(iso_row$orphan_tmp, 1L)
  })
})
