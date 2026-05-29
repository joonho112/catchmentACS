test_that("T-P3-CACHE-FP-01 cache put writes RDS and fingerprint sidecar", {
  with_test_cache({
    key <- .cacs_cache_key(cache_payload(16), "isochrone")
    expect_true(.cacs_cache_put(list(x = 1), key, "isochrone"))
    ns_dir <- file.path(cacs_cache_dir(), "isochrone")
    expect_true(file.exists(file.path(ns_dir, paste0(key, ".rds"))))
    expect_true(file.exists(file.path(ns_dir, paste0(key, ".fingerprint"))))
  })
})

test_that("T-P3-CACHE-FP-02 sidecar metadata records sha256 digest", {
  with_test_cache({
    key <- .cacs_cache_key(cache_payload(16), "isochrone")
    val <- list(x = 1, y = "a")
    .cacs_cache_put(val, key, "isochrone")
    meta <- readRDS(file.path(cacs_cache_dir(), "isochrone", paste0(key, ".fingerprint")))
    expect_identical(meta$algorithm, "sha256")
    expect_identical(meta$schema_version, "cache-v0.3")
    expect_true(meta$serialize)
    expect_match(meta$digest, "^[0-9a-f]{64}$")
    expect_identical(meta$digest, .cacs_cache_value_digest(val))
  })
})

test_that("T-P3-CACHE-FP-03 matching fingerprint returns cached object", {
  with_test_cache({
    key <- .cacs_cache_key(cache_payload(16), "isochrone")
    val <- list(message = "ok", n = 2L)
    .cacs_cache_put(val, key, "isochrone")
    expect_identical(suppressMessages(.cacs_cache_get(key, "isochrone")), val)
  })
})

test_that("T-P3-CACHE-FP-04 missing sidecar invalidates legacy RDS", {
  with_test_cache({
    key <- .cacs_cache_key(cache_payload(16), "isochrone")
    ns_dir <- file.path(cacs_cache_dir(), "isochrone")
    dir.create(ns_dir, recursive = TRUE, showWarnings = FALSE)
    path <- file.path(ns_dir, paste0(key, ".rds"))
    saveRDS(list(old = TRUE), path)
    expect_message(
      got <- .cacs_cache_get(key, "isochrone"),
      class = "catchmentACS_message_cache_legacy_invalidated"
    )
    expect_null(got)
    expect_false(file.exists(path))
  })
})

test_that("T-P3-CACHE-FP-05 malformed sidecar invalidates pair", {
  with_test_cache({
    key <- .cacs_cache_key(cache_payload(16), "isochrone")
    ns_dir <- file.path(cacs_cache_dir(), "isochrone")
    dir.create(ns_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(list(x = 1), file.path(ns_dir, paste0(key, ".rds")))
    saveRDS(list(algorithm = "sha256", digest = "bad"), file.path(ns_dir, paste0(key, ".fingerprint")))
    expect_message(
      got <- .cacs_cache_get(key, "isochrone"),
      class = "catchmentACS_message_cache_fingerprint_mismatch"
    )
    expect_null(got)
    expect_equal(cacs_cache_status()$n_entries[1], 0L)
  })
})

test_that("T-P3-CACHE-FP-06 corrupt RDS invalidates pair", {
  with_test_cache({
    key <- .cacs_cache_key(cache_payload(16), "isochrone")
    ns_dir <- file.path(cacs_cache_dir(), "isochrone")
    dir.create(ns_dir, recursive = TRUE, showWarnings = FALSE)
    writeLines("not-rds", file.path(ns_dir, paste0(key, ".rds")))
    saveRDS(
      list(algorithm = "sha256", digest = paste(rep("0", 64), collapse = ""),
           schema_version = "cache-v0.3", serialize = TRUE),
      file.path(ns_dir, paste0(key, ".fingerprint"))
    )
    expect_message(
      got <- .cacs_cache_get(key, "isochrone"),
      class = "catchmentACS_message_cache_fingerprint_mismatch"
    )
    expect_null(got)
  })
})

test_that("T-P3-CACHE-FP-07 digest mismatch invalidates pair", {
  with_test_cache({
    key <- .cacs_cache_key(cache_payload(16), "isochrone")
    .cacs_cache_put(list(x = 1), key, "isochrone")
    saveRDS(list(x = 2), file.path(cacs_cache_dir(), "isochrone", paste0(key, ".rds")))
    expect_message(
      got <- .cacs_cache_get(key, "isochrone"),
      class = "catchmentACS_message_cache_fingerprint_mismatch"
    )
    expect_null(got)
  })
})

test_that("T-P3-CACHE-FP-08 status counts legacy and fingerprint files", {
  with_test_cache({
    key1 <- .cacs_cache_key(cache_payload(16), "isochrone")
    key2 <- paste0(substr(key1, 1, 63), "0")
    ns_dir <- file.path(cacs_cache_dir(), "isochrone")
    dir.create(ns_dir, recursive = TRUE, showWarnings = FALSE)
    .cacs_cache_put(list(x = 1), key1, "isochrone")
    saveRDS(list(old = TRUE), file.path(ns_dir, paste0(key2, ".rds")))
    status <- cacs_cache_status()
    iso <- status[status$namespace == "isochrone", ]
    expect_equal(iso$n_entries, 2L)
    expect_equal(iso$n_fingerprints, 1L)
    expect_equal(iso$legacy_entries, 1L)
  })
})

test_that("T-P3-CACHE-FP-09 clear removes fingerprints and temp files", {
  with_test_cache({
    key <- .cacs_cache_key(cache_payload(16), "isochrone")
    ns_dir <- file.path(cacs_cache_dir(), "isochrone")
    dir.create(ns_dir, recursive = TRUE, showWarnings = FALSE)
    .cacs_cache_put(list(x = 1), key, "isochrone")
    writeLines("tmp", file.path(ns_dir, paste0(key, ".fingerprint.tmp")))
    suppressMessages(out <- cacs_clear_cache("isochrone", confirm = FALSE))
    expect_gte(sum(out$n_removed), 3L)
    expect_length(list.files(ns_dir), 0L)
  })
})

test_that("T-P3-CACHE-FP-10 recursive payload normalization is stable", {
  a <- list(z = 1, nested = list(b = 2, a = 1), x = "same")
  b <- list(x = "same", nested = list(a = 1, b = 2), z = 1)
  pa <- c(cache_payload(13), list(extra = a, extra2 = "q", extra3 = "r"))
  pb <- c(cache_payload(13), list(extra3 = "r", extra2 = "q", extra = b))
  expect_identical(.cacs_cache_key(pa, "isochrone"),
                   .cacs_cache_key(pb, "isochrone"))
})
