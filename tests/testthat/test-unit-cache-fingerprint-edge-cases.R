test_that("T-CACHE-EDGE-01 sidecar without rds is a miss", {
  with_test_cache({
    key <- cache_key_for("isochrone")
    dir.create(cache_ns_dir("isochrone"), recursive = TRUE, showWarnings = FALSE)
    saveRDS(list(algorithm = "sha256", digest = strrep("0", 64),
                 schema_version = "cache-v0.3", serialize = TRUE),
            file.path(cache_ns_dir("isochrone"), paste0(key, ".fingerprint")))
    expect_null(.cacs_cache_get(key, "isochrone"))
  })
})

test_that("T-CACHE-EDGE-02 fingerprint tmp is reported", {
  with_test_cache({
    dir.create(cache_ns_dir("isochrone"), recursive = TRUE, showWarnings = FALSE)
    writeLines("tmp", file.path(cache_ns_dir("isochrone"), "abc.fingerprint.tmp"))
    row <- cacs_cache_status()[cacs_cache_status()$namespace == "isochrone", ]
    expect_equal(row$orphan_fingerprint_tmp, 1L)
  })
})

test_that("T-CACHE-EDGE-03 clear removes fingerprint tmp", {
  with_test_cache({
    dir.create(cache_ns_dir("isochrone"), recursive = TRUE, showWarnings = FALSE)
    writeLines("tmp", file.path(cache_ns_dir("isochrone"), "abc.fingerprint.tmp"))
    suppressMessages(out <- cacs_clear_cache("isochrone", confirm = FALSE))
    expect_equal(sum(out$n_removed), 1L)
  })
})

test_that("T-CACHE-EDGE-04 zero-byte rds invalidates", {
  with_test_cache({
    key <- cache_key_for("isochrone")
    .cacs_cache_put(list(x = 1), key, "isochrone")
    file.create(file.path(cache_ns_dir("isochrone"), paste0(key, ".rds")))
    expect_message(got <- .cacs_cache_get(key, "isochrone"),
                   class = "catchmentACS_message_cache_fingerprint_mismatch")
    expect_null(got)
  })
})

test_that("T-CACHE-EDGE-05 unsupported fingerprint algorithm invalidates", {
  with_test_cache({
    key <- cache_key_for("isochrone")
    .cacs_cache_put(list(x = 1), key, "isochrone")
    saveRDS(list(algorithm = "md5", digest = strrep("0", 64),
                 schema_version = "cache-v0.3", serialize = TRUE),
            file.path(cache_ns_dir("isochrone"), paste0(key, ".fingerprint")))
    expect_message(got <- .cacs_cache_get(key, "isochrone"),
                   class = "catchmentACS_message_cache_fingerprint_mismatch")
    expect_null(got)
  })
})

test_that("T-CACHE-EDGE-06 invalid digest length invalidates", {
  with_test_cache({
    key <- cache_key_for("isochrone")
    .cacs_cache_put(list(x = 1), key, "isochrone")
    saveRDS(list(algorithm = "sha256", digest = "abc",
                 schema_version = "cache-v0.3", serialize = TRUE),
            file.path(cache_ns_dir("isochrone"), paste0(key, ".fingerprint")))
    expect_message(got <- .cacs_cache_get(key, "isochrone"),
                   class = "catchmentACS_message_cache_fingerprint_mismatch")
    expect_null(got)
  })
})

test_that("T-CACHE-EDGE-07 text sidecar invalidates", {
  with_test_cache({
    key <- cache_key_for("isochrone")
    .cacs_cache_put(list(x = 1), key, "isochrone")
    writeLines("not rds", file.path(cache_ns_dir("isochrone"), paste0(key, ".fingerprint")))
    expect_message(got <- .cacs_cache_get(key, "isochrone"),
                   class = "catchmentACS_message_cache_fingerprint_mismatch")
    expect_null(got)
  })
})

test_that("T-CACHE-EDGE-08 second put overwrites object and sidecar", {
  with_test_cache({
    key <- cache_key_for("isochrone")
    .cacs_cache_put(list(x = 1), key, "isochrone")
    .cacs_cache_put(list(x = 2), key, "isochrone")
    expect_identical(suppressMessages(.cacs_cache_get(key, "isochrone")), list(x = 2))
  })
})

test_that("T-CACHE-EDGE-09 malformed payload names abort", {
  pl <- cache_payload(16)
  names(pl)[1] <- ""
  expect_error(.cacs_cache_key(pl, "isochrone"), class = "catchmentACS_error_schema")
})

test_that("T-CACHE-EDGE-10 duplicated payload names abort", {
  pl <- cache_payload(16)
  names(pl)[2] <- names(pl)[1]
  expect_error(.cacs_cache_key(pl, "isochrone"), class = "catchmentACS_error_schema")
})

test_that("T-CACHE-EDGE-11 non-list payload aborts", {
  expect_error(.cacs_cache_key("not-list", "isochrone"), class = "catchmentACS_error_schema")
})

test_that("T-CACHE-EDGE-12 invalid namespace is rejected", {
  expect_error(.cacs_cache_key(cache_payload(12), "bogus"))
})

test_that("T-CACHE-EDGE-13 status all namespaces share same columns", {
  with_test_cache({
    st <- cacs_cache_status()
    expect_true(all(c("namespace", "n_entries", "n_fingerprints",
                      "legacy_entries", "orphan_fingerprint_tmp") %in% names(st)))
    expect_equal(nrow(st), 4L)
  })
})

test_that("T-CACHE-EDGE-14 clear acs_test namespace only", {
  with_test_cache(namespace_mode = "test", {
    key <- cache_key_for("acs")
    .cacs_cache_put(data.frame(x = 1), key, "acs")
    suppressMessages(out <- cacs_clear_cache("acs_test", confirm = FALSE))
    expect_equal(out$n_removed, 2L)
    expect_equal(cacs_cache_status()$n_entries[cacs_cache_status()$namespace == "acs_test"], 0L)
  })
})

test_that("T-CACHE-EDGE-15 disabled put does not create sidecar tmp", {
  with_test_cache({
    withr::local_options(catchmentACS.cache_enabled = FALSE)
    key <- cache_key_for("isochrone")
    .cacs_cache_put(list(x = 1), key, "isochrone")
    expect_false(dir.exists(cache_ns_dir("isochrone")))
  })
})

test_that("T-CACHE-EDGE-16 fingerprint read helper rejects NULL metadata", {
  with_test_cache({
    path <- tempfile("fp_")
    saveRDS(NULL, path)
    expect_null(.cacs_cache_read_fingerprint(path))
  })
})

test_that("T-CACHE-EDGE-17 wrong fingerprint schema version invalidates", {
  with_test_cache({
    key <- cache_key_for("isochrone")
    .cacs_cache_put(list(x = 1), key, "isochrone")
    saveRDS(list(algorithm = "sha256", digest = strrep("0", 64),
                 schema_version = "cache-v0.2", serialize = TRUE),
            file.path(cache_ns_dir("isochrone"), paste0(key, ".fingerprint")))
    expect_message(got <- .cacs_cache_get(key, "isochrone"),
                   class = "catchmentACS_message_cache_fingerprint_mismatch")
    expect_null(got)
  })
})

test_that("T-CACHE-EDGE-18 serialize flag must be TRUE", {
  with_test_cache({
    key <- cache_key_for("isochrone")
    .cacs_cache_put(list(x = 1), key, "isochrone")
    saveRDS(list(algorithm = "sha256", digest = strrep("0", 64),
                 schema_version = "cache-v0.3", serialize = FALSE),
            file.path(cache_ns_dir("isochrone"), paste0(key, ".fingerprint")))
    expect_message(got <- .cacs_cache_get(key, "isochrone"),
                   class = "catchmentACS_message_cache_fingerprint_mismatch")
    expect_null(got)
  })
})

test_that("T-CACHE-EDGE-19 uppercase fingerprint digest invalidates", {
  with_test_cache({
    key <- cache_key_for("isochrone")
    .cacs_cache_put(list(x = 1), key, "isochrone")
    saveRDS(list(algorithm = "sha256", digest = toupper(strrep("a", 64)),
                 schema_version = "cache-v0.3", serialize = TRUE),
            file.path(cache_ns_dir("isochrone"), paste0(key, ".fingerprint")))
    expect_message(got <- .cacs_cache_get(key, "isochrone"),
                   class = "catchmentACS_message_cache_fingerprint_mismatch")
    expect_null(got)
  })
})
