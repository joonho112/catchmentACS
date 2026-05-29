# Shared helpers for v0.3 cache contract tests.

with_test_cache <- function(code,
                            mode = "production",
                            namespace_mode = NULL,
                            cache_dir = tempfile("cacs_cache_test_"),
                            announce = FALSE,
                            cleanup = TRUE) {
  if (!is.null(namespace_mode)) {
    mode <- namespace_mode
  }
  old_env <- Sys.getenv(c(
    "CACS_NO_CONFIRM", "CACS_CACHE_ENABLED", "CACS_CACHE_NAMESPACE_MODE",
    "CACS_NO_CACHE", "CACS_CACHE_TEST_MODE", "CACS_CACHE_ACS",
    "CACS_CACHE_ISOCHRONE", "CACS_CACHE_INTERSECT"
  ), unset = NA_character_)
  withr::with_options(
    list(
      catchmentACS.cache_dir = cache_dir,
      catchmentACS.cache_namespace_mode = mode,
      catchmentACS.cache_enabled = TRUE,
      catchmentACS.cache_announce = announce,
      catchmentACS.stale_threshold_rows = 1000L
    ),
    {
      Sys.setenv(CACS_NO_CONFIRM = "1")
      Sys.unsetenv(c(
        "CACS_CACHE_ENABLED", "CACS_CACHE_NAMESPACE_MODE", "CACS_NO_CACHE",
        "CACS_CACHE_TEST_MODE", "CACS_CACHE_ACS", "CACS_CACHE_ISOCHRONE",
        "CACS_CACHE_INTERSECT"
      ))
      on.exit({
        if (isTRUE(cleanup) && dir.exists(cache_dir)) {
          unlink(cache_dir, recursive = TRUE)
        }
        for (nm in names(old_env)) {
          if (is.na(old_env[[nm]])) {
            Sys.unsetenv(nm)
          } else {
            do.call(Sys.setenv, stats::setNames(list(old_env[[nm]]), nm))
          }
        }
        memoise::forget(.cacs_cache_dir_memo)
        .cacs_cache_reset_state()
      }, add = TRUE)
      memoise::forget(.cacs_cache_dir_memo)
      .cacs_cache_reset_state()
      force(code)
    }
  )
}

with_cache_root <- function(code, mode = "production") {
  with_test_cache(code, mode = mode)
}

cache_payload <- function(x) {
  n <- if (is.character(x)) {
    switch(
      x,
      isochrone = 16L,
      acs = 12L,
      intersect = 14L,
      stop("unknown cache namespace")
    )
  } else {
    as.integer(x)
  }
  as.list(stats::setNames(seq_len(n), paste0("k", seq_len(n))))
}

cache_key_for <- function(namespace) {
  .cacs_cache_key(cache_payload(namespace), namespace)
}

cache_key <- cache_key_for

cache_paths <- function(root, namespace, key, mode = "production") {
  effective <- if (identical(namespace, "acs") && identical(mode, "test")) {
    "acs_test"
  } else {
    namespace
  }
  ns_dir <- file.path(root, effective)
  list(
    namespace = effective,
    dir = ns_dir,
    rds = file.path(ns_dir, paste0(key, ".rds")),
    fingerprint = file.path(ns_dir, paste0(key, ".fingerprint")),
    rds_tmp = file.path(ns_dir, paste0(key, ".rds.tmp")),
    fingerprint_tmp = file.path(ns_dir, paste0(key, ".fingerprint.tmp"))
  )
}

cache_acs_tbl <- function(n = 5L, state = "AL", year = 2023L) {
  out <- tibble::tibble(
    GEOID = sprintf("0100100%04d", seq_len(n)),
    NAME = paste("Tract", seq_len(n)),
    variable = "B19013_001",
    estimate = seq_len(n) * 100,
    moe = seq_len(n) * 10
  )
  attr(out, "cacs_provenance") <- list(
    state = state,
    year = year,
    survey = "acs5",
    geography = "tract",
    variables = "B19013_001",
    cache_key = "upstream-key",
    cache_namespace = "acs"
  )
  attr(out, "cacs_acs_provenance") <- attr(out, "cacs_provenance")
  out
}

cache_iso_sf <- function(ring_topology = "cumulative", res_param = 70L) {
  geom <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
  ))), crs = 4326)
  out <- sf::st_sf(
    tibble::tibble(
      site_id = "S1",
      drive_time_min = 5L,
      ring_topology = ring_topology
    ),
    geometry = geom
  )
  attr(out, "cacs_isochrone_provenance") <- list(
    cache_key = "iso-upstream-key",
    cache_namespace = "isochrone",
    res_param = res_param,
    ring_topology = ring_topology
  )
  attr(out, "cacs_provenance") <- attr(out, "cacs_isochrone_provenance")
  out
}

cache_ns_dir <- function(namespace, root = cacs_cache_dir(create = TRUE)) {
  file.path(root, namespace)
}

cache_write_legacy_rds <- function(namespace, key, value) {
  ns_dir <- cache_ns_dir(namespace)
  dir.create(ns_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(value, file.path(ns_dir, paste0(key, ".rds")))
}

bug010_vars <- function(fx) {
  sort(unique(fx$acs_sf$variable))
}

bug010_acs_key <- function(state = "AL",
                           year = 2023L,
                           variables = bug010_vars(load_replay_fixture("cache_poison"))) {
  payload <- list(
    state = state,
    year = as.integer(year),
    survey = "acs5",
    geography = "tract",
    variables = sort(unique(variables)),
    geometry_vintage = .acs_geometry_vintage(year, "tract"),
    tidycensus_version = as.character(utils::packageVersion("tidycensus")),
    tigris_version = as.character(utils::packageVersion("tigris")),
    sf_version = as.character(utils::packageVersion("sf")),
    schema_version = "1.0",
    package_version = as.character(utils::packageVersion("catchmentACS")),
    r_version = paste(R.version$major, R.version$minor, sep = ".")
  )
  .cacs_cache_key(payload, "acs")
}

bug010_mock_codebook <- function(vars) {
  tibble::tibble(name = vars, label = vars, concept = "cache poison regression")
}
