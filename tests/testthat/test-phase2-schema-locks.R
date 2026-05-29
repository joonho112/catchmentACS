# ============================================================================
# Phase 2 schema locks: cumulative rings, res default, validator preflight.
# ============================================================================


.P2_V020_LONG_REQUIRED_COLS <- c(
  "site_id", "drive_time_min", "variable", "estimate", "moe",
  "weight_sum", "n_tracts", "n_tracts_num", "n_tracts_den",
  "provider", "profile", "osm_snapshot_date", "acs_year",
  "weight_method", "estimand_family", "weight_basis",
  "moe_formula_requested", "moe_formula_effective", "moe_fallback",
  "moe_fallback_reason", "failure_origin",
  "weight_uncertainty_propagated"
)

.P2_ISSUE_COLS <- c(
  "severity", "check", "col", "actual", "expected", "fix_hint", "example"
)

.P2_LOCK_ENV <- new.env(parent = emptyenv())

.p2_read_extdata <- function(name) {
  path <- system.file("extdata", name, package = "catchmentACS")
  testthat::skip_if(!nzchar(path) || !file.exists(path),
                    paste("extdata fixture unavailable:", name))
  readRDS(path)
}

.p2_upgrade_iso <- function(iso_sf) {
  out <- iso_sf
  if (!"ring_topology" %in% names(out)) {
    out$ring_topology <- "cumulative"
  }
  out
}

.p2_fixture032_weighted <- function() {
  if (!exists("weighted032", envir = .P2_LOCK_ENV, inherits = FALSE)) {
    fx <- load_replay_fixture("032_3site_fresh")
    iso <- .p2_upgrade_iso(fx$iso_sf_v2)
    out <- suppressWarnings(cacs_intersect_weight(
      iso_sf = iso,
      acs_sf = fx$acs_sf,
      verbose = FALSE
    ))
    assign("fixture032", fx, envir = .P2_LOCK_ENV)
    assign("weighted032", out, envir = .P2_LOCK_ENV)
  }
  get("weighted032", envir = .P2_LOCK_ENV, inherits = FALSE)
}

.p2_fixture032 <- function() {
  .p2_fixture032_weighted()
  get("fixture032", envir = .P2_LOCK_ENV, inherits = FALSE)
}

.p2_ordered <- function(x) {
  x[order(x$site_id, x$drive_time_min, x$variable), , drop = FALSE]
}

.p2_msg <- function(expr) {
  conditionMessage(rlang::catch_cnd(expr))
}

.p2_help_text <- function(topic) {
  rd_path <- file.path("man", paste0(topic, ".Rd"))
  if (!file.exists(rd_path)) {
    rd_path <- testthat::test_path("..", "..", "man", paste0(topic, ".Rd"))
  }
  if (file.exists(rd_path)) {
    return(paste(capture.output(tools::Rd2txt(rd_path)), collapse = "\n"))
  }
  help_ref <- utils::help(topic, package = "catchmentACS")
  if (length(help_ref) == 0L) {
    testthat::skip(paste("help topic unavailable:", topic))
  }
  paste(capture.output(tools::Rd2txt(utils:::.getHelpFile(help_ref))),
        collapse = "\n")
}


test_that("T-P2-SCHEMA-01 ISO canonical schema is 16 columns", {
  expect_equal(length(.ISO_CANONICAL_COLS), 16L)
})

test_that("T-P2-SCHEMA-02 ISO canonical schema includes ring_topology once", {
  expect_equal(sum(.ISO_CANONICAL_COLS == "ring_topology"), 1L)
  expect_equal(tail(.ISO_CANONICAL_COLS, 1L), "ring_topology")
})

test_that("T-P2-SCHEMA-03 long mandatory schema is 23 columns", {
  expect_equal(length(.LONG_REQUIRED_COLS), 23L)
})

test_that("T-P2-SCHEMA-04 ring_topology is the only new long column", {
  expect_equal(setdiff(.LONG_REQUIRED_COLS, .P2_V020_LONG_REQUIRED_COLS),
               "ring_topology")
  expect_equal(setdiff(.P2_V020_LONG_REQUIRED_COLS, .LONG_REQUIRED_COLS),
               character(0))
})

test_that("T-P2-SCHEMA-05 common long schema order is preserved", {
  expect_equal(.LONG_REQUIRED_COLS[.LONG_REQUIRED_COLS != "ring_topology"],
               .P2_V020_LONG_REQUIRED_COLS)
})

test_that("T-P2-SCHEMA-06 long schema excludes annulus ring bounds", {
  expect_false(any(c("isomin", "isomax") %in% .LONG_REQUIRED_COLS))
})

test_that("T-P2-SCHEMA-07 Phase 2 helper exports are present", {
  exports <- getNamespaceExports("catchmentACS")
  expect_true("cacs_rings_to_cumulative" %in% exports)
  expect_true("cacs_validate_iso" %in% exports)
})

test_that("T-P2-SCHEMA-08 cacs_validate_iso help is generated", {
  txt <- .p2_help_text("cacs_validate_iso")
  expect_match(txt, "cacs_validate_iso", fixed = TRUE)
  expect_match(txt, "severity", fixed = TRUE)
})

test_that("T-P2-SCHEMA-09 Phase 2 condition classes are registered", {
  expect_equal(.cacs_cond_classes("message", "res_default_changed")[[1L]],
               "catchmentACS_message_res_default_changed")
  err <- rlang::catch_cnd(.cli_abort_annulus_input("annulus"))
  expect_equal(head(class(err), 6L), c(
    "catchmentACS_error_annulus_input",
    "cacs_error_annulus_input",
    "cacs_error_schema",
    "catchmentACS_error_schema",
    "catchmentACS_error",
    "catchmentACS_condition"
  ))
  expect_s3_class(err, "catchmentACS_error_annulus_input")
  expect_s3_class(err, "cacs_error_annulus_input")
  expect_s3_class(err, "catchmentACS_error_schema")
})

test_that("T-P2-SCHEMA-10 cacs_run accepts res in iso_args", {
  expect_true("res" %in% .CACS_RUN_ARG_KEYS$iso)
})

test_that("T-P2-SCHEMA-11 legacy 2025 isochrones ship as 16-column sf", {
  iso <- .p2_read_extdata("legacy_2025_isochrones.rds")
  expect_s3_class(iso, "sf")
  expect_equal(ncol(iso), 16L)
  expect_true(all(.ISO_CANONICAL_COLS %in% names(iso)))
})

test_that("T-P2-SCHEMA-12 legacy 2025 isochrones are cumulative", {
  iso <- .p2_read_extdata("legacy_2025_isochrones.rds")
  expect_equal(unique(iso$ring_topology), "cumulative")
  expect_true(.validate_iso_schema(iso))
})

test_that("T-P2-SCHEMA-13 replay fixture preserves v0.2 ISO baseline shape", {
  fx <- load_replay_fixture("032_3site_fresh")
  expect_s3_class(fx$iso_sf_v2, "sf")
  expect_equal(ncol(fx$iso_sf_v2), 15L)
  expect_false("ring_topology" %in% names(fx$iso_sf_v2))
})

test_that("T-P2-SCHEMA-14 replay fixture preserves v0.2 weighted baseline shape", {
  fx <- load_replay_fixture("032_3site_fresh")
  expect_equal(ncol(fx$weighted_seam_v2), 22L)
  expect_equal(names(fx$weighted_seam_v2), .P2_V020_LONG_REQUIRED_COLS)
})

test_that("T-P2-SCHEMA-15 in-memory ISO upgrade adds only ring_topology", {
  fx <- load_replay_fixture("032_3site_fresh")
  iso_v3 <- .p2_upgrade_iso(fx$iso_sf_v2)
  expect_equal(setdiff(names(iso_v3), names(fx$iso_sf_v2)), "ring_topology")
  expect_equal(setdiff(names(fx$iso_sf_v2), names(iso_v3)), character(0))
})

test_that("T-P2-SCHEMA-16 upgraded replay ISO validates", {
  fx <- load_replay_fixture("032_3site_fresh")
  iso_v3 <- .p2_upgrade_iso(fx$iso_sf_v2)
  expect_true(.validate_iso_schema(iso_v3))
})

test_that("T-P2-SCHEMA-17 upgraded replay ISO has zero preflight issues", {
  fx <- load_replay_fixture("032_3site_fresh")
  expect_equal(nrow(cacs_validate_iso(.p2_upgrade_iso(fx$iso_sf_v2))), 0L)
})

test_that("T-P2-SCHEMA-18 annulus replay hard-rejects with special class", {
  annulus <- load_replay_fixture("annulus_input")
  err <- rlang::catch_cnd(.validate_iso_schema(annulus))
  expect_s3_class(err, "catchmentACS_error_annulus_input")
  expect_s3_class(err, "cacs_error_annulus_input")
  expect_match(conditionMessage(err), "cacs_rings_to_cumulative", fixed = TRUE)
})

test_that("T-P2-SCHEMA-19 annulus preflight reports conversion example", {
  annulus <- load_replay_fixture("annulus_input")
  issues <- cacs_validate_iso(annulus)
  hit <- issues[issues$check == "iso_annulus_topology", ]
  expect_equal(nrow(hit), 1L)
  expect_equal(hit$example, "iso_sf <- cacs_rings_to_cumulative(iso_sf)")
})

test_that("T-P2-SCHEMA-20 issue tibble schema is stable", {
  annulus <- load_replay_fixture("annulus_input")
  expect_equal(names(cacs_validate_iso(annulus)), .P2_ISSUE_COLS)
})

test_that("T-P2-SCHEMA-21 preflight reports multiple perturbed issues", {
  iso <- sf::st_transform(.p2_read_extdata("legacy_2025_isochrones.rds")[1, ],
                          4269)
  iso$ring_topology <- NULL
  iso$provider_downgrade <- "FALSE"
  issues <- cacs_validate_iso(iso)
  expect_true(all(c(
    "iso_missing_column", "iso_crs", "iso_provider_downgrade_type"
  ) %in% issues$check))
})

test_that("T-P2-SCHEMA-22 CRS validator keeps transform example", {
  iso <- sf::st_transform(.p2_read_extdata("legacy_2025_isochrones.rds")[1, ],
                          4269)
  msg <- .p2_msg(.validate_iso_schema(iso))
  expect_match(msg, "iso_sf <- sf::st_transform(iso_sf, 4326)", fixed = TRUE)
})

test_that("T-P2-SCHEMA-23 logical validator keeps repair example", {
  iso <- .p2_read_extdata("legacy_2025_isochrones.rds")[1, ]
  iso$provider_downgrade <- "FALSE"
  msg <- .p2_msg(.validate_iso_schema(iso))
  expect_match(msg, "iso_sf$provider_downgrade <- FALSE", fixed = TRUE)
})

test_that("T-P2-SCHEMA-24 missing ring_topology points to preflight helper", {
  iso <- .p2_read_extdata("legacy_2025_isochrones.rds")[1, ]
  iso$ring_topology <- NULL
  msg <- .p2_msg(.validate_iso_schema(iso))
  expect_match(msg, "cacs_validate_iso(iso_sf)", fixed = TRUE)
})

test_that("T-P2-GOLDEN-01 direct intersection emits 23 mandatory columns", {
  out <- .p2_fixture032_weighted()
  expect_equal(names(tibble::as_tibble(out)[.LONG_REQUIRED_COLS]),
               .LONG_REQUIRED_COLS)
})

test_that("T-P2-GOLDEN-02 ring_topology is the only new weighted column", {
  out <- tibble::as_tibble(.p2_fixture032_weighted())[.LONG_REQUIRED_COLS]
  fx <- .p2_fixture032()
  expect_equal(setdiff(names(out), names(fx$weighted_seam_v2)),
               "ring_topology")
})

test_that("T-P2-GOLDEN-03 ring_topology is cumulative on every weighted row", {
  out <- .p2_fixture032_weighted()
  expect_equal(unique(out$ring_topology), "cumulative")
})

test_that("T-P2-GOLDEN-04 weighted row count and keys match v0.2", {
  out <- .p2_ordered(tibble::as_tibble(.p2_fixture032_weighted()))
  ref <- .p2_ordered(.p2_fixture032()$weighted_seam_v2)
  expect_equal(nrow(out), nrow(ref))
  expect_equal(out[c("site_id", "drive_time_min", "variable")],
               ref[c("site_id", "drive_time_min", "variable")],
               ignore_attr = TRUE)
})

test_that("T-P2-GOLDEN-05 common weighted values match v0.2", {
  out <- .p2_ordered(tibble::as_tibble(.p2_fixture032_weighted()))
  ref <- .p2_ordered(.p2_fixture032()$weighted_seam_v2)
  common <- c("estimate", "moe", "weight_sum", "n_tracts",
              "n_tracts_num", "n_tracts_den")
  expect_equal(out[common], ref[common], tolerance = 1e-8, ignore_attr = TRUE)
})

test_that("T-P2-GOLDEN-06 provenance and formula columns match v0.2", {
  out <- .p2_ordered(tibble::as_tibble(.p2_fixture032_weighted()))
  ref <- .p2_ordered(.p2_fixture032()$weighted_seam_v2)
  common <- c(
    "provider", "profile", "osm_snapshot_date", "acs_year",
    "weight_method", "estimand_family", "weight_basis",
    "moe_formula_requested", "moe_formula_effective", "moe_fallback",
    "moe_fallback_reason", "failure_origin",
    "weight_uncertainty_propagated"
  )
  expect_equal(out[common], ref[common], ignore_attr = TRUE)
})

test_that("T-P2-COVERAGE-01 provider validator branches stay classed", {
  iso_provider <- .p2_read_extdata("legacy_2025_isochrones.rds")[1, ]
  iso_provider$provider <- "not_a_provider"
  expect_error(.validate_iso_schema(iso_provider),
               class = "catchmentACS_error_schema",
               regexp = "provider")

  iso_requested <- .p2_read_extdata("legacy_2025_isochrones.rds")[1, ]
  iso_requested$provider_requested <- "not_a_provider"
  expect_error(.validate_iso_schema(iso_requested),
               class = "catchmentACS_error_schema",
               regexp = "provider_requested")
})

test_that("T-P2-COVERAGE-02 geometry-type validator branch stays classed", {
  iso <- .p2_read_extdata("legacy_2025_isochrones.rds")[1, ]
  sf::st_geometry(iso) <- sf::st_sfc(sf::st_point(c(-86.8, 33.5)),
                                     crs = 4326)
  expect_error(.validate_iso_schema(iso),
               class = "catchmentACS_error_schema",
               regexp = "POLYGON")
})

test_that("T-P2-COVERAGE-03 cacs_validate_iso reports site_id type and empties", {
  iso_type <- .p2_read_extdata("legacy_2025_isochrones.rds")[1, ]
  iso_type$site_id <- 1L
  issues_type <- cacs_validate_iso(iso_type)
  expect_true("iso_site_id_type" %in% issues_type$check)

  iso_empty <- .p2_read_extdata("legacy_2025_isochrones.rds")[1, ]
  iso_empty$site_id <- ""
  issues_empty <- cacs_validate_iso(iso_empty)
  expect_true("iso_site_id_empty" %in% issues_empty$check)
})

test_that("T-P2-COVERAGE-04 rings helper rejects boundary violations", {
  x_zero <- load_replay_fixture("annulus_input")
  x_zero$isomax[[1L]] <- 0L
  expect_error(cacs_rings_to_cumulative(x_zero),
               class = "catchmentACS_error_schema",
               regexp = "isomax")

  x_reversed <- load_replay_fixture("annulus_input")
  x_reversed$isomax[[1L]] <- x_reversed$isomin[[1L]]
  expect_error(cacs_rings_to_cumulative(x_reversed),
               class = "catchmentACS_error_schema",
               regexp = "isomax")
})
