# ============================================================================
# Unit tests for cacs_describe() provenance helper (v0.3 Step 4.3).
# ============================================================================


.mk_describe_fixture <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)

    iso <- readRDS(system.file("extdata", "legacy_2025_isochrones.rds",
                               package = "catchmentACS"))
    sites <- readRDS(system.file("extdata", "legacy_2025_sites.rds",
                                 package = "catchmentACS"))
    acs <- readRDS(system.file("extdata", "sample_alabama_subset.rds",
                               package = "catchmentACS"))

    iso_sub <- iso[iso$site_id == "AL_SITE_01" &
                     iso$drive_time_min == 5L, ]
    sites_sub <- sites[sites$site_id == "AL_SITE_01", ]

    weighted <- suppressMessages(
      cacs_intersect_weight(iso_sub, acs, verbose = FALSE)
    )
    propagated <- cacs_propagate_moe(weighted)
    derived <- suppressWarnings(cacs_derive_rates(propagated))
    run <- suppressWarnings(suppressMessages(
      cacs_run(
        sites = sites_sub,
        state = "AL",
        year = 2023,
        drive_times = 5L,
        variables = unname(cacs_acs_default_vars),
        provider = "osrm",
        weight_method = "area",
        precomputed_isochrones = iso_sub,
        acs = acs,
        output = "long",
        verbose = FALSE
      )
    ))

    cached <<- list(
      iso = iso_sub,
      acs = acs,
      weighted = weighted,
      propagated = propagated,
      derived = derived,
      run = run
    )
    cached
  }
})


.describe_quietly <- function(x) {
  suppressMessages(cacs_describe(x))
}


test_that("T-DESC-01 cacs_describe is exported and S3 methods are registered", {
  expect_true("cacs_describe" %in% getNamespaceExports("catchmentACS"))
  expect_true(is.function(cacs_describe))
  expect_true("cacs_describe.cacs_run_result" %in% methods("cacs_describe"))
  expect_true("cacs_describe.tbl_df" %in% methods("cacs_describe"))
  expect_true("cacs_describe.sf" %in% methods("cacs_describe"))
  expect_true("cacs_describe.list" %in% methods("cacs_describe"))
  expect_true("print.cacs_description" %in% methods("print"))
})


test_that("T-DESC-02 cacs_run_result describe returns structured metadata", {
  fx <- .mk_describe_fixture()
  before_class <- class(fx$run)
  before_meta_names <- names(attr(fx$run, "cacs_run_result_metadata"))

  desc <- .describe_quietly(fx$run)

  expect_s3_class(desc, "cacs_description")
  expect_equal(desc$object_type, "cacs_run_result")
  expect_true("cacs_run_result_metadata" %in% desc$attributes_present)
  expect_setequal(names(desc$data$metadata), before_meta_names)
  expect_equal(class(fx$run), before_class)
})


test_that("T-DESC-03 cacs_run_result sections expose run and rate provenance", {
  fx <- .mk_describe_fixture()
  desc <- .describe_quietly(fx$run)

  expect_true("Run" %in% names(desc$sections))
  expect_true(any(grepl("Provider/profile: osrm", desc$sections$Run,
                        fixed = TRUE)))
  expect_true(any(grepl("Drive times: 5", desc$sections$Run,
                        fixed = TRUE)))
  expect_true("Rates" %in% names(desc$sections))
  expect_true(any(grepl("Carrier-missing rate rows:",
                        desc$sections$Rates, fixed = TRUE)))
  expect_false(is.null(desc$data$rate_provenance))
})


test_that("T-DESC-04 weighted seam describe reports carrier table status", {
  fx <- .mk_describe_fixture()
  desc <- .describe_quietly(fx$weighted)

  expect_equal(desc$object_type, "weighted_seam")
  expect_true("cacs_aggregation_carriers" %in% desc$attributes_present)
  expect_true(desc$data$carrier_rows > 0L)
  expect_true(any(grepl("Carrier rows:", desc$sections[["Carrier Table"]],
                        fixed = TRUE)))
  expect_true(any(grepl("Weight method:", desc$sections$Aggregation,
                        fixed = TRUE)))
})


test_that("T-DESC-05 propagated seam describe reports MOE provenance and carriers", {
  fx <- .mk_describe_fixture()
  desc <- .describe_quietly(fx$propagated)

  expect_equal(desc$object_type, "propagated_seam")
  expect_true("cacs_moe_provenance" %in% desc$attributes_present)
  expect_true("cacs_aggregation_carriers" %in% desc$attributes_present)
  expect_false(is.null(desc$data$moe_provenance))
  expect_true(any(grepl("Confidence level:", desc$sections[["MOE Propagation"]],
                        fixed = TRUE)))
})


test_that("T-DESC-06 derived rate tibble describe reports consumed carriers", {
  fx <- .mk_describe_fixture()
  desc <- .describe_quietly(fx$derived)

  expect_equal(desc$object_type, "derived_rates")
  expect_true("cacs_rate_provenance" %in% desc$attributes_present)
  expect_false("cacs_aggregation_carriers" %in% desc$attributes_present)
  expect_true(any(grepl("Carrier attribute: absent or already consumed",
                        desc$sections[["Carrier Table"]], fixed = TRUE)))
  expect_true(any(grepl("Rate rows:", desc$sections[["Rate Derivation"]],
                        fixed = TRUE)))
})


test_that("T-DESC-07 isochrone sf describe distinguishes spatial provenance", {
  fx <- .mk_describe_fixture()
  desc <- .describe_quietly(fx$iso)

  expect_equal(desc$object_type, "isochrone_sf")
  expect_true("Spatial Provenance" %in% names(desc$sections))
  expect_true(any(grepl("EPSG:", desc$sections$Object, fixed = TRUE)))
  expect_true(any(grepl("Ring topology:", desc$sections[["Spatial Provenance"]],
                        fixed = TRUE)))
})


test_that("T-DESC-08 ACS sf describe is not misclassified as isochrone", {
  fx <- .mk_describe_fixture()
  desc <- .describe_quietly(fx$acs)

  expect_equal(desc$object_type, "acs_sf")
  expect_true(any(grepl("ACS variables:", desc$sections[["Spatial Provenance"]],
                        fixed = TRUE)))
})


test_that("T-DESC-09 default method handles plain objects without provenance", {
  desc <- .describe_quietly(data.frame(x = 1:3))

  expect_s3_class(desc, "cacs_description")
  expect_equal(desc$object_type, "unknown")
  expect_true(any(grepl("No catchmentACS provenance available",
                        desc$sections$Object, fixed = TRUE)))
})


test_that("T-DESC-10 missing run metadata degrades gracefully", {
  fx <- .mk_describe_fixture()
  run_no_meta <- fx$run
  attr(run_no_meta, "cacs_run_result_metadata") <- NULL

  desc <- .describe_quietly(run_no_meta)

  expect_equal(desc$object_type, "cacs_run_result")
  expect_false("cacs_run_result_metadata" %in% desc$attributes_present)
  expect_true(any(grepl("Provider/profile:", desc$sections$Run,
                        fixed = TRUE)))
})


test_that("T-DESC-11 output='both' list delegates to its long branch", {
  fx <- .mk_describe_fixture()
  both <- list(long = fx$run, list_column = tibble::tibble())

  desc <- .describe_quietly(both)

  expect_equal(desc$object_type, "cacs_run_result")
  expect_true("cacs_run_result_metadata" %in% desc$attributes_present)
})


test_that("T-DESC-12 rate audit counter is surfaced when attached", {
  fx <- .mk_describe_fixture()
  derived <- fx$derived
  prov <- attr(derived, "cacs_rate_provenance")
  prov$n_rate_audit_out_of_bounds <- 1L
  attr(derived, "cacs_rate_provenance") <- prov

  desc <- .describe_quietly(derived)

  expect_true(any(grepl("Out-of-range audit rows: 1",
                        desc$sections[["Rate Derivation"]], fixed = TRUE)))
})

