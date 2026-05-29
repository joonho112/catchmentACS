# ============================================================================
# Unit test: area-weighted scalar / median proxy estimates are a PER-VARIABLE
# normalized-mean-weighted average (regression guard for the mean-weight
# deflation bug).
#
# Bug history: `.compute_family_aggregation()` computed
#   mean_wt = int_area_m2 / sum(int_area_m2)
# under group_by(site_id, drive_time_min) only. Because the working table is in
# long form (one row per site x drive_time x GEOID x VARIABLE), that denominator
# summed each tract's intersection area once PER VARIABLE, inflating it by
# n_variables and deflating every `area_weighted_scalar_proxy` and
# `median_proxy` ESTIMATE by 1 / n_variables. The fix adds `variable` to the
# group_by so the mean weights sum to one within each (site, drive_time,
# variable) cell.
#
# Contract locked here:
#   - sum of the per-variable mean weights == 1,
#   - the proxy estimate == sum(mean_wt * tract_value), and
#   - the proxy estimate lies within [min, max] of the contributing tract
#     values (a normalized-mean weighting is a convex combination).
# Offline: uses the bundled legacy_2025 / sample_alabama_subset fixtures.
# ============================================================================

test_that("scalar/median proxy estimate is a per-variable weighted mean (no n_variables deflation)", {
  skip_if_not_installed("sf")

  withr::local_options(catchmentACS.progress = "off")
  # Compute fresh (do not let a stale intersect-cache entry mask the result).
  old_cache <- getOption("catchmentACS.cache_enabled", NULL)
  withr::defer(options(catchmentACS.cache_enabled = old_cache))
  cacs_set_cache(FALSE, scope = "session")

  iso <- readRDS(system.file("extdata", "legacy_2025_isochrones.rds",
                             package = "catchmentACS"))
  acs <- readRDS(system.file("extdata", "sample_alabama_subset.rds",
                             package = "catchmentACS"))

  proxy_var <- "B19013_001"   # median household income -> median_proxy family
  acs_x <- sf::st_drop_geometry(acs)
  acs_x <- acs_x[acs_x$variable == proxy_var, c("GEOID", "estimate")]
  names(acs_x)[2] <- "X"

  checked <- FALSE
  for (s in unique(iso$site_id)) {
    iso1 <- iso[iso$site_id == s & iso$drive_time_min == 15L, ]
    if (nrow(iso1) == 0L) next

    ag <- cacs_intersect_weight(
      iso_sf = iso1, acs_sf = acs, weight_method = "area",
      keep_tract_audit = TRUE, verbose = FALSE
    )
    pr <- ag[!is.na(ag$variable) & ag$variable == proxy_var, , drop = FALSE]
    if (nrow(pr) != 1L || is.na(pr$estimate)) next

    au <- attr(ag, "cacs_tract_audit")
    d  <- merge(unique(au[, c("GEOID", "int_area_m2")]), acs_x, by = "GEOID")
    d  <- d[!is.na(d$X), , drop = FALSE]
    if (nrow(d) < 2L) next   # need >= 2 contributing tracts for a meaningful blend

    mean_wt <- d$int_area_m2 / sum(d$int_area_m2)
    hand    <- sum(mean_wt * d$X)

    expect_equal(sum(mean_wt), 1)                       # weights sum to one
    expect_equal(pr$estimate, hand, tolerance = 1e-6)   # == per-variable weighted mean
    expect_gte(pr$estimate, min(d$X))                   # convex combination: within range
    expect_lte(pr$estimate, max(d$X))
    checked <- TRUE
    break
  }

  expect_true(
    checked,
    info = "expected a complete multi-tract median_proxy cell in the bundled fixtures"
  )
})
