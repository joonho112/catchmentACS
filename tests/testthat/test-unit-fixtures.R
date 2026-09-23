# ============================================================================
# Unit smoke tests for helper-fixtures.R loaders.
# ============================================================================

test_that("T-FIXTURE-01 synthetic fixture loaders resolve bundled RDS files", {
  loaders <- list(
    helper_load_synthetic_3site_4tract,
    helper_load_synthetic_single_site,
    helper_load_synthetic_2tract,
    helper_load_synthetic_moe_C1_success,
    helper_load_synthetic_moe_C1_to_C2,
    helper_load_synthetic_moe_carriers,
    helper_load_al_10_site_sample
  )

  for (loader in loaders) {
    obj <- loader()
    expect_false(is.null(obj))
  }
})
