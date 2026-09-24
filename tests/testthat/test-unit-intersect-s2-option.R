# cacs_intersect_weight() turns off s2 (spherical geometry in sf) while it runs
# and then puts the option sf_use_s2 back as it was, also when the option was
# not set. Made-up tracts; no network.

test_that("IW-S2-01 cacs_intersect_weight() turns s2 off while it runs and leaves the option sf_use_s2 as it was", {
  x <- helper_acs_two_tracts()
  orig_transform <- .safe_transform_5070
  for (before in list(NULL, TRUE, FALSE)) {
    s2_inside <- logical()
    testthat::local_mocked_bindings(
      .safe_transform_5070 = function(...) {
        s2_inside <<- c(s2_inside, sf::sf_use_s2())
        orig_transform(...)
      }
    )
    withr::with_options(
      list(sf_use_s2 = before, catchmentACS.cache_enabled = FALSE),
      {
        out <- suppressMessages(cacs_intersect_weight(x$iso, x$acs, verbose = FALSE))
        expect_identical(getOption("sf_use_s2"), before,
                         info = paste("option before the call:", format(before)))
      }
    )
    expect_s3_class(out, "tbl_df")
    expect_true(length(s2_inside) > 0L)
    expect_false(any(s2_inside))
  }
})
