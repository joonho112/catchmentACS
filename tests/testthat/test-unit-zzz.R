# ============================================================================
# Unit tests for package load/attach hooks.
# ============================================================================

test_that(".onLoad seeds option defaults without clobbering user values", {
  old <- options(
    catchmentACS.verbose = NULL,
    cacs.return_se = NULL
  )
  withr::defer(options(old))

  # Use the namespace-qualified internal lookup so the test works under both
  # `devtools::test()` (which uses devtools::load_all) and `R CMD check`
  # (which uses the installed-package context).
  catchmentACS:::.onLoad(tempdir(), "catchmentACS")
  expect_true(getOption("catchmentACS.verbose"))
  expect_false(getOption("cacs.return_se"))

  options(catchmentACS.verbose = FALSE, cacs.return_se = TRUE)
  catchmentACS:::.onLoad(tempdir(), "catchmentACS")
  expect_false(getOption("catchmentACS.verbose"))
  expect_true(getOption("cacs.return_se"))
})

test_that(".onAttach remains silent", {
  expect_silent(catchmentACS:::.onAttach(tempdir(), "catchmentACS"))
})

