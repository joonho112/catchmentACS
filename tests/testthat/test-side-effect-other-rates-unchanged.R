# Side-effect locks: SSI-specific changes must not perturb other rates.

test_that("P4-SIDE-01 changing SSI numerator changes only ssi_rate", {
  base <- .phase4_rate_estimates(.phase4_derive())
  changed <- .phase4_rate_estimates(
    .phase4_derive(.phase4_rate_values(B19056_002 = 50))
  )
  expect_equal(changed[setdiff(names(base), "ssi_rate")],
               base[setdiff(names(base), "ssi_rate")])
  expect_false(isTRUE(all.equal(changed[["ssi_rate"]], base[["ssi_rate"]])))
})

test_that("P4-SIDE-02 changing SSI denominator changes only ssi_rate", {
  base <- .phase4_rate_estimates(.phase4_derive())
  changed <- .phase4_rate_estimates(
    .phase4_derive(.phase4_rate_values(B19056_001 = 250))
  )
  expect_equal(changed[setdiff(names(base), "ssi_rate")],
               base[setdiff(names(base), "ssi_rate")])
  expect_false(isTRUE(all.equal(changed[["ssi_rate"]], base[["ssi_rate"]])))
})

test_that("P4-SIDE-03 changing old B11001 divisor affects no sanctioned rate", {
  base <- .phase4_rate_estimates(.phase4_derive())
  changed <- .phase4_rate_estimates(
    .phase4_derive(.phase4_rate_values(B11001_001 = 1))
  )
  expect_equal(changed, base)
})

test_that("P4-SIDE-04 missing SSI numerator leaves other rate estimates unchanged", {
  base <- .phase4_rate_estimates(.phase4_derive())
  missing <- .phase4_rate_estimates(
    .phase4_derive_with_warnings(drop = "B19056_002")$out
  )
  keep <- setdiff(names(base), "ssi_rate")
  expect_equal(missing[keep], base[keep])
  expect_true(is.na(missing[["ssi_rate"]]))
})

test_that("P4-SIDE-05 missing LFP denominator leaves other rate estimates unchanged", {
  base <- .phase4_rate_estimates(.phase4_derive())
  missing <- .phase4_rate_estimates(
    .phase4_derive_with_warnings(drop = "B23025_001")$out
  )
  keep <- setdiff(names(base), "labor_force_participation")
  expect_equal(missing[keep], base[keep])
  expect_true(is.na(missing[["labor_force_participation"]]))
})

test_that("P4-SIDE-06 changing SSI carriers leaves other rate MOEs unchanged", {
  base <- .phase4_derive()
  changed <- .phase4_derive(.phase4_rate_values(B19056_002 = 50))
  keep <- setdiff(names(cacs_acs_default_rates), "ssi_rate")
  b <- base[base$variable %in% keep & base$estimand_family == "derived_rate", ]
  c <- changed[changed$variable %in% keep &
                 changed$estimand_family == "derived_rate", ]
  expect_equal(stats::setNames(c$moe, c$variable),
               stats::setNames(b$moe, b$variable))
})

test_that("P4-SIDE-07 changing SSI carriers keeps rate row cardinality", {
  base <- .phase4_derive()
  changed <- .phase4_derive(.phase4_rate_values(B19056_002 = 50))
  expect_equal(sum(base$estimand_family == "derived_rate"),
               sum(changed$estimand_family == "derived_rate"))
})

test_that("P4-SIDE-08 changing SSI carriers keeps non-SSI failure origins none", {
  changed <- .phase4_derive(.phase4_rate_values(B19056_002 = 50))
  keep <- setdiff(names(cacs_acs_default_rates), "ssi_rate")
  rows <- changed[changed$variable %in% keep &
                    changed$estimand_family == "derived_rate", ]
  expect_true(all(rows$failure_origin == "none"))
})

test_that("P4-SIDE-09 audit warnings do not mutate returned estimates", {
  vals <- .phase4_rate_values(B19056_002 = 300)
  no_audit <- .phase4_rate_estimates(.phase4_derive(vals, audit = FALSE))
  with_audit <- .phase4_rate_estimates(
    .phase4_derive_with_warnings(vals, audit = TRUE)$out
  )
  expect_equal(with_audit, no_audit)
})

test_that("P4-SIDE-10 cacs_describe does not mutate a derived output", {
  out <- .phase4_derive()
  before_attrs <- names(attributes(out))
  suppressMessages(cacs_describe(out))
  expect_equal(names(attributes(out)), before_attrs)
})

