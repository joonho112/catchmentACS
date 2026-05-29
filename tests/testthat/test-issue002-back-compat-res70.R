# ===========================================================================
# test-issue002-back-compat-res70.R
#
# v0.4 plan Step 4.3 — back-compat regression: explicit res = 70L calls
# preserve v0.3 behavior (no demo-budget downgrade, no message emit).
# ===========================================================================

test_that("ISSUE002-BC-01 explicit res=70L on demo: passthrough silent", {
  out <- catchmentACS:::.cacs_osrm_resolve_demo_res(70L, "demo", TRUE)
  expect_identical(out$res_effective, 70L)
  expect_false(out$downgraded)
  expect_false(out$emit_msg)
})

test_that("ISSUE002-BC-02 explicit res=70L on docker: passthrough silent", {
  out <- catchmentACS:::.cacs_osrm_resolve_demo_res(70L, "docker", TRUE)
  expect_identical(out$res_effective, 70L)
  expect_false(out$emit_msg)
})

test_that("ISSUE002-BC-03 explicit res=50L (v0.2 compat) on demo: passthrough silent", {
  out <- catchmentACS:::.cacs_osrm_resolve_demo_res(50L, "demo", TRUE)
  expect_identical(out$res_effective, 50L)
  expect_false(out$emit_msg)
})

test_that("ISSUE002-BC-04 protection disabled: omitted + demo → v0.3 70L default", {
  out <- catchmentACS:::.cacs_osrm_resolve_demo_res(NULL, "demo", FALSE)
  expect_identical(out$res_effective, 70L)
  expect_false(out$emit_msg)
})

test_that("ISSUE002-BC-05 docker mode: 70L irrespective of protect option", {
  for (protect in c(TRUE, FALSE)) {
    out <- catchmentACS:::.cacs_osrm_resolve_demo_res(NULL, "docker", protect)
    expect_identical(out$res_effective, 70L)
    expect_false(out$emit_msg)
  }
})
