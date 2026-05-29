# helper-fixtures.R — synthetic fixture loaders for test-*.R files
#
# Will host:
#   - helper_load_synthetic_3site_4tract() — §25.2 master fixture
#   - helper_load_synthetic_single_site() — T21-09 quarter overlap = 0.25 area_wt
#   - helper_load_synthetic_2tract() — T21-10 50%/30% overlaps
#   - helper_load_synthetic_moe_carriers() — T22-09/T22-10 known C1/C2 fixtures
#   - helper_load_al_10_site_sample() — Phase 3 integration light-weight
#
# Cross-ref: §16.1 (helper convention), §25.2 (7 synthetic fixtures catalogue).

helper_fixture_path <- function(name) {
  system.file("testdata", name, package = "catchmentACS", mustWork = TRUE)
}

helper_load_fixture <- function(name) {
  readRDS(helper_fixture_path(name))
}

helper_load_synthetic_3site_4tract <- function() {
  helper_load_fixture("synthetic_3site_4tract.rds")
}

helper_load_synthetic_single_site <- function() {
  helper_load_fixture("synthetic_single_site.rds")
}

helper_load_synthetic_2tract <- function() {
  helper_load_fixture("synthetic_2tract.rds")
}

helper_load_synthetic_moe_C1_success <- function() {
  helper_load_fixture("synthetic_moe_C1_success.rds")
}

helper_load_synthetic_moe_C1_to_C2 <- function() {
  helper_load_fixture("synthetic_moe_C1_to_C2.rds")
}

helper_load_synthetic_moe_carriers <- function() {
  helper_load_fixture("synthetic_moe_carriers.rds")
}

helper_load_al_10_site_sample <- function() {
  helper_load_fixture("al_10_site_sample.rds")
}
