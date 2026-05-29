# Replay fixture helpers for v0.3 regression tests.

replay_fixture_names <- function() {
  c("031_3site_anchors", "032_3site_fresh", "cache_poison", "annulus_input")
}

replay_fixture_path <- function(name) {
  file_name <- paste0("fixture_", name, ".rds")
  installed <- system.file("testdata", file_name, package = "catchmentACS")
  if (nzchar(installed) && file.exists(installed)) {
    return(installed)
  }

  local_candidates <- c(
    file.path("inst", "testdata", file_name),
    testthat::test_path("..", "..", "inst", "testdata", file_name)
  )
  for (local in local_candidates) {
    if (file.exists(local)) {
      return(normalizePath(local, winslash = "/", mustWork = TRUE))
    }
  }

  ""
}

skip_if_no_replay_fixture <- function(name, path_fun = replay_fixture_path) {
  path <- path_fun(name)
  if (!nzchar(path)) {
    testthat::skip(paste0("Replay fixture not installed: ", name))
  }
  invisible(TRUE)
}

load_replay_fixture <- function(name, path_fun = replay_fixture_path) {
  skip_if_no_replay_fixture(name, path_fun = path_fun)
  path <- path_fun(name)
  readRDS(path)
}

skip_if_no_replay_fixtures <- function(names = replay_fixture_names(),
                                       path_fun = replay_fixture_path) {
  for (nm in names) {
    skip_if_no_replay_fixture(nm, path_fun = path_fun)
  }
  invisible(TRUE)
}
