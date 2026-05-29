# ============================================================================
# Unit tests for R/cli.R wrapper factories.
# 7 cases:
#   - Condition class hierarchy
#   - 3-line format rendering
#   - Parent condition chaining
#   - cacs_phase attribute keying for §24.7
#   - Re-emission through outer withCallingHandlers
#   - Credential-family dispatch
#   - Cache-info message class
# ============================================================================

test_that("T-CLI-01 .cli_abort_schema attaches full class chain", {
  expect_error(
    .cli_abort_schema("bad input"),
    class = "catchmentACS_error_schema"
  )
  expect_error(
    .cli_abort_schema("bad input"),
    class = "catchmentACS_error"
  )
  expect_error(
    .cli_abort_schema("bad input"),
    class = "catchmentACS_condition"
  )
})

test_that("T-CLI-02 3-line format (head / x / i) renders all rows", {
  msg <- tryCatch(
    .cli_abort_schema(c(
      "{.arg sites} must be {.cls sf}.",
      "x" = "Got {.cls character}.",
      "i" = "See {.help cacs_isochrone}."
    )),
    error = function(e) conditionMessage(e)
  )
  expect_match(msg, "must be")
  expect_match(msg, "Got")
  expect_match(msg, "See")
})

test_that("T-CLI-03 parent condition is chained for re-attribution", {
  inner <- tryCatch(stop("GEOS topology"), error = function(e) e)
  outer <- tryCatch(
    .cli_abort_geometry(
      "Repair failed in batch.",
      parent = inner
    ),
    error = function(e) e
  )
  expect_s3_class(outer, "catchmentACS_error_geometry")
  expect_s3_class(outer$parent, "simpleError")
  expect_match(conditionMessage(outer$parent), "GEOS topology")
})

test_that("T-CLI-04 warnings carry cacs_phase attribute for §24.7 keying", {
  cond <- tryCatch(
    .cli_warn_runtime("test", phase = "intersect_weight"),
    catchmentACS_warning = function(w) w
  )
  expect_s3_class(cond, "catchmentACS_warning_runtime")
  expect_equal(cond$cacs_phase, "intersect_weight")
})

test_that("T-CLI-05 condition is re-emittable by outer withCallingHandlers", {
  captured <- list()
  withCallingHandlers(
    {
      .cli_warn_provenance("provenance issue", phase = "acs_prefetch")
      .cli_warn_runtime("runtime issue",       phase = "intersect_weight")
    },
    catchmentACS_warning = function(w) {
      key <- w$cacs_phase
      if (is.na(key) || is.null(key)) key <- "orchestrator"
      captured[[key]] <<- c(captured[[key]], list(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_named(captured, c("acs_prefetch", "intersect_weight"),
               ignore.order = TRUE)
  expect_s3_class(captured$acs_prefetch[[1L]],
                  "catchmentACS_warning_provenance")
  expect_s3_class(captured$intersect_weight[[1L]],
                  "catchmentACS_warning_runtime")
})

test_that("T-CLI-06 .cli_abort_credential dispatches as credential family", {
  expect_error(
    .cli_abort_credential(c(
      "Missing token.",
      "x" = "ORS_API_KEY unset.",
      "i" = "Use {.code Sys.setenv}."
    )),
    class = "catchmentACS_error_credential"
  )
})

test_that("T-CLI-07 .cli_inform_cache emits message-class condition", {
  msg <- tryCatch(
    {
      .cli_inform_cache("Cache hit", phase = "intersect_weight")
      NULL
    },
    catchmentACS_message = function(m) m
  )
  expect_s3_class(msg, "catchmentACS_message_cache")
  expect_equal(msg$cacs_phase, "intersect_weight")
})

test_that("T-CLI-08 .cacs_emit informs through rlang with cli formatting", {
  env <- list2env(list(value = 42L), parent = emptyenv())
  msg <- tryCatch(
    {
      .cacs_emit(
        level = "inform",
        family = "progress",
        message = c("Progress {value}", "i" = "Still catchable."),
        phase = "unit-phase",
        .envir = env
      )
      NULL
    },
    catchmentACS_message_progress = function(m) m
  )

  expect_s3_class(msg, "catchmentACS_message_progress")
  expect_equal(msg$cacs_phase, "unit-phase")
  expect_match(conditionMessage(msg), "Progress 42")
  expect_match(conditionMessage(msg), "Still catchable")
})

test_that("T-CLI-09 .cacs_emit warns through rlang with phase metadata", {
  env <- list2env(list(value = "x"), parent = emptyenv())
  captured <- NULL

  withCallingHandlers(
    .cacs_emit(
      level = "warn",
      family = "runtime",
      message = c("Runtime issue {value}", "i" = "Muffle restart works."),
      phase = "runtime-phase",
      .envir = env
    ),
    catchmentACS_warning_runtime = function(w) {
      captured <<- w
      invokeRestart("muffleWarning")
    }
  )

  expect_s3_class(captured, "catchmentACS_warning_runtime")
  expect_equal(captured$cacs_phase, "runtime-phase")
  expect_match(conditionMessage(captured), "Runtime issue x")
  expect_match(conditionMessage(captured), "Muffle restart works")
})

test_that("T-CLI-10 .cacs_emit abort preserves cli bullets and parent", {
  inner <- tryCatch(stop("inner failure"), error = function(e) e)
  env <- list2env(list(cls = "numeric"), parent = emptyenv())
  err <- tryCatch(
    .cacs_emit(
      level = "abort",
      family = "schema",
      message = c("{.arg x} is invalid.", "x" = "Got {.cls {cls}}."),
      parent = inner,
      .envir = env
    ),
    error = function(e) e
  )

  expect_s3_class(err, "catchmentACS_error_schema")
  expect_s3_class(err$parent, "simpleError")
  expect_match(conditionMessage(err), "x")
  expect_match(conditionMessage(err), "numeric")
})

test_that("T-CLI-11 .warn_skip_water_tract uses standardized warning path", {
  warn <- tryCatch(
    .warn_skip_water_tract(c("01001009900", "01003009900")),
    catchmentACS_warning_geometry_skip = function(w) w
  )

  expect_s3_class(warn, "catchmentACS_warning_geometry_skip")
  expect_s3_class(warn, "catchmentACS_warning")
  expect_match(conditionMessage(warn), "Skipping 2 tract")
})
