# capture-conditions.R - cacs_capture_conditions() and its helpers: put the
# package's messages and warnings in a table instead of showing them.


#' Capture the package's messages and warnings in a table
#'
#' Evaluates an expression and returns a tibble listing the standardized
#' catchmentACS messages and warnings given while it runs, that is, those
#' with the class `catchmentACS_condition` (see [`catchmentACS-conditions`]).
#' The captured messages and warnings are not shown.
#'
#' The conditions are captured with `withCallingHandlers()`, so `expr` runs
#' to the end. Messages and warnings that are not captured, because they
#' come from other packages or do not match `classes`, are shown as usual.
#' Errors are not captured: an error in `expr` stops
#' `cacs_capture_conditions()`, and the conditions captured before it are
#' not returned.
#'
#' @param expr An expression to evaluate, such as a call to [cacs_run()].
#' @param classes A character vector of the classes to capture, or `NULL`
#'   (the default) for all messages and warnings of the package. Each
#'   element can be a full class name, such as
#'   `"catchmentACS_message_progress_summary"`, or `"message"` or
#'   `"warning"` for all messages or all warnings of the package. It can
#'   also be the part of a class name after
#'   `catchmentACS_message_`, `catchmentACS_warning_`, or
#'   `catchmentACS_error_`, such as `"water_tract_filter"` or `"progress"`.
#'   A name that matches no class captures nothing, without an error.
#' @param return_value A string giving what to return: `"conditions"` for
#'   the table of captured conditions, or `"both"` for a list with the value
#'   of `expr` as well. `NULL` (the default) uses the option
#'   `catchmentACS.capture_return_value`, which is `"conditions"` unless it
#'   has been changed.
#'
#' @return With `return_value = "conditions"`, a tibble with one row for each
#'   captured condition, in the order in which they were given, and these
#'   columns:
#'   \describe{
#'     \item{`class`}{The most specific class of the condition, such as
#'       `"catchmentACS_message_progress_summary"`.}
#'     \item{`message`}{The text of the condition, as returned by
#'       `conditionMessage()`.}
#'     \item{`phase`}{The step that gave the condition: `"acs"` for
#'       [cacs_acs_prefetch()], `"isochrone"` for [cacs_isochrone()],
#'       `"intersect"` for [cacs_intersect_weight()], `"moe"` for
#'       [cacs_propagate_moe()], `"rates"` for [cacs_derive_rates()], `"run"`
#'       for [cacs_run()] itself, and `"cache"` for [cacs_set_cache()] and
#'       [cacs_clear_cache()]. Messages and warnings about saved results have
#'       the value of their step (some of those about ACS data saved in test
#'       mode have `"acs_test"`; see `namespace_mode` in
#'       [cacs_get_cache_state()]). A progress line or summary line has the
#'       label of its step, such as `"Intersect+weight"` (see the "Progress
#'       messages" section of [cacs_run()]). The warning of
#'       [cacs_validate_osrm_endpoint()] has `"isochrone"`, and the message and
#'       warning of the maps about the nearest site have the name of the
#'       function, such as `"cacs_plot_site_rates"`. The warnings of
#'       [cacs_intersect_weight()] about repaired geometries and the message
#'       of [as_tibble.cacs_run_result()] have `NA`. The values are not the
#'       names of the elements of the `cacs_run_warnings` attribute of a
#'       [cacs_run()] result.}
#'     \item{`timestamp`}{The time at which the condition was given, as a
#'       date-time in UTC.}
#'     \item{`call`}{The call in which the condition was given, as text, or
#'       `NA` when there is none. For a condition given while [cacs_run()]
#'       runs a step, the text can include the data passed to the step and be
#'       very long.}
#'   }
#'   With `return_value = "both"`, a list with two elements: `result`, the
#'   value of `expr`, and `conditions`, the tibble above.
#'
#' @section Keeping the value of the expression:
#' With `return_value = "conditions"`, the value of `expr` is not returned.
#' An assignment made with `<-` inside `expr` does not keep it either,
#' because `expr` is evaluated in a new environment:
#' `cacs_capture_conditions(acs <- cacs_acs_prefetch("AL"))` leaves `acs` as
#' it was. With `return_value = "both"`, the value is kept as the `result`
#' element of the list. An assignment made with `<<-` inside `expr` also
#' keeps it, in the first variable of that name found by searching from the
#' environment in which `cacs_capture_conditions()` is called (see
#' [assignOps][base::assignOps]).
#'
#' @examples
#' # Turn the cache off while this example runs (see ?cacs_set_cache).
#' old <- options(catchmentACS.cache_enabled = FALSE)
#'
#' # Example data bundled with the package: the drive-time areas are circles
#' # with a radius of 1 km per minute, and the ACS data are made up.
#' library(sf)
#' iso <- readRDS(system.file("extdata", "legacy_2025_isochrones.rds",
#'                            package = "catchmentACS"))
#' acs <- readRDS(system.file("extdata", "sample_alabama_subset.rds",
#'                            package = "catchmentACS"))
#' iso_07 <- iso[iso$site_id == "AL_SITE_07" & iso$drive_time_min == 10, ]
#' site_07 <- data.frame(site_id = "AL_SITE_07", lon = -85.365, lat = 31.655)
#'
#' # The messages of a run, captured instead of shown; out$result is the
#' # result of cacs_run()
#' out <- cacs_capture_conditions(
#'   cacs_run(site_07, state = "AL", drive_times = 10,
#'            precomputed_isochrones = iso_07, acs = acs),
#'   return_value = "both"
#' )
#' out$conditions[, c("class", "message")]
#'
#' # Only the warnings: the range check on rates, turned on here, warns
#' # about two rates of the made-up data
#' old_audit <- options(catchmentACS.audit_rates = TRUE)
#' warned <- cacs_capture_conditions(
#'   cacs_run(site_07, state = "AL", drive_times = 10,
#'            precomputed_isochrones = iso_07, acs = acs, verbose = FALSE),
#'   classes = "warning"
#' )
#' warned[, c("class", "message")]
#' options(old_audit)
#'
#' options(old)
#'
#' # The download from the Census Bureau needs a Census API key.
#' \dontrun{
#' out <- cacs_capture_conditions(
#'   cacs_acs_prefetch(state = "AL", year = 2023),
#'   classes = "water_tract_filter",
#'   return_value = "both"
#' )
#' al <- out$result
#' out$conditions$message
#' }
#' @family validation and conditions
#' @export
cacs_capture_conditions <- function(expr, classes = NULL,
                                    return_value = NULL) {
  quo <- rlang::enquo(expr)
  targets <- .cacs_normalize_capture_classes(classes)

  if (is.null(return_value)) {
    return_value <- getOption("catchmentACS.capture_return_value",
                              "conditions")
  }
  return_value <- match.arg(return_value, c("conditions", "both"))

  if (identical(return_value, "both")) {
    return(.cacs_capture_conditions_with_value(quo, targets))
  }

  # Below: the same capture without keeping the value of `expr`.
  # The handler adds each record to an environment, which it can change in
  # place.
  captured <- new.env(parent = emptyenv())
  captured$records <- list()

  handler <- function(cnd) {
    if (!.cacs_condition_matches(cnd, targets)) {
      return(invisible(NULL))
    }
    captured$records[[length(captured$records) + 1L]] <- .cacs_condition_record(cnd)
    .cacs_try_muffle_condition(cnd)
    invisible(NULL)
  }

  withCallingHandlers(
    rlang::eval_tidy(quo),
    catchmentACS_condition = handler
  )

  .cacs_condition_records_tbl(captured$records)
}


#' Capture the conditions and keep the value of the expression
#'
#' Used by `cacs_capture_conditions(return_value = "both")`. Each matching
#' message or warning is recorded and muffled, as in the other path, and an
#' error in `expr` is not caught. The value of `expr` is returned as well; the
#' other path drops it, and the user reads it back as `out$result`.
#'
#' @param quo A quosure of the user's expression, from `rlang::enquo()`.
#' @param targets The class names to capture, from
#'   `.cacs_normalize_capture_classes()`, or `NULL` for any
#'   `catchmentACS_condition`.
#' @return A list with `result`, the value of `expr`, and `conditions`, the
#'   tibble described on the help page (`class`, `message`, `phase`,
#'   `timestamp`, `call`).
#' @keywords internal
#' @noRd
.cacs_capture_conditions_with_value <- function(quo, targets) {
  # The handler adds each record to an environment, which it can change in
  # place.
  captured <- new.env(parent = emptyenv())
  captured$records <- list()

  handler <- function(cnd) {
    if (!.cacs_condition_matches(cnd, targets)) {
      return(invisible(NULL))
    }
    captured$records[[length(captured$records) + 1L]] <- .cacs_condition_record(cnd)
    .cacs_try_muffle_condition(cnd)
    invisible(NULL)
  }

  result <- withCallingHandlers(
    rlang::eval_tidy(quo),
    catchmentACS_condition = handler
  )

  list(
    result     = result,
    conditions = .cacs_condition_records_tbl(captured$records)
  )
}


#' The condition classes the package gives
#'
#' With `include_parents = TRUE` the list also holds the classes that name a
#' group, such as `catchmentACS_message` and `catchmentACS_warning_cache`,
#' which is what `classes = NULL` needs in order to catch a condition that
#' carries only a group class.
#'
#' @keywords internal
#' @noRd
.cacs_known_classes <- function(include_parents = FALSE) {
  leaf <- c(
    "catchmentACS_message_water_tract_filter",
    "catchmentACS_message_progress_tick",
    "catchmentACS_message_progress_summary",
    "catchmentACS_message_res_default_changed",
    "catchmentACS_message_demo_budget_protected",
    "catchmentACS_message_rate_first_changed",
    "catchmentACS_message_listcol_iso_filled",
    "catchmentACS_message_perf_fix_applied",
    "catchmentACS_message_resolve_site",
    "catchmentACS_message_cache",
    "catchmentACS_message_cache_legacy_invalidated",
    "catchmentACS_message_cache_fingerprint_mismatch",
    "catchmentACS_message_cache_enabled_announce",
    "catchmentACS_warning_cache_stale_suspect",
    "catchmentACS_warning_provider_quota_exhausted",
    "catchmentACS_warning_rate_out_of_range",
    "catchmentACS_warning_carrier_missing",
    "catchmentACS_warning_resolve_site_distant",
    "catchmentACS_warning_geometry_skip",
    "catchmentACS_warning_runtime",
    "catchmentACS_warning_provenance",
    "catchmentACS_warning_variable",
    "catchmentACS_warning_partial",
    "catchmentACS_error_annulus_input",
    # The two names without the package prefix are older names carried only by
    # the drive-time band error (`.ANNULUS_ERROR_CLASSES`); an error raised
    # anywhere else has `catchmentACS_error_schema` but not `cacs_error_schema`.
    "cacs_error_annulus_input",
    "cacs_error_schema",
    "catchmentACS_error_schema",
    "catchmentACS_error_geometry",
    "catchmentACS_error_credential",
    "catchmentACS_error_network",
    "catchmentACS_error_operator",
    "catchmentACS_error_variable",
    "catchmentACS_error_missing_suggest"
  )
  if (!isTRUE(include_parents)) {
    return(unique(leaf))
  }
  unique(c(
    leaf,
    "catchmentACS_condition",
    "catchmentACS_message",
    "catchmentACS_message_progress",
    "catchmentACS_message_cache",
    "catchmentACS_warning",
    "catchmentACS_warning_cache",
    "catchmentACS_warning_runtime",
    "catchmentACS_error"
  ))
}


#' Turn the `classes` argument into a list of class names
#' @keywords internal
#' @noRd
.cacs_normalize_capture_classes <- function(classes) {
  if (is.null(classes)) {
    return(NULL)
  }
  if (!is.character(classes) || anyNA(classes) || any(!nzchar(classes))) {
    .cli_abort_schema(c(
      "{.arg classes} must be NULL or a non-missing character vector.",
      "i" = "Use full classes such as {.val catchmentACS_message_progress} or short suffixes such as {.val progress}."
    ))
  }
  unique(unlist(lapply(classes, .cacs_expand_capture_class),
                use.names = FALSE))
}


#' Expand one entry of `classes` into the class names it can mean
#'
#' A short name such as `"progress"` becomes the message, warning, and error
#' class with that ending, because the user need not know which kind it is.
#' The last two branches add the older names of the two errors that have them.
#'
#' @keywords internal
#' @noRd
.cacs_expand_capture_class <- function(cls) {
  cls <- as.character(cls)[[1L]]
  if (startsWith(cls, "catchmentACS_") || startsWith(cls, "cacs_")) {
    return(cls)
  }
  if (identical(cls, "condition")) {
    return("catchmentACS_condition")
  }
  if (cls %in% c("message", "warning", "error")) {
    return(paste0("catchmentACS_", cls))
  }
  out <- c(
    cls,
    paste0("catchmentACS_message_", cls),
    paste0("catchmentACS_warning_", cls),
    paste0("catchmentACS_error_", cls)
  )
  if (identical(cls, "annulus_input")) {
    out <- c(out, "cacs_error_annulus_input")
  }
  if (identical(cls, "schema")) {
    out <- c(out, "cacs_error_schema")
  }
  unique(out)
}


#' Is this condition one of those asked for?
#' @keywords internal
#' @noRd
.cacs_condition_matches <- function(cnd, targets) {
  cnd_classes <- class(cnd)
  if (is.null(targets)) {
    return(any(cnd_classes %in% .cacs_known_classes(include_parents = TRUE)) ||
             "catchmentACS_condition" %in% cnd_classes)
  }
  any(cnd_classes %in% targets)
}


#' Record one condition as a list of the five column values
#' @keywords internal
#' @noRd
.cacs_condition_record <- function(cnd) {
  list(
    class = .cacs_condition_leaf_class(cnd),
    message = conditionMessage(cnd),
    phase = .cacs_condition_phase(cnd),
    timestamp = as.POSIXct(Sys.time(), tz = "UTC"),
    call = .cacs_condition_call(cnd)
  )
}


#' Turn the recorded conditions into the tibble that is returned
#'
#' An empty run returns the same five columns with no rows, so the result of
#' `cacs_capture_conditions()` can be used without checking for that case.
#'
#' @keywords internal
#' @noRd
.cacs_condition_records_tbl <- function(records) {
  if (length(records) == 0L) {
    return(tibble::tibble(
      class = character(),
      message = character(),
      phase = character(),
      timestamp = as.POSIXct(character(), tz = "UTC"),
      call = character()
    ))
  }
  tibble::tibble(
    class = vapply(records, `[[`, character(1), "class"),
    message = vapply(records, `[[`, character(1), "message"),
    phase = vapply(records, `[[`, character(1), "phase"),
    timestamp = as.POSIXct(
      vapply(records, function(x) as.numeric(x$timestamp), numeric(1)),
      origin = "1970-01-01",
      tz = "UTC"
    ),
    call = vapply(records, `[[`, character(1), "call")
  )
}


#' The most specific package class of one condition
#'
#' The `class` column shows this one, because a condition carries the classes
#' of its groups as well and the most specific one says what happened.
#'
#' @keywords internal
#' @noRd
.cacs_condition_leaf_class <- function(cnd) {
  parents <- c(
    "catchmentACS_condition",
    "catchmentACS_message",
    "catchmentACS_message_progress",
    "catchmentACS_message_cache",
    "catchmentACS_warning",
    "catchmentACS_warning_cache",
    "catchmentACS_warning_runtime",
    "catchmentACS_error"
  )
  cnd_classes <- class(cnd)
  cacs_classes <- cnd_classes[
    startsWith(cnd_classes, "catchmentACS_") |
      startsWith(cnd_classes, "cacs_")
  ]
  leaf <- setdiff(cacs_classes, parents)
  if (length(leaf) > 0L) {
    return(leaf[[1L]])
  }
  if (length(cacs_classes) > 0L) {
    return(cacs_classes[[1L]])
  }
  cnd_classes[[1L]]
}


#' The label the package code gave the condition, as one string
#' @keywords internal
#' @noRd
.cacs_condition_phase <- function(cnd) {
  phase <- cnd$cacs_phase
  if (is.null(phase) || length(phase) == 0L || is.na(phase[[1L]])) {
    return(NA_character_)
  }
  as.character(phase[[1L]])
}


#' The call in which the condition was given, as one string
#' @keywords internal
#' @noRd
.cacs_condition_call <- function(cnd) {
  call <- cnd$call
  if (is.null(call)) {
    call <- conditionCall(cnd)
  }
  if (is.null(call)) {
    return(NA_character_)
  }
  if (is.environment(call)) {
    return("<environment>")
  }
  paste(deparse(call, width.cutoff = 500L), collapse = "\n")
}


#' Keep a captured message or warning from also being shown
#'
#' `tryInvokeRestart()` does nothing when the restart is missing, so a
#' condition signalled without one is captured and still shown.
#'
#' @keywords internal
#' @noRd
.cacs_try_muffle_condition <- function(cnd) {
  if (inherits(cnd, "message")) {
    return(tryInvokeRestart("muffleMessage"))
  }
  if (inherits(cnd, "warning")) {
    return(tryInvokeRestart("muffleWarning"))
  }
  invisible(NULL)
}
