# ============================================================================
# capture-conditions.R - user-facing condition capture helper.
# ============================================================================


#' Capture catchmentACS conditions from an expression
#'
#' Evaluates an expression and returns a tibble describing standardized
#' catchmentACS messages and warnings emitted while it runs. This is a small
#' user-facing wrapper around `withCallingHandlers()` so scripts, reports, and
#' tests can subscribe to the package's condition classes without repeating
#' restart-handling boilerplate.
#'
#' @param expr Expression to evaluate.
#' @param classes Optional character vector of condition classes to capture.
#'   Use full class names such as `"catchmentACS_message_water_tract_filter"`,
#'   parent classes such as `"catchmentACS_message"`, or short suffixes such as
#'   `"water_tract_filter"`. `NULL` captures every condition inheriting from
#'   `"catchmentACS_condition"`.
#' @param return_value One of `"conditions"` (default) or `"both"`. When
#'   `"conditions"`, returns the conditions tibble and *discards* the evaluated
#'   expression's return value. When `"both"`, returns a 2-element list
#'   `list(result, conditions)` where `$result` is the value of `expr` and
#'   `$conditions` is the same tibble shape as the `"conditions"` mode. The
#'   default may be changed for a session via
#'   `options(catchmentACS.capture_return_value = "both")`.
#'
#' @return When `return_value = "conditions"` (default), a tibble with columns
#'   `class`, `message`, `phase`, `timestamp`, and `call`. `class` is the most
#'   specific catchmentACS class on the condition. `phase` is read from the
#'   condition's `cacs_phase` metadata when present.
#'
#'   When `return_value = "both"`, a 2-element list:
#'   - `result`: the return value of `expr` (any R object, including `NULL`).
#'   - `conditions`: the conditions tibble above.
#'
#' @section Error behavior:
#' Messages and warnings that match `classes` are muffled after capture so they
#' are not printed twice. Errors are never muffled or swallowed; they continue
#' to propagate to the caller.
#'
#' @section Capturing both the result and the conditions:
#' With the default `return_value = "conditions"`, this helper discards the
#' return value of `expr`. That makes the natural-looking pattern
#'
#' \preformatted{
#' captured <- cacs_capture_conditions(
#'   acs <- cacs_acs_prefetch(state = "AL", year = 2023)
#' )
#' }
#'
#' silently lose `acs`: the `<-` assignment lives in the helper's evaluation
#' frame and is garbage-collected when the helper returns. There are two ways
#' to keep the result:
#'
#' 1. **`return_value = "both"` (recommended)** — the helper returns a list
#'    with both `$result` and `$conditions`, and you re-assign externally:
#'
#'    \preformatted{
#'    out <- cacs_capture_conditions(
#'      cacs_acs_prefetch(state = "AL", year = 2023),
#'      return_value = "both"
#'    )
#'    acs <- out$result
#'    nrow(out$conditions)  # how many water-tract messages were emitted
#'    }
#'
#' 2. **`<<-` super-assignment** — `<<-` walks the search path so the binding
#'    lands in the global environment (or the script's calling environment):
#'
#'    \preformatted{
#'    acs <- NULL
#'    captured <- cacs_capture_conditions(
#'      acs <<- cacs_acs_prefetch(state = "AL", year = 2023)
#'    )
#'    }
#'
#' The `return_value = "both"` form is preferred because it is local
#' (no `<<-`), names the data (`out$result` / `out$conditions`), and reads
#' as a single expression. The `<<-` pattern remains valid too.
#'
#' @examples
#' cacs_capture_conditions(1 + 1)
#'
#' cacs_capture_conditions(
#'   rlang::inform(
#'     "Example water-tract notice",
#'     class = c(
#'       "catchmentACS_message_water_tract_filter",
#'       "catchmentACS_message",
#'       "catchmentACS_condition"
#'     ),
#'     cacs_phase = "acs"
#'   ),
#'   classes = "water_tract_filter"
#' )
#'
#' \dontrun{
#' cacs_capture_conditions(
#'   cacs_run(sites = sites, state = "AL", verbose = TRUE)
#' )
#' }
#'
#' @seealso [catchmentACS-conditions] for the full condition-class hierarchy
#'   and how to catch each class, and [cacs_run()] for the pipeline whose
#'   conditions you will most often capture.
#' @family validation and conditions
#' @export
cacs_capture_conditions <- function(expr, classes = NULL,
                                    return_value = NULL) {
  quo <- rlang::enquo(expr)
  targets <- .cacs_normalize_capture_classes(classes)

  # v0.4 Issue 001: `return_value` arg controls return shape.
  # Default `NULL` -> read option (default "conditions" = v0.3 behavior).
  if (is.null(return_value)) {
    return_value <- getOption("catchmentACS.capture_return_value",
                              "conditions")
  }
  return_value <- match.arg(return_value, c("conditions", "both"))

  if (identical(return_value, "both")) {
    return(.cacs_capture_conditions_with_value(quo, targets))
  }

  # v0.3 path (preserved verbatim for backward compatibility).
  captured <- list()

  handler <- function(cnd) {
    if (!.cacs_condition_matches(cnd, targets)) {
      return(invisible(NULL))
    }
    captured[[length(captured) + 1L]] <<- .cacs_condition_record(cnd)
    .cacs_try_muffle_condition(cnd)
    invisible(NULL)
  }

  withCallingHandlers(
    rlang::eval_tidy(quo),
    catchmentACS_condition = handler
  )

  .cacs_condition_records_tbl(captured)
}


# ---------------------------------------------------------------------------
# v0.4 Issue 001 Layer 1 helper: `.cacs_capture_conditions_with_value()`.
#
# Pure function used by `cacs_capture_conditions(return_value = "both")` to
# capture *both* the conditions and the evaluated value of `expr`. Mirrors the
# v0.3 `cacs_capture_conditions()` body, but additionally retains the result
# of `rlang::eval_tidy(quo)` and returns `list(result, conditions)`.
#
# Caller responsibilities (Phase 3 Step 3.1):
#   - Pass an `rlang::enquo()` quosure of the user expression.
#   - Pass the normalized class targets (output of `.cacs_normalize_capture_classes()`).
#
# Side effects:
#   - Same as v0.3 capture path: messages/warnings emitted while evaluating
#     `expr` are captured + muffled per class match; errors propagate.
#   - In-expression `<-` assignments do NOT propagate to the caller env (this
#     is the v0.3 footgun the `$result` slot resolves: the user re-assigns
#     externally as `acs <- out$result`).
# ---------------------------------------------------------------------------


#' Capture both the result and the conditions of an evaluated expression
#'
#' v0.4 Issue 001 Layer 1 helper. Used by `cacs_capture_conditions()` when
#' the user passes `return_value = "both"`. Returns the v0.3 conditions tibble
#' AND the evaluated expression's return value, in a 2-element list. The v0.3
#' `<-`-inside-expr scope footgun is resolved at the user level by writing
#' `acs <- out$result` after the helper returns.
#'
#' @param quo An `rlang::enquo()` quosure of the user expression.
#' @param targets Normalized class targets (output of
#'   `.cacs_normalize_capture_classes()`); pass `NULL` to capture any
#'   `catchmentACS_condition`.
#'
#' @return A 2-element list:
#'   - `result`: the return value of `rlang::eval_tidy(quo)` (any R object,
#'     including `NULL`).
#'   - `conditions`: the same tibble shape as v0.3 `cacs_capture_conditions()`
#'     (`class`, `message`, `phase`, `timestamp`, `call`).
#' @keywords internal
#' @noRd
.cacs_capture_conditions_with_value <- function(quo, targets) {
  captured <- list()

  handler <- function(cnd) {
    if (!.cacs_condition_matches(cnd, targets)) {
      return(invisible(NULL))
    }
    captured[[length(captured) + 1L]] <<- .cacs_condition_record(cnd)
    .cacs_try_muffle_condition(cnd)
    invisible(NULL)
  }

  result <- withCallingHandlers(
    rlang::eval_tidy(quo),
    catchmentACS_condition = handler
  )

  list(
    result     = result,
    conditions = .cacs_condition_records_tbl(captured)
  )
}


#' Known catchmentACS condition classes (internal)
#' @keywords internal
#' @noRd
.cacs_known_classes <- function(include_parents = FALSE) {
  leaf <- c(
    "catchmentACS_message_water_tract_filter",
    "catchmentACS_message_progress_tick",
    "catchmentACS_message_progress_summary",
    "catchmentACS_message_res_default_changed",
    "catchmentACS_message_demo_budget_protected",   # NEW v0.4 (Issue 002)
    "catchmentACS_message_rate_first_changed",      # NEW v0.4 (Issue 004)
    "catchmentACS_message_listcol_iso_filled",      # NEW v0.4 (Issue 003)
    "catchmentACS_message_perf_fix_applied",
    "catchmentACS_message_resolve_site",
    "catchmentACS_message_cache",
    "catchmentACS_message_cache_legacy_invalidated",
    "catchmentACS_message_cache_fingerprint_mismatch",
    "catchmentACS_message_cache_enabled_announce",
    "catchmentACS_warning_cache_stale_suspect",
    "catchmentACS_warning_provider_quota_exhausted", # NEW v0.4 (Issue 002 Step 4.2)
    "catchmentACS_warning_rate_out_of_range",
    "catchmentACS_warning_carrier_missing",
    "catchmentACS_warning_resolve_site_distant",
    "catchmentACS_warning_geometry_skip",
    "catchmentACS_warning_runtime",
    "catchmentACS_warning_provenance",
    "catchmentACS_warning_variable",
    "catchmentACS_warning_partial",
    "catchmentACS_error_annulus_input",
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


#' Normalize user-supplied condition class filters (internal)
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


#' Expand full, parent, or short condition class filter aliases (internal)
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


#' Should this condition be captured for the requested filter? (internal)
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


#' Capture one condition as a list row (internal)
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


#' Convert captured condition records to the public tibble shape (internal)
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


#' Most specific catchmentACS class for one condition (internal)
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


#' Condition phase metadata as a scalar character (internal)
#' @keywords internal
#' @noRd
.cacs_condition_phase <- function(cnd) {
  phase <- cnd$cacs_phase
  if (is.null(phase) || length(phase) == 0L || is.na(phase[[1L]])) {
    return(NA_character_)
  }
  as.character(phase[[1L]])
}


#' Condition call as a scalar character (internal)
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


#' Muffle message/warning conditions after capture when possible (internal)
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
