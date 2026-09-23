# cli.R - the wrappers that every error, warning, and message of the package
# goes through. cli formats the text and rlang signals it.
#
# Each condition carries the classes
#   c("catchmentACS_<kind>_<family>",
#     "catchmentACS_<kind>",
#     "catchmentACS_condition")
# with a parent family in between for some of them (see the branches below).
# A handler can therefore ask for one family,
# `tryCatch(catchmentACS_error_schema = ...)`, or for every error of the
# package, `tryCatch(catchmentACS_error = ...)`. `cacs_capture_conditions()`
# and `cacs_run()` collect conditions by the class `catchmentACS_condition`.
#
# The wrappers pass `.envir = rlang::caller_env()` to cli, so a `{...}`
# expression in a message is evaluated where the wrapper was called: the
# message of `.repair_geometry()` in R/utils.R can write
# `{sum(!is_valid | is.na(is_valid))}` about its own local values.


.cacs_cond_classes <- function(kind, family) {
  kind   <- match.arg(kind, c("error", "warning", "message"))
  family <- match.arg(family, c(
    # used by errors only
    "schema", "credential", "geometry", "operator", "network",
    # used by warnings only
    "provenance", "runtime", "geometry_skip",
    "rate_out_of_range", "carrier_missing",
    "cache_stale_suspect",
    # used by messages only
    "cache", "cache_legacy_invalidated", "cache_fingerprint_mismatch",
    "cache_enabled_announce", "progress", "water_tract_filter",
    "res_default_changed", "perf_fix_applied", "resolve_site",
    "progress_tick", "progress_summary",
    "demo_budget_protected",   # the omitted OSRM res was lowered for the demo server
    "rate_first_changed",      # as_tibble() changed the row order
    "listcol_iso_filled",      # the isochrone list column was filled
    # a warning as well
    "provider_quota_exhausted", # HTTP 429 from the OSRM demo server
    # "variable" is used by an error and by a warning; "resolve_site_distant"
    # only by a warning
    "variable", "resolve_site_distant"
  ))
  leaf_class <- paste0("catchmentACS_", kind, "_", family)
  kind_class <- paste0("catchmentACS_", kind)
  if (identical(kind, "message") &&
      family %in% c("progress_tick", "progress_summary")) {
    return(c(leaf_class, "catchmentACS_message_progress",
             kind_class, "catchmentACS_condition"))
  }
  if (identical(kind, "message") && startsWith(family, "cache_")) {
    return(c(leaf_class, "catchmentACS_message_cache",
             kind_class, "catchmentACS_condition"))
  }
  if (identical(kind, "warning") && startsWith(family, "cache_")) {
    return(c(leaf_class, "catchmentACS_warning_cache",
             kind_class, "catchmentACS_condition"))
  }
  if (identical(kind, "warning") && identical(family, "carrier_missing")) {
    return(c(leaf_class, "catchmentACS_warning_runtime",
             kind_class, "catchmentACS_condition"))
  }
  c(leaf_class, kind_class, "catchmentACS_condition")
}


#' Signal one condition of the package
#'
#' The single place where a condition of the package is signaled: cli formats
#' the message and rlang signals it, so the classes stay catchable in a
#' non-interactive `Rscript` while the cli bullets and `{...}` expressions are
#' still rendered. An error has each of its lines formatted with
#' `cli::format_inline()` and handed to `rlang::abort(use_cli_format = TRUE)`;
#' a warning and a message are formatted as a block first. `classes` passes a
#' class chain of its own and skips the list of families above; two callers do
#' that, `.cli_abort_annulus_input()` below and the partial-failure warning of
#' `cacs_run()`.
#' @keywords internal
#' @noRd
.cacs_emit <- function(level,
                       family = NULL,
                       message,
                       classes = NULL,
                       parent = NULL,
                       phase = NA_character_,
                       call = rlang::caller_env(),
                       .envir = rlang::caller_env(),
                       .frequency = NULL,
                       .frequency_id = NULL,
                       ...) {
  level <- match.arg(level, c("inform", "warn", "abort"))
  kind <- switch(level,
                 inform = "message",
                 warn   = "warning",
                 abort  = "error")
  if (is.null(classes)) {
    classes <- .cacs_cond_classes(kind, family)
  }

  if (identical(level, "abort")) {
    msg <- message
    msg[] <- vapply(as.character(msg), cli::format_inline,
                    character(1L), .envir = .envir)
    rlang::abort(
      message = msg,
      class = classes,
      parent = parent,
      call = call,
      use_cli_format = TRUE,
      ...
    )
  }

  formatted <- switch(level,
                      inform = cli::format_message(message, .envir = .envir),
                      warn   = cli::format_warning(message, .envir = .envir))
  args <- c(
    list(
      message = formatted,
      class = classes,
      parent = parent,
      call = call,
      `cacs_phase` = phase
    ),
    list(...)
  )
  if (!is.null(.frequency)) {
    args$.frequency <- .frequency
  }
  if (!is.null(.frequency_id)) {
    args$.frequency_id <- .frequency_id
  }

  if (identical(level, "inform")) {
    do.call(rlang::inform, args)
  } else {
    do.call(rlang::warn, args)
  }
  invisible(NULL)
}


#' Stop because the input has the wrong shape
#' @keywords internal
#' @noRd
.cli_abort_schema <- function(message,
                              parent = NULL,
                              call = rlang::caller_env(),
                              .envir = rlang::caller_env(),
                              ...) {
  .cacs_emit(
    level = "abort",
    family = "schema",
    message = message,
    parent = parent,
    call = call,
    .envir = .envir,
    ...
  )
}

#' Describe one failed check as a list
#' @keywords internal
#' @noRd
.schema_issue <- function(check,
                          col = NA_character_,
                          actual = NA_character_,
                          expected = NA_character_,
                          fix_hint = NA_character_,
                          example = NA_character_,
                          severity = "error") {
  list(
    severity = as.character(severity[[1L]]),
    check = as.character(check[[1L]]),
    col = as.character(col[[1L]]),
    actual = as.character(actual[[1L]]),
    expected = as.character(expected[[1L]]),
    fix_hint = as.character(fix_hint[[1L]]),
    example = as.character(example[[1L]])
  )
}

#' Put the issue lists of `.schema_issue()` into a tibble with seven columns
#' @keywords internal
#' @noRd
.schema_issue_tbl <- function(issues) {
  pull_chr <- function(name) {
    vapply(issues, function(issue) {
      value <- issue[[name]]
      if (is.null(value) || length(value) == 0L) {
        return(NA_character_)
      }
      as.character(value[[1L]])
    }, character(1))
  }
  tibble::tibble(
    severity = pull_chr("severity"),
    check = pull_chr("check"),
    col = pull_chr("col"),
    actual = pull_chr("actual"),
    expected = pull_chr("expected"),
    fix_hint = pull_chr("fix_hint"),
    example = pull_chr("example")
  )
}

#' The line that points readers to the porting article
#' @keywords internal
#' @noRd
.schema_vignette_hint <- function() {
  paste0(
    "For full schema examples, run: ",
    "vignette('porting-v01-to-v03', package = 'catchmentACS')"
  )
}

#' Stop with a list of failed checks
#'
#' Shows the first `max_issues` of them, each as a line with what was expected
#' and what was found, then the number left over.
#' @keywords internal
#' @noRd
.cli_abort_schema_with_template <- function(headline,
                                            issues,
                                            help = .schema_vignette_hint(),
                                            max_issues = 5L,
                                            parent = NULL,
                                            call = rlang::caller_env(),
                                            .envir = rlang::caller_env(),
                                            ...) {
  stopifnot(is.list(issues))
  scalar_or_na <- function(value) {
    if (is.null(value) || length(value) == 0L) NA_character_ else value[[1L]]
  }
  shown <- utils::head(issues, max_issues)
  lines <- unlist(lapply(shown, function(issue) {
    col <- scalar_or_na(issue$col)
    col_part <- if (!is.na(col) && nzchar(col)) paste0(" [", col, "]") else ""
    expected <- scalar_or_na(issue$expected)
    actual <- scalar_or_na(issue$actual)
    fix_hint <- scalar_or_na(issue$fix_hint)
    example <- scalar_or_na(issue$example)
    out <- c(
      "x" = paste0(
        issue$check, col_part, ": expected ", expected,
        "; got ", actual, "."
      )
    )
    if (!is.na(fix_hint) && nzchar(fix_hint)) {
      out <- c(out, "i" = paste0("Fix: ", fix_hint))
    }
    if (!is.na(example) && nzchar(example)) {
      out <- c(out, "i" = paste0("Example: ", example))
    }
    out
  }), use.names = TRUE)
  hidden <- length(issues) - length(shown)
  if (hidden > 0L) {
    lines <- c(lines, "i" = paste0("... and ", hidden, " more schema issue(s)."))
  }
  if (!is.null(help) && !identical(help, FALSE)) {
    lines <- c(lines, "i" = help)
  }
  .cli_abort_schema(c(headline, lines),
                    parent = parent, call = call, .envir = .envir, ...)
}

#' Stop because the areas are rings rather than whole areas
#'
#' The checks in R/checks.R call this when a drive-time area covers the band
#' between two drive times (`isomin > 0`). `cacs_intersect_weight()` takes
#' only areas that reach from 0 up to each drive time;
#' `cacs_rings_to_cumulative()` converts the others.
#'
#' Uses the fixed chain `.ANNULUS_ERROR_CLASSES` (R/aaa-globals.R) instead of
#' the families above: `catchmentACS_error_annulus_input` comes first, then
#' the two older `cacs_` names, then `catchmentACS_error_schema` and its
#' parents, so a handler written for any of them still catches this error.
#' @keywords internal
#' @noRd
.cli_abort_annulus_input <- function(message,
                                     parent = NULL,
                                     call = rlang::caller_env(),
                                     .envir = rlang::caller_env(),
                                     ...) {
  .cacs_emit(
    level = "abort",
    message = message,
    classes = .ANNULUS_ERROR_CLASSES,
    parent = parent,
    call = call,
    .envir = .envir,
    ...
  )
}

#' Stop because a key is missing, or the feature is not written yet
#'
#' Both the missing Census API key and the parts that are not implemented, the
#' Mapbox and r5r providers and weighting by population, use this class.
#' @keywords internal
#' @noRd
.cli_abort_credential <- function(message,
                                  parent = NULL,
                                  call = rlang::caller_env(),
                                  .envir = rlang::caller_env(),
                                  ...) {
  .cacs_emit(
    level = "abort",
    family = "credential",
    message = message,
    parent = parent,
    call = call,
    .envir = .envir,
    ...
  )
}

#' Stop because a boundary cannot be repaired or intersected
#' @keywords internal
#' @noRd
.cli_abort_geometry <- function(message,
                                parent = NULL,
                                call = rlang::caller_env(),
                                .envir = rlang::caller_env(),
                                ...) {
  .cacs_emit(
    level = "abort",
    family = "geometry",
    message = message,
    parent = parent,
    call = call,
    .envir = .envir,
    ...
  )
}

#' Stop because a file, a folder, or a service refused the request
#' @keywords internal
#' @noRd
.cli_abort_operator <- function(message,
                                parent = NULL,
                                call = rlang::caller_env(),
                                .envir = rlang::caller_env(),
                                ...) {
  .cacs_emit(
    level = "abort",
    family = "operator",
    message = message,
    parent = parent,
    call = call,
    .envir = .envir,
    ...
  )
}

#' Stop because no ACS variable code is left
#'
#' `cacs_acs_prefetch()` calls this when the ACS variable list has none of the
#' requested codes. When it has some of them, the call goes to
#' `.cli_warn_variable()` instead and the rest are downloaded.
#' @keywords internal
#' @noRd
.cli_abort_variable <- function(message,
                                parent = NULL,
                                call = rlang::caller_env(),
                                .envir = rlang::caller_env(),
                                ...) {
  .cacs_emit(
    level = "abort",
    family = "variable",
    message = message,
    parent = parent,
    call = call,
    .envir = .envir,
    ...
  )
}

#' Stop because the download did not succeed
#'
#' `cacs_acs_prefetch()` calls this when the third attempt at the ACS download
#' has failed.
#' @keywords internal
#' @noRd
.cli_abort_network <- function(message,
                               parent = NULL,
                               call = rlang::caller_env(),
                               .envir = rlang::caller_env(),
                               ...) {
  .cacs_emit(
    level = "abort",
    family = "network",
    message = message,
    parent = parent,
    call = call,
    .envir = .envir,
    ...
  )
}


#' Warn about the record kept with a result
#' @keywords internal
#' @noRd
.cli_warn_provenance <- function(message,
                                 parent = NULL,
                                 phase = NA_character_,
                                 call = rlang::caller_env(),
                                 .envir = rlang::caller_env(),
                                 ...) {
  .cacs_emit(
    level = "warn",
    family = "provenance",
    message = message,
    parent = parent,
    phase = phase,
    call = call,
    .envir = .envir,
    ...
  )
}

#' Warn about something met while the step runs
#' @keywords internal
#' @noRd
.cli_warn_runtime <- function(message,
                              parent = NULL,
                              phase = NA_character_,
                              call = rlang::caller_env(),
                              .envir = rlang::caller_env(),
                              ...) {
  .cacs_emit(
    level = "warn",
    family = "runtime",
    message = message,
    parent = parent,
    phase = phase,
    call = call,
    .envir = .envir,
    ...
  )
}

#' Warn that some ACS variable codes were skipped
#'
#' `cacs_acs_prefetch()` calls this when the ACS variable list has some but
#' not all of the requested codes; the rest are downloaded.
#' @keywords internal
#' @noRd
.cli_warn_variable <- function(message,
                               parent = NULL,
                               phase = NA_character_,
                               call = rlang::caller_env(),
                               .envir = rlang::caller_env(),
                               ...) {
  .cacs_emit(
    level = "warn",
    family = "variable",
    message = message,
    parent = parent,
    phase = phase,
    call = call,
    .envir = .envir,
    ...
  )
}

#' Warn that a derived rate is outside the range it should have
#' @keywords internal
#' @noRd
.cli_warn_rate_out_of_range <- function(message,
                                        parent = NULL,
                                        phase = NA_character_,
                                        call = rlang::caller_env(),
                                        .envir = rlang::caller_env(),
                                        ...) {
  .cacs_emit(
    level = "warn",
    family = "rate_out_of_range",
    message = message,
    parent = parent,
    phase = phase,
    call = call,
    .envir = .envir,
    ...
  )
}

#' Warn that a rate has no numerator and denominator to work from
#'
#' Its class chain has `catchmentACS_warning_runtime` as the parent, so a
#' handler for runtime warnings catches it as well.
#' @keywords internal
#' @noRd
.cli_warn_carrier_missing <- function(message,
                                      parent = NULL,
                                      phase = NA_character_,
                                      call = rlang::caller_env(),
                                      .envir = rlang::caller_env(),
                                      ...) {
  .cacs_emit(
    level = "warn",
    family = "carrier_missing",
    message = message,
    parent = parent,
    phase = phase,
    call = call,
    .envir = .envir,
    ...
  )
}


#' Report reading or writing a saved result
#' @keywords internal
#' @noRd
.cli_inform_cache <- function(message,
                              phase = NA_character_,
                              call = rlang::caller_env(),
                              .envir = rlang::caller_env(),
                              ...) {
  .cacs_emit(
    level = "inform",
    family = "cache",
    message = message,
    phase = phase,
    call = call,
    .envir = .envir,
    ...
  )
}

#' Report that a saved result from an older layout was dropped
#' @keywords internal
#' @noRd
.cli_inform_cache_legacy_invalidated <- function(message,
                                                 phase = NA_character_,
                                                 call = rlang::caller_env(),
                                                 .envir = rlang::caller_env(),
                                                 ...) {
  .cacs_emit(
    level = "inform",
    family = "cache_legacy_invalidated",
    message = message,
    phase = phase,
    call = call,
    .envir = .envir,
    ...
  )
}

#' Report that a saved result failed its checksum
#' @keywords internal
#' @noRd
.cli_inform_cache_fingerprint_mismatch <- function(message,
                                                   phase = NA_character_,
                                                   call = rlang::caller_env(),
                                                   .envir = rlang::caller_env(),
                                                   ...) {
  .cacs_emit(
    level = "inform",
    family = "cache_fingerprint_mismatch",
    message = message,
    phase = phase,
    call = call,
    .envir = .envir,
    ...
  )
}

#' Report that the cache is on and where it writes
#' @keywords internal
#' @noRd
.cli_inform_cache_enabled_announce <- function(message,
                                               phase = NA_character_,
                                               call = rlang::caller_env(),
                                               .envir = rlang::caller_env(),
                                               ...) {
  .cacs_emit(
    level = "inform",
    family = "cache_enabled_announce",
    message = message,
    phase = phase,
    call = call,
    .envir = .envir,
    ...
  )
}

#' Warn that a saved result looks too small
#' @keywords internal
#' @noRd
.cli_warn_cache_stale_suspect <- function(message,
                                          phase = NA_character_,
                                          call = rlang::caller_env(),
                                          .envir = rlang::caller_env(),
                                          ...) {
  .cacs_emit(
    level = "warn",
    family = "cache_stale_suspect",
    message = message,
    phase = phase,
    call = call,
    .envir = .envir,
    ...
  )
}

#' Say that an omitted OSRM `res` became 70L
#'
#' `cacs_isochrone()` shows this when `res` was not given and the value used
#' is 70: with `osrm_mode = "docker"`, or with the demo server when
#' `catchmentACS.osrm_demo_budget_protect` is `FALSE`. The demo server with
#' that option left alone gives the message below instead. rlang shows either
#' one once per session.
#' @keywords internal
#' @noRd
.cli_inform_res_default_changed <- function() {
  .cacs_emit(
    level = "inform",
    family = "res_default_changed",
    c(
      "{.fn cacs_isochrone} used {.code res = 70L} for OSRM because {.arg res} was not given.",
      "i" = "A larger {.arg res} gives more detailed areas but needs more requests and more time; to choose it, use",
      " " = "{.code cacs_isochrone(res = 50L)} or {.code cacs_run(iso_args = list(res = 50L))}."
    ),
    phase = "isochrone",
    .frequency = "once",
    .frequency_id = "catchmentACS_res_default_changed"
  )
}

#' Warn that the OSRM demo server is refusing requests
#'
#' `cacs_validate_osrm_endpoint()` shows this when its test request comes back
#' with HTTP 429, which means the public demo server has had enough requests
#' for now. Waiting or `osrm_mode = "docker"` are the ways on.
#' @keywords internal
#' @noRd
.cli_warn_provider_quota_exhausted <- function(message,
                                               phase = "isochrone",
                                               call = rlang::caller_env(),
                                               .envir = rlang::caller_env(),
                                               ...) {
  .cacs_emit(
    level = "warn",
    family = "provider_quota_exhausted",
    message = message,
    phase = phase,
    call = call,
    .envir = .envir,
    ...
  )
}

#' Say that an omitted OSRM `res` became 30L on the demo server
#'
#' `cacs_isochrone()` shows this once per session when `res` was not given,
#' `osrm_mode` is `"demo"`, and `catchmentACS.osrm_demo_budget_protect` is
#' `TRUE`, which is its default. A `res` that is given, and
#' `osrm_mode = "docker"`, say nothing.
#' @keywords internal
#' @noRd
.cli_inform_demo_budget_protected <- function() {
  .cacs_emit(
    level = "inform",
    family = "demo_budget_protected",
    c(
      "{.fn cacs_isochrone} used {.code res = 30L} on the public OSRM demo server because {.arg res} was not given.",
      "i" = "With the lower value, fewer requests are sent for each site, so the server is less likely to refuse them (HTTP status 429).",
      "i" = "For more detailed areas, give a larger {.arg res}, as in",
      " " = "{.code cacs_isochrone(res = 70L)} or {.code cacs_run(iso_args = list(res = 70L))}.",
      "i" = "A local OSRM server uses 70L when {.arg res} is not given: {.code osrm_mode = \"docker\"}.",
      "i" = "{.code options(catchmentACS.osrm_demo_budget_protect = FALSE)} makes the demo server use 70L as well."
    ),
    phase = "isochrone",
    .frequency = "once",
    .frequency_id = "catchmentACS_demo_budget_protected"
  )
}

#' Say that `as_tibble()` puts the rate rows first
#'
#' `as_tibble()` on a `cacs_run_result` shows this once per session when the
#' option `catchmentACS.rate_first_default = TRUE`, not the argument
#' `rate_first`, has moved the derived rate rows above the ACS variables of the
#' same site. An explicit `rate_first = TRUE` sorts without this message.
#' @keywords internal
#' @noRd
.cli_inform_rate_first_changed <- function() {
  .cacs_emit(
    level = "inform",
    family = "rate_first_changed",
    c(
      "{.fn as_tibble} put the rate rows first within each site and drive time because {.code options(catchmentACS.rate_first_default = TRUE)} is set.",
      "i" = "Pass {.code rate_first = FALSE} to keep the order of the rows."
    ),
    .frequency = "once",
    .frequency_id = "catchmentACS_rate_first_changed"
  )
}

#' Say that the `isochrone` column holds the drive-time areas
#'
#' `cacs_run(output = "list_column")` shows this when it has the areas to put
#' in the column; older results left `NULL` in every cell.
#' @keywords internal
#' @noRd
.cli_inform_listcol_iso_filled <- function() {
  # Not throttled here: `.pivot_to_list_column()` (R/run.R) decides, once per
  # `cacs_run()` call, whether to call this at all.
  .cacs_emit(
    level = "inform",
    family = "listcol_iso_filled",
    c(
      "The {.field isochrone} column holds the drive-time area of each site and drive time.",
      "i" = "The areas are the ones built by {.fn cacs_isochrone} or given in {.arg precomputed_isochrones}."
    ),
    phase = "run"
  )
}

#' Say that the OSRM server in use needs no wait between requests
#'
#' `cacs_isochrone()` shows this when the server in use is not the public demo
#' one, where the osrm package waits a second after every 75 grid points it
#' sends and a site therefore takes much longer.
#' @keywords internal
#' @noRd
.cli_inform_perf_fix_applied <- function(message,
                                         phase = "isochrone",
                                         call = rlang::caller_env(),
                                         .envir = rlang::caller_env(),
                                         ...) {
  .cacs_emit(
    level = "inform",
    family = "perf_fix_applied",
    message = message,
    phase = phase,
    call = call,
    .envir = .envir,
    ...
  )
}

#' Say which site a pair of coordinates was matched to
#'
#' The plot functions show this when `(lat, lon)` is given in place of a
#' `site_id`: they pick the nearest site of the result and report the
#' distance.
#' @keywords internal
#' @noRd
.cli_inform_resolve_site <- function(message,
                                     phase = "plot",
                                     call = rlang::caller_env(),
                                     .envir = rlang::caller_env(),
                                     ...) {
  .cacs_emit(
    level = "inform",
    family = "resolve_site",
    message = message,
    phase = phase,
    call = call,
    .envir = .envir,
    ...
  )
}

#' Warn that the matched site is far from the coordinates
#'
#' Comes after the message above when the nearest site is more than 5 km from
#' the `(lat, lon)` that was given (`warning_threshold_km` in
#' `.cacs_promote_latlon_to_site()`, R/plot-helpers.R).
#' @keywords internal
#' @noRd
.cli_warn_resolve_site_distant <- function(message,
                                           phase = "plot",
                                           call = rlang::caller_env(),
                                           .envir = rlang::caller_env(),
                                           ...) {
  .cacs_emit(
    level = "warn",
    family = "resolve_site_distant",
    message = message,
    phase = phase,
    call = call,
    .envir = .envir,
    ...
  )
}

#' Report where a step has got to
#' @keywords internal
#' @noRd
.cli_inform_progress <- function(message,
                                 phase = NA_character_,
                                 call = rlang::caller_env(),
                                 .envir = rlang::caller_env(),
                                 ...) {
  .cacs_emit(
    level = "inform",
    family = "progress",
    message = message,
    phase = phase,
    call = call,
    .envir = .envir,
    ...
  )
}


#' Say which water tracts were dropped
#'
#' `.drop_water_tracts()` (R/acs-prefetch.R) shows this after it has removed
#' tracts from a `cacs_acs_prefetch()` result. A state can have many of them,
#' so the message names five and counts the rest as `(+N more)`.
#'
#' Condition class: `catchmentACS_message_water_tract_filter` /
#' `catchmentACS_message` / `catchmentACS_condition`.
#'
#' @param geoids A character vector of the GEOIDs dropped by the filter.
#'   Repeated values are counted once.
#' @param reasons A character vector with one reason per element of `geoids`
#'   (`"geoid_pattern"`, `"zero_area"`, or `"geoid_pattern+zero_area"`). The
#'   message does not show them.
#' @keywords internal
#' @noRd
.cli_inform_water_tract_filter <- function(geoids, reasons = NULL) {
  geoids_unique <- unique(geoids)
  n_total <- length(geoids_unique)
  capped  <- utils::head(geoids_unique, 5L)
  more_n  <- n_total - length(capped)
  tail    <- if (more_n > 0L) sprintf(" (+%d more)", more_n) else ""
  body    <- paste(capped, collapse = ", ")
  .cacs_emit(
    level = "inform",
    family = "water_tract_filter",
    message = c(
      "Dropped {n_total} water or special-purpose tract{?s} from the ACS data.",
      "i" = "GEOIDs: {body}{tail}.",
      "*" = "Use {.code drop_water_tracts = FALSE} to keep them."
    ),
    phase = "acs"
  )
}


#' Warn that tracts without a usable area are being skipped
#'
#' `cacs_intersect_weight()` shows this when some tracts have an area of zero
#' or less and at least one tract is left; those rows are dropped and the work
#' goes on. When no tract is left it stops with `catchmentACS_error_geometry`
#' instead. Like the message above, the text names five GEOIDs and counts the
#' rest.
#'
#' Condition class: `catchmentACS_warning_geometry_skip` /
#' `catchmentACS_warning` / `catchmentACS_condition`.
#'
#' @param geoids A character vector of the GEOIDs being skipped.
#' @keywords internal
#' @noRd
.warn_skip_water_tract <- function(geoids) {
  capped <- utils::head(geoids, 5L)
  more_n <- length(geoids) - length(capped)
  tail   <- if (more_n > 0L) sprintf(" (+%d more)", more_n) else ""
  msg    <- sprintf(
    "Skipping %d tract(s) with degenerate (non-positive or non-finite) area: %s%s",
    length(geoids),
    paste(capped, collapse = ", "),
    tail
  )
  .cacs_emit("warn", "geometry_skip", msg, phase = "intersect")
}


# The progress messages of the steps. A step asks for a reporter, ticks it
# once per unit of work, and finishes it; the reporter decides what, if
# anything, is shown. Both kinds of line are ordinary messages with a class,
# not a cli progress bar, so they survive a non-interactive run and reach the
# handler of `cacs_run()` like every other condition of the package. The
# classes come from `.cacs_cond_classes()` above:
#   * `progress_tick`    -> one line per unit of work, in `"bar"` mode only
#   * `progress_summary` -> the line at the end, in every mode but `"silent"`
#
# The first of these that applies decides the mode:
#   1. verbose == FALSE                                   -> `"silent"`
#   2. Sys.getenv("CACS_QUIET") == "1"                    -> `"silent"`
#   3. getOption("catchmentACS.progress") == "off"        -> `"silent"`
#                                              == "force" -> `"bar"`
#                                              == "auto"  -> fall through
#   4. N < 5                                              -> `"bookend"`
#   5. N >= 5                                             -> `"bar"`
# The "Progress messages" section of `?cacs_run` says the same thing for
# users, together with the throttling of the tick lines below.


#' Decide what a step shows
#'
#' Reads the flag, the environment variable, and the option; changes nothing.
#' Returns `"silent"`, `"bookend"`, or `"bar"`.
#'
#' @param n A single integer, the total number of iterations (sites, tracts,
#'   and so on) the caller will tick across.
#' @param verbose A single TRUE or FALSE, the caller's verbose flag.
#' @keywords internal
#' @noRd
.cacs_progress_mode <- function(n, verbose) {
  if (!isTRUE(verbose)) return("silent")
  if (identical(Sys.getenv("CACS_QUIET"), "1")) return("silent")
  opt <- getOption("catchmentACS.progress", "auto")
  if (identical(opt, "off"))   return("silent")
  if (identical(opt, "force")) return("bar")
  # "auto", or the option unset: an `n` that is not one usable number counts
  # as a short run.
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 5L) {
    return("bookend")
  }
  "bar"
}


#' The clock, in one place so that tests can replace it
#' @keywords internal
#' @noRd
.cacs_now <- function() Sys.time()


#' Format seconds as M:SS / H:MM:SS
#'
#' @param secs A single number of seconds.
#' @keywords internal
#' @noRd
.format_duration <- function(secs) {
  if (!is.numeric(secs) || length(secs) != 1L || is.na(secs) || secs < 0) {
    return("--:--")
  }
  isec <- as.integer(round(secs))
  if (isec >= 3600L) {
    h <- isec %/% 3600L
    m <- (isec %% 3600L) %/% 60L
    s <- isec %% 60L
    sprintf("%d:%02d:%02d", h, m, s)
  } else {
    m <- isec %/% 60L
    s <- isec %% 60L
    sprintf("%d:%02d", m, s)
  }
}


#' Work out the time left from the time spent so far
#'
#' `"0:00"` once `i` reaches `n`, and `"--:--"` for arguments it cannot use.
#' The reporter below does this arithmetic in its own closure, where it can
#' also keep the estimate from growing, so the only caller left is the test
#' file `tests/testthat/test-unit-progress-cli.R`.
#'
#' @param elapsed_sec A single number, the seconds since `start_time`.
#' @param i A single integer, the current iteration (counting from 1).
#' @param n A single integer, the total number of iterations.
#' @keywords internal
#' @noRd
.format_eta <- function(elapsed_sec, i, n) {
  if (!is.numeric(elapsed_sec) || !is.numeric(i) || !is.numeric(n) ||
      length(elapsed_sec) != 1L || length(i) != 1L || length(n) != 1L ||
      is.na(elapsed_sec) || is.na(i) || is.na(n) ||
      i < 1L || n < 1L || i > n) {
    return("--:--")
  }
  if (i >= n) return("0:00")
  rate_sec_per_iter <- elapsed_sec / i
  remaining <- as.numeric(rate_sec_per_iter) * (n - i)
  if (!is.finite(remaining) || remaining < 0) return("--:--")
  .format_duration(remaining)
}


#' Show the line at the end of a step
#'
#' Reads "{label} complete: {n_success}/{n_total} in {elapsed} ({rate}/sec)",
#' with " - {n_failed} failure(s)" when something failed. The rate counts all
#' the units, not only the ones that succeeded. Class chain:
#' `catchmentACS_message_progress_summary` / `catchmentACS_message_progress` /
#' `catchmentACS_message` / `catchmentACS_condition`.
#'
#' The three settings that turn progress messages off are read again here, so
#' the line stays away even when a caller signals the summary itself.
#'
#' @param start_time A single `POSIXct` time, when the reporter started.
#' @param n_total A single integer, the number of iterations expected.
#' @param n_success A single integer, the number of them that succeeded.
#' @param n_failed A single integer, the number of them that failed.
#' @param label A single string naming the step, such as `"Isochrones"`.
#' @param verbose A single TRUE or FALSE, the caller's verbose flag; `FALSE`
#'   shows nothing.
#' @keywords internal
#' @noRd
.cacs_progress_summary <- function(start_time, n_total, n_success, n_failed,
                                   label, verbose = TRUE) {
  if (!isTRUE(verbose)) return(invisible(NULL))
  if (identical(Sys.getenv("CACS_QUIET"), "1")) return(invisible(NULL))
  opt <- getOption("catchmentACS.progress", "auto")
  if (identical(opt, "off")) return(invisible(NULL))

  elapsed_sec <- as.numeric(difftime(.cacs_now(), start_time, units = "secs"))
  if (!is.finite(elapsed_sec) || elapsed_sec < 0) elapsed_sec <- 0
  elapsed_fmt <- .format_elapsed(elapsed_sec)
  rate_per_sec <- if (elapsed_sec > 0) n_total / elapsed_sec else NA_real_
  rate_fmt <- if (is.finite(rate_per_sec)) {
    sprintf("%.1f", rate_per_sec)
  } else {
    "n/a"
  }

  msg <- sprintf(
    "%s complete: %d/%d in %s (%s/sec)",
    label, as.integer(n_success), as.integer(n_total),
    elapsed_fmt, rate_fmt
  )
  if (as.integer(n_failed) > 0L) {
    msg <- paste0(msg, sprintf(" - %d failure(s)", as.integer(n_failed)))
  }

  .cacs_emit("inform", "progress_summary", msg, phase = label)
  invisible(NULL)
}


#' Write the time a step took
#'
#' The same formatting as the time left, but of the time that has passed, so
#' the line at the end reports the real duration instead of `"0:00"`.
#'
#' @param secs A single number of seconds.
#' @keywords internal
#' @noRd
.format_elapsed <- function(secs) {
  .format_duration(secs)
}


#' Make the reporter for one step
#'
#' The functions it returns share one start time and one counter. What they
#' do depends on the mode from `.cacs_progress_mode()`:
#'   * `"silent"`  - nothing is shown.
#'   * `"bookend"` - `tick()` only counts; `finish()` shows the line at the
#'                   end.
#'   * `"bar"`     - `tick()` also shows a line with the count and the time
#'                   left, carrying the class
#'                   `catchmentACS_message_progress_tick`; `finish()` shows
#'                   the line at the end.
#'
#' With more than 50 units the tick lines are thinned out: the first, the
#' last, and every tenth in between, or every `k`-th with
#' `options(catchmentACS.progress_throttle = k)`. The time left never grows
#' within one reporter, and `finish()` shows its line only once.
#'
#' Callers register `finish()` with `on.exit()`, so the line at the end is
#' shown even when the step stops with an error.
#'
#' @param n A single integer, the number of iterations expected.
#' @param label A single string naming the step, such as `"Isochrones"`.
#' @param verbose A single TRUE or FALSE, the caller's verbose flag.
#'
#' @return a list of `tick` (which takes `detail = NULL`), `finish` (which
#'   takes `n_success` and `n_failed`), `eta`, and the mode as a string.
#' @keywords internal
#' @noRd
.cacs_progress_reporter <- function(n, label, verbose = TRUE) {
  n           <- as.integer(n)
  label       <- as.character(label)[[1L]]
  mode        <- .cacs_progress_mode(n, verbose)
  start_time  <- .cacs_now()
  state <- new.env(parent = emptyenv())
  state$i             <- 0L
  state$prior_eta_sec <- Inf       # the estimate may only fall
  state$finished      <- FALSE

  eta_closure <- function() {
    if (state$i < 1L || state$i > n) return("--:--")
    elapsed <- as.numeric(difftime(.cacs_now(), start_time, units = "secs"))
    if (!is.finite(elapsed) || elapsed < 0) elapsed <- 0
    # Never above the previous estimate of this reporter.
    raw_remaining <- if (state$i >= n) 0 else (elapsed / state$i) * (n - state$i)
    clamped <- min(state$prior_eta_sec, raw_remaining)
    if (is.finite(clamped) && clamped >= 0) state$prior_eta_sec <- clamped
    if (!is.finite(clamped) || clamped < 0) return("--:--")
    .format_duration(clamped)
  }

  tick_closure <- function(detail = NULL) {
    state$i <- state$i + 1L
    if (!identical(mode, "bar")) return(invisible(NULL))
    eta_str <- eta_closure()
    detail_str <- if (is.null(detail) || !nzchar(as.character(detail)[[1L]])) {
      ""
    } else {
      paste0(" - ", as.character(detail)[[1L]])
    }
    msg <- sprintf(
      "%s: %d/%d (ETA %s)%s",
      label, state$i, n, eta_str, detail_str
    )
    throttle_every <- getOption("catchmentACS.progress_throttle", 10L)
    if (!is.numeric(throttle_every) || length(throttle_every) != 1L ||
        is.na(throttle_every) || throttle_every < 1L) {
      throttle_every <- 10L
    }
    throttle_every <- as.integer(throttle_every)
    emit_tick <- n <= 50L || state$i == 1L || state$i == n ||
      ((state$i - 1L) %% throttle_every == 0L)
    if (isTRUE(emit_tick)) {
      .cacs_emit("inform", "progress_tick", msg, phase = label)
    }
    invisible(NULL)
  }

  finish_closure <- function(n_success = n, n_failed = 0L) {
    if (isTRUE(state$finished)) return(invisible(NULL))
    state$finished <- TRUE
    if (identical(mode, "silent")) return(invisible(NULL))
    .cacs_progress_summary(
      start_time = start_time,
      n_total    = n,
      n_success  = as.integer(n_success),
      n_failed   = as.integer(n_failed),
      label      = label,
      verbose    = verbose
    )
  }

  list(
    tick   = tick_closure,
    finish = finish_closure,
    eta    = eta_closure,
    mode   = mode
  )
}
