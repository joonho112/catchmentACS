# ============================================================================
# cli.R - wrapper factories around cli::cli_abort / cli_warn / cli_inform.
#         Sec. 13.4 (3-line format) + Sec. 13.5 (condition class hierarchy) +
#         Sec. 24.7 (must be re-emittable by .call_with_capture).
# ============================================================================
#
# Class chain (per Sec. 13.5):
#   c("catchmentACS_<kind>_<family>",
#     "catchmentACS_<kind>",
#     "catchmentACS_condition")
# This permits per-family `tryCatch(catchmentACS_error_schema = ...)` AND
# per-kind `tryCatch(catchmentACS_error = ...)` parent dispatch.
#
# All wrappers forward `.envir = rlang::caller_env()` to cli so that
# {glue} interpolation in `message` evaluates in the calling function's
# environment (e.g. {sum(is_valid)} expressions in .repair_geometry).
# ============================================================================


# ---- Private helper: build standardized class chain ------------------------

.cacs_cond_classes <- function(kind, family) {
  kind   <- match.arg(kind, c("error", "warning", "message"))
  family <- match.arg(family, c(
    # error-only families
    "schema", "credential", "geometry", "operator", "network",
    # warn-only families
    "provenance", "runtime", "geometry_skip",
    "rate_out_of_range", "carrier_missing",
    # cache-specific warning family
    "cache_stale_suspect",
    # message-only families
    "cache", "cache_legacy_invalidated", "cache_fingerprint_mismatch",
    "cache_enabled_announce", "progress", "water_tract_filter",
    "res_default_changed", "perf_fix_applied", "resolve_site",
    # v0.2 F1 progress sub-families (Step 5.1 hybrid reporter)
    "progress_tick", "progress_summary",
    # v0.4 NEW message families (plan §sec-step-1-2)
    "demo_budget_protected",   # Issue 002 — OSRM omitted-res demo downgrade
    "rate_first_changed",      # Issue 004 — as_tibble rate-first migration
    "listcol_iso_filled",      # Issue 003 — list-column resolved iso fill
    # v0.4 NEW warning family (plan §sec-step-4-2)
    "provider_quota_exhausted", # Issue 002 Option 4 — pre-flight quota probe
    # cross-kind (error + warning)
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


# ---- Private helper: cli formatting + rlang signaling ----------------------

#' Emit a standardized catchmentACS condition (internal)
#'
#' Centralizes condition emission so `cli` owns only message formatting while
#' `rlang` owns signaling. This keeps the class hierarchy catchable under
#' non-interactive `Rscript` while preserving cli bullet/glue rendering.
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


# ---- ABORT factories (4) ---------------------------------------------------

#' Schema-class abort wrapper (internal)
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

#' Build one structured schema issue (internal)
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

#' Convert structured schema issues to a stable tibble (internal)
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

#' Shared schema migration pointer (internal)
#' @keywords internal
#' @noRd
.schema_vignette_hint <- function() {
  paste0(
    "For full schema examples, run: ",
    "vignette('porting-v01-to-v03', package = 'catchmentACS')"
  )
}

#' Schema abort wrapper with issue/fix/example template (internal)
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

#' Annulus-topology abort wrapper (internal)
#'
#' Prepends the v0.3 UF-1 leaf class while preserving legacy aliases and the
#' existing schema family chain so older handlers continue to work.
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

#' Credential-class abort wrapper (internal)
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

#' Geometry-class abort wrapper (internal)
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

#' Operator-class abort wrapper (internal)
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

#' Variable-class abort wrapper (internal)
#'
#' Sec. 20.7 - fires when all requested ACS variables are invalid in the
#' tidycensus codebook (E-20-07). Partial invalidity flows through
#' \code{.cli_warn_variable()} instead.
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

#' Network-class abort wrapper (internal)
#'
#' Sec. 20.7 - fires when transient HTTP 5xx retries are exhausted (E-20-14).
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


# ---- WARN factories (3) ----------------------------------------------------

#' Provenance-class warning (internal)
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

#' Runtime-class warning (internal)
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

#' Variable-class warning (internal)
#'
#' Sec. 20.7 - fires when a subset of requested ACS variables are missing in
#' the tidycensus codebook (W-20-04 partial skip).
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

#' Rate-audit warning wrapper (internal)
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

#' Carrier-missing derived-rate warning wrapper (internal)
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


# ---- INFORM factories ------------------------------------------------------

#' Cache-event message wrapper (internal)
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

#' Cache legacy-invalidation message wrapper (internal)
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

#' Cache fingerprint-mismatch message wrapper (internal)
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

#' Cache-enabled announcement message wrapper (internal)
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

#' Suspicious cache-hit warning wrapper (internal)
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

#' OSRM res default-change message wrapper (internal)
#'
#' v0.3 UF-2: omitted OSRM `res` now resolves to 70L. The notice is classed
#' and throttled by rlang to once per R session.
#' @keywords internal
#' @noRd
.cli_inform_res_default_changed <- function() {
  .cacs_emit(
    level = "inform",
    family = "res_default_changed",
    c(
      "catchmentACS v0.3.0 changed the OSRM isochrone `res` default from 50 to 70 for more detailed polygon geometry.",
      "i" = "Specify `res = 50L` explicitly to keep prior behavior.",
      "i" = "Specify `res = 70L` explicitly to acknowledge the new default and silence this notice.",
      "i" = "This notice fires once per session."
    ),
    .frequency = "once",
    .frequency_id = "catchmentACS_res_default_changed"
  )
}

#' v0.4 Issue 002 Step 4.2 — provider-quota-exhausted warning (internal)
#'
#' Emitted by `cacs_validate_osrm_endpoint()` when a pre-flight probe returns
#' HTTP 429, signaling that the OSRM public-demo endpoint quota is currently
#' exhausted. Caller can choose to switch to `osrm_mode = "docker"` or wait.
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

#' v0.4 Issue 002 — OSRM demo-budget protection notice (internal)
#'
#' Emitted by `cacs_isochrone()` (once per session) when `osrm_mode = "demo"`
#' and `res` is omitted and `catchmentACS.osrm_demo_budget_protect = TRUE`,
#' announcing the v0.4 down-resolution of the omitted-res default from 70L
#' to 30L for the public demo endpoint. Explicit `res = 70L` and
#' `osrm_mode = "docker"` paths remain silent.
#' @keywords internal
#' @noRd
.cli_inform_demo_budget_protected <- function() {
  .cacs_emit(
    level = "inform",
    family = "demo_budget_protected",
    c(
      "catchmentACS v0.4.0 downgraded the OSRM omitted-`res` default from 70L to 30L for the public demo endpoint to protect against HTTP 429 rate-limits.",
      "i" = "Specify `res = 70L` explicitly to keep the v0.3 high-resolution default.",
      "i" = "Switch to `osrm_mode = \"docker\"` (local OSRM) for the 70L default without quota cost.",
      "i" = "Set `options(catchmentACS.osrm_demo_budget_protect = FALSE)` to disable this downgrade.",
      "i" = "This notice fires once per session."
    ),
    .frequency = "once",
    .frequency_id = "catchmentACS_demo_budget_protected"
  )
}

#' v0.4 Issue 004 — as_tibble rate-first migration notice (internal)
#'
#' Emitted by `as_tibble.cacs_run_result()` (once per session) when the
#' `rate_first = TRUE` default re-orders rows so derived rate rows surface
#' above source ACS B-codes within each site block. The rate-first lift can be
#' disabled via `rate_first = FALSE` or
#' `options(catchmentACS.rate_first_default = FALSE)`.
#' @keywords internal
#' @noRd
.cli_inform_rate_first_changed <- function() {
  .cacs_emit(
    level = "inform",
    family = "rate_first_changed",
    c(
      "catchmentACS v0.4.0 changed `as_tibble(cacs_run_result)` row order so derived rate rows surface above source ACS variables within each site block.",
      "i" = "Pass `rate_first = FALSE` to skip the rate-first lift and return the underlying long-format order.",
      "i" = "Set `options(catchmentACS.rate_first_default = FALSE)` to make that opt-out global.",
      "i" = "Scripts that select by `variable %in% rate_vars` are unaffected.",
      "i" = "This notice fires once per session."
    ),
    .frequency = "once",
    .frequency_id = "catchmentACS_rate_first_changed"
  )
}

#' v0.5 — list-column resolved-iso fill notice (internal)
#'
#' Emitted by `.pivot_to_list_column()` (once per `cacs_run()` call when
#' resolved isochrones are available) announcing that the `$isochrone`
#' list-column was populated from one-row `sf` cells instead of the legacy
#' `<NULL>` placeholder.
#' @keywords internal
#' @noRd
.cli_inform_listcol_iso_filled <- function() {
  # Frequency is left at default (always); the caller in `.pivot_to_list_column()`
  # gates per-cacs_run-call via `attr(out, "iso_was_filled")`.
  .cacs_emit(
    level = "inform",
    family = "listcol_iso_filled",
    c(
      "v0.5: `cacs_run(output = \"list_column\")` fills the `$isochrone` column from resolved isochrones when available.",
      "i" = "Computed and precomputed paths now carry one row per site/time pair."
    )
  )
}

#' OSRM performance-path message wrapper (internal)
#'
#' Phase 6: emits when the effective OSRM routing endpoint avoids the public
#' demo-server forced-sleep path.
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

#' Site-coordinate resolution message wrapper (internal)
#'
#' Phase 7 C-06: emitted when a documented `(lat, lon)` plot input is mapped
#' to the nearest available `site_id` for Stage 4 / pipeline filtering.
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

#' Site-coordinate distant-resolution warning wrapper (internal)
#'
#' Phase 7 C-06: emitted in addition to the resolution notice when a
#' `(lat, lon)` plot input is more than 5 km from the nearest site.
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

#' Progress-event message wrapper (internal)
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


# ---- BUG-001 (F3) water-tract filter helpers ------------------------------

#' Inform caller that water/special-purpose tracts were dropped (internal)
#'
#' Emitted by `.drop_water_tracts()` when the hybrid water-tract filter
#' (regex `^[0-9]{2}[0-9]{3}99[0-9]{4}$` + `st_area() <= 0 | !is.finite()`
#' fallback) drops one or more tracts from `cacs_acs_prefetch()` output.
#' Caps the printed GEOID list at 5 with a `(+N more)` suffix so prefetches
#' touching many coastal/water tracts stay readable.
#'
#' Condition class: `catchmentACS_message_water_tract_filter` /
#' `catchmentACS_message` / `catchmentACS_condition`.
#'
#' @param geoids character; unique GEOIDs dropped by the filter.
#' @param reasons character; one-per-GEOID diagnostic reason
#'   (`"geoid_pattern"`, `"zero_area"`, or `"geoid_pattern+zero_area"`).
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
      "Dropped {n_total} water/special-purpose tract(s) from ACS prefetch.",
      "i" = "GEOID(s): {body}{tail}.",
      "*" = "Set {.arg drop_water_tracts = FALSE} to retain."
    ),
    phase = "acs"
  )
}


#' Warn-and-skip wrapper for degenerate-geometry tracts (internal)
#'
#' BUG-001 (F3) Layer (b): when `cacs_intersect_weight()` encounters a
#' subset of tracts with non-positive or non-finite area (and at least
#' one healthy tract remains), demote the v0.1 fail-loud abort to a
#' warn-and-skip path. The abort is preserved only for the
#' all-degenerate case (fires `catchmentACS_error_geometry` upstream).
#'
#' Condition class: `catchmentACS_warning_geometry_skip` /
#' `catchmentACS_warning` / `catchmentACS_condition`.
#'
#' @param geoids character; GEOIDs being skipped.
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
  .cacs_emit("warn", "geometry_skip", msg)
}


# ============================================================================
# v0.2 F1 verbose progress reporter (hybrid bar/line mode).
#
# Threshold-based reporter: bookend when N<5, cli_progress_bar when N>=5.
# Tri-state option `catchmentACS.progress` ("auto"/"off"/"force"), CI-safe
# fallback via classed cli::cli_inform() messages (NOT cli_progress_step()).
#
# Mode resolver priority chain (highest first):
#   1. verbose == FALSE                                   -> "silent"
#   2. Sys.getenv("CACS_QUIET") == "1"                    -> "silent"
#   3. getOption("catchmentACS.progress") == "off"        -> "silent"
#                                              == "force" -> "bar"
#                                              == "auto"  -> fall through
#   4. N < 5                                              -> "bookend"
#   5. N >= 5                                             -> "bar"
#
# Conditions emitted carry the standardized class chain so that the
# `.call_with_capture()` handler in `cacs_run()` (§24.7) re-emits them via
# the existing `catchmentACS_message` parent dispatch without per-family
# edits. Two new sub-families are registered above in `.cacs_cond_classes()`:
#   * `progress_tick`    -> per-event tick (bar mode only)
#   * `progress_summary` -> end-of-call summary (always except silent)
# ============================================================================


#' Resolve progress reporting mode (internal)
#'
#' Pure resolver - no side effects. Returns one of `c("silent", "bookend",
#' "bar")` per the §5.1 locked priority chain.
#'
#' @param n integer length(1); the total number of iterations (sites,
#'   tracts, etc.) the caller will tick across.
#' @param verbose logical(1); the caller's verbose flag.
#' @keywords internal
#' @noRd
.cacs_progress_mode <- function(n, verbose) {
  if (!isTRUE(verbose)) return("silent")
  if (identical(Sys.getenv("CACS_QUIET"), "1")) return("silent")
  opt <- getOption("catchmentACS.progress", "auto")
  if (identical(opt, "off"))   return("silent")
  if (identical(opt, "force")) return("bar")
  # opt == "auto" (or unset) -> threshold dispatch
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 5L) {
    return("bookend")
  }
  "bar"
}


#' Current time helper for progress tests (internal)
#' @keywords internal
#' @noRd
.cacs_now <- function() Sys.time()


#' Format seconds as M:SS / H:MM:SS (internal)
#'
#' @param secs numeric(1) seconds.
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


#' Format an ETA (internal)
#'
#' Monotone non-increasing across calls within a single reporter (clamped
#' to `min(prior_eta, computed_eta)` by the closure that owns prior_eta).
#' Uses a simple `M:SS` / `HH:MM:SS` sprintf fallback - prettyunits is not a
#' v0.2 dependency.
#'
#' @param elapsed_sec numeric(1); seconds elapsed since `start_time`.
#' @param i integer(1); current iteration index (1-based).
#' @param n integer(1); total iterations.
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


#' Emit standardized end-of-call progress summary (internal)
#'
#' Format: "{label} complete: {n_success}/{n_total} in {elapsed} ({rate}/sec)".
#' Class chain: `catchmentACS_message_progress_summary` /
#' `catchmentACS_message_progress` / `catchmentACS_message` /
#' `catchmentACS_condition`. The `progress_summary` sub-class is registered
#' in `.cacs_cond_classes()` above.
#'
#' Always emitted except in silent mode (caller guards via
#' `.cacs_progress_mode()`).
#'
#' @param start_time POSIXct(1); when the reporter started.
#' @param n_total integer(1); total iterations expected.
#' @param n_success integer(1); successful iterations.
#' @param n_failed integer(1); failed iterations.
#' @param label character(1); short phase label (e.g. "Isochrones").
#' @param verbose logical(1); caller's verbose flag (no-op when FALSE).
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


#' Format an elapsed-time interval (internal)
#'
#' Companion to `.format_eta()`. Unlike ETA which clamps to "0:00" at
#' i == n, this renders the actual elapsed wallclock as `M:SS` /
#' `H:MM:SS` for the end-of-call summary.
#'
#' @param secs numeric(1) seconds.
#' @keywords internal
#' @noRd
.format_elapsed <- function(secs) {
  .format_duration(secs)
}


#' Construct a verbose progress reporter (internal)
#'
#' Returns a list of closures (`tick`, `finish`, `eta`) over a shared start
#' time + counter state. Selects the reporting mode via
#' `.cacs_progress_mode()`:
#'   * `"silent"`  - all closures are no-ops.
#'   * `"bookend"` - `tick()` no-op; `finish()` emits the standardized summary.
#'   * `"bar"`     - `tick()` emits a classed `cli_inform()` message (CI-safe
#'                  vs. `cli_progress_step()`) carrying the
#'                  `catchmentACS_message_progress_tick` class; `finish()`
#'                  emits the summary.
#'
#' The ETA is monotonic non-increasing across ticks within a single reporter
#' (clamped via `min(prior_eta, computed_eta)`).
#'
#' Always pair construction with `on.exit(prog$finish(...))` at the call site
#' so the summary lands even on mid-loop abort.
#'
#' @param n integer(1); total iterations expected.
#' @param label character(1); short phase label (e.g. "Isochrones").
#' @param verbose logical(1); caller's verbose flag.
#'
#' @return list(tick, finish, eta) of closures (tick takes `detail = NULL`,
#'   finish takes `n_success` / `n_failed`).
#' @keywords internal
#' @noRd
.cacs_progress_reporter <- function(n, label, verbose = TRUE) {
  n           <- as.integer(n)
  label       <- as.character(label)[[1L]]
  mode        <- .cacs_progress_mode(n, verbose)
  start_time  <- .cacs_now()
  state <- new.env(parent = emptyenv())
  state$i             <- 0L
  state$prior_eta_sec <- Inf       # for monotonic clamping
  state$finished      <- FALSE

  eta_closure <- function() {
    if (state$i < 1L || state$i > n) return("--:--")
    elapsed <- as.numeric(difftime(.cacs_now(), start_time, units = "secs"))
    if (!is.finite(elapsed) || elapsed < 0) elapsed <- 0
    # Monotone clamp: never grow above the previous estimate within this reporter.
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
