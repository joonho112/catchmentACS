# ============================================================================
# catchmentACS-conditions.R - top-level condition class documentation.
# ============================================================================


#' catchmentACS condition classes
#'
#' catchmentACS emits standardized, classed conditions (R's umbrella term for
#' messages, warnings, and errors) so that production scripts, Quarto
#' documents, and tests can respond to progress, cache, ACS filtering,
#' geometry, rate, schema, and provider events without parsing message text.
#'
#' Every standardized condition inherits from the root class
#' `catchmentACS_condition`. Within that, messages also inherit from
#' `catchmentACS_message`, warnings from `catchmentACS_warning`, and errors
#' from `catchmentACS_error`; each of those then has more specific leaf
#' classes (see the class tree below). To catch a condition, subscribe to the
#' class at whatever level of specificity you need: a single leaf class for
#' one exact event, or a parent class such as `catchmentACS_warning` to catch
#' every warning the package emits. Use [cacs_capture_conditions()] for the
#' common cases, or write your own `tryCatch()` / `withCallingHandlers()`
#' handlers keyed on these class names.
#'
#' @section Class tree:
#' \preformatted{
#' catchmentACS_condition
#' |-- catchmentACS_message
#' |   |-- catchmentACS_message_water_tract_filter
#' |   |-- catchmentACS_message_progress
#' |   |   |-- catchmentACS_message_progress_tick
#' |   |   `-- catchmentACS_message_progress_summary
#' |   |-- catchmentACS_message_res_default_changed
#' |   |-- catchmentACS_message_demo_budget_protected
#' |   |-- catchmentACS_message_rate_first_changed
#' |   |-- catchmentACS_message_listcol_iso_filled
#' |   |-- catchmentACS_message_perf_fix_applied
#' |   |-- catchmentACS_message_resolve_site
#' |   `-- catchmentACS_message_cache
#' |       |-- catchmentACS_message_cache_legacy_invalidated
#' |       |-- catchmentACS_message_cache_fingerprint_mismatch
#' |       `-- catchmentACS_message_cache_enabled_announce
#' |-- catchmentACS_warning
#' |   |-- catchmentACS_warning_cache
#' |   |   `-- catchmentACS_warning_cache_stale_suspect
#' |   |-- catchmentACS_warning_runtime
#' |   |   `-- catchmentACS_warning_carrier_missing
#' |   |-- catchmentACS_warning_rate_out_of_range
#' |   |-- catchmentACS_warning_resolve_site_distant
#' |   |-- catchmentACS_warning_geometry_skip
#' |   |-- catchmentACS_warning_provenance
#' |   |-- catchmentACS_warning_variable
#' |   `-- catchmentACS_warning_partial
#' `-- catchmentACS_error
#'     |-- catchmentACS_error_annulus_input
#'     |-- catchmentACS_error_schema
#'     |-- catchmentACS_error_geometry
#'     |-- catchmentACS_error_credential
#'     |-- catchmentACS_error_network
#'     |-- catchmentACS_error_operator
#'     |-- catchmentACS_error_variable
#'     `-- catchmentACS_error_missing_suggest
#' }
#'
#' @section Common classes:
#' `catchmentACS_message_water_tract_filter` announces ACS water or
#' special-purpose tracts dropped by `cacs_acs_prefetch()`.
#'
#' `catchmentACS_message_progress_tick` and
#' `catchmentACS_message_progress_summary` report long-running phase progress.
#'
#' `catchmentACS_message_res_default_changed` announces the grid resolution
#' (`res`) chosen for OSRM when you omit the `res` argument.
#'
#' `catchmentACS_message_demo_budget_protected` announces that the omitted-`res`
#' OSRM default was downgraded from `70L` to `30L` when `osrm_mode = "demo"`,
#' protecting against HTTP 429 rate limits on the public OSRM demo endpoint.
#' Suppress via `options(catchmentACS.osrm_demo_budget_protect = FALSE)`.
#'
#' `catchmentACS_message_rate_first_changed` announces a row-order change when
#' coercing a run result to a tibble: derived rate rows surface above the
#' source ACS rows within each site block. Disable the rate-first lift via
#' `rate_first = FALSE` or
#' `options(catchmentACS.rate_first_default = FALSE)`.
#'
#' `catchmentACS_message_listcol_iso_filled` announces that the
#' `cacs_run(output = "list_column")` `$isochrone` column was populated from
#' resolved isochrones.
#'
#' `catchmentACS_message_perf_fix_applied` announces that an OSRM endpoint is
#' using the fast-server path rather than the public demo server's
#' forced-sleep path.
#'
#' `catchmentACS_message_resolve_site` announces that a visualization `(lat,
#' lon)` input was resolved to the nearest `site_id`.
#'
#' `catchmentACS_message_cache_*` and `catchmentACS_warning_cache_*` describe
#' cache hits, legacy invalidation, fingerprint mismatch, cache enablement, and
#' stale-cache suspicion.
#'
#' `catchmentACS_warning_rate_out_of_range` and
#' `catchmentACS_warning_carrier_missing` describe rate derivation and MOE
#' fallback concerns.
#'
#' `catchmentACS_warning_resolve_site_distant` warns that a visualization
#' `(lat, lon)` input resolved to a site more than 5 km away.
#'
#' `catchmentACS_warning_geometry_skip` reports degenerate tract geometry that
#' was skipped while preserving the rest of an intersection.
#'
#' `catchmentACS_warning_partial` reports partial isochrone failure rows carried
#' through by `cacs_run()`.
#'
#' `catchmentACS_error_missing_suggest` reports a missing optional package for
#' visualization helpers.
#'
#' @section Backward compatibility:
#' Two legacy error-class aliases are retained for handlers written against
#' early development builds. Annulus-input errors carry
#' `cacs_error_annulus_input` alongside the canonical
#' `catchmentACS_error_annulus_input`, and schema-family errors carry
#' `cacs_error_schema` alongside the canonical `catchmentACS_error_schema`.
#' New code should prefer the canonical `catchmentACS_*` class names; the
#' legacy aliases are compatibility shims.
#'
#' @section Capturing conditions:
#' Use [cacs_capture_conditions()] for the common case:
#'
#' \preformatted{
#' conds <- cacs_capture_conditions(
#'   cacs_run(sites = sites, state = "AL", verbose = TRUE)
#' )
#'
#' water <- cacs_capture_conditions(
#'   cacs_acs_prefetch(state = "AL", verbose = TRUE),
#'   classes = "water_tract_filter"
#' )
#' }
#'
#' To write custom handlers directly, subscribe to a leaf class or to a parent
#' class:
#'
#' \preformatted{
#' withCallingHandlers(
#'   cacs_run(sites = sites, state = "AL"),
#'   catchmentACS_message_progress = function(m) {
#'     # progress messages
#'     invokeRestart("muffleMessage")
#'   },
#'   catchmentACS_warning = function(w) {
#'     # warnings
#'     invokeRestart("muffleWarning")
#'   }
#' )
#' }
#'
#' @section Muting:
#' `suppressMessages()` mutes message conditions and `suppressWarnings()` mutes
#' warning conditions. Prefer [cacs_capture_conditions()] when you need an audit
#' table rather than complete silence.
#'
#' @seealso [cacs_capture_conditions()] for capturing these conditions into a
#'   tibble, [cacs_run()] for the pipeline that emits most of them, and
#'   [cacs_describe()] for a plain-language summary of a result.
#' @family validation and conditions
#' @name catchmentACS-conditions
#' @aliases catchmentACS_condition catchmentACS_message catchmentACS_warning
#' @aliases catchmentACS_error catchmentACS-conditions
NULL
