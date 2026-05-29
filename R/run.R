# ============================================================================
# run.R - cacs_run() orchestrator entry point (Sec. 24, Phase 7).
#
# Houses the public `cacs_run()` function -- the single user entry point for
# the catchmentACS package -- plus 4 internal helpers:
#   - `.validate_run_arg_list()`     (Step 7.1)
#   - `.call_with_capture()`         (Step 7.2)
#   - `.build_na_propagation_rows()` (Step 7.2)
#   - `.pivot_to_list_column()`      (Step 7.3)
#
# Step 7.1 implements Sec. 24.3 steps 1-4 (thin arg validation + execution
# path determination + sub-call arg-list validation + warning collector init).
# Step 7.2 implements Sec. 24.3 steps 5-11 (bypass-aware 5-call sequence with
# phase-keyed warning capture + failed-pair separation + partial-success NA
# propagation). Step 7.3 implements Sec. 24.3 step 12 + Sec. 24.5 3-mode
# output dispatch (long / list_column / both) + Sec. 24.8 16-field
# `cacs_run_provenance` + sub-call attribute carry-through.
#
# 4-tier arg policy (Sec. 24.2):
#   1. Argument boundary  : Thin fail-loud + delegate (E-24-01..05).
#   2. Bypass dispatch    : Schema check + skip Sec.19/Sec.20 -- Step 7.2.
#   3. Partial-success    : NA-propagation row append -- Step 7.2.
#   4. Output dispatch    : Pure reshape (long / list_column / both) -- Step 7.3.
#
# Cross-ref: Sec. 11.2 (21-arg LOCKED signature), Sec. 24 (full spec),
# Sec. 42 (Phase 7 implementation guide).
# ============================================================================


#' Run the catchmentACS orchestrator
#'
#' Single entry point for the catchmentACS package. Wraps the five-call
#' sequence ([cacs_acs_prefetch()] -> [cacs_isochrone()] ->
#' [cacs_intersect_weight()] -> [cacs_propagate_moe()] ->
#' [cacs_derive_rates()]) into a one-line invocation. Supports four execution
#' paths via bypass dispatch over `precomputed_isochrones` and `acs`. See
#' `vignette("getting-started")` for a walkthrough and `vignette("methodology")`
#' for the full algorithm.
#'
#' Returns canonical long output by default, or list-column or both
#' representations when requested. Per-pair isochrone failures returned by a
#' provider are preserved as NA-propagated rows with
#' `failure_origin = "isochrone"`. Batch-level provider errors still abort so
#' callers do not mistake a failed run for a complete analytic result.
#'
#' @param sites `data.frame`/`tbl_df` or `sf` with `site_id`, `lon`, `lat`
#'   columns.
#' @param state `character(1)` USPS code (e.g. `"AL"`) or FIPS code.
#' @param year Integer ACS terminal vintage year (default `2023L`).
#' @param drive_times Numeric vector of drive-time bands in minutes
#'   (default `c(5, 10, 15)`).
#' @param variables Character vector of ACS variable codes, or `NULL` for
#'   the package default (`cacs_acs_default_vars`).
#' @param provider One of `"osrm"`, `"ors"`, `"mapbox"`, `"r5r"`. OSRM and
#'   ORS are implemented; Mapbox and r5r are reserved fail-loud stubs.
#' @param weight_method One of `"area"` (default) or `"population"`. Area
#'   weighting is implemented; population weighting is reserved and fails loud
#'   in the current release.
#' @param moe_formula `NULL` for automatic per-variable selection of the
#'   margin-of-error formula, or an explicit formula override (scalar or named
#'   list per variable). Top-level only -- passing `moe_args$formula` is
#'   reserved and aborts; use `moe_formula` instead.
#' @param output One of `"long"` (default), `"list_column"`, `"both"`.
#' @param precomputed_isochrones An `sf` to bypass [cacs_isochrone()] and
#'   feed [cacs_intersect_weight()] directly, or `NULL` (default) to compute
#'   via `provider`.
#' @param cache_dir Character path or `NULL` for `cacs_cache_dir()`.
#' @param acs An `sf` to bypass [cacs_acs_prefetch()], or `NULL` (default) to
#'   fetch via tidycensus.
#' @param bg_pop_sf Reserved for future `weight_method = "population"`
#'   support. Ignored when `weight_method = "area"`; population weighting
#'   fails loud before this surface is used in the current release.
#' @param rates Named `list` of `c(num, den)` ACS code pairs; default
#'   `cacs_acs_default_rates` (the value-frozen curated catalogue). Custom
#'   rate catalogues are deferred and fail validation.
#' @param formula_dispatch One of `"general_ratio_conservative"` (default),
#'   `"proportion_subset"`, `"auto"`. Top-level only -- passing
#'   `rate_args$formula_dispatch` is reserved and aborts; use `formula_dispatch`
#'   instead.
#' @param iso_args Named `list` of [cacs_isochrone()] passthrough arguments.
#'   Unknown keys warn and are dropped.
#' @param acs_args Named `list` of [cacs_acs_prefetch()] passthrough arguments.
#'   Unknown keys warn and are dropped.
#' @param weight_args Named `list` of [cacs_intersect_weight()] passthrough
#'   arguments. Unknown keys warn and are dropped.
#' @param moe_args Named `list` of [cacs_propagate_moe()] passthrough
#'   arguments. Unknown keys warn and are dropped. The reserved `formula` key
#'   aborts -- use top-level `moe_formula` instead.
#' @param rate_args Named `list` of [cacs_derive_rates()] passthrough
#'   arguments. Reserved as an empty surface in the current release. The
#'   reserved `formula_dispatch` key aborts -- use top-level `formula_dispatch`
#'   instead.
#' @param verbose `TRUE` (default) to emit classed progress messages. The flag
#'   is forwarded to all five sub-calls while phase-keyed warning capture is
#'   preserved.
#'
#' @return A `tbl_df` in the canonical long schema (when `output = "long"`),
#'   or a list-column tibble (when `output = "list_column"`), or a named
#'   `list(long = ..., list_column = ...)` (when `output = "both"`). The
#'   list-column representation includes one-row `sf` cells in `$isochrone`
#'   for every resolved `(site_id, drive_time_min)` pair on computed and
#'   precomputed isochrone paths; unmatched defensive helper paths remain
#'   `NULL`.
#'
#'   The long schema is the **27-column** canonical layout (23 mandatory
#'   columns plus 4 carrier numeric columns left-joined by
#'   [cacs_propagate_moe()] for margin-of-error math). The `n_tracts_num` and
#'   `n_tracts_den` columns sit immediately after `n_tracts`; both are
#'   populated on derived-rate rows from per-numerator and per-denominator
#'   carrier counts and are `NA_integer_` on source-variable rows (see the
#'   "Policy on `n_tracts_num` and `n_tracts_den`" section of
#'   [cacs_derive_rates()]). The returned object carries
#'   `cacs_schema_version = "1.0"`, run-level provenance
#'   (`cacs_run_provenance`), a phase-keyed warning list
#'   (`cacs_run_warnings`), and sub-call provenance attributes
#'   (`cacs_aggregation_provenance`, `cacs_moe_provenance`,
#'   `cacs_rate_provenance`, `cacs_confidence_level`) when the underlying
#'   sub-calls populated them.
#'
#' @section Provider and weighting status:
#' The current release supports live OSRM and ORS isochrone paths plus fully
#' offline bypass paths through `precomputed_isochrones` and `acs`. Mapbox and
#' r5r remain reserved provider names that fail loud if selected. Area
#' weighting is the only implemented aggregation method; `weight_method =
#' "population"` is reserved for a future release and aborts before network or
#' spatial work begins.
#'
#' @section Progress reporting:
#' A verbose progress reporter covers the three long-running steps
#' ([cacs_isochrone()], [cacs_acs_prefetch()], [cacs_intersect_weight()]). The
#' orchestrator also propagates progress to [cacs_propagate_moe()] and
#' [cacs_derive_rates()] with one summary condition per call. The reporter has
#' three modes, auto-selected per call:
#' \itemize{
#'   \item \strong{silent} -- no progress output at all.
#'   \item \strong{bookend} -- start + end summary only (no per-event tick);
#'     used for small batches and the single-shot ACS prefetch (N = 1).
#'   \item \strong{bar} -- per-tick classed condition with monotone ETA +
#'     rate, CI-safe via \code{rlang::inform()} after \pkg{cli} formatting
#'     (not \code{cli_progress_step()}).
#' }
#' Resolution follows a 5-rule priority chain (highest first): (1)
#' \code{verbose = FALSE} -> silent; (2) \code{Sys.getenv("CACS_QUIET") ==
#' "1"} -> silent; (3) \code{getOption("catchmentACS.progress")} ==
#' \code{"off"} -> silent, \code{"force"} -> bar, \code{"auto"} (default)
#' -> fall through; (4) N < 5 -> bookend; (5) N >= 5 -> bar. The N axis
#' is per-iteration (sites for \code{cacs_isochrone()}, (site, drive_time)
#' pairs for \code{cacs_intersect_weight()}, fixed at 1 for
#' \code{cacs_acs_prefetch()}, \code{cacs_propagate_moe()}, and
#' \code{cacs_derive_rates()}). Ticks emit classed conditions
#' (\code{catchmentACS_message_progress_tick} /
#' \code{catchmentACS_message_progress_summary}) so downstream capture via
#' \code{withCallingHandlers()} works without coupling to \pkg{cli} internals.
#' \cr\cr
#' \strong{Global toggles:}
#' \itemize{
#'   \item Disable everywhere: \code{Sys.setenv(CACS_QUIET = "1")} or
#'         \code{options(catchmentACS.progress = "off")}.
#'   \item Force the bar at any N (including N = 1):
#'         \code{options(catchmentACS.progress = "force")}.
#'   \item Restore auto (default): \code{options(catchmentACS.progress =
#'         "auto")} or \code{options(catchmentACS.progress = NULL)}.
#' }
#' \code{cacs_run(verbose = TRUE)} forwards progress reporting to all five
#' sub-calls. Warnings still route through the phase-keyed aggregation
#' contract, while progress messages remain ordinary classed messages that
#' downstream callers can catch with \code{catchmentACS_message_progress}.
#'
#' @section Avoiding the carrier-attribute join trap:
#' Do not reach for `attr(cacs_run(...), "cacs_aggregation_carriers")` to
#' recover `weight_sum` keyed by `(site_id, drive_time_min, variable)`:
#' [cacs_derive_rates()] consumes and drops that attribute, so it is
#' **always `NULL` on the final `cacs_run()` output**. Read the first-class
#' `weight_sum` column directly instead (see the section below). For per-rate
#' provenance, use the metadata attribute on the returned `cacs_run_result`:
#' `attr(out, "cacs_run_result_metadata")`. That attribute carries run-level
#' fields including `provider`, `profile`, `drive_times`, site-level success
#' and failure tallies, wall-clock seconds, and the margin-of-error fallback
#' bucket breakdown.
#'
#' @section weight_sum exposure (use the column, not the carrier attribute):
#' The `weight_sum` column is a first-class member of the canonical long
#' output. Downstream consumers should always read it directly off the
#' returned tibble -- for example, `result$weight_sum` or
#' `dplyr::select(result, site_id, drive_time_min, variable, weight_sum)` --
#' rather than reaching into the `cacs_aggregation_carriers` attribute via
#' `attr(out, "cacs_aggregation_carriers")$weight_sum` and joining back by
#' `(site_id, drive_time_min, variable)`. That carrier attribute is
#' **consumed and dropped** by [cacs_derive_rates()], so it is absent on the
#' final `cacs_run()` output, and any code path that tries to recover
#' `weight_sum` from it will silently fail (a `NULL` attribute yields an empty
#' join and `NA` columns) instead of erroring. The column-first read is the
#' only supported access path.
#'
#' @details
#' The orchestrator selects one of four execution paths based on which bypass
#' arguments (`precomputed_isochrones`, `acs`) are supplied:
#' \itemize{
#'   \item Both `NULL` (default): runs the full
#'     [cacs_acs_prefetch()] -> [cacs_isochrone()] ->
#'     [cacs_intersect_weight()] -> [cacs_propagate_moe()] ->
#'     [cacs_derive_rates()] chain.
#'   \item `precomputed_isochrones` supplied: skips [cacs_isochrone()].
#'   \item `acs` supplied: skips [cacs_acs_prefetch()].
#'   \item Both supplied: skips both fetch steps (the fully offline,
#'     no-network mode used by the example below).
#' }
#'
#' @seealso [cacs_isochrone()], [cacs_acs_prefetch()],
#'   [cacs_intersect_weight()], [cacs_propagate_moe()],
#'   [cacs_derive_rates()]. The S3 methods for the `cacs_run_result` return
#'   value are documented at `?print.cacs_run_result`; `print()` and
#'   `summary()` dispatch to those methods because the class chain
#'   `c("cacs_run_result", "tbl_df", "tbl", "data.frame")` is prepended.
#' @family core pipeline
#' @export
#' @examples
#' \donttest{
#' # The example uses bundled fixtures so it exercises the cacs_run()
#' # pipeline without live OSRM or Census API calls. Tier 1 demonstrates
#' # the smallest end-to-end shape; Tiers 2 and 3 are nested inside
#' # \dontrun{} blocks so they stay within the R CMD check examples time
#' # budget and remain runnable interactively against a live osrm +
#' # tidycensus install.
#' iso   <- readRDS(system.file("extdata", "legacy_2025_isochrones.rds",
#'                              package = "catchmentACS"))
#' sites <- readRDS(system.file("extdata", "legacy_2025_sites.rds",
#'                              package = "catchmentACS"))
#' acs   <- readRDS(system.file("extdata", "sample_alabama_subset.rds",
#'                              package = "catchmentACS"))
#'
#' # ----- Tier 1 --- 2-site, 1-drive-time smoke (bundled fixtures, no net) ---
#' iso_subset   <- iso[iso$site_id %in% c("AL_SITE_01", "AL_SITE_02") &
#'                     iso$drive_time_min == 5, ]
#' sites_subset <- sites[sites$site_id %in% c("AL_SITE_01", "AL_SITE_02"), ]
#' out <- suppressWarnings(cacs_run(
#'   sites                  = sites_subset,
#'   state                  = "AL",
#'   year                   = 2023,
#'   drive_times            = 5,
#'   variables              = unname(cacs_acs_default_vars),
#'   provider               = "osrm",
#'   precomputed_isochrones = iso_subset,
#'   acs                    = acs,
#'   weight_method          = "area",
#'   output                 = "long",
#'   verbose                = FALSE
#' ))
#' print(out)        # rich cli-formatted header + tibble preview
#' summary(out)      # per-rate breakdown + n_tracts five-number summary
#' attr(out, "cacs_run_result_metadata")$wall_clock_seconds
#'
#' \dontrun{
#'   # ----- Tier 2 --- 5-site, 3-band batch showing progress UX -------------
#'   # The progress reporter is active inside `cacs_run(verbose = TRUE)` and
#'   # direct sub-calls (see the "Progress reporting" section above).
#'   out_batch <- cacs_run(
#'     sites                  = cacs_alabama_sites[1:5, ],
#'     state                  = "AL",
#'     year                   = 2023,
#'     drive_times            = c(5, 10, 15),
#'     variables              = unname(cacs_acs_default_vars),
#'     provider               = "osrm",
#'     verbose                = TRUE
#'   )
#'   print(out_batch)
#'
#'   # ----- Tier 3 --- end-to-end dplyr / ggplot2 backward-compat -----------
#'   library(dplyr)
#'   # Because the S3 class chain *prepends* `cacs_run_result` over the
#'   # tibble / data.frame chain, every dplyr verb still works -- even when
#'   # the verb strips the metadata attribute (the print method then degrades
#'   # to the generic tibble print).
#'   out_batch |>
#'     dplyr::filter(variable == "poverty_rate") |>
#'     dplyr::arrange(dplyr::desc(estimate))
#' }
#' }
cacs_run <- function(sites,
                     state,
                     year = 2023,
                     drive_times = c(5, 10, 15),
                     variables = NULL,
                     provider = c("osrm", "ors", "mapbox", "r5r"),
                     weight_method = c("area", "population"),
                     moe_formula = NULL,
                     output = c("long", "list_column", "both"),
                     precomputed_isochrones = NULL,
                     cache_dir = NULL,
                     acs = NULL,
                     bg_pop_sf = NULL,
                     rates = cacs_acs_default_rates,
                     formula_dispatch = "general_ratio_conservative",
                     iso_args = list(),
                     acs_args = list(),
                     weight_args = list(),
                     moe_args = list(),
                     rate_args = list(),
                     verbose = TRUE) {

  # --------------------------------------------------------------------------
  # Step 1 - Thin argument validation (Sec. 24.2 4-tier policy: argument
  # boundary = thin fail-loud + delegate). Deep validation of sites / state /
  # year / variables / drive_times / etc. is the responsibility of the
  # downstream sub-functions (Sec. 19 step 1, Sec. 20 step 1, Sec. 21 step 1);
  # `cacs_run()` only enforces the 5 hard E-24-01..05 boundary checks plus
  # 3 `match.arg()` enum locks. Doing more here would duplicate the sub-call
  # validation and risk drift between the two layers.
  # --------------------------------------------------------------------------

  output        <- match.arg(output)
  provider      <- match.arg(provider)
  weight_method <- match.arg(weight_method)

  # Capture wall-clock start for the F6 cacs_run_result_metadata$wall_clock_seconds
  # field (Step 4.2 contract).
  run_start <- Sys.time()

  # E-24-01 sites: non-sf and non-tibble/data.frame aborts. sf inherits
  # data.frame so a single check covers both representations.
  if (!inherits(sites, c("sf", "tbl_df", "data.frame"))) {
    .cli_abort_schema(c(
      "{.arg sites} must be an {.cls sf} or {.cls tbl_df}/{.cls data.frame}.",
      "x" = "Got {.cls {class(sites)[[1L]]}}.",
      "i" = "Pass {.code cacs_alabama_sites} or a tibble/data.frame with {.field site_id}, {.field lon}, {.field lat}."
    ))                                                                  # E-24-01
  }

  # E-24-02 state: scalar character required.
  if (!is.character(state) || length(state) != 1L || is.na(state) ||
      !nzchar(state)) {
    .cli_abort_schema(c(
      "{.arg state} must be a non-empty scalar {.cls character}.",
      "x" = "Got {.cls {class(state)[[1L]]}} of length {.val {length(state)}}.",
      "i" = "Use a USPS code (e.g. {.val AL}) or 2-digit FIPS (e.g. {.val 01})."
    ))                                                                  # E-24-02
  }

  # year: positive integer-ish scalar (deep range / availability check is
  # cacs_acs_prefetch()'s job). Out-of-domain types here would otherwise
  # silently coerce inside `c(list(year = year), ...)`, so the type guard
  # belongs at the boundary.
  if (!is.numeric(year) || length(year) != 1L || is.na(year) ||
      !is.finite(year)) {
    .cli_abort_schema(c(
      "{.arg year} must be a finite scalar {.cls numeric}.",
      "x" = "Got {.cls {class(year)[[1L]]}} of length {.val {length(year)}}."
    ))                                                                  # E-24-02-year
  }

  # drive_times: positive numeric vector (deep monotone / max-band check is
  # cacs_isochrone()'s job per Sec. 19 step 1).
  if (!is.numeric(drive_times) || length(drive_times) == 0L ||
      anyNA(drive_times) || any(!is.finite(drive_times)) ||
      any(drive_times <= 0)) {
    .cli_abort_schema(c(
      "{.arg drive_times} must be a positive finite {.cls numeric} vector.",
      "i" = "Each band must be > 0 minutes; see Sec. 19 for the per-provider max."
    ))                                                                  # E-24-02-drive
  }

  # E-24-03 precomputed_isochrones: sf or NULL.
  if (!is.null(precomputed_isochrones) &&
      !inherits(precomputed_isochrones, "sf")) {
    .cli_abort_schema(c(
      "{.arg precomputed_isochrones} must be an {.cls sf} or {.code NULL}.",
      "x" = "Got {.cls {class(precomputed_isochrones)[[1L]]}}.",
      "i" = "Pass an sf produced by {.fn cacs_isochrone} for the 4-call-A / 3-call path."
    ))                                                                  # E-24-03
  }

  # E-24-04 acs: sf or NULL.
  if (!is.null(acs) && !inherits(acs, "sf")) {
    .cli_abort_schema(c(
      "{.arg acs} must be an {.cls sf} or {.code NULL}.",
      "x" = "Got {.cls {class(acs)[[1L]]}}.",
      "i" = "Pass an sf validated by {.fn cacs_acs_validate} for the Path B / 3-call path."
    ))                                                                  # E-24-04
  }

  # E-24-05 weight_method = "population" is reserved but not implemented.
  # Fast-fail before Sec.19/Sec.20 entry so the user does not wait on live
  # routing or ACS requests for a path that is intentionally deferred.
  if (identical(weight_method, "population")) {
    .cli_abort_credential(c(
      "{.field weight_method} = {.val population} is deferred to a future release.",
      "i" = "Use {.val area} in the current release.",
      "x" = "Fast-fail at the orchestrator boundary; no provider or ACS request was started."
    ))                                                                  # E-24-05
  }

  # 5 arg-list type guards (each becomes a hard schema abort with E-24-13 if
  # the user passes a non-list -- full-list key validation is delegated to
  # `.validate_run_arg_list()` in Step 3 below).
  if (!is.list(iso_args)) {
    .cli_abort_schema(c(
      "{.arg iso_args} must be a {.cls list}.",
      "x" = "Got {.cls {class(iso_args)[[1L]]}}."
    ))                                                                  # E-24-13
  }
  if (!is.list(acs_args)) {
    .cli_abort_schema(c(
      "{.arg acs_args} must be a {.cls list}.",
      "x" = "Got {.cls {class(acs_args)[[1L]]}}."
    ))                                                                  # E-24-13
  }
  if (!is.list(weight_args)) {
    .cli_abort_schema(c(
      "{.arg weight_args} must be a {.cls list}.",
      "x" = "Got {.cls {class(weight_args)[[1L]]}}."
    ))                                                                  # E-24-13
  }
  if (!is.list(moe_args)) {
    .cli_abort_schema(c(
      "{.arg moe_args} must be a {.cls list}.",
      "x" = "Got {.cls {class(moe_args)[[1L]]}}."
    ))                                                                  # E-24-13
  }
  if (!is.list(rate_args)) {
    .cli_abort_schema(c(
      "{.arg rate_args} must be a {.cls list}.",
      "x" = "Got {.cls {class(rate_args)[[1L]]}}."
    ))                                                                  # E-24-13
  }

  # --------------------------------------------------------------------------
  # Step 2 - Execution path determination (Sec. 24.4 4-path table).
  #
  # The 2-bit boolean state (precomputed_isochrones x acs) selects exactly
  # one of the 4 execution paths. The 4 case_when clauses are written as
  # mutually-exclusive so order does not matter -- `dplyr::case_when()`
  # short-circuits on the first TRUE.
  # --------------------------------------------------------------------------

  has_iso <- !is.null(precomputed_isochrones)
  has_acs <- !is.null(acs)
  execution_path <- if      (has_iso &&  has_acs) "3-call"
                    else if (has_iso && !has_acs) "4-call-A"
                    else if (!has_iso && has_acs) "4-call-B"
                    else                          "5-call"

  if (isTRUE(verbose)) {
    .cli_inform_progress(
      c("i" = "Execution path: {.val {execution_path}}"),
      phase = "Sec. 24 orchestrator"
    )                                                                  # M-24-01
  }

  # --------------------------------------------------------------------------
  # Step 3 - Validate explicit sub-call arg lists against sanctioned keys
  # (Sec. 24.4 .CACS_RUN_ARG_KEYS 5-list).
  #
  # The 2 reserved keys `moe_args$formula` and `rate_args$formula_dispatch`
  # are hard-aborted (E-24-13): they are top-level public
  # surface and silent forwarding would let two competing values exist in
  # the same call. Every other unknown key is W-24-01 warn + drop.
  # --------------------------------------------------------------------------

  if ("formula" %in% names(moe_args)) {
    .cli_abort_schema(c(
      "{.arg moe_args$formula} is reserved.",
      "x" = "Pass {.arg moe_formula} at the top level of {.fn cacs_run} instead.",
      "i" = "See Sec. 24.4 (.CACS_RUN_ARG_KEYS contract) for the full reserved-key list."
    ))                                                                  # E-24-13
  }
  if ("formula_dispatch" %in% names(rate_args)) {
    .cli_abort_schema(c(
      "{.arg rate_args$formula_dispatch} is reserved.",
      "x" = "Pass {.arg formula_dispatch} at the top level of {.fn cacs_run} instead.",
      "i" = "See Sec. 24.4 (.CACS_RUN_ARG_KEYS contract) for the full reserved-key list."
    ))                                                                  # E-24-13
  }

  passthrough_iso <- .validate_run_arg_list(
    iso_args,    "iso", .CACS_RUN_ARG_KEYS$iso
  )
  passthrough_acs <- .validate_run_arg_list(
    acs_args,    "acs", .CACS_RUN_ARG_KEYS$acs
  )
  passthrough_wgt <- .validate_run_arg_list(
    weight_args, "wgt", .CACS_RUN_ARG_KEYS$wgt
  )
  passthrough_moe <- .validate_run_arg_list(
    moe_args,    "moe", .CACS_RUN_ARG_KEYS$moe
  )
  passthrough_rat <- .validate_run_arg_list(
    rate_args,   "rat", .CACS_RUN_ARG_KEYS$rat
  )

  # --------------------------------------------------------------------------
  # Step 4 - Initialize warning collector (Sec. 24.7 phase-keyed list).
  #
  # An `environment` is used rather than a `list` so that downstream
  # `.call_with_capture()` invocations (Phase 7.2) can mutate the entries in
  # place across the 5 phases without having to thread the collector through
  # every return value. The 6 phase keys are seeded as empty lists so
  # downstream code can `c(warning_log$entries[[phase]], ...)` without first
  # checking for `NULL`. The 6th key `orchestrator` carries Sec. 24-native
  # warnings (W-24-01, W-24-02, ...).
  # --------------------------------------------------------------------------

  warning_log <- new.env(parent = emptyenv())
  warning_log$entries <- list(
    isochrone        = list(),
    acs_prefetch     = list(),
    intersect_weight = list(),
    propagate_moe    = list(),
    derive_rates     = list(),
    orchestrator     = list()
  )

  # --------------------------------------------------------------------------
  # Step 5 - PHASE 1: ACS resolution (bypass or Sec.20 call).
  #
  # Design: explicit phase string +
  # explicit `if (!is.null(acs))` branch over Sec.24.4 4-path table. M-24-02
  # fires only on bypass and only when `verbose = TRUE` so that the user can
  # confirm in-band that no Sec.20 HTTP call was made.
  # --------------------------------------------------------------------------

  if (!is.null(acs)) {
    acs_resolved <- acs
    if (isTRUE(verbose)) {
      .cli_inform_progress(
        c("i" = "ACS bypass: user-supplied {.cls sf} ({nrow(acs)} row{?s})."),
        phase = "Sec. 24 orchestrator"
      )                                                                # M-24-02
    }
  } else {
    acs_resolved <- .call_with_capture(
      fn          = cacs_acs_prefetch,
      args        = c(
        list(state = state, year = year, variables = variables,
             cache_dir = cache_dir, verbose = verbose),
        passthrough_acs
      ),
      warning_log = warning_log,
      phase       = "acs_prefetch"
    )
  }

  # --------------------------------------------------------------------------
  # Step 6 - PHASE 2: Isochrone resolution (bypass or Sec.19 call).
  #
  # Symmetric to Step 5. M-24-03 fires only on bypass when `verbose = TRUE`.
  # The deep schema check on `precomputed_isochrones` must happen before
  # Step 7, which reads canonical status columns (`isochrone_empty`,
  # `failure_reason`) to split failed pairs.
  # --------------------------------------------------------------------------

  if (!is.null(precomputed_isochrones)) {
    iso_resolved <- precomputed_isochrones
    if (isTRUE(verbose)) {
      .cli_inform_progress(
        c("i" = "Isochrone bypass: precomputed {.cls sf} ({nrow(precomputed_isochrones)} row{?s})."),
        phase = "Sec. 24 orchestrator"
      )                                                                # M-24-03
    }
  } else {
    iso_resolved <- .call_with_capture(
      fn          = cacs_isochrone,
      args        = c(
        list(sites = sites, drive_times = drive_times,
             provider = provider, cache_dir = cache_dir,
             verbose = verbose),
        passthrough_iso
      ),
      warning_log = warning_log,
      phase       = "isochrone"
    )
  }
  .validate_iso_schema(iso_resolved)

  # --------------------------------------------------------------------------
  # Step 7 - Identify failed (site_id, drive_time_min) pairs (partial-success
  # separation per Sec.24.6 + E-24-15 catastrophic boundary per Sec.24.8).
  #
  # `iso_summary` group_by + summarise produces 1 row per pair with two
  # invalidity flags:
  #   - `pair_empty`  : every row of the pair has `isochrone_empty = TRUE`
  #                     (provider returned, but with empty geometry).
  #   - `pair_failed` : any row of the pair has a non-NA `failure_reason`
  #                     (provider returned an explicit failure code).
  # The union (`pair_invalid`) drives `valid_pairs` / `failed_pairs`
  # separation. The blueprint pseudocode (Sec.24.3 step 7) names both columns
  # explicitly so the partial-success policy is byte-traceable from the
  # spec to the code.
  # --------------------------------------------------------------------------

  iso_summary <- iso_resolved |>
    sf::st_drop_geometry() |>
    dplyr::group_by(.data$site_id, .data$drive_time_min) |>
    dplyr::summarise(
      pair_empty     = all(.data$isochrone_empty, na.rm = TRUE),
      pair_failed    = any(!is.na(.data$failure_reason)),
      pair_invalid   = .data$pair_empty || .data$pair_failed,
      failure_reason = dplyr::case_when(
        .data$pair_failed ~ paste(unique(stats::na.omit(.data$failure_reason)),
                                  collapse = ";"),
        .data$pair_empty  ~ "isochrone_empty",
        TRUE              ~ NA_character_
      ),
      .groups = "drop"
    )

  valid_pairs <- iso_summary |>
    dplyr::filter(!.data$pair_invalid) |>
    dplyr::select("site_id", "drive_time_min")

  failed_pairs <- iso_summary |>
    dplyr::filter(.data$pair_invalid) |>
    dplyr::select("site_id", "drive_time_min", "failure_reason")

  if (nrow(valid_pairs) == 0L) {
    n_pair_total <- nrow(iso_summary)
    .cli_abort_operator(c(
      "All {n_pair_total} (site_id, drive_time_min) pair{?s} failed isochrone.",
      "x" = "No valid pair remains; analysis is not meaningful.",
      "i" = "Check provider connectivity, site coordinates, and {.arg drive_times} bands."
    ))                                                                  # E-24-15
  }

  if (nrow(failed_pairs) > 0L) {
    n_failed <- nrow(failed_pairs)
    n_total  <- nrow(iso_summary)
    .cacs_emit(
      level = "warn",
      message = c(
        "Partial isochrone failure: {n_failed} of {n_total} pair{?s} failed.",
        "i" = "Failed pairs carry NA-propagated rows; see {.code attr(out, 'cacs_run_warnings')}."
      ),
      classes = c("catchmentACS_warning_partial",
                  "catchmentACS_warning",
                  "catchmentACS_condition"),
      .envir = rlang::current_env()
    )                                                                   # W-24-02
    warning_log$entries$orchestrator <- c(
      warning_log$entries$orchestrator,
      list(list(
        code         = "W-24-02",
        n_failed     = nrow(failed_pairs),
        n_total      = nrow(iso_summary),
        failed_pairs = failed_pairs,
        captured_at  = Sys.time()
      ))
    )
  }

  # Restrict the isochrone sf to valid pairs only before Sec.21 entry -- Sec.21's
  # per-site loop should not waste work on pairs that already failed at the
  # provider layer.
  iso_sf_valid <- iso_resolved |>
    dplyr::semi_join(valid_pairs, by = c("site_id", "drive_time_min"))

  # --------------------------------------------------------------------------
  # Step 8 - PHASE 3: cacs_intersect_weight() with capture.
  #
  # Forward the user's verbose preference to Sec.21 so the longest local CPU
  # phase answers the F1 "is it stuck?" question. Warnings still propagate via
  # .call_with_capture(); progress messages remain classed message conditions.
  # --------------------------------------------------------------------------

  weighted_acs <- .call_with_capture(
    fn          = cacs_intersect_weight,
    args        = c(
	      list(iso_sf = iso_sf_valid, acs_sf = acs_resolved,
	           bg_pop_sf = bg_pop_sf, weight_method = weight_method,
	           verbose = verbose),
      passthrough_wgt
    ),
    warning_log = warning_log,
    phase       = "intersect_weight"
  )

  # --------------------------------------------------------------------------
  # Step 9 - PHASE 4: cacs_propagate_moe() with capture.
  # --------------------------------------------------------------------------

  moe_propagated <- .call_with_capture(
    fn          = cacs_propagate_moe,
    args        = c(
      list(data = weighted_acs, formula = moe_formula,
           verbose = verbose),
      passthrough_moe
    ),
    warning_log = warning_log,
    phase       = "propagate_moe"
  )

  # --------------------------------------------------------------------------
  # Step 10 - PHASE 5: cacs_derive_rates() with capture.
  # --------------------------------------------------------------------------

  rate_derived <- .call_with_capture(
    fn          = cacs_derive_rates,
    args        = c(
      list(weighted_acs = moe_propagated, rates = rates,
           formula_dispatch = formula_dispatch, verbose = verbose),
      passthrough_rat
    ),
    warning_log = warning_log,
    phase       = "derive_rates"
  )

  # --------------------------------------------------------------------------
  # Step 11 - Partial-success NA propagation (Sec.24.6 lazy append policy).
  #
  # If any failed pair exists, build byte-identical NA rows for each failed
  # (site_id, drive_time_min) x variable combination (all ACS source vars +
  # all rate names) and bind to the bottom of `rate_derived`. The NA rows
  # carry `failure_origin = "isochrone"`, `n_tracts = NA_integer_`,
  # `moe_fallback_reason = "n/a"` per Sec.24.6 4-field contract -- these
  # sentinels are *distinct* from the Sec.21 `n_tracts = 0L` /
  # `failure_origin = "intersection"` empty-intersection encoding.
  # --------------------------------------------------------------------------

  if (nrow(failed_pairs) > 0L) {
    na_rows <- .build_na_propagation_rows(
      failed_pairs = failed_pairs,
      rates        = rates,
      template     = rate_derived
    )
    long_final <- dplyr::bind_rows(rate_derived, na_rows)
  } else {
    long_final <- rate_derived
  }

  # --------------------------------------------------------------------------
  # Step 12 - Output dispatch + attribute carry-through (Sec. 24.3 step 12 +
  # Sec. 24.5 3-mode dispatch + Sec. 24.8 16-field provenance).
  #
  # `long_final` carries all sub-call provenance attributes from Sec.22 / Sec.23 +
  # the Sec.21 carrier attribute. We preserve those by attaching the new run
  # attributes *on top* without dropping the upstream ones. For `output =
  # "both"`, both representations share the same run-level attributes so a
  # downstream consumer can read provenance from either branch.
  # --------------------------------------------------------------------------

  # v0.5: thread the resolved isochrone sf into the list-column `$isochrone`
  # cell. This covers both precomputed bypass paths and computed 5-call /
  # 4-call-B paths. Use the full `iso_resolved` rather than `iso_sf_valid`
  # so failed-pair diagnostics remain available in the list-column surface.
  out <- switch(
    output,
    "long"        = long_final,
    "list_column" = .pivot_to_list_column(long_final, sites,
                                          iso_sf = iso_resolved),
    "both"        = list(
      long        = long_final,
      list_column = .pivot_to_list_column(long_final, sites,
                                          iso_sf = iso_resolved)
    )
  )

  # 16-field run-level provenance (Sec. 24.8). Field set is the union of the
  # blueprint's "what was requested" inputs and "what was observed" derived
  # quantities -- execution_path lives in here (not just M-24-01 console) so a
  # 5-year audit of a frozen tibble can answer "which dispatch path was taken"
  # without re-reading the cli stream.
  run_provenance <- list(
    generated_at                  = Sys.time(),
    cacs_ver                      = as.character(utils::packageVersion("catchmentACS")),
    R_version                     = as.character(getRversion()),
    schema_version                = "1.0",
    execution_path                = execution_path,
    state                         = state,
    year                          = as.integer(year),
    drive_times                   = sort(unique(as.integer(drive_times))),
    provider                      = provider,
    weight_method                 = weight_method,
    moe_formula                   = moe_formula %||% "auto",
    formula_dispatch              = formula_dispatch,
    output_format                 = output,
    level                         = moe_args$level %||% 0.90,
    min_weight                    = weight_args$min_weight %||% 1e-6,
    n_sites_input                 = dplyr::n_distinct(sites$site_id),
    n_pairs_valid                 = nrow(valid_pairs),
    n_pairs_failed                = nrow(failed_pairs),
    n_sites_with_any_valid_pair   = dplyr::n_distinct(valid_pairs$site_id),
    n_sites_with_any_failed_pair  = dplyr::n_distinct(failed_pairs$site_id),
    bypass_iso                    = !is.null(precomputed_isochrones),
    bypass_acs                    = !is.null(acs)
  )

  run_warnings <- as.list(warning_log$entries)

  # Carry sub-call attributes through one anchor object (`long_final`) so we
  # can copy them onto `out` (or onto both `out$long` + `out$list_column`)
  # in one place. `attr(x, name) <- NULL` is a no-op when the source is NULL,
  # so we *do* propagate "this sub-call did not populate this attribute" as
  # NULL -- that is a meaningful audit signal (e.g. Sec.22 confidence level when
  # `cacs_propagate_moe()` did not run on a row).
  agg_prov  <- attr(weighted_acs,    "cacs_aggregation_provenance")
  moe_prov  <- attr(moe_propagated,  "cacs_moe_provenance")
  rate_prov <- attr(rate_derived,    "cacs_rate_provenance")
  conf_lvl  <- attr(moe_propagated,  "cacs_confidence_level")

  attach_run_attrs <- function(x) {
    attr(x, "cacs_schema_version")          <- "1.0"
    attr(x, "cacs_run_provenance")          <- run_provenance
    attr(x, "cacs_run_warnings")            <- run_warnings
    attr(x, "cacs_aggregation_provenance") <- agg_prov
    attr(x, "cacs_moe_provenance")          <- moe_prov
    attr(x, "cacs_rate_provenance")         <- rate_prov
    attr(x, "cacs_confidence_level")        <- conf_lvl
    x
  }

  if (identical(output, "both")) {
    # Names locked to c("long", "list_column") in this order -- both
    # representations carry the same run-level attribute set so downstream
    # consumers can read provenance from either branch.
    out$long        <- attach_run_attrs(out$long)
    out$list_column <- attach_run_attrs(out$list_column)
  } else {
    out <- attach_run_attrs(out)
  }

  # --------------------------------------------------------------------------
  # F6 cacs_run_result S3 class + cacs_run_result_metadata attribute attach
  # (Step 4.2 locked contract). Step 4.3 closed the 3
  # polish flags below:
  #   1. n_sites_success / n_sites_failed: site-level full-success / full-fail;
  #      partial-success sites bucket into n_sites_success (documented).
  #   2. moe_fallback_summary$c1_to_c2_count: now uses
  #      `moe_fallback_reason == "negative_variance"` (per
  #      `.warn_derive_rates_summary` in R/derive-rates.R, the C1->C2 cascade
  #      writes "negative_variance" as the fallback reason).
  #   3. profile: pulled from `iso_resolved$profile[1]` so the bypass and the
  #      live-fetch path both surface the resolved provider profile (and not
  #      just the `iso_args$profile` user-passthrough, which is NULL on
  #      bypass).
  # --------------------------------------------------------------------------

  # Site-level success/failure tally. A site is "fully successful" when every
  # one of its drive-time pairs validated at the isochrone layer; "fully
  # failed" when none did. Partial-success sites (some valid + some failed)
  # bucket into `n_sites_success` to preserve the v0.2 contract that
  # `n_sites_success + n_sites_failed <= n_sites_total` and any "partial"
  # split is recoverable from `run_provenance$n_sites_with_any_failed_pair`.
  .n_sites_success <- run_provenance$n_sites_with_any_valid_pair
  .n_sites_failed  <- max(
    run_provenance$n_sites_input - run_provenance$n_sites_with_any_valid_pair,
    0L
  )

  # moe_fallback_summary: tally moe_fallback_reason rows on long_final. The
  # spec lists three buckets (c1_to_c2_count, zero_den_count,
  # missing_moe_count). On the v0.2 long schema, the C1->C2 cascade writes
  # `moe_fallback_reason = "negative_variance"` (see
  # `.warn_derive_rates_summary` in R/derive-rates.R + Sec. 22.7 / Sec. 23.5),
  # while `.moe_zero_denominator_self` writes "zero_denominator" and
  # `.moe_missing_self` writes "missing_moe". NA propagation rows carry
  # `moe_fallback_reason = "n/a"` and are excluded from all three counts.
  .moe_fallback_summary <- if ("moe_fallback_reason" %in% colnames(long_final)) {
    .reason <- long_final$moe_fallback_reason
    list(
      c1_to_c2_count    = sum(.reason == "negative_variance", na.rm = TRUE),
      zero_den_count    = sum(.reason == "zero_denominator",  na.rm = TRUE),
      missing_moe_count = sum(.reason == "missing_moe",        na.rm = TRUE)
    )
  } else {
    list(c1_to_c2_count    = NA_integer_,
         zero_den_count    = NA_integer_,
         missing_moe_count = NA_integer_)
  }

  # profile resolution: prefer the resolved profile carried on the isochrone
  # sf (`iso_resolved$profile[1]`). This works for both bypass (the user-
  # supplied iso sf carries its profile from `.iso_via_*` normalisation) and
  # live-fetch (Sec.19 step 9 writes `profile` to every row).
  .iso_profile <- if (!is.null(iso_resolved) &&
                      "profile" %in% colnames(iso_resolved) &&
                      nrow(iso_resolved) > 0L) {
    .p <- iso_resolved$profile[[1L]]
    if (is.na(.p) || !nzchar(.p)) passthrough_iso$profile %||% "driving"
    else .p
  } else {
    passthrough_iso$profile %||% "driving"
  }

  .metadata <- list(
    generated_at         = run_provenance$generated_at,
    cacs_version         = utils::packageVersion("catchmentACS"),
    provider             = provider,
    profile              = .iso_profile,
    drive_times          = sort(unique(as.numeric(drive_times))),
    n_sites_total        = run_provenance$n_sites_input,
    n_sites_success      = .n_sites_success,
    n_sites_failed       = .n_sites_failed,
    wall_clock_seconds   = as.numeric(difftime(Sys.time(), run_start,
                                               units = "secs")),
    skipped_geoids       = attr(weighted_acs, "skipped_geoids") %||% character(0),
    moe_fallback_summary = .moe_fallback_summary
  )

  if (identical(output, "both")) {
    out$long        <- .attach_run_result_class(out$long,        .metadata)
    out$list_column <- .attach_run_result_class(out$list_column, .metadata)
  } else {
    out <- .attach_run_result_class(out, .metadata)
  }

  if (isTRUE(verbose)) {
    .cli_inform_progress(
      c(
        "i" = "Run complete: {.val {run_provenance$n_pairs_valid}} valid / {.val {run_provenance$n_pairs_failed}} failed pair{?s} across {.val {run_provenance$n_sites_input}} site{?s}.",
        "i" = "Output mode: {.val {output}} (schema v{run_provenance$schema_version})."
      ),
      phase = "Sec. 24 orchestrator"
    )                                                                    # M-24-04
  }

  out
}


# ============================================================================
# Internal helpers (Step 7.1 / 7.2 surface).
# ============================================================================


#' Validate one sub-call arg list against the sanctioned key set
#'
#' Per Sec. 24.3 step 3 + Sec. 24.4 `.CACS_RUN_ARG_KEYS` contract: each of
#' the 5 explicit sub-call passthrough lists (`iso_args`, `acs_args`,
#' `weight_args`, `moe_args`, `rate_args`) is filtered against the sanctioned
#' key set for the destination sub-call. Unknown keys warn (W-24-01,
#' `catchmentACS_warning_provenance` class) and are dropped from the
#' returned list so they never reach the sub-call. The `label` argument is
#' the short tag used in the warning message (`"iso"`, `"acs"`, `"wgt"`,
#' `"moe"`, `"rat"`).
#'
#' This helper assumes the caller has already established that `args_list`
#' is a `list` (the E-24-13 type guard runs in the public function before
#' delegation); calling it with a non-list aborts schema-class for safety.
#' For an empty `sanctioned_keys` set (the v1.0 `rat` reservation), *any*
#' user-supplied key triggers the W-24-01 warning + drop.
#'
#' @keywords internal
#' @noRd
.validate_run_arg_list <- function(args_list, label, sanctioned_keys) {
  if (!is.list(args_list)) {
    .cli_abort_schema(c(
      "{.arg {label}_args} must be a {.cls list}.",
      "x" = "Got {.cls {class(args_list)[[1L]]}}."
    ))                                                                  # E-24-13
  }
  if (length(args_list) == 0L) {
    return(list())
  }

  arg_names <- names(args_list)
  if (is.null(arg_names) || any(!nzchar(arg_names))) {
    .cli_abort_schema(c(
      "{.arg {label}_args} must be a fully-named {.cls list}.",
      "x" = "At least one element has no name.",
      "i" = "Sub-call passthrough lists are key-value forwarded to the sub-function."
    ))                                                                  # E-24-13
  }

  unknown <- setdiff(arg_names, sanctioned_keys)
  if (length(unknown) > 0L) {
    if (length(sanctioned_keys) == 0L) {
      .cli_warn_provenance(
        c(
          "Unknown {.arg {label}_args} key{?s}: {.val {unknown}}.",
          "i" = "{.arg {label}_args} is reserved as an empty surface at v1.0; all keys are dropped.",
          "x" = "Dropping {.val {unknown}} before sub-call forward."
        ),
        phase = "Sec. 24 orchestrator"
      )                                                                 # W-24-01
    } else {
      .cli_warn_provenance(
        c(
          "Unknown {.arg {label}_args} key{?s}: {.val {unknown}}.",
          "i" = "Sanctioned key{?s}: {.val {sanctioned_keys}}.",
          "x" = "Dropping {.val {unknown}} before sub-call forward."
        ),
        phase = "Sec. 24 orchestrator"
      )                                                                 # W-24-01
    }
  }

  args_list[intersect(arg_names, sanctioned_keys)]
}


#' Call a sub-function, capturing its `cli_warn` warnings into the phase-keyed log
#'
#' Per Sec. 24.7 + Sec. 42.4 Step 7.2 -- wraps a sub-call (`cacs_acs_prefetch()`,
#' `cacs_isochrone()`, `cacs_intersect_weight()`, `cacs_propagate_moe()`,
#' `cacs_derive_rates()`) in `withCallingHandlers()` so that every warning the
#' sub-call emits is *both* captured into the phase-keyed `warning_log$entries`
#' list *and* re-emitted to the console (no silent muffle). Errors are not
#' caught here; phase-level aborts propagate to the caller. The captured entry
#' is a 4-field record (`message`, `class`, `phase`, `captured_at`) so a 5-year
#' audit can read `attr(out, "cacs_run_warnings")$<phase>` and see every
#' warning that fired in that phase together with its origin condition class.
#'
#' Implementation note (Sec. 42.4 reviewer Axis 1): `withCallingHandlers()` is
#' chosen over `tryCatch(warning = ...)` because the latter *replaces* the
#' warning with a returned value (silent muffle by default); the former lets
#' the warning continue to propagate to the outer console handler while also
#' giving us a side-effecting capture. The handler does *not* call
#' `invokeRestart("muffleWarning")` -- we want the warning to reach the
#' top-level handler so the user sees it in the live console.
#'
#' @param fn The sub-function to call (an unquoted function object).
#' @param args A `list` of named arguments to forward via `do.call()`.
#' @param warning_log An `environment` with a list slot `entries` (one slot
#'   per phase, seeded in Step 4 of `cacs_run()`).
#' @param phase A scalar character matching one of the 6 seeded phase keys
#'   in `warning_log$entries` (`"acs_prefetch"`, `"isochrone"`,
#'   `"intersect_weight"`, `"propagate_moe"`, `"derive_rates"`,
#'   `"orchestrator"`). Phase typos would silently create a new slot -- the
#'   caller (`cacs_run()`) hard-codes the 5 sub-call phase strings.
#'
#' @return The return value of `do.call(fn, args)` -- sub-call attributes
#'   carry through untouched (sub-call provenance is preserved for Sec.24.8).
#' @keywords internal
#' @noRd
.call_with_capture <- function(fn, args, warning_log, phase) {
  withCallingHandlers(
    do.call(fn, args),
    warning = function(w) {
      warning_log$entries[[phase]] <- c(
        warning_log$entries[[phase]],
        list(list(
          message     = conditionMessage(w),
          class       = class(w),
          phase       = phase,
          captured_at = Sys.time()
        ))
      )
      # Deliberately no invokeRestart("muffleWarning") -- let the warning
      # propagate to the outer console handler so the user sees it live.
    }
  )
}


#' Build NA-propagation rows for failed (site_id, drive_time_min) pairs
#'
#' Per Sec. 24.6 + Sec. 42.4 Step 7.2 -- for each failed pair x (all source
#' ACS variables + all rate names), produce one row with byte-identical
#' column schema to the `template` (the successful `rate_derived` long
#' tibble from Sec.23). Four sentinel fields encode the Sec.24.6 contract:
#'
#'   * `estimate = NA_real_`           -- computation not attempted.
#'   * `n_tracts = NA_integer_`        -- intersection not attempted
#'                                       (distinct from Sec.21's `0L` empty
#'                                       intersection encoding).
#'   * `failure_origin = "isochrone"`  -- upstream phase identification
#'                                       (distinct from `"intersection"`).
#'   * `moe_fallback_reason = "n/a"`   -- MOE stage not reached
#'                                       (distinct from MOE fallback reasons).
#'
#' Column membership and order match `template` exactly so
#' `dplyr::bind_rows(rate_derived, na_rows)` preserves the mandatory
#' canonical schema (+ optional `se`). The `estimand_family` slot is
#' looked up from the template per variable when available, so derived-rate
#' rows are still classified as `"derived_rate"` even on NA propagation.
#'
#' @param failed_pairs A `tibble` with `site_id`, `drive_time_min` columns
#'   (and optional `failure_reason`, currently unused in row construction --
#'   the diagnostic surface is `warning_log$entries$orchestrator`).
#' @param rates A named `list` of `c(num, den)` ACS code pairs (rate names
#'   become `variable` values in the NA rows alongside ACS source codes).
#' @param template The successful `rate_derived` long tibble from Sec.23; its
#'   `unique(variable)` set + column schema drive the NA-row construction.
#'
#' @return A `tibble` with the same columns as `template`, one row per
#'   (failed pair x variable). When `failed_pairs` has 0 rows, returns a
#'   0-row tibble with the template's column structure.
#' @keywords internal
#' @noRd
.build_na_propagation_rows <- function(failed_pairs, rates, template) {
  template_cols <- colnames(template)

  # Variable universe: every ACS code present in the successful rows + every
  # rate name from `rates`. Rate names from `rates` may already appear in
  # `unique(template$variable)`; `unique()` deduplicates either way. This
  # makes the NA-row schema robust to corner cases where a failed pair has
  # never been computed at all (the only template a downstream test sees is
  # whatever Sec.23 emitted on the valid pairs).
  template_vars <- if ("variable" %in% template_cols) {
    unique(template$variable)
  } else {
    character(0)
  }
  rate_names <- names(rates) %||% character(0)
  all_vars   <- unique(c(template_vars, rate_names))

  # Pair-fast empty exit: zero failed pairs returns a 0-row tibble with the
  # template's column structure (so `bind_rows()` is a no-op).
  if (nrow(failed_pairs) == 0L || length(all_vars) == 0L) {
    empty <- template[integer(0), , drop = FALSE]
    return(empty)
  }

  # Cross-product: every failed pair x every variable name. tidyr::expand_grid
  # preserves character-class on input columns (unlike base R's expand.grid
  # which factor-coerces by default).
  na_skeleton <- tidyr::expand_grid(
    site_id        = unique(failed_pairs$site_id),
    drive_time_min = unique(failed_pairs$drive_time_min),
    variable       = all_vars
  )
  # Restrict to actual failed (site_id, drive_time_min) pairs rather than
  # the full Cartesian product (a site with 1 failed drive_time and 2
  # successful drive_times should produce NA rows only for the failed one).
  na_skeleton <- na_skeleton |>
    dplyr::semi_join(failed_pairs, by = c("site_id", "drive_time_min"))

  # Build the long-schema NA frame. Columns set per Sec.24.6 4-field
  # contract; types match the Sec.12.3.1 canonical schema (double for numeric,
  # integer for n_tracts, character for the categorical metadata).
  na_rows <- na_skeleton |>
    dplyr::mutate(
      estimate                      = NA_real_,
      moe                           = NA_real_,
      se                            = NA_real_,
      ring_topology                 = "cumulative",
      weight_sum                    = NA_real_,
      n_tracts                      = NA_integer_,
      provider                      = NA_character_,
      profile                       = NA_character_,
      osm_snapshot_date             = NA_character_,
      acs_year                      = NA_integer_,
      weight_method                 = NA_character_,
      estimand_family               = NA_character_,
      weight_basis                  = NA_character_,
      moe_formula_requested         = NA_character_,
      moe_formula_effective         = NA_character_,
      moe_fallback                  = NA,           # logical NA
      moe_fallback_reason           = "n/a",        # Sec.24.6 explicit sentinel
      failure_origin                = "isochrone",  # Sec.24.6 upstream marker
      weight_uncertainty_propagated = NA            # logical NA
    )

  # Fill `estimand_family` from the template's variable -> family map so the
  # NA rows preserve the Sec.7.1 family classification (derived_rate rows stay
  # "derived_rate" on NA propagation, etc.).
  if ("estimand_family" %in% template_cols && "variable" %in% template_cols) {
    template_family <- template |>
      dplyr::distinct(.data$variable, .data$estimand_family) |>
      stats::na.omit()
    if (nrow(template_family) > 0L) {
      na_rows <- na_rows |>
        dplyr::select(-"estimand_family") |>
        dplyr::left_join(template_family, by = "variable")
    }
  }

  # Add any template columns we missed (forward-compatibility seam for Sec.22
  # / Sec.23 schema extensions in v1.1) as NA, then drop any columns we
  # invented that the template does not carry (e.g. `se` if Sec.22 ran at
  # `level = 0.90` and elided the SE column).
  missing_in_na <- setdiff(template_cols, colnames(na_rows))
  for (col in missing_in_na) {
    template_col <- template[[col]]
    # Match the template's NA type per column class so dplyr::bind_rows()
    # does not promote types on the union (e.g. integer + double -> double).
    na_value <- switch(
      typeof(template_col),
      "integer"   = NA_integer_,
      "double"    = NA_real_,
      "logical"   = NA,
      "character" = NA_character_,
      NA
    )
    na_rows[[col]] <- na_value
  }

  # Reorder + restrict to byte-identical column order vs the template.
  na_rows[, template_cols, drop = FALSE]
}


#' Pivot the canonical long tibble into the Sec. 12.3.2 8-col list-column format
#'
#' Implements the `output = "list_column"` branch of Sec. 24.5. Produces a
#' tibble with one row per `(site_id, drive_time_min)` and four list-columns
#' nesting the per-site results so a visualization layer can read everything
#' from a single row:
#'
#'   * `acs_estimates` (list of tibble): variable-level estimates + MOE +
#'     metadata for non-rate rows (`estimand_family != "derived_rate"`).
#'   * `derived_rates` (list of tibble): same shape, restricted to derived
#'     rate rows.
#'   * `metadata`      (list of list):   per-row provenance summary --
#'     provider / profile / OSM snapshot / weight method / failure_origin /
#'     n_tracts. The minimal v0.1 surface; richer keys (cache_hit,
#'     routing_engine_version, package versions) land in v0.2.
#'   * `isochrone`     (list of sf or NULL): Sec. 12.3.2 reserves this
#'     column for the per-row sfc POLYGON. When the caller supplies the
#'     resolved isochrone sf, matching `(site_id, drive_time_min)` cells are
#'     populated with one-row `sf` objects; unmatched pairs remain `NULL`.
#'
#' The 8-col surface (`site_id`, `lon`, `lat`, `drive_time_min`, `isochrone`,
#' `acs_estimates`, `derived_rates`, `metadata`) matches the Sec. 12.3.2
#' contract exactly. `tidyr::unnest(out, acs_estimates)` recovers the
#' non-rate subset of the long format (info-bijective for the variable rows).
#'
#' Implementation order:
#'   1. Strip geometry from `sites_input` if it is `sf` and pull a row-unique
#'      `(site_id, lon, lat)` table so the eventual left_join is 1-to-many on
#'      site_id only (not on drive_time_min, which is what nests).
#'   2. Build `acs_nested` + `rates_nested` via `tidyr::nest()` keyed on
#'      `(site_id, drive_time_min)`; the `data` slot becomes `acs_estimates`
#'      / `derived_rates` respectively.
#'   3. Build the per-pair `metadata` list-column from a small subset of the
#'      long columns (one list per pair).
#'   4. Cross site coords with the unique `(site_id, drive_time_min)` set,
#'      then left_join the three list-columns + add the placeholder
#'      `isochrone = vector("list", n)` column.
#'   5. Reorder to the Sec. 12.3.2 8-col schema and return.
#'
#' @param long_final The combined long-format tibble (valid + NA-propagation
#'   rows) returned by Step 11 of Sec. 24.3.
#' @param sites_input The user-supplied `sites` tibble or sf (Sec. 12.2.1),
#'   carried in so the helper can recover `lon` / `lat` per site for the
#'   8-col schema even when `long_final` only carries `site_id`.
#'
#' @return A `tbl_df` with exactly 8 columns: `site_id`, `lon`, `lat`,
#'   `drive_time_min`, `isochrone`, `acs_estimates`, `derived_rates`,
#'   `metadata`. One row per `(site_id, drive_time_min)`.
#' @keywords internal
#' @noRd
.pivot_to_list_column <- function(long_final, sites_input, iso_sf = NULL) {
  # ----- 1. sites coords (site_id -> lon, lat) -----------------------------
  sites_tbl <- if (inherits(sites_input, "sf")) {
    sf::st_drop_geometry(sites_input)
  } else {
    tibble::as_tibble(sites_input)
  }
  # `sites_tbl$lon`/`lat` may be absent if the user passed only the sf
  # geometry -- in that case fall back to NA columns so the 8-col schema
  # stays type-stable. The deep `lon`/`lat` validation lives in Sec.19 step 1.
  sites_coords <- tibble::tibble(
    site_id = unique(sites_tbl$site_id),
    lon     = if (!is.null(sites_tbl$lon))
                sites_tbl$lon[match(unique(sites_tbl$site_id), sites_tbl$site_id)]
              else NA_real_,
    lat     = if (!is.null(sites_tbl$lat))
                sites_tbl$lat[match(unique(sites_tbl$site_id), sites_tbl$site_id)]
              else NA_real_
  )

  long_tbl <- tibble::as_tibble(long_final)

  # ----- 2. acs_estimates + derived_rates nested ---------------------------
  # `estimand_family` is the Sec. 12.3.1 classifier -- non-rate rows nest
  # into `acs_estimates`, derived-rate rows nest into `derived_rates`.
  # NA-propagation rows preserve their template family via
  # `.build_na_propagation_rows()`, so failed pairs land in the correct
  # nested column.
  is_rate_row <- !is.na(long_tbl$estimand_family) &
                  long_tbl$estimand_family == "derived_rate"

  acs_part   <- long_tbl[!is_rate_row, , drop = FALSE]
  rates_part <- long_tbl[ is_rate_row, , drop = FALSE]

  # tidyr::nest() with `.by` keys the nest on those columns and bundles
  # every other column into the named list-column -- same semantics as the
  # negative-selection form but without requiring `tidyselect::` in
  # DESCRIPTION Imports.
  acs_nested <- if (nrow(acs_part) > 0L) {
    tidyr::nest(
      acs_part,
      .by  = c("site_id", "drive_time_min"),
      .key = "acs_estimates"
    )
  } else {
    tibble::tibble(
      site_id        = character(0),
      drive_time_min = integer(0),
      acs_estimates  = list()
    )
  }
  rates_nested <- if (nrow(rates_part) > 0L) {
    tidyr::nest(
      rates_part,
      .by  = c("site_id", "drive_time_min"),
      .key = "derived_rates"
    )
  } else {
    tibble::tibble(
      site_id        = character(0),
      drive_time_min = integer(0),
      derived_rates  = list()
    )
  }

  # ----- 3. metadata list-column (one list per pair) -----------------------
  # Per-pair provenance summary -- picks the first row per pair as the
  # representative (all rows in a pair share the same provider / profile /
  # acs_year / weight_method / osm_snapshot_date by construction of the
  # Sec.21 -> Sec.22 -> Sec.23 carry-through). `failure_origin` flags whether the
  # pair is an NA-propagation pair vs a normally-computed one.
  meta_pairs <- long_tbl |>
    dplyr::group_by(.data$site_id, .data$drive_time_min) |>
    dplyr::summarise(
      provider          = dplyr::first(.data$provider),
      profile           = dplyr::first(.data$profile),
      osm_snapshot_date = dplyr::first(.data$osm_snapshot_date),
      acs_year          = dplyr::first(.data$acs_year),
      weight_method     = dplyr::first(.data$weight_method),
      failure_origin    = dplyr::first(.data$failure_origin),
      n_tracts          = dplyr::first(.data$n_tracts),
      .groups = "drop"
    )
  metadata_col <- lapply(seq_len(nrow(meta_pairs)), function(i) {
    list(
      provider          = meta_pairs$provider[[i]],
      profile           = meta_pairs$profile[[i]],
      osm_snapshot_date = meta_pairs$osm_snapshot_date[[i]],
      acs_year          = meta_pairs$acs_year[[i]],
      weight_method     = meta_pairs$weight_method[[i]],
      failure_origin    = meta_pairs$failure_origin[[i]],
      n_tracts          = meta_pairs$n_tracts[[i]]
    )
  })
  meta_tbl <- tibble::tibble(
    site_id        = meta_pairs$site_id,
    drive_time_min = meta_pairs$drive_time_min,
    metadata       = metadata_col
  )

  # ----- 4. cross site coords with the unique pair set + assemble ----------
  # Build the (site_id, drive_time_min) skeleton from the union of every
  # nested + metadata key so any side that has the pair surfaces a row.
  pair_keys <- dplyr::bind_rows(
    dplyr::select(acs_nested,   "site_id", "drive_time_min"),
    dplyr::select(rates_nested, "site_id", "drive_time_min"),
    dplyr::select(meta_tbl,     "site_id", "drive_time_min")
  ) |>
    dplyr::distinct() |>
    dplyr::arrange(.data$site_id, .data$drive_time_min)

  out <- pair_keys |>
    dplyr::left_join(sites_coords, by = "site_id") |>
    dplyr::left_join(acs_nested,   by = c("site_id", "drive_time_min")) |>
    dplyr::left_join(rates_nested, by = c("site_id", "drive_time_min")) |>
    dplyr::left_join(meta_tbl,     by = c("site_id", "drive_time_min"))

  # `isochrone` starts as a type-stable placeholder list. v0.5 fills matching
  # cells from the resolved isochrone sf for computed and precomputed paths;
  # unmatched pairs intentionally remain NULL.
  out$isochrone <- vector("list", nrow(out))
  if (!is.null(iso_sf)) {
    out <- .cacs_pipe_iso_to_list_column(out, iso_sf = iso_sf)
    if (isTRUE(attr(out, "iso_was_filled"))) {
      # Caller-level emit. `.frequency` default = "always" -- the per-cacs_run
      # call gate is the `if (!is.null(iso_sf))` above.
      .cli_inform_listcol_iso_filled()
    }
  }

  # ----- 5. reorder to the locked 8-col Sec. 12.3.2 schema -----------------
  out[, c("site_id", "lon", "lat", "drive_time_min",
          "isochrone", "acs_estimates", "derived_rates", "metadata"),
      drop = FALSE]
}


# ============================================================================
# F6 cacs_run_result S3 surface (Phase 4, Step 4.2 -- locked contract).
#
# Class chain (prepended over the existing tbl_df/tbl/data.frame chain):
#   c("cacs_run_result", "tbl_df", "tbl", "data.frame")
#
# Metadata is attached as a single attribute (`cacs_run_result_metadata`) so
# that dplyr verbs that strip the cacs_run_result class still leave the bare
# tibble usable -- the print method guards `is.null(meta)` and degrades to
# the generic tibble-style preview when called on a post-dplyr stripped
# object.
# ============================================================================


#' Attach the cacs_run_result class + metadata attribute
#'
#' Tiny internal helper that prepends the `cacs_run_result` class on top of
#' the existing tibble/data.frame chain (without overwriting it) and attaches
#' the F6 metadata list as a single named attribute. The class is *prepended*
#' so that `print` / `summary` dispatch hits our methods first while every
#' downstream dplyr / sf / ggplot2 consumer still sees a fully-functional
#' tibble.
#'
#' @param out A `tbl_df` / `tbl` / `data.frame` returned by `cacs_run()`.
#' @param metadata A `list` per the Step 4.2 contract (11 named slots:
#'   `generated_at`, `cacs_version`, `provider`, `profile`, `drive_times`,
#'   `n_sites_total`, `n_sites_success`, `n_sites_failed`,
#'   `wall_clock_seconds`, `skipped_geoids`, `moe_fallback_summary`).
#'
#' @return `out` with `class` chain prepended and
#'   `cacs_run_result_metadata` attribute attached.
#' @keywords internal
#' @noRd
.attach_run_result_class <- function(out, metadata) {
  attr(out, "cacs_run_result_metadata") <- metadata
  class(out) <- c("cacs_run_result", class(out))
  out
}


#' Drop only the cacs_run_result class for dplyr-safe aggregation
#'
#' @keywords internal
#' @noRd
.cacs_run_result_plain <- function(x) {
  class(x) <- setdiff(class(x), "cacs_run_result")
  x
}


#' Group a catchmentACS run result as a plain tibble
#'
#' The `cacs_run_result` class is only a presentation wrapper around the
#' canonical long-format tibble returned by [cacs_run()]. dplyr grouping and
#' aggregation therefore drop down to the bare tibble class chain so that
#' classed reconstruction does not interfere with the result.
#'
#' @param .data A `cacs_run_result`, as returned by [cacs_run()].
#' @param ... Grouping variables passed to [dplyr::group_by()].
#' @param .add,.drop Passed to [dplyr::group_by()].
#'
#' @return A `grouped_df` without the `cacs_run_result` presentation class.
#' @family result summaries
#' @seealso [cacs_run()], [summary.cacs_run_result()],
#'   [as_tibble.cacs_run_result()], [cacs_describe()].
#' @exportS3Method dplyr::group_by cacs_run_result
group_by.cacs_run_result <- function(.data, ..., .add = FALSE, .drop = NULL) {
  data_plain <- .cacs_run_result_plain(.data)
  if (is.null(.drop)) {
    .drop <- dplyr::group_by_drop_default(data_plain)
  }
  dplyr::group_by(data_plain, ..., .add = .add, .drop = .drop)
}


#' Print a catchmentACS run result
#'
#' Custom `print` method for the `cacs_run_result` S3 class returned by
#' [cacs_run()]. It emits a cli header summarising the run-level metadata, a
#' "Top 5 rates (cross-site mean +/- standard deviation)" preview when rate
#' variables are present, an option-gated "Rates per site" mini-block, and the
#' standard tibble preview. It falls back to the generic tibble print when the
#' metadata attribute has been stripped (for example, by an upstream dplyr verb
#' that did not preserve attributes).
#'
#' @section Capturing cli output:
#' The section headers are emitted by `cli` on the message/condition stream.
#' A bare `capture.output(print(x))` captures the tibble preview on standard
#' output but may miss headers such as "catchmentACS run result", "Top 5
#' rates", and "Rates per site". For tests, prefer
#' `testthat::capture_messages(print(x))` or
#' `capture.output(print(x), type = "message")` when you need to assert on the
#' cli header text. Script-level logs can also merge both streams with the
#' shell pattern `Rscript script.R 2>&1 | tee run.log`.
#'
#' @param x A `cacs_run_result`, as returned by [cacs_run()].
#' @param ... Passed to the tibble print method via `NextMethod()`.
#' @param n_head Number of leading rows to preview (default `5`).
#' @param n_tail Number of trailing rows to preview (default `3`).
#'
#' @return Invisibly returns `x` (standard print contract).
#' @family result summaries
#' @seealso [cacs_run()], [summary.cacs_run_result()],
#'   [as_tibble.cacs_run_result()], [cacs_describe()].
#' @exportS3Method print cacs_run_result
print.cacs_run_result <- function(x, ..., n_head = 5, n_tail = 3) {
  meta <- attr(x, "cacs_run_result_metadata")
  if (is.null(meta)) {
    # Defensive: dplyr verb (or similar) stripped the metadata. Fall back to
    # the generic tibble print so downstream consumers do not see a broken
    # header for an otherwise-valid tibble.
    return(NextMethod())
  }
  x_plain <- .cacs_run_result_plain(x)

  cli::cli_h1("catchmentACS run result")
  cli::cli_text("Generated at: {format(meta$generated_at)}")
  cli::cli_text("Package version: {as.character(meta$cacs_version)}")
  cli::cli_text("Provider: {meta$provider} / {meta$profile}")
  cli::cli_text("Drive times: {meta$drive_times} min")
  cli::cli_text(paste0(
    "Sites: {meta$n_sites_success} success / ",
    "{meta$n_sites_failed} failed / ",
    "{meta$n_sites_total} total"
  ))

  if (length(meta$skipped_geoids) > 0L) {
    cli::cli_text(
      "Water/degenerate tracts skipped: {length(meta$skipped_geoids)}"
    )
  }

  # v0.4 Issue 004 Track 2: rate preview block -- renamed from "Top-3" to
  # "Top 5 rates (cross-site mean +/- sd)" (more honest about cross-site
  # collapse). Plus a per-site mini-block gated by
  # `catchmentACS.summary_per_site_max` option (default 12L).
  rate_vars <- c("poverty_rate", "snap_rate", "ssi_rate",
                 "unemp_rate", "labor_force_participation")
  if (nrow(x_plain) > 0L &&
      all(c("variable", "estimate", "moe") %in% colnames(x_plain))) {
    rates_summary <- x_plain |>
      dplyr::filter(.data$variable %in% rate_vars) |>
      dplyr::group_by(.data$variable) |>
      dplyr::summarise(
        mean_estimate = mean(.data$estimate, na.rm = TRUE),
        sd_estimate   = stats::sd(.data$estimate, na.rm = TRUE),
        n             = dplyr::n(),
        .groups       = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(.data$mean_estimate)) |>
      utils::head(5L)
    if (nrow(rates_summary) > 0L) {
      cli::cli_h2("Top 5 rates (cross-site mean +/- sd)")
      print(rates_summary)
    }

    # Per-site mini-block (option-gated).
    per_site_max <- getOption("catchmentACS.summary_per_site_max", 12L)
    n_sites_x <- dplyr::n_distinct(x_plain$site_id)
    if (isTRUE(n_sites_x <= per_site_max) && isTRUE(per_site_max > 0L)) {
      pivot <- .cacs_rates_per_site_pivot(x_plain)
      if (!is.null(pivot$wide) && nrow(pivot$wide) > 0L) {
        cli::cli_h2("Rates per site")
        print(pivot$wide)
      }
    }
  }

  cli::cli_text("Wall clock: {round(meta$wall_clock_seconds, 1)}s")
  cli::cli_h2("Tibble preview")
  NextMethod()

  cli::cli_alert_info(
    "For full output: {.code as_tibble(x)} or {.code x[]}"
  )

  invisible(x)
}


#' Summarise a catchmentACS run result
#'
#' Custom `summary` method for the `cacs_run_result` S3 class returned by
#' [cacs_run()]. It mirrors the `summary.lm` pattern by returning a structured
#' S3 list (`cacs_run_summary`) with its own `print` method. The summary
#' records the run metadata; the row, site, variable, and drive-time tallies; a
#' five-number summary of `n_tracts`; a per-rate breakdown (mean, standard
#' deviation, and count of missing values grouped by rate variable); and the
#' overall rate at which the margin of error (MOE) fell back to a default.
#'
#' @param object A `cacs_run_result`, as returned by [cacs_run()].
#' @param ... Currently unused (reserved for future filter arguments).
#' @param breakdown One of `"cross_site"` (the default), `"per_site"`, or
#'   `"both"`. Controls *what* [print.cacs_run_summary()] shows: the returned
#'   list always carries both the `rates_breakdown` slot (cross-site collapse)
#'   and the `rates_per_site` / `rates_per_site_moe` slots (a wide rate matrix
#'   keyed by `site_id` and, when present, `drive_time_min`) regardless.
#'
#' @return A `cacs_run_summary` list (class
#'   `c("cacs_run_summary", "list")`).
#' @family result summaries
#' @seealso [cacs_run()], [print.cacs_run_summary()],
#'   [cacs_summary_as_markdown()], [cacs_describe()].
#' @exportS3Method summary cacs_run_result
summary.cacs_run_result <- function(object, ...,
                                    breakdown = c("cross_site",
                                                  "per_site",
                                                  "both")) {
  breakdown <- match.arg(breakdown)
  meta <- attr(object, "cacs_run_result_metadata")

  rate_vars <- c("poverty_rate", "snap_rate", "ssi_rate",
                 "unemp_rate", "labor_force_participation")

  # v0.4 correctness fix: drop the `cacs_run_result` presentation class before
  # dplyr aggregation. v0.3's classed rates_breakdown path could misalign group
  # keys and summary values; all internal aggregation now uses the plain tibble.
  object_plain <- .cacs_run_result_plain(object)

  rates_breakdown <- if (nrow(object_plain) > 0L &&
                         all(c("variable", "estimate") %in% colnames(object_plain))) {
    object_plain |>
      dplyr::filter(.data$variable %in% rate_vars) |>
      dplyr::group_by(.data$variable) |>
      dplyr::summarise(
        mean = mean(.data$estimate, na.rm = TRUE),
        sd   = stats::sd(.data$estimate, na.rm = TRUE),
        n_NA = sum(is.na(.data$estimate)),
        .groups = "drop"
      )
  } else {
    tibble::tibble(
      variable = character(0),
      mean     = numeric(0),
      sd       = numeric(0),
      n_NA     = integer(0)
    )
  }

  # v0.4 Issue 004 Track 1: unconditional per-site rate pivot.
  # Layer 1 helper from Step 2.1. Always computed, regardless of `breakdown`.
  pivot <- .cacs_rates_per_site_pivot(object_plain)
  rates_per_site     <- pivot$wide
  rates_per_site_moe <- pivot$wide_moe

  moe_fallback_rate <- if ("moe_fallback" %in% colnames(object_plain)) {
    .fb <- object_plain$moe_fallback
    mean(!is.na(.fb) & .fb, na.rm = FALSE)
  } else {
    NA_real_
  }

  n_tracts_summary <- if ("n_tracts" %in% colnames(object_plain) &&
                          any(!is.na(object_plain$n_tracts))) {
    stats::fivenum(object_plain$n_tracts)
  } else {
    rep(NA_real_, 5L)
  }

  out <- list(
    metadata           = meta,
    n_rows             = nrow(object_plain),
    n_sites            = if ("site_id" %in% colnames(object_plain))
                           dplyr::n_distinct(object_plain$site_id) else 0L,
    n_variables        = if ("variable" %in% colnames(object_plain))
                           dplyr::n_distinct(object_plain$variable) else 0L,
    n_drive_times      = if ("drive_time_min" %in% colnames(object_plain))
                           dplyr::n_distinct(object_plain$drive_time_min) else 0L,
    n_tracts_summary   = n_tracts_summary,
    rates_breakdown    = rates_breakdown,
    rates_per_site     = rates_per_site,        # v0.4 NEW (Issue 004 Track 1)
    rates_per_site_moe = rates_per_site_moe,    # v0.4 NEW (Issue 004 Track 1)
    moe_fallback_rate  = moe_fallback_rate
  )

  attr(out, "breakdown") <- breakdown          # v0.4 -- used by print method
  class(out) <- c("cacs_run_summary", "list")
  out
}


#' Print a catchmentACS run summary
#'
#' Companion `print` method for the structured summary list returned by
#' [summary.cacs_run_result()]. It emits a cli header followed by the run
#' metadata one-liner; the row, site, and variable tallies; the `n_tracts`
#' five-number summary; the per-rate breakdown table; and the overall rate at
#' which the margin of error (MOE) fell back to a default.
#'
#' @param x A `cacs_run_summary`, as returned by [summary.cacs_run_result()].
#' @param ... Currently unused.
#'
#' @return Invisibly returns `x`.
#' @family result summaries
#' @seealso [cacs_run()], [summary.cacs_run_result()],
#'   [cacs_summary_as_markdown()], [cacs_describe()].
#' @exportS3Method print cacs_run_summary
print.cacs_run_summary <- function(x, ...) {
  cli::cli_h1("catchmentACS run summary")

  meta <- x$metadata
  if (!is.null(meta)) {
    cli::cli_text("Generated at: {format(meta$generated_at)}")
    cli::cli_text("Provider: {meta$provider} / {meta$profile}")
    cli::cli_text(paste0(
      "Sites: {meta$n_sites_success} success / ",
      "{meta$n_sites_failed} failed / ",
      "{meta$n_sites_total} total"
    ))
  }

  cli::cli_h2("Run tallies")
  cli::cli_text("Rows: {x$n_rows}")
  cli::cli_text("Sites: {x$n_sites}")
  cli::cli_text("Variables: {x$n_variables}")
  cli::cli_text("Drive-time bands: {x$n_drive_times}")

  cli::cli_h2("n_tracts five-number summary")
  .fn <- x$n_tracts_summary
  if (length(.fn) == 5L && !all(is.na(.fn))) {
    cli::cli_text(paste0(
      "min={round(.fn[1], 1)}, q1={round(.fn[2], 1)}, ",
      "median={round(.fn[3], 1)}, q3={round(.fn[4], 1)}, ",
      "max={round(.fn[5], 1)}"
    ))
  } else {
    cli::cli_text("(unavailable)")
  }

  # v0.4 Issue 004 Track 1: `breakdown` attribute controls which block(s)
  # to print. Default `"cross_site"` reproduces v0.3 print verbatim. Plus
  # an automatic per-site block when `n_sites` is small (gated by option).
  breakdown <- attr(x, "breakdown") %||% "cross_site"
  per_site_max <- getOption("catchmentACS.summary_per_site_max", 12L)
  show_cross <- breakdown %in% c("cross_site", "both")
  show_per   <- breakdown %in% c("per_site", "both") ||
                (identical(breakdown, "cross_site") &&
                 isTRUE(x$n_sites <= per_site_max) &&
                 isTRUE(per_site_max > 0L) &&
                 !is.null(x$rates_per_site) &&
                 nrow(x$rates_per_site) > 0L)

  if (show_cross) {
    cli::cli_h2("Rates breakdown (mean / sd / n_NA)")
    if (nrow(x$rates_breakdown) > 0L) {
      print(x$rates_breakdown)
    } else {
      cli::cli_text("(no rate variables found)")
    }
  }
  if (show_per) {
    cli::cli_h2("Rates per site")
    if (!is.null(x$rates_per_site) && nrow(x$rates_per_site) > 0L) {
      print(x$rates_per_site)
    } else {
      cli::cli_text("(no per-site rate data)")
    }
  }

  cli::cli_h2("MOE fallback rate")
  if (is.na(x$moe_fallback_rate)) {
    cli::cli_text("(no moe_fallback column available)")
  } else {
    cli::cli_text("{round(100 * x$moe_fallback_rate, 1)}% of rows fell back")
  }

  invisible(x)
}


#' Render a catchmentACS summary table as pipe Markdown
#'
#' Convenience wrapper for rendering summary tables -- especially the
#' `rates_breakdown` slot of `summary(cacs_run(...))` -- as Quarto- and
#' GitHub-friendly pipe Markdown. Numeric columns are rounded to three digits
#' unless overridden via `...`.
#'
#' @param summary_tbl A data frame or tibble summary table. Tables with a
#'   `site_id` column plus one or more recognised rate columns are treated as
#'   per-site (or per-site, per-drive-time) rate matrices; `drive_time_min`,
#'   when present, is kept immediately after `site_id`.
#' @param ... Additional arguments passed to [knitr::kable()], such as
#'   `digits`, `caption`, `col.names`, or `align`.
#'
#' @return A `knitr_kable` character vector containing a pipe-format Markdown
#'   table.
#' @family result summaries
#' @seealso [cacs_run()], [summary.cacs_run_result()],
#'   [print.cacs_run_summary()], [cacs_describe()].
#' @export
#'
#' @examples
#' if (requireNamespace("knitr", quietly = TRUE)) {
#'   summary_tbl <- tibble::tibble(
#'     variable = "poverty_rate",
#'     mean = 0.1234,
#'     sd = 0.0567,
#'     n_NA = 0L
#'   )
#'   cacs_summary_as_markdown(summary_tbl)
#' }
cacs_summary_as_markdown <- function(summary_tbl, ...) {
  if (!inherits(summary_tbl, "data.frame")) {
    .cli_abort_schema(c(
      "{.arg summary_tbl} must be a data frame or tibble.",
      "x" = "Got {.cls {class(summary_tbl)[[1L]]}}."
    ))
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg knitr} is required to render summary Markdown.",
      "i" = "Install it with {.code install.packages(\"knitr\")}."
    ), class = c("catchmentACS_error_missing_suggest",
                 "catchmentACS_error",
                 "catchmentACS_condition"))
  }

  args <- c(list(x = summary_tbl, format = "pipe"), list(...))

  # v0.4 Issue 004 Track 4: detect per-site shapes by column-name signature.
  # When the input is a `rates_per_site` (numeric) or `rates_per_site_moe`
  # (character "x +/- y") shape, render with a rate-first column order +
  # informative default caption.
  cols <- colnames(summary_tbl)
  rate_vars <- names(cacs_acs_default_rates)
  is_per_site <- "site_id" %in% cols && any(rate_vars %in% cols)
  if (is_per_site) {
    # Detect MOE-string variant by looking at the first non-NA cell in the
    # first rate column. Character -> MOE-string variant; numeric -> estimate-only.
    first_rate_col <- intersect(rate_vars, cols)[[1L]]
    first_vals <- summary_tbl[[first_rate_col]]
    is_moe_string <- is.character(first_vals)
    if (is.null(args$caption)) {
      args$caption <- if (is_moe_string) {
        "Rates per site (estimate \u00b1 90% MOE)"
      } else {
        "Rates per site (estimate)"
      }
    }
    # Reorder columns to canonical: key columns + sanctioned rates order.
    key_cols <- c("site_id", intersect("drive_time_min", cols))
    cols_ordered <- c(key_cols,
                      intersect(rate_vars, cols),
                      setdiff(cols, c(key_cols, rate_vars)))
    args$x <- summary_tbl[, cols_ordered, drop = FALSE]
  }

  if (is.null(args$digits)) {
    args$digits <- 3
  }
  do.call(knitr::kable, args)
}
# ----------------------------------------------------------------------------
# .cacs_format_estimate_moe_string() -- Helper #2 of Step 2.1.
#
# Vectorized "<estimate> +/- <moe>" formatter (base R + vapply). NA-safe per
# the Step 2.1 contract:
#   - both NA            -> NA_character_
#   - estimate NA only   -> "NA +/- <moe>"
#   - moe NA only        -> "<estimate> +/- NA"
#   - neither NA         -> "<estimate> +/- <moe>"
#
# Numeric formatting uses sprintf("%.*f", digits, x) so the output digit count
# is exactly `digits` (no scientific notation, no trailing-zero stripping --
# the column is a presentation surface, not a round-trip serialization).
# The U+00B1 PLUS-MINUS SIGN literal is the same one used in Issue 004's
# "Rates per site (estimate +/- 90% MOE)" examples.
# ----------------------------------------------------------------------------


#' Format an estimate / MOE pair as a single "x +/- y" string
#'
#' Pure vectorized formatter shared by `.cacs_rates_per_site_pivot()` (for
#' the `wide_moe` list element) and downstream Markdown/print helpers.
#' NA-safe per the Step 2.1 four-way table: only the literal `"NA"` token
#' appears in mixed-NA cells; both-NA collapses to `NA_character_` so
#' downstream consumers can `is.na()`-test cleanly.
#'
#' @param estimate Numeric vector of point estimates.
#' @param moe Numeric vector of margins of error (same length as `estimate`).
#' @param digits Integer scalar; number of fractional digits passed to
#'   `sprintf("%.*f", digits, x)`. Default 3.
#'
#' @return Character vector of the same length as `estimate`; `NA_character_`
#'   only when *both* sides are `NA`.
#' @keywords internal
#' @noRd
.cacs_format_estimate_moe_string <- function(estimate, moe, digits = 3L) {
  # Length parity + scalar-digits guards. These are cheap, byte-traceable,
  # and let the per-cell vapply below stay assumption-free.
  if (length(estimate) != length(moe)) {
    stop(".cacs_format_estimate_moe_string(): length(estimate) must equal length(moe).",
         call. = FALSE)
  }
  if (length(digits) != 1L || !is.numeric(digits) || is.na(digits) ||
      !is.finite(digits) || digits < 0) {
    stop(".cacs_format_estimate_moe_string(): `digits` must be a non-negative scalar integer.",
         call. = FALSE)
  }
  digits <- as.integer(digits)

  n <- length(estimate)
  if (n == 0L) {
    return(character(0))
  }

  na_est <- is.na(estimate)
  na_moe <- is.na(moe)

  # Per-side string ahead of the vapply: this keeps the hot path branch-free
  # and lets vapply collapse the 4-way truth table in a single pass.
  est_str <- ifelse(na_est, "NA", sprintf("%.*f", digits, estimate))
  moe_str <- ifelse(na_moe, "NA", sprintf("%.*f", digits, moe))

  vapply(
    seq_len(n),
    function(i) {
      if (na_est[i] && na_moe[i]) NA_character_
      else paste0(est_str[i], " \u00b1 ", moe_str[i])
    },
    character(1)
  )
}


# ----------------------------------------------------------------------------
# .cacs_rates_per_site_pivot() -- Helper #1 of Step 2.1.
#
# Pure pivot: long-format `run_result` -> list(wide, wide_moe). Base R hot
# path -- `split()` by site_id, `match()` rate-name into a pre-allocated
# numeric matrix per site, then `do.call(rbind, ...)` to assemble. No
# dplyr / tidyr in the body (only `tibble::tibble()` for the output
# constructor, which is the package convention for shape stability under
# 0-row edge cases). NA cells fall out naturally because the pre-allocated
# matrices start as `NA_real_`.
#
# Contract per `_plan-master-v0.4-update/03-phase-2-helpers.qmd` Step 2.1:
#   - Input:  `run_result_long` (a `cacs_run_result` or bare long tibble with
#             at minimum `site_id`, `variable`, `estimate` columns;
#             `moe` optional), and `rates_names` (defaulting to
#             `names(.SANCTIONED_RATES_V1)`).
#   - Output: list(wide = <tibble>, wide_moe = <tibble or NULL>):
#       * `wide`     : tibble[n_sites x (1 + n_rates)], `site_id` first,
#                      one numeric column per rate in `rates_names` order.
#       * `wide_moe` : same shape, character cells = "<est> +/- <moe>", or
#                      NULL with `attr(out, "moe_unavailable") = TRUE` when
#                      the input has no `moe` column.
#   - Edge cases:
#       * `n_sites = 0`           -> 0-row tibble, column names/types kept.
#       * (site, rate) missing    -> NA_real_ in `$wide`, NA_character_ in
#                                    `$wide_moe`.
#       * `moe` column absent     -> `$wide_moe = NULL` + list-level attr.
# ----------------------------------------------------------------------------


#' Fill the `$isochrone` list-column of `.pivot_to_list_column()`'s output
#'
#' v0.4 Issue 003 Layer 1 helper, broadened in v0.5 to accept any resolved
#' isochrone `sf`. Replaces the `vector("list", n)` placeholders with
#' per-`(site_id, drive_time_min)` rows from `iso_sf`. If `iso_sf = NULL`,
#' `out` is returned unchanged as a defensive helper fallback.
#'
#' Pure function: takes `out` + `iso_sf`, returns mutated `out`. Caller is
#' responsible for emitting `catchmentACS_message_listcol_iso_filled` (gated
#' by the returned `attr(out, "iso_was_filled")` flag).
#'
#' @param out A tibble already shaped by `.pivot_to_list_column()` (must
#'   contain `site_id`, `drive_time_min`, and a list-column `isochrone` of
#'   length `nrow(out)`).
#' @param iso_sf An `sf` of resolved isochrones keyed by
#'   `(site_id, drive_time_min)`, or `NULL` for the defensive no-fill path.
#'
#' @return The same `out` tibble with `$isochrone` filled (1-row sf per
#'   matching pair) when `iso_sf` is non-NULL; unchanged otherwise. When
#'   any fill happened, `attr(out, "iso_was_filled") = TRUE` is set so the
#'   caller can decide whether to emit a classed message.
#' @keywords internal
#' @noRd
.cacs_pipe_iso_to_list_column <- function(out, iso_sf = NULL) {
  if (is.null(iso_sf)) {
    return(out)
  }
  # Defensive: required columns
  if (!all(c("site_id", "drive_time_min") %in% colnames(out))) {
    return(out)
  }
  if (!inherits(iso_sf, "sf")) {
    return(out)
  }
  iso_cols <- colnames(iso_sf)
  if (!all(c("site_id", "drive_time_min") %in% iso_cols)) {
    return(out)
  }

  # Coerce iso_sf keys for robust matching (drive_time_min sometimes integer,
  # sometimes numeric in different fixture/source paths).
  iso_sites <- as.character(iso_sf$site_id)
  iso_dtm   <- suppressWarnings(as.integer(iso_sf$drive_time_min))
  out_sites <- as.character(out$site_id)
  out_dtm   <- suppressWarnings(as.integer(out$drive_time_min))

  any_filled <- FALSE
  isochrone_col <- if (is.list(out$isochrone)) out$isochrone else vector("list", nrow(out))

  for (i in seq_len(nrow(out))) {
    hit <- which(iso_sites == out_sites[[i]] &
                 iso_dtm   == out_dtm[[i]])
    if (length(hit) >= 1L) {
      # Slice keeps sf class + geometry column for the matched row(s).
      isochrone_col[[i]] <- iso_sf[hit[[1L]], , drop = FALSE]
      any_filled <- TRUE
    }
  }
  out$isochrone <- isochrone_col

  if (any_filled) {
    attr(out, "iso_was_filled") <- TRUE
  }
  out
}


#' Pivot the long-format run-result into a key x rate wide tibble
#'
#' Shared internal helper for Issue 004's four-track per-site rate-matrix
#' deliverable. Computes the per-site rate matrix *once* so every downstream
#' S3 surface (`summary()`, `print()`, `as_tibble()`,
#' `cacs_summary_as_markdown()`) renders byte-identical column ordering --
#' "the per-site wide table is computed once and serialized as the same
#' logical object" (Issue 004, Recommendation).
#'
#' Base-R / `vapply` hot path: filters the long tibble to rate rows with
#' `%in%`, splits by `site_id` plus `drive_time_min` when available, then
#' writes each key's estimates into a single pre-allocated `NA_real_` matrix
#' slot via `match(rate, rates_names)`.
#' The matrix is `cbind`/`tibble`-wrapped at the end. Zero non-base
#' dependencies in the per-site loop.
#'
#' @param run_result_long A `cacs_run_result` (or bare long tibble) with at
#'   minimum `site_id`, `variable`, `estimate` columns; `moe` optional.
#' @param rates_names Character vector of rate names in the desired output
#'   column order. Defaults to `names(.SANCTIONED_RATES_V1)` (the v1.0
#'   sanctioned five).
#'
#' @return A 2-element `list`:
#'   * `wide`     -- `tibble[n_keys x (key columns + n_rates)]`; `site_id`
#'     first, optional `drive_time_min` second, then one `NA_real_`-padded
#'     numeric column per rate in `rates_names` order.
#'   * `wide_moe` -- same shape but character `"<est> +/- <moe>"` cells, or
#'     `NULL` when input has no `moe` column. The returned list carries
#'     `attr(out, "moe_unavailable") = TRUE` in the latter case.
#'
#' @keywords internal
#' @noRd
.cacs_rates_per_site_pivot <- function(run_result_long, rates_names = NULL) {

  # ----- 1. defaults + minimal guards -------------------------------------
  if (is.null(rates_names)) {
    rates_names <- names(.SANCTIONED_RATES_V1)
  }
  if (!is.character(rates_names) || length(rates_names) == 0L ||
      anyNA(rates_names) || any(!nzchar(rates_names))) {
    stop(".cacs_rates_per_site_pivot(): `rates_names` must be a non-empty character vector.",
         call. = FALSE)
  }
  rl_cols <- colnames(run_result_long)
  required_cols <- c("site_id", "variable", "estimate")
  missing_cols <- setdiff(required_cols, rl_cols)
  if (length(missing_cols) > 0L) {
    stop(
      ".cacs_rates_per_site_pivot(): input missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  has_moe <- "moe" %in% rl_cols
  has_drive_time <- "drive_time_min" %in% rl_cols
  key_cols <- c("site_id", if (has_drive_time) "drive_time_min" else NULL)

  n_rates <- length(rates_names)

  # ----- 2. 0-row fast path ------------------------------------------------
  # Both an outright empty input and an input that has rows but zero rate
  # rows resolve to the same shape: 0 x (1 + n_rates) tibble with the right
  # column names + numeric types. `tibble::tibble()` here is the same
  # constructor `.pivot_to_list_column()` uses for its 0-row branches, so
  # the shape contract stays consistent across helpers.
  build_empty <- function() {
    key_empty <- list(site_id = character(0))
    if (has_drive_time) {
      key_empty$drive_time_min <- run_result_long$drive_time_min[0]
    }
    cols <- c(
      key_empty,
      stats::setNames(replicate(n_rates, numeric(0), simplify = FALSE),
                      rates_names)
    )
    tibble::as_tibble(cols)
  }
  build_empty_moe <- function() {
    key_empty <- list(site_id = character(0))
    if (has_drive_time) {
      key_empty$drive_time_min <- run_result_long$drive_time_min[0]
    }
    cols <- c(
      key_empty,
      stats::setNames(replicate(n_rates, character(0), simplify = FALSE),
                      rates_names)
    )
    tibble::as_tibble(cols)
  }

  # ----- 3. filter to rate rows + early exit ------------------------------
  # Subset by index vector (base R) to keep the dependency surface minimal;
  # column drop happens at the same time so the per-site split stays small.
  keep_cols <- c(key_cols, "variable", "estimate",
                 if (has_moe) "moe" else NULL)
  is_rate_row <- run_result_long$variable %in% rates_names
  filtered <- run_result_long[is_rate_row, keep_cols, drop = FALSE]

  if (nrow(filtered) == 0L) {
    out <- list(
      wide     = build_empty(),
      wide_moe = if (has_moe) build_empty_moe() else NULL
    )
    if (!has_moe) attr(out, "moe_unavailable") <- TRUE
    return(out)
  }

  # Duplicate key-rate rows would otherwise overwrite an earlier matrix cell.
  # Abort loudly because the presentation surface cannot choose the right value.
  dup_data <- filtered[c(key_cols, "variable")]
  dup_key <- do.call(
    paste,
    c(lapply(dup_data, as.character), sep = "\r")
  )
  if (anyDuplicated(dup_key)) {
    stop(
      ".cacs_rates_per_site_pivot(): duplicate rate rows for key column(s) ",
      paste(c(key_cols, "variable"), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  # ----- 4. key ordering ---------------------------------------------------
  # `unique()` preserves first-occurrence order; we mirror that as the row
  # order of the output so a caller who passed a stably-ordered input gets a
  # stably-ordered output (matches v0.3 09_rates_wide.rds row order).
  key_data <- filtered[key_cols]
  key_id <- do.call(
    paste,
    c(lapply(key_data, as.character), sep = "\r")
  )
  key_keep <- !duplicated(key_id)
  key_tbl <- tibble::as_tibble(key_data[key_keep, , drop = FALSE])
  key_ids <- key_id[key_keep]
  n_keys <- length(key_ids)

  # ----- 5. base R hot path: split + match -> pre-allocated matrices -------
  # `split()` returns a named list of integer row indices by key. We
  # then walk each key once, `match()` its rate names into the
  # `rates_names` position vector, and write into a single n_keys x n_rates
  # pre-allocated `NA_real_` matrix. Cells with no row (the "missing
  # (key, rate) row" edge case) stay `NA_real_` by construction.
  est_mat <- matrix(
    NA_real_,
    nrow = n_keys, ncol = n_rates,
    dimnames = list(NULL, rates_names)
  )
  moe_mat <- if (has_moe) {
    matrix(NA_real_, nrow = n_keys, ncol = n_rates,
           dimnames = list(NULL, rates_names))
  } else {
    NULL
  }

  # Group row indices by key; using `factor(..., levels = key_ids)`
  # locks the split order to the unique() order regardless of how `split()`
  # would otherwise sort character keys.
  key_factor <- factor(key_id, levels = key_ids)
  row_idx_by_key <- split(seq_len(nrow(filtered)), key_factor)

  for (i in seq_len(n_keys)) {
    idx <- row_idx_by_key[[i]]
    if (length(idx) == 0L) next  # key has no rate row at all -> stays NA
    col_pos <- match(filtered$variable[idx], rates_names)
    # Defensive: drop any NA col_pos (shouldn't happen because of the
    # earlier `%in%` filter, but a future caller might pass a custom
    # `rates_names` that doesn't perfectly cover `unique(filtered$variable)`).
    good <- !is.na(col_pos)
    if (any(good)) {
      est_mat[i, col_pos[good]] <- filtered$estimate[idx][good]
      if (has_moe) {
        moe_mat[i, col_pos[good]] <- filtered$moe[idx][good]
      }
    }
  }

  # ----- 6. matrix -> tibble (single do.call(rbind, ...)-style construction) ---
  # `tibble::tibble()` named-list constructor is the package's standard 0-row
  # safe builder; passing each rate column by name in `rates_names` order
  # locks the column order to the contract regardless of platform / locale.
  wide_cols <- c(
    as.list(key_tbl),
    stats::setNames(
      lapply(seq_len(n_rates), function(j) est_mat[, j]),
      rates_names
    )
  )
  wide <- tibble::as_tibble(wide_cols)

  # ----- 7. wide_moe -------------------------------------------------------
  # Per-column vectorized format (one `.cacs_format_estimate_moe_string()`
  # call per rate column rather than per cell) so the helper does
  # n_rates calls -- cheap for the n_sites = 50 perf test.
  if (has_moe) {
    moe_cols_list <- lapply(seq_len(n_rates), function(j) {
      .cacs_format_estimate_moe_string(est_mat[, j], moe_mat[, j], digits = 3L)
    })
    wide_moe_cols <- c(
      as.list(key_tbl),
      stats::setNames(moe_cols_list, rates_names)
    )
    wide_moe <- tibble::as_tibble(wide_moe_cols)
  } else {
    wide_moe <- NULL
  }

  out <- list(wide = wide, wide_moe = wide_moe)
  if (!has_moe) attr(out, "moe_unavailable") <- TRUE
  out
}



# ============================================================================
# v0.4 Issue 004 Track 3 -- `as_tibble.cacs_run_result()` rate-first sort.
# ============================================================================


#' Convert a catchmentACS run result to a plain tibble (rate-first by default)
#'
#' An S3 method on `tibble::as_tibble()` so that `as_tibble(run_result)` -- the
#' canonical way to drop the `cacs_run_result` class for downstream dplyr or
#' ggplot2 consumers -- re-orders rows so that derived rate rows surface
#' *above* the source ACS variables within each site block. This makes the
#' most important deliverable (the five rates per site) the first thing the
#' user sees on `print(as_tibble(x), n = 10)`.
#'
#' Disable the rate-first lift via `as_tibble(x, rate_first = FALSE)` or
#' `options(catchmentACS.rate_first_default = FALSE)`. In that mode the
#' underlying long-format row order is returned as-is, with no additional
#' alphabetical re-sort. The first time the rate-first default takes effect in
#' a session it is announced once via an informational message.
#'
#' @param x A `cacs_run_result`, as returned by [cacs_run()].
#' @param ... Passed through to the data-frame method of [tibble::as_tibble()].
#' @param rate_first Logical. When `TRUE` (default), rate rows sort above
#'   source ACS rows within each `site_id` and `drive_time_min` block. When
#'   `FALSE`, the rate-first lift is skipped and the underlying long-format row
#'   order is returned as-is. The default is taken from
#'   `getOption("catchmentACS.rate_first_default", TRUE)` when not supplied.
#'
#' @return A plain `tbl_df` (class chain `c("tbl_df", "tbl", "data.frame")`).
#' @family result summaries
#' @seealso [cacs_run()], [summary.cacs_run_result()],
#'   [group_by.cacs_run_result()], [cacs_describe()].
#' @exportS3Method tibble::as_tibble cacs_run_result
as_tibble.cacs_run_result <- function(x, ..., rate_first = NULL) {
  if (is.null(rate_first)) {
    rate_first <- getOption("catchmentACS.rate_first_default", TRUE)
  }

  # Drop class first so NextMethod()-equivalent semantics flow through
  # tibble's default coercion.
  class(x) <- setdiff(class(x), "cacs_run_result")
  out <- tibble::as_tibble(x, ...)

  if (isTRUE(rate_first) &&
      all(c("site_id", "drive_time_min", "variable") %in% colnames(out)) &&
      nrow(out) > 0L) {
    rate_vars <- names(cacs_acs_default_rates)
    # 0 = rate row (sorts first within site block); 1 = source ACS row.
    is_rate <- out$variable %in% rate_vars
    .rate_sort <- ifelse(is_rate, 0L, 1L)
    ord <- order(out$site_id,
                 out$drive_time_min,
                 .rate_sort,
                 out$variable)
    out <- out[ord, , drop = FALSE]
    .cli_inform_rate_first_changed()  # once_per_session emit
  }

  out
}
