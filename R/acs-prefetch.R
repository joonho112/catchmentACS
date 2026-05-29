# ============================================================================
# acs-prefetch.R - cacs_acs_prefetch() single-state ACS fetch wrapper.
#                  Sec. 20.3 algorithm Steps 1-7 (validation, codebook,
#                  cache key, tidycensus retry, schema validation) +
#                  Steps 8-10 (suppression carry-through, 17-field
#                  provenance attribute, .rds + optional .gpkg persist)
#                  appended in Step 4.2.
#
# v0.1 alpha scope (per Sec. 20.2 + Sec. 39.3):
#   * state: scalar only (multi-state deferred to a future release)
#   * survey: "acs5" only (acs1 deferred v1.1)
#   * geography: "tract" only ("block group"/"county" deferred v0.2/v1.1)
#   * variables: NULL / "core" / "extended" / character vector - 4 modes
#
# 4-tier policy per Sec. 20.1:
#   1. Schema      - fail-loud (state/year/survey/geography/scalar args)
#   2. Variable    - permissive partial skip (W-20-04) / all invalid abort
#                    (E-20-07)
#   3. Network     - 3-retry exponential backoff (1s/2s/4s) on 5xx;
#                    4xx immediate abort (E-20-13);
#                    retry-exhausted abort (E-20-14)
#   4. Suppression - carry-through (Step 8 in Step 4.2)
#
# Internal helpers wired here:
#   * .tidycensus_get_acs_call() - thin seam for local_mocked_bindings()
#   * .validate_state_in_fips()  - tigris::fips_codes lookup
#   * .classify_tidycensus_error() - 4xx/5xx/unknown bucketing
#   * .resolve_variables()       - Sec. 20.3 step 3, 4-mode resolution
#   * .resolve_cache_dir()       - Sec. 20.2 cache_dir resolution
#   * .acs_geometry_vintage()    - Sec. 20.4 cache-key tuple component
#   * .drop_water_tracts()       - v0.2 BUG-001 (F3) hybrid water-tract
#                                  filter (regex + zero-area fallback)
# ============================================================================


# ---------------------------------------------------------------------------
# BUG-001 (F3) water-tract GEOID regex constant.
#
# Census Bureau convention: special-purpose / water tracts encode the
# county-internal tract code as `99nn.nn` (digits 6-7 == "99"). The 11-digit
# GEOID is STATE(2) + COUNTY(3) + TRACT(6); the water-code mask therefore is
# `^[0-9]{2}[0-9]{3}99[0-9]{4}$`. Defence-in-depth: this lexical signal is
# paired with a degenerate-area fallback (st_area() <= 0 | !is.finite()) so
# either Census schema drift OR sf/PROJ geometry drift surfaces the bad row.
# ---------------------------------------------------------------------------

.WATER_TRACT_REGEX <- "^[0-9]{2}[0-9]{3}99[0-9]{4}$"


#' Fetch single-state ACS tract data with caching and resilient retries
#'
#' Wraps `tidycensus::get_acs()` to pull American Community Survey (ACS)
#' five-year estimates for the census tracts of a single state, then caches
#' the result with a content-addressed SHA-256 key so repeat calls are served
#' from disk. The fetch is hardened with a four-tier validation and resilience
#' policy: strict argument checking, tolerant handling of invalid variable
#' codes (skip the bad ones, abort only if all are invalid), exponential-backoff
#' retries on transient server errors, and carry-through of the Census Bureau's
#' suppression sentinels into clean `NA`s. Every result also carries a
#' provenance attribute recording exactly how it was produced. The current
#' release accepts a single state as a scalar USPS or FIPS code; passing a
#' vector of states is not yet supported.
#'
#' @param state Character(1) USPS code (`"AL"`) or 2-digit FIPS code
#'   (`"01"`). Must be a single uppercase string; lowercase input and vectors
#'   are rejected rather than silently coerced, so the cache key stays stable.
#' @param year Integer ACS five-year end year, within the release-verified
#'   range `[2009, 2024]`; default `2023L`.
#' @param variables Which ACS variables to fetch. One of: `NULL` (the package's
#'   default catalogue, the dataset `cacs_acs_default_vars`); `"core"`, an alias
#'   for `NULL`; `"extended"`, a placeholder that currently resolves to the same
#'   set as `"core"`; or a character vector of explicit ACS variable codes
#'   (each matching the pattern `` `^B[0-9]{5}_[0-9]{3}$` ``, e.g. `"B19013_001"`).
#' @param survey Character(1); `"acs5"` (the five-year ACS) is the only value
#'   accepted in the current release.
#' @param geography Character(1); `"tract"` is the only value accepted in the
#'   current release. Block-group and county geographies are planned for a
#'   future release.
#' @param cache_dir Character(1) directory, or `NULL` (the default) to resolve
#'   the cache root via [cacs_cache_dir()] and write under its `acs/`
#'   subdirectory.
#' @param write_gpkg Logical(1); default `FALSE`. When `TRUE`, a GeoPackage
#'   (`.gpkg`) is written alongside the canonical `.rds` cache file for use in
#'   QGIS, Python, or other GIS tools.
#' @param force_refresh Logical(1); default `FALSE`. When `TRUE`, the cache
#'   lookup is bypassed and a live tidycensus call is forced. The fresh result
#'   is still written back to the cache when cache writes are enabled.
#' @param drop_water_tracts Logical(1); default `TRUE`. When `TRUE`, any tract
#'   whose GEOID matches the Census water / special-purpose mask
#'   `` `^[0-9]{2}[0-9]{3}99[0-9]{4}$` `` or whose geometry has non-positive or
#'   non-finite area is dropped right after schema validation and before
#'   suppression handling. A message lists up to 5 dropped GEOIDs (with a
#'   `(+N more)` suffix when the list overflows). Set to `FALSE` to retain
#'   water tracts (e.g. for coastline-overlap research); downstream,
#'   [cacs_intersect_weight()] then demotes its own degenerate-area abort to a
#'   warn-and-skip path (see that function's details).
#' @param verbose Logical(1); default `TRUE`. Controls progress reporting.
#'   Because this function makes a single `tidycensus::get_acs()` round-trip
#'   per call, the automatic mode shows only a start message (\dQuote{Fetching
#'   ACS...}) and a completion summary, with no per-step progress bar. Override
#'   with `Sys.setenv(CACS_QUIET = "1")` to silence output, or
#'   `options(catchmentACS.progress = "off" | "auto" | "force")`; setting
#'   `"force"` upgrades the start/finish messages to a ticking progress bar.
#'   See [cacs_run()] for the package-wide progress reporting policy.
#'
#' @return An `sf` tibble with 6 canonical columns (`GEOID`, `NAME`,
#'   `variable`, `estimate`, `moe`, `geometry`) in EPSG:4269 (NAD83), where
#'   `moe` is the ACS margin of error. The result also carries a provenance
#'   attribute recording how it was produced (state, year, variable counts,
#'   suppression tallies, package versions, and more) and a
#'   `cacs_schema_version` attribute set to `"1.0"`.
#'
#' @section Cache behavior:
#' ACS cache entries are guarded by SHA-256 fingerprint sidecar files. Older
#' `.rds` entries written without a sidecar are treated as a cache miss and
#' recomputed. A cached result with suspiciously few rows (fewer than
#' `getOption("catchmentACS.stale_threshold_rows", 1000L)`) emits a stale-cache
#' warning with a recovery recipe. Set the threshold to `0L` to disable the
#' heuristic, or use a state-specific option such as
#' `options(catchmentACS.stale_threshold_WY = 500L)` for small-state workflows.
#' A guard against single-variable false positives is controlled by
#' `options(catchmentACS.stale_min_variables = 6L)`. Inspect cache state with
#' [cacs_get_cache_state()] and disable caching for a session with
#' [cacs_set_cache()] (`cacs_set_cache(FALSE)`).
#'
#' @section Live Census access:
#' Cache hits are served without a `CENSUS_API_KEY`. Cache misses and
#' `force_refresh = TRUE` require a live Census API key before any tidycensus
#' metadata or data request is attempted. This release pins five-year ACS
#' support to the Census years verified for it, currently `2009` through
#' `2024`.
#'
#' @section Water tracts:
#' Some census tracts cover open water or other special-purpose areas and have
#' no usable geometry, which can break downstream spatial weighting. When
#' `drop_water_tracts = TRUE` (the default), such tracts are removed using two
#' independent signals, so a tract slips through only if *both* the Census
#' GEOID convention and the geometry-area check fail to flag it:
#' \itemize{
#'   \item \strong{Primary - GEOID pattern}: digits 6-7 of the GEOID equal
#'         `"99"`, per the Census water / special-purpose tract convention.
#'   \item \strong{Fallback - degenerate area}: `sf::st_area()` is
#'         non-positive or non-finite, which catches geometry corruption even
#'         when the GEOID pattern does not fire.
#' }
#' Dropped GEOIDs are reported in a single message (capped at 5 GEOIDs with a
#' `(+N more)` suffix). Opt out via `drop_water_tracts = FALSE`.
#'
#' @seealso [cacs_run()] for the end-to-end pipeline and the package-wide
#'   progress reporting policy; [cacs_intersect_weight()] for the next step
#'   that consumes this output; [cacs_acs_validate()] to check ACS data;
#'   [cacs_get_cache_state()] and [cacs_set_cache()] for cache control.
#' @family core pipeline
#' @export
#' @examples
#' \dontrun{
#' al <- cacs_acs_prefetch(state = "AL", year = 2023, survey = "acs5",
#'                         geography = "tract", variables = NULL)
#' }
cacs_acs_prefetch <- function(state,
                              year = 2023L,
                              variables = NULL,
                              survey = "acs5",
                              geography = "tract",
                              cache_dir = NULL,
                              write_gpkg = FALSE,
                              force_refresh = FALSE,
                              drop_water_tracts = TRUE,
                              verbose = TRUE) {

  # =======================================================================
  # Sec. 20.3 Step 1 - Validate state (canonical scalar; scalar-only contract)
  # =======================================================================

  # 1a: must be character
  if (!is.character(state)) {
    .cli_abort_schema(c(
      "{.arg state} must be {.cls character(1)} USPS code or 2-digit FIPS.",
      "x" = "Got class {.cls {class(state)[1L]}}.",
      "i" = "Pass a single string such as {.val AL} or {.val 01}."
    ))
  }

  # 1b: must be scalar (no silent vector promotion)
  if (length(state) != 1L) {
    .cli_abort_schema(c(
      "{.arg state} must be a single string in the current release.",
      "x" = "Got {.cls character} of length {.val {length(state)}}: {.val {utils::head(state, 5L)}}.",
      "*" = "Multi-state: call once per state + {.fn dplyr::bind_rows}, or use the {.fn cacs_run} orchestrator (Sec. 24). Vector signature deferred to v1.1."
    ))
  }

  # 1c: must be 2 characters wide (USPS code OR 2-digit FIPS)
  if (is.na(state) || nchar(state) != 2L) {
    .cli_abort_schema(c(
      "{.arg state} must be exactly 2 characters wide (USPS code or 2-digit FIPS).",
      "x" = "Got {.val {state}} (nchar = {.val {if (is.na(state)) NA_integer_ else nchar(state)}})."
    ))
  }

  # 1d: must be uppercase (no silent toupper to keep cache key stable)
  if (state != toupper(state)) {
    .cli_abort_schema(c(
      "{.arg state} must be uppercase (no silent toupper in v0.1).",
      "i" = "Try {.val {toupper(state)}} explicitly to preserve frozen-snapshot cache-key stability (Sec. 20.2)."
    ))
  }

  # 1e: must resolve via tigris::fips_codes
  .validate_state_in_fips(state)


  # =======================================================================
  # Sec. 20.3 Step 2 - Validate year / survey / geography / logical scalars
  # =======================================================================

  year_range <- .cacs_acs5_year_range()
  # year: scalar numeric, finite, integer-valued, in the release-verified range
  if (length(year) != 1L || is.na(year) ||
      !is.numeric(year) || !is.finite(year)) {
    .cli_abort_schema(c(
      "{.arg year} must be a finite numeric scalar.",
      "x" = "Got {.cls {class(year)[1L]}} of length {.val {length(year)}}."
    ))
  }
  if (!isTRUE(year == floor(year))) {
    .cli_abort_schema(c(
      "{.arg year} must be an integer-valued ACS5 end year.",
      "x" = "Got {.val {year}}.",
      "i" = "Use a whole-number year such as {.val 2024}."
    ))
  }
  if (year < year_range[["min"]] || year > year_range[["max"]]) {
    .cli_abort_schema(c(
      "{.arg year} must be in the release-verified ACS5 range {.val [{year_range[['min']]}, {year_range[['max']]}]}.",
      "x" = "Got {.val {year}}.",
      "i" = "ACS 5-year coverage begins in 2009; v0.5.0 is verified through the 2024 ACS5 release."
    ))
  }
  year <- as.integer(year)

  # survey: acs5 only in v0.1 (acs1 deferred E-20-08 hint)
  if (!is.character(survey) || length(survey) != 1L || is.na(survey)) {
    .cli_abort_schema(c(
      "{.arg survey} must be a {.cls character(1)} value."
    ))
  }
  if (!identical(survey, "acs5")) {
    .cli_abort_schema(c(
      "{.arg survey} must be {.val acs5} in the current release.",
      "x" = "Got {.val {survey}}.",
      "i" = "{.val acs1} support deferred to v1.1 (large-area only)."
    ))
  }

  # geography: tract only in the current release
  if (!is.character(geography) || length(geography) != 1L || is.na(geography)) {
    .cli_abort_schema(c(
      "{.arg geography} must be a {.cls character(1)} value."
    ))
  }
  if (!identical(geography, "tract")) {
    .cli_abort_schema(c(
      "{.arg geography} must be {.val tract} in the current release.",
      "x" = "Got {.val {geography}}.",
      "i" = "{.val block group} and {.val county} support are deferred to future releases."
    ))
  }

  # logical scalars
  for (nm in c("write_gpkg", "force_refresh", "drop_water_tracts", "verbose")) {
    val <- get(nm)
    if (!is.logical(val) || length(val) != 1L || is.na(val)) {
      .cli_abort_schema(c(
        "{.arg {nm}} must be {.cls logical(1)} (TRUE or FALSE).",
        "x" = "Got {.cls {class(val)[1L]}} of length {.val {length(val)}}."
      ))
    }
  }

  # cache_dir: NULL or character(1)
  if (!is.null(cache_dir)) {
    if (!is.character(cache_dir) || length(cache_dir) != 1L || is.na(cache_dir)) {
      .cli_abort_schema(c(
        "{.arg cache_dir} must be {.cls character(1)} or {.val NULL}."
      ))
    }
  }


  # =======================================================================
  # Sec. 20.3 Step 3 - Resolve variables (4-mode: NULL / "core" / "extended" /
  #                                           character vector)
  # =======================================================================

  resolved <- .resolve_variables(variables)
  variables_resolved <- resolved$vars
  vars_source        <- resolved$source
  variables_effective <- variables_resolved
  variables_skipped   <- character(0)


  # =======================================================================
  # Sec. 20.3 Step 5 - Compute 12-dim ACS cache key per Sec. 20.4
  # =======================================================================

  make_cache_key <- function(vars) {
    cache_payload <- list(
      state              = state,
      year               = year,
      survey             = survey,
      geography          = geography,
      variables          = sort(unique(vars)),
      geometry_vintage   = .acs_geometry_vintage(year, geography),
      tidycensus_version = as.character(utils::packageVersion("tidycensus")),
      tigris_version     = as.character(utils::packageVersion("tigris")),
      sf_version         = as.character(utils::packageVersion("sf")),
      schema_version     = "1.0",
      package_version    = as.character(utils::packageVersion("catchmentACS")),
      r_version          = paste(R.version$major, R.version$minor, sep = ".")
    )  # 12 keys per Sec. 20.4
    .cacs_cache_key(cache_payload, namespace = "acs")
  }

  read_cached_acs <- function(key) {
    if (isTRUE(force_refresh)) {
      return(NULL)
    }
    cached <- suppressMessages(.cacs_cache_get(
      key, "acs", cache_dir = cache_dir, replay_conditions = FALSE
    ))
    if (is.null(cached)) {
      return(NULL)
    }
    cached_conditions <- .cacs_cache_conditions(cached)
    if (isTRUE(verbose)) {
      .cli_inform_cache(
        c("Cache hit for ACS {.val {year}} {.val {survey}} {.val {geography}} state {.val {state}}.",
          "i" = "{.val {nrow(cached)}} row{?s} from cached snapshot."),
        phase = "acs"
      )
    }
    # The cached object already carries the Sec. 20.5 17-field provenance
    # attribute from the prior write (Step 10 below). Round-trip it
    # unchanged for cache-hit identity (T20-10).
    #
    # v0.2 BUG-001 (F3): the cache key does NOT include `drop_water_tracts`
    # (it would force a full re-pull for every toggle). Instead, the
    # water-tract filter is re-applied here on cache HIT - that way both
    # legacy caches (written by v0.1 pre-filter) AND fresh caches honor
    # the runtime `drop_water_tracts` argument. The filter is cheap (a
    # regex + an st_area() call) compared to the network round-trip it
    # avoids. Provenance attributes carried by `cached` survive because
    # `.drop_water_tracts()` returns an `sf` subset that preserves
    # attributes via standard `[` indexing.
    filtered <- .drop_water_tracts(
      acs_sf            = cached,
      drop_water_tracts = drop_water_tracts,
      verbose           = verbose
    )
    out_cached <- .cacs_cache_strip_conditions(filtered$kept_sf)
    if (isTRUE(verbose) && isTRUE(drop_water_tracts) &&
        length(cached_conditions) > 0L &&
        length(filtered$dropped_geoids) == 0L) {
      .cacs_cache_signal_conditions(cached_conditions)
    }
    out_cached
  }

  cache_key <- make_cache_key(variables_effective)
  cached_out <- read_cached_acs(cache_key)
  if (!is.null(cached_out)) {
    return(cached_out)
  }


  # =======================================================================
  # Sec. 20.3 Step 4 - Codebook validation (partial-batch tolerance)
  # =======================================================================
  #
  # Cache hits above are allowed without a Census key. Any cache miss (or
  # force_refresh) needs a live key before metadata or data requests. Then
  # tidycensus::load_variables() is the canonical codebook. We tolerate
  # non-availability of the codebook itself (offline / mock test contexts) by
  # passing through to get_acs(), except for endpoint/year-not-found failures
  # that should surface as schema-tier year availability errors.

  if (!nzchar(Sys.getenv("CENSUS_API_KEY"))) {
    .cli_abort_credential(c(
      "{.envvar CENSUS_API_KEY} is not set.",
      "i" = "Set via {.code Sys.setenv(CENSUS_API_KEY = '<key>')} or add to {.path ~/.Renviron}.",
      "*" = "Request a key at {.url https://api.census.gov/data/key_signup.html}."
    ))
  }

  codebook_error <- NULL
  codebook <- tryCatch(
    tidycensus::load_variables(year, survey),
    error = function(e) {
      codebook_error <<- e
      NULL
    }
  )

  if (!is.null(codebook_error) &&
      isTRUE(.is_tidycensus_endpoint_not_found(codebook_error))) {
    .cli_abort_schema(c(
      "tidycensus could not load the ACS5 codebook for {.val {year}}.",
      "x" = "Message: {.val {conditionMessage(codebook_error)}}.",
      "i" = "catchmentACS v0.5.0 is verified for ACS5 years {.val {year_range[['min']]}} through {.val {year_range[['max']]}}."
    ))
  }

  if (!is.null(codebook) && "name" %in% names(codebook)) {
    valid_mask <- variables_resolved %in% codebook$name
    n_invalid  <- sum(!valid_mask)

    # 4c - ALL invalid -> abort E-20-07
    if (!any(valid_mask)) {
      .cli_abort_variable(c(
        "{.arg variables} contains 0 codes valid in the tidycensus codebook for ({.val {year}}, {.val {survey}}).",
        "x" = "All {.val {length(variables_resolved)}} requested invalid: {.val {utils::head(variables_resolved, 5L)}}.",
        "i" = "Check {.fn tidycensus::load_variables}({.val {year}}, {.val {survey}})."
      ))
    }

    # 4d - partial invalid -> W-20-04 + skip
    if (!all(valid_mask)) {
      skipped <- variables_resolved[!valid_mask]
      .cli_warn_variable(c(
        "Skipping {.val {n_invalid}} invalid variable{?s} in codebook ({.val {year}}, {.val {survey}}).",
        "i" = "Skipped: {.val {utils::head(skipped, 5L)}}."
      ))
      variables_effective <- variables_resolved[valid_mask]
      variables_skipped   <- skipped
    }
  }

  cache_key_after_codebook <- make_cache_key(variables_effective)
  if (!identical(cache_key_after_codebook, cache_key)) {
    cache_key <- cache_key_after_codebook
    cached_out <- read_cached_acs(cache_key)
    if (!is.null(cached_out)) {
      return(cached_out)
    }
  }


  # =======================================================================
  # Sec. 20.3 Step 6 - tidycensus fetch with 3-retry exponential backoff
  # =======================================================================

  if (isTRUE(verbose)) {
    .cli_inform_cache(
      c("i" = "Fetching ACS {.val {year}} {.val {survey}} {.val {geography}} for state {.val {state}}..."),
      phase = "acs"
    )
  }

  # v0.2 F1: bookend-mode reporter for the single get_acs() round-trip.
  # N = 1 -> mode resolver returns "bookend" (no per-event tick, just
  # start/finish summary). `prog$tick()` is still called once so the eta()
  # closure can report a sane elapsed-time at finish; in bookend mode the
  # tick is a no-op on emission. on.exit() ensures the summary lands even
  # if the retry chain exhausts and aborts via .cli_abort_network/operator.
  acs_prog <- .cacs_progress_reporter(n = 1L, label = "ACS prefetch",
                                      verbose = verbose)
  acs_prog_state <- new.env(parent = emptyenv())
  acs_prog_state$n_success <- 0L
  acs_prog_state$n_failed  <- 1L
  on.exit(
    acs_prog$finish(
      n_success = acs_prog_state$n_success,
      n_failed  = acs_prog_state$n_failed
    ),
    add = TRUE
  )

  result <- NULL
  for (attempt in seq_len(3L)) {
    result <- tryCatch(
      .tidycensus_get_acs_call(
        geography = geography,
        variables = variables_effective,
        state     = state,
        year      = year,
        survey    = survey
      ),
      error = function(e) .classify_tidycensus_error(e)
    )

    # Success: result is the raw sf object (not a classified condition)
    if (!inherits(result, "tidycensus_classified_error")) break

    # 4xx - immediate abort (E-20-13 operator error)
    if (inherits(result, "tidycensus_4xx")) {
      .cli_abort_operator(c(
        "tidycensus returned 4xx (bad request).",
        "x" = "Message: {.val {attr(result, 'cacs_msg')}}.",
        "i" = "Likely invalid year/survey combination or quota exhausted."
      ))
    }

    # 5xx - retry with exponential backoff (1s, 2s, 4s)
    if (inherits(result, "tidycensus_5xx") && attempt < 3L) {
      backoff <- 2L^(attempt - 1L)  # 1, 2, 4 seconds
      .cli_warn_runtime(c(
        "tidycensus transient 5xx; retry {.val {attempt}}/3 after {.val {backoff}}s.",
        "i" = "Message: {.val {attr(result, 'cacs_msg')}}."
      ), phase = "acs")
      Sys.sleep(backoff)
      next
    }

    # Retry exhausted -> E-20-14 network error
    if (attempt == 3L) {
      .cli_abort_network(c(
        "tidycensus retry exhausted (3 attempts).",
        "x" = "Last message: {.val {attr(result, 'cacs_msg')}}."
      ))
    }
  }

  # If the result is still a classified error here, something unexpected
  # happened (e.g. neither 4xx nor 5xx classification matched). Treat as
  # network error for safety.
  if (inherits(result, "tidycensus_classified_error")) {
    .cli_abort_network(c(
      "tidycensus fetch failed with unclassified error.",
      "x" = "Message: {.val {attr(result, 'cacs_msg')}}."
    ))
  }

  out <- result

  # v0.2 F1: fetch succeeded; flip the bookend summary to success. The
  # on.exit() registered above will emit "ACS prefetch complete: 1/1 in ..."
  # on function return. tick() increments the internal counter so eta()
  # reports 0:00 at finish time.
  acs_prog$tick()
  acs_prog_state$n_success <- 1L
  acs_prog_state$n_failed  <- 0L


  # =======================================================================
  # Sec. 20.3 Step 7 - Post-fetch schema validation (delegates to Phase 2.3)
  # =======================================================================

  ok <- .validate_acs_schema(out, abort = TRUE)  # aborts on any failure
  stopifnot(isTRUE(ok))


  # =======================================================================
  # v0.2 BUG-001 (F3) - Water-tract hybrid filter (regex + zero-area).
  #
  # Runs immediately after schema validation and BEFORE suppression carry-
  # through (Step 8) so the row counts attached to provenance (n_tracts,
  # n_total_rows, n_suppressed) reflect the post-filter universe. Default
  # ON (`drop_water_tracts = TRUE`); users opt out by passing FALSE.
  # =======================================================================

  cached_conditions <- list()
  drop_result <- withCallingHandlers(
    .drop_water_tracts(
      acs_sf            = out,
      drop_water_tracts = drop_water_tracts,
      verbose           = verbose
    ),
    catchmentACS_message_water_tract_filter = function(cnd) {
      cached_conditions[[length(cached_conditions) + 1L]] <<- cnd
    }
  )
  out <- drop_result$kept_sf


  # =======================================================================
  # Sec. 20.3 Step 8 - Suppression carry-through + MOE sentinel handling
  # =======================================================================
  #
  # The U.S. Census Bureau encodes "estimate suppressed" / "MOE not
  # available" cells with the negative sentinel value -555555555. Silent
  # passthrough would produce *negative variance* downstream in Sec. 22 MOE
  # propagation, yielding non-sensible aggregated MOEs. Convert sentinel
  # -> NA *before* attribute attach so downstream consumers see clean NAs.
  #
  # We additionally track:
  #   * n_suppressed / n_estimate_suppressed - rows where estimate is NA
  #   * n_moe_sentinel                       - rows where moe is sentinel
  #   * n_negative_moe                       - non-sentinel negative MOEs
  #   * n_total_rows                         - total rows in the long-format sf
  # and emit W-20-05-style warnings when either estimate suppression or MOE
  # sentinel rates exceed 10% of rows.

  total_rows <- nrow(out)
  estimate_suppressed <- is.na(out$estimate)
  moe_sentinel <- !is.na(out$moe) & (out$moe == -555555555)
  negative_moe <- !is.na(out$moe) & out$moe < 0 & !moe_sentinel

  n_estimate_suppressed <- sum(estimate_suppressed)
  n_moe_sentinel <- sum(moe_sentinel)
  n_negative_moe <- sum(negative_moe)
  n_suppressed <- n_estimate_suppressed

  if (n_moe_sentinel > 0L) {
    out$moe[moe_sentinel] <- NA_real_
  }
  # Defensive: any remaining negative MOE is invalid - also coerce to NA.
  out$moe[!is.na(out$moe) & out$moe < 0] <- NA_real_

  if (total_rows > 0L && (n_estimate_suppressed / total_rows) > 0.10) {
    .cli_warn_runtime(c(
      "More than 10% of rows have suppressed estimates ({n_estimate_suppressed}/{total_rows}).",
      "i" = "Suppressed estimates remain {.val NA} for downstream aggregation.",
      "*" = "Downstream point estimates and MOEs may be unavailable for affected variables."
    ), phase = "acs")
  }

  if (total_rows > 0L && (n_moe_sentinel / total_rows) > 0.10) {
    .cli_warn_runtime(c(
      "More than 10% of rows have suppressed MOEs ({n_moe_sentinel}/{total_rows}).",
      "i" = "Bureau suppression sentinel ({.val -555555555}) converted to {.val NA}.",
      "*" = "Downstream MOE propagation will treat these rows as missing uncertainty."
    ), phase = "acs")
  }


  # =======================================================================
  # Sec. 20.3 Step 9 - Attach provenance attribute (Sec. 20.5 17 fields)
  # =======================================================================
  #
  # ACS row dimension is tract x variable (potentially ~12k rows for AL);
  # row-level provenance columns would duplicate identical values N times.
  # Object-level attribute attach is the memory-efficient encoding chosen
  # in Sec. 20.5. Downstream Sec. 21 cacs_intersect_weight() reads these attrs and
  # demotes them into per-row long-format columns (acs_year etc.) at that
  # boundary.

  prov_versions <- tryCatch(
    list(
      tidycensus = as.character(utils::packageVersion("tidycensus")),
      sf         = as.character(utils::packageVersion("sf"))
    ),
    error = function(e) list(tidycensus = NA_character_, sf = NA_character_)
  )

  # Use named character vector for variables so callers can introspect
  # symbolic ACS code -> human label later (label column carried by tidycensus
  # codebook); v0.1 stores names == values for round-trip stability.
  vars_named <- variables_effective
  names(vars_named) <- variables_effective

  prov <- list(
    state                 = state,
    year                  = year,
    survey                = survey,
    geography             = geography,
    variables             = vars_named,
    variable_source       = vars_source,
    n_variables_requested = length(variables_resolved),
    n_variables_received  = length(variables_effective),
    n_variables_skipped   = length(variables_skipped),
    n_tracts              = length(unique(out$GEOID)),
    n_suppressed          = n_suppressed,
    n_estimate_suppressed = n_estimate_suppressed,
    n_moe_sentinel        = n_moe_sentinel,
    n_negative_moe        = n_negative_moe,
    n_total_rows          = total_rows,
    cache_key             = cache_key,
    cache_namespace       = "acs",
    generated_at          = as.POSIXct(Sys.time(), tz = "UTC"),
    tidycensus_version    = prov_versions$tidycensus,
    sf_version            = prov_versions$sf,
    acs_geometry_vintage  = .acs_geometry_vintage(year, geography)
  )

  attr(out, "cacs_provenance") <- prov
  attr(out, "cacs_acs_provenance") <- prov
  attr(out, "cacs_schema_version") <- "1.0"

  # Strip the temporary Step 4.1 staging hooks now that their content has
  # been folded into the provenance attribute.
  attr(out, "cacs_cache_key_pending") <- NULL
  attr(out, "cacs_vars_source")       <- NULL
  attr(out, "cacs_variables_skipped") <- NULL


  # =======================================================================
  # Sec. 20.3 Step 10 - Persist (.rds + optional .gpkg)
  # =======================================================================
  #
  # `.cacs_cache_put()` (Phase 2.2) writes atomically via `<key>.rds.tmp ->
  # file.rename()` to `<key>.rds` under <cacs_cache_dir>/acs/. Optional
  # GeoPackage co-write (`write_gpkg = TRUE`) provides QGIS/Python
  # interop; .rds remains the canonical cache layer regardless.
  #
  # Explicit `cache_dir` is honored by the shared cache helpers and interpreted
  # as the cache root; this function writes under `<cache_dir>/acs/`.

  put_ok <- .cacs_cache_put(
    out, cache_key, "acs", cache_dir = cache_dir,
    conditions = cached_conditions
  )
  if (isTRUE(verbose) && isTRUE(put_ok)) {
    .cli_inform_cache(
      c("Wrote ACS fixture ({.val {total_rows}} row{?s}, key {substr(cache_key, 1, 8)}...).",
        "i" = "Cache namespace: {.val acs}; state {.val {state}}, year {.val {year}}."),
      phase = "acs"
    )
  }

  if (isTRUE(write_gpkg)) {
    base_dir   <- .cacs_cache_base_dir(cache_dir = cache_dir, create = TRUE)
    gpkg_dir   <- file.path(base_dir, "acs")
    if (!dir.exists(gpkg_dir)) {
      dir.create(gpkg_dir, recursive = TRUE, showWarnings = FALSE)
    }
    gpkg_path  <- file.path(gpkg_dir, paste0(cache_key, ".gpkg"))
    gpkg_ok <- tryCatch({
      suppressWarnings(suppressMessages(
        sf::st_write(out, gpkg_path, driver = "GPKG",
                     delete_dsn = TRUE, quiet = TRUE)
      ))
      TRUE
    },
    error = function(e) {
      .cli_warn_runtime(c(
        "GPKG write failed; .rds cache intact.",
        "x" = "Path: {.path {gpkg_path}}",
        "i" = "Error: {.val {conditionMessage(e)}}"
      ), phase = "acs")
      FALSE
    })
    if (isTRUE(verbose) && isTRUE(gpkg_ok)) {
      .cli_inform_cache(
        c("Wrote ACS GeoPackage co-cache.",
          "i" = "Path: {.path {gpkg_path}}"),
        phase = "acs"
      )
    }
  }

  out
}


# ============================================================================
# Internal: .tidycensus_get_acs_call() - thin seam for local_mocked_bindings()
# ============================================================================

#' Thin wrapper around \code{tidycensus::get_acs()} for test mocking
#'
#' Tests inject a mock via
#' \code{testthat::local_mocked_bindings(.tidycensus_get_acs_call = mockfn,
#' .package = "catchmentACS")} so the live HTTP path is exercised only in
#' the \code{CACS_LIVE_PROVIDER=true} smoke test.
#'
#' @keywords internal
#' @noRd
.tidycensus_get_acs_call <- function(geography, variables, state, year, survey) {
  tidycensus::get_acs(
    geography    = geography,
    variables    = variables,
    state        = state,
    year         = year,
    survey       = survey,
    output       = "tidy",
    geometry     = TRUE,
    cache_table  = TRUE
  )
}


# ============================================================================
# Internal: .validate_state_in_fips() - tigris::fips_codes lookup
# ============================================================================

#' Validate that `state` matches a known USPS code or 2-digit FIPS
#'
#' @keywords internal
#' @noRd
.validate_state_in_fips <- function(state) {
  fips <- tryCatch(tigris::fips_codes, error = function(e) NULL)
  if (is.null(fips)) {
    # tigris not available - fall back to a hardcoded 51-USPS list so unit
    # tests can run offline without tigris's lazy load.
    usps <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI",
              "ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN",
              "MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH",
              "OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA",
              "WV","WI","WY")
    fips_codes_hardcoded <- sprintf("%02d", c(1,2,4,5,6,8,9,10,11,12,13,15,
                                              16,17,18,19,20,21,22,23,24,25,
                                              26,27,28,29,30,31,32,33,34,35,
                                              36,37,38,39,40,41,42,44,45,46,
                                              47,48,49,50,51,53,54,55,56))
    ok <- state %in% usps || state %in% fips_codes_hardcoded
  } else {
    ok <- state %in% unique(fips$state) ||
          state %in% unique(fips$state_code)
  }
  if (!isTRUE(ok)) {
    .cli_abort_schema(c(
      "{.arg state} is not a known USPS code or 2-digit FIPS.",
      "x" = "Got {.val {state}}.",
      "i" = "Try {.val AL} (USPS) or {.val 01} (FIPS) - see {.fn tigris::fips_codes}."
    ))
  }
  invisible(TRUE)
}


# ============================================================================
# Internal: .cacs_acs5_year_range() - release-verified ACS5 support window
# ============================================================================

#' Release-verified ACS5 year range
#'
#' v0.5.0 intentionally pins ACS5 support to the latest Census ACS5 year that
#' was validated for this package release. Do not derive this from Sys.Date():
#' Census publishes API endpoints on its own release calendar.
#'
#' @return named integer vector `c(min = 2009L, max = 2024L)`
#' @keywords internal
#' @noRd
.cacs_acs5_year_range <- function() {
  c(min = 2009L, max = 2024L)
}


# ============================================================================
# Internal: .is_tidycensus_endpoint_not_found()
# ============================================================================

#' Detect tidycensus endpoint/year-not-found metadata failures
#'
#' @keywords internal
#' @noRd
.is_tidycensus_endpoint_not_found <- function(e) {
  msg <- tolower(conditionMessage(e))
  grepl("api endpoint not found|does this data set exist|404|not[[:space:]_.-]*found",
        msg)
}


# ============================================================================
# Internal: .resolve_variables() - Sec. 20.3 step 3 4-mode resolution
# ============================================================================

#' Resolve `variables` argument to a canonical character vector + source label
#'
#' @return list(vars = character, source = character(1))
#' @keywords internal
#' @noRd
.resolve_variables <- function(variables) {
  # 3a - NULL -> default catalogue
  if (is.null(variables)) {
    e <- new.env(parent = emptyenv())
    utils::data("cacs_acs_default_vars", package = "catchmentACS", envir = e)
    vars <- unname(e$cacs_acs_default_vars)
    return(list(vars = sort(unique(vars)), source = "default_catalogue"))
  }

  # 3b/3c - string shorthand
  if (is.character(variables) && length(variables) == 1L &&
      variables %in% c("core", "extended")) {
    e <- new.env(parent = emptyenv())
    utils::data("cacs_acs_default_vars", package = "catchmentACS", envir = e)
    vars <- unname(e$cacs_acs_default_vars)
    # v0.1: "extended" maps to same set as core (placeholder per Sec. 20.6).
    src  <- if (identical(variables, "core")) "core_alias" else "extended_alias"
    return(list(vars = sort(unique(vars)), source = src))
  }

  # 3d - explicit character vector
  if (is.character(variables) && length(variables) > 0L) {
    if (any(is.na(variables)) || any(!nzchar(variables))) {
      .cli_abort_schema(c(
        "{.arg variables} must not contain {.val NA} or empty strings.",
        "i" = "Drop blanks/NA before passing."
      ))
    }
    # Pattern check: ACS codes look like ^B[0-9]{5}_[0-9]{3}$
    pat <- "^B[0-9]{5}_[0-9]{3}$"
    if (!all(grepl(pat, variables))) {
      bad <- variables[!grepl(pat, variables)]
      .cli_abort_schema(c(
        "{.arg variables} contains code{?s} that do not match the ACS pattern {.val {pat}}.",
        "x" = "First malformed: {.val {utils::head(bad, 5L)}}.",
        "i" = "ACS codes look like {.val B19013_001}."
      ))
    }
    return(list(vars = sort(unique(variables)), source = "user_supplied"))
  }

  # 3e - anything else -> schema abort
  .cli_abort_schema(c(
    "{.arg variables} must be {.val NULL}, {.val core}, {.val extended}, or a non-empty character vector.",
    "x" = "Got {.cls {class(variables)[1L]}}."
  ))
}


# ============================================================================
# Internal: .classify_tidycensus_error() - bucket 4xx / 5xx / unknown
# ============================================================================

#' Classify a captured tidycensus error condition
#'
#' Returns an object whose class chain carries one of
#' \code{c("tidycensus_4xx", "tidycensus_classified_error")},
#' \code{c("tidycensus_5xx", "tidycensus_classified_error")}, or
#' \code{c("tidycensus_unknown", "tidycensus_classified_error")}.
#' Step 6 of the algorithm dispatches on these classes.
#'
#' @keywords internal
#' @noRd
.classify_tidycensus_error <- function(e) {
  msg <- conditionMessage(e)
  msg_lc <- tolower(msg)

  bucket <- if (grepl("\\b5[0-9]{2}\\b|server error|timeout|timed.out|504|502|503", msg_lc)) {
    "tidycensus_5xx"
  } else if (grepl("\\b4[0-9]{2}\\b|bad request|forbidden|unauthorized|not.found|400|401|403|404", msg_lc)) {
    "tidycensus_4xx"
  } else {
    "tidycensus_unknown"
  }

  obj <- structure(
    list(parent = e),
    class      = c(bucket, "tidycensus_classified_error")
  )
  attr(obj, "cacs_msg") <- msg
  obj
}


# ============================================================================
# Internal: .resolve_cache_dir() - Step 6 / Step 4.2 cache_dir resolution
# ============================================================================

#' Resolve effective ACS cache directory (creating if necessary)
#'
#' @keywords internal
#' @noRd
.resolve_cache_dir <- function(cache_dir = NULL) {
  if (!is.null(cache_dir)) {
    if (!dir.exists(dirname(cache_dir))) {
      .cli_abort_operator(c(
        "Parent of {.path {cache_dir}} does not exist.",
        "i" = "Create the parent directory first."
      ))
    }
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
    return(normalizePath(cache_dir, mustWork = FALSE, winslash = "/"))
  }
  # Default: <cacs_cache_dir>/acs
  file.path(cacs_cache_dir(create = TRUE), "acs")
}


# ============================================================================
# Internal: .acs_geometry_vintage() - Sec. 20.4 cache-key tuple component
# ============================================================================

#' Map an ACS (year, geography) to its tigris geometry vintage label
#'
#' v0.1 returns a deterministic string keyed off the ACS terminal year +
#' geography. If a future tidycensus change shifts the geometry vintage
#' independently, this function is the single point of update.
#'
#' @keywords internal
#' @noRd
.acs_geometry_vintage <- function(year, geography) {
  paste0("tigris_", as.integer(year), "_", geography)
}


# ============================================================================
# Internal: .drop_water_tracts() - v0.2 BUG-001 (F3) hybrid water-tract filter
# ============================================================================

#' Hybrid water/special-purpose tract drop (defence in depth)
#'
#' Implements the locked Step 3.1 hybrid scheme: combine
#' the Census GEOID convention regex (digits 6-7 == \code{"99"}) with a
#' degenerate-area fallback so the filter is robust against either signal
#' drifting in isolation. Each dropped row is annotated with the reason it
#' was dropped (\code{"geoid_pattern"}, \code{"zero_area"}, or
#' \code{"geoid_pattern+zero_area"}) for downstream provenance.
#'
#' @param acs_sf an \code{sf} tibble in the canonical 6-col long ACS schema
#'   (must carry \code{GEOID} + geometry).
#' @param drop_water_tracts logical(1); when \code{FALSE} the function is a
#'   no-op (returns the input untouched + empty diagnostic vectors).
#' @param verbose logical(1); when \code{TRUE} and any rows are dropped,
#'   emits \code{catchmentACS_message_water_tract_filter}.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{\code{kept_sf}}{the post-filter \code{sf}.}
#'   \item{\code{dropped_geoids}}{character vector of dropped GEOIDs.}
#'   \item{\code{dropped_reasons}}{character vector aligned with
#'     \code{dropped_geoids}.}
#' }
#'
#' @keywords internal
#' @noRd
.drop_water_tracts <- function(acs_sf,
                               drop_water_tracts = TRUE,
                               verbose = TRUE) {
  if (!isTRUE(drop_water_tracts)) {
    return(list(
      kept_sf         = acs_sf,
      dropped_geoids  = character(0),
      dropped_reasons = character(0)
    ))
  }
  if (is.null(acs_sf) || nrow(acs_sf) == 0L) {
    return(list(
      kept_sf         = acs_sf,
      dropped_geoids  = character(0),
      dropped_reasons = character(0)
    ))
  }

  # Criterion 1 - lexical GEOID pattern (Census water-tract convention).
  pat_match <- grepl(.WATER_TRACT_REGEX, acs_sf$GEOID)

  # Criterion 2 - degenerate geometry. suppressWarnings() silences the
  # non-equal-area "longitude/latitude" notice for EPSG:4269 inputs; the
  # `<= 0` predicate is projection-insensitive (positive vs zero/non-finite
  # holds regardless of CRS scaling). The area test fires even when the
  # GEOID convention regex misses (defence in depth).
  area_m <- suppressWarnings(as.numeric(sf::st_area(acs_sf)))
  area_bad <- !is.finite(area_m) | area_m <= 0

  drop_mask <- pat_match | area_bad

  # Diagnostic reasons aligned with each row of the input (then subset
  # to the drop_mask universe before returning).
  reasons <- character(length(acs_sf$GEOID))
  reasons[pat_match & !area_bad] <- "geoid_pattern"
  reasons[area_bad & !pat_match] <- "zero_area"
  reasons[pat_match & area_bad]  <- "geoid_pattern+zero_area"

  if (any(drop_mask) && isTRUE(verbose)) {
    .cli_inform_water_tract_filter(
      geoids  = unique(acs_sf$GEOID[drop_mask]),
      reasons = reasons[drop_mask]
    )
  }

  list(
    kept_sf         = acs_sf[!drop_mask, , drop = FALSE],
    dropped_geoids  = acs_sf$GEOID[drop_mask],
    dropped_reasons = reasons[drop_mask]
  )
}
