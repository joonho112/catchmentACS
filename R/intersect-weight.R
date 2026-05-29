# ============================================================================
# intersect-weight.R - cacs_intersect_weight() (THE HEART of catchmentACS)
#
# Sec. 21 deep spec + Sec. 40 implementation plan (Phase 5).
#
# Current release weighting layer (Decision #5):
#   - weight_method = "area"        : functional (Steps 5.2-5.4)
#   - weight_method = "population"  : fail-loud abort with future-release hint
#
# Step 5.1 covers Sec. 21.3 PRE-LOOP steps 1-4. Steps 5-15 land in 5.2/5.3/5.4.
#
# Defensive stance: hardened against
#   (a) sf_use_s2 leak on mid-flight abort  -> layered on.exit chain
#   (b) corrupt CRS metadata                -> tryCatch around each st_transform
#   (c) MULTIPOLYGON area precompute        -> documented + asserted
#   (d) CONUS boundary edge cases           -> 0.25 deg pad in .bbox_outside_conus_dc
#   (e) silent batch start before long loop -> verbose pre-loop summary inform
# ============================================================================


# ----------------------------------------------------------------------------
# Public entry (Sec. 21.2 LOCKED 6-arg signature)
# ----------------------------------------------------------------------------

#' Aggregate ACS tract estimates to isochrone catchments by area weight
#'
#' Aggregates American Community Survey (ACS) tract-level estimates to
#' isochrone catchments via area-weighted intersection on EPSG:5070 (NAD83 /
#' CONUS Albers Equal Area). An *isochrone* is the drive-time travel polygon
#' around a site; a *tract* is a small Census geographic unit. The current
#' release supports `weight_method = "area"` only; `weight_method =
#' "population"` is a planned future extension and currently stops with a
#' clear error.
#'
#' The function intersects each isochrone with ACS tract geometries, drops
#' spatial slivers (negligible overlap fragments), computes area weights,
#' aggregates the source ACS variables, and attaches the carrier table used
#' by downstream margin-of-error (MOE) propagation and rate derivation.
#'
#' Results are cached. The cache key is content-aware: it folds in the
#' upstream isochrone identity, the actual ACS estimate values, any optional
#' block-group population inputs, the weight parameters, and the versions of
#' the spatial software stack. This prevents stale reuse when ACS estimates
#' change while an upstream identifier, row count, or geometry envelope stays
#' the same.
#'
#' @param iso_sf canonical isochrone `sf` POLYGON layer, the output of
#'   [cacs_isochrone()]. It must arrive in EPSG:4326 for schema validation,
#'   then is transformed internally to EPSG:5070 for area calculations.
#' @param acs_sf canonical ACS tract `sf` MULTIPOLYGON layer, the output of
#'   [cacs_acs_prefetch()]. It must arrive in EPSG:4269 (NAD83) for schema
#'   validation, then is transformed internally to EPSG:5070 for area
#'   calculations.
#' @param bg_pop_sf optional `sf` block-group population surface
#'   (any CRS, reprojected internally). Reserved for a future
#'   `weight_method = "population"` release.
#' @param weight_method one of `"area"` (default) or `"population"`. The
#'   `"population"` option is a planned future extension and currently stops
#'   with a clear error.
#' @param min_weight numeric dimensionless fraction in the half-open interval
#'   `[0, 1)`; default `1e-6`. Tract-level slivers whose area weight is at or
#'   below `min_weight` are dropped.
#' @param verbose logical(1); default `TRUE`. Controls the progress reporter,
#'   which advances once per `site_id` / `drive_time_min` pair as each
#'   completes (whether it succeeds or fails). The reporting mode
#'   auto-selects: silent when `verbose = FALSE`, when
#'   `Sys.getenv("CACS_QUIET") == "1"`, or when
#'   `options(catchmentACS.progress = "off")`; a brief start-and-summary
#'   message for fewer than 5 pairs; and a full `cli` progress bar with ETA
#'   and rate for 5 or more pairs. Force the bar at any size with
#'   `options(catchmentACS.progress = "force")`. See [cacs_run()] for the
#'   package-wide progress reporting policy.
#' @param keep_tract_audit logical(1); default `FALSE`. When `TRUE`, attach a
#'   `"cacs_tract_audit"` attribute: a tibble keyed by `site_id`,
#'   `drive_time_min`, and tract `GEOID` holding the area-weight ingredients
#'   used before variable-level aggregation.
#'
#' @return A long `tibble` of one row per `site_id`, `drive_time_min`, and
#'   ACS `variable`, carrying the canonical aggregation columns (`estimate`,
#'   `moe`, and supporting provenance). It also carries a
#'   `"cacs_aggregation_carriers"` attribute keyed by `site_id`,
#'   `drive_time_min`, and `variable` (see the Carrier provenance schema
#'   section below).
#'
#' @details
#' The function applies a five-stage policy: (1) schema validation of
#' `iso_sf` (EPSG:4326) and `acs_sf` (EPSG:4269); (2) defensive reprojection
#' to EPSG:5070 plus geometry repair; (3) per-(site, drive-time) intersection
#' with a spatial-index prefilter; (4) area-weight computation, where each
#' tract's weight is the fraction of its area falling inside the catchment,
#' `area(catchment intersect tract) / area(tract)`, followed by a sliver
#' filter at `min_weight`; and (5) per-estimand-family aggregation that builds
#' the carrier tibble used by downstream MOE propagation and rate derivation.
#'
#' Cache reads and writes are controlled by [cacs_set_cache()] and the legacy
#' `options(catchmentACS.cache_intersect = FALSE)` and
#' `CACS_CACHE_INTERSECT=0` switches. Older cache entries written without the
#' current content fingerprint are treated as a miss and recomputed.
#'
#' @section Estimand families and weights:
#' The primary area weight is a *coverage fraction*, not a universal
#' normalized weight:
#' \deqn{coverage\_wt_j = area(catchment \cap tract_j) / area(tract_j).}
#' `spatial_total` rows use this coverage weight to allocate tract counts into
#' catchment totals. `derived_rate` rows preserve the corresponding
#' coverage-weighted numerator and denominator totals as carrier values, then
#' [cacs_derive_rates()] computes the ratio and its MOE.
#'
#' Scalar and proxy estimands use a separate normalized mean weight:
#' \deqn{mean\_wt_j = area(catchment \cap tract_j) /
#'       \sum_k area(catchment \cap tract_k).}
#' `area_weighted_scalar_proxy` and `median_proxy` rows use this mean-weight
#' path, so their weights sum to one within a site and drive-time cell.
#' Legacy or defensive `area_weighted_rate_proxy` rows, if encountered, follow
#' the same normalized mean-weight proxy path; curated catchment rates are
#' produced as `derived_rate` rows from numerator and denominator carriers.
#' `population_weighted_scalar_proxy` is reserved for a future population
#' weighting release because `weight_method = "population"` currently fails
#' loud. `metadata_only` variables do not receive numeric aggregation.
#'
#' Area weighting assumes, for allocation purposes, that the ACS characteristic
#' is uniformly distributed within each tract. The function transforms validated
#' inputs to EPSG:5070 (NAD83 / CONUS Albers Equal Area) before measuring areas
#' so numerator and denominator areas are evaluated in the same equal-area
#' metric.
#'
#' @section Carrier provenance schema:
#' The returned long tibble carries a `"cacs_aggregation_carriers"`
#' attribute: an internal carrier tibble consumed by [cacs_propagate_moe()],
#' [cacs_derive_rates()], and [cacs_describe()]. It is keyed by `site_id`,
#' `drive_time_min`, and `variable`, and holds the per-variable aggregation
#' ingredients (the weighted totals and means and their raw variances, the
#' weight sum, and the contributing tract count) that those downstream
#' functions need. The output also carries a `"cacs_schema_version"`
#' attribute and a public `ring_topology` column.
#'
#' **Degenerate-tract policy.** When `acs_sf` carries one or more tracts with
#' non-positive or non-finite area (for example a Census water tract such as
#' `01003990000` in Baldwin Co., AL that slipped past the water-tract filter
#' in [cacs_acs_prefetch()]), the function emits a warning for each skipped
#' tract and continues with the healthy subset instead of stopping. The
#' skipped tract `GEOID`s are attached as a `"skipped_geoids"` attribute,
#' which is always present (an empty character vector when nothing was
#' skipped). The function still stops with an error when **every** tract is
#' degenerate, since there is then nothing to intersect; that error points
#' back to `cacs_acs_prefetch(drop_water_tracts = TRUE)`. See the Water
#' tracts section of [cacs_acs_prefetch()] for the primary upstream filter.
#'
#' @section Tract audit attribute:
#' Set `keep_tract_audit = TRUE` to attach
#' `attr(out, "cacs_tract_audit")`. The attribute has columns `site_id`,
#' `drive_time_min`, `GEOID`, `area_wt`, `int_area_m2`, and `tract_area_m2`.
#' It is useful when auditing which ACS tracts contributed to a catchment and
#' by how much. The default is `FALSE` so ordinary pipelines do not carry the
#' additional per-tract table in memory. Intersect cache entries may store this
#' internal audit table for future opt-in calls, but it is stripped from
#' returned objects unless explicitly requested.
#'
#' @seealso [cacs_run()] for the one-call pipeline and the package-wide
#'   progress reporting policy; [cacs_propagate_moe()] and
#'   [cacs_derive_rates()] for the downstream consumers of the carrier table.
#' @family core pipeline
#' @export
#' @examples
#' \donttest{
#' iso <- readRDS(system.file("extdata", "legacy_2025_isochrones.rds",
#'                            package = "catchmentACS"))
#' acs <- readRDS(system.file("extdata", "sample_alabama_subset.rds",
#'                            package = "catchmentACS"))
#' iso_subset <- iso[iso$site_id == "AL_SITE_01" &
#'                   iso$drive_time_min == 5, ]
#' agg <- cacs_intersect_weight(iso_sf = iso_subset, acs_sf = acs,
#'                              weight_method = "area", verbose = FALSE)
#' head(agg[, c("site_id", "drive_time_min", "variable", "estimate", "moe")])
#' }
cacs_intersect_weight <- function(iso_sf,
                                   acs_sf,
                                   bg_pop_sf     = NULL,
                                   weight_method = c("area", "population"),
                                   min_weight    = 1e-6,
                                   verbose       = TRUE,
                                   keep_tract_audit = FALSE) {

  weight_method <- match.arg(weight_method)

  # ============================================================================
  # Sec. 21.3 Decision #5: the current release supports `weight_method =
  # "area"` only; `"population"` remains a future extension.
  #
  # HOIST: fail-loud BEFORE any schema validation, EPSG:5070 transform, geometry
  # repair, or per-site loop so the user does not pay for Steps 1-10 work that
  # will be discarded. Condition class is `catchmentACS_error_credential`,
  # matching the current cli.R factory inventory.
  # ============================================================================

  if (identical(weight_method, "population")) {
    .cli_abort_credential(c(
      "{.field weight_method} = {.val population} is deferred to a future release.",
      "i" = "Use {.val area} in the current release.",
      "i" = "Tracking: {.fn .apply_pop_weight} stub will gain a body in a future release."
    ))
  }

  # ============================================================================
  # Sec. 21.3 PRE-LOOP Step 1 - schema validation triplet (fail-loud)
  # ============================================================================

  # 1a/b: iso + acs schemas (Phase 2.3 validators handle E-21-01..10)
  .validate_iso_schema(iso_sf)
  .validate_acs_schema(acs_sf)

  # 1c: bg_pop_sf branch is UNREACHABLE in v0.1 (population fail-loud above
  #     aborts first). Kept for v0.2 wire-in; harmless dead code under v0.1
  #     invariants (the `identical(weight_method, "population")` guard never
  #     evaluates TRUE here because of the hoisted abort).
  if (identical(weight_method, "population")) {                         # nocov start
    if (is.null(bg_pop_sf)) {
      .cli_abort_schema(c(
        "{.arg weight_method = 'population'} requires {.arg bg_pop_sf}.",
        "*" = "Either set {.arg weight_method = 'area'} or pass a block-group population surface.",
        "i" = "Expected columns: {.field GEOID}, {.field tract_geoid}, {.field population_est}, {.field geometry}."
      ))                                                                # E-21-11
    }
    .validate_bg_pop_schema(bg_pop_sf)                                  # E-21-12..14
  }                                                                     # nocov end

  # 1d: min_weight + verbose range/type guards
  .validate_min_weight(min_weight)                                      # E-21-21
  if (!is.logical(keep_tract_audit) || length(keep_tract_audit) != 1L ||
      is.na(keep_tract_audit)) {
    .cli_abort_schema(c(
      "{.arg keep_tract_audit} must be a non-NA logical scalar.",
      "x" = "Got {.cls {class(keep_tract_audit)[[1L]]}} of length {.val {length(keep_tract_audit)}}."
    ))
  }
  .validate_verbose(verbose)

  # 1e: CONUS+DC scope guard (defensive: 0.25-degree tolerance pad).
  iso_bbox <- sf::st_bbox(iso_sf)
  acs_bbox <- sf::st_bbox(acs_sf)
  bad_input <- character(0)
  if (.bbox_outside_conus_dc(iso_bbox)) bad_input <- c(bad_input, "iso_sf")
  if (.bbox_outside_conus_dc(acs_bbox)) bad_input <- c(bad_input, "acs_sf")
  if (length(bad_input) > 0L) {
    .cli_abort_operator(c(
      "Geographic scope outside v1.0 CONUS+DC.",
      "x" = "Out-of-scope input: {.field {bad_input}}.",
      "i" = "v1.0 supports CONUS+DC only. Use v1.1 {.code crs_area = 'auto'} when available."
    ))                                                                  # E-21-19
  }

  # 1f: cross-bbox sanity (Q-OQ4 cross-state hint, soft)
  if (.bbox_extends_beyond(iso_bbox, acs_bbox)) {
    .cli_warn_provenance(c(
      "Cross-state catchment detected; iso_sf bbox extends beyond acs_sf bbox.",
      "i" = "v1.0 proceeds with current acs_sf only; see Sec. 24 orchestrator."
    ))                                                                  # W-21-04
  }

  # ============================================================================
  # Sec. 21.3 POST-LOOP Step 15 (HOIST) - composite cache GET.
  #
  # Schema is validated, bboxes are CONUS-confirmed, and the (iso_sf, acs_sf,
  # bg_pop_sf, weight_method, min_weight) tuple is now safe to hash. We probe
  # the cache BEFORE the expensive EPSG:5070 transform + per-site loop so a
  # repeat call costs only the 14-dim SHA-256 + an .rds read.
  #
  # Cache key is the §21.8 composite (14-dim SHA-256 - see
  # `.cache_key_intersect()` below). Transitive invalidation: any change in
  # iso_sf cache_key, current acs_sf content hash, bg_pop hashes, weight
  # params, iso topology/res, or the sf/GEOS/PROJ/R stack invalidates this
  # entry automatically.
  # ============================================================================

  intersect_cache_key <- NULL
  if (isTRUE(.cacs_cache_intersect_enabled())) {
    intersect_cache_key <- .cache_key_intersect(
      iso_sf        = iso_sf,
      acs_sf        = acs_sf,
      bg_pop_sf     = bg_pop_sf,
      weight_method = weight_method,
      min_weight    = min_weight
    )
    cached <- suppressMessages(
      .cacs_cache_get(intersect_cache_key, "intersect")
    )
    if (!is.null(cached)) {
      if (isTRUE(keep_tract_audit) &&
          is.null(attr(cached, "cacs_tract_audit", exact = TRUE))) {
        cached <- NULL
      } else {
        if (!isTRUE(keep_tract_audit)) {
          attr(cached, "cacs_tract_audit") <- NULL
        }
        if (isTRUE(verbose)) {
          .cli_inform_cache(
            c("Cache hit: returning cached intersect-weight output (key {substr(intersect_cache_key, 1, 8)}...).",
              "i" = "Weight method = {.val {weight_method}}, min_weight = {.val {min_weight}}."),
            phase = "intersect"
          )
        }
        return(cached)
      }
    }
  }

  # ============================================================================
  # Sec. 21.3 PRE-LOOP Step 2 - s2 toggle (defensive: layered on.exit chain
  # ensures restore even if a later handler or a downstream abort fires).
  # ============================================================================

  prev_s2 <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(prev_s2)), add = TRUE, after = FALSE)
  suppressMessages(sf::sf_use_s2(FALSE))

  # Step 2 (cont.) - EPSG:5070 single-shot transform (corrupt-CRS safe).
  iso_5070    <- .safe_transform_5070(iso_sf,    "iso_sf")
  acs_5070    <- .safe_transform_5070(acs_sf,    "acs_sf")
  bg_pop_5070 <- if (identical(weight_method, "population")) {
    .safe_transform_5070(bg_pop_sf, "bg_pop_sf")
  } else {
    NULL
  }

  # ============================================================================
  # Sec. 21.3 PRE-LOOP Step 3 - geometry repair (3-step retry per input).
  # ============================================================================

  iso_5070 <- .repair_geometry(iso_5070)
  acs_5070 <- .repair_geometry(acs_5070)
  if (!is.null(bg_pop_5070)) bg_pop_5070 <- .repair_geometry(bg_pop_5070)

  # ============================================================================
  # Sec. 21.3 PRE-LOOP Step 4 - pre-compute tract areas in m^2.
  #
  # MULTIPOLYGON note: sf::st_area() returns one value per feature regardless of
  # geometry type. For MULTIPOLYGON it is the SUM of component polygon areas,
  # which is exactly what we want as the tract denominator.
  # ============================================================================

  acs_5070$tract_area_m2 <- as.numeric(sf::st_area(acs_5070))
  stopifnot(
    "Internal: tract_area_m2 length must match nrow(acs_5070)" =
      length(acs_5070$tract_area_m2) == nrow(acs_5070)
  )

  # v0.2 BUG-001 (F3) Layer (b): demote the v0.1 fail-loud abort to
  # warn-and-skip on degenerate tracts. Layer (a) (.drop_water_tracts() in
  # cacs_acs_prefetch()) handles the common Census water-tract case before
  # we ever get here; this layer is the defense-in-depth catch for
  # user-supplied acs_sf that bypassed the prefetch filter. Fail-loud is
  # preserved when ALL tracts are degenerate (truly nothing to intersect).
  skipped_geoids_acc <- character(0)
  degen_mask <- !is.finite(acs_5070$tract_area_m2) | acs_5070$tract_area_m2 <= 0
  if (any(degen_mask)) {
    degen_geoids <- acs_5070$GEOID[degen_mask]
    if (all(degen_mask)) {
      .cli_abort_geometry(c(
        "All tract(s) have non-positive or non-finite area - cannot intersect.",
        "x" = "Affected GEOIDs (first 5): {.val {utils::head(degen_geoids, 5L)}}.",
        "i" = "Check {.fn cacs_acs_prefetch} output or pass {.arg drop_water_tracts = TRUE}."
      ))                                                                # E-21-16
    }
    # Partial degeneracy: warn, accumulate for the output attribute, and
    # continue with the healthy subset.
    .warn_skip_water_tract(degen_geoids)
    skipped_geoids_acc <- c(skipped_geoids_acc, degen_geoids)
    acs_5070 <- acs_5070[!degen_mask, , drop = FALSE]
  }

  # ============================================================================
  # Pre-loop summary (verbose).
  # ============================================================================

  if (isTRUE(verbose)) {
    n_sites_unique <- length(unique(iso_5070$site_id))
    n_dt_unique    <- length(unique(iso_5070$drive_time_min))
    n_tracts       <- nrow(acs_5070)
    .cli_inform_progress(c(
      "Pre-loop ready: {n_sites_unique} site(s), {n_dt_unique} drive_time(s), {n_tracts} ACS tract(s) on EPSG:5070.",
      "i" = "Per-site loop (Step 5.2) will iterate {n_sites_unique * n_dt_unique} (site, drive_time) pair(s)."
    ))
  }

  # ============================================================================
  # Sec. 21.3 PER-SITE LOOP Step 5 - site iteration index (deterministic order).
  #
  # Deterministic ordering invariant: arrange(site_id,
  # drive_time_min) BEFORE iteration matters for §1.7 epsilon-determinism and
  # for stable bind_rows() output regardless of upstream row order. distinct()
  # is defensive against duplicate (site_id, drive_time_min) rows that may
  # escape Phase 3 normalization.
  # ============================================================================

  pair_index <- iso_5070 |>
    sf::st_drop_geometry() |>
    dplyr::distinct(.data$site_id, .data$drive_time_min) |>
    dplyr::arrange(.data$site_id, .data$drive_time_min)

  n_iter <- nrow(pair_index)
  if (n_iter == 0L) {
    .cli_abort_operator(c(
      "{.arg iso_sf} has zero unique (site_id, drive_time_min) pair(s).",
      "i" = "Check that {.fn cacs_isochrone} produced a non-empty batch."
    ))                                                                  # E-21-17
  }

  # ============================================================================
  # Sec. 21.3 PER-SITE LOOP Step 6 - per-site dispatch via map + tryCatch.
  #
  # Per-site error isolation (T21-15): a single site's geometry / intersection
  # failure must NOT abort the batch. The per-element tryCatch() wrapper
  # converts both `catchmentACS_error_*` (our cli abort family) and plain
  # `error` conditions to `.empty_site_result()` rows so the result list shape
  # stays uniform.
  #
  # NOTE on iteration primitive: §21.3 step 6 pseudocode literally writes
  # `purrr::map(seq_len(n_iter), function(i) { tryCatch(...) })`. We use base
  # R's `lapply()` which has byte-identical sequential semantics, avoiding a
  # new dependency on `purrr` (§14.1 12-Imports lock). `lapply()` is the
  # synonym for `purrr::map()` when no other purrr feature (progress,
  # type-strict variants, futures) is needed.
  #
  # `bag_per_site` is implicit: lapply()/map() returns the list directly. The
  # subsequent dplyr::bind_rows() yields the intermediate per-tract long
  # tibble that Step 5.3 will pivot into family dispatch.
  # ============================================================================

  # v0.2 F1: per-(site, drive_time) progress reporter. Wraps the lapply()
  # boundary so `prog$tick()` lands after each pair completes (success or
  # tryCatch-converted failure). The `on.exit(finish)` guard ensures the
  # standardized summary lands on mid-loop abort. Failure tally accumulates
  # via tryCatch-side bumping of `n_failed_iw_state$n_failed`.
  iw_prog <- .cacs_progress_reporter(n = n_iter, label = "Intersect+weight",
                                     verbose = verbose)
  n_failed_iw_state <- new.env(parent = emptyenv())
  n_failed_iw_state$n_success <- 0L
  n_failed_iw_state$n_failed  <- 0L
  on.exit(
    iw_prog$finish(
      n_success = n_failed_iw_state$n_success,
      n_failed  = n_failed_iw_state$n_failed
    ),
    add = TRUE
  )

  bag_per_site <- lapply(seq_len(n_iter), function(i) {
    sid <- pair_index$site_id[[i]]
    dt  <- pair_index$drive_time_min[[i]]
    res <- tryCatch(
      .intersect_one_site(
        sid        = sid,
        dt         = dt,
        iso_5070   = iso_5070,
        acs_5070   = acs_5070,
        min_weight = min_weight
      ),
      catchmentACS_error = function(e) {
        n_failed_iw_state$n_failed <- n_failed_iw_state$n_failed + 1L
        .empty_site_result(
          site_id        = sid,
          drive_time_min = dt,
          failure_reason = conditionMessage(e)
        )
      },
      error = function(e) {
        n_failed_iw_state$n_failed <- n_failed_iw_state$n_failed + 1L
        .empty_site_result(
          site_id        = sid,
          drive_time_min = dt,
          failure_reason = conditionMessage(e)
        )
      }
    )
    # Tick AFTER the pair completes so abort still reports partial via on.exit.
    iw_prog$tick(detail = paste0(sid, "/", dt, "min"))
    res
  })
  # Successful pairs = total - failures (failures bumped in the tryCatch
  # handlers above; not all empty rows are failures, e.g. a pair with zero
  # intersecting tracts but no exception still counts as success).
  n_failed_iw_state$n_success <- as.integer(n_iter -
                                            n_failed_iw_state$n_failed)

  # ----------------------------------------------------------------------------
  # INTERMEDIATE return marker (Step 5.2 boundary).
  #
  # bind_rows() yields the intermediate per-tract long tibble. Each row is
  # one (site_id, drive_time_min, GEOID) intersection slice carrying area_wt,
  # int_area_m2, and the original ACS attributes (variable, estimate, moe).
  #
  # Empty-site failures carry through as `.empty_site_result()` rows (NA in
  # most columns, n_tracts = 0, failure_reason populated). bind_rows() pads
  # missing columns with NA - this is intentional and consumed by Step 5.3
  # family dispatch which group_by(variable) drops NA-variable rows from the
  # aggregation but preserves them in the public output.
  # ----------------------------------------------------------------------------

  intersect_all <- dplyr::bind_rows(bag_per_site)
  tract_audit_tbl <- if (isTRUE(keep_tract_audit)) {
    .collect_tract_audit(intersect_all)
  } else {
    NULL
  }

  # ============================================================================
  # Sec. 21.3 FAMILY DISPATCH Step 11-13 (Phase 5.3 CORE).
  #
  # Algorithm step 11: family-specific weight columns (coverage_wt + mean_wt).
  # Algorithm step 12: per-variable aggregation with universal carriers.
  # Algorithm step 13: 6-family case_when dispatch of estimate / moe /
  #                    weight_basis + min_weight variable-level filter.
  #
  # Architecture: single dplyr mutate(case_when) chain (Blueprint frozen
  # contract). Returns
  # list(public, carriers, empty_carry) for Step 5.4 to bind, attach the
  # carrier attribute, and emit the canonical long output.
  # ============================================================================

  family_out <- .compute_family_aggregation(                            # nolint: object_usage_linter
    intersect_all = intersect_all,
    weight_method = weight_method,
    min_weight    = min_weight
  )

  # ============================================================================
  # Sec. 21.3 POST-LOOP Step 14 - bind public rows + carry-through provenance.
  #
  # Inputs from Step 5.3:
  #   - family_out$public      : per-(site, drive_time, variable) success rows
  #                              (12 cols incl. failure_origin = "none")
  #   - family_out$carriers    : 9-col batch-level keyed tibble for §22/§23
  #   - family_out$empty_carry : 1-row-per-failed-site tibble from
  #                              .empty_site_result() (15 cols incl.
  #                              failure_origin = "intersection")
  #
  # Bind strategy: dplyr::bind_rows() pads missing columns with NA so the two
  # tibbles' divergent schemas (success rows have estimand_family populated;
  # empty rows leave it NA) merge cleanly. The post-bind mutate fills the
  # remaining 8 canonical columns (provider/profile/osm_snapshot_date +
  # acs_year + weight_method + moe_formula_requested + moe_fallback +
  # moe_fallback_reason) at row-level.
  # ============================================================================

  # Sanitize empty_carry before bind_rows. The per-site loop sends two row
  # shapes through here:
  #   (a) success sites -> sf rows from .intersect_one_site() (per-tract
  #       intersect with geometry + per-tract diagnostic columns).
  #   (b) failed sites  -> 1-row plain tibble from .empty_site_result()
  #       (Step 5.2 utils.R, 15 cols including `failure_reason`).
  #
  # `.compute_family_aggregation()` already routes (a) into `family_out$public`
  # (per-variable aggregated). What lands in `family_out$empty_carry` is the
  # subset of `intersect_all` where variable is NA - in practice that is only
  # the empty-site rows. But when ALL sites succeed, the partition yields a
  # 0-row sf carrier that vctrs cannot unify with the plain `public` tbl_df.
  #
  # We:
  #   1. strip sf class + geometry
  #   2. drop columns that overlap with what left_join() will supply or what
  #      the explicit mutate fills, so the join produces no `.x`/`.y` suffix
  #      conflicts
  #   3. keep `failure_reason` because some test paths read it pre-NULL'ing.
  upstream_supplied <- c("provider", "profile", "osm_snapshot_date",
                         "ring_topology")
  mutate_supplied   <- c(
    "acs_year", "weight_method",
    "moe_formula_requested", "moe_fallback", "moe_fallback_reason"
  )

  empty_norm <- family_out$empty_carry
  if (inherits(empty_norm, "sf")) {
    empty_norm <- sf::st_drop_geometry(empty_norm)
  }
  drop_from_empty <- intersect(
    names(empty_norm),
    c(upstream_supplied, mutate_supplied,
      # per-tract diagnostic cols that must not bleed into 20-col schema
      "GEOID", "NAME", "tract_area_m2", "area_wt", "int_area_m2",
      "routing_engine_version", "polygon_simplification_tolerance",
      "generated_at", "isochrone_empty", "provider_requested",
      "provider_downgrade", "osm_snapshot_status", "retry_count")
  )
  empty_norm <- empty_norm[, setdiff(names(empty_norm), drop_from_empty),
                           drop = FALSE]
  empty_norm <- tibble::as_tibble(empty_norm)

  out <- dplyr::bind_rows(family_out$public, empty_norm)

  # Step 14b: row-level provider/profile/osm_snapshot_date carry-through
  # (left_join on (site_id, drive_time_min) since these are isochrone-side
  # provenance and identical across all variables for a given (site, drive_time)
  # pair). iso_provenance is the 5-col sf carrier defined in Step 5.1 §21.3
  # step 6. Drop geometry before join to keep `out` a plain tbl_df.
  iso_provenance <- iso_5070 |>
    sf::st_drop_geometry() |>
    dplyr::distinct(
      .data$site_id, .data$drive_time_min,
      .data$provider, .data$profile, .data$osm_snapshot_date,
      .data$ring_topology
    )

  out <- out |>
    dplyr::left_join(iso_provenance, by = c("site_id", "drive_time_min"))

  # Step 14c-j: fill the remaining canonical columns at row level
  #
  # `acs_year` from acs_sf provenance attribute (Phase 4 carry-through).
  # The authoritative source for acs_year is attr(acs_sf, "cacs_provenance")$year;
  # fixtures without provenance fall back to NA_integer_.
  acs_year_resolved <- .extract_acs_year(acs_sf)

  # moe_formula_requested: family -> sanctioned formula label (Sec. 22.2).
  # Reuses the same dispatch helper that seeds moe_formula_effective in Step
  # 5.3; on entry to §22 the two are byte-identical (§22 may revise effective
  # on fallback while leaving requested intact for audit).
  out$moe_formula_requested <- .dispatch_moe_formula_effective(
    out$estimand_family
  )

  # Fill remaining row-level provenance columns. The vectorized assignments
  # below preserve row order from bind_rows().
  out$acs_year                      <- acs_year_resolved
  out$weight_method                 <- weight_method
  out$moe_fallback                  <- FALSE                # v0.1; §22 may revise
  out$moe_fallback_reason           <- "n/a"                # v0.1; §22 may revise
  out$weight_uncertainty_propagated <- FALSE                # v0.1 lock

  # Backfill: empty-site rows from .empty_site_result() arrive with
  # moe_formula_effective = NA. Mirror moe_formula_requested into effective for
  # those rows so the 20-col validator's non-NA-effective expectation (carried
  # over by §22) is preserved. moe_fallback stays NA for empty rows because
  # there was nothing to fall back from.
  na_eff <- is.na(out$moe_formula_effective)
  if (any(na_eff)) {
    out$moe_formula_effective[na_eff] <- out$moe_formula_requested[na_eff]
    out$moe_fallback[na_eff]          <- NA
  }

  # failure_origin: success rows already carry "none" from Step 5.3; empty
  # rows already carry "intersection" from .empty_site_result(). Defensive:
  # any unexpected NA (e.g., from a future bind_rows pad) becomes "none".
  out$failure_origin <- ifelse(is.na(out$failure_origin),
                               "none",
                               out$failure_origin)

  # Carrier tibble (Phase 6.2 / Phase 6.4 lookup contract per §22.3 step 6).
  carrier_tbl <- family_out$carriers

  # Drop the .empty_site_result()-specific `failure_reason` column (not part of
  # the §21.7 22-col canonical schema; the upstream failure cause is captured
  # in the verbose log / `failure_origin` enum).
  out$failure_reason <- NULL

  # v0.2 F4: source rows (per-ACS-variable + empty-site rows) carry NA for
  # the bivariate `n_tracts_num` / `n_tracts_den` columns. Only derived rate
  # rows from `cacs_derive_rates()` populate these from the carrier lookup;
  # bivariate-transparency rationale: only derived-rate rows populate these.
  out$n_tracts_num <- NA_integer_
  out$n_tracts_den <- NA_integer_

  # Reorder columns to §21.7 canonical sequence so str(out) / dput() are
  # diff-stable across runs and across success / empty row mixes.
  out <- out[, .LONG_REQUIRED_COLS]

  # ============================================================================
  # Sec. 21.3 POST-LOOP Step 15 - validate output + attach 3 attributes +
  # composite cache key + cache put + return.
  #
  # Reviewer 3-axis verification (Decision #4) - inline assertions:
  #   1. `cacs_aggregation_carriers` survives the bind_rows() above because
  #      we attach it AFTER the bind (attribute attach order matters).
  #   2. Composite cache key transitive invalidation: any of the 14 payload
  #      dimensions changing yields a different SHA-256 (see
  #      .cache_key_intersect()).
  #   3. §22 lookup contract: carrier has byte-identical column schema
  #      (`site_id, drive_time_min, variable, estimand_family, est_total,
  #      var_total_raw, est_mean, var_mean_raw, weight_sum`) for the
  #      .lookup_carrier_num/_den/_var() helpers (Phase 6.2). Carrier key
  #      uniqueness is asserted by `.validate_intersect_output()`.
  # ============================================================================

  attr(out, "cacs_aggregation_carriers") <- carrier_tbl
  if (isTRUE(keep_tract_audit)) {
    attr(out, "cacs_tract_audit") <- tract_audit_tbl
  }

  # v0.2 BUG-001 (F3) Layer (b): expose any tracts that were skipped due
  # to degenerate geometry so callers can audit / log them. Always
  # attached (empty character(0) when nothing was skipped) so downstream
  # code can use `attr(out, "skipped_geoids")` without an existence check.
  attr(out, "skipped_geoids") <- skipped_geoids_acc

  # Provenance attribute (§21.7) - 12-field list for audit trail.
  attr(out, "cacs_aggregation_provenance") <- list(
    n_sites_input        = nrow(pair_index),
    n_sites_with_data    = sum(out$n_tracts > 0L, na.rm = TRUE),
    n_sites_empty        = sum(out$n_tracts == 0L, na.rm = TRUE),
    weight_method        = weight_method,
    min_weight           = min_weight,
    generated_at         = as.POSIXct(Sys.time(), tz = "UTC"),
    sf_ver               = as.character(utils::packageVersion("sf")),
    geos_ver             = as.character(sf::sf_extSoftVersion()[["GEOS"]]),
    proj_ver             = as.character(sf::sf_extSoftVersion()[["PROJ"]]),
    cacs_ver             = as.character(utils::packageVersion("catchmentACS")),
	    acs_year             = acs_year_resolved,
	    n_variables_unique   = length(unique(stats::na.omit(out$variable))),
	    n_input_rows_missing_acs_estimate =
	      family_out$n_input_rows_missing_acs_estimate %||% 0L,
	    n_output_groups_missing_acs_estimate =
	      family_out$n_output_groups_missing_acs_estimate %||% 0L
	  )

  attr(out, "cacs_schema_version") <- "1.0"

  # Validate the canonical long output (Phase 2.3 checks.R).
  # Sec. 12.3.1 20-col schema + carrier key uniqueness + carrier numeric
  # contracts.
  .validate_intersect_output(out, abort = TRUE,
                             require_symmetric_keys = TRUE)

  # Composite cache put (Phase 2.2 cache.R).
  # Reuse the pre-loop cache key when the cache GET path computed one. The
  # fallback guards unusual cases where cache state changes during execution.
  # On any cache write failure, .cacs_cache_put() emits W-RUNTIME and returns
  # FALSE without blocking the return path.
  if (isTRUE(.cacs_cache_intersect_enabled())) {
    if (is.null(intersect_cache_key)) {
      intersect_cache_key <- .cache_key_intersect(
        iso_sf        = iso_sf,
        acs_sf        = acs_sf,
        bg_pop_sf     = bg_pop_sf,
        weight_method = weight_method,
        min_weight    = min_weight
      )
    }
    cache_out <- out
    if (isTRUE(keep_tract_audit) && !is.null(tract_audit_tbl)) {
      attr(cache_out, "cacs_tract_audit") <- tract_audit_tbl
    }
    .cacs_cache_put(cache_out, intersect_cache_key, "intersect")
    if (isTRUE(verbose)) {
      .cli_inform_cache(
        c("Wrote intersect cache entry (key {substr(intersect_cache_key, 1, 8)}...).",
          "i" = "Namespace: {.val intersect}; weight_method = {.val {weight_method}}, min_weight = {.val {min_weight}}."),
        phase = "intersect"
      )
    }
  }

  out
}


# ============================================================================
# Internal: population weight stub (future-release placeholder)
# ============================================================================

#' Apply population weight (future-release stub - fail-loud abort)
#'
#' The current release supports `weight_method = "area"` only. This helper is the
#' future-release wire-in point for the Sec. 21.6.3 dasymetric population
#' weight computation. Current-release calls reach this only if the caller
#' bypasses the public-entry fail-loud guard (e.g. via direct `:::` access).
#' Body aborts with the same `catchmentACS_error_credential` class so the
#' future transition is signature-only until behavior is intentionally enabled.
#'
#' @param inter_one per-tract slice tibble for one (site_id, drive_time_min)
#'   pair (the output of `.intersect_one_site()` from Step 5.2).
#' @param bg_pop_5070 EPSG:5070 block-group population sf (Step 5.1 prepared).
#'
#' @return Future release: list with `coverage_wt`, `mean_wt` numeric vectors
#'   of length `nrow(inter_one)`. Current release: never returns -- aborts
#'   fail-loud.
#'
#' @keywords internal
#' @noRd
.apply_pop_weight <- function(inter_one, bg_pop_5070) {
  .cli_abort_credential(c(
    "{.fn .apply_pop_weight} is deferred to a future release.",
    "i" = "The current release supports {.val area} weighting only."
  ))
}


# ============================================================================
# Internal: family dispatch + carrier construction (Phase 5.3 CORE)
# ============================================================================

#' Compute per-variable family aggregation with carrier split (internal)
#'
#' Implements Sec. 21.3 algorithm steps 11-13 on the per-tract long tibble
#' produced by Step 5.2's `.intersect_one_site()` + `bind_rows()`.
#'
#' Returns a `list` with three named tibbles:
#'   - `public`      : per-variable aggregated public rows (pre-Step 5.4
#'                     provenance attach).
#'   - `carriers`    : batch-level keyed tibble for Sec. 22/23 lookup
#'                     (10 cols: 4 keys + 4 aggregation carriers + weight_sum
#'                     + n_tracts; v0.2 schema growth for F4 n_tracts_num/den).
#'   - `empty_carry` : per-(site, drive_time) carry-through rows for
#'                     empty / failed sites, untouched from `.empty_site_result()`.
#'
#' Architecture: single dplyr
#' `mutate()` with three `dplyr::case_when()` blocks (estimate / moe /
#' weight_basis) dispatching across all 6 estimand families. Each Blueprint
#' Sec. 21.5 dispatch table row maps line-for-line to one case_when branch.
#'
#' @param intersect_all `tbl_df` from Step 5.2 with one row per
#'   (site_id, drive_time_min, GEOID) intersection slice. Empty-site rows
#'   (variable = NA, n_tracts = 0) pass through untouched in `empty_carry`.
#' @param weight_method validated `"area"` (v0.1; `"population"` already aborted
#'   at public entry).
#' @param min_weight dimensionless sliver threshold (Sec. 21.6.4 variable level).
#'
#' @keywords internal
#' @noRd
.sum_weighted_estimate <- function(w, x) {
  if (anyNA(x)) return(NA_real_)
  sum(w * x)
}


#' @keywords internal
#' @noRd
.sum_weighted_variance <- function(w, estimate, moe) {
  if (anyNA(estimate) || anyNA(moe)) return(NA_real_)
  sum((w * moe / .Z_ACS_90)^2)
}


#' @keywords internal
#' @noRd
.compute_family_aggregation <- function(intersect_all,
                                        weight_method = "area",
                                        min_weight    = 1e-6) {

  # ---- Partition: success rows (variable populated) vs empty-site rows ----
  # Empty-site rows come from `.empty_site_result()` with variable = NA. They
  # carry through to the public output untouched and are excluded from the
  # carrier tibble (no per-variable aggregation to compute).
  is_empty    <- is.na(intersect_all$variable)
  empty_carry <- intersect_all[is_empty, , drop = FALSE]
  success     <- intersect_all[!is_empty, , drop = FALSE]

  # Drop sticky sf geometry list-column if present (bind_rows of mixed
  # sf + tbl_df preserves a sticky geometry column we no longer need).
  if (inherits(success, "sf")) {
    success <- sf::st_drop_geometry(success)
  }

  # Defensive: if every row is empty, short-circuit with empty carrier schema
  # so Step 5.4's bind + attribute attach still finds a 9-col tibble.
  if (nrow(success) == 0L) {
    return(list(
      public      = .empty_family_public(),
      carriers    = .empty_family_carriers(),
      empty_carry = empty_carry,
      n_input_rows_missing_acs_estimate = 0L,
      n_output_groups_missing_acs_estimate = 0L
    ))
  }

  # ---- Step 11: family-specific weight columns (area branch only in v0.1) --
  #
  # `area_wt` (from Step 5.2) is the universal first product
  # |I_s ∩ T_j| / |T_j|. For Family A spatial_total we use it directly as
  # `coverage_wt`. For Family B scalar/median proxies we use the normalized
  # `mean_wt = int_area_m2 / sum(int_area_m2)` normalized within each
  # (site, drive_time, VARIABLE) cell, so the mean weights sum to one per
  # variable. `success` is long (one row per site x drive_time x GEOID x
  # variable), so the normalizing denominator MUST include `variable`: grouping
  # only by (site, drive_time) sums each tract's intersection area once per
  # variable and deflates every scalar/median proxy estimate by n_variables.
  # `coverage_wt` is the per-row area fraction |I_s n T_j| / |T_j| and is
  # unaffected by the grain.
  #
  # population branch unreachable in v0.1 (fail-loud at public entry).

  success <- success |>
    dplyr::group_by(.data$site_id, .data$drive_time_min, .data$variable) |>
    dplyr::mutate(
      coverage_wt = .data$area_wt,
      mean_wt     = .data$int_area_m2 /
                      sum(.data$int_area_m2, na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  weight_basis_total  <- "coverage"
  weight_basis_scalar <- if (identical(weight_method, "area")) {
    "area_mean"
  } else {
    "population_mean"          # nocov; v0.2 only (population fail-loud upstream)
  }

  # ---- Step 12: per-variable aggregation with universal carriers ----------
  #
  # `.classify_acs_variable_batch()` returns a character vector aligned with
  # `unique(success$variable)`; we expand via named-vector lookup so the
  # group_by includes a row-level `estimand_family` column.

  uniq_vars     <- unique(success$variable)
  family_map    <- .classify_acs_variable_batch(uniq_vars)
  family_lookup <- stats::setNames(family_map, uniq_vars)
  success$estimand_family <- unname(family_lookup[success$variable])

  agg <- success |>
    dplyr::group_by(
      .data$site_id, .data$drive_time_min,
      .data$variable, .data$estimand_family
    ) |>
    dplyr::summarise(
      # universal weight diagnostics (Sec. 21.7 long-output column 7)
      weight_sum    = sum(.data$coverage_wt, na.rm = TRUE),
      n_tracts      = dplyr::n(),
      n_missing_estimate = sum(is.na(.data$estimate)),

      # Family A carriers (spatial_total + derived_rate numerator/denominator)
      est_total     = .sum_weighted_estimate(.data$coverage_wt,
                                             .data$estimate),
      var_total_raw = .sum_weighted_variance(.data$coverage_wt,
                                             .data$estimate, .data$moe),

      # Family B carriers (*_scalar_proxy + median_proxy)
      est_mean      = .sum_weighted_estimate(.data$mean_wt,
                                             .data$estimate),
      var_mean_raw  = .sum_weighted_variance(.data$mean_wt,
                                             .data$estimate, .data$moe),

      .groups       = "drop"
    )

  # ---- Step 13: 6-family case_when dispatch (estimate / moe / weight_basis)
  #
  # One mutate, three case_when's. Each Sec. 21.5 dispatch table row maps
  # line-for-line to one case_when branch. derived_rate emits NA for estimate
  # AND moe (Phase 6.4 cacs_derive_rates() reads carriers via Sec. 22 lookup).
  # population_weighted_scalar_proxy is unreachable in v0.1 (weight_method
  # fail-loud) but the branch is FROZEN now so v0.2 wire-in is signature-only.
  # area_weighted_rate_proxy is in `.ESTIMAND_FAMILIES` (aaa-globals.R) but NOT
  # in the Sec. 21.5 dispatch table — flagged as Blueprint drift v1.0.2 patch
  # candidate; treated here as a scalar-mean variant (defensive coverage so an
  # accidentally-classified row does not silently fall through to NA).
  #
  # min_weight variable-level filter (Sec. 21.6.4): drop variable rows where
  # weight_sum < min_weight EXCEPT derived_rate (small num/den can still
  # produce a meaningful rate; Sec. 23 handles the zero-denominator branch).

  agg <- agg |>
    dplyr::mutate(
      estimate = dplyr::case_when(
        .data$estimand_family == "spatial_total"                    ~ .data$est_total,
        .data$estimand_family == "area_weighted_scalar_proxy"       ~ .data$est_mean,
        .data$estimand_family == "population_weighted_scalar_proxy" ~ .data$est_mean,
        .data$estimand_family == "area_weighted_rate_proxy"         ~ .data$est_mean,
        .data$estimand_family == "median_proxy"                     ~ .data$est_mean,
        .data$estimand_family == "derived_rate"                     ~ NA_real_,
        .data$estimand_family == "metadata_only"                    ~ NA_real_,
        TRUE                                                        ~ NA_real_
      ),
      moe = dplyr::case_when(
        .data$estimand_family == "spatial_total"                    ~ .Z_ACS_90 * sqrt(.data$var_total_raw),
        .data$estimand_family %in% c("area_weighted_scalar_proxy",
                                     "population_weighted_scalar_proxy",
                                     "area_weighted_rate_proxy",
                                     "median_proxy")                ~ .Z_ACS_90 * sqrt(.data$var_mean_raw),
        .data$estimand_family == "derived_rate"                     ~ NA_real_,
        .data$estimand_family == "metadata_only"                    ~ NA_real_,
        TRUE                                                        ~ NA_real_
      ),
      weight_basis = dplyr::case_when(
        .data$estimand_family == "spatial_total"                    ~ weight_basis_total,
        .data$estimand_family %in% c("area_weighted_scalar_proxy",
                                     "area_weighted_rate_proxy",
                                     "median_proxy")                ~ weight_basis_scalar,
        .data$estimand_family == "population_weighted_scalar_proxy" ~ "population_mean",
        .data$estimand_family == "derived_rate"                     ~ weight_basis_total,
        .data$estimand_family == "metadata_only"                    ~ "none",
        TRUE                                                        ~ "none"
      ),
      moe_formula_effective = .dispatch_moe_formula_effective(
        .data$estimand_family
      ),
      failure_origin                = "none",
      weight_uncertainty_propagated = FALSE   # v0.1 lock
    ) |>
    dplyr::filter(
      .data$weight_sum >= min_weight | .data$estimand_family == "derived_rate"
    )

  # ---- Carrier split: 9-col carrier tibble (Sec. 22/23 internal lookup) vs
  # public-row subset for Step 5.4 to augment with row-level provenance at
  # post-loop. Storage as per-variable `est_total` allows Phase 6.2 to project:
  #   - num_est ← est_total[variable == num_var_code]
  #   - den_est ← est_total[variable == den_var_code]
  #   - num_var ← var_total_raw[variable == num_var_code]
  #   - den_var ← var_total_raw[variable == den_var_code]
  # Byte-identical with Sec. 22.3 step 6 contract.
  # ------------------------------------------------------------------------

  carrier_rows <- agg |>
    dplyr::select(
      "site_id", "drive_time_min", "variable", "estimand_family",
      "est_total", "var_total_raw", "est_mean", "var_mean_raw",
      "weight_sum", "n_tracts"
    )

  public_rows <- agg |>
    dplyr::select(
      "site_id", "drive_time_min", "variable", "estimate", "moe",
      "weight_sum", "n_tracts", "estimand_family", "weight_basis",
      "moe_formula_effective", "failure_origin",
      "weight_uncertainty_propagated"
    )

  list(
    public      = public_rows,
    carriers    = carrier_rows,
    empty_carry = empty_carry,
    n_input_rows_missing_acs_estimate = as.integer(sum(is.na(success$estimate))),
    n_output_groups_missing_acs_estimate = as.integer(sum(agg$n_missing_estimate > 0L))
  )
}


# ---- Helpers used by .compute_family_aggregation() -------------------------

#' Empty per-variable public-row template (internal)
#' @keywords internal
#' @noRd
.empty_family_public <- function() {
  tibble::tibble(
    site_id                       = character(0),
    drive_time_min                = integer(0),
    variable                      = character(0),
    estimate                      = numeric(0),
    moe                           = numeric(0),
    weight_sum                    = numeric(0),
    n_tracts                      = integer(0),
    estimand_family               = character(0),
    weight_basis                  = character(0),
    moe_formula_effective         = character(0),
    failure_origin                = character(0),
    weight_uncertainty_propagated = logical(0)
  )
}

#' Empty 10-col carrier template (internal)
#' @keywords internal
#' @noRd
.empty_family_carriers <- function() {
  tibble::tibble(
    site_id         = character(0),
    drive_time_min  = integer(0),
    variable        = character(0),
    estimand_family = character(0),
    est_total       = numeric(0),
    var_total_raw   = numeric(0),
    est_mean        = numeric(0),
    var_mean_raw    = numeric(0),
    weight_sum      = numeric(0),
    n_tracts        = integer(0)
  )
}

#' Dispatch `moe_formula_effective` from estimand_family (internal)
#'
#' Family → Sec. 22.2 sanctioned formula. Used by Step 5.3 to seed the
#' `moe_formula_effective` column (Sec. 22 will revise on fallback). Step 5.4
#' will produce the row-level `moe_formula_requested` via the same map.
#'
#' @keywords internal
#' @noRd
.dispatch_moe_formula_effective <- function(estimand_family) {
  dplyr::case_when(
    estimand_family == "spatial_total"                    ~ "weighted_sum",
    estimand_family == "area_weighted_scalar_proxy"       ~ "weighted_mean",
    estimand_family == "population_weighted_scalar_proxy" ~ "weighted_mean",
    estimand_family == "area_weighted_rate_proxy"         ~ "weighted_mean",
    estimand_family == "median_proxy"                     ~ "weighted_mean",
    estimand_family == "derived_rate"                     ~ "general_ratio_conservative",
    estimand_family == "metadata_only"                    ~ NA_character_,
    TRUE                                                  ~ NA_character_
  )
}


# ============================================================================
# Internal helpers - PRE-LOOP support
# ============================================================================

#' Validate min_weight scalar (E-21-21)
#' @keywords internal
#' @noRd
.validate_min_weight <- function(min_weight) {
  if (!is.numeric(min_weight) ||
      length(min_weight) != 1L ||
      is.na(min_weight) ||
      !is.finite(min_weight) ||
      min_weight < 0 ||
      min_weight >= 1) {
    .cli_abort_schema(c(
      "{.arg min_weight} must be a finite numeric scalar in {.val [0, 1)}.",
      "x" = "Got value {.val {min_weight}}."
    ))                                                                  # E-21-21
  }
  invisible(TRUE)
}


#' Validate verbose flag
#' @keywords internal
#' @noRd
.validate_verbose <- function(verbose) {
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    .cli_abort_schema(c(
      "{.arg verbose} must be a non-missing length-1 logical."
    ))
  }
  invisible(TRUE)
}


#' Safe EPSG:5070 reprojection (Sec. 21.4)
#'
#' Wraps `sf::st_transform(x, 5070)` in `tryCatch()` so corrupt CRS metadata
#' surfaces as a classed schema abort with the offending argument name.
#'
#' @keywords internal
#' @noRd
.safe_transform_5070 <- function(x, arg_name) {
  tryCatch(
    sf::st_transform(x, 5070),
    error = function(e) {
      .cli_abort_schema(
        c(
          "Failed to reproject {.arg {arg_name}} to EPSG:5070.",
          "x" = "PROJ rejected the transform.",
          "i" = "Check the input's CRS with {.code sf::st_crs(...)}; original error: {conditionMessage(e)}."
        ),
        parent = e
      )
    }
  )
}


#' CONUS+DC bbox guard with small boundary tolerance
#'
#' v1.0 supports CONUS + DC only. The reference bbox is roughly
#' lon `[-125.0, -66.93]`, lat `[24.39, 49.39]`. We add a 0.25-degree pad so
#' coastal/border features that legitimately straddle the line still pass.
#'
#' AK/HI/PR remain unambiguously outside this padded box:
#'   - Alaska east bound : lon ~ -130    (outside)
#'   - Hawaii north end  : lat ~ 22.23   (outside; below 24.14 even with pad)
#'   - Puerto Rico west  : lon ~ -67.28  (outside; east of -66.68 even with pad)
#'
#' @keywords internal
#' @noRd
.bbox_outside_conus_dc <- function(bbox) {
  pad <- 0.25
  lon_min <- -125.0 - pad     # -125.25
  lon_max <-  -66.93 + pad    #  -66.68
  lat_min <-   24.39 - pad    #   24.14
  lat_max <-   49.39 + pad    #   49.64

  if (any(!is.finite(c(bbox["xmin"], bbox["xmax"], bbox["ymin"], bbox["ymax"])))) {
    return(TRUE)
  }
  bbox["xmin"] < lon_min ||
    bbox["xmax"] > lon_max ||
    bbox["ymin"] < lat_min ||
    bbox["ymax"] > lat_max
}


#' Iso bbox extends beyond ACS bbox (W-21-04 soft hint)
#' @keywords internal
#' @noRd
.bbox_extends_beyond <- function(iso_bbox, acs_bbox) {
  pad <- 0.05
  iso_bbox["xmin"] < acs_bbox["xmin"] - pad ||
    iso_bbox["xmax"] > acs_bbox["xmax"] + pad ||
    iso_bbox["ymin"] < acs_bbox["ymin"] - pad ||
    iso_bbox["ymax"] > acs_bbox["ymax"] + pad
}


# ============================================================================
# Internal helpers - PER-SITE LOOP support (Step 5.2)
# ============================================================================


#' Internal sf noise filter for managed planar operations
#'
#' sf emits its "although coordinates are longitude/latitude ... assumes that
#' they are planar" notice as a message on some versions and as a warning on
#' others. catchmentACS production calls transform public inputs to EPSG:5070
#' before intersection; this helper only keeps direct internal calls quiet
#' without muting caller-facing catchmentACS warnings.
#'
#' @param expr Expression to evaluate.
#' @return The value of `expr`.
#' @keywords internal
#' @noRd
.cacs_muffle_internal_sf_planar_noise <- function(expr) {
  is_planar_noise <- function(cnd) {
    msg <- conditionMessage(cnd)
    grepl("longitude/latitude", msg, ignore.case = TRUE) &&
      grepl("planar", msg, ignore.case = TRUE) &&
      grepl("st_intersects|st_intersection", msg, ignore.case = TRUE)
  }
  is_attr_noise <- function(cnd) {
    grepl("attribute variables are assumed to be spatially constant",
          conditionMessage(cnd), ignore.case = TRUE)
  }

  withCallingHandlers(
    expr,
    message = function(m) {
      if (is_planar_noise(m)) {
        invokeRestart("muffleMessage")
      }
    },
    warning = function(w) {
      if (is_planar_noise(w) || is_attr_noise(w)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}


#' Per-site intersection closure (algorithm steps 7-10)
#'
#' Runs Sec. 21.3 steps 7 (iso subset), 8 (STR-tree prefilter), 9 (exact
#' intersection + repair), and 10 (area_wt + sliver filter) for one
#' `(site_id, drive_time_min)` pair. Returns either the per-tract intersect
#' tibble (an `sf` object carrying `area_wt`, `int_area_m2`, ACS attribute
#' columns, and geometry) OR a 1-row tibble from `.empty_site_result()` for
#' any of the 4 zero-data branches:
#'
#'   - iso_one empty or duplicated     -> `iso_empty_or_duplicated`
#'   - no candidate tracts             -> `no_tract_intersection`
#'   - intersection empty post-repair  -> `intersection_empty_after_repair`
#'   - all rows filtered as slivers    -> `all_slivers_below_min_weight`
#'
#' Caller (`cacs_intersect_weight()` per-site map) wraps this in `tryCatch()`
#' so any unhandled exception (e.g. a GEOS panic that bypasses
#' `.repair_geometry()`) also converts to an `.empty_site_result()` row.
#'
#' Defensive measures:
#'   - inner `tryCatch()` around `sf::st_intersection()` GEOS call
#'   - NA-safe sliver filter `!is.na(area_wt) & area_wt > min_weight`
#'   - W-21-08 `area_wt > 1` defensive clamp against float64 boundary
#'
#' @param sid character(1) site_id for this iteration.
#' @param dt integer(1) drive_time_min for this iteration.
#' @param iso_5070 the EPSG:5070 isochrone sf for the whole batch (Step 5.1).
#' @param acs_5070 the EPSG:5070 ACS sf with `tract_area_m2` pre-computed.
#' @param min_weight dimensionless sliver threshold (Sec. 21.6.4).
#'
#' @return either an `sf` tibble (one row per intersecting tract sliver, with
#'   `area_wt` and `int_area_m2` columns added) OR a 1-row plain tibble from
#'   `.empty_site_result()`.
#'
#' @keywords internal
#' @noRd
.intersect_one_site <- function(sid, dt, iso_5070, acs_5070, min_weight) {

  # ---- Step 7: subset this site's isochrone polygon -----------------------
  iso_one <- iso_5070[iso_5070$site_id == sid &
                        iso_5070$drive_time_min == dt, ]

  if (nrow(iso_one) == 0L ||
      nrow(iso_one) > 1L ||
      all(sf::st_is_empty(iso_one))) {
    return(.empty_site_result(
      site_id        = sid,
      drive_time_min = dt,
      n_tracts       = 0L,
      failure_reason = "iso_empty_or_duplicated"
    ))
  }

  # ---- Step 8: bbox prefilter + STR-tree candidate tracts -----------------
  #
  # sf::st_intersects(x, y, sparse = TRUE) returns a list of length nrow(x).
  # We want candidate tract indices, so x = acs_5070 and we keep rows where
  # the corresponding list element is non-empty. sf's internal STR-tree
  # index makes this O(K log M) rather than O(M).
  # internal-only: production inputs have already been transformed to
  # EPSG:5070. The targeted handler guards direct internal tests without
  # silencing caller-facing warnings from the public entry point.
  hits <- .cacs_muffle_internal_sf_planar_noise(
    sf::st_intersects(acs_5070, iso_one, sparse = TRUE)
  )
  candidate_idx <- which(lengths(hits) > 0L)
  if (length(candidate_idx) == 0L) {
    return(.empty_site_result(
      site_id        = sid,
      drive_time_min = dt,
      n_tracts       = 0L,
      failure_reason = "no_tract_intersection"
    ))
  }
  acs_candidates <- acs_5070[candidate_idx, ]

  # ---- Step 9: exact intersection geometry (GEOS heavy op, K << M) --------
  #
  # st_agr <- "constant" suppresses the spatially-constant attribute warning
  # that st_intersection() emits when the LHS has non-geometric attributes.
  # The result inherits acs_candidates' attribute columns (including the
  # tract_area_m2 pre-computed in Step 5.1 / algorithm step 4).
  #
  # Inner tryCatch: a GEOS panic on a single pair gets
  # converted to a classed geometry abort that the outer per-site tryCatch
  # then routes to .empty_site_result(). This is defense-in-depth on top of
  # `.repair_geometry()` retry chain.
  sf::st_agr(acs_candidates) <- "constant"
  sf::st_agr(iso_one)        <- "constant"

  intersect_geom <- tryCatch(
    .cacs_muffle_internal_sf_planar_noise(
      sf::st_intersection(acs_candidates, iso_one)
    ),
    error = function(e) {
      .cli_abort_geometry(c(
        "GEOS intersection failed for site {.val {sid}} at drive_time {.val {dt}}.",
        "x" = "Underlying error: {.val {conditionMessage(e)}}",
        "i" = "Inspect {.code sf::st_is_valid(acs_sf)} and {.code sf::st_is_valid(iso_sf)}."
      ))
    }
  )

  intersect_geom <- .repair_geometry(intersect_geom)

  if (nrow(intersect_geom) == 0L || all(sf::st_is_empty(intersect_geom))) {
    return(.empty_site_result(
      site_id        = sid,
      drive_time_min = dt,
      n_tracts       = 0L,
      failure_reason = "intersection_empty_after_repair"
    ))
  }

  # ---- Step 10: area_wt + sliver filter (Sec. 21.6.1 + 21.6.4) ------------
  #
  # area_wt = int_area_m2 / tract_area_m2 is the universal first product
  # |I_s intersect T_j| / |T_j|. Both numerator and denominator are computed
  # in the same EPSG:5070 metric (m^2) so the ratio is dimensionless in
  # [0, 1]. Sliver filter is strict inequality: area_wt > min_weight keeps;
  # equality drops (axis 3 boundary contract). NA-safe filter
  # guards against any unexpected NA from upstream repair.
  intersect_geom$int_area_m2 <- as.numeric(sf::st_area(intersect_geom))
  intersect_geom$area_wt     <- intersect_geom$int_area_m2 /
                                  intersect_geom$tract_area_m2

  intersect_geom <- intersect_geom[
    !is.na(intersect_geom$area_wt) & intersect_geom$area_wt > min_weight, ]

  if (nrow(intersect_geom) == 0L) {
    return(.empty_site_result(
      site_id        = sid,
      drive_time_min = dt,
      n_tracts       = 0L,
      failure_reason = "all_slivers_below_min_weight"
    ))
  }

  # W-21-08 defensive clamp: float64 noise on full-coverage
  # tracts can produce area_wt = 1 + epsilon. Clamp silently when excess
  # <= 1e-9 (machine noise); warn + clamp above that (real precision loss).
  over_unit <- which(intersect_geom$area_wt > 1)
  if (length(over_unit) > 0L) {
    excess <- max(intersect_geom$area_wt[over_unit] - 1)
    if (excess > 1e-9) {
      .cli_warn_runtime(c(
        "{.field area_wt} > 1 detected for {.val {length(over_unit)}} row(s); clamping to 1.",
        "i" = "Max excess: {.val {excess}} (likely GEOS float64 noise on full-coverage tracts)."
      ))                                                                # W-21-08
    }
    intersect_geom$area_wt[over_unit] <- 1
  }

  # Attach iteration keys so Step 5.3 group_by can recover them after the
  # st_drop_geometry() call that precedes family dispatch.
  intersect_geom$site_id        <- sid
  intersect_geom$drive_time_min <- as.integer(dt)

  intersect_geom
}


#' Collect per-tract audit rows from the intersection intermediate (internal)
#'
#' The intersection intermediate is one row per intersecting tract x ACS
#' variable. The audit attribute is one row per intersecting tract, so this
#' helper keeps only geometry-free weight ingredients and de-duplicates across
#' variables.
#'
#' @param intersect_all bound per-site intersection intermediate.
#' @return tibble with columns site_id, drive_time_min, GEOID, area_wt,
#'   int_area_m2, tract_area_m2.
#' @keywords internal
#' @noRd
.collect_tract_audit <- function(intersect_all) {
  cols <- c("site_id", "drive_time_min", "GEOID", "area_wt",
            "int_area_m2", "tract_area_m2")
  empty <- tibble::tibble(
    site_id        = character(),
    drive_time_min = integer(),
    GEOID          = character(),
    area_wt        = numeric(),
    int_area_m2    = numeric(),
    tract_area_m2  = numeric()
  )
  if (is.null(intersect_all) || nrow(intersect_all) == 0L ||
      !all(cols %in% names(intersect_all))) {
    return(empty)
  }
  x <- intersect_all
  if (inherits(x, "sf")) {
    x <- sf::st_drop_geometry(x)
  }
  x <- x[!is.na(x$GEOID) & !is.na(x$area_wt), cols, drop = FALSE]
  if (nrow(x) == 0L) return(empty)
  x <- tibble::as_tibble(x)
  x$site_id <- as.character(x$site_id)
  x$drive_time_min <- as.integer(x$drive_time_min)
  x$GEOID <- as.character(x$GEOID)
  x$area_wt <- as.numeric(x$area_wt)
  x$int_area_m2 <- as.numeric(x$int_area_m2)
  x$tract_area_m2 <- as.numeric(x$tract_area_m2)
  x |>
    dplyr::distinct() |>
    dplyr::arrange(.data$site_id, .data$drive_time_min, .data$GEOID)
}


# ============================================================================
# Internal helpers - POST-LOOP support (Step 5.4)
# ============================================================================


#' Resolve ACS reporting year from acs_sf provenance (internal)
#'
#' The authoritative source for `acs_year` is
#' `attr(acs_sf, "cacs_provenance")$year`. Synthetic fixtures bypass
#' `cacs_acs_prefetch()` and therefore lack the attribute; we fall back to
#' `NA_integer_` rather than abort so per-site-loop fixtures (T21-09/10/12/15)
#' still produce a valid canonical long output.
#'
#' Single-source rule: `cacs_acs_provenance` is the alias attribute name used
#' by some downstream consumers; we honor either spelling so v1.1 rename does
#' not silently break.
#'
#' @param acs_sf the validated ACS sf passed to `cacs_intersect_weight()`.
#' @return integer(1) ACS reporting year, or `NA_integer_` when the attribute
#'   is absent or malformed.
#' @keywords internal
#' @noRd
.extract_acs_year <- function(acs_sf) {
  prov <- attr(acs_sf, "cacs_provenance") %||% attr(acs_sf, "cacs_acs_provenance")
  if (is.null(prov) || !is.list(prov)) {
    return(NA_integer_)
  }
  yr <- prov$year %||% prov$acs_year
  if (is.null(yr) || length(yr) != 1L || is.na(yr)) {
    return(NA_integer_)
  }
  as.integer(yr)
}


#' Extract or compute a stable cache key for an input sf (internal)
#'
#' Returns the upstream-produced cache key when present (Phase 4's
#' `attr(acs_sf, "cacs_provenance")$cache_key`; future Phase 3 may attach the
#' analogous attribute on isochrone outputs). For sf without a pre-computed
#' key we fall back to a deterministic SHA-256 over the sf's coordinates +
#' the canonical attribute columns so synthetic fixtures still partition the
#' composite cache key space correctly.
#'
#' This is the single seam through which `.cache_key_intersect()`'s
#' transitive-invalidation contract is implemented: any change in the
#' upstream sf yields a different fallback hash, mirroring what an
#' upstream-cached call would do via its own cache_key.
#'
#' @param x an `sf` object (iso_sf or acs_sf shape).
#' @return character(1) cache key (hex digest).
#' @keywords internal
#' @noRd
.extract_cache_key <- function(x) {
  if (is.null(x)) return(NA_character_)
  prov <- attr(x, "cacs_provenance") %||%
          attr(x, "cacs_acs_provenance") %||%
          attr(x, "cacs_isochrone_provenance")
  if (is.list(prov) && is.character(prov$cache_key) &&
      length(prov$cache_key) == 1L && nzchar(prov$cache_key)) {
    return(prov$cache_key)
  }
  # Fallback: deterministic hash of coordinates + non-geometry attribute
  # columns. We do NOT include sf-internal attributes (CRS metadata, agr) since
  # those would force false invalidations across version-equivalent CRS specs.
  body_cols <- sf::st_drop_geometry(x)
  coords    <- tryCatch(sf::st_coordinates(x), error = function(e) NULL)
  digest::digest(list(coords = coords, attrs = body_cols),
                 algo = "sha256", serialize = TRUE)
}

.extract_acs_content_hash <- function(acs_sf) {
  if (is.null(acs_sf)) return(NA_character_)
  attrs <- tryCatch(sf::st_drop_geometry(acs_sf), error = function(e) acs_sf)
  attrs <- as.data.frame(attrs, stringsAsFactors = FALSE)
  keep <- intersect(c("GEOID", "variable", "estimate", "moe", "NAME"), names(attrs))
  attrs_keep <- attrs[, keep, drop = FALSE]
  geometry_hash <- tryCatch(
    {
      wkb <- sf::st_as_binary(sf::st_geometry(acs_sf), EWKB = TRUE)
      vapply(
        wkb,
        function(x) digest::digest(x, algo = "sha256", serialize = FALSE),
        character(1)
      )
    },
    error = function(e) {
      geom_txt <- tryCatch(
        as.character(sf::st_as_text(sf::st_geometry(acs_sf))),
        error = function(e2) rep(NA_character_, nrow(attrs_keep))
      )
      vapply(
        geom_txt,
        digest::digest,
        character(1),
        algo = "sha256",
        serialize = TRUE
      )
    }
  )
  attrs_keep$.geometry_hash <- geometry_hash
  ord_cols <- intersect(c("GEOID", "variable", "NAME", "estimate", "moe"),
                        names(attrs_keep))
  if (length(ord_cols) > 0L && nrow(attrs_keep) > 0L) {
    ord <- do.call(order, attrs_keep[ord_cols])
    attrs_keep <- attrs_keep[ord, , drop = FALSE]
  }
  rownames(attrs_keep) <- NULL
  crs_id <- tryCatch({
    crs <- sf::st_crs(acs_sf)
    if (!is.na(crs$epsg)) as.character(crs$epsg) else crs$wkt
  }, error = function(e) NA_character_)
  prov <- attr(acs_sf, "cacs_provenance") %||%
    attr(acs_sf, "cacs_acs_provenance")
  prov_keep <- if (is.list(prov)) {
    prov[intersect(names(prov), c(
      "state", "year", "survey", "geography", "variables",
      "cache_key", "cache_namespace", "acs_geometry_vintage"
    ))]
  } else {
    list()
  }
  digest::digest(
    list(
      schema = "acs-content-v1",
      crs = crs_id,
      rows = attrs_keep,
      provenance = prov_keep
    ),
    algo = "sha256",
    serialize = TRUE
  )
}

.extract_acs_content_aware_cache_key <- function(acs_sf) {
  digest::digest(
    list(
      upstream_cache_key = .extract_cache_key(acs_sf),
      acs_content_hash = .extract_acs_content_hash(acs_sf)
    ),
    algo = "sha256",
    serialize = TRUE
  )
}

.extract_iso_ring_topology <- function(iso_sf) {
  if (!is.null(iso_sf) && "ring_topology" %in% names(iso_sf)) {
    vals <- sort(unique(as.character(stats::na.omit(iso_sf$ring_topology))))
    if (length(vals) > 0L) return(paste(vals, collapse = "|"))
  }
  prov <- attr(iso_sf, "cacs_isochrone_provenance") %||%
    attr(iso_sf, "cacs_provenance")
  val <- if (is.list(prov)) prov$ring_topology else NULL
  if (is.character(val) && length(val) >= 1L && nzchar(val[[1L]])) {
    return(paste(sort(unique(val)), collapse = "|"))
  }
  "unknown"
}

.extract_iso_res_param <- function(iso_sf) {
  prov <- attr(iso_sf, "cacs_isochrone_provenance") %||%
    attr(iso_sf, "cacs_provenance")
  val <- if (is.list(prov)) prov$res_param %||% prov$res else NULL
  if (is.null(val)) {
    val <- attr(iso_sf, "cacs_res_param", exact = TRUE)
  }
  if (is.null(val) || length(val) == 0L || is.na(val[[1L]])) {
    return("unknown")
  }
  as.character(val[[1L]])
}


#' Composite cache key factory for cacs_intersect_weight (14-dim, internal)
#'
#' Sec. 21.8 composite payload — 3 upstream cache_keys/hashes + 2 weight
#' params + 5-element software stack + schema version + bg_pop pop hash =
#' 14 sorted-named elements. Defers to `.cacs_cache_key(., "intersect")`
#' which validates the dim count (= 14L per `.CACS_CACHE_NAMESPACE_DIMS`)
#' and produces a SHA-256 digest.
#'
#' Transitive invalidation by construction:
#'   - iso_sf upstream change   -> iso_cache_key shifts  -> different digest
#'   - acs_sf upstream change   -> acs_cache_key shifts  -> different digest
#'   - bg_pop_sf change         -> bg_pop_*_hash shift   -> different digest
#'   - weight_method / min_weight diff -> different digest
#'   - sf/GEOS/PROJ/R stack bump       -> different digest
#'
#' @param iso_sf canonical isochrone sf (any class accepted; .extract_cache_key
#'   handles the upstream-key vs fallback-hash branch).
#' @param acs_sf canonical ACS sf.
#' @param bg_pop_sf optional block-group population sf or NULL.
#' @param weight_method character(1) sanctioned weight method.
#' @param min_weight numeric(1) dimensionless sliver threshold.
#' @return character(1) SHA-256 hex digest.
#' @keywords internal
#' @noRd
.cache_key_intersect <- function(iso_sf,
                                  acs_sf,
                                  bg_pop_sf,
                                  weight_method,
                                  min_weight) {

  payload <- list(
    iso_cache_key    = .extract_cache_key(iso_sf),
    acs_cache_key    = .extract_acs_content_aware_cache_key(acs_sf),
    bg_pop_geom_hash = if (!is.null(bg_pop_sf)) {
      digest::digest(sf::st_coordinates(bg_pop_sf), algo = "sha256")
    } else {
      NA_character_
    },
    bg_pop_pop_hash  = if (!is.null(bg_pop_sf) && "population_est" %in% names(bg_pop_sf)) {
      digest::digest(bg_pop_sf$population_est, algo = "sha256")
    } else {
      NA_character_
    },
    weight_method    = weight_method,
    min_weight       = min_weight,
    schema_version   = "1.0",
    package_version  = as.character(utils::packageVersion("catchmentACS")),
    sf_version       = as.character(utils::packageVersion("sf")),
    geos_version     = as.character(sf::sf_extSoftVersion()[["GEOS"]]),
    proj_version     = as.character(sf::sf_extSoftVersion()[["PROJ"]]),
    r_version        = paste(R.version$major, R.version$minor, sep = "."),
    iso_ring_topology = .extract_iso_ring_topology(iso_sf),
    iso_res_param    = .extract_iso_res_param(iso_sf)
  )                                                                  # 14 keys

  .cacs_cache_key(payload, namespace = "intersect")
}


#' Cache-write toggle for the intersect namespace (internal)
#'
#' Resolution order (first non-empty wins):
#'   1. `options(catchmentACS.cache_intersect)` — test isolation
#'   2. `Sys.getenv("CACS_CACHE_INTERSECT")` — user opt-out
#'   3. TRUE (default — cache is on)
#'
#' Mirrors the pattern in `.cacs_cache_dir_impl()` so unit tests can disable
#' the cache namespace without setting an unwritable cache dir.
#'
#' @return logical(1) TRUE if cache get/put should run.
#' @keywords internal
#' @noRd
.cacs_cache_intersect_enabled <- function() {
  if (!.cacs_cache_enabled("intersect")) return(FALSE)
  opt <- getOption("catchmentACS.cache_intersect", NULL)
  if (!is.null(opt)) return(isTRUE(opt))
  envv <- Sys.getenv("CACS_CACHE_INTERSECT", unset = NA)
  if (!is.na(envv) && nzchar(envv)) {
    return(!identical(tolower(envv), "false") &&
             !identical(envv, "0"))
  }
  TRUE
}


#' Null-coalesce operator (internal, alias of rlang::%||%)
#'
#' Already exported from rlang via NAMESPACE; redeclared here only to silence
#' R CMD check's "no visible binding" on internal call sites that load before
#' the rlang import is wired. Pure forward.
#'
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
