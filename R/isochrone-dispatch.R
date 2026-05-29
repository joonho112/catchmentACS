# ============================================================================
# isochrone-dispatch.R - cacs_isochrone() public entry + provider dispatch.
#                       Sec. 19.2 byte-identical signature + Sec. 19.3 step 1-3 body.
#                       Steps 4-10 placeholder (Step 3.4 will fill in).
# ============================================================================


#' Compute drive-time isochrones via provider dispatch
#'
#' Given a tibble/data.frame or `sf` of POINT sites, computes one isochrone
#' (drive-time catchment polygon) per `(site_id, drive_time_min)` pair via the
#' chosen routing provider, and returns a 16-column canonical `sf`.
#'
#' Returned objects carry `attr(x, "cacs_isochrone_provenance")` with the cache
#' key, effective polygon resolution (`res`), effective OSRM server and profile,
#' request-budget diagnostics, and the cumulative ring topology, so downstream
#' caching can distinguish these inputs.
#'
#' @param sites Either an `sf` POINT object in EPSG:4326, or a
#'   tibble/data.frame with `lon`, `lat`, and `site_id` columns. A legacy
#'   `point_id` column is auto-renamed to `site_id` with a warning.
#' @param drive_times Numeric vector of drive times in minutes; default
#'   `c(5, 10, 15)`.
#' @param provider One of `c("osrm", "ors", "mapbox", "r5r")`; default
#'   `"osrm"`. OSRM and ORS are implemented in the current release. Mapbox
#'   and r5r are reserved provider names that fail loud with a future-release
#'   message.
#' @param profile Character(1) routing profile; default `"car"`.
#' @param osrm_mode One of `c("demo", "docker")`; default `"demo"`.
#' @param ors_api_key Character(1) ORS API key.
#' @param mapbox_token Character(1) Mapbox token.
#' @param r5r_core An r5r core object or `NULL`.
#' @param osm_snapshot_date `Date`, character ISO date string, or `NULL`.
#' @param cache_dir Character(1) directory path or `NULL`.
#'   Cache entries are written with `sha256` fingerprint sidecars.
#' @param verbose Logical(1); default `TRUE`. Drives the progress reporter.
#'   The mode is auto-selected by a priority chain: (1) `verbose = FALSE`
#'   silences output; (2) `Sys.getenv("CACS_QUIET") == "1"` silences output;
#'   (3) `getOption("catchmentACS.progress")` of `"off"` silences, `"force"`
#'   forces the bar, `"auto"` (default) falls through to the size threshold;
#'   (4) fewer than 5 sites shows a start-and-end summary only; (5) 5 or more
#'   sites shows a per-site progress bar with ETA and rate. See the "Progress
#'   reporting" section of [cacs_run()] for the package-wide policy and how to
#'   globally disable or force the bar regardless of batch size.
#' @param ... Provider-specific passthrough; unknown arguments warn and are
#'   dropped. For OSRM, `res` controls polygon detail. If omitted, public demo
#'   calls resolve to `30L` by default to reduce HTTP 429 (rate-limit) risk;
#'   docker and protection-disabled calls resolve to `70L`. Explicit `res`
#'   values are respected and silent.
#'
#' @return An `sf` object with 16 columns in the canonical isochrone schema.
#'
#' @section Provider status:
#' OSRM is the default and supports both the public demo endpoint
#' (`osrm_mode = "demo"`) and a self-hosted/local endpoint
#' (`osrm_mode = "docker"`). ORS is implemented through the optional
#' `openrouteservice` package and requires a non-empty key supplied through
#' `ors_api_key` or `ORS_API_KEY`.
#' Mapbox and r5r are intentionally retained in the provider enum for
#' compatibility and roadmap visibility, but they abort immediately in this
#' release instead of returning partial or silent placeholder results.
#'
#' @section OSRM res defaults and demo-budget protection:
#'
#' When `res` is omitted, `osrm_mode = "docker"` and callers who disable demo
#' protection use a high-resolution default of `70L` for more detailed polygon
#' geometry. The public demo endpoint is protected by resolving an omitted
#' `res` to `30L` when `osrm_mode = "demo"` and
#' `getOption("catchmentACS.osrm_demo_budget_protect", TRUE)` is `TRUE`.
#'
#' The protected demo path emits a once-per-session message of class
#' `catchmentACS_message_demo_budget_protected`. The docker and
#' protection-disabled paths emit a `catchmentACS_message_res_default_changed`
#' notice and resolve to `70L`. To silence either notice, pass `res`
#' explicitly, e.g. `res = 30L`, `res = 50L`, or `res = 70L`.
#'
#' @section Per-urbanicity expected divergence:
#'
#' Raw isochrone polygon area is sensitive to road-network density, routing
#' engine snapshots, and polygon-resolution choices. In an Alabama validation
#' replay, the practical area-difference bands were approximately urban
#' +/-5 percent, mid-density +/-15 percent, and rural +/-30 percent under
#' ordinary conditions. A sparse rural anchor exceeded that wide typical band
#' at about 55 percent area difference versus the archive run, while its
#' tract-membership Jaccard index was 1.000.
#'
#' For policy aggregation, tract membership and downstream weighted estimates
#' are the trusted invariants; raw polygon area is a routing/snapshot diagnostic.
#' When comparing to older archive runs, interpret area divergence by site
#' context: use tight bands for urban sites, wider bands for mid-density sites,
#' and inspect rural outliers with a tract-membership/Jaccard check before
#' treating an area difference as an aggregation bug. See
#' `vignette("getting-started", package = "catchmentACS")` and
#' `vignette("porting-v01-to-v03", package = "catchmentACS")` for examples.
#'
#' @section OSRM performance note:
#'
#' The upstream `osrm` package computes isochrones by querying a travel-time
#' table over a square grid of `res * res` destinations. With the public
#' `routing.openstreetmap.de` demo endpoint, that package chunks requests in
#' groups of 75 and sleeps one second per full chunk. At `res = 70L`, this
#' implies 4,900 grid destinations and about 65 seconds of forced sleep per
#' site before ordinary network and geometry overhead. The public-demo
#' omitted default of `30L` substantially lowers that table budget.
#'
#' `osrm_mode = "docker"` now resolves to a local OSRM server
#' (`http://0.0.0.0:5000/` by default, override with
#' `options(catchmentACS.osrm_docker_server = "...")` or `osrm.server = ...`).
#' Custom/self-hosted endpoints use the fast `osrm` package path with no
#' public-demo forced sleep. The effective server/profile participate in the
#' isochrone cache key.
#'
#' @seealso [cacs_run()] for the orchestrator that drives this step and the
#'   package-wide progress reporting policy, [cacs_intersect_weight()] for the
#'   downstream step this output feeds, and [cacs_acs_validate()].
#' @family core pipeline
#' @export
#' @examples
#' \dontrun{
#' data("cacs_alabama_sites")
#' iso <- cacs_isochrone(
#'   sites       = cacs_alabama_sites[1:3, ],
#'   drive_times = c(5, 10),
#'   provider    = "osrm",
#'   osrm_mode   = "demo"
#' )
#' }
cacs_isochrone <- function(
  sites,
  drive_times = c(5, 10, 15),
  provider = c("osrm", "ors", "mapbox", "r5r"),
  profile = "car",
  osrm_mode = c("demo", "docker"),
  ors_api_key = Sys.getenv("ORS_API_KEY"),
  mapbox_token = Sys.getenv("MAPBOX_TOKEN"),
  r5r_core = NULL,
  osm_snapshot_date = NULL,
  cache_dir = NULL,
  verbose = TRUE,
  ...
) {

  # v0.2 F1 verbose progress reporter (hybrid bar/line mode).
  # Validate `verbose` early so providers can rely on it being logical(1).
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    .cli_abort_schema(c(
      "{.arg verbose} must be {.cls logical(1)} (TRUE or FALSE).",
      "x" = "Got {.cls {class(verbose)[1L]}} of length {.val {length(verbose)}}."
    ))
  }

  # Step 1: Normalize sites (first-failure schema abort)
  sites <- .normalize_sites_input(sites)

  # Step 2a: Resolve enum args
  provider_choices <- .SANCTIONED_PROVIDERS
  provider  <- tryCatch(
    match.arg(provider),
    error = function(e) {
      .cli_abort_schema(c(
        "{.arg provider} must be one of {.val {provider_choices}}.",
        "x" = "Got {.val {provider}}.",
        "i" = "The current release implements {.val osrm} and {.val ors}; {.val mapbox} and {.val r5r} are future-provider stubs."
      ))
    }
  )
  osrm_mode <- tryCatch(
    match.arg(osrm_mode),
    error = function(e) {
      .cli_abort_schema(c(
        "{.arg osrm_mode} must be one of {.val {c('demo', 'docker')}}.",
        "x" = "Got {.val {osrm_mode}}."
      ))
    }
  )

  # Step 2b: Validate scalar args (collation up to 5 per Sec. 19.3 step 2)
  .validate_iso_scalar_args(
    drive_times       = drive_times,
    profile           = profile,
    ors_api_key       = ors_api_key,
    mapbox_token      = mapbox_token,
    r5r_core          = r5r_core,
    osm_snapshot_date = osm_snapshot_date,
    cache_dir         = cache_dir
  )

  # Step 2c: provider-specific ... passthrough. Sanctioned keys are forwarded;
  # unknown keys warn and are dropped.
  provider_args <- .split_iso_provider_args(provider, list(...))
  if (identical(provider, "osrm")) {
    res_omitted <- !"res" %in% names(provider_args$known)
    res_decision <- .cacs_osrm_resolve_demo_res(
      res_param = if (res_omitted) NULL else provider_args$known$res,
      osrm_mode = osrm_mode,
      options_protect = getOption("catchmentACS.osrm_demo_budget_protect", TRUE)
    )
    provider_args$known$res <- res_decision$res_effective
    if (res_omitted) {
      if (isTRUE(res_decision$emit_msg)) {
        .cli_inform_demo_budget_protected()
      } else {
        .cli_inform_res_default_changed()
      }
    }

    provider_args$known <- .resolve_osrm_provider_args(
      osrm_mode = osrm_mode,
      profile = profile,
      args = provider_args$known
    )
    if (isTRUE(verbose)) {
      .emit_osrm_perf_mode(
        provider_args$known,
        drive_times = drive_times,
        n_sites = nrow(sites)
      )
    }
  }
  if (length(provider_args$unknown_names) > 0L) {
    .cli_warn_provenance(c(
      "Unknown argument{?s} {.arg {provider_args$unknown_names}} passed via {.code ...}.",
      "i" = "Not forwarded to {.field provider} = {.val {provider}} in the current release."
    ))
  }

  # Step 3: Resolve osm_snapshot_date (frozen-snapshot reproducibility)
  snap <- .resolve_osm_snapshot(provider, osm_snapshot_date)

  # Step 4: Construct cache key (16-dim per Sec. 19.6 + v0.3 topology) ----
  cache_payload <- list(
    sites_coords_hash   = digest::digest(sf::st_coordinates(sites)),
    site_ids_hash       = digest::digest(sites$site_id),
    drive_times_hash    = digest::digest(sort(unique(as.integer(drive_times)))),
    provider            = provider,
    profile             = profile,
    osrm_mode           = osrm_mode,
    osm_snapshot_date   = as.character(snap$date),
    osm_snapshot_status = as.character(snap$status),
    ors_api_key_hash    = if (identical(provider, "ors"))    digest::digest(ors_api_key)  else "",
    mapbox_token_hash   = if (identical(provider, "mapbox")) digest::digest(mapbox_token) else "",
    r5r_core_hash       = if (identical(provider, "r5r"))    digest::digest(r5r_core)     else "",
    passthrough_normalized = provider_args$known,
    sf_version          = as.character(utils::packageVersion("sf")),
    proj_version        = as.character(sf::sf_extSoftVersion()[["PROJ"]]),
    geos_version        = as.character(sf::sf_extSoftVersion()[["GEOS"]]),
    ring_topology       = "cumulative"
  )  # 16 keys per Sec. 19.6 + v0.3 cache contract

  cache_key <- .cacs_cache_key(cache_payload, namespace = "isochrone")

  # Step 4b: Cache lookup --------------------------------------------------
  cached <- suppressMessages(.cacs_cache_get(cache_key, "isochrone", cache_dir = cache_dir))
  if (!is.null(cached)) {
    cached <- .attach_isochrone_cache_provenance(
      cached,
      cache_key = cache_key,
      provider = provider,
      profile = profile,
      osrm_mode = osrm_mode,
      snap = snap,
      provider_args = provider_args,
      drive_times = drive_times
    )
    .cli_inform_cache(
      "Cache hit; returning cached isochrone for {.field provider} = {.val {provider}} ({.val {nrow(sites)}} sites).",
      phase = "isochrone"
    )
    # v0.2 F1: cache hit skips the reporter (no per-site loop to tick). The
    # cache-hit inform above is the user-facing signal; we suppress the
    # standardized summary to avoid implying a re-run happened.
    return(cached)
  }

  # Steps 5-9: provider dispatch + canonical-schema normalize ---------------
  # v0.2 F1: spin up the verbose progress reporter and pass `on_site_tick`
  # down to provider helpers. `n_success` / `n_failed` are counted at the
  # `out` boundary below (`failure_reason` non-NA flags failed sites). The
  # `on.exit(finish)` guards against mid-dispatch abort (e.g. HTTP 429
  # operator stop) so the standardized summary still lands.
  n_sites <- nrow(sites)
  prog <- .cacs_progress_reporter(
    n       = n_sites,
    label   = "Isochrones",
    verbose = verbose
  )
  prog_finish_state <- new.env(parent = emptyenv())
  prog_finish_state$n_success <- 0L
  prog_finish_state$n_failed  <- n_sites
  on.exit(
    prog$finish(
      n_success = prog_finish_state$n_success,
      n_failed  = prog_finish_state$n_failed
    ),
    add = TRUE
  )

  out_raw <- switch(
    provider,
    "osrm"   = do.call(.iso_via_osrm, c(
      list(sites = sites, drive_times = drive_times, profile = profile,
           osrm_mode = osrm_mode, .on_site_tick = prog$tick),
      provider_args$known
    )),
    "ors"    = do.call(.iso_via_ors, c(
      list(sites = sites, drive_times = drive_times, profile = profile,
           api_key = ors_api_key, .on_site_tick = prog$tick),
      provider_args$known
    )),
    "mapbox" = do.call(.iso_via_mapbox, c(
      list(sites = sites, drive_times = drive_times, profile = profile, token = mapbox_token),
      provider_args$known
    )),
    "r5r"    = do.call(.iso_via_r5r, c(
      list(sites = sites, drive_times = drive_times, profile = profile, r5r_core = r5r_core),
      provider_args$known
    ))
  )

  out <- .normalize_iso_schema(out_raw, snap)
  out <- .attach_isochrone_cache_provenance(
    out,
    cache_key = cache_key,
    provider = provider,
    profile = profile,
    osrm_mode = osrm_mode,
    snap = snap,
    provider_args = provider_args,
    drive_times = drive_times
  )

  # v0.2 F1: tally success/failure from the canonical 16-col schema so the
  # standardized summary reflects the true outcome (failure_reason non-NA
  # means the per-site retry chain exhausted or hit a non-retriable 4xx).
  # Per-site is the natural unit: each unique site_id is one "iteration"
  # regardless of how many drive_times it produced.
  per_site_failed <- out |>
    sf::st_drop_geometry() |>
    dplyr::group_by(.data$site_id) |>
    dplyr::summarise(failed = any(!is.na(.data$failure_reason)), .groups = "drop")
  prog_finish_state$n_failed  <- as.integer(sum(per_site_failed$failed))
  prog_finish_state$n_success <- as.integer(nrow(per_site_failed) -
                                            prog_finish_state$n_failed)

  # Step 10: Persist to cache (W-12 on write failure, never blocks return) --
  .cacs_cache_put(out, cache_key, "isochrone", cache_dir = cache_dir)

  out
}

.attach_isochrone_cache_provenance <- function(out,
                                               cache_key,
                                               provider,
                                               profile,
                                               osrm_mode,
                                               snap,
                                               provider_args,
                                               drive_times) {
  res_param <- if ("res" %in% names(provider_args$known)) {
    provider_args$known$res
  } else {
    NA_integer_
  }
  iso_prov <- list(
    cache_key = cache_key,
    cache_namespace = "isochrone",
    provider = provider,
    profile = profile,
    osrm_mode = osrm_mode,
    osm_snapshot_date = as.character(snap$date),
    osm_snapshot_status = as.character(snap$status),
    res_param = res_param,
    osrm_server = provider_args$known$osrm.server %||% NA_character_,
    osrm_profile = provider_args$known$osrm.profile %||% NA_character_,
    osrm_request_budget = if (identical(provider, "osrm")) {
      .osrm_request_budget(
        res = res_param,
        breaks = drive_times,
        osrm_server = provider_args$known$osrm.server %||% NA_character_
      )
    } else {
      NULL
    },
    ring_topology = "cumulative"
  )
  attr(out, "cacs_isochrone_provenance") <- iso_prov
  attr(out, "cacs_provenance") <- iso_prov
  attr(out, "cacs_res_param") <- res_param
  out
}


# ============================================================================
# Internal: .normalize_sites_input()  (Sec. 19.3 step 1)
# ============================================================================

#' @keywords internal
#' @noRd
.normalize_sites_input <- function(sites) {

  if (is.null(sites) || !inherits(sites, c("sf", "tbl_df", "data.frame"))) {
    .cli_abort_schema(c(
      "{.arg sites} must be {.cls sf}, {.cls tbl_df}, or {.cls data.frame}.",
      "x" = "Got {.cls {if (is.null(sites)) 'NULL' else class(sites)[[1L]]}}.",
      "i" = "Pass columns {.field site_id}, {.field lon}, and {.field lat}, or wrap with {.code sf::st_as_sf(x, coords = c('lon','lat'), crs = 4326)}."
    ))
  }

  # W-01: legacy point_id rename
  nm <- names(sites)
  if ("point_id" %in% nm && !"site_id" %in% nm) {
    .cli_warn_provenance(c(
      "Renamed {.field point_id} {.arg ->} {.field site_id} (legacy alias).",
      "i" = "Use {.field site_id} directly going forward."
    ))
    names(sites)[names(sites) == "point_id"] <- "site_id"
  }

  if (!"site_id" %in% names(sites)) {
    .cli_abort_schema(c(
      "{.arg sites} must contain a {.field site_id} column."
    ))
  }

  dup_idx <- duplicated(sites$site_id)
  n_dup <- sum(dup_idx)
  if (n_dup > 0L) {
    dup_vals <- unique(sites$site_id[dup_idx])
    .cli_abort_schema(c(
      "{.field site_id} must be unique.",
      "x" = "Found {.val {n_dup}} duplicated row{?s}: {.val {utils::head(dup_vals, 5L)}}."
    ))
  }

  if (inherits(sites, "sf")) {
    crs <- sf::st_crs(sites)
    if (is.na(crs)) {
      .cli_abort_schema(c(
        "{.arg sites} has missing CRS.",
        "i" = "Set with {.code sf::st_set_crs(sites, 4326)}."
      ))
    }
    if (!identical(as.integer(crs$epsg), 4326L)) {
      .cli_abort_schema(c(
        "{.arg sites} CRS must be {.val EPSG:4326}.",
        "x" = "Got {.val {as.character(crs$epsg)}}.",
        "i" = "Re-project with {.code sf::st_transform(sites, 4326)}."
      ))
    }
    gtypes <- unique(as.character(sf::st_geometry_type(sites)))
    if (!all(gtypes == "POINT")) {
      .cli_abort_schema(c(
        "{.arg sites} geometry must be {.val POINT}.",
        "x" = "Got {.val {gtypes}}."
      ))
    }
  } else {
    if (!all(c("lon", "lat") %in% names(sites))) {
      missing <- setdiff(c("lon", "lat"), names(sites))
      .cli_abort_schema(c(
        "{.arg sites} non-sf inputs must contain {.field lon} and {.field lat} columns.",
        "x" = "Missing: {.field {missing}}."
      ))
    }
    if (!is.numeric(sites$lon) || !is.numeric(sites$lat)) {
      .cli_abort_schema(c(
        "{.field lon} and {.field lat} must be {.cls numeric}."
      ))
    }
    bad_lon <- !is.na(sites$lon) & (sites$lon < -180 | sites$lon > 180)
    bad_lat <- !is.na(sites$lat) & (sites$lat <  -90 | sites$lat >  90)
    if (any(bad_lon) || any(bad_lat)) {
      .cli_abort_schema(c(
        "{.field lon}/{.field lat} out of range.",
        "x" = "lon must be in {.val [-180, 180]}; lat in {.val [-90, 90]}."
      ))
    }
    sites <- sf::st_as_sf(sites, coords = c("lon", "lat"), crs = 4326)
  }

  sites
}


# ============================================================================
# Internal: .validate_iso_scalar_args()  (Sec. 19.3 step 2 - collation up to 5)
# ============================================================================

#' @keywords internal
#' @noRd
.validate_iso_scalar_args <- function(drive_times, profile, ors_api_key,
                                      mapbox_token, r5r_core,
                                      osm_snapshot_date, cache_dir) {
  errs <- character(0)

  # E-03: drive_times
  if (!is.numeric(drive_times) || length(drive_times) == 0L) {
    errs <- c(errs, "x" = "{.arg drive_times} must be a non-empty {.cls numeric} vector; got {.cls {class(drive_times)[[1L]]}}.")
  } else {
    if (length(drive_times) > 6L) {
      errs <- c(errs, "x" = "{.arg drive_times} length must be {.val <=6}; got {length(drive_times)}.")
    }
    bad <- drive_times[!is.finite(drive_times) | drive_times <= 0 |
                       drive_times > 60 | drive_times != as.integer(drive_times)]
    if (length(bad) > 0L) {
      errs <- c(errs, "x" = "{.arg drive_times} values must be positive integers in {.val [1, 60]}; bad: {.val {bad}}.")
    }
  }

  # E-05: profile
  if (!is.character(profile) || length(profile) != 1L || is.na(profile) || !nzchar(profile)) {
    errs <- c(errs, "x" = "{.arg profile} must be a non-empty {.cls character} scalar.")
  }

  # E-07: ors_api_key
  if (!is.character(ors_api_key) || length(ors_api_key) != 1L) {
    errs <- c(errs, "x" = "{.arg ors_api_key} must be {.cls character(1)}.")
  }

  # E-08: mapbox_token
  if (!is.character(mapbox_token) || length(mapbox_token) != 1L) {
    errs <- c(errs, "x" = "{.arg mapbox_token} must be {.cls character(1)}.")
  }

  # E-09: r5r_core
  if (!is.null(r5r_core) && !inherits(r5r_core, c("r5r_core", "list", "jobjRef"))) {
    errs <- c(errs, "x" = "{.arg r5r_core} must be {.val NULL} or an r5r core object.")
  }

  # E-10: osm_snapshot_date
  if (!is.null(osm_snapshot_date)) {
    ok <- inherits(osm_snapshot_date, "Date") ||
          (is.character(osm_snapshot_date) && length(osm_snapshot_date) == 1L &&
           !is.na(suppressWarnings(as.Date(osm_snapshot_date))))
    if (!ok) {
      errs <- c(errs, "x" = "{.arg osm_snapshot_date} must be {.cls Date} / ISO date string / {.val NULL}.")
    }
  }

  # E-11: cache_dir
  if (!is.null(cache_dir)) {
    if (!is.character(cache_dir) || length(cache_dir) != 1L || is.na(cache_dir)) {
      errs <- c(errs, "x" = "{.arg cache_dir} must be {.cls character(1)} or {.val NULL}.")
    }
  }

  if (length(errs) > 0L) {
    n_err <- length(errs)
    head_msg <- if (n_err == 1L) {
      "Invalid argument to {.fn cacs_isochrone}."
    } else {
      paste0("Invalid arguments to {.fn cacs_isochrone} (", n_err, " violations).")
    }
    body <- utils::head(errs, 5L)
    tail <- if (n_err > 5L) {
      c("i" = paste0("... and ", n_err - 5L, " more validation failure(s) suppressed."))
    } else character(0)
    .cli_abort_schema(c(head_msg, body, tail))
  }

  invisible(TRUE)
}


# ============================================================================
# Internal: .resolve_osm_snapshot()  (Sec. 19.3 step 3)
# ============================================================================

#' @keywords internal
#' @noRd
.resolve_osm_snapshot <- function(provider, osm_snapshot_date) {
  if (!is.null(osm_snapshot_date)) {
    d_str <- as.character(
      if (inherits(osm_snapshot_date, "Date")) osm_snapshot_date
      else as.Date(osm_snapshot_date)
    )
    return(list(date = d_str, status = "user_supplied"))
  }
  # Step 3.4 will add provider-specific best-effort lookup
  list(date = "unknown", status = "unknown_best_effort")
}


# ============================================================================
# Internal: .split_iso_provider_args()  (Sec. 19.4 sanctioned passthrough)
# ============================================================================

#' @keywords internal
#' @noRd
.split_iso_provider_args <- function(provider, args) {
  if (length(args) == 0L) {
    return(list(known = list(), unknown_names = character(0)))
  }
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))

  allowed <- switch(
    provider,
    osrm   = .OSRM_SANCTIONED_PASSTHROUGH,
    ors    = .ORS_SANCTIONED_PASSTHROUGH,
    mapbox = character(0),
    r5r    = character(0),
    character(0)
  )

  named <- nzchar(nms)
  known_mask <- named & nms %in% allowed
  unknown <- nms[!known_mask]
  unknown[!nzchar(unknown)] <- "<unnamed>"

  list(
    known = args[known_mask],
    unknown_names = unique(unknown)
  )
}

.normalize_osrm_server <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    return(.OSRM_PUBLIC_DEMO_SERVER)
  }
  x <- trimws(x)
  if (!grepl("/$", x)) x <- paste0(x, "/")
  x
}

.resolve_osrm_provider_args <- function(osrm_mode, profile, args = list()) {
  args <- args %||% list()
  if ("osrm.profile_name" %in% names(args) &&
      !"osrm.profile" %in% names(args)) {
    args$osrm.profile <- args$osrm.profile_name
  }
  args$osrm.profile_name <- NULL

  if (!"osrm.profile" %in% names(args)) {
    args$osrm.profile <- profile
  }
  if (!"osrm.server" %in% names(args)) {
    if (identical(osrm_mode, "docker")) {
      args$osrm.server <- getOption(
        "catchmentACS.osrm_docker_server",
        .OSRM_DOCKER_SERVER_DEFAULT
      )
    } else {
      args$osrm.server <- getOption("osrm.server", .OSRM_PUBLIC_DEMO_SERVER)
    }
  }
  args$osrm.server <- .normalize_osrm_server(args$osrm.server)
  args
}

.emit_osrm_perf_mode <- function(provider_args, drive_times, n_sites) {
  budget <- .osrm_request_budget(
    res = provider_args$res %||% .OSRM_RES_DEFAULT,
    breaks = drive_times,
    osrm_server = provider_args$osrm.server %||% .OSRM_PUBLIC_DEMO_SERVER
  )
  if (isTRUE(budget$public_demo)) {
    return(invisible(FALSE))
  }
  .cli_inform_perf_fix_applied(c(
    "OSRM fast-server path active for {.val {n_sites}} site{?s}.",
    "i" = "Server: {.url {provider_args$osrm.server}}.",
    "i" = "Public-demo forced sleep avoided: about {.val {budget$public_demo_sleep_floor_sec_per_site}} second{?s} per site at res = {.val {budget$res}}."
  ))
  invisible(TRUE)
}


# Local %||%
`%||%` <- function(x, y) if (is.null(x)) y else x
