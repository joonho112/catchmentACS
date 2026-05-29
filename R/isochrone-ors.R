# ============================================================================
# isochrone-ors.R - .iso_via_ors() body (Step 3.3 of Phase 3).
#
# Implements Sec. 19.4 row 2 (ORS backend) - mirror of .iso_via_osrm() with
# ORS-specific differences:
#   - Sanctioned `...` keys whitelist: c("attributes", "area_units", "smoothing")
#   - Retry chain: 3 attempts, exponential backoff 1s / 2s / 4s
#     * HTTP 5xx / timeout / NA status     -> retry (transient)
#     * HTTP 4xx (other than 401/403/429)  -> no retry, row-level failure
#     * HTTP 429                           -> batch abort E-13 (operator)
#     * HTTP 401 / 403                     -> batch abort E-14 (credential)
#   - ORS isochrone responses come back as GeoJSON FeatureCollection with a
#     `value` property in *seconds* when `range_type = "time"`. We divide by
#     60 to map back onto the user-supplied `drive_time_min` breaks.
#   - Returns the 16-col canonical sf per Sec. 19.5 directly so the placeholder
#     dispatch in Step 3.1 (which does `out <- switch(...); out`) continues
#     to receive an sf object end-to-end. Step 3.4 will route through
#     `.normalize_iso_schema()`; the 16-col layer coded here is
#     forward-compatible (identical column names + types to .iso_via_osrm).
#
# Cross-ref: Sec. 19.3 step 6 (per-site helper contract),
#            Sec. 19.4 row 2 (ORS helper table),
#            Sec. 19.5 (16-col canonical schema),
#            Sec. 19.7 E-13 / E-14 (operator-decision abort).
# ============================================================================


# ---- Sanctioned `...` whitelist for ORS backend ----------------------------
.ORS_SANCTIONED_PASSTHROUGH <- c("attributes", "area_units", "smoothing")


#' OpenRouteService backend dispatch (internal)
#'
#' Per-site dispatch around `openrouteservice::ors_isochrones()` with
#' 3-attempt exponential retry on HTTP 5xx / timeout, batch-abort on HTTP 429
#' (E-13) and 401/403 (E-14), and a row-level `failure_reason` carry-through
#' on retry exhaustion or non-retriable 4xx.
#'
#' @param sites An sf POINT object (EPSG:4326) with a `site_id` column.
#'   Validated upstream by `.normalize_sites_input()` (Step 3.1).
#' @param drive_times Integer-coercible numeric vector of drive times in
#'   minutes. Validated upstream by `.validate_iso_scalar_args()`.
#' @param profile Character(1) routing profile (default `"car"`); the ORS
#'   `profile` argument string (e.g. `"driving-car"`) is derived by
#'   `.ors_profile_string()` from this short alias.
#' @param api_key Character(1) ORS API key. Validated upstream
#'   (`nzchar(api_key)` enforced at dispatch); here we just forward.
#' @param ... Provider-specific passthrough; only members of
#'   `.ORS_SANCTIONED_PASSTHROUGH` are forwarded.
#' @param .on_site_tick Optional function-of-one-arg (`detail`) called after
#'   each completed site's retry chain. v0.2 F1 progress reporter hook;
#'   pass `NULL` (default) when the helper is called outside dispatch.
#' @return A 16-column sf per Sec. 19.5 canonical schema.
#' @keywords internal
#' @noRd
.iso_via_ors <- function(sites, drive_times, profile, api_key = NULL,
                         .on_site_tick = NULL, ...) {
  rlang::check_installed("openrouteservice", reason = "for provider = 'ors'")

  # --- Defence-in-depth: api_key must be non-empty (E-07 also caught at
  # dispatch in Step 3.1; this is the second line, in case .iso_via_ors is
  # called directly by tests or downstream code).
  if (is.null(api_key) || !is.character(api_key) ||
      length(api_key) != 1L || !nzchar(api_key)) {
    .cli_abort_credential(c(
      "{.field provider} = {.val ors} requires a non-empty {.envvar ORS_API_KEY}.",
      "i" = "Set in {.file .Renviron} then restart R, or pass {.arg ors_api_key} explicitly."
    ))
  }

  # --- Sanctioned passthrough whitelist ------------------------------------
  dots <- list(...)
  if (length(dots) > 0L && !is.null(names(dots))) {
    sanctioned <- intersect(names(dots), .ORS_SANCTIONED_PASSTHROUGH)
    pt <- dots[sanctioned]
  } else {
    pt <- list()
  }

  # --- Convert profile alias to ORS profile string -------------------------
  ors_profile <- .ors_profile_string(profile)

  # --- Convert breaks (min -> seconds for ORS range_type = "time") ---------
  breaks_int <- sort(as.integer(unique(drive_times)))
  range_seconds <- breaks_int * 60L

  # --- Per-site retry loop -------------------------------------------------
  n_sites <- nrow(sites)
  per_site <- vector("list", n_sites)
  for (i in seq_len(n_sites)) {
    per_site[[i]] <- .ors_retry_one_site(
      site_row    = sites[i, , drop = FALSE],
      breaks_min  = breaks_int,
      breaks_sec  = range_seconds,
      profile     = ors_profile,
      api_key     = api_key,
      passthrough = pt
    )
    # v0.2 F1: per-site tick (no-op when caller did not pass a reporter).
    if (is.function(.on_site_tick)) {
      tick_detail <- as.character(sites$site_id[[i]])
      tryCatch(.on_site_tick(detail = tick_detail), error = function(e) NULL)
    }
  }

  # --- Batch-abort: E-13 (429) and E-14 (401/403) --------------------------
  http_codes <- vapply(per_site, function(r) r$http_status, integer(1))

  if (any(!is.na(http_codes) & http_codes == 429L)) {
    .cli_abort_operator(c(
      "Rate limit (HTTP 429) from {.field provider} = {.val ors}.",
      "i" = "See {.help cacs_isochrone} Sec. 5.4 Trigger 1: switch provider or upgrade quota.",
      "i" = "ORS free tier has a hard daily/per-minute quota; consider a self-hosted ORS or {.val osrm} fallback."
    ))
  }
  if (any(!is.na(http_codes) & http_codes %in% c(401L, 403L))) {
    bad_status <- http_codes[!is.na(http_codes) & http_codes %in% c(401L, 403L)][[1]]
    .cli_abort_credential(c(
      "Authentication failure (HTTP {.val {bad_status}}) from {.field provider} = {.val ors}.",
      "i" = "Rotate {.envvar ORS_API_KEY} (visit {.url https://openrouteservice.org/dev/}) and restart R."
    ))
  }

  # --- Assemble Sec. 19.5 16-col canonical sf ----------------------------------
  generated_at <- Sys.time()
  engine_version <- tryCatch(
    paste0("openrouteservice-pkg/", utils::packageVersion("openrouteservice")),
    error = function(e) "unknown"
  )

  rows <- list()
  for (i in seq_len(n_sites)) {
    elem <- per_site[[i]]
    site_id_i <- sites$site_id[[i]]
    for (j in seq_along(breaks_int)) {
      dt_j <- breaks_int[[j]]
      geom_j <- elem$geom[[j]]
      empty_j <- isTRUE(elem$empty[[j]]) ||
                 is.null(geom_j) ||
                 length(geom_j) == 0L ||
                 sf::st_is_empty(geom_j)
      fail_j <- if (is.na(elem$failure_reason)) NA_character_ else elem$failure_reason

      g_out <- if (empty_j) sf::st_polygon() else geom_j

      rows[[length(rows) + 1L]] <- tibble::tibble(
        site_id                          = as.character(site_id_i),
        drive_time_min                   = as.integer(dt_j),
        provider                         = "ors",
        profile                          = as.character(profile),
        osm_snapshot_date                = NA_character_,
        routing_engine_version           = engine_version,
        polygon_simplification_tolerance = NA_real_,
        generated_at                     = generated_at,
        isochrone_empty                  = empty_j,
        provider_requested               = "ors",
        provider_downgrade               = FALSE,
        osm_snapshot_status              = NA_character_,
        failure_reason                   = fail_j,
        retry_count                      = as.integer(elem$attempts),
        ring_topology                    = "cumulative",
        geom                             = list(g_out)
      )
    }
  }

  out_tbl <- dplyr::bind_rows(rows)
  geom_sfc <- sf::st_sfc(out_tbl$geom, crs = 4326)
  out_tbl$geom <- NULL
  out_sf <- sf::st_sf(out_tbl, geometry = geom_sfc)

  # Order columns per Sec. 19.5 (geometry inserted at position 3)
  ord <- c("site_id", "drive_time_min", "geometry", "provider", "profile",
           "osm_snapshot_date", "routing_engine_version",
           "polygon_simplification_tolerance", "generated_at",
           "isochrone_empty", "provider_requested", "provider_downgrade",
           "osm_snapshot_status", "failure_reason", "retry_count",
           "ring_topology")
  out_sf <- out_sf[, ord]

  # Aggregate W-09 / W-10 soft warnings (Sec. 19.7)
  n_empty <- sum(out_sf$isochrone_empty, na.rm = TRUE)
  n_fail  <- sum(!is.na(out_sf$failure_reason))
  if (n_empty > 0L) {
    .cli_warn_runtime(c(
      "{n_empty} empty isochrone{?s} produced by {.field provider} = {.val ors}.",
      "i" = "See {.field isochrone_empty} column for row-level masking."
    ))
  }
  if (n_fail > 0L) {
    .cli_warn_runtime(c(
      "{n_fail} site-row{?s} failed after retry; see {.field failure_reason}.",
      "i" = "Batch continued; downstream {.fn cacs_intersect_weight} will mark these EMPTY."
    ))
  }

  out_sf
}


#' Internal: 3-attempt exponential retry for one ORS site
#'
#' @param site_row 1-row sf POINT in EPSG:4326.
#' @param breaks_min Integer minute breaks (sorted, unique); used to align
#'   ORS response polygons (whose `value` property is in seconds) back to the
#'   caller's minute scale.
#' @param breaks_sec Integer second breaks (= `breaks_min * 60`); the
#'   `range` argument forwarded to `openrouteservice::ors_isochrones()`.
#' @param profile ORS profile string (e.g. `"driving-car"`).
#' @param api_key ORS API key.
#' @param passthrough Named list of sanctioned `...` keys.
#' @return A list with elements
#'   \describe{
#'     \item{geom}{`list` of length `length(breaks_min)`; each element is an
#'       `sfg` polygon (possibly empty) or `NULL` if the attempt failed.}
#'     \item{empty}{logical vector, length `length(breaks_min)`, TRUE when
#'       the polygon is empty.}
#'     \item{attempts}{integer count of attempts made (1, 2, or 3).}
#'     \item{http_status}{integer status (200 on success; 4xx/5xx on error;
#'       `NA_integer_` when no status could be parsed).}
#'     \item{failure_reason}{character condition message, `NA_character_`
#'       on success.}
#'   }
#' @keywords internal
#' @noRd
.ors_retry_one_site <- function(site_row, breaks_min, breaks_sec,
                                profile, api_key, passthrough = list()) {
  max_attempts <- 3L
  backoff <- c(1, 2, 4)   # seconds; index = attempt completed

  # Pre-compute coords once (ORS expects c(lon, lat)).
  coords <- sf::st_coordinates(site_row)[1L, c("X", "Y"), drop = TRUE]
  loc_vec <- c(coords[["X"]], coords[["Y"]])

  last_out <- list(
    geom = vector("list", length(breaks_min)),
    empty = rep(TRUE, length(breaks_min)),
    attempts = 0L,
    http_status = NA_integer_,
    failure_reason = "no_attempt_made"
  )

  for (attempt in seq_len(max_attempts)) {
    out <- tryCatch(
      {
        iso_obj <- .ors_call_isochrones(
          locations   = loc_vec,
          profile     = profile,
          range       = breaks_sec,
          api_key     = api_key,
          passthrough = passthrough
        )
        .ors_parse_response(iso_obj, breaks_min, attempt)
      },
      error = function(e) {
        msg <- conditionMessage(e)
        list(
          geom = vector("list", length(breaks_min)),
          empty = rep(TRUE, length(breaks_min)),
          attempts = attempt,
          http_status = .extract_http_status(msg),
          failure_reason = msg
        )
      }
    )

    last_out <- out

    # Success path: stop retrying.
    if (is.na(out$failure_reason)) {
      return(out)
    }

    # Batch-abort statuses (429/401/403): stop immediately so caller raises
    # the operator/credential abort.
    if (!is.na(out$http_status) &&
        out$http_status %in% c(401L, 403L, 429L)) {
      return(out)
    }

    # Non-retriable 4xx (other than 401/403/429): stop, record failure.
    if (!is.na(out$http_status) &&
        out$http_status >= 400L && out$http_status < 500L) {
      return(out)
    }

    # Otherwise: retriable (5xx, NA, timeout) - back off and try again.
    if (attempt < max_attempts) {
      Sys.sleep(backoff[[attempt]])
    }
  }

  last_out
}


#' Internal: thin wrapper around `openrouteservice::ors_isochrones()`
#' (testable seam).
#'
#' Existence of this wrapper lets tests use
#' `testthat::local_mocked_bindings(.ors_call_isochrones = ..., .package = "catchmentACS")`
#' instead of having to monkey-patch `openrouteservice::ors_isochrones`
#' directly (which would require `.package = "openrouteservice"` and the
#' dependency loaded).
#'
#' @keywords internal
#' @noRd
.ors_call_isochrones <- function(locations, profile, range,
                                 api_key, passthrough = list()) {
  args <- list(
    locations  = list(locations),  # ORS expects list-of-coordinate pairs
    profile    = profile,
    range      = range,
    range_type = "time",
    api_key    = api_key
  )
  for (nm in names(passthrough)) {
    args[[nm]] <- passthrough[[nm]]
  }
  do.call(openrouteservice::ors_isochrones, args)
}


#' Internal: normalize raw `openrouteservice::ors_isochrones()` response to
#' the per-site list shape consumed by `.iso_via_ors()`.
#'
#' ORS returns a GeoJSON FeatureCollection (`sf` after package conversion)
#' with one feature per (location, range) pair. The `value` property holds
#' the cutoff in *seconds* when `range_type = "time"`; we divide by 60 to
#' align with the caller's minute scale.
#'
#' @param iso_obj Either an `sf` data frame, a list with a `features` element,
#'   or a GeoJSON-like list - whatever `openrouteservice::ors_isochrones()`
#'   returned. We try the sf path first.
#' @param breaks_min Integer minute breaks expected by the caller.
#' @param attempt Integer attempt count to record in the return value.
#' @keywords internal
#' @noRd
.ors_parse_response <- function(iso_obj, breaks_min, attempt) {
  if (is.null(iso_obj)) {
    return(list(
      geom = vector("list", length(breaks_min)),
      empty = rep(TRUE, length(breaks_min)),
      attempts = attempt,
      http_status = 200L,
      failure_reason = "empty_or_invalid_ors_response"
    ))
  }

  # Path 1: sf-style return (preferred - openrouteservice >= 0.5 supports
  # `output = "sf"` or returns sf directly when CRS is present).
  if (inherits(iso_obj, "sf")) {
    geoms <- sf::st_geometry(iso_obj)
    value_col <- if ("value" %in% names(iso_obj)) {
      as.integer(iso_obj$value)
    } else {
      NULL
    }
  } else if (is.list(iso_obj) && !is.null(iso_obj$features)) {
    # Path 2: raw GeoJSON-as-list - extract via sf::st_read on the JSON.
    parsed <- tryCatch(
      sf::st_read(jsonlite_toJSON_safe(iso_obj), quiet = TRUE),
      error = function(e) NULL
    )
    if (is.null(parsed)) {
      return(list(
        geom = vector("list", length(breaks_min)),
        empty = rep(TRUE, length(breaks_min)),
        attempts = attempt,
        http_status = 200L,
        failure_reason = "ors_response_parse_failed"
      ))
    }
    geoms <- sf::st_geometry(parsed)
    value_col <- if ("value" %in% names(parsed)) as.integer(parsed$value) else NULL
  } else {
    return(list(
      geom = vector("list", length(breaks_min)),
      empty = rep(TRUE, length(breaks_min)),
      attempts = attempt,
      http_status = 200L,
      failure_reason = "ors_response_unrecognized_shape"
    ))
  }

  # ORS `value` is in seconds for range_type = "time"; convert to minutes
  # for alignment with `breaks_min`.
  value_min <- if (!is.null(value_col)) as.integer(value_col / 60L) else NULL

  geom_list <- vector("list", length(breaks_min))
  empty_flag <- rep(TRUE, length(breaks_min))

  for (j in seq_along(breaks_min)) {
    b <- breaks_min[[j]]
    idx <- if (!is.null(value_min)) {
      which(value_min == b)[1L]
    } else if (length(geoms) >= j) {
      j
    } else {
      NA_integer_
    }

    if (!is.na(idx) && idx >= 1L && idx <= length(geoms)) {
      g <- geoms[[idx]]
      if (!is.null(g) && !sf::st_is_empty(g)) {
        geom_list[[j]] <- g
        empty_flag[[j]] <- FALSE
      }
    }
  }

  list(
    geom = geom_list,
    empty = empty_flag,
    attempts = attempt,
    http_status = 200L,
    failure_reason = NA_character_
  )
}


#' Internal: convert short profile alias (e.g. `"car"`, `"bike"`) to ORS
#' profile string (`"driving-car"`, `"cycling-regular"`, ...).
#'
#' Pass-through if the alias already matches a canonical ORS profile.
#'
#' @keywords internal
#' @noRd
.ors_profile_string <- function(profile) {
  alias_map <- c(
    "car"  = "driving-car",
    "hgv"  = "driving-hgv",
    "bike" = "cycling-regular",
    "foot" = "foot-walking",
    "walk" = "foot-walking",
    "wheelchair" = "wheelchair"
  )
  canonical <- c(alias_map,
                 "driving-car" = "driving-car",
                 "driving-hgv" = "driving-hgv",
                 "cycling-regular" = "cycling-regular",
                 "cycling-road" = "cycling-road",
                 "cycling-mountain" = "cycling-mountain",
                 "cycling-electric" = "cycling-electric",
                 "foot-walking" = "foot-walking",
                 "foot-hiking" = "foot-hiking")
  out <- canonical[[profile]] %||% NULL
  if (is.null(out)) {
    # Defer to ORS server-side validation; we only normalize the obvious
    # short forms above and pass anything else through verbatim.
    return(profile)
  }
  out
}


# Local %||% - duplicated from isochrone-dispatch.R to keep file standalone.
`%||%` <- function(x, y) if (is.null(x)) y else x


# Defensive jsonlite stub so test fixtures without jsonlite still parse
# the sf-fast path. Real ORS responses parsed via sf path above.
jsonlite_toJSON_safe <- function(x) {
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::toJSON(x, auto_unbox = TRUE, force = TRUE)
  } else {
    stop("Cannot parse ORS list response without jsonlite; install jsonlite or upgrade openrouteservice to >= 0.5")
  }
}
