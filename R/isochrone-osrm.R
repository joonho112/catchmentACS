# ============================================================================
# isochrone-osrm.R - .iso_via_osrm() body (Step 3.2 of Phase 3).
#
# Implements Sec. 19.4 row 1 (OSRM backend):
#   - Sanctioned `...` keys whitelist:
#     c("osrm.server", "osrm.profile", "osrm.profile_name", "res")
#   - Retry chain: 3 attempts, exponential backoff 1s / 2s / 4s
#     * HTTP 5xx / timeout / NA status     -> retry (transient)
#     * HTTP 4xx (other than 401/403/429)  -> no retry, row-level failure
#     * HTTP 429                           -> batch abort E-13 (operator)
#     * HTTP 401 / 403                     -> batch abort E-14 (operator)
#   - Returns the 16-col canonical sf per Sec. 19.5 directly so that the
#     placeholder dispatch in Step 3.1 (which does `out <- switch(...); out`)
#     continues to receive an sf object end-to-end. Step 3.4 will route
#     through `.normalize_iso_schema()` and refactor the helper to return
#     the raw per-site list described in Sec. 19.3 step 6; the 16-col layer
#     coded here is forward-compatible (identical column names + types).
#
# Cross-ref: Sec. 19.3 step 6 (per-site helper contract),
#            Sec. 19.4 row 1 (OSRM helper table),
#            Sec. 19.5 (16-col canonical schema),
#            Sec. 19.7 E-13 / E-14 (operator-decision abort).
# ============================================================================


# ---- Sanctioned `...` whitelist for OSRM backend ---------------------------
.OSRM_SANCTIONED_PASSTHROUGH <- c(
  "osrm.server", "osrm.profile", "osrm.profile_name", "res"
)


# Exact upstream `n` values supported by osrm 5.x. Only these can avoid the
# `res` deprecation message without changing the effective grid resolution.
.OSRM_RES_TO_N <- c(
  "13" = 100L,
  "18" = 200L,
  "27" = 500L,
  "37" = 1000L,
  "52" = 2000L,
  "81" = 5000L,
  "114" = 10000L,
  "161" = 20000L,
  "254" = 50000L
)


# ----------------------------------------------------------------------------
# v0.4 Issue 002 Layer 1 helper: dynamic res-downgrade decision (pure function).
#
# Decision table (4 inputs × outputs):
#
#   res_param   osrm_mode   options_protect   ->  res_effective  downgraded  emit_msg
#   NULL        "demo"      TRUE              ->  30L            TRUE        TRUE
#   NULL        "demo"      FALSE             ->  70L            FALSE       FALSE
#   NULL        "docker"    *                 ->  70L            FALSE       FALSE
#   <integer>   *           *                 ->  <integer>      FALSE       FALSE
#
# Pure function: option lookup happens OUTSIDE (caller passes
# `options_protect = getOption("catchmentACS.osrm_demo_budget_protect", TRUE)`).
# The function does not emit — the caller is responsible for invoking
# `.cli_inform_demo_budget_protected()` when `emit_msg = TRUE`.
# ----------------------------------------------------------------------------


#' Resolve OSRM omitted-res default with v0.4 demo-budget protection
#'
#' v0.4 Issue 002 Layer 1 helper.
#' @keywords internal
#' @noRd
.cacs_osrm_resolve_demo_res <- function(res_param,
                                        osrm_mode = c("demo", "docker"),
                                        options_protect = TRUE) {

  osrm_mode <- match.arg(osrm_mode)
  if (!is.logical(options_protect) || length(options_protect) != 1L ||
      is.na(options_protect)) {
    stop(".cacs_osrm_resolve_demo_res(): `options_protect` must be a non-NA logical scalar.",
         call. = FALSE)
  }

  # Explicit res from user → passthrough (highest precedence).
  if (!is.null(res_param)) {
    return(list(
      res_effective = as.integer(res_param),
      downgraded    = FALSE,
      emit_msg      = FALSE,
      msg_text      = NULL
    ))
  }

  # Omitted res. Branch on osrm_mode + options_protect.
  if (identical(osrm_mode, "demo") && isTRUE(options_protect)) {
    return(list(
      res_effective = .OSRM_RES_DEFAULT_DEMO,   # 30L
      downgraded    = TRUE,
      emit_msg      = TRUE,
      msg_text      = "OSRM omitted-`res` downgraded to 30L on public demo to avoid HTTP 429."
    ))
  }

  # Omitted res + (demo without protect, or docker) → v0.3 70L default.
  list(
    res_effective = .OSRM_RES_DEFAULT,   # 70L
    downgraded    = FALSE,
    emit_msg      = FALSE,
    msg_text      = NULL
  )
}


#' OSRM backend dispatch (internal)
#'
#' Per-site dispatch around `osrm::osrmIsochrone()` with 3-attempt exponential
#' retry on HTTP 5xx / timeout, batch-abort on HTTP 429 (E-13) and 401/403
#' (E-14), and a row-level `failure_reason` carry-through on retry exhaustion
#' or non-retriable 4xx.
#'
#' @param sites An sf POINT object (EPSG:4326) with a `site_id` column.
#'   Validated upstream by `.normalize_sites_input()` (Step 3.1).
#' @param drive_times Integer-coercible numeric vector of drive times in
#'   minutes. Validated upstream by `.validate_iso_scalar_args()`.
#' @param profile Character(1) routing profile (default `"car"`).
#' @param osrm_mode `"demo"` or `"docker"` (default `"demo"`); informational
#'   carry-through (driving the OSRM server choice happens through the
#'   `osrm.server` option, which the user can supply via `...`).
#' @param ... Provider-specific passthrough; only members of
#'   `.OSRM_SANCTIONED_PASSTHROUGH` are forwarded.
#' @param .on_site_tick Optional function-of-one-arg (`detail`) called after
#'   each completed site's retry chain. v0.2 F1 progress reporter hook; pass
#'   `NULL` (default) when the helper is called outside dispatch.
#' @return A 16-column sf per Sec. 19.5 canonical schema.
#' @keywords internal
#' @noRd
.iso_via_osrm <- function(sites, drive_times, profile,
                          osrm_mode = c("demo", "docker"),
                          .on_site_tick = NULL, ...) {
  osrm_mode <- match.arg(osrm_mode)
  rlang::check_installed("osrm", reason = "for provider = 'osrm'")

  # --- Sanctioned passthrough whitelist ------------------------------------
  dots <- list(...)
  if (length(dots) > 0L && !is.null(names(dots))) {
    sanctioned <- intersect(names(dots), .OSRM_SANCTIONED_PASSTHROUGH)
    pt <- dots[sanctioned]
  } else {
    pt <- list()
  }

  if ("osrm.profile_name" %in% names(pt) && !"osrm.profile" %in% names(pt)) {
    pt$osrm.profile <- pt$osrm.profile_name
  }

  # --- Apply transient `osrm.server` / `osrm.profile` options --------------
  if ("osrm.server" %in% names(pt)) {
    old_server <- getOption("osrm.server")
    options(osrm.server = pt$osrm.server)
    on.exit(options(osrm.server = old_server), add = TRUE)
  }
  if ("osrm.profile" %in% names(pt)) {
    old_profile <- getOption("osrm.profile")
    options(osrm.profile = pt$osrm.profile)
    on.exit(options(osrm.profile = old_profile), add = TRUE)
  }

  # v0.4 Issue 002: dynamic res-downgrade for public OSRM demo. Pure-function
  # decision in `.cacs_osrm_resolve_demo_res()` (Layer 1 helper, Step 2.3).
  # Caller-level emit gated by once_per_session via the cli helper.
  .osrm_res_resolved <- .cacs_osrm_resolve_demo_res(
    res_param       = if ("res" %in% names(pt)) pt$res else NULL,
    osrm_mode       = osrm_mode,
    options_protect = getOption(
      "catchmentACS.osrm_demo_budget_protect", TRUE
    )
  )
  res_param <- .osrm_res_resolved$res_effective
  if (isTRUE(.osrm_res_resolved$emit_msg)) {
    .cli_inform_demo_budget_protected()
  }

  # --- Per-site retry loop -------------------------------------------------
  breaks_int <- sort(as.integer(unique(drive_times)))
  n_sites <- nrow(sites)
  per_site <- vector("list", n_sites)
  for (i in seq_len(n_sites)) {
    per_site[[i]] <- .osrm_retry_one_site(
      site_row = sites[i, , drop = FALSE],
      breaks   = breaks_int,
      res      = res_param
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
    # v0.4 Issue 002: enriched 429 hint listing 3 remediation paths in order.
    .cli_abort_operator(c(
      "Rate limit (HTTP 429) from {.field provider} = {.val osrm}.",
      "i" = "Three ways to recover:",
      "*" = "Lower {.arg res}: try {.code iso_args = list(res = 30L)} (lightest OSRM table budget).",
      "*" = "Switch to local OSRM: {.code osrm_mode = \"docker\"} (no public-demo quota cost).",
      "*" = "Switch provider: {.code provider = \"ors\"} (requires {.envvar ORS_API_KEY}).",
      "i" = "See {.help cacs_isochrone} Sec. 5.4 Trigger 1 for the full triage."
    ))
  }
  if (any(!is.na(http_codes) & http_codes %in% c(401L, 403L))) {
    bad_status <- http_codes[!is.na(http_codes) & http_codes %in% c(401L, 403L)][[1]]
    .cli_abort_credential(c(
      "Authentication failure (HTTP {.val {bad_status}}) from {.field provider} = {.val osrm}.",
      "i" = "OSRM demo server requires no auth; check the {.code osrm.server} option.",
      "i" = "If using a private OSRM instance, rotate credentials and restart R."
    ))
  }

  # --- Assemble Sec. 19.5 16-col canonical sf ----------------------------------
  generated_at <- Sys.time()
  engine_version <- tryCatch(
    paste0("osrm-pkg/", utils::packageVersion("osrm")),
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
        provider                         = "osrm",
        profile                          = as.character(profile),
        osm_snapshot_date                = NA_character_,
        routing_engine_version           = engine_version,
        polygon_simplification_tolerance = NA_real_,
        generated_at                     = generated_at,
        isochrone_empty                  = empty_j,
        provider_requested               = "osrm",
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

  # Aggregate W-09 / W-10 soft warnings (Sec. 19.7) -- v0.4 Step 4.2 below
  # adds an exported pre-flight helper `cacs_validate_osrm_endpoint()` that
  # tests the public-demo quota *before* the heavy isochrone path. This
  # block below stays at the v0.3 location; the pre-flight helper sits
  # at the bottom of this file.
  n_empty <- sum(out_sf$isochrone_empty, na.rm = TRUE)
  n_fail  <- sum(!is.na(out_sf$failure_reason))
  if (n_empty > 0L) {
    .cli_warn_runtime(c(
      "{n_empty} empty isochrone{?s} produced by {.field provider} = {.val osrm}.",
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


#' Internal: 3-attempt exponential retry for one OSRM site
#'
#' @param site_row 1-row sf POINT in EPSG:4326.
#' @param breaks Integer minute breaks (sorted, unique).
#' @param res Optional public `res` value. Public `cacs_isochrone()` calls
#'   resolve omitted `res` before reaching here; `.osrm_call_isochrone()` then
#'   forwards it to upstream `osrm` as `n` only when the mapping is exact.
#' @return A list with elements
#'   \describe{
#'     \item{geom}{`list` of length `length(breaks)`; each element is an
#'       `sfg` polygon (possibly empty) or `NULL` if the attempt failed.}
#'     \item{empty}{logical vector, length `length(breaks)`, TRUE when the
#'       polygon is empty.}
#'     \item{attempts}{integer count of attempts made (1, 2, or 3).}
#'     \item{http_status}{integer status (200 on success; 4xx/5xx on error;
#'       `NA_integer_` when no status could be parsed).}
#'     \item{failure_reason}{character condition message, `NA_character_`
#'       on success.}
#'   }
#' @keywords internal
#' @noRd
.osrm_retry_one_site <- function(site_row, breaks, res = NULL) {
  max_attempts <- 3L
  backoff <- c(1, 2, 4)   # seconds; index = attempt completed

  # Pre-compute coords once.
  coords <- sf::st_coordinates(site_row)[1L, c("X", "Y"), drop = TRUE]
  loc_vec <- c(coords[["X"]], coords[["Y"]])

  last_out <- list(
    geom = vector("list", length(breaks)),
    empty = rep(TRUE, length(breaks)),
    attempts = 0L,
    http_status = NA_integer_,
    failure_reason = "no_attempt_made"
  )

  for (attempt in seq_len(max_attempts)) {
    out <- tryCatch(
      {
        iso_sf <- .osrm_call_isochrone(
          loc = loc_vec,
          breaks = breaks,
          res = res
        )
        .osrm_parse_response(iso_sf, breaks, attempt)
      },
      error = function(e) {
        msg <- conditionMessage(e)
        list(
          geom = vector("list", length(breaks)),
          empty = rep(TRUE, length(breaks)),
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


#' Internal: thin wrapper around `osrm::osrmIsochrone()` (testable seam).
#'
#' Existence of this wrapper lets tests use
#' `testthat::local_mocked_bindings(.osrm_call_isochrone = ..., .package = "catchmentACS")`
#' instead of having to monkey-patch `osrm::osrmIsochrone` directly (which
#' would require `.package = "osrm"` and the dependency loaded).
#'
#' @keywords internal
#' @noRd
.osrm_call_isochrone <- function(loc, breaks, res = NULL) {
  args <- c(
    list(
      loc = loc,
      breaks = breaks
    ),
    .cacs_osrm_resolution_args(res)
  )
  do.call(osrm::osrmIsochrone, args)
}


#' Internal: build version-safe OSRM resolution arguments
#'
#' catchmentACS keeps `res` as its public argument in v0.4.x. Newer `osrm`
#' releases prefer `n`, but only a small fixed set of `n` values map exactly
#' back to the old `res` grid sizes. To avoid silently changing polygon budgets,
#' this helper forwards `n` only on exact mappings and otherwise preserves
#' `res`.
#'
#' @keywords internal
#' @noRd
.cacs_osrm_resolution_args <- function(res = NULL) {
  if (is.null(res)) {
    return(list())
  }

  supports_n <- requireNamespace("osrm", quietly = TRUE) &&
    "n" %in% names(formals(osrm::osrmIsochrone))
  if (!isTRUE(supports_n)) {
    return(list(res = res))
  }

  res_num <- suppressWarnings(as.numeric(res))
  if (length(res_num) == 1L && is.finite(res_num) &&
      identical(res_num, as.numeric(as.integer(res_num)))) {
    key <- as.character(as.integer(res_num))
    if (key %in% names(.OSRM_RES_TO_N)) {
      return(list(n = unname(.OSRM_RES_TO_N[[key]])))
    }
  }

  list(res = res)
}


#' Internal: estimate the request shape used by `osrm::osrmIsochrone()`.
#'
#' Mirrors the installed `osrm` package's grid/chunk logic so catchmentACS can
#' expose the hidden public-demo forced-sleep cost without depending on a live
#' OSRM call.
#'
#' @keywords internal
#' @noRd
.osrm_request_budget <- function(res, breaks, osrm_server) {
  res <- suppressWarnings(as.integer(res))
  if (length(res) != 1L || is.na(res) || res <= 0L) {
    res <- .OSRM_RES_DEFAULT
  }
  breaks <- suppressWarnings(as.integer(breaks))
  breaks <- breaks[is.finite(breaks)]
  tmax <- if (length(breaks) == 0L) NA_integer_ else max(breaks)

  server <- .normalize_osrm_server(osrm_server)
  public_demo <- identical(server, .OSRM_PUBLIC_DEMO_SERVER)
  grid_points <- res * res
  chunk_size <- if (public_demo) {
    .OSRM_PUBLIC_DEMO_CHUNK_SIZE
  } else {
    .OSRM_CUSTOM_SERVER_CHUNK_SIZE
  }
  full_chunks <- grid_points %/% chunk_size
  remainder <- grid_points %% chunk_size
  table_calls <- full_chunks + ifelse(remainder > 0L, 1L, 0L)
  sleep_floor <- if (public_demo) full_chunks else 0L
  public_demo_full_chunks <- grid_points %/% .OSRM_PUBLIC_DEMO_CHUNK_SIZE
  public_demo_remainder <- grid_points %% .OSRM_PUBLIC_DEMO_CHUNK_SIZE

  list(
    res = res,
    tmax_min = tmax,
    grid_points = grid_points,
    chunk_size = as.integer(chunk_size),
    table_calls = as.integer(table_calls),
    public_demo = public_demo,
    sleep_floor_sec_per_site = as.integer(sleep_floor),
    public_demo_table_calls_per_site = as.integer(
      public_demo_full_chunks + ifelse(public_demo_remainder > 0L, 1L, 0L)
    ),
    public_demo_sleep_floor_sec_per_site = as.integer(public_demo_full_chunks)
  )
}


#' Internal: normalize raw `osrm::osrmIsochrone()` response to the
#' per-site list shape consumed by `.iso_via_osrm()`.
#'
#' OSRM returns one row per (isomin, isomax) break band - i.e. one row per
#' drive-time threshold. We extract the *outer* polygon for each break and
#' produce one list element per break.
#'
#' @keywords internal
#' @noRd
.osrm_parse_response <- function(iso_sf, breaks, attempt) {
  if (is.null(iso_sf) || !inherits(iso_sf, c("sf", "sfc"))) {
    return(list(
      geom = vector("list", length(breaks)),
      empty = rep(TRUE, length(breaks)),
      attempts = attempt,
      http_status = 200L,
      failure_reason = "empty_or_invalid_osrm_response"
    ))
  }

  if (inherits(iso_sf, "sf") &&
      all(c("isomin", "isomax") %in% names(iso_sf))) {
    isomin_num <- suppressWarnings(as.numeric(iso_sf$isomin))
    if (any(isomin_num > 0, na.rm = TRUE)) {
      iso_sf$site_id <- "osrm_response"
      iso_sf <- cacs_rings_to_cumulative(iso_sf, drive_times = breaks)
    }
  }

  # Standardize: extract geometry, prefer matching by `isomax` if available.
  geom_list <- vector("list", length(breaks))
  empty_flag <- rep(TRUE, length(breaks))

  geoms <- if (inherits(iso_sf, "sf")) sf::st_geometry(iso_sf) else iso_sf
  isomax <- if (inherits(iso_sf, "sf") && "isomax" %in% names(iso_sf)) {
    as.integer(iso_sf$isomax)
  } else {
    NULL
  }

  for (j in seq_along(breaks)) {
    b <- breaks[[j]]
    idx <- if (!is.null(isomax)) {
      which(isomax == b)[1L]
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


#' Internal: parse an HTTP status code out of an error condition message.
#'
#' OSRM (and downstream `httr`) errors usually embed a 3-digit status code in
#' the message string ("HTTP 503 ...", "status 429", "client error (429)").
#' We extract the first 3-digit token that looks like an HTTP status.
#'
#' @keywords internal
#' @noRd
.extract_http_status <- function(msg) {
  if (is.null(msg) || !is.character(msg) || length(msg) == 0L) {
    return(NA_integer_)
  }
  m <- regmatches(msg, regexpr("(?<![0-9])[1-5][0-9]{2}(?![0-9])", msg, perl = TRUE))
  if (length(m) == 0L || !nzchar(m)) return(NA_integer_)
  as.integer(m)
}


#' @keywords internal
#' @noRd
.cacs_osrm_probe_profile <- function(profile = "car") {
  profile <- as.character(profile %||% "car")
  if (length(profile) != 1L || is.na(profile) || !nzchar(profile)) {
    profile <- "car"
  }
  switch(profile,
    car = "car",
    driving = "car",
    bike = "bike",
    bicycle = "bike",
    foot = "foot",
    walk = "foot",
    walking = "foot",
    profile
  )
}


#' @keywords internal
#' @noRd
.cacs_osrm_public_demo_prefix <- function(profile = "car") {
  osrm_profile <- .cacs_osrm_probe_profile(profile)
  switch(osrm_profile,
    car = "routed-car",
    bike = "routed-bike",
    foot = "routed-foot",
    paste0("routed-", osrm_profile)
  )
}


#' @keywords internal
#' @noRd
.cacs_osrm_probe_url <- function(server = NULL, profile = "car") {
  if (is.null(server) || !nzchar(server)) {
    server <- getOption("osrm.server", .OSRM_PUBLIC_DEMO_SERVER)
  }
  base <- sub("/+$", "", server)
  route_path <- "route/v1/driving/-86.8,33.5;-86.7,33.4"

  # routing.openstreetmap.de exposes OSRM behind profile-prefixed services
  # (/routed-car, /routed-bike, /routed-foot). Self-hosted OSRM generally
  # exposes /route/v1 directly, so leave custom hosts untouched.
  if (grepl("^https?://routing[.]openstreetmap[.]de(/|$)", base)) {
    if (grepl("/routed-[^/]+$", base)) {
      return(paste0(base, "/", route_path))
    }
    prefix <- .cacs_osrm_public_demo_prefix(profile)
    return(paste0(base, "/", prefix, "/", route_path))
  }

  paste0(base, "/", route_path)
}


# ============================================================================
# v0.4 Step 4.2 — exported pre-flight quota probe.
# ============================================================================


#' Pre-flight check of an OSRM endpoint's current quota state
#'
#' Sends a small, lightweight `/route` ping that tests whether an OSRM routing
#' endpoint is currently accepting requests, before the much heavier
#' [cacs_isochrone()] table-API path. Use it when you are about to launch a
#' large batch (for example 50 or more sites at a high `res` grid resolution)
#' and want to fail fast on an unreachable endpoint or an exhausted request
#' quota. For the public `routing.openstreetmap.de` demo server, the probe
#' expands the configured base server to the profile-prefixed service path
#' (for example `/routed-car/route/v1/driving` for the default car profile).
#' Custom or self-hosted OSRM endpoints are probed at `/route/v1/driving`.
#'
#' On HTTP 429 (rate limit) the function emits a classed warning
#' (`catchmentACS_warning_provider_quota_exhausted`) and returns
#' `quota_ok = FALSE`. Other non-200 HTTP statuses also return
#' `quota_ok = FALSE` but do not imply quota exhaustion. You can then inspect
#' `http_status`, switch to `osrm_mode = "docker"`, or call [cacs_isochrone()]
#' directly and rely on its provider error message. This function never throws
#' an error, even on quota exhaustion: it is a probe, not a gate.
#'
#' @param server Character scalar: the OSRM endpoint URL. Defaults to the
#'   current `getOption("osrm.server")`, or to the package's built-in
#'   public-demo server if that option is unset.
#' @param timeout Numeric scalar: HTTP timeout in seconds (default `5`).
#'
#' @return A 1-row tibble with columns:
#'   - `endpoint` (character) — the configured endpoint/base server used to
#'     build the probe URL.
#'   - `quota_ok` (logical) — `TRUE` if the probe request returned HTTP 200,
#'     `FALSE` on HTTP 429 or any other error.
#'   - `response_ms` (numeric) — wall-clock milliseconds of the probe.
#'   - `http_status` (integer) — HTTP status code, or `NA_integer_` on
#'     connection failure.
#'
#' @section Side effects:
#' On HTTP 429, emits `catchmentACS_warning_provider_quota_exhausted` —
#' suppressible by handling the class via `tryCatch()` or
#' `withCallingHandlers()`. Other HTTP statuses or connection errors do
#' NOT emit a warning; inspect `http_status` and `quota_ok`.
#'
#' @examples
#' \dontrun{
#' # Quick quota check before a batch run.
#' state <- cacs_validate_osrm_endpoint()
#' if (state$quota_ok) {
#'   iso <- cacs_isochrone(sites_sf, 10L, "osrm")
#' } else if (identical(state$http_status, 429L)) {
#'   message("OSRM demo currently saturated; switching to docker.")
#'   iso <- cacs_isochrone(sites_sf, 10L, "osrm", osrm_mode = "docker")
#' } else {
#'   stop("OSRM probe failed; inspect state$http_status before routing.")
#' }
#' }
#' @seealso [cacs_isochrone()] for the isochrone builder this probe guards,
#'   and [cacs_validate_iso()] for validating an isochrone object's schema.
#' @family validation and conditions
#' @export
cacs_validate_osrm_endpoint <- function(server = NULL, timeout = 5) {
  if (is.null(server) || !nzchar(server)) {
    server <- getOption("osrm.server", .OSRM_PUBLIC_DEMO_SERVER)
  }
  if (!is.numeric(timeout) || length(timeout) != 1L || is.na(timeout) ||
      timeout <= 0) {
    stop("`timeout` must be a positive numeric scalar.", call. = FALSE)
  }
  # Build a tiny /route request between two near-by coordinates. /route is
  # the lightest OSRM endpoint and a fast indicator of overall service
  # health + quota state.
  url <- .cacs_osrm_probe_url(
    server = server,
    profile = getOption("osrm.profile", "car")
  )

  t0 <- Sys.time()
  req <- tryCatch({
    httr2::req_perform(
      httr2::req_error(
        httr2::req_timeout(httr2::request(url), timeout),
        is_error = function(resp) FALSE
      )
    )
  }, error = function(e) e)

  elapsed_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000

  if (inherits(req, "error")) {
    # Connection failure / DNS / timeout
    return(tibble::tibble(
      endpoint    = server,
      quota_ok    = FALSE,
      response_ms = elapsed_ms,
      http_status = NA_integer_
    ))
  }

  status <- as.integer(httr2::resp_status(req))
  quota_ok <- isTRUE(status == 200L)

  if (isTRUE(status == 429L)) {
    .cli_warn_provider_quota_exhausted(c(
      "OSRM endpoint {.url {server}} returned HTTP 429 (quota exhausted).",
      "i" = "Switch to {.code osrm_mode = \"docker\"} (local OSRM) or wait + retry.",
      "i" = "Lower {.arg res} (e.g., {.code res = 30L}) can also help on subsequent calls."
    ))
  }

  tibble::tibble(
    endpoint    = server,
    quota_ok    = quota_ok,
    response_ms = elapsed_ms,
    http_status = status
  )
}
