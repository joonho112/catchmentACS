# isochrone-ors.R - .iso_via_ors(), the openrouteservice helper called by
# cacs_isochrone(). It works site by site, like the OSRM helper in
# R/isochrone-osrm.R. The retry policy and the returned columns are the same;
# the reply format is what differs.
#
#   - Each site is tried up to three times, with a pause of 1 second after
#     the first attempt and 2 after the second. An HTTP 5xx, a timeout, or a
#     reply with no status is tried again; any other HTTP 4xx is recorded in
#     `failure_reason` and not tried again; HTTP 429 and HTTP 401 or 403 stop
#     the whole call.
#   - Unlike OSRM, openrouteservice answers with a GeoJSON feature collection
#     whose `value` property is in seconds when `range_type = "time"`, so the
#     values are divided by 60 to line up with the drive times in minutes.
#   - It returns the 16 columns of `.ISO_CANONICAL_COLS` itself, in that
#     order; `.normalize_iso_schema()` then fills in the two snapshot columns.


# The `...` arguments cacs_isochrone() forwards to openrouteservice.
.ORS_SANCTIONED_PASSTHROUGH <- c("attributes", "area_units", "smoothing")


#' Build the drive-time areas with openrouteservice
#'
#' Calls `openrouteservice::ors_isochrones()` once per site, with the retries
#' described at the top of the file. A site that fails keeps its rows, with
#' the message in `failure_reason`.
#'
#' @param sites An sf POINT object (EPSG:4326) with a `site_id` column,
#'   already checked by `.normalize_sites_input()`.
#' @param drive_times Numeric vector of drive times in minutes that can be
#'   made integer, already checked by `.validate_iso_scalar_args()`.
#' @param profile Character(1) routing profile (default `"car"`).
#'   `.ors_profile_string()` turns a short name such as `"car"` into the
#'   openrouteservice name `"driving-car"`.
#' @param api_key Character(1) openrouteservice API key. This function holds
#'   the only check that it is not empty.
#' @param ... Only the names in `.ORS_SANCTIONED_PASSTHROUGH` are forwarded.
#' @param .on_site_tick Optional function of one argument (`detail`) called
#'   after each site, for the progress reporter of `cacs_isochrone()`; `NULL`
#'   (the default) when this helper is called on its own.
#' @return An sf with the 16 columns of `.ISO_CANONICAL_COLS`.
#' @keywords internal
#' @noRd
.iso_via_ors <- function(sites, drive_times, profile, api_key = NULL,
                         .on_site_tick = NULL, ...) {
  rlang::check_installed("openrouteservice", reason = "for provider = 'ors'")

  # This is the only check that the key is not empty: the argument check in
  # cacs_isochrone() only asks for a string of length 1, so `""` reaches
  # here.
  if (is.null(api_key) || !is.character(api_key) ||
      length(api_key) != 1L || !nzchar(api_key)) {
    .cli_abort_credential(c(
      "{.field provider} = {.val ors} requires a non-empty {.envvar ORS_API_KEY}.",
      "i" = "Set in {.file .Renviron} then restart R, or pass {.arg ors_api_key} explicitly."
    ))
  }

  dots <- list(...)
  if (length(dots) > 0L && !is.null(names(dots))) {
    sanctioned <- intersect(names(dots), .ORS_SANCTIONED_PASSTHROUGH)
    pt <- dots[sanctioned]
  } else {
    pt <- list()
  }

  ors_profile <- .ors_profile_string(profile)

  # openrouteservice takes the ranges in seconds.
  breaks_int <- sort(as.integer(unique(drive_times)))
  range_seconds <- breaks_int * 60L

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
    if (is.function(.on_site_tick)) {
      tick_detail <- as.character(sites$site_id[[i]])
      tryCatch(.on_site_tick(detail = tick_detail), error = function(e) NULL)
    }
    # A request limit or a refused key ends the whole call before the next
    # site: every remaining site would meet the same answer, and a service
    # that limits requests is sent no more of them.
    .ors_stop_on_http_status(per_site[[i]]$http_status)
  }

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

  # The order of .ISO_CANONICAL_COLS, with geometry third.
  ord <- c("site_id", "drive_time_min", "geometry", "provider", "profile",
           "osm_snapshot_date", "routing_engine_version",
           "polygon_simplification_tolerance", "generated_at",
           "isochrone_empty", "provider_requested", "provider_downgrade",
           "osm_snapshot_status", "failure_reason", "retry_count",
           "ring_topology")
  out_sf <- out_sf[, ord]

  # One warning for the empty rows and one for the failed rows, rather than
  # one per row.
  n_empty <- sum(out_sf$isochrone_empty, na.rm = TRUE)
  n_fail  <- sum(!is.na(out_sf$failure_reason))
  if (n_empty > 0L) {
    .cli_warn_runtime(c(
      "{n_empty} drive-time area{?s} from {.val ors} {?is/are} empty ({.code isochrone_empty = TRUE}).",
      "i" = "{.fn cacs_run} gives these site and drive-time pairs rows of {.code NA} with {.code failure_origin = \"isochrone\"}."
    ), phase = "isochrone")
  }
  if (n_fail > 0L) {
    .cli_warn_runtime(c(
      "Routing failed for {n_fail} row{?s}; the error message is in the {.field failure_reason} column.",
      "i" = "{.fn cacs_run} gives these site and drive-time pairs rows of {.code NA} with {.code failure_origin = \"isochrone\"}."
    ), phase = "isochrone")
  }

  out_sf
}


#' Try one site up to three times
#'
#' @param site_row 1-row sf POINT in EPSG:4326.
#' @param breaks_min Integer minute breaks (sorted, unique); used to align
#'   ORS response polygons (whose `value` property is in seconds) back to the
#'   caller's minute scale.
#' @param breaks_sec Integer second breaks (= `breaks_min * 60`); the
#'   `range` argument forwarded to `openrouteservice::ors_isochrones()`.
#' @param profile ORS profile string (e.g. `"driving-car"`).
#' @param api_key ORS API key.
#' @param passthrough Named list of the forwarded `...` arguments.
#' @return A list with elements
#'   * `geom`: a list of length `length(breaks_min)`; each element is an
#'     `sfg` polygon (possibly empty) or `NULL` if the attempt failed.
#'   * `empty`: a logical vector of length `length(breaks_min)`, `TRUE` when
#'     the polygon is empty.
#'   * `attempts`: the number of attempts made (1, 2, or 3).
#'   * `http_status`: the HTTP status (200 on success, 4xx or 5xx on error,
#'     `NA_integer_` when no status could be read).
#'   * `failure_reason`: the condition message, `NA_character_` on success.
#' @keywords internal
#' @noRd
.ors_retry_one_site <- function(site_row, breaks_min, breaks_sec,
                                profile, api_key, passthrough = list()) {
  max_attempts <- 3L
  # Seconds waited after an attempt. The third value is never reached, because
  # the loop below waits only while `attempt < max_attempts`.
  backoff <- c(1, 2, 4)

  # openrouteservice takes the location as c(lon, lat).
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

    if (is.na(out$failure_reason)) {
      return(out)
    }

    # 429, 401 and 403 are returned at once: .iso_via_ors() turns them into
    # the error that ends the call.
    if (!is.na(out$http_status) &&
        out$http_status %in% c(401L, 403L, 429L)) {
      return(out)
    }

    # Any other 4xx says the request itself is wrong, so it is not repeated.
    if (!is.na(out$http_status) &&
        out$http_status >= 400L && out$http_status < 500L) {
      return(out)
    }

    # A 5xx, a timeout, or no status at all: wait and try again.
    if (attempt < max_attempts) {
      Sys.sleep(backoff[[attempt]])
    }
  }

  last_out
}


#' Stop the call after a request limit or a refused key from openrouteservice
#'
#' `.iso_via_ors()` calls this after each site, so no request is sent after
#' an answer with HTTP status 429, 401, or 403.
#'
#' @param http_status The HTTP status of the site just tried, or `NA`.
#' @param call The environment of the call reported in the error.
#' @return `NULL`, invisibly, for any other status.
#' @keywords internal
#' @noRd
.ors_stop_on_http_status <- function(http_status, call = rlang::caller_env()) {
  if (is.na(http_status)) {
    return(invisible(NULL))
  }
  if (http_status == 429L) {
    .cli_abort_operator(c(
      "Rate limit (HTTP 429) from {.field provider} = {.val ors}.",
      "i" = "The free openrouteservice plan limits requests per minute and per day.",
      "i" = "Wait and try again, use a self-hosted openrouteservice server, or use {.code provider = \"osrm\"}."
    ), call = call)
  }
  if (http_status %in% c(401L, 403L)) {
    .cli_abort_credential(c(
      "Authentication failure (HTTP {.val {http_status}}) from {.field provider} = {.val ors}.",
      "i" = "Check the key in {.envvar ORS_API_KEY}; keys are issued at {.url https://openrouteservice.org/dev/}."
    ), call = call)
  }
  invisible(NULL)
}


#' Send one request to openrouteservice
#'
#' The call goes through this wrapper so that a test can replace it with
#' `testthat::local_mocked_bindings(.ors_call_isochrones = ...,
#' .package = "catchmentACS")`, which works without openrouteservice
#' installed.
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


#' Turn one openrouteservice reply into the list `.iso_via_ors()` reads
#'
#' The reply is a GeoJSON feature collection with one feature for each
#' location and range. The `value` property of a feature holds its cutoff in
#' seconds when `range_type = "time"`, so it is divided by 60 to match the
#' drive times in minutes.
#'
#' @param iso_obj Whatever `openrouteservice::ors_isochrones()` returned: an
#'   `sf` data frame, a list with a `features` element, or something else.
#'   The `sf` form is read first.
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

  if (inherits(iso_obj, "sf")) {
    geoms <- sf::st_geometry(iso_obj)
    value_col <- if ("value" %in% names(iso_obj)) {
      as.integer(iso_obj$value)
    } else {
      NULL
    }
  } else if (is.list(iso_obj) && !is.null(iso_obj$features)) {
    # A list: write it back to JSON and let sf read it.
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


#' Turn a short profile name into an openrouteservice profile name
#'
#' `"car"` becomes `"driving-car"`, and an openrouteservice name is returned
#' as it is. Any other value stops with the R error "subscript out of
#' bounds", raised by the lookup below.
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
    # Not reached: `canonical[[profile]]` stops first for a name that is not
    # in the table.
    return(profile)
  }
  out
}


# %||%, with the same body in five files; the reason is in R/intersect-weight.R.
`%||%` <- function(x, y) if (is.null(x)) y else x


# Used only when openrouteservice returns a list rather than an sf object.
# jsonlite is in Suggests, so this stops when it is not installed.
jsonlite_toJSON_safe <- function(x) {
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::toJSON(x, auto_unbox = TRUE, force = TRUE)
  } else {
    stop("Cannot parse ORS list response without jsonlite; install jsonlite or upgrade openrouteservice to >= 0.5")
  }
}
