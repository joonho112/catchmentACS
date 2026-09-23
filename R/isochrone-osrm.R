# isochrone-osrm.R - .iso_via_osrm(), the OSRM helper called by
# cacs_isochrone(). It works site by site, like the openrouteservice helper
# in R/isochrone-ors.R.
#
#   - Each site is tried up to three times, with a pause of 1 second after
#     the first attempt and 2 after the second. An HTTP 5xx, a timeout, or a
#     reply with no status is tried again; any other HTTP 4xx is recorded in
#     `failure_reason` and not tried again; HTTP 429 and HTTP 401 or 403 stop
#     the whole call.
#   - It returns the 16 columns of `.ISO_CANONICAL_COLS` itself, in that
#     order; `.normalize_iso_schema()` then fills in the two snapshot columns.
#   - The rest of the file holds the `res` handling: how many grid points
#     OSRM is asked for, and what that costs on the public demo server.


# The `...` arguments cacs_isochrone() forwards to the OSRM helper.
.OSRM_SANCTIONED_PASSTHROUGH <- c(
  "osrm.server", "osrm.profile", "osrm.profile_name", "res"
)


# The nine res-to-n pairs that osrm 5.0.0 holds in its own get_resolution().
# Neither default of this package, 30 or 70, is among them.
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


#' Choose the `res` value when the caller gave none
#'
#' The choice depends on three things:
#'
#'   res given   osrm_mode   options_protect     res used   emit_msg
#'   no          "demo"      TRUE                30         TRUE
#'   no          "demo"      FALSE               70         FALSE
#'   no          "docker"    either              70         FALSE
#'   yes         either      either              as given   FALSE
#'
#' The option is read by the caller and passed in, and the message is left to
#' the caller too, so this function answers the same way for the same three
#' values.
#' @keywords internal
#' @noRd
.cacs_osrm_resolve_demo_res <- function(res_param,
                                        osrm_mode = c("demo", "docker"),
                                        options_protect = TRUE) {

  osrm_mode <- match.arg(osrm_mode)
  if (!is.logical(options_protect) || length(options_protect) != 1L ||
      is.na(options_protect)) {
    stop("The option catchmentACS.osrm_demo_budget_protect must be TRUE or FALSE.",
         call. = FALSE)
  }

  # A `res` from the caller wins over both other values.
  if (!is.null(res_param)) {
    return(list(
      res_effective = as.integer(res_param),
      downgraded    = FALSE,
      emit_msg      = FALSE,
      msg_text      = NULL
    ))
  }

  if (identical(osrm_mode, "demo") && isTRUE(options_protect)) {
    return(list(
      res_effective = .OSRM_RES_DEFAULT_DEMO,   # 30L
      downgraded    = TRUE,
      emit_msg      = TRUE,
      msg_text      = "OSRM omitted-`res` downgraded to 30L on public demo to avoid HTTP 429."
    ))
  }

  # Demo without the protection, or docker.
  list(
    res_effective = .OSRM_RES_DEFAULT,   # 70L
    downgraded    = FALSE,
    emit_msg      = FALSE,
    msg_text      = NULL
  )
}


#' Build the drive-time areas with OSRM
#'
#' Calls `osrm::osrmIsochrone()` once per site, with the retries described at
#' the top of the file. A site that fails keeps its rows, with the message in
#' `failure_reason`.
#'
#' @param sites An sf POINT object (EPSG:4326) with a `site_id` column,
#'   already checked by `.normalize_sites_input()`.
#' @param drive_times Numeric vector of drive times in minutes that can be
#'   made integer, already checked by `.validate_iso_scalar_args()`.
#' @param profile Character(1) routing profile (default `"car"`).
#' @param osrm_mode `"demo"` or `"docker"` (default `"demo"`). It sets the
#'   default `res` and is recorded; which server is used comes from the
#'   `osrm.server` option, which the caller can set through `...`.
#' @param ... Only the names in `.OSRM_SANCTIONED_PASSTHROUGH` are forwarded.
#' @param .on_site_tick Optional function of one argument (`detail`) called
#'   after each site, for the progress reporter of `cacs_isochrone()`; `NULL`
#'   (the default) when this helper is called on its own.
#' @return An sf with the 16 columns of `.ISO_CANONICAL_COLS`.
#' @keywords internal
#' @noRd
.iso_via_osrm <- function(sites, drive_times, profile,
                          osrm_mode = c("demo", "docker"),
                          .on_site_tick = NULL, ...) {
  osrm_mode <- match.arg(osrm_mode)
  # osrm reads the server and the profile from the options osrm.server and
  # osrm.profile, and loading osrm sets both to its own defaults. The values
  # the caller had are read before check_installed() can load it, so that
  # on.exit() below puts those back rather than the defaults; an option that
  # was not set keeps the value osrm gave it.
  caller_server  <- getOption("osrm.server")
  caller_profile <- getOption("osrm.profile")
  rlang::check_installed("osrm", reason = "for provider = 'osrm'")

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

  # The server and the profile of this call are set as options for osrm and
  # put back by on.exit().
  if ("osrm.server" %in% names(pt)) {
    old_server <- caller_server %||% getOption("osrm.server")
    options(osrm.server = pt$osrm.server)
    on.exit(options(osrm.server = old_server), add = TRUE)
  }
  if ("osrm.profile" %in% names(pt)) {
    old_profile <- caller_profile %||% getOption("osrm.profile")
    options(osrm.profile = pt$osrm.profile)
    on.exit(options(osrm.profile = old_profile), add = TRUE)
  }

  # cacs_isochrone() has usually resolved `res` already and passes it in
  # through `...`, so this call returns it unchanged and says nothing. The
  # branch matters when this helper is called on its own.
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

  breaks_int <- sort(as.integer(unique(drive_times)))
  n_sites <- nrow(sites)
  per_site <- vector("list", n_sites)
  for (i in seq_len(n_sites)) {
    per_site[[i]] <- .osrm_retry_one_site(
      site_row = sites[i, , drop = FALSE],
      breaks   = breaks_int,
      res      = res_param
    )
    if (is.function(.on_site_tick)) {
      tick_detail <- as.character(sites$site_id[[i]])
      tryCatch(.on_site_tick(detail = tick_detail), error = function(e) NULL)
    }
    # A request limit or a refused key ends the whole call before the next
    # site: every remaining site would meet the same answer, and a server
    # that limits requests is sent no more of them.
    .osrm_stop_on_http_status(per_site[[i]]$http_status)
  }

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
      "{n_empty} drive-time area{?s} from {.val osrm} {?is/are} empty ({.code isochrone_empty = TRUE}).",
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


#' Stop the call after a request limit or a refused key from OSRM
#'
#' `.iso_via_osrm()` calls this after each site, so no request is sent after
#' an answer with HTTP status 429, 401, or 403.
#'
#' @param http_status The HTTP status of the site just tried, or `NA`.
#' @param call The environment of the call reported in the error.
#' @return `NULL`, invisibly, for any other status.
#' @keywords internal
#' @noRd
.osrm_stop_on_http_status <- function(http_status, call = rlang::caller_env()) {
  if (is.na(http_status)) {
    return(invisible(NULL))
  }
  if (http_status == 429L) {
    .cli_abort_operator(c(
      "Rate limit (HTTP 429) from {.field provider} = {.val osrm}.",
      "i" = "Three ways to recover:",
      "*" = "Lower {.arg res}, which sends fewer requests: {.code res = 30L} in {.fn cacs_isochrone}, or {.code iso_args = list(res = 30L)} in {.fn cacs_run}.",
      "*" = "Use a local OSRM server, which has no request limit: {.code osrm_mode = \"docker\"}.",
      "*" = "Switch provider: {.code provider = \"ors\"} uses openrouteservice and needs {.envvar ORS_API_KEY}.",
      "i" = "See the \"OSRM servers\" and \"OSRM grid resolution\" sections of {.help cacs_isochrone}."
    ), call = call)
  }
  if (http_status %in% c(401L, 403L)) {
    .cli_abort_credential(c(
      "Authentication failure (HTTP {.val {http_status}}) from {.field provider} = {.val osrm}.",
      "i" = "The public OSRM demo server needs no key; check the server named by the {.code osrm.server} option.",
      "i" = "A server of your own that needs a key refused the one it was given; check that key."
    ), call = call)
  }
  invisible(NULL)
}


#' Try one site up to three times
#'
#' @param site_row 1-row sf POINT in EPSG:4326.
#' @param breaks Integer minute breaks (sorted, unique).
#' @param res The `res` value, already chosen by the caller.
#'   `.osrm_call_isochrone()` sends it to osrm as `n` only for the values in
#'   `.OSRM_RES_TO_N`.
#' @return A list with elements
#'   * `geom`: a list of length `length(breaks)`; each element is an `sfg`
#'     polygon (possibly empty) or `NULL` if the attempt failed.
#'   * `empty`: a logical vector of length `length(breaks)`, `TRUE` when the
#'     polygon is empty.
#'   * `attempts`: the number of attempts made (1, 2, or 3).
#'   * `http_status`: the HTTP status (200 on success, 4xx or 5xx on error,
#'     `NA_integer_` when no status could be read).
#'   * `failure_reason`: the condition message, `NA_character_` on success.
#' @keywords internal
#' @noRd
.osrm_retry_one_site <- function(site_row, breaks, res = NULL) {
  max_attempts <- 3L
  # Seconds waited after an attempt. The third value is never reached, because
  # the loop below waits only while `attempt < max_attempts`.
  backoff <- c(1, 2, 4)

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

    if (is.na(out$failure_reason)) {
      return(out)
    }

    # 429, 401 and 403 are returned at once: .iso_via_osrm() turns them into
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


#' Send one request to OSRM
#'
#' The call goes through this wrapper so that a test can replace it with
#' `testthat::local_mocked_bindings(.osrm_call_isochrone = ...,
#' .package = "catchmentACS")`, which works without osrm installed.
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


#' Name the grid-size argument osrm expects
#'
#' catchmentACS takes `res`. osrm 5.0.0 asks for `n` instead and turns it back
#' into a `res` through nine fixed pairs, the ones in `.OSRM_RES_TO_N`: an `n`
#' outside them gives a warning and is replaced by `n = 500`, that is
#' `res = 27`. So `n` is sent only for a `res` that is one of the nine, and
#' any other `res` is sent as `res`, which osrm still uses while reporting the
#' argument as deprecated. Sending the nearest `n` instead would change the
#' grid, and with it the areas.
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


#' Estimate what one site costs OSRM
#'
#' Works out, without sending anything, how many table requests
#' `osrm::osrmIsochrone()` needs for one site and how long the public demo
#' server makes it wait: `res * res` grid points sent in chunks of
#' `.OSRM_PUBLIC_DEMO_CHUNK_SIZE` on the demo server and
#' `.OSRM_CUSTOM_SERVER_CHUNK_SIZE` elsewhere, with one second of waiting per
#' full chunk on the demo server. The count is an upper bound on both
#' servers: osrm 5.0.0 sends only the grid points within reach of the site,
#' about three quarters of `res * res`, and takes 999 of them at a time from
#' a server other than the public demo one rather than 450.
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


#' Turn one OSRM reply into the list `.iso_via_osrm()` reads
#'
#' osrm returns one row for each band between two drive times, with the band
#' in `isomin` and `isomax`. When any `isomin` is above zero the rows are
#' bands rather than whole areas, and `cacs_rings_to_cumulative()` adds each
#' band to the ones inside it. The row for each drive time is then found by
#' its `isomax`, or by position when the column is missing.
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


#' Read an HTTP status out of an error message
#'
#' The errors from osrm and openrouteservice give the status in one of a few
#' forms: "OSRM API request failed [503]", "HTTP 503 ...", "(HTTP 429)",
#' "status 429", or "client error (429)". Only a number from 100 to 599 in one
#' of these forms is read. Other numbers are not: a connection failure says
#' "Failed to connect to routing.openstreetmap.de port 443" or
#' "[routing.openstreetmap.de:443]", and reading 443 there would treat a
#' failure that may not happen again as a rejected request (not tried again,
#' and saved in the cache). A message without a status gives `NA`.
#'
#' @keywords internal
#' @noRd
.extract_http_status <- function(msg) {
  if (is.null(msg) || !is.character(msg) || length(msg) == 0L || is.na(msg[[1L]])) {
    return(NA_integer_)
  }
  msg <- msg[[1L]]
  forms <- c(
    "\\bHTTP(?:/[0-9.]+)?[ :]*(?:status[ :]*)?([1-5][0-9]{2})(?![0-9])",
    "\\[([1-5][0-9]{2})\\]",
    "\\bstatus(?:[ _]code)?[ :=]*([1-5][0-9]{2})(?![0-9])",
    "\\(([1-5][0-9]{2})\\)"
  )
  for (form in forms) {
    m <- regmatches(msg, regexec(form, msg, perl = TRUE, ignore.case = TRUE))[[1L]]
    if (length(m) >= 2L) {
      return(as.integer(m[[2L]]))
    }
  }
  NA_integer_
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


# cacs_validate_osrm_endpoint(), which sends one small request so a caller
# can see whether a server answers before cacs_isochrone() sends many.


#' Check whether an OSRM server answers a routing request
#'
#' Sends one small request to an Open Source Routing Machine (OSRM) server
#' and reports whether the server answered it with HTTP status 200.
#' [cacs_isochrone()] sends many requests for each site, so this shows
#' beforehand whether the server can be reached and is accepting requests.
#'
#' When the server answers with status 429, meaning that its limit on
#' requests has been reached, the function gives a warning of class
#' `catchmentACS_warning_provider_quota_exhausted` and still returns its
#' result. Other statuses give no warning. A server that cannot be reached,
#' or does not answer within `timeout` seconds, gives `quota_ok = FALSE` and
#' `http_status = NA` without a warning or an error.
#'
#' The request is for a route between two fixed points near Birmingham,
#' Alabama, so a server whose map data do not cover them may not answer with
#' status 200. It is sent to `route/v1/driving/-86.8,33.5;-86.7,33.4` under
#' the server address; on the public demo server, `routed-car/`,
#' `routed-bike/`, or `routed-foot/` comes first, following the option
#' `osrm.profile`.
#'
#' @param server A string giving the address of the OSRM server, or `NULL`
#'   (the default). With `NULL` or `""`, the address is the value of the
#'   option `osrm.server` at the time of the call, or the public OSRM demo
#'   server `https://routing.openstreetmap.de/` when the option is not set.
#'   Loading the osrm package, for example with `library(osrm)`, sets that
#'   option to the demo server and replaces a value set before (see the "OSRM
#'   servers" section of [cacs_isochrone()]). The option
#'   `catchmentACS.osrm_docker_server` is not used, so the local server that
#'   [cacs_isochrone()] uses with
#'   `osrm_mode = "docker"` is checked only when its address is given here.
#' @param timeout A single positive number giving the number of seconds to
#'   wait for an answer; the default is 5.
#'
#' @return A tibble with one row and four columns:
#'   \describe{
#'     \item{`endpoint`}{The server address used.}
#'     \item{`quota_ok`}{`TRUE` when the server answered with HTTP status
#'       200, and `FALSE` otherwise, including when there was no answer.}
#'     \item{`response_ms`}{The time taken, in milliseconds, including any
#'       time spent waiting for an answer that did not come.}
#'     \item{`http_status`}{The HTTP status of the answer, as an integer, or
#'       `NA` when there was no answer.}
#'   }
#'
#' @examples
#' # A local port where no server is expected to answer: quota_ok is FALSE
#' # and http_status is NA, without an error
#' cacs_validate_osrm_endpoint("http://localhost:9", timeout = 1)
#'
#' # Contacts the public OSRM demo server, which limits the requests it
#' # accepts, and then builds a drive-time area there.
#' \dontrun{
#' site_07 <- data.frame(site_id = "AL_SITE_07", lon = -85.365, lat = 31.655)
#'
#' # Check the server before building the drive-time areas
#' check <- cacs_validate_osrm_endpoint()
#' check
#' if (check$quota_ok) {
#'   iso <- cacs_isochrone(site_07, drive_times = 10)
#' } else if (identical(check$http_status, 429L)) {
#'   # Request limit reached: build the areas with a local OSRM server
#'   iso <- cacs_isochrone(site_07, drive_times = 10, osrm_mode = "docker")
#' } else {
#'   stop("The OSRM server is not accepting requests; see check$http_status.")
#' }
#' }
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
  # A route between two near-by points is the smallest request OSRM answers,
  # so it shows whether the server is reachable and still taking requests.
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
    # The server could not be reached, or did not answer in time.
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
      "The OSRM server at {.url {server}} answered that its request limit has been reached (HTTP status 429).",
      "i" = "Wait and try again, or use a local OSRM server: {.code osrm_mode = \"docker\"}.",
      "i" = "A lower {.arg res} sends fewer requests: {.code res = 30L} in {.fn cacs_isochrone}, or {.code iso_args = list(res = 30L)} in {.fn cacs_run}."
    ))
  }

  tibble::tibble(
    endpoint    = server,
    quota_ok    = quota_ok,
    response_ms = elapsed_ms,
    http_status = status
  )
}
