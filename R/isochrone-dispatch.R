# isochrone-dispatch.R - cacs_isochrone(), which checks the arguments, looks
# in the cache, calls the helper for the chosen routing service
# (R/isochrone-osrm.R, R/isochrone-ors.R, R/isochrone-mapbox.R,
# R/isochrone-r5r.R), puts the result in the shared column order
# (R/isochrone-normalize.R), and saves it.


#' Build drive-time areas around sites with a routing service
#'
#' Builds a drive-time area around each site for each drive time, using a
#' routing service: the Open Source Routing Machine (OSRM, the default) or
#' openrouteservice. Each area, also called an isochrone, covers the places
#' that the routing service finds reachable from the site within that many
#' minutes, so the 10-minute area includes the 5-minute area.
#'
#' Routing for a site is tried up to three times, and not again after an
#' HTTP 4xx error such as a bad request. A site for which routing fails keeps
#' its rows, with an empty geometry and the error message in
#' `failure_reason`; warnings report the number of rows that failed or have
#' no area. If the routing service answers that its request limit has been
#' reached (HTTP status 429), the function stops with an error as soon as
#' that answer comes, sends no requests for the remaining sites, and returns
#' no result. It does the same when the service refuses access (HTTP status
#' 401 or 403), for example because an API key is not accepted.
#'
#' The requests have no time limit of their own: a server that accepts the
#' connection but does not answer makes the function wait, and a server that
#' cannot be reached is tried three times for each site before the site is
#' reported as failed. [cacs_validate_osrm_endpoint()] checks an OSRM server
#' with a time limit and can be called first.
#'
#' Unless the cache is turned off with [cacs_set_cache()], the result is
#' saved in the cache folder (`cache_dir`, or [cacs_cache_dir()]), which by
#' default lasts only for the R session. A later call with the same arguments
#' (other than `verbose`) and option settings returns the saved result
#' without contacting the routing service, as long as the installed versions
#' of sf, PROJ, and GEOS have not changed. A result is not saved when routing
#' failed for a site because the service did not answer, timed out, or
#' answered with an HTTP 5xx error, since a later call may succeed; a saved
#' result with such a failure is deleted, and its sites are routed again. A
#' site that the service refused with another HTTP 4xx error, such as a bad
#' request, is saved with its `failure_reason`, and a warning is given each
#' time the saved result is used. [cacs_clear_cache()] with
#' `namespace = "isochrone"` removes only the results saved in the folder
#' returned by [cacs_cache_dir()], whose help page describes the cache
#' folder, how long saved results are kept, and how they are checked.
#'
#' @param sites An `sf` object of points in EPSG:4326, or a data frame with
#'   numeric columns `lon` and `lat` (longitude and latitude). An `sf` object
#'   in another coordinate reference system, including NAD83 (EPSG:4269),
#'   gives an error. Either form needs a column `site_id` with a different
#'   value for each site; a numeric `site_id` becomes a string in the result.
#'   Without a `site_id` column, a column `point_id` is used as `site_id`,
#'   with a warning.
#' @param drive_times A numeric vector of drive times in minutes: at most six
#'   values, each a whole number from 1 to 60; the default is `c(5, 10, 15)`.
#'   The values can be in any order, and a repeated value is used once.
#' @param provider A string giving the routing service: `"osrm"` (the
#'   default) for OSRM or `"ors"` for openrouteservice; see the
#'   "Provider status" section. `"mapbox"` and `"r5r"` are also accepted, but
#'   they are not implemented yet; using them gives an error of class
#'   `catchmentACS_error_credential`, after the other arguments are checked
#'   and before any request is sent.
#' @param profile A string giving the routing profile, such as `"car"` (the
#'   default). With OSRM, the public demo server offers `"car"`, `"bike"`,
#'   and `"foot"`; with a profile that is not available, routing fails for
#'   every site and the error is recorded in `failure_reason`. With
#'   openrouteservice, the accepted values are the openrouteservice profiles
#'   `"driving-car"`, `"driving-hgv"`, `"cycling-regular"`, `"cycling-road"`,
#'   `"cycling-mountain"`, `"cycling-electric"`, `"foot-walking"`,
#'   `"foot-hiking"`, and `"wheelchair"`, and the short names `"car"`,
#'   `"hgv"`, `"bike"` (for `"cycling-regular"`), `"foot"`, and `"walk"` (both
#'   for `"foot-walking"`); any other value stops the function with the R
#'   error "subscript out of bounds".
#' @param osrm_mode A string giving where OSRM requests are sent: `"demo"`
#'   (the default) for the public OSRM demo server or `"docker"` for a local
#'   OSRM server. It also sets the default `res`; see the OSRM sections
#'   below.
#' @param ors_api_key A string giving the openrouteservice API key, by
#'   default the value of the `ORS_API_KEY` environment variable. It is used
#'   only with `provider = "ors"`; see the "Provider status" section.
#' @param mapbox_token A string, by default the value of the `MAPBOX_TOKEN`
#'   environment variable. It is not used (see `provider`), but a value that
#'   is not a single string, such as `NULL`, gives an error.
#' @param r5r_core An r5r core object, or `NULL` (the default). It is not
#'   used (see `provider`), but its class is checked.
#' @param osm_snapshot_date A date to record as the date of the OpenStreetMap
#'   data used by the routing service: a `Date` object or a string in the
#'   form year-month-day, such as `"2025-04-01"`, or `NULL` (the default).
#'   The date is only recorded in the result; it is not sent to the routing
#'   service and does not change the areas. A call with a different date
#'   does not reuse a saved result.
#' @param cache_dir A path to the cache folder, or `NULL` (the default) to use
#'   [cacs_cache_dir()]. A folder outside the temporary folder of the R
#'   session is tidied as described in [cacs_cache_dir()].
#' @param verbose A logical value. With `TRUE` (the default), the function
#'   shows a summary line when the step finishes and, with five or more
#'   sites, a line as each site finishes. See the "Progress messages"
#'   section of [cacs_run()] for how to turn them off or show the lines for
#'   any number of sites. The function shows the message that a saved result
#'   is returned, and the notices about the default `res`, even when
#'   `verbose` is `FALSE`.
#' @param ... Additional named arguments, which depend on `provider`. With
#'   OSRM, the accepted names are `res`, which sets how detailed the areas are
#'   (see the OSRM sections below); `osrm.server`, the address of an OSRM
#'   server to use instead of the one chosen by `osrm_mode`; and
#'   `osrm.profile` or `osrm.profile_name`, a profile name to use in the
#'   requests instead of `profile`. With openrouteservice, the accepted names
#'   are `attributes`, `area_units`, and `smoothing`, which are passed on to
#'   `openrouteservice::ors_isochrones()`. Other arguments give a warning and
#'   are not used.
#'
#' @return An `sf` tibble with one row for each site and drive time, in the
#'   order of `sites` and then by increasing drive time, and 16 columns:
#'
#'   - `site_id`: the site identifier, converted to a string.
#'   - `drive_time_min`: the drive time in minutes.
#'   - `geometry`: the drive-time area, of geometry type `POLYGON` or
#'     `MULTIPOLYGON`, in longitude and latitude (EPSG:4326); empty when there
#'     is no area.
#'   - `provider`: the routing service used, `"osrm"` or `"ors"`.
#'   - `profile`: the routing profile, such as `"car"`.
#'   - `osm_snapshot_date`: the date given in `osm_snapshot_date`, as a
#'     string, or `"unknown"`.
#'   - `routing_engine_version`: `"osrm-pkg/"` or `"openrouteservice-pkg/"`
#'     followed by the version of the R package that sent the requests (not
#'     the version of the routing server).
#'   - `polygon_simplification_tolerance`: always `NA`.
#'   - `provider_requested`: the service asked for, always the same as
#'     `provider`.
#'   - `provider_downgrade`: whether another service was used instead,
#'     always `FALSE`.
#'   - `generated_at`: the time the areas were built; a result read from the
#'     cache keeps the time of the original call.
#'   - `isochrone_empty`: `TRUE` when the row has no area, because routing
#'     failed or because the service returned no area for that drive time
#'     (with OSRM, for example, when the site is too far from the road
#'     network).
#'   - `osm_snapshot_status`: `"user_supplied"` when `osm_snapshot_date` was
#'     given, otherwise `"unknown_best_effort"`.
#'   - `failure_reason`: `NA` when routing succeeded, otherwise the error
#'     message from the last attempt.
#'   - `retry_count`: the number of attempts made for the site, counting the
#'     first (1 to 3).
#'   - `ring_topology`: always `"cumulative"`, meaning that each area
#'     includes the areas of the shorter drive times.
#'
#'   The attributes `cacs_isochrone_provenance` and `cacs_provenance` hold
#'   the same list, a record of how the result was produced: the cache key,
#'   `provider`, `profile`, `osrm_mode`, the snapshot date and status,
#'   `ring_topology`, and, for OSRM, the `res` value, the OSRM server and
#'   profile, and an estimate of the requests per site
#'   (`osrm_request_budget`). The
#'   attribute `cacs_res_param` holds the `res` value (`NA` for
#'   openrouteservice). Rows selected from the result keep these attributes,
#'   including the cache key that [cacs_intersect_weight()] uses to recognize
#'   the areas in its own cache.
#'
#' @section Provider status:
#' `provider = "osrm"` (the default) uses OSRM through the osrm package and
#' needs no API key; `osrm_mode` chooses the server (see the OSRM sections
#' below). `provider = "ors"` uses openrouteservice through the
#' openrouteservice package and needs an API key, given in `ors_api_key`; if
#' the key is empty, the function stops with an error of class
#' `catchmentACS_error_credential` before any request is sent. The
#' openrouteservice route has been checked only with simulated responses
#' from openrouteservice, not with the service itself. The two services
#' compute the areas in different ways, so the area for the same site and
#' drive time can differ between them.
#'
#' @section OSRM servers:
#' With `osrm_mode = "demo"`, the requests go to the public OSRM demo server
#' at `https://routing.openstreetmap.de/`; with `osrm_mode = "docker"`, they
#' go to `http://0.0.0.0:5000/`, or to the address set in the option
#' `catchmentACS.osrm_docker_server`. An address given in `osrm.server` (see
#' `...`) is used instead with either mode. With `osrm_mode = "demo"`, the
#' osrm package's own option `osrm.server` is also used. The osrm package
#' sets that option to the demo server when it is loaded, for example by
#' `library(osrm)`, replacing a value set earlier; a value set before a call
#' to this function is kept, even when the call loads osrm.
#'
#' @section OSRM grid resolution:
#' The osrm package draws the areas of a site from travel times to a grid of
#' `res` by `res` points around it, sized for the longest drive time, so the
#' areas for shorter drive times rest on fewer points. A larger `res` gives
#' more detailed areas but needs more requests and more time. On the demo
#' server, the osrm package waits one second after every 75 grid points that
#' it sends, which comes to about 10 seconds for each site at `res = 30L`
#' and about a minute at `res = 70L`. There is no such wait with other
#' servers.
#'
#' If `res` is not given, it is `70L` with `osrm_mode = "docker"`. The
#' public demo server is protected by resolving `res` to `30L` with
#' `osrm_mode = "demo"`. With the lower value, fewer requests are sent for
#' each site, so the server is less likely to answer that its request limit
#' has been reached (HTTP status 429). With
#' `options(catchmentACS.osrm_demo_budget_protect = FALSE)`,
#' `res` is `70L` with either mode. A message reports the value used, once
#' per R session; its class is
#' `catchmentACS_message_demo_budget_protected` for `30L` and
#' `catchmentACS_message_res_default_changed` for `70L`. When `res` is
#' given, such as `res = 30L`, `res = 50L`, or `res = 70L`, that value is
#' used and neither message is shown.
#'
#' From version 5.0.0, the osrm package may show the message "'res' is
#' deprecated, use 'n' instead." for each site. The `res` value is still
#' used, and `n` is not accepted by `cacs_isochrone()`.
#'
#' Areas built with another server, other OpenStreetMap data, or another
#' `res` can differ, and so can the estimates computed from them.
#'
#' @seealso [cacs_validate_osrm_endpoint()] checks whether an OSRM server is
#'   accepting requests, and [cacs_validate_iso()] checks whether drive-time
#'   areas made with other tools are in the form that the package expects.
#'   The routing services, and how to use a local OSRM server, are described
#'   in `vignette("providers", package = "catchmentACS")`
#'   (<https://joonho112.github.io/catchmentACS/articles/providers.html>).
#' @family steps of the calculation
#' @export
#' @examples
#' # The 10-minute area that cacs_isochrone() built for one site in
#' # Birmingham, Alabama, with the public OSRM demo server, kept in a file that
#' # comes with the package
#' iso_bhm <- readRDS(system.file("extdata", "visual_walkthrough_fixture.rds",
#'                                package = "catchmentACS"))$iso_sf
#' sf::st_drop_geometry(iso_bhm)[, c("site_id", "drive_time_min", "provider",
#'                                   "isochrone_empty", "failure_reason")]
#'
#' # A table with no rows: the area has the form that the package expects
#' cacs_validate_iso(iso_bhm)
#'
#' # These calls send several dozen requests to the public OSRM demo server,
#' # which limits how many it accepts.
#' \dontrun{
#' library(sf)
#' # The site at the center of the drive-time areas for AL_SITE_07 that come
#' # with the package (used in the examples of cacs_run() and others)
#' site_07 <- data.frame(site_id = "AL_SITE_07", lon = -85.365, lat = 31.655)
#'
#' # 5-, 10-, and 15-minute areas from the public OSRM demo server
#' iso <- cacs_isochrone(site_07, drive_times = c(5, 10, 15))
#' st_drop_geometry(iso)[, c("site_id", "drive_time_min", "isochrone_empty")]
#'
#' # A finer grid gives more detailed areas but takes longer
#' iso_fine <- cacs_isochrone(site_07, drive_times = c(5, 10, 15), res = 50L)
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

  # `verbose` is checked before anything else because the provider helpers
  # are handed the progress reporter built from it.
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    .cli_abort_schema(c(
      "{.arg verbose} must be {.cls logical(1)} (TRUE or FALSE).",
      "x" = "Got {.cls {class(verbose)[1L]}} of length {.val {length(verbose)}}."
    ))
  }

  sites <- .normalize_sites_input(sites)

  provider_choices <- .SANCTIONED_PROVIDERS
  provider  <- tryCatch(
    match.arg(provider),
    error = function(e) {
      .cli_abort_schema(c(
        "{.arg provider} must be one of {.val {provider_choices}}.",
        "x" = "Got {.val {provider}}.",
        "i" = "Routing is implemented for {.val osrm} and {.val ors}; {.val mapbox} and {.val r5r} are not implemented yet."
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

  # One error listing every argument that failed, up to five of them.
  .validate_iso_scalar_args(
    drive_times       = drive_times,
    profile           = profile,
    ors_api_key       = ors_api_key,
    mapbox_token      = mapbox_token,
    r5r_core          = r5r_core,
    osm_snapshot_date = osm_snapshot_date,
    cache_dir         = cache_dir
  )

  # Arguments in `...` that the chosen service accepts are forwarded to its
  # helper; the rest give a warning and are dropped.
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
      "{.fn cacs_isochrone} does not use the argument{?s} {.arg {provider_args$unknown_names}} given in {.arg ...}; {?it is/they are} dropped.",
      "i" = "See {.arg ...} in {.help cacs_isochrone} for the names accepted with each provider."
    ), phase = "isochrone")
  }

  snap <- .resolve_osm_snapshot(provider, osm_snapshot_date)

  # The cache key is built from these 16 values, so a call that differs in
  # any of them gets its own saved result. `verbose` is not among them.
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
  )

  cache_key <- .cacs_cache_key(cache_payload, namespace = "isochrone")

  cached <- suppressMessages(.cacs_cache_get(cache_key, "isochrone", cache_dir = cache_dir))
  # A saved result in which a site failed in a way that may be temporary is
  # not used. Earlier versions of the package saved such results; the copy is
  # deleted and the sites are routed again.
  if (!is.null(cached) && .iso_has_temporary_failure(cached)) {
    .iso_cache_remove(cache_key, cache_dir = cache_dir)
    cached <- NULL
  }
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
    # Sites that the routing service rejected come back from the saved result;
    # say so, as the first call did.
    n_rejected <- sum(!is.na(cached$failure_reason))
    if (n_rejected > 0L) {
      .cli_warn_runtime(c(
        "The saved result has {n_rejected} row{?s} for which the routing service rejected the request; the error message is in the {.field failure_reason} column.",
        "i" = "The service would reject the same request again, so it is not sent while the result stays in the cache."
      ), phase = "isochrone")
    }
    # A saved result is returned without the progress reporter: there is no
    # per-site loop, and the summary line would read like a fresh run. The
    # message above is what the user sees instead.
    return(cached)
  }

  # The progress reporter is passed to the provider helper as `on_site_tick`
  # and counts sites. It starts with every site marked as failed and the
  # counts are set from `out` below, so if the run stops part way (an HTTP
  # 429, say) the summary from `on.exit()` reports every site as failed.
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

  # A site counts as failed when any of its rows has a `failure_reason`: the
  # attempts for that site ran out or ended on an HTTP 4xx that is not
  # retried. The count is per site, not per row, because one site produces
  # one row for each drive time.
  per_site_failed <- out |>
    sf::st_drop_geometry() |>
    dplyr::group_by(.data$site_id) |>
    dplyr::summarise(failed = any(!is.na(.data$failure_reason)), .groups = "drop")
  prog_finish_state$n_failed  <- as.integer(sum(per_site_failed$failed))
  prog_finish_state$n_success <- as.integer(nrow(per_site_failed) -
                                            prog_finish_state$n_failed)

  # The result is saved unless a site failed in a way that may be temporary:
  # the server did not answer, timed out, or answered with an HTTP 5xx status
  # after the attempts ran out. A saved result would repeat such a failure on
  # later calls. A site that the service rejected with another HTTP 4xx status
  # is saved with its failure_reason, because the same request would be
  # rejected again, and saving spares the other sites a new request. A failed
  # write gives a warning inside .cacs_cache_put() and the result is returned
  # all the same.
  if (!.iso_has_temporary_failure(out)) {
    .cacs_cache_put(out, cache_key, "isochrone", cache_dir = cache_dir)
  }

  out
}

#' Does a routing result have a failure that may be temporary?
#'
#' A site whose `failure_reason` holds no HTTP status, or a 5xx status, failed
#' after its attempts ran out: the server did not answer, timed out, or had an
#' error, and a later call may succeed. Any other HTTP 4xx status means that
#' the service rejected the request, which it would do again. The status is
#' read from the message with `.extract_http_status()`, as the retry rule in
#' `.osrm_retry_one_site()` and `.ors_retry_one_site()` reads it.
#'
#' @param x A result of `cacs_isochrone()`.
#' @return `TRUE` or `FALSE`.
#' @keywords internal
#' @noRd
.iso_has_temporary_failure <- function(x) {
  if (!"failure_reason" %in% names(x)) {
    return(FALSE)
  }
  reasons <- unique(x$failure_reason[!is.na(x$failure_reason)])
  if (length(reasons) == 0L) {
    return(FALSE)
  }
  status <- vapply(reasons, .extract_http_status, integer(1), USE.NAMES = FALSE)
  any(is.na(status) | status >= 500L)
}


#' Delete a saved routing result
#'
#' @param key The cache key of the result.
#' @param cache_dir As in `cacs_isochrone()`.
#' @return `TRUE`, invisibly.
#' @keywords internal
#' @noRd
.iso_cache_remove <- function(key, cache_dir = NULL) {
  base <- tryCatch(
    .cacs_cache_base_dir(cache_dir = cache_dir, create = FALSE),
    error = function(e) NULL
  )
  if (is.null(base)) {
    return(invisible(TRUE))
  }
  ns_dir <- file.path(base, .cacs_cache_effective_namespace("isochrone", cache_dir = cache_dir))
  .cacs_cache_invalidate_pair(
    file.path(ns_dir, paste0(key, ".rds")),
    .cacs_cache_fingerprint_path(ns_dir, key)
  )
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


# .normalize_sites_input(): the `sites` argument as a checked sf of points.

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

  nm <- names(sites)
  if ("point_id" %in% nm && !"site_id" %in% nm) {
    .cli_warn_provenance(c(
      "The {.field point_id} column of {.arg sites} is used as {.field site_id}.",
      "i" = "Name the column {.field site_id} to avoid this warning."
    ), phase = "isochrone")
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
        "x" = "{.field lon} must be between -180 and 180, and {.field lat} between -90 and 90."
      ))
    }
    sites <- sf::st_as_sf(sites, coords = c("lon", "lat"), crs = 4326)
  }

  sites
}


# .validate_iso_scalar_args(): collects the failures of the scalar arguments
# and reports them in one error, listing at most five.

#' @keywords internal
#' @noRd
.validate_iso_scalar_args <- function(drive_times, profile, ors_api_key,
                                      mapbox_token, r5r_core,
                                      osm_snapshot_date, cache_dir) {
  errs <- character(0)

  if (!is.numeric(drive_times) || length(drive_times) == 0L) {
    errs <- c(errs, "x" = "{.arg drive_times} must be a non-empty {.cls numeric} vector; got {.cls {class(drive_times)[[1L]]}}.")
  } else {
    if (length(drive_times) > 6L) {
      errs <- c(errs, "x" = "{.arg drive_times} must have at most 6 values; got {length(drive_times)}.")
    }
    bad <- drive_times[!is.finite(drive_times) | drive_times <= 0 |
                       drive_times > 60 | drive_times != as.integer(drive_times)]
    if (length(bad) > 0L) {
      errs <- c(errs, "x" = "{.arg drive_times} values must be whole numbers from 1 to 60; not allowed: {.val {bad}}.")
    }
  }

  if (!is.character(profile) || length(profile) != 1L || is.na(profile) || !nzchar(profile)) {
    errs <- c(errs, "x" = "{.arg profile} must be a non-empty {.cls character} scalar.")
  }

  # An empty string passes here; only .iso_via_ors() rejects it.
  if (!is.character(ors_api_key) || length(ors_api_key) != 1L) {
    errs <- c(errs, "x" = "{.arg ors_api_key} must be {.cls character(1)}.")
  }

  if (!is.character(mapbox_token) || length(mapbox_token) != 1L) {
    errs <- c(errs, "x" = "{.arg mapbox_token} must be {.cls character(1)}.")
  }

  if (!is.null(r5r_core) && !inherits(r5r_core, c("r5r_core", "list", "jobjRef"))) {
    errs <- c(errs, "x" = "{.arg r5r_core} must be {.code NULL} or an r5r core object.")
  }

  if (!is.null(osm_snapshot_date)) {
    ok <- inherits(osm_snapshot_date, "Date") ||
          (is.character(osm_snapshot_date) && length(osm_snapshot_date) == 1L &&
           !is.na(suppressWarnings(as.Date(osm_snapshot_date))))
    if (!ok) {
      errs <- c(errs, "x" = "{.arg osm_snapshot_date} must be a {.cls Date}, a date string such as {.val 2025-04-01}, or {.code NULL}.")
    }
  }

  if (!is.null(cache_dir)) {
    if (!is.character(cache_dir) || length(cache_dir) != 1L || is.na(cache_dir)) {
      errs <- c(errs, "x" = "{.arg cache_dir} must be {.cls character(1)} or {.code NULL}.")
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


# .resolve_osm_snapshot(): the OpenStreetMap snapshot date recorded with the
# result. The date is only recorded, never sent to the routing service.

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
  # Nothing is looked up: without a date from the caller, the snapshot is
  # recorded as unknown whichever service is used.
  list(date = "unknown", status = "unknown_best_effort")
}


# .split_iso_provider_args(): splits `...` into the arguments the chosen
# service accepts and the names that are dropped.

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

# The message is about avoiding the demo server's forced waits, so it says
# nothing when the demo server is the one in use.
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
    "Using the OSRM server at {.url {provider_args$osrm.server}} for {n_sites} site{?s}.",
    "i" = "The osrm package does not wait between requests to this server; on the public demo server it would wait about {budget$public_demo_sleep_floor_sec_per_site} second{?s} for each site at {.code res = {budget$res}}."
  ))
  invisible(TRUE)
}


# %||%, with the same body in five files; the reason is in R/intersect-weight.R.
`%||%` <- function(x, y) if (is.null(x)) y else x
