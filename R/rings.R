# ============================================================================
# rings.R - explicit topology conversion helpers
# ============================================================================


#' Convert annulus-topology isochrone rings to cumulative-topology rings
#'
#' The current release accepts cumulative isochrone rings only: each row
#' represents the area reachable within `[0, isomax]`. If your isochrone source
#' returns annulus bands such as `(0, 5]`, `(5, 10]`, and `(10, 15]`, use this
#' helper to union the bands into cumulative polygons before calling
#' [cacs_intersect_weight()] or passing precomputed isochrones to [cacs_run()].
#'
#' @param iso_sf An `sf` data frame with polygon geometry and columns
#'   `site_id`, `isomin`, and `isomax`.
#' @param drive_times Optional numeric vector of drive times to retain. If
#'   `NULL`, all unique `isomax` values found in `iso_sf` are retained.
#'
#' @return An `sf` object with one row per `site_id` by requested drive time,
#'   `isomin = 0`, `isomax = drive_time_min`, and
#'   `ring_topology = "cumulative"`.
#'
#' @seealso [cacs_validate_iso()], [cacs_intersect_weight()], [cacs_run()],
#'   [cacs_isochrone()]
#'
#' @family rate and MOE helpers
#'
#' @examples
#' poly <- function(xmin, xmax, ymin, ymax) {
#'   sf::st_polygon(list(rbind(
#'     c(xmin, ymin),
#'     c(xmax, ymin),
#'     c(xmax, ymax),
#'     c(xmin, ymax),
#'     c(xmin, ymin)
#'   )))
#' }
#'
#' annulus <- sf::st_sf(
#'   site_id = c("A", "A"),
#'   isomin = c(0L, 5L),
#'   isomax = c(5L, 10L),
#'   geometry = sf::st_sfc(
#'     poly(0, 1, 0, 1),
#'     poly(1, 2, 0, 1),
#'     crs = 4326
#'   )
#' )
#'
#' cumulative <- cacs_rings_to_cumulative(annulus, drive_times = c(5, 10))
#' sf::st_drop_geometry(cumulative)[
#'   , c("site_id", "isomin", "isomax", "drive_time_min", "ring_topology")
#' ]
#' unique(cumulative$ring_topology)
#'
#' @export
cacs_rings_to_cumulative <- function(iso_sf, drive_times = NULL) {
  .validate_rings_sf(iso_sf)

  isomin <- .whole_numeric_column(iso_sf$isomin, "isomin")
  isomax <- .whole_numeric_column(iso_sf$isomax, "isomax")
  if (any(isomin < 0, na.rm = TRUE)) {
    .cli_abort_schema("{.field isomin} must be nonnegative.")
  }
  if (any(isomax <= 0, na.rm = TRUE)) {
    .cli_abort_schema("{.field isomax} must be positive.")
  }
  if (any(isomax <= isomin, na.rm = TRUE)) {
    .cli_abort_schema("{.field isomax} must be greater than {.field isomin}.")
  }

  if ("ring_topology" %in% names(iso_sf)) {
    if (!is.character(iso_sf$ring_topology) || anyNA(iso_sf$ring_topology)) {
      .cli_abort_schema("{.field ring_topology} must be a non-missing character column.")
    }
    bad_topology <- setdiff(unique(iso_sf$ring_topology),
                            c("annulus", "cumulative"))
    if (length(bad_topology) > 0L) {
    .cli_abort_schema(c(
      "{.field ring_topology} contains unsupported value(s).",
      "x" = "Got {.val {bad_topology}}.",
        "i" = "Supported values are {.val annulus} and {.val cumulative}."
      ))
    }
  }

  drive_times_resolved <- .resolve_ring_drive_times(isomax, drive_times)

  site_ids <- sort(unique(iso_sf$site_id))
  out_rows <- vector("list", length(site_ids) * length(drive_times_resolved))
  k <- 0L

  for (sid in site_ids) {
    site_idx <- which(iso_sf$site_id == sid)
    site_sf <- iso_sf[site_idx, , drop = FALSE]
    site_isomin <- isomin[site_idx]
    site_isomax <- isomax[site_idx]

    for (dt in drive_times_resolved) {
      if (!dt %in% site_isomax) {
        .cli_abort_schema(c(
          "Requested drive time not present for every site.",
          "x" = "Missing {.val {dt}} for {.field site_id} {.val {sid}}."
        ))
      }

      use_idx <- which(site_isomax <= dt)
      cumulative_like <- all(site_isomin[use_idx] == 0)
      if (!cumulative_like) {
        .assert_complete_annulus_chain(
          site_id = sid,
          target = dt,
          isomin = site_isomin[use_idx],
          isomax = site_isomax[use_idx]
        )
      }

      target_idx <- which(site_isomax == dt)[1L]
      meta <- sf::st_drop_geometry(site_sf[target_idx, , drop = FALSE])
      meta$isomin <- 0L
      meta$isomax <- as.integer(dt)
      meta$drive_time_min <- as.integer(dt)
      meta$ring_topology <- "cumulative"

      geom <- sf::st_union(sf::st_geometry(site_sf[use_idx, , drop = FALSE]))
      k <- k + 1L
      out_rows[[k]] <- sf::st_sf(
        meta,
        geometry = sf::st_sfc(geom[[1L]], crs = sf::st_crs(iso_sf))
      )
    }
  }

  out <- do.call(rbind, out_rows[seq_len(k)])
  ord <- order(out$site_id, out$drive_time_min)
  out <- out[ord, , drop = FALSE]
  row.names(out) <- NULL
  out
}


#' @keywords internal
#' @noRd
.validate_rings_sf <- function(iso_sf) {
  if (!inherits(iso_sf, "sf")) {
    .cli_abort_schema(c(
      "{.arg iso_sf} must be an {.cls sf} object.",
      "x" = "Got {.cls {class(iso_sf)[[1]]}}."
    ))
  }
  if (nrow(iso_sf) == 0L) {
    .cli_abort_schema("{.arg iso_sf} must contain at least one ring row.")
  }

  required <- c("site_id", "isomin", "isomax")
  missing <- setdiff(required, names(iso_sf))
  if (length(missing) > 0L) {
    .cli_abort_schema(c(
      "{.arg iso_sf} missing required column(s): {.field {missing}}.",
      "i" = "Annulus conversion needs {.field site_id}, {.field isomin}, and {.field isomax}."
    ))
  }

  crs <- sf::st_crs(iso_sf)$epsg
  if (is.na(crs) || !identical(as.integer(crs), 4326L)) {
    .cli_abort_schema(c(
      "{.arg iso_sf} CRS must be {.val EPSG:4326}.",
      "x" = "Got {.val {if (is.na(crs)) 'NA' else as.character(crs)}}."
    ))
  }

  gtypes <- as.character(sf::st_geometry_type(iso_sf))
  bad_gtype <- setdiff(unique(gtypes), c("POLYGON", "MULTIPOLYGON"))
  if (length(bad_gtype) > 0L) {
    .cli_abort_schema(c(
      "{.arg iso_sf} geometry type must be {.val POLYGON} or {.val MULTIPOLYGON}.",
      "x" = "Got {.val {bad_gtype}}."
    ))
  }

  if (!is.character(iso_sf$site_id) ||
      anyNA(iso_sf$site_id) ||
      any(!nzchar(iso_sf$site_id))) {
    .cli_abort_schema("{.field site_id} must be non-empty character values.")
  }

  invisible(TRUE)
}


#' @keywords internal
#' @noRd
.whole_numeric_column <- function(x, col) {
  if (!is.integer(x) && !is.numeric(x)) {
    .cli_abort_schema("{.field {col}} must be numeric or integer.")
  }
  out <- as.numeric(x)
  if (anyNA(out) || any(!is.finite(out))) {
    .cli_abort_schema("{.field {col}} must contain finite non-missing values.")
  }
  if (any(out != floor(out))) {
    .cli_abort_schema("{.field {col}} must contain whole-minute values.")
  }
  as.integer(out)
}


#' @keywords internal
#' @noRd
.resolve_ring_drive_times <- function(isomax, drive_times) {
  if (is.null(drive_times)) {
    return(sort(unique(as.integer(isomax))))
  }
  if (!is.integer(drive_times) && !is.numeric(drive_times)) {
    .cli_abort_schema("{.arg drive_times} must be numeric or integer.")
  }
  out <- as.numeric(drive_times)
  if (length(out) == 0L || anyNA(out) || any(!is.finite(out)) ||
      any(out <= 0) || any(out != floor(out))) {
    .cli_abort_schema("{.arg drive_times} must contain positive whole-minute values.")
  }
  out <- sort(unique(as.integer(out)))
  missing <- setdiff(out, unique(as.integer(isomax)))
  if (length(missing) > 0L) {
    .cli_abort_schema(c(
      "{.arg drive_times} contains value(s) not present in {.field isomax}.",
      "x" = "Missing {.val {missing}}."
    ))
  }
  out
}


#' @keywords internal
#' @noRd
.assert_complete_annulus_chain <- function(site_id, target, isomin, isomax) {
  intervals <- unique(data.frame(
    isomin = as.integer(isomin),
    isomax = as.integer(isomax)
  ))
  intervals <- intervals[order(intervals$isomin, intervals$isomax), ,
                         drop = FALSE]

  cursor <- 0L
  for (i in seq_len(nrow(intervals))) {
    if (!identical(intervals$isomin[[i]], cursor)) {
      .cli_abort_schema(c(
        "Annulus rings must form a complete chain from 0 to each requested drive time.",
        "x" = "Gap for {.field site_id} {.val {site_id}} before {.val {target}} minutes.",
        "i" = "Expected next {.field isomin} to be {.val {cursor}}, got {.val {intervals$isomin[[i]]}}."
      ))
    }
    cursor <- intervals$isomax[[i]]
    if (identical(cursor, as.integer(target))) {
      return(invisible(TRUE))
    }
  }

  .cli_abort_schema(c(
    "Annulus rings must form a complete chain from 0 to each requested drive time.",
    "x" = "Chain for {.field site_id} {.val {site_id}} stops at {.val {cursor}} before requested {.val {target}}."
  ))
}
