# isochrone-normalize.R - .normalize_iso_schema(), the last step of
# cacs_isochrone(), run on whatever the routing service helper returned. It
# does two things.
#
#   1. Writes the snapshot date and status from .resolve_osm_snapshot() into
#      `osm_snapshot_date` and `osm_snapshot_status`, which the service
#      helpers leave as NA. The two columns are read as values, not as
#      "missing": "unknown" means no date was given, and "user_supplied" or
#      "unknown_best_effort" says where the date came from. Only NA entries
#      are written, so a value the helper did set is kept.
#
#   2. Checks the result with .validate_iso_schema() (R/checks.R), so a
#      helper that returns different columns stops here rather than in
#      cacs_intersect_weight() or later.


# The column order of every cacs_isochrone() result.
.ISO_CANONICAL_COLS <- c(
  "site_id", "drive_time_min", "geometry", "provider", "profile",
  "osm_snapshot_date", "routing_engine_version",
  "polygon_simplification_tolerance", "generated_at",
  "isochrone_empty", "provider_requested", "provider_downgrade",
  "osm_snapshot_status", "failure_reason", "retry_count",
  "ring_topology"
)


#' Put an isochrone helper's sf into the shared 16-column form
#'
#' Writes `snap$date` and `snap$status` into the two snapshot columns where
#' the helper left `NA`, puts the columns in the order of
#' `.ISO_CANONICAL_COLS`, and checks the result with
#' `.validate_iso_schema()`.
#'
#' @param sf_in An `sf` returned by an `.iso_via_*()` helper, with the 16
#'   columns and `osm_snapshot_date` and `osm_snapshot_status` left
#'   `NA_character_`.
#' @param snap A `list(date = chr, status = chr)` from
#'   `.resolve_osm_snapshot()`. `date` may be the string `"unknown"`;
#'   `status` is `"user_supplied"` or `"unknown_best_effort"`.
#'
#' @return The checked `sf`, with the 16 columns in that order.
#' @keywords internal
#' @noRd
.normalize_iso_schema <- function(sf_in, snap) {

  if (!inherits(sf_in, "sf")) {
    .cli_abort_schema(c(
      "{.fn .normalize_iso_schema} requires an {.cls sf} input.",
      "x" = "Got {.cls {class(sf_in)[[1]]}}.",
      "i" = "The routing helpers must return an {.cls sf} with the 16 columns of {.fn cacs_isochrone} output."
    ))
  }
  if (!is.list(snap) ||
      !all(c("date", "status") %in% names(snap))) {
    .cli_abort_schema(c(
      "{.arg snap} must be a {.cls list} with {.field date} and {.field status} elements.",
      "x" = "Got: {.val {names(snap)}}.",
      "i" = "Source: {.fn .resolve_osm_snapshot}."
    ))
  }

  out <- sf_in

  # Only NA entries are written, so a date the helper set is kept.
  snap_date_chr <- as.character(snap$date %||% NA_character_)
  snap_stat_chr <- as.character(snap$status %||% NA_character_)

  if ("osm_snapshot_date" %in% names(out)) {
    mask <- is.na(out$osm_snapshot_date)
    if (any(mask)) {
      out$osm_snapshot_date[mask] <- snap_date_chr
    }
  } else {
    out$osm_snapshot_date <- snap_date_chr
  }

  if ("osm_snapshot_status" %in% names(out)) {
    mask <- is.na(out$osm_snapshot_status)
    if (any(mask)) {
      out$osm_snapshot_status[mask] <- snap_stat_chr
    }
  } else {
    out$osm_snapshot_status <- snap_stat_chr
  }

  missing_cols <- setdiff(.ISO_CANONICAL_COLS, names(out))
  if (length(missing_cols) > 0L) {
    .cli_abort_schema(c(
      "The routing result is missing column{?s} of {.fn cacs_isochrone} output: {.field {missing_cols}}.",
      "i" = "See the Value section of {.help cacs_isochrone} for the 16 columns."
    ))
  }
  out <- out[, .ISO_CANONICAL_COLS]

  .validate_iso_schema(out, abort = TRUE)

  out
}


# %||%, with the same body in five files; the reason is in R/intersect-weight.R.
`%||%` <- function(x, y) if (is.null(x)) y else x
