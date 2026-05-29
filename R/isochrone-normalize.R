# ============================================================================
# isochrone-normalize.R - `.normalize_iso_schema()` helper (Step 3.4 of Phase 3).
#
# Implements Sec. 19.3 step 9 (canonical 16-col assembly) as a *forward-compat
# seam* on top of the 16-col sf that the Step 3.2/3.3 `.iso_via_*()` helpers
# already emit. Two responsibilities:
#
#   1. Merge `snap$date` / `snap$status` (from `.resolve_osm_snapshot()` in
#      Step 3.1) into the `osm_snapshot_date` / `osm_snapshot_status` columns
#      that the backend helpers leave as `NA_character_`. Sec. 19.5 row 6 + row 13
#      treat these as *row-level provenance* and an `NA` here would defeat the
#      "unknown" / "explicit" / "user_supplied" sentinel space described in
#      Sec. 19.5 footnote ("NA means no value; unknown means best-effort lookup
#      failed to confirm").
#
#   2. Validate the result via `.validate_iso_schema()` (Phase 2.3 / checks.R),
#      so a v0.2 backend that drifts from the 16-col contract fails-loud here
#      instead of downstream in Sec. 21 / Sec. 22.
#
# The helper is intentionally tolerant of the (hypothetical) case where a
# backend has already set the columns to non-NA - we only overwrite values
# that are NA so that user-supplied or backend-detected snapshot data is not
# clobbered.
#
# Cross-ref: Sec. 15.3 row 13, Sec. 19.3 step 9, Sec. 19.5 16-col canonical contract,
#            Sec. 19.6 cache payload row 6 ("unknown" sentinel preserved in key
#            space), Sec. 38.6 (Step 3.4 spec).
# ============================================================================


# ---- Sec. 19.5 canonical column order (single source of truth) -----------------

.ISO_CANONICAL_COLS <- c(
  "site_id", "drive_time_min", "geometry", "provider", "profile",
  "osm_snapshot_date", "routing_engine_version",
  "polygon_simplification_tolerance", "generated_at",
  "isochrone_empty", "provider_requested", "provider_downgrade",
  "osm_snapshot_status", "failure_reason", "retry_count",
  "ring_topology"
)


#' Normalize an isochrone backend sf to the Sec. 19.5 16-col canonical schema
#'
#' Merges `snap$date` / `snap$status` into the snapshot provenance columns
#' (only when the backend left them `NA`), reorders to the canonical column
#' sequence, and validates via `.validate_iso_schema()`.
#'
#' @param sf_in An `sf` returned by a `.iso_via_*()` helper. Expected to
#'   already carry the 16-col schema with `osm_snapshot_date` and
#'   `osm_snapshot_status` left `NA_character_`.
#' @param snap A `list(date = chr, status = chr)` from `.resolve_osm_snapshot()`
#'   (Step 3.1). `date` may be the literal string `"unknown"`; `status` is
#'   one of `"explicit"`, `"unknown_best_effort"`, `"user_supplied"`.
#'
#' @return The validated `sf` with Sec. 19.5 16-col canonical schema.
#' @keywords internal
#' @noRd
.normalize_iso_schema <- function(sf_in, snap) {

  # --- Pre-conditions ------------------------------------------------------
  if (!inherits(sf_in, "sf")) {
    .cli_abort_schema(c(
      "{.fn .normalize_iso_schema} requires an {.cls sf} input.",
      "x" = "Got {.cls {class(sf_in)[[1]]}}.",
      "i" = "Backend `.iso_via_*()` helpers must return the 16-col canonical sf."
    ))
  }
  if (!is.list(snap) ||
      !all(c("date", "status") %in% names(snap))) {
    .cli_abort_schema(c(
      "{.arg snap} must be a {.cls list} with {.field date} and {.field status} elements.",
      "x" = "Got: {.val {names(snap)}}.",
      "i" = "Source: {.fn .resolve_osm_snapshot} (Step 3.1)."
    ))
  }

  out <- sf_in

  # --- Merge snapshot provenance (only where backend left NA) --------------
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

  # --- Re-order to canonical 16-col sequence (geometry at pos 3) -----------
  missing_cols <- setdiff(.ISO_CANONICAL_COLS, names(out))
  if (length(missing_cols) > 0L) {
    .cli_abort_schema(c(
      "Backend output missing canonical column{?s}: {.field {missing_cols}}.",
      "i" = "See {.help cacs_isochrone} (Sec. 19.5 canonical 16-column schema)."
    ))
  }
  out <- out[, .ISO_CANONICAL_COLS]

  # --- Validate (Sec. 19.5 + Sec. 12.2.2) ------------------------------------------
  .validate_iso_schema(out, abort = TRUE)

  out
}


# Local %||% - duplicated from isochrone-dispatch.R to keep file standalone.
`%||%` <- function(x, y) if (is.null(x)) y else x
