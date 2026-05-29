# ============================================================================
# isochrone-mapbox.R - Mapbox future-release fail-loud stub (Decision #5).
#
# Current release: body is a fail-loud abort with a future-release deferral hint.
# Signature preserved so `cacs_isochrone(..., provider = "mapbox")` reaches
# this dispatch arm and aborts visibly (never silently no-ops).
#
# Class: catchmentACS_error_credential (per blueprint Sec. 38.5 verification 5;
# also routes through `.cli_abort_credential()` so callers can
# `tryCatch(catchmentACS_error_credential = ...)`).
#
# Cross-ref: Sec. 11.2 (signature lock), Sec. 38.5 (Step 3.3 stub spec),
#            Sec. 19.9 v0.2 deferral list.
# ============================================================================


#' Mapbox backend (future-release fail-loud stub)
#'
#' Body is a fail-loud abort so a user explicitly requesting
#' `provider = "mapbox"` in the current release is never silently no-op'd. Once
#' the Mapbox helper lands in a future release the body fills in; the signature stays
#' byte-identical to preserve Sec. 11.2 lock.
#'
#' @keywords internal
#' @noRd
.iso_via_mapbox <- function(sites, drive_times, profile, token = NULL, ...) {
  .cli_abort_credential(c(
    "{.field provider} = {.val mapbox} is deferred to a future release.",
    "i" = "Use {.val osrm} or {.val ors} in the current release.",
    "i" = "See {.help cacs_isochrone} for the current provider status."
  ))
}
