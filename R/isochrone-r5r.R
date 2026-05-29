# ============================================================================
# isochrone-r5r.R - r5r future-release fail-loud stub (Decision #5).
#
# Current release: body is a fail-loud abort with a future-release deferral hint.
# Signature preserved so `cacs_isochrone(..., provider = "r5r")` reaches
# this dispatch arm and aborts visibly (never silently no-ops).
#
# Class: catchmentACS_error_credential (per blueprint Sec. 38.5 verification 6;
# also routes through `.cli_abort_credential()` so callers can
# `tryCatch(catchmentACS_error_credential = ...)`).
#
# Cross-ref: Sec. 11.2 (signature lock), Sec. 38.5 (Step 3.3 stub spec),
#            Sec. 19.9 v0.2 deferral list, Sec. 14 (r5r in Suggests).
# ============================================================================


#' r5r backend (future-release fail-loud stub)
#'
#' Body is a fail-loud abort so a user explicitly requesting
#' `provider = "r5r"` in the current release is never silently no-op'd. Once the
#' r5r helper lands in a future release the body fills in; the signature stays
#' byte-identical to preserve Sec. 11.2 lock.
#'
#' @keywords internal
#' @noRd
.iso_via_r5r <- function(sites, drive_times, profile, r5r_core = NULL, ...) {
  .cli_abort_credential(c(
    "{.field provider} = {.val r5r} is deferred to a future release.",
    "i" = "Use {.val osrm} or {.val ors} in the current release.",
    "i" = "r5r needs a 4-8 GB RAM core built via {.fn r5r::setup_r5}; see {.help cacs_isochrone} for the current provider status."
  ))
}
