#' Package load and attach hooks
#'
#' `.onLoad()` seeds the two catchmentACS-owned option defaults
#' (`catchmentACS.verbose`, `cacs.return_se`) without clobbering values the
#' user has already set in their .Rprofile or session. `.onAttach()` is
#' deliberately silent per Sec. 14.7 - no welcome banner, no startup message.
#'
#' @keywords internal
#' @noRd
NULL

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.catchmentACS <- list(
    # v0.1/v0.3 options (kept)
    catchmentACS.verbose                  = TRUE,    # Sec. 13.5 default chatty cli
    cacs.return_se                        = FALSE,   # Sec. 22.2 default: moe not se
    # v0.4 NEW options (plan §sec-step-1-1)
    catchmentACS.summary_per_site_max     = 12L,     # Issue 004 — auto-print gate
    catchmentACS.osrm_demo_budget_protect = TRUE,    # Issue 002 — demo-safe default
    catchmentACS.capture_return_value     = "conditions", # Issue 001 — back-compat default
    catchmentACS.rate_first_default       = TRUE     # Issue 004 — as_tibble rate-first
  )
  toset <- !(names(op.catchmentACS) %in% names(op))
  if (any(toset)) options(op.catchmentACS[toset])
  .cacs_cache_reset_state()
  invisible()
}

.onAttach <- function(libname, pkgname) {
  # Sec. 14.7 - silent attach. No packageStartupMessage() in v1.0; the
  # decision against a welcome banner is final for this release.
  invisible()
}
