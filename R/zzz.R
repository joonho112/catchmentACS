#' zzz.R - the package load and attach hooks.
#'
#' `.onLoad()` sets the defaults of six options, each only if the user has
#' not already set it in their .Rprofile or in the session, and clears the
#' counts of cache hits and misses that the session keeps. Other options the
#' package reads, such as `catchmentACS.cache_enabled` and
#' `catchmentACS.audit_rates`, have no default set here; the call that reads
#' them gives the default. No code reads `catchmentACS.verbose`, so setting
#' it changes nothing; messages follow the `verbose` argument of each
#' function. `.onAttach()` prints nothing: attaching the package shows no
#' message.
#'
#' @keywords internal
#' @noRd
NULL

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.catchmentACS <- list(
    catchmentACS.verbose                  = TRUE,
    cacs.return_se                        = FALSE,   # an se column as well as moe
    catchmentACS.summary_per_site_max     = 12L,     # at most this many sites in print()
    catchmentACS.osrm_demo_budget_protect = TRUE,    # lower res on the demo server
    catchmentACS.capture_return_value     = "conditions",
    catchmentACS.rate_first_default       = FALSE    # TRUE: rate rows first in as_tibble()
  )
  toset <- !(names(op.catchmentACS) %in% names(op))
  if (any(toset)) options(op.catchmentACS[toset])
  .cacs_cache_reset_state()
  invisible()
}

.onAttach <- function(libname, pkgname) {
  invisible()
}
