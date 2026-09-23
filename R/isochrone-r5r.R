# isochrone-r5r.R - .iso_via_r5r(), the r5r arm of the provider choice in
# cacs_isochrone(). Routing with r5r is not implemented, so the body only
# stops with an error. The arm is here so that provider = "r5r" gives that
# error rather than an empty result.
#
# The error has class catchmentACS_error_credential, the class the package
# also uses when an API key is missing or refused, so a caller that catches
# that class catches this too.


#' Stop, because routing with r5r is not implemented
#'
#' The arguments match the other `.iso_via_*()` helpers, so the provider
#' choice in `cacs_isochrone()` calls this one the same way.
#'
#' @keywords internal
#' @noRd
.iso_via_r5r <- function(sites, drive_times, profile, r5r_core = NULL, ...) {
  .cli_abort_credential(c(
    "{.field provider} = {.val r5r} is deferred to a future release.",
    "i" = "Use {.val osrm} or {.val ors} in the current release.",
    "i" = "See the \"Provider status\" section of {.help cacs_isochrone}."
  ))
}
