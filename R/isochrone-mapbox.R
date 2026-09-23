# isochrone-mapbox.R - .iso_via_mapbox(), the Mapbox arm of the provider
# choice in cacs_isochrone(). Mapbox routing is not implemented, so the body
# only stops with an error. The arm is here so that provider = "mapbox" gives
# that error rather than an empty result.
#
# The error has class catchmentACS_error_credential, the class the package
# also uses when an API key is missing or refused, so a caller that catches
# that class catches this too.


#' Stop, because Mapbox routing is not implemented
#'
#' The arguments match the other `.iso_via_*()` helpers, so the provider
#' choice in `cacs_isochrone()` calls this one the same way.
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
