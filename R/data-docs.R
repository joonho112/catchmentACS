#' Ten example sites near Alabama city centers
#'
#' @description
#' Ten made-up site locations in Alabama, as an `sf` object of points. Each
#' point lies near the center of an Alabama city. The points were not taken
#' from the locations of Pre-K classrooms or of any other facility, and the
#' data contain no administrative records.
#'
#' The sites are the `sites` input of examples that need a routing service,
#' such as the one in [cacs_run()], and the default `sites_df` of the map
#' functions, such as [cacs_plot_site_isochrone()]. The bundled files
#' `legacy_2025_sites.rds` and `legacy_2025_isochrones.rds`, used by several
#' examples that run offline, give the same `site_id` values to other
#' points, 130 to 440 kilometers away.
#'
#' @format An `sf` object (a data frame with a geometry column) with 10 rows
#'   and 5 columns. The points are in longitude and latitude on WGS 84
#'   (EPSG:4326).
#' \describe{
#'   \item{`site_id`}{A string, `"AL_SITE_01"` to `"AL_SITE_10"`.}
#'   \item{`site_name`}{A string, `"Sample Site 1"` to `"Sample Site 10"`.}
#'   \item{`county_fips`}{A string giving the five-digit FIPS code of the
#'     county that contains the point, such as `"01073"` (Jefferson County)
#'     for the point near Birmingham.}
#'   \item{`region_label`}{A string naming the part of the state, such as
#'     `"Birmingham Metro"` or `"North"`; each site has a different one.}
#'   \item{`geometry`}{The point.}
#' }
#'
#' @source Made for this package from public coordinates of the centers of
#'   ten Alabama cities: Birmingham, Mobile, Huntsville, Montgomery,
#'   Tuscaloosa, Auburn, Decatur, Florence, Dothan, and Gadsden, in the order
#'   of `site_id`. Each point was then moved by a random amount of up to
#'   0.007 degrees of longitude and of latitude (less than 1 kilometer in
#'   all), drawn with a fixed random seed.
#'
#' @docType data
#' @name cacs_alabama_sites
#' @keywords datasets
#' @usage data("cacs_alabama_sites")
#' @examples
#' library(sf)
#' data("cacs_alabama_sites", package = "catchmentACS")
#' print(cacs_alabama_sites)
#' nrow(cacs_alabama_sites)               # 10
#' sf::st_crs(cacs_alabama_sites)$epsg    # 4326
#'
#' # Needs a Census API key, and routes three sites on the public OSRM demo
#' # server with the osrm package; that server limits the requests it accepts.
#' \dontrun{
#' library(sf)
#' result <- cacs_run(
#'   sites         = cacs_alabama_sites[1:3, ],
#'   state         = "AL",
#'   year          = 2023,
#'   drive_times   = c(5, 10, 15),
#'   variables     = "core",
#'   provider      = "osrm",
#'   weight_method = "area",
#'   output        = "long",
#'   verbose       = FALSE
#' )
#' head(result)
#' }
"cacs_alabama_sites"


#' ACS variable codes used by default
#'
#' @description
#' The 14 American Community Survey (ACS) variable codes that
#' [cacs_acs_prefetch()] and [cacs_run()] use when `variables` is `NULL`
#' (the default). Ten of them are the numerators and denominators of the
#' five rates in [`cacs_acs_default_rates`]. The other four are total
#' population (`B01003_001`), total households (`B11001_001`), median
#' household income (`B19013_001`), and per capita income (`B19301_001`).
#'
#' The names are short labels for the codes, such as `pov_below` for
#' `B17001_002`. The package uses only the codes, so results name each
#' variable by its code.
#'
#' @format A named character vector of length 14. Each value is an ACS
#'   variable code: `B` and five digits for the table, an underscore, and
#'   three digits, such as `B17001_002`.
#' @source The codes are from the ACS detailed tables of the U.S. Census
#'   Bureau. The variables and their names were chosen for this package.
#' @docType data
#' @name cacs_acs_default_vars
#' @keywords datasets
#' @usage data("cacs_acs_default_vars")
#' @examples
#' data("cacs_acs_default_vars", package = "catchmentACS")
#' cacs_acs_default_vars
#' names(cacs_acs_default_vars)
#' length(cacs_acs_default_vars)  # 14
"cacs_acs_default_vars"
