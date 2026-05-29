#' Synthetic Alabama Pre-K sample sites
#'
#' @description
#' A small, ready-to-use `sf` POINT layer of 10 anonymous Alabama
#' locations, supplied as example input for [cacs_run()] and the
#' worked examples throughout the documentation and vignettes. It lets
#' you exercise the full catchment pipeline without preparing your own
#' site file.
#'
#' Coordinates are synthetic: they are derived from public Alabama city
#' centroids and then perturbed with a small reproducible random jitter
#' (about 0.007 degrees, roughly 0.5 mile, per axis). This preserves the
#' rough statewide geographic spread of Pre-K activity while ensuring no
#' point corresponds to a real facility. These are **not** actual First
#' Class Pre-K facility locations and contain no administrative records.
#'
#' @format An `sf` POINT object with 10 rows and 4 attribute columns
#'   plus a geometry column. The coordinate reference system is
#'   geographic WGS 84 (EPSG:4326).
#' \describe{
#'   \item{`site_id`}{Character. Anonymous site identifier,
#'     `"AL_SITE_01"` through `"AL_SITE_10"`.}
#'   \item{`site_name`}{Character. Anonymous site label,
#'     `"Sample Site 1"` through `"Sample Site 10"`.}
#'   \item{`county_fips`}{Character. 5-digit county FIPS code of the
#'     anchor city.}
#'   \item{`region_label`}{Character. Coarse Alabama regional grouping
#'     (e.g. `"Birmingham Metro"`, `"North"`).}
#'   \item{`geometry`}{`sfc_POINT` geometry in EPSG:4326.}
#' }
#'
#' @source Synthetic coordinates derived from public Alabama city
#'   centroids (U.S. Census Bureau Gazetteer / GNIS) and perturbed with
#'   a reproducible uniform jitter. The points are placeholders for
#'   demonstration only and do not represent any real facility.
#'
#' @docType data
#' @name cacs_alabama_sites
#' @keywords datasets
#' @usage data("cacs_alabama_sites")
#' @examples
#' data("cacs_alabama_sites")
#' print(cacs_alabama_sites)
#' nrow(cacs_alabama_sites)               # 10
#' sf::st_crs(cacs_alabama_sites)$epsg    # 4326
#'
#' # Run the catchmentACS pipeline (requires network + routing provider)
#' \dontrun{
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


#' Default ACS source variables for `cacs_acs_prefetch()`
#'
#' @description
#' Named character vector of 14 American Community Survey (ACS) variable
#' codes. These are the variables [cacs_acs_prefetch()] fetches by
#' default when `variables = NULL`. The names are short human-readable
#' aliases (e.g. `"pov_below"`, `"med_hh_inc"`) and the values are the
#' corresponding Census Bureau ACS table cell codes (e.g. `"B17001_002"`).
#'
#' Together these source estimates supply the numerators, denominators,
#' and direct estimates that the package combines into its derived rate
#' indicators (see [cacs_derive_rates()]). For example, `ssi_hh`
#' (`"B19056_002"`) and `ssi_total` (`"B19056_001"`) form the
#' Supplemental Security Income rate.
#'
#' @format Named `character` vector of length 14. Each value is an ACS
#'   variable code of the form `B#####_###` (five table digits, three
#'   cell digits).
#' @source Author-curated mapping of analysis aliases to U.S. Census
#'   Bureau ACS detailed-table variable codes.
#' @docType data
#' @name cacs_acs_default_vars
#' @keywords datasets
#' @usage data("cacs_acs_default_vars")
#' @examples
#' data("cacs_acs_default_vars")
#' cacs_acs_default_vars
#' names(cacs_acs_default_vars)
#' length(cacs_acs_default_vars)  # 14
"cacs_acs_default_vars"
