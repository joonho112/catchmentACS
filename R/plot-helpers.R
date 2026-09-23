# plot-helpers.R - the helpers shared by the cacs_plot_site_*() functions in
# R/plot-site.R: the leaflet check, site input resolution, the default map
# view, the palettes, and the popup HTML builders.

#' Stop unless leaflet is installed
#'
#' Called first by every `cacs_plot_site_*()` function. `leaflet` is in
#' Suggests rather than Imports, so installing catchmentACS does not install
#' it, and this helper turns the missing package into one clear error.
#'
#' @return `TRUE`, invisibly, when leaflet is installed; an error otherwise.
#' @keywords internal
#' @noRd
.assert_leaflet_available <- function() {
  if (requireNamespace("leaflet", quietly = TRUE)) {
    return(invisible(TRUE))
  }
  cli::cli_abort(c(
    "Package {.pkg leaflet} is required for catchmentACS visualization functions but not installed.",
    "i" = "Install via: {.code install.packages(\"leaflet\")}",
    "i" = "All {.fn cacs_plot_site_*} functions require this dependency."
  ), class = c("catchmentACS_error_missing_suggest",
               "catchmentACS_error",
               "catchmentACS_condition"))
}


#' Read the site given to a cacs_plot_site_*() function
#'
#' Every `cacs_plot_site_*()` function takes the site in one of three forms.
#' When more than one is given, the first of these wins:
#'   1. `site_id`    - looked up in `sites_df$site_id`.
#'   2. `(lat, lon)` - used as given, with no lookup.
#'   3. `site_name`  - looked up in `sites_df$site_name`, case included, and
#'                     must match one row.
#'
#' Bad input stops through `.cli_abort_schema()`, the helper the rest of the
#' package uses for input checks, so these errors carry the same class.
#'
#' `sites_df` defaults to the bundled `cacs_alabama_sites` when `NULL`.
#'
#' @param site_id A single string, the site identifier to look up, or `NULL`.
#' @param lat,lon Single numbers giving WGS84 coordinates, used as they are
#'   (`lat` in `[-90, 90]`, `lon` in `[-180, 180]`), or `NULL`.
#' @param site_name A single string matched against `sites_df$site_name`,
#'   case included, or `NULL`.
#' @param sites_df An `sf` POINT object with the columns `site_id` and
#'   `site_name`; `NULL` uses the bundled `cacs_alabama_sites`.
#'
#' @return list with elements `site_id`, `lat`, `lon`, `name`. Unresolved
#'   fields are `NA_character_` / `NA_real_` as appropriate (e.g. when
#'   the caller passed only `(lat, lon)`, `site_id` and `name` are NA).
#' @keywords internal
#' @noRd
.cacs_resolve_site_input <- function(site_id = NULL,
                                     lat = NULL,
                                     lon = NULL,
                                     site_name = NULL,
                                     sites_df = NULL) {
  if (is.null(site_id) && is.null(lat) && is.null(lon) && is.null(site_name)) {
    .cli_abort_schema_with_template(
      "No site input provided.",
      list(.schema_issue(
        check = "plot_site_input_missing",
        col = "site_id/lat/lon/site_name",
        actual = "all NULL",
        expected = "one site input form",
        fix_hint = "Pass site_id, lat + lon, or site_name.",
        example = "cacs_plot_site_isochrone(site_id = \"AL_SITE_01\", iso_sf = iso_sf)"
      )),
      help = "Every cacs_plot_site_*() function takes the site in these three ways."
    )
  }

  # `::` reads the bundled table whether or not the package is attached, and
  # an object of the same name in the global environment is not used instead.
  if (is.null(sites_df)) {
    sites_df <- catchmentACS::cacs_alabama_sites
  }

  # The order of the three branches below sets which input wins.
  if (!is.null(site_id)) {
    if (!is.character(site_id) || length(site_id) != 1L || is.na(site_id)) {
      .cli_abort_schema(c(
        "{.arg site_id} must be a non-NA character scalar.",
        "x" = "Got {.cls {class(site_id)}} of length {length(site_id)}."
      ))
    }
    hit <- which(sites_df$site_id == site_id)
    if (length(hit) != 1L) {
      .cli_abort_schema(c(
        "{.arg site_id} {.val {site_id}} not found in {.arg sites_df}.",
        "i" = "Available site_id(s): {.val {sites_df$site_id}}."
      ))
    }
    coords <- sf::st_coordinates(sites_df[hit, ])
    return(list(
      site_id = site_id,
      lat     = as.numeric(coords[1L, "Y"]),
      lon     = as.numeric(coords[1L, "X"]),
      name    = if ("site_name" %in% names(sites_df)) {
        as.character(sites_df$site_name[hit])
      } else NA_character_
    ))
  }

  if (!is.null(lat) || !is.null(lon)) {
    if (is.null(lat) || is.null(lon)) {
      .cli_abort_schema(c(
        "{.arg lat} and {.arg lon} must both be supplied together.",
        "x" = "Got {.arg lat}={.val {lat}} / {.arg lon}={.val {lon}}."
      ))
    }
    if (!is.numeric(lat) || length(lat) != 1L || !is.finite(lat) ||
        lat < -90 || lat > 90) {
      .cli_abort_schema(c(
        "{.arg lat} must be a finite numeric scalar in [-90, 90].",
        "x" = "Got {.val {lat}}."
      ))
    }
    if (!is.numeric(lon) || length(lon) != 1L || !is.finite(lon) ||
        lon < -180 || lon > 180) {
      .cli_abort_schema(c(
        "{.arg lon} must be a finite numeric scalar in [-180, 180].",
        "x" = "Got {.val {lon}}."
      ))
    }
    return(list(
      site_id = NA_character_,
      lat     = as.numeric(lat),
      lon     = as.numeric(lon),
      name    = NA_character_
    ))
  }

  # Reached only when site_id, lat and lon are all NULL.
  if (!is.character(site_name) || length(site_name) != 1L || is.na(site_name)) {
    .cli_abort_schema(c(
      "{.arg site_name} must be a non-NA character scalar.",
      "x" = "Got {.cls {class(site_name)}} of length {length(site_name)}."
    ))
  }
  if (!"site_name" %in% names(sites_df)) {
    .cli_abort_schema(c(
      "{.arg sites_df} has no {.field site_name} column.",
      "i" = "Cannot resolve {.arg site_name} {.val {site_name}}."
    ))
  }
  hit <- which(sites_df$site_name == site_name)
  if (length(hit) == 0L) {
    .cli_abort_schema(c(
      "{.arg site_name} {.val {site_name}} not found in {.arg sites_df}.",
      "i" = "Available site_name(s): {.val {sites_df$site_name}}."
    ))
  }
  if (length(hit) > 1L) {
    matched_ids <- if ("site_id" %in% names(sites_df)) {
      as.character(sites_df$site_id[hit])
    } else as.character(hit)
    .cli_abort_schema(c(
      "{.arg site_name} {.val {site_name}} matched {length(hit)} rows in {.arg sites_df}.",
      "x" = "Ambiguous match - need a unique resolution.",
      "i" = "Disambiguate via {.arg site_id} (candidates: {.val {matched_ids}})."
    ))
  }
  coords <- sf::st_coordinates(sites_df[hit, ])
  list(
    site_id = if ("site_id" %in% names(sites_df)) {
      as.character(sites_df$site_id[hit])
    } else NA_character_,
    lat     = as.numeric(coords[1L, "Y"]),
    lon     = as.numeric(coords[1L, "X"]),
    name    = site_name
  )
}


#' Find the site nearest to a pair of plot coordinates
#'
#' `cacs_plot_site_rates()` and `cacs_plot_site_pipeline()` select the rows of
#' `run_result` by `site_id`, so a `(lat, lon)` input has to be turned into
#' one. This helper takes the nearest row of `sites_df` and returns its id and
#' coordinates. It stops when that row's `site_id` is not in `run_result`.
#'
#' @param lat,lon Single numbers giving the input coordinates in WGS84.
#' @param sites_df An `sf` object or data frame with a `site_id` column and
#'   either POINT geometry or `lon` and `lat` columns; `NULL` uses the
#'   bundled `cacs_alabama_sites`.
#' @param run_result The long table from `cacs_run()`, which must have a
#'   `site_id` column.
#'
#' @return list with `site_id`, `site_name`, `lat`, `lon`, `distance_km`, and
#'   `matched_row`.
#' @keywords internal
#' @noRd
.resolve_site_from_latlon <- function(lat, lon, sites_df = NULL, run_result) {
  if (!is.numeric(lat) || length(lat) != 1L || !is.finite(lat) ||
      lat < -90 || lat > 90) {
    .cli_abort_schema(c(
      "{.arg lat} must be a finite numeric scalar in [-90, 90].",
      "x" = "Got {.val {lat}}."
    ))
  }
  if (!is.numeric(lon) || length(lon) != 1L || !is.finite(lon) ||
      lon < -180 || lon > 180) {
    .cli_abort_schema(c(
      "{.arg lon} must be a finite numeric scalar in [-180, 180].",
      "x" = "Got {.val {lon}}."
    ))
  }
  if (!inherits(run_result, c("tbl_df", "data.frame")) ||
      !"site_id" %in% names(run_result)) {
    .cli_abort_schema(c(
      "{.arg run_result} must include a {.field site_id} column.",
      "i" = "A {.code (lat, lon)} input is matched to a {.field site_id} in {.arg run_result}."
    ))
  }
  if (is.null(sites_df)) {
    sites_df <- catchmentACS::cacs_alabama_sites
  }
  if (!inherits(sites_df, "data.frame") || !"site_id" %in% names(sites_df)) {
    .cli_abort_schema(c(
      "{.arg sites_df} must include a {.field site_id} column.",
      "i" = "Pass {.data cacs_alabama_sites} or the same sites table used by {.fn cacs_run}."
    ))
  }
  if (nrow(sites_df) == 0L) {
    .cli_abort_schema(c(
      "{.arg sites_df} has no rows.",
      "i" = "Cannot resolve `(lat, lon)` to a {.field site_id}."
    ))
  }

  if (inherits(sites_df, "sf")) {
    sites_sf <- sf::st_transform(sites_df, 4326)
  } else {
    if (!all(c("lon", "lat") %in% names(sites_df))) {
      .cli_abort_schema(c(
        "{.arg sites_df} must be an sf POINT object or include {.field lon} and {.field lat} columns.",
        "i" = "Pass {.data cacs_alabama_sites} or the original sites table used by {.fn cacs_run}."
      ))
    }
    sites_sf <- sf::st_as_sf(sites_df, coords = c("lon", "lat"),
                             crs = 4326, remove = FALSE)
  }

  coords <- sf::st_coordinates(sites_sf)
  if (nrow(coords) != nrow(sites_sf)) {
    .cli_abort_schema(c(
      "{.arg sites_df} must contain POINT geometries.",
      "i" = "Cannot resolve `(lat, lon)` from non-point site geometry."
    ))
  }

  target_sf <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
  )
  match_idx <- sf::st_nearest_feature(target_sf, sites_sf)
  matched_site_id <- as.character(sites_sf$site_id[[match_idx]])
  result_site_ids <- unique(as.character(run_result$site_id))
  if (!matched_site_id %in% result_site_ids) {
    .cli_abort_schema(c(
      "Nearest site to `(lat, lon)` is not present in {.arg run_result}.",
      "x" = "Nearest {.field site_id}: {.val {matched_site_id}}.",
      "i" = "Pass the {.arg sites_df} used for {.fn cacs_run}, or call with an explicit {.arg site_id} from {.code unique(run_result$site_id)}.",
      "i" = "When {.arg sites_df} is omitted, {.data cacs_alabama_sites} is used as the default site set."
    ))
  }

  target_5070 <- sf::st_transform(target_sf, 5070)
  sites_5070 <- sf::st_transform(sites_sf[match_idx, , drop = FALSE], 5070)
  distance_km <- as.numeric(sf::st_distance(target_5070, sites_5070)) / 1000
  site_name <- if ("site_name" %in% names(sites_sf)) {
    as.character(sites_sf$site_name[[match_idx]])
  } else {
    NA_character_
  }

  list(
    site_id = matched_site_id,
    site_name = site_name,
    lat = as.numeric(coords[match_idx, "Y"]),
    lon = as.numeric(coords[match_idx, "X"]),
    distance_km = distance_km,
    matched_row = sites_sf[match_idx, , drop = FALSE]
  )
}


#' Use the nearest site id when the plot input was (lat, lon)
#'
#' Returns `resolved` unchanged unless it holds coordinates and no `site_id`.
#' Otherwise it reports the match as a message and, when the match is farther
#' than `warning_threshold_km`, also warns.
#' @keywords internal
#' @noRd
.cacs_promote_latlon_to_site <- function(resolved,
                                         sites_df = NULL,
                                         run_result,
                                         context = "cacs_plot_site_rates",
                                         warning_threshold_km = 5) {
  if (!is.na(resolved$site_id) ||
      is.na(resolved$lat) ||
      is.na(resolved$lon)) {
    return(resolved)
  }

  input_lat <- resolved$lat
  input_lon <- resolved$lon
  hit <- .resolve_site_from_latlon(
    lat = input_lat,
    lon = input_lon,
    sites_df = sites_df,
    run_result = run_result
  )
  site_label <- if (!is.na(hit$site_name) && nzchar(hit$site_name)) {
    paste0(" (", hit$site_name, ")")
  } else {
    ""
  }

  .cli_inform_resolve_site(c(
    "Resolved plot `(lat, lon)` input to the nearest available site.",
    "i" = sprintf("Input: lat = %.6f, lon = %.6f.", input_lat, input_lon),
    "i" = sprintf("Matched: site_id = %s%s; distance = %.2f km.",
                  hit$site_id, site_label, hit$distance_km)
  ), phase = context)

  if (is.finite(hit$distance_km) && hit$distance_km > warning_threshold_km) {
    .cli_warn_resolve_site_distant(c(
      "The resolved site is more than 5 km from the requested `(lat, lon)`.",
      "x" = sprintf("Distance to matched site_id = %s%s is %.2f km.",
                    hit$site_id, site_label, hit$distance_km),
      "i" = "Verify the coordinates and pass an explicit site_id if this match is not intended."
    ), phase = context)
  }

  resolved$site_id <- hit$site_id
  resolved$name <- hit$site_name
  resolved$lat <- hit$lat
  resolved$lon <- hit$lon
  attr(resolved, "distance_km") <- hit$distance_km
  attr(resolved, "matched_row") <- hit$matched_row
  resolved
}


#' Build the default view box for leaflet::fitBounds()
#'
#' Returns a box in WGS84 degrees centered at `(lat, lon)` and padded by
#' `padding_km` on each side. The padding is converted with two
#' approximations:
#'   * 1 degree of latitude  ~ 111 km  (the same at every latitude).
#'   * 1 degree of longitude ~ 111 * cos(lat * pi/180) km
#'     (shorter toward the poles).
#'
#' They are close enough for the first view of one site, a few tens of
#' kilometers across, and not for an area that spans a continent.
#'
#' @param lat,lon Single numbers giving the center coordinates in WGS84.
#' @param padding_km A single positive number, the padding in kilometers on
#'   each side of the center, so the box spans about `2 * padding_km` from
#'   north to south.
#'
#' @return list with elements `lng1`, `lat1`, `lng2`, `lat2` suitable for
#'   `leaflet::fitBounds(map, lng1, lat1, lng2, lat2)`.
#' @keywords internal
#' @noRd
.cacs_default_view <- function(lat, lon, padding_km = 5) {
  if (!is.numeric(padding_km) || length(padding_km) != 1L ||
      !is.finite(padding_km) || padding_km <= 0) {
    .cli_abort_schema(c(
      "{.arg padding_km} must be a finite positive numeric scalar.",
      "x" = "Got {.val {padding_km}}."
    ))
  }
  if (!is.numeric(lat) || length(lat) != 1L || !is.finite(lat) ||
      lat < -90 || lat > 90) {
    .cli_abort_schema(c(
      "{.arg lat} must be a finite numeric scalar in [-90, 90].",
      "x" = "Got {.val {lat}}."
    ))
  }
  if (!is.numeric(lon) || length(lon) != 1L || !is.finite(lon) ||
      lon < -180 || lon > 180) {
    .cli_abort_schema(c(
      "{.arg lon} must be a finite numeric scalar in [-180, 180].",
      "x" = "Got {.val {lon}}."
    ))
  }
  km_per_deg_lat <- 111
  # cos(lat) reaches zero at the poles and km_per_deg_lon is a divisor two
  # lines below, so clamp it to a small positive value.
  cos_lat <- cos(lat * pi / 180)
  if (!is.finite(cos_lat) || cos_lat < 1e-6) cos_lat <- 1e-6
  km_per_deg_lon <- 111 * cos_lat
  dlat <- padding_km / km_per_deg_lat
  dlon <- padding_km / km_per_deg_lon
  list(
    lng1 = lon - dlon,
    lat1 = lat - dlat,
    lng2 = lon + dlon,
    lat2 = lat + dlat
  )
}


# Palettes and popup HTML builders for the maps.
#
# The popup builders paste values into HTML, so every value that comes from
# the data goes through htmltools::htmlEscape() and every number through
# formatC(). htmltools is in Suggests; leaflet imports it, so it is installed
# wherever a map can be drawn, and where it is missing the builders insert the
# value as it is.


#' Build the viridis palette for `area_wt`
#'
#' Returns a `leaflet::colorNumeric()` closure whose domain is `[0, 1]` rather
#' than the range of `values`, so that the same `area_wt` gives the same color
#' on every site's map. `values` is only checked (numeric, and within `[0, 1]`
#' once non-finite entries are dropped); it does not set the domain.
#'
#' @param values A numeric vector of `area_wt` values, used for that check
#'   alone.
#' @param na_color A single string giving the CSS color used for NA inputs.
#'   Default `"#CCCCCC"`, a mid gray.
#'
#' @return a `leaflet::colorNumeric()` closure that maps a numeric vector to
#'   hex colors. A value outside `[0, 1]` comes back as `na_color`, with a
#'   warning from leaflet that some values were outside the color scale.
#' @keywords internal
#' @noRd
.cacs_palette_areawt <- function(values, na_color = "#CCCCCC") {
  .assert_leaflet_available()
  if (!is.numeric(values)) {
    .cli_abort_schema(c(
      "{.arg values} must be numeric for {.fn .cacs_palette_areawt}.",
      "x" = "Got {.cls {class(values)[[1]]}}."
    ))
  }
  finite <- values[is.finite(values)]
  if (length(finite) > 0L && (min(finite) < -1e-9 || max(finite) > 1 + 1e-9)) {
    .cli_abort_schema(c(
      "{.arg values} for {.fn .cacs_palette_areawt} must lie in [0, 1].",
      "x" = "Range observed: [{min(finite)}, {max(finite)}]."
    ))
  }
  # leaflet::colorNumeric takes either a palette name or a function that
  # returns n colors. viridisLite::viridis cannot be passed here: leaflet
  # calls the function with the vector of rescaled values, and the first
  # argument of viridis() is the number of colors. The name string lets
  # leaflet look the palette up through its own viridisLite import.
  leaflet::colorNumeric(
    palette  = "viridis",
    domain   = c(0, 1),
    na.color = na_color
  )
}


#' Build the ColorBrewer palette for one variable
#'
#' Returns a `leaflet::colorBin()` closure whose ColorBrewer palette follows
#' `variable_family`, through the `switch()` below. The breaks come from
#' `classInt::classIntervals(style = "jenks")` when the values hold five or
#' more distinct finite numbers and classInt is installed, and from `pretty()`
#' otherwise. With two to four distinct values `pretty()` is asked for that
#' many bins rather than five; with fewer than two the palette gets a single
#' bin around the value, or around zero when no finite value is left.
#'
#' @param values A numeric vector of the estimates to color, one per tract on
#'   the variable map. Non-finite values are dropped before the breaks are
#'   computed, and `NA` given to the returned closure comes back as
#'   `na_color`.
#' @param variable_family A single `estimand_family` value
#'   (`.ESTIMAND_FAMILIES`) or `NA`. `"metadata_only"` gives an error; `NA`
#'   and any other unknown value take the `"YlGnBu"` arm of the `switch()`.
#' @param na_color A single string giving the CSS color used for NA inputs.
#'   Default `"#CCCCCC"`.
#'
#' @return a `leaflet::colorBin()` palette closure.
#' @keywords internal
#' @noRd
.cacs_palette_chloropleth <- function(values,
                                      variable_family,
                                      na_color = "#CCCCCC") {
  .assert_leaflet_available()

  if (!is.numeric(values)) {
    .cli_abort_schema(c(
      "{.arg values} must be numeric for {.fn .cacs_palette_chloropleth}.",
      "x" = "Got {.cls {class(values)[[1]]}}."
    ))
  }
  if (length(variable_family) > 1L) {
    .cli_abort_schema(c(
      "{.arg variable_family} must be a length-1 character (or NA).",
      "x" = "Got length {length(variable_family)}."
    ))
  }

  if (!is.na(variable_family) &&
      identical(as.character(variable_family), "metadata_only")) {
    .cli_abort_schema(c(
      "{.arg variable_family} {.val metadata_only} cannot be mapped.",
      "x" = "Rows of that kind have {.code NA} estimates.",
      "i" = "Choose a {.arg variable} that is a count, a median, a per-person value, or a rate."
    ))
  }

  brewer_pal <- switch(
    if (is.na(variable_family)) "_na_" else as.character(variable_family),
    "spatial_total"                    = "Greens",
    "area_weighted_scalar_proxy"       = "YlOrRd",
    "population_weighted_scalar_proxy" = "YlOrRd",
    "area_weighted_rate_proxy"         = "YlOrRd",
    "median_proxy"                     = "PuOr",
    "derived_rate"                     = "RdYlBu",
    # The last, unnamed arm covers NA and any other value.
    "YlGnBu"
  )

  finite <- values[is.finite(values)]
  uniq_n <- length(unique(finite))

  n_bins <- if (uniq_n < 5L) max(uniq_n, 1L) else 5L

  bins <- NULL
  if (uniq_n >= 2L) {
    # classInt is in Suggests, not Imports, so the jenks breaks are used only
    # where it is installed and pretty() covers the rest.
    if (uniq_n >= 5L && requireNamespace("classInt", quietly = TRUE)) {
      bins <- tryCatch(
        suppressWarnings({
          ci <- classInt::classIntervals(finite, n = n_bins, style = "jenks")
          unique(as.numeric(ci$brks))
        }),
        error = function(e) NULL
      )
    }
    if (is.null(bins) || length(bins) < 2L) {
      bins <- unique(pretty(finite, n = n_bins))
    }
    if (length(bins) < 2L) {
      # colorBin() reads a length-1 `bins` as the number of bins, not as a
      # break, so pass two breaks around the value instead.
      eps  <- max(abs(finite)) * 1e-6 + 1e-9
      bins <- c(finite[1L] - eps, finite[1L] + eps)
    }
  } else {
    # One distinct finite value, or none: a single bin around it.
    base <- if (uniq_n == 1L) finite[[1L]] else 0
    eps  <- abs(base) * 1e-6 + 1e-9
    bins <- c(base - eps, base + eps)
  }

  leaflet::colorBin(
    palette  = brewer_pal,
    domain   = finite,
    bins     = bins,
    na.color = na_color,
    pretty   = FALSE
  )
}


#' Fill the `{{field}}` markers of a popup template
#'
#' Replaces every `{{field}}` in `template` with the matching value of the
#' named list `data`. It escapes nothing, so the popup builders keep the
#' choice of which fields are escaped. A marker with no matching name is
#' replaced with `""`, which lets `data` leave optional fields out.
#'
#' @param template A single string holding the raw HTML template.
#' @param data A named list whose values each turn into one string through
#'   `as.character()`.
#' @return character(1), the filled template.
#' @keywords internal
#' @noRd
.fill_template <- function(template, data) {
  if (length(template) != 1L || !is.character(template)) {
    .cli_abort_schema("{.fn .fill_template} requires a single character(1) template.")
  }
  if (!is.list(data) || (length(data) > 0L && is.null(names(data)))) {
    .cli_abort_schema("{.fn .fill_template} requires a named list for {.arg data}.")
  }
  out <- template
  for (nm in names(data)) {
    pat <- paste0("\\{\\{\\s*", nm, "\\s*\\}\\}")
    val <- as.character(data[[nm]])
    if (length(val) == 0L || is.na(val)) val <- ""
    out <- gsub(pat, val, out, perl = TRUE)
  }
  # Drop the markers that nothing filled, so the popup does not show them.
  out <- gsub("\\{\\{\\s*[A-Za-z0-9_]+\\s*\\}\\}", "", out, perl = TRUE)
  out
}


#' Extract a named <!--BEGIN name-->...<!--END name--> block
#'
#' Takes one section out of the template file. `inst/templates/popup.html`
#' holds three sections, `"site"`, `"tract"` and `"rate"`, marked off by HTML
#' comments. The section name goes into a regular expression, so it is
#' restricted to `[A-Za-z0-9_-]+`.
#'
#' @param raw A single string holding the whole template file.
#' @param section A single string naming the section.
#' @return character(1) section body.
#' @keywords internal
#' @noRd
.extract_template_section <- function(raw, section) {
  if (!is.character(section) || length(section) != 1L ||
      !grepl("^[A-Za-z0-9_-]+$", section)) {
    .cli_abort_schema(c(
      "Bad popup template section name {.val {section}}.",
      "i" = "Allowed: {.val site}, {.val tract}, {.val rate}."
    ))
  }
  pat <- paste0("<!--BEGIN\\s+", section, "-->([\\s\\S]*?)<!--END\\s+",
                section, "-->")
  m <- regmatches(raw, regexec(pat, raw, perl = TRUE))[[1L]]
  if (length(m) < 2L) {
    .cli_abort_schema(c(
      "Popup template section {.val {section}} not found.",
      "i" = "The installed popup template is incomplete; reinstall {.pkg catchmentACS}."
    ))
  }
  m[[2L]]
}


#' Read one popup template section and fill it
#'
#' Finds `inst/templates/popup.html` with `system.file()`, takes the section
#' out with `.extract_template_section()`, and fills it with
#' `.fill_template()`. The values in `data` are escaped by the caller, the
#' three popup builders below.
#'
#' @param template_name A single string naming the section, one of `"site"`,
#'   `"tract"`, or `"rate"`.
#' @param data A named list of single strings, already escaped.
#'
#' @return character(1) HTML for `leaflet::addPopups()` or
#'   `leaflet::addMarkers(popup = ...)`.
#' @keywords internal
#' @noRd
.render_popup <- function(template_name, data) {
  path <- system.file("templates", "popup.html", package = "catchmentACS")
  if (!nzchar(path) || !file.exists(path)) {
    .cli_abort_schema(c(
      "Popup template file {.file inst/templates/popup.html} not found.",
      "i" = "The template is part of the package; reinstall {.pkg catchmentACS}."
    ))
  }
  raw     <- paste(readLines(path, warn = FALSE), collapse = "\n")
  section <- .extract_template_section(raw, template_name)
  .fill_template(section, data)
}


#' Build the site-marker popup HTML
#'
#' Calls `.render_popup("site", ...)` with the text fields escaped by
#' `htmltools::htmlEscape()` and the coordinates formatted by `formatC()`.
#' A field that is `NULL` or `NA` is shown as an empty string or as `n/a`.
#'
#' @param site_row A named list, or a one-row data frame or tibble, with at
#'   least `site_id`, `site_name`, `lat`, and `lon`.
#' @return character(1) HTML.
#' @keywords internal
#' @noRd
.cacs_popup_site <- function(site_row) {
  esc <- function(x) {
    if (!requireNamespace("htmltools", quietly = TRUE)) {
      return(as.character(.coalesce_na(x, "")))
    }
    htmltools::htmlEscape(as.character(.coalesce_na(x, "")), attribute = FALSE)
  }
  fnum <- function(x, digits = 4L) {
    if (is.null(x) || !is.numeric(x) || !is.finite(x)) return("n/a")
    formatC(x, digits = digits, format = "f")
  }
  data <- list(
    site_id   = esc(site_row[["site_id"]]),
    site_name = esc(.coalesce_na(site_row[["site_name"]], NA_character_)),
    lat       = fnum(site_row[["lat"]], digits = 5L),
    lon       = fnum(site_row[["lon"]], digits = 5L)
  )
  .render_popup("site", data)
}


#' Build the tract polygon popup HTML
#'
#' Builds one popup for one tract row. `GEOID`, `area_wt` and
#' `intersection_km2` are always shown, each as `n/a` when the row has no
#' such field; the table of variables is added only when `vars` is not empty.
#'
#' @param tract_row A named list, or a one-row data frame, with at least
#'   `GEOID`, `area_wt`, and any variable columns named in `vars`. A missing
#'   `intersection_km2` is shown as `n/a`.
#' @param vars A character vector of the variable columns to show, one row
#'   each in the popup body. Default `character(0)`, no variable rows.
#' @return character(1) HTML.
#' @keywords internal
#' @noRd
.cacs_popup_tract <- function(tract_row, vars = character(0)) {
  esc <- function(x) {
    if (!requireNamespace("htmltools", quietly = TRUE)) {
      return(as.character(.coalesce_na(x, "")))
    }
    htmltools::htmlEscape(as.character(.coalesce_na(x, "")), attribute = FALSE)
  }
  fnum <- function(x, digits = 4L) {
    if (is.null(x) || !is.numeric(x) || !is.finite(x)) return("n/a")
    formatC(x, digits = digits, format = "f")
  }
  rows_html <- ""
  if (length(vars) > 0L) {
    rows_html <- paste(
      vapply(vars, function(v) {
        val <- tract_row[[v]]
        sprintf(
          "<tr><td>%s</td><td>%s</td></tr>",
          esc(v),
          if (is.numeric(val)) fnum(val) else esc(val)
        )
      }, character(1)),
      collapse = ""
    )
    rows_html <- paste0("<table class=\"cacs-popup-vars\">", rows_html,
                        "</table>")
  }
  data <- list(
    geoid            = esc(tract_row[["GEOID"]]),
    area_wt          = fnum(tract_row[["area_wt"]], digits = 4L),
    intersection_km2 = fnum(tract_row[["intersection_km2"]], digits = 4L),
    variable_rows    = rows_html  # HTML built above from escaped values
  )
  .render_popup("tract", data)
}


#' Build the per-site rate popup HTML
#'
#' Builds one popup for one rate row: the estimate, its margin of error, the
#' `n_tracts_num` and `n_tracts_den` counts, the formula used, and
#' `moe_fallback` written out as "fallback applied", "no fallback" or "n/a",
#' so the reader does not have to know the column's values.
#'
#' @param rate_row A named list, or a one-row data frame, with `variable`,
#'   `estimate`, `moe`, `n_tracts_num`, `n_tracts_den`, `moe_fallback`, and
#'   `moe_formula_effective`.
#' @return character(1) HTML.
#' @keywords internal
#' @noRd
.cacs_popup_rate <- function(rate_row) {
  esc <- function(x) {
    if (!requireNamespace("htmltools", quietly = TRUE)) {
      return(as.character(.coalesce_na(x, "")))
    }
    htmltools::htmlEscape(as.character(.coalesce_na(x, "")), attribute = FALSE)
  }
  fnum <- function(x, digits = 4L) {
    if (is.null(x) || !is.numeric(x) || !is.finite(x)) return("n/a")
    formatC(x, digits = digits, format = "f")
  }
  fnt <- function(x) {
    if (is.null(x) || !is.finite(as.numeric(x))) return("n/a")
    formatC(as.integer(x), format = "d")
  }
  fallback <- rate_row[["moe_fallback"]]
  fallback_str <- if (isTRUE(fallback)) {
    "fallback applied"
  } else if (identical(fallback, FALSE)) {
    "no fallback"
  } else {
    "n/a"
  }
  data <- list(
    variable      = esc(rate_row[["variable"]]),
    estimate      = fnum(rate_row[["estimate"]], digits = 4L),
    moe           = fnum(rate_row[["moe"]],      digits = 4L),
    n_tracts_num  = fnt(rate_row[["n_tracts_num"]]),
    n_tracts_den  = fnt(rate_row[["n_tracts_den"]]),
    moe_fallback  = esc(fallback_str),
    moe_formula   = esc(.coalesce_na(rate_row[["moe_formula_effective"]],
                                     "n/a"))
  )
  .render_popup("rate", data)
}


#' Replace NULL or a single NA, for the popup builders
#'
#' The package's `%||%`, defined with the same body in several files
#' (`R/isochrone-normalize.R`, `R/intersect-weight.R` and others), replaces
#' `NULL` alone. This helper also replaces a length-1 `NA`, and carries an
#' ordinary name so that it does not collide with that operator.
#'
#' @param a The value to test.
#' @param b The value to use when `a` is `NULL` or a length-1 `NA`.
#' @keywords internal
#' @noRd
.coalesce_na <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 1L && is.na(a)) return(b)
  a
}
