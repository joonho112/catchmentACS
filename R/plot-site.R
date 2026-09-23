# plot-site.R - the four maps of one site, and the function that builds all
# four at once.
#
# Each map function returns one leaflet widget and can be called on its own:
# it finds the site with .cacs_resolve_site_input() (R/plot-helpers.R) and
# takes the data it draws as arguments. Each one asks for the leaflet package
# before anything else, so a user without it gets the install hint rather than
# an error from somewhere further in.


#' Map a site and its drive-time areas (map 1 of 4)
#'
#' @description
#' Draws an interactive `leaflet` map of one site and its drive-time areas
#' (isochrones), the areas reachable from the site within each drive time.
#' It is the first of the four maps that [cacs_plot_site_pipeline()]
#' builds.
#'
#' @details
#' The site is drawn as a point. Each drive time in the `drive_time_min`
#' column of `iso_sf` has its own color, shown in a legend; without that
#' column, all the areas are drawn in one color.
#'
#' @param site_id A string giving the `site_id` of the site to map, used to
#'   find its point in `sites_df` and its drive-time areas in `iso_sf`.
#' @param lat,lon Single numbers giving the latitude and longitude of a
#'   point in degrees (WGS 84), used when `site_id` is `NULL`. Both must be
#'   given. The point is marked at that location, and the areas of every
#'   site in `iso_sf` are used.
#' @param site_name A string giving the name of the site, compared with the
#'   `site_name` column of `sites_df` including case, and used when
#'   `site_id`, `lat`, and `lon` are `NULL`. It must match one row.
#' @param iso_sf An `sf` object of drive-time areas in EPSG:4326 (longitude
#'   and latitude), such as the result of [cacs_isochrone()], with the
#'   columns `site_id` and `drive_time_min`.
#' @param sites_df An `sf` object of site points in longitude and latitude
#'   with a `site_id` column, or `NULL` (the default) for
#'   [`cacs_alabama_sites`]. The bundled file `legacy_2025_isochrones.rds`
#'   uses the same `site_id` values for other places, so with its areas the
#'   default puts the point and the initial view far from them.
#' @param tiles A string naming the background map, one of the tile
#'   providers in `leaflet::providers`; the default is `"OpenStreetMap"`, the
#'   standard OpenStreetMap map, which needs no API key. The map is built
#'   offline, and the tiles are downloaded when it is displayed. The CARTO
#'   tiles, such as `"CartoDB.Positron"`, need an API key, which these
#'   functions do not send, so they show a notice asking for one.
#' @param padding_km A single positive number. The initial view shows at
#'   least this many kilometers on each side of the site; the default is 5.
#' @param ... Not used. Any argument given here is ignored.
#'
#' @return A `leaflet` map (an HTML widget).
#' @family maps of one site
#' @seealso `vignette("visual-walkthrough", package = "catchmentACS")`
#'   (<https://joonho112.github.io/catchmentACS/articles/visual-walkthrough.html>)
#'   builds all four maps for one site, one section each.
#' @export
#' @examples
#' if (requireNamespace("leaflet", quietly = TRUE)) {
#'   # Example data bundled with the package for one site in Birmingham,
#'   # Alabama: a 10-minute drive-time area from the OSRM routing service,
#'   # 2019-2023 ACS estimates for nearby tracts, and the cacs_run() result.
#'   fx <- readRDS(system.file("extdata", "visual_walkthrough_fixture.rds",
#'                             package = "catchmentACS"))
#'   cacs_plot_site_isochrone(site_id = "AL_BHM_01", iso_sf = fx$iso_sf,
#'                            sites_df = fx$sites_df)
#' }
cacs_plot_site_isochrone <- function(site_id = NULL, lat = NULL, lon = NULL,
                                     site_name = NULL, iso_sf,
                                     sites_df = NULL,
                                     tiles = "OpenStreetMap",
                                     padding_km = 5, ...) {
  .assert_leaflet_available()

  resolved <- .cacs_resolve_site_input(site_id, lat, lon, site_name, sites_df)

  # A point given as (lat, lon) has no site_id to filter on, so every area in
  # iso_sf is drawn and the caller chooses which rows to pass.
  if (!is.na(resolved$site_id)) {
    if (!"site_id" %in% names(iso_sf)) {
      .cli_abort_schema(c(
        "{.arg iso_sf} has no {.field site_id} column.",
        "i" = "Cannot filter to {.val {resolved$site_id}}."
      ))
    }
    site_iso <- iso_sf[iso_sf$site_id == resolved$site_id, ]
    if (nrow(site_iso) == 0L) {
      .cli_abort_schema(c(
        "No isochrone found in {.arg iso_sf} for site_id = {.val {resolved$site_id}}.",
        "i" = "Check {.code unique(iso_sf$site_id)} for available sites."
      ))
    }
  } else {
    site_iso <- iso_sf
  }

  bbox <- .cacs_default_view(resolved$lat, resolved$lon,
                             padding_km = padding_km)

  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles(tiles) |>
    leaflet::fitBounds(lng1 = bbox$lng1, lat1 = bbox$lat1,
                       lng2 = bbox$lng2, lat2 = bbox$lat2)

  if ("drive_time_min" %in% names(site_iso)) {
    dt_levels <- sort(unique(site_iso$drive_time_min))
    pal <- leaflet::colorFactor("viridis", domain = dt_levels,
                                reverse = FALSE)
    for (dt in dt_levels) {
      ring <- site_iso[site_iso$drive_time_min == dt, ]
      m <- leaflet::addPolygons(
        m, data = ring,
        color = pal(dt), weight = 2, opacity = 0.8,
        fillColor = pal(dt), fillOpacity = 0.25,
        label = sprintf("%d-min isochrone", dt),
        group = sprintf("iso_%d_min", dt)
      )
    }
    m <- leaflet::addLegend(m, "bottomright", pal = pal,
                            values = dt_levels,
                            title = "Drive time (min)", opacity = 0.8)
  } else {
    m <- leaflet::addPolygons(
      m, data = site_iso,
      color = "#3182bd", weight = 2, opacity = 0.8,
      fillColor = "#3182bd", fillOpacity = 0.25,
      label = "isochrone"
    )
  }

  site_popup <- .cacs_popup_site(list(
    site_id   = resolved$site_id,
    site_name = resolved$name,
    lat       = resolved$lat,
    lon       = resolved$lon
  ))
  m <- leaflet::addCircleMarkers(
    m, lng = resolved$lon, lat = resolved$lat,
    radius = 6, color = "#e34a33", fillColor = "#e34a33",
    fillOpacity = 0.9, stroke = FALSE,
    popup = site_popup,
    label = if (!is.na(resolved$name)) resolved$name else resolved$site_id
  )

  m
}


#' Map the tracts that overlap a drive-time area (map 2 of 4)
#'
#' @description
#' Draws an interactive `leaflet` map of one drive-time area of a site and
#' the census tracts it overlaps. The part of each tract inside the area is
#' shaded by the tract's coverage weight, labeled `area_wt` on the map: the
#' share of the tract's area that lies inside the drive-time area. The color
#' scale runs from 0 to 1 on every map, so a weight has the same color on
#' all of them. The outline of the area and the site's point are drawn on
#' top.
#'
#' @details
#' [cacs_intersect_weight()] adds up counts with the same coverage weights,
#' and combines medians and per-person values with area shares instead (each
#' tract's share of the total overlap area). This function computes the
#' overlaps itself from `iso_sf` and `tract_sf`, measuring areas in the
#' equal-area projection EPSG:5070 as [cacs_intersect_weight()] does, but
#' without its `min_weight` cut-off.
#'
#' Clicking a tract shows its `GEOID` (the tract's Census identifier), its
#' coverage weight, and the area of its part inside the drive-time area in
#' square kilometers (`intersection_km2`).
#'
#' With `lat` and `lon` instead of `site_id`, the rows of `iso_sf` are not
#' narrowed to one site. The drive time is then chosen among the drive times
#' of all the sites in `iso_sf`, and the area of every site for that drive
#' time is drawn, so a map of one area needs an `iso_sf` that holds the rows
#' of one site. [cacs_plot_site_rates()] and [cacs_plot_site_pipeline()]
#' differ: they map the site in `sites_df` nearest to the point.
#'
#' @inheritParams cacs_plot_site_isochrone
#' @param tract_sf An `sf` object of census tract polygons with a `GEOID`
#'   column, such as the result of [cacs_acs_prefetch()]. When a tract has
#'   several rows, as in that result (one for each variable), the first row
#'   is used.
#' @param drive_time_min A single number giving the drive time, in minutes,
#'   of the area to map, or `NULL` (the default) for the longest drive time
#'   of the site in `iso_sf`. It must be one of the drive times in `iso_sf`.
#'
#' @return A `leaflet` map (an HTML widget).
#' @family maps of one site
#' @seealso `vignette("visual-walkthrough", package = "catchmentACS")`
#'   (<https://joonho112.github.io/catchmentACS/articles/visual-walkthrough.html>)
#'   builds all four maps for one site, one section each.
#' @export
#' @examples
#' if (requireNamespace("leaflet", quietly = TRUE)) {
#'   # Example data bundled with the package for one site in Birmingham,
#'   # Alabama: a 10-minute drive-time area from the OSRM routing service,
#'   # 2019-2023 ACS estimates for nearby tracts, and the cacs_run() result.
#'   fx <- readRDS(system.file("extdata", "visual_walkthrough_fixture.rds",
#'                             package = "catchmentACS"))
#'   cacs_plot_site_intersection(site_id = "AL_BHM_01", iso_sf = fx$iso_sf,
#'                               tract_sf = fx$tract_sf,
#'                               sites_df = fx$sites_df)
#' }
cacs_plot_site_intersection <- function(site_id = NULL, lat = NULL, lon = NULL,
                                        site_name = NULL,
                                        iso_sf, tract_sf,
                                        sites_df = NULL,
                                        tiles = "OpenStreetMap",
                                        padding_km = 5,
                                        drive_time_min = NULL, ...) {
  .assert_leaflet_available()

  resolved <- .cacs_resolve_site_input(site_id, lat, lon, site_name, sites_df)

  if (!is.na(resolved$site_id)) {
    if (!"site_id" %in% names(iso_sf)) {
      .cli_abort_schema(c(
        "{.arg iso_sf} has no {.field site_id} column.",
        "i" = "Cannot filter to {.val {resolved$site_id}}."
      ))
    }
    site_iso <- iso_sf[iso_sf$site_id == resolved$site_id, ]
    if (nrow(site_iso) == 0L) {
      .cli_abort_schema(c(
        "No isochrone found in {.arg iso_sf} for site_id = {.val {resolved$site_id}}.",
        "i" = "Check {.code unique(iso_sf$site_id)} for available sites."
      ))
    }
  } else {
    site_iso <- iso_sf
  }
  if ("drive_time_min" %in% names(site_iso)) {
    dt_levels <- sort(unique(site_iso$drive_time_min))
    dt_pick <- if (is.null(drive_time_min)) {
      max(dt_levels)
    } else {
      if (!drive_time_min %in% dt_levels) {
        .cli_abort_schema(c(
          "{.arg drive_time_min} = {.val {drive_time_min}} not found in {.arg iso_sf}.",
          "i" = "Available: {.val {dt_levels}}."
        ))
      }
      drive_time_min
    }
    site_iso <- site_iso[site_iso$drive_time_min == dt_pick, ]
  } else {
    dt_pick <- NA_integer_
  }

  # tract_sf has one row for each tract and variable, so the same geometry
  # comes several times; one row for each GEOID is enough to draw and to
  # intersect.
  if (!"GEOID" %in% names(tract_sf)) {
    .cli_abort_schema_with_template(
      "{.arg tract_sf} has no {.field GEOID} column.",
      list(.schema_issue(
        check = "plot_tract_geoid_missing",
        col = "GEOID",
        actual = "missing",
        expected = "tract GEOID column",
        fix_hint = "Pass the same acs_sf object used for cacs_intersect_weight().",
        example = "tract_sf <- acs_sf"
      )),
      help = "cacs_plot_site_intersection() joins the intersection geometry to the tracts by GEOID."
    )
  }
  tract_geom <- tract_sf[!duplicated(tract_sf$GEOID), c("GEOID")]

  # Areas are measured in EPSG:5070, an equal-area projection in meters, as in
  # `cacs_intersect_weight()`, so the share shown here is that step's coverage
  # weight, computed again without its geometry repair and its `min_weight`
  # filter.
  iso_5070   <- sf::st_transform(site_iso,   5070)
  tract_5070 <- sf::st_transform(tract_geom, 5070)
  tract_5070$tract_area_m2 <- as.numeric(sf::st_area(tract_5070))

  # st_agr() says that the columns hold for every part of a shape, which is
  # what st_intersection() assumes; without it the call warns that it does.
  sf::st_agr(tract_5070) <- "constant"
  sf::st_agr(iso_5070)   <- "constant"
  inter <- suppressWarnings(sf::st_intersection(tract_5070, iso_5070))
  if (nrow(inter) == 0L) {
    .cli_abort_schema(c(
      "Isochrone does not intersect any tract in {.arg tract_sf}.",
      "i" = "Check coverage of {.code sf::st_bbox(tract_sf)} vs {.code sf::st_bbox(iso_sf)}."
    ))
  }
  inter$intersection_km2 <- as.numeric(sf::st_area(inter)) / 1e6
  inter$area_wt <- (inter$intersection_km2 * 1e6) / inter$tract_area_m2
  # Slivers at the edge of the area, and rows whose share cannot be computed,
  # are dropped; rounding can put a share just outside [0, 1].
  inter <- inter[is.finite(inter$area_wt) & inter$area_wt > 0, ]
  inter$area_wt <- pmin(pmax(inter$area_wt, 0), 1)

  # leaflet draws longitude and latitude, so the shapes go back to EPSG:4326.
  inter_wgs  <- sf::st_transform(inter,    4326)
  iso_outline <- sf::st_transform(site_iso, 4326)

  bbox <- .cacs_default_view(resolved$lat, resolved$lon,
                             padding_km = padding_km)

  pal <- .cacs_palette_areawt(inter_wgs$area_wt)
  popups <- vapply(seq_len(nrow(inter_wgs)), function(i) {
    .cacs_popup_tract(list(
      GEOID            = inter_wgs$GEOID[i],
      area_wt          = inter_wgs$area_wt[i],
      intersection_km2 = inter_wgs$intersection_km2[i]
    ))
  }, character(1))

  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles(tiles) |>
    leaflet::fitBounds(lng1 = bbox$lng1, lat1 = bbox$lat1,
                       lng2 = bbox$lng2, lat2 = bbox$lat2) |>
    leaflet::addPolygons(
      data        = inter_wgs,
      fillColor   = pal(inter_wgs$area_wt),
      fillOpacity = 0.65,
      color       = "#555555", weight = 0.5, opacity = 0.6,
      popup       = popups,
      label       = sprintf("GEOID %s (area_wt = %0.3f)",
                            inter_wgs$GEOID, inter_wgs$area_wt),
      group       = "tracts"
    ) |>
    leaflet::addPolygons(
      data        = iso_outline,
      fill        = FALSE,
      color       = "#3182bd", weight = 2.5, opacity = 0.9,
      label       = if (!is.na(dt_pick)) {
        sprintf("%d-min isochrone", dt_pick)
      } else "isochrone",
      group       = "isochrone"
    ) |>
    leaflet::addLegend(
      "bottomright", pal = pal, values = c(0, 1),
      title = "area_wt", opacity = 0.85
    )

  site_popup <- .cacs_popup_site(list(
    site_id   = resolved$site_id,
    site_name = resolved$name,
    lat       = resolved$lat,
    lon       = resolved$lon
  ))
  m <- leaflet::addCircleMarkers(
    m, lng = resolved$lon, lat = resolved$lat,
    radius = 6, color = "#e34a33", fillColor = "#e34a33",
    fillOpacity = 0.9, stroke = FALSE,
    popup = site_popup,
    label = if (!is.na(resolved$name)) resolved$name else resolved$site_id
  )

  m
}


#' Map an ACS variable by tract in a drive-time area (map 3 of 4)
#'
#' @description
#' Draws an interactive `leaflet` map of one American Community Survey (ACS)
#' variable over the census tracts that overlap a drive-time area of a site.
#' The part of each tract inside the area is shaded by the tract's estimate
#' as it is in `acs_sf`, before any weighting. The outline of the area and
#' the site's point are drawn on top.
#'
#' @details
#' Clicking a tract shows the values that [cacs_plot_site_intersection()]
#' shows: `GEOID`, the coverage weight `area_wt` (the share of the tract's
#' area inside the drive-time area), and `intersection_km2` (the area of
#' that part in square kilometers), with the variable code. It also shows
#' the tract's estimate, its margin of error (`moe`, the half-width of the
#' 90 percent confidence interval published with the estimate), and the
#' coverage weight times the estimate, labeled "area_wt x estimate". For a
#' count, the products add up to the estimate that [cacs_intersect_weight()]
#' gives for the area, which is `NA` when any tract's estimate is missing.
#' For a median or a per-person value, that estimate is instead an average
#' of the tract estimates weighted by area shares (see
#' [cacs_intersect_weight()]), and the products do not add up to it.
#'
#' The estimates are grouped into color classes by natural breaks when the
#' classInt package is installed and there are at least five different
#' estimates, and by `pretty()` otherwise. A tract with a missing estimate,
#' or with no row for `variable` in `acs_sf`, is gray.
#'
#' With `lat` and `lon` instead of `site_id`, the rows of `iso_sf` are not
#' narrowed to one site. The drive time is then chosen among the drive times
#' of all the sites in `iso_sf`, and the area of every site for that drive
#' time is drawn, so a map of one area needs an `iso_sf` that holds the rows
#' of one site. [cacs_plot_site_rates()] and [cacs_plot_site_pipeline()]
#' differ: they map the site in `sites_df` nearest to the point.
#'
#' @inheritParams cacs_plot_site_isochrone
#' @inheritParams cacs_plot_site_intersection
#' @param acs_sf A data frame or `sf` object of tract estimates in the long
#'   form of the result of [cacs_acs_prefetch()], with the columns `GEOID`,
#'   `variable`, `estimate`, and `moe`. It is often the same object as
#'   `tract_sf`. The estimates are mapped as they are: unlike
#'   [cacs_intersect_weight()], the map does not set Census Bureau annotation
#'   codes, such as `-666666666`, to `NA`, and it draws a tract with two rows
#'   for the variable twice instead of giving an error.
#' @param variable A string giving the ACS variable code to map, one of the
#'   values in the `variable` column of `acs_sf`. The default is
#'   `"B17001_002"`, the number of people whose income in the past 12 months
#'   was below the poverty level.
#' @param variable_family A string giving the kind of quantity that
#'   `variable` is, as in the `estimand_family` column of [cacs_run()]
#'   results. It only chooses the ColorBrewer palette: `"Greens"` for
#'   `"spatial_total"` (a count; the default), `"PuOr"` for `"median_proxy"`
#'   (a median), `"YlOrRd"` for `"area_weighted_scalar_proxy"` (a
#'   per-person value), and `"RdYlBu"` for `"derived_rate"` (a rate). `NA`
#'   and unrecognized values give `"YlGnBu"`, and `"metadata_only"` gives an
#'   error. The value is not checked against `variable`.
#'
#' @return A `leaflet` map (an HTML widget).
#' @family maps of one site
#' @seealso `vignette("visual-walkthrough", package = "catchmentACS")`
#'   (<https://joonho112.github.io/catchmentACS/articles/visual-walkthrough.html>)
#'   builds all four maps for one site, one section each.
#' @export
#' @examples
#' if (requireNamespace("leaflet", quietly = TRUE)) {
#'   # Example data bundled with the package for one site in Birmingham,
#'   # Alabama: a 10-minute drive-time area from the OSRM routing service,
#'   # 2019-2023 ACS estimates for nearby tracts, and the cacs_run() result.
#'   fx <- readRDS(system.file("extdata", "visual_walkthrough_fixture.rds",
#'                             package = "catchmentACS"))
#'   # Median household income, a median
#'   cacs_plot_site_weighted(site_id = "AL_BHM_01", iso_sf = fx$iso_sf,
#'                           tract_sf = fx$tract_sf, acs_sf = fx$acs_sf,
#'                           variable = "B19013_001",
#'                           variable_family = "median_proxy",
#'                           sites_df = fx$sites_df)
#' }
cacs_plot_site_weighted <- function(site_id = NULL, lat = NULL, lon = NULL,
                                    site_name = NULL,
                                    iso_sf, tract_sf, acs_sf,
                                    sites_df = NULL,
                                    variable = "B17001_002",
                                    tiles = "OpenStreetMap",
                                    padding_km = 5,
                                    drive_time_min = NULL,
                                    variable_family = "spatial_total", ...) {
  .assert_leaflet_available()

  resolved <- .cacs_resolve_site_input(site_id, lat, lon, site_name, sites_df)

  if (!"variable" %in% names(acs_sf)) {
    .cli_abort_schema(c(
      "{.arg acs_sf} has no {.field variable} column.",
      "i" = "Pass a long-format ACS sf (one row per (GEOID, variable))."
    ))
  }
  if (!"GEOID" %in% names(acs_sf)) {
    .cli_abort_schema_with_template(
      "{.arg acs_sf} has no {.field GEOID} column.",
      list(.schema_issue(
        check = "plot_acs_geoid_missing",
        col = "GEOID",
        actual = "missing",
        expected = "ACS tract GEOID column",
        fix_hint = "Pass the same acs_sf object used for cacs_intersect_weight().",
        example = "acs_sf <- tract_sf"
      )),
      help = "cacs_plot_site_weighted() joins the ACS estimates to the intersection geometry by GEOID."
    )
  }
  if (!is.character(variable) || length(variable) != 1L || is.na(variable)) {
    .cli_abort_schema(c(
      "{.arg variable} must be a non-NA character scalar.",
      "x" = "Got {.cls {class(variable)}} of length {length(variable)}."
    ))
  }
  if (!variable %in% acs_sf$variable) {
    .cli_abort_schema(c(
      "{.arg variable} {.val {variable}} not found in {.arg acs_sf}.",
      "i" = "Available: {.val {sort(unique(acs_sf$variable))}}."
    ))
  }

  if (!is.na(resolved$site_id)) {
    if (!"site_id" %in% names(iso_sf)) {
      .cli_abort_schema(c(
        "{.arg iso_sf} has no {.field site_id} column.",
        "i" = "Cannot filter to {.val {resolved$site_id}}."
      ))
    }
    site_iso <- iso_sf[iso_sf$site_id == resolved$site_id, ]
    if (nrow(site_iso) == 0L) {
      .cli_abort_schema(c(
        "No isochrone found in {.arg iso_sf} for site_id = {.val {resolved$site_id}}.",
        "i" = "Check {.code unique(iso_sf$site_id)} for available sites."
      ))
    }
  } else {
    site_iso <- iso_sf
  }
  if ("drive_time_min" %in% names(site_iso)) {
    dt_levels <- sort(unique(site_iso$drive_time_min))
    dt_pick <- if (is.null(drive_time_min)) {
      max(dt_levels)
    } else {
      if (!drive_time_min %in% dt_levels) {
        .cli_abort_schema(c(
          "{.arg drive_time_min} = {.val {drive_time_min}} not found in {.arg iso_sf}.",
          "i" = "Available: {.val {dt_levels}}."
        ))
      }
      drive_time_min
    }
    site_iso <- site_iso[site_iso$drive_time_min == dt_pick, ]
  } else {
    dt_pick <- NA_integer_
  }

  # One row for each tract, as in the map of stage 2.
  if (!"GEOID" %in% names(tract_sf)) {
    .cli_abort_schema_with_template(
      "{.arg tract_sf} has no {.field GEOID} column.",
      list(.schema_issue(
        check = "plot_tract_geoid_missing",
        col = "GEOID",
        actual = "missing",
        expected = "tract GEOID column",
        fix_hint = "Pass the same acs_sf object used for cacs_intersect_weight().",
        example = "tract_sf <- acs_sf"
      )),
      help = "cacs_plot_site_weighted() joins the intersection geometry to the tracts by GEOID."
    )
  }
  tract_geom <- tract_sf[!duplicated(tract_sf$GEOID), c("GEOID")]

  # The shares are computed as in the map of stage 2.
  iso_5070   <- sf::st_transform(site_iso,   5070)
  tract_5070 <- sf::st_transform(tract_geom, 5070)
  tract_5070$tract_area_m2 <- as.numeric(sf::st_area(tract_5070))
  sf::st_agr(tract_5070) <- "constant"
  sf::st_agr(iso_5070)   <- "constant"
  inter <- suppressWarnings(sf::st_intersection(tract_5070, iso_5070))
  if (nrow(inter) == 0L) {
    .cli_abort_schema(c(
      "Isochrone does not intersect any tract in {.arg tract_sf}.",
      "i" = "Check coverage of {.code sf::st_bbox(tract_sf)} vs {.code sf::st_bbox(iso_sf)}."
    ))
  }
  inter$intersection_km2 <- as.numeric(sf::st_area(inter)) / 1e6
  inter$area_wt <- (inter$intersection_km2 * 1e6) / inter$tract_area_m2
  inter <- inter[is.finite(inter$area_wt) & inter$area_wt > 0, ]
  inter$area_wt <- pmin(pmax(inter$area_wt, 0), 1)

  # The estimate of the chosen variable is joined to the tract pieces by
  # GEOID; a tract without a row for it keeps NA and is drawn in the color
  # the palette gives missing values.
  acs_drop <- if (inherits(acs_sf, "sf")) {
    sf::st_drop_geometry(acs_sf)
  } else acs_sf
  panel <- acs_drop[acs_drop$variable == variable,
                    c("GEOID", "variable", "estimate", "moe"),
                    drop = FALSE]
  inter <- merge(inter, panel, by = "GEOID", all.x = TRUE,
                 suffixes = c("", ".y"))

  inter_wgs   <- sf::st_transform(inter,    4326)
  iso_outline <- sf::st_transform(site_iso, 4326)

  bbox <- .cacs_default_view(resolved$lat, resolved$lon,
                             padding_km = padding_km)

  pal <- .cacs_palette_chloropleth(inter_wgs$estimate, variable_family)
  fnum <- function(x, digits = 2L) {
    if (!is.numeric(x) || !is.finite(x)) return("n/a")
    formatC(x, digits = digits, format = "f")
  }
  popups <- vapply(seq_len(nrow(inter_wgs)), function(i) {
    est <- inter_wgs$estimate[i]
    aw  <- inter_wgs$area_wt[i]
    contrib <- if (is.finite(est) && is.finite(aw)) aw * est else NA_real_
    extra_rows <- sprintf(
      paste0(
        "<table class=\"cacs-popup-vars\">",
        "<tr><td>variable</td><td>%s</td></tr>",
        "<tr><td>estimate</td><td>%s</td></tr>",
        "<tr><td>moe</td><td>%s</td></tr>",
        "<tr><td>area_wt x estimate</td><td>%s</td></tr>",
        "</table>"
      ),
      variable, fnum(est), fnum(inter_wgs$moe[i]), fnum(contrib)
    )
    base <- .cacs_popup_tract(list(
      GEOID            = inter_wgs$GEOID[i],
      area_wt          = aw,
      intersection_km2 = inter_wgs$intersection_km2[i]
    ))
    paste0(base, extra_rows)
  }, character(1))

  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles(tiles) |>
    leaflet::fitBounds(lng1 = bbox$lng1, lat1 = bbox$lat1,
                       lng2 = bbox$lng2, lat2 = bbox$lat2) |>
    leaflet::addPolygons(
      data        = inter_wgs,
      fillColor   = pal(inter_wgs$estimate),
      fillOpacity = 0.7,
      color       = "#555555", weight = 0.5, opacity = 0.6,
      popup       = popups,
      label       = sprintf("GEOID %s (%s = %s)",
                            inter_wgs$GEOID, variable,
                            vapply(inter_wgs$estimate, fnum, character(1))),
      group       = "tracts"
    ) |>
    leaflet::addPolygons(
      data        = iso_outline,
      fill        = FALSE,
      color       = "#3182bd", weight = 2.5, opacity = 0.9,
      label       = if (!is.na(dt_pick)) {
        sprintf("%d-min isochrone", dt_pick)
      } else "isochrone",
      group       = "isochrone"
    ) |>
    leaflet::addLegend(
      "bottomright", pal = pal, values = inter_wgs$estimate,
      title = variable, opacity = 0.85
    )

  site_popup <- .cacs_popup_site(list(
    site_id   = resolved$site_id,
    site_name = resolved$name,
    lat       = resolved$lat,
    lon       = resolved$lon
  ))
  m <- leaflet::addCircleMarkers(
    m, lng = resolved$lon, lat = resolved$lat,
    radius = 6, color = "#e34a33", fillColor = "#e34a33",
    fillOpacity = 0.9, stroke = FALSE,
    popup = site_popup,
    label = if (!is.na(resolved$name)) resolved$name else resolved$site_id
  )

  m
}


#' Map a site with its rates and margins of error (map 4 of 4)
#'
#' @description
#' Draws an interactive `leaflet` map of a site and the outline of its
#' drive-time areas in `iso_sf`, merged into one. Clicking the site's point
#' opens a pop-up that lists the site's values from `run_result` for the
#' five rates in [`cacs_acs_default_rates`].
#'
#' @details
#' For each rate, the pop-up shows the estimate and its margin of error
#' (MOE, the half-width of a 90 percent confidence interval unless another
#' level was used for `run_result`), both as proportions to four decimal
#' places. It also shows the numbers of tracts combined for the numerator
#' and for the denominator (`n_tracts_num` and `n_tracts_den`, filled in by
#' [cacs_derive_rates()]), the MOE formula used (`moe_formula_effective`),
#' and whether the chosen formula could not be used (`moe_fallback`). A
#' rate with `moe_fallback = TRUE` also has an asterisk before its name:
#' either the ratio formula was used in place of the proportion formula, or
#' the denominator is zero and the rate is `NA` (see [cacs_derive_rates()]).
#'
#' When `run_result` has rates for more than one drive time of the site,
#' each rate is listed once for each drive time, and the pop-up does not
#' show the drive time.
#'
#' @inheritParams cacs_plot_site_isochrone
#' @param lat,lon Single numbers giving the latitude and longitude of a
#'   point in degrees (WGS 84), used when `site_id` is `NULL`. Both must be
#'   given. The site in `sites_df` nearest to the point is mapped, and a
#'   message of class `catchmentACS_message_resolve_site` gives its
#'   `site_id` and its distance from the point. If that site has no rows in
#'   `run_result`, the function stops with an error. If it is more than 5 km
#'   from the point, a warning of class
#'   `catchmentACS_warning_resolve_site_distant` is also given.
#' @param run_result A data frame of rates with the columns `site_id`,
#'   `variable`, `estimate`, `moe`, `n_tracts_num`, `n_tracts_den`, and
#'   `moe_fallback`, such as the long form of a [cacs_run()] result or the
#'   result of [cacs_derive_rates()]. The rows of the chosen site for the
#'   five rates are used, and there must be at least one.
#'
#' @return A `leaflet` map (an HTML widget).
#' @family maps of one site
#' @seealso `vignette("visual-walkthrough", package = "catchmentACS")`
#'   (<https://joonho112.github.io/catchmentACS/articles/visual-walkthrough.html>)
#'   builds all four maps for one site, one section each.
#' @export
#' @examples
#' if (requireNamespace("leaflet", quietly = TRUE)) {
#'   # Example data bundled with the package for one site in Birmingham,
#'   # Alabama: a 10-minute drive-time area from the OSRM routing service,
#'   # 2019-2023 ACS estimates for nearby tracts, and the cacs_run() result.
#'   fx <- readRDS(system.file("extdata", "visual_walkthrough_fixture.rds",
#'                             package = "catchmentACS"))
#'   # Clicking the site's point opens the pop-up with the five rates
#'   cacs_plot_site_rates(site_id = "AL_BHM_01", iso_sf = fx$iso_sf,
#'                        run_result = fx$run_result, sites_df = fx$sites_df)
#' }
cacs_plot_site_rates <- function(site_id = NULL, lat = NULL, lon = NULL,
                                 site_name = NULL,
                                 iso_sf, run_result, sites_df = NULL,
                                 tiles = "OpenStreetMap",
                                 padding_km = 5, ...) {
  .assert_leaflet_available()

  resolved <- .cacs_resolve_site_input(site_id, lat, lon, site_name, sites_df)

  if (!inherits(run_result, c("tbl_df", "data.frame"))) {
    .cli_abort_schema(c(
      "{.arg run_result} must be a {.cls tbl_df} or {.cls data.frame}.",
      "x" = "Got {.cls {class(run_result)[[1L]]}}.",
      "i" = "Pass the result of {.fn cacs_run}."
    ))
  }
  required_cols <- c("site_id", "variable", "estimate", "moe",
                     "n_tracts_num", "n_tracts_den", "moe_fallback")
  missing_cols <- setdiff(required_cols, names(run_result))
  if (length(missing_cols) > 0L) {
    .cli_abort_schema(c(
      "{.arg run_result} missing required column{?s}: {.field {missing_cols}}.",
      "i" = "Pass the result of {.fn cacs_run}."
    ))
  }
  resolved <- .cacs_promote_latlon_to_site(
    resolved = resolved,
    sites_df = sites_df,
    run_result = run_result,
    context = "cacs_plot_site_rates"
  )

  # The five names of cacs_acs_default_rates; the other rows of run_result
  # hold ACS variables and are not shown on this map.
  RATE_NAMES <- c("poverty_rate", "snap_rate", "ssi_rate", "unemp_rate",
                  "labor_force_participation")
  if (is.na(resolved$site_id)) {
    .cli_abort_schema(c(
      "{.fn cacs_plot_site_rates} requires a resolvable {.arg site_id}.",
      "i" = "Direct (lat, lon) input cannot filter {.arg run_result} by site."
    ))
  }
  site_rates <- run_result[run_result$site_id == resolved$site_id &
                             run_result$variable %in% RATE_NAMES, , drop = FALSE]
  if (nrow(site_rates) == 0L) {
    .cli_abort_schema(c(
      "No rate rows found in {.arg run_result} for site_id = {.val {resolved$site_id}}.",
      "i" = "Check {.code unique(run_result$site_id)} for available sites."
    ))
  }

  if (!"site_id" %in% names(iso_sf)) {
    .cli_abort_schema(c(
      "{.arg iso_sf} has no {.field site_id} column.",
      "i" = "Cannot filter to {.val {resolved$site_id}}."
    ))
  }
  site_iso <- iso_sf[iso_sf$site_id == resolved$site_id, ]
  if (nrow(site_iso) == 0L) {
    .cli_abort_schema(c(
      "No isochrone found in {.arg iso_sf} for site_id = {.val {resolved$site_id}}.",
      "i" = "Check {.code unique(iso_sf$site_id)} for available sites."
    ))
  }

  # The rings of the site are joined into one outline, so a site with several
  # drive times is drawn once instead of as overlapping shapes.
  iso_5070 <- sf::st_transform(site_iso, 5070)
  sf::st_agr(iso_5070) <- "constant"
  iso_union_5070 <- suppressWarnings(sf::st_union(iso_5070))
  iso_union <- sf::st_transform(sf::st_sfc(iso_union_5070, crs = 5070), 4326)

  bbox <- .cacs_default_view(resolved$lat, resolved$lon,
                             padding_km = padding_km)

  # One block for each rate, put inside a single element so that the marker
  # has one pop-up. A row whose margin of error came from the other formula
  # (moe_fallback) gets an asterisk before its name.
  site_rates <- site_rates[order(match(site_rates$variable, RATE_NAMES)), ,
                            drop = FALSE]
  popup_blocks <- vapply(seq_len(nrow(site_rates)), function(i) {
    rr <- as.list(site_rates[i, , drop = FALSE])
    if (isTRUE(rr$moe_fallback)) {
      rr$variable <- paste0("* ", rr$variable)
    }
    .cacs_popup_rate(rr)
  }, character(1))
  header <- sprintf(
    "<div class=\"cacs-popup-rates-header\"><strong>Site:</strong> %s</div>",
    if (!is.na(resolved$name)) resolved$name else resolved$site_id
  )
  rates_popup <- paste0(
    "<div class=\"cacs-popup-rates\">",
    header,
    paste0(popup_blocks, collapse = "\n"),
    "</div>"
  )

  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles(tiles) |>
    leaflet::fitBounds(lng1 = bbox$lng1, lat1 = bbox$lat1,
                       lng2 = bbox$lng2, lat2 = bbox$lat2) |>
    leaflet::addPolygons(
      data        = iso_union,
      color       = "#3182bd", weight = 2.5, opacity = 0.85,
      fillColor   = "#3182bd", fillOpacity = 0.15,
      label       = "isochrone (union)",
      group       = "isochrone"
    ) |>
    leaflet::addCircleMarkers(
      lng = resolved$lon, lat = resolved$lat,
      radius = 7, color = "#e34a33", fillColor = "#e34a33",
      fillOpacity = 0.9, stroke = FALSE,
      popup = rates_popup,
      popupOptions = leaflet::popupOptions(autoClose = FALSE,
                                           closeOnClick = FALSE,
                                           keepInView = TRUE,
                                           maxWidth = 400),
      label = if (!is.na(resolved$name)) resolved$name else resolved$site_id
    )

  m
}


#' Build all four maps for one site
#'
#' @description
#' Builds, for one site, the four maps that show step by step how the
#' package computes its estimates from American Community Survey (ACS) data,
#' and returns them in one list. The maps come from
#' [cacs_plot_site_isochrone()] (the site and its drive-time areas),
#' [cacs_plot_site_intersection()] (the census tracts that overlap one area,
#' shaded by coverage weight), [cacs_plot_site_weighted()] (one ACS variable
#' over those tracts), and [cacs_plot_site_rates()] (the five rates). A
#' tract's coverage weight is the share of its area inside the drive-time
#' area.
#'
#' @details
#' The site is looked up once, before any map is built, and used for all
#' four. `iso_sf`, `sites_df`, `tiles`, `padding_km`, and `...` are passed
#' to all four functions, `tract_sf` and `drive_time_min` to the second and
#' third, `acs_sf`, `variable`, and `variable_family` to the third, and
#' `run_result` to the fourth.
#'
#' The function does not contact a routing service or the Census Bureau.
#' Its inputs are results computed beforehand: the drive-time areas by
#' [cacs_isochrone()], the tract estimates by [cacs_acs_prefetch()], and the
#' rates by [cacs_run()].
#'
#' @inheritParams cacs_plot_site_isochrone
#' @inheritParams cacs_plot_site_weighted
#' @inheritParams cacs_plot_site_rates
#' @param lat,lon Single numbers giving the latitude and longitude of a
#'   point in degrees (WGS 84), used when `site_id` is `NULL`. The nearest
#'   site in `sites_df` is found as in [cacs_plot_site_rates()], with one
#'   message, and used for all four maps.
#' @param drive_time_min A single number giving the drive time, in minutes,
#'   of the area in the second and third maps, or `NULL` (the default) for
#'   the longest drive time of the site in `iso_sf`. The first map draws all
#'   the areas of the site, and the fourth their outline.
#' @param ... Passed to the four map functions, which do not use them.
#'
#' @return A list of class `"cacs_site_plot_pipeline"` with four `leaflet`
#'   maps, `isochrone`, `intersection`, `weighted`, and `rates`, from the
#'   four functions in that order. See [print.cacs_site_plot_pipeline()] for
#'   what printing the list shows. The attributes `site_id`, `lat`, `lon`,
#'   and `name` record the site used, and `variable` the variable of the
#'   third map.
#' @family maps of one site
#' @seealso The four maps are shown for one site in
#'   `vignette("visual-walkthrough", package = "catchmentACS")`
#'   (<https://joonho112.github.io/catchmentACS/articles/visual-walkthrough.html>).
#' @export
#' @examples
#' if (requireNamespace("leaflet", quietly = TRUE)) {
#'   # Example data bundled with the package for one site in Birmingham,
#'   # Alabama: a 10-minute drive-time area from the OSRM routing service,
#'   # 2019-2023 ACS estimates for nearby tracts, and the cacs_run() result.
#'   fx <- readRDS(system.file("extdata", "visual_walkthrough_fixture.rds",
#'                             package = "catchmentACS"))
#'   maps <- cacs_plot_site_pipeline(
#'     site_id    = "AL_BHM_01",
#'     iso_sf     = fx$iso_sf,
#'     tract_sf   = fx$tract_sf,
#'     acs_sf     = fx$acs_sf,
#'     run_result = fx$run_result,
#'     sites_df   = fx$sites_df
#'   )
#'   # A summary of the four maps; no map is drawn
#'   print(maps)
#'   # The second map
#'   maps$intersection
#' }
#'
#' # Downloads ACS data with a Census API key and builds three areas on the
#' # public OSRM demo server with the osrm package; the server limits requests.
#' \dontrun{
#' library(sf)
#' sites <- cacs_alabama_sites[1:3, ]
#' iso <- cacs_isochrone(sites, drive_times = 10)
#' acs <- cacs_acs_prefetch("AL")
#' out <- cacs_run(sites, state = "AL", drive_times = 10,
#'                 precomputed_isochrones = iso, acs = acs)
#' maps_live <- cacs_plot_site_pipeline(site_id = "AL_SITE_01", iso_sf = iso,
#'                                      tract_sf = acs, acs_sf = acs,
#'                                      run_result = out)
#' }
cacs_plot_site_pipeline <- function(site_id = NULL, lat = NULL, lon = NULL,
                                    site_name = NULL,
                                    iso_sf, tract_sf, acs_sf, run_result,
                                    sites_df = NULL,
                                    variable = "B17001_002",
                                    variable_family = "spatial_total",
                                    tiles = "OpenStreetMap",
                                    padding_km = 5,
                                    drive_time_min = NULL, ...) {
  .assert_leaflet_available()

  # The site is resolved once here, so an argument that names no site gives an
  # error before any of the four maps is built.
  resolved <- .cacs_resolve_site_input(site_id, lat, lon, site_name, sites_df)
  resolved <- .cacs_promote_latlon_to_site(
    resolved = resolved,
    sites_df = sites_df,
    run_result = run_result,
    context = "cacs_plot_site_pipeline"
  )

  # `tiles`, `padding_km`, and `...` go to every map, so the four look alike.
  isochrone <- cacs_plot_site_isochrone(
    site_id    = resolved$site_id,
    iso_sf     = iso_sf,
    sites_df   = sites_df,
    tiles      = tiles,
    padding_km = padding_km, ...
  )
  intersection <- cacs_plot_site_intersection(
    site_id        = resolved$site_id,
    iso_sf         = iso_sf,
    tract_sf       = tract_sf,
    sites_df       = sites_df,
    tiles          = tiles,
    padding_km     = padding_km,
    drive_time_min = drive_time_min, ...
  )
  weighted <- cacs_plot_site_weighted(
    site_id         = resolved$site_id,
    iso_sf          = iso_sf,
    tract_sf        = tract_sf,
    acs_sf          = acs_sf,
    sites_df        = sites_df,
    variable        = variable,
    variable_family = variable_family,
    tiles           = tiles,
    padding_km      = padding_km,
    drive_time_min  = drive_time_min, ...
  )
  rates <- cacs_plot_site_rates(
    site_id    = resolved$site_id,
    iso_sf     = iso_sf,
    run_result = run_result,
    sites_df   = sites_df,
    tiles      = tiles,
    padding_km = padding_km, ...
  )

  # The site and the variable are kept as attributes, so print() and format()
  # can name them without looking the site up again.
  out <- list(
    isochrone    = isochrone,
    intersection = intersection,
    weighted     = weighted,
    rates        = rates
  )
  class(out) <- c("cacs_site_plot_pipeline", "list")
  attr(out, "site_id")  <- resolved$site_id
  attr(out, "lat")      <- resolved$lat
  attr(out, "lon")      <- resolved$lon
  attr(out, "name")     <- resolved$name
  attr(out, "variable") <- variable
  out
}


#' Print a summary of the four maps
#'
#' @description
#' Prints the site (its `site_id` and name), the variable of the third map,
#' and the four elements of the list, `x$isochrone`, `x$intersection`,
#' `x$weighted`, and `x$rates`, with a short note on each. No map is drawn;
#' printing one of the elements draws that map.
#'
#' @details
#' The summary is written with the cli package as messages, so
#' `capture.output(print(x), type = "message")` captures it and
#' `capture.output(print(x))` does not.
#'
#' @param x A list of class `"cacs_site_plot_pipeline"`, as returned by
#'   [cacs_plot_site_pipeline()].
#' @param ... Not used.
#' @return `x`, invisibly.
#' @family maps of one site
#' @exportS3Method print cacs_site_plot_pipeline
#' @examples
#' if (requireNamespace("leaflet", quietly = TRUE)) {
#'   fx <- readRDS(system.file("extdata", "visual_walkthrough_fixture.rds",
#'                             package = "catchmentACS"))
#'   maps <- cacs_plot_site_pipeline(
#'     site_id = "AL_BHM_01", iso_sf = fx$iso_sf, tract_sf = fx$tract_sf,
#'     acs_sf = fx$acs_sf, run_result = fx$run_result, sites_df = fx$sites_df
#'   )
#'   # Lists the four maps and the variable of the third, without drawing a map
#'   print(maps)
#' }
print.cacs_site_plot_pipeline <- function(x, ...) {
  meta_site <- attr(x, "site_id") %||% NA_character_
  meta_name <- attr(x, "name")    %||% NA_character_
  meta_var  <- attr(x, "variable") %||% NA_character_

  site_label <- if (!is.na(meta_name)) {
    sprintf("%s (%s)", meta_site, meta_name)
  } else {
    meta_site
  }

  cli::cli_h1("catchmentACS site plot pipeline")
  cli::cli_text("Site: {site_label}")
  cli::cli_text("Variable (Stage 3): {meta_var}")
  cli::cli_h2("4 leaflet widgets returned")
  cli::cli_ul(c(
    "{.code x$isochrone}     - Stage 1: site point + isochrone polygon",
    "{.code x$intersection}  - Stage 2: tracts colored by area_wt",
    "{.code x$weighted}      - Stage 3: tract choropleth on {.code {meta_var}}",
    "{.code x$rates}         - Stage 4: 5-rate popup summary"
  ))
  cli::cli_alert_info("Show a map by typing its name, such as {.code x$isochrone}, in the console.")
  invisible(x)
}


#' Describe the four maps in one line
#'
#' @description
#' Returns a one-line description of the list that names its class and the
#' `site_id` of its site, such as
#' `"<cacs_site_plot_pipeline: 4 widgets for site AL_BHM_01>"`, for use in
#' text (for example, `sprintf("Maps: %s", format(x))`).
#'
#' @inheritParams print.cacs_site_plot_pipeline
#' @return A string.
#' @family maps of one site
#' @exportS3Method format cacs_site_plot_pipeline
#' @examples
#' if (requireNamespace("leaflet", quietly = TRUE)) {
#'   fx <- readRDS(system.file("extdata", "visual_walkthrough_fixture.rds",
#'                             package = "catchmentACS"))
#'   maps <- cacs_plot_site_pipeline(
#'     site_id = "AL_BHM_01", iso_sf = fx$iso_sf, tract_sf = fx$tract_sf,
#'     acs_sf = fx$acs_sf, run_result = fx$run_result, sites_df = fx$sites_df
#'   )
#'   sprintf("Maps: %s", format(maps))
#' }
format.cacs_site_plot_pipeline <- function(x, ...) {
  sprintf("<cacs_site_plot_pipeline: 4 widgets for site %s>",
          attr(x, "site_id") %||% "(unresolved)")
}


# %||%, with the same body in five files; the reason is in R/intersect-weight.R.
`%||%` <- function(x, y) if (is.null(x)) y else x
