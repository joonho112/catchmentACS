# R/plot-site.R - F5 leaflet visualization suite (5 fns).
# Step 6.1 [CORE] lays the skeleton; Steps 6.3-6.6 fill in bodies.
# Step 6.7 ships the visual-walkthrough.qmd vignette.
#
# Each per-stage helper returns one leaflet widget. Each per-stage
# fn is standalone and resolves its input via .cacs_resolve_site_input()
# (defined in R/plot-helpers.R). The pipeline wrapper returns a classed
# list with a print.cacs_site_plot_pipeline() S3 method.
#
# All 5 leaflet helpers guard via .assert_leaflet_available() first so that
# missing-leaflet users get the standard helpful install hint before any
# plotting work begins.


#' Map a site and its isochrone (walkthrough stage 1)
#'
#' @description
#' Draws an interactive `leaflet` map showing one site as a point marker
#' together with its *isochrone* (the area reachable from the site within
#' a given drive time). This is the first of four stage maps that together
#' explain how the package turns a site location into neighbourhood
#' statistics.
#'
#' You select the site in one of three ways, tried in this order: a
#' `site_id`, a direct `lat`/`lon` coordinate pair, or a `site_name`. When
#' the isochrone data records a `drive_time_min` column, the reachable
#' rings are shaded by drive time and a legend is added; otherwise a single
#' uniform polygon is drawn. The chosen site is always marked on top.
#'
#' @param site_id Site identifier to look up in `sites_df`. When omitted,
#'   `sites_df` defaults to the bundled `cacs_alabama_sites` table.
#' @param lat,lon Alternative to `site_id`: a direct latitude/longitude
#'   coordinate pair in WGS84 (`lat` in `[-90, 90]`, `lon` in `[-180, 180]`).
#' @param site_name Alternative to `site_id`: an exact match against the
#'   `site_name` column of `sites_df`.
#' @param iso_sf Isochrone polygons (an `sf` object) such as those returned
#'   by [cacs_isochrone()].
#' @param sites_df An `sf` table of sites. When `NULL`, the bundled
#'   `cacs_alabama_sites` table is used.
#' @param tiles Name of the `leaflet` basemap provider tiles
#'   (default `"CartoDB.Positron"`).
#' @param padding_km Positive number giving the map padding, in kilometres,
#'   around the site point when setting the initial view. Default `5`.
#' @param ... Reserved for future keyword arguments; currently unused.
#'
#' @return A `leaflet` map (an HTML widget).
#' @family visual walkthrough helpers
#' @seealso [cacs_plot_site_pipeline()] to build all four stage maps at
#'   once, and [cacs_run()] for the end-to-end aggregation these maps
#'   illustrate.
#' @export
#' @examples
#' \dontrun{
#' iso <- readRDS(system.file("extdata", "legacy_2025_isochrones.rds",
#'                            package = "catchmentACS"))
#' cacs_plot_site_isochrone(
#'   site_id = unique(iso$site_id)[1],
#'   iso_sf  = iso
#' )
#' }
cacs_plot_site_isochrone <- function(site_id = NULL, lat = NULL, lon = NULL,
                                     site_name = NULL, iso_sf,
                                     sites_df = NULL,
                                     tiles = "CartoDB.Positron",
                                     padding_km = 5, ...) {
  .assert_leaflet_available()

  # 1. Resolve site input via shared helper (site_id > (lat, lon) > site_name).
  resolved <- .cacs_resolve_site_input(site_id, lat, lon, site_name, sites_df)
  # resolved = list(site_id, lat, lon, name)

  # 2. Filter iso_sf to this site when site_id resolved. Direct (lat, lon)
  #    input has no site_id, so we trust the caller to have already
  #    subsetted iso_sf to the polygons they want shown.
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

  # 3. Compute auto-bounds around the site point.
  bbox <- .cacs_default_view(resolved$lat, resolved$lon,
                             padding_km = padding_km)

  # 4. Build leaflet widget with provider tiles + initial bbox.
  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles(tiles) |>
    leaflet::fitBounds(lng1 = bbox$lng1, lat1 = bbox$lat1,
                       lng2 = bbox$lng2, lat2 = bbox$lat2)

  # 5. Add isochrone polygons. If drive_time_min present, color-grade the
  #    rings via a viridis discrete palette + add a legend; otherwise
  #    render as a single uniform polygon layer.
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

  # 6. Add site marker (popup via shared .cacs_popup_site() helper).
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


#' Map isochrone overlap with census tracts (walkthrough stage 2)
#'
#' @description
#' Draws the isochrone outline on top of every census *tract* (a small
#' Census Bureau geographic area) it touches, shading each tract by its
#' *area weight*: the share of that tract's area that falls inside the
#' isochrone, from `0` (no overlap) to `1` (fully contained). This is the
#' second of four stage maps and shows how each tract contributes to a
#' site's catchment.
#'
#' The overlap is recomputed on the fly by intersecting `iso_sf` with
#' `tract_sf` in an equal-area projection, so this map stays self-contained
#' and does not require pre-aggregated results. You select the site with a
#' `site_id`, a `lat`/`lon` pair, or a `site_name`, tried in that order.
#'
#' @param site_id Site identifier to look up in `sites_df`. When omitted,
#'   `sites_df` defaults to the bundled `cacs_alabama_sites` table.
#' @param lat,lon Alternative to `site_id`: a direct latitude/longitude
#'   coordinate pair in WGS84 (`lat` in `[-90, 90]`, `lon` in `[-180, 180]`).
#' @param site_name Alternative to `site_id`: an exact match against the
#'   `site_name` column of `sites_df`.
#' @param iso_sf Isochrone polygons (an `sf` object) such as those returned
#'   by [cacs_isochrone()].
#' @param tract_sf Tract polygons (an `sf` object) with at least a `GEOID`
#'   column and polygon geometry, typically the same ACS data passed to
#'   [cacs_intersect_weight()]. If the table is in long format (one row per
#'   `GEOID` and variable) it is reduced to one row per `GEOID` for the
#'   geometric work.
#' @param sites_df An `sf` table of sites. When `NULL`, the bundled
#'   `cacs_alabama_sites` table is used.
#' @param tiles Name of the `leaflet` basemap provider tiles
#'   (default `"CartoDB.Positron"`).
#' @param padding_km Positive number giving the map padding, in kilometres,
#'   around the site point when setting the initial view. Default `5`.
#' @param drive_time_min Single integer, or `NULL`, naming which drive-time
#'   ring to intersect. When `NULL`, the largest available ring for the
#'   site is used.
#' @param ... Reserved for future keyword arguments; currently unused.
#'
#' @return A `leaflet` map (an HTML widget).
#' @family visual walkthrough helpers
#' @seealso [cacs_plot_site_pipeline()] to build all four stage maps at
#'   once, and [cacs_run()] for the end-to-end aggregation these maps
#'   illustrate.
#' @export
#' @examples
#' \dontrun{
#' iso <- readRDS(system.file("extdata", "legacy_2025_isochrones.rds",
#'                            package = "catchmentACS"))
#' acs <- readRDS(system.file("extdata", "sample_alabama_subset.rds",
#'                            package = "catchmentACS"))
#' cacs_plot_site_intersection(
#'   site_id  = "AL_SITE_01",
#'   iso_sf   = iso,
#'   tract_sf = acs
#' )
#' }
cacs_plot_site_intersection <- function(site_id = NULL, lat = NULL, lon = NULL,
                                        site_name = NULL,
                                        iso_sf, tract_sf,
                                        sites_df = NULL,
                                        tiles = "CartoDB.Positron",
                                        padding_km = 5,
                                        drive_time_min = NULL, ...) {
  .assert_leaflet_available()

  # 1. Resolve site input via shared helper (site_id > (lat, lon) > site_name).
  resolved <- .cacs_resolve_site_input(site_id, lat, lon, site_name, sites_df)

  # 2. Pick the iso polygon for this site (+ drive_time ring).
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

  # 3. Dedupe long-format tract_sf to one geometry per GEOID for sf ops.
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
      help = "Stage 2 joins intersection geometry by tract GEOID."
    )
  }
  tract_geom <- tract_sf[!duplicated(tract_sf$GEOID), c("GEOID")]

  # 4. Reproject to EPSG:5070 for the area calc (metric / equal-area).
  iso_5070   <- sf::st_transform(site_iso,   5070)
  tract_5070 <- sf::st_transform(tract_geom, 5070)
  tract_5070$tract_area_m2 <- as.numeric(sf::st_area(tract_5070))

  # 5. Per-tract intersection -> area_wt. Suppress the spatially-constant
  #    warning that st_intersection emits with attribute-bearing LHS.
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
  # Defensive clamp + sanity drop (drop micro slivers + NA rows).
  inter <- inter[is.finite(inter$area_wt) & inter$area_wt > 0, ]
  inter$area_wt <- pmin(pmax(inter$area_wt, 0), 1)

  # 6. Reproject back to WGS84 for leaflet rendering.
  inter_wgs  <- sf::st_transform(inter,    4326)
  iso_outline <- sf::st_transform(site_iso, 4326)

  # 7. Default view bounds.
  bbox <- .cacs_default_view(resolved$lat, resolved$lon,
                             padding_km = padding_km)

  # 8. Build leaflet widget.
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

  # 9. Add site marker.
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


#' Map a census variable across overlapping tracts (walkthrough stage 3)
#'
#' @description
#' Draws a *choropleth* (a map that shades areas by a data value) of one
#' American Community Survey (ACS) variable across the census tracts that
#' overlap the site's isochrone. Tract colour is chosen automatically from
#' a ColorBrewer palette suited to the variable type. Each tract's pop-up
#' reports its `GEOID` (Census tract identifier), the variable code, the
#' `estimate`, its margin of error (MOE, the 90 percent confidence band
#' published with each ACS estimate), the area weight, and the area weight
#' times the estimate (that tract's contribution to the site total).
#'
#' As in [cacs_plot_site_intersection()], the tract overlaps are recomputed
#' on the fly from `iso_sf` and `tract_sf`, so this map needs no
#' pre-aggregated results.
#'
#' @param site_id Site identifier to look up in `sites_df`. When omitted,
#'   `sites_df` defaults to the bundled `cacs_alabama_sites` table.
#' @param lat,lon Alternative to `site_id`: a direct latitude/longitude
#'   coordinate pair in WGS84 (`lat` in `[-90, 90]`, `lon` in `[-180, 180]`).
#' @param site_name Alternative to `site_id`: an exact match against the
#'   `site_name` column of `sites_df`.
#' @param iso_sf Isochrone polygons (an `sf` object) such as those returned
#'   by [cacs_isochrone()].
#' @param tract_sf Tract polygons (an `sf` object) with a `GEOID` column and
#'   polygon geometry, typically the same ACS data passed to
#'   [cacs_intersect_weight()].
#' @param acs_sf Long-format ACS data (an `sf` object) with columns `GEOID`,
#'   `variable`, `estimate`, and `moe`, typically the same object as
#'   `tract_sf`.
#' @param variable Single ACS variable code to map (default `"B17001_002"`,
#'   the count of people with income below the poverty level). It must be
#'   present in `acs_sf$variable`, otherwise the function stops with an
#'   error.
#' @param sites_df An `sf` table of sites. When `NULL`, the bundled
#'   `cacs_alabama_sites` table is used.
#' @param tiles Name of the `leaflet` basemap provider tiles
#'   (default `"CartoDB.Positron"`).
#' @param padding_km Positive number giving the map padding, in kilometres,
#'   around the site point when setting the initial view. Default `5`.
#' @param drive_time_min Single integer, or `NULL`, naming which drive-time
#'   ring to intersect. When `NULL`, the largest available ring for the
#'   site is used.
#' @param variable_family Single string naming the variable's measurement
#'   family, used to pick the ColorBrewer palette (or `NA` for a neutral
#'   fallback). Default `"spatial_total"`.
#' @param ... Reserved for future keyword arguments; currently unused.
#'
#' @return A `leaflet` map (an HTML widget).
#' @family visual walkthrough helpers
#' @seealso [cacs_plot_site_pipeline()] to build all four stage maps at
#'   once, and [cacs_run()] for the end-to-end aggregation these maps
#'   illustrate.
#' @export
#' @examples
#' \dontrun{
#' iso <- readRDS(system.file("extdata", "legacy_2025_isochrones.rds",
#'                            package = "catchmentACS"))
#' acs <- readRDS(system.file("extdata", "sample_alabama_subset.rds",
#'                            package = "catchmentACS"))
#' cacs_plot_site_weighted(
#'   site_id  = "AL_SITE_01",
#'   iso_sf   = iso,
#'   tract_sf = acs,
#'   acs_sf   = acs,
#'   variable = "B17001_002"
#' )
#' }
cacs_plot_site_weighted <- function(site_id = NULL, lat = NULL, lon = NULL,
                                    site_name = NULL,
                                    iso_sf, tract_sf, acs_sf,
                                    sites_df = NULL,
                                    variable = "B17001_002",
                                    tiles = "CartoDB.Positron",
                                    padding_km = 5,
                                    drive_time_min = NULL,
                                    variable_family = "spatial_total", ...) {
  .assert_leaflet_available()

  # 1. Resolve site input.
  resolved <- .cacs_resolve_site_input(site_id, lat, lon, site_name, sites_df)

  # 2. Validate `variable` against acs_sf$variable.
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
      help = "Stage 3 merges ACS estimates back to intersection geometry by GEOID."
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

  # 3. Filter iso to (site, drive_time_min).
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

  # 4. Dedupe long-format tract_sf to one geometry per GEOID.
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
      help = "Stage 3 joins intersection geometry by tract GEOID."
    )
  }
  tract_geom <- tract_sf[!duplicated(tract_sf$GEOID), c("GEOID")]

  # 5. Intersect (EPSG:5070).
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

  # 6. Pull the (GEOID, variable, estimate, moe) panel and merge by GEOID.
  acs_drop <- if (inherits(acs_sf, "sf")) {
    sf::st_drop_geometry(acs_sf)
  } else acs_sf
  panel <- acs_drop[acs_drop$variable == variable,
                    c("GEOID", "variable", "estimate", "moe"),
                    drop = FALSE]
  inter <- merge(inter, panel, by = "GEOID", all.x = TRUE,
                 suffixes = c("", ".y"))

  # 7. Reproject to WGS84 for leaflet.
  inter_wgs   <- sf::st_transform(inter,    4326)
  iso_outline <- sf::st_transform(site_iso, 4326)

  # 8. Default view bounds.
  bbox <- .cacs_default_view(resolved$lat, resolved$lon,
                             padding_km = padding_km)

  # 9. Palette + popup HTML.
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

  # 10. Build widget.
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

  # 11. Site marker.
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


#' Map a site's headline rate estimates (walkthrough stage 4)
#'
#' @description
#' Draws the "answer" map for a site: a site marker on the combined
#' outline of all its isochrone rings, with a pinned pop-up listing the
#' five headline rates (poverty, SNAP, SSI, unemployment, and labour-force
#' participation). Each rate shows its point estimate, its 90 percent
#' margin of error (MOE) band, and the tract counts used in the numerator
#' and denominator (the `n_tracts_num` and `n_tracts_den` columns produced
#' by [cacs_intersect_weight()]). This is the fourth and final stage map.
#'
#' When a rate's margin of error had to be computed with the fallback
#' formula rather than the primary one (recorded in the `moe_fallback`
#' column), its name is prefixed with an asterisk so you know the reported
#' margin used the conservative fallback formula.
#'
#' You select the site with a `site_id`, a `lat`/`lon` pair, or a
#' `site_name`, tried in that order.
#'
#' @section Resolving the site:
#' Because the rate table is keyed by `site_id`, this map needs a concrete
#' site. If you supply a direct `lat`/`lon` pair, it is matched to the
#' nearest site in `sites_df` whose `site_id` also appears in `run_result`,
#' and a message reports the matched `site_id`, the `site_name` if known,
#' and the distance in kilometres. If the nearest match is more than 5 km
#' from the requested point, a warning asks you to check the coordinates or
#' pass a `site_id` directly.
#'
#' @param site_id Site identifier to look up in `sites_df`. When omitted,
#'   `sites_df` defaults to the bundled `cacs_alabama_sites` table.
#' @param lat,lon Alternative to `site_id`: a direct latitude/longitude
#'   coordinate pair in WGS84 (`lat` in `[-90, 90]`, `lon` in `[-180, 180]`),
#'   matched to the nearest known site (see the section above).
#' @param site_name Alternative to `site_id`: an exact match against the
#'   `site_name` column of `sites_df`.
#' @param iso_sf Isochrone polygons (an `sf` object) such as those returned
#'   by [cacs_isochrone()].
#' @param run_result Output of [cacs_run()]; filtered internally to the
#'   chosen site's rate rows.
#' @param sites_df An `sf` table of sites. When `NULL`, the bundled
#'   `cacs_alabama_sites` table is used.
#' @param tiles Name of the `leaflet` basemap provider tiles
#'   (default `"CartoDB.Positron"`).
#' @param padding_km Positive number giving the map padding, in kilometres,
#'   around the site point when setting the initial view. Default `5`.
#' @param ... Reserved for future keyword arguments; currently unused.
#'
#' @return A `leaflet` map (an HTML widget).
#' @family visual walkthrough helpers
#' @seealso [cacs_plot_site_pipeline()] to build all four stage maps at
#'   once, and [cacs_run()] for the end-to-end aggregation these maps
#'   illustrate.
#' @export
#' @examples
#' \dontrun{
#' iso <- readRDS(system.file("extdata", "legacy_2025_isochrones.rds",
#'                            package = "catchmentACS"))
#' result <- ... # a cacs_run() output
#' cacs_plot_site_rates(
#'   site_id = "AL_SITE_01",
#'   iso_sf  = iso,
#'   run_result = result
#' )
#' }
cacs_plot_site_rates <- function(site_id = NULL, lat = NULL, lon = NULL,
                                 site_name = NULL,
                                 iso_sf, run_result, sites_df = NULL,
                                 tiles = "CartoDB.Positron",
                                 padding_km = 5, ...) {
  .assert_leaflet_available()

  # 1. Resolve site input via shared helper (site_id > (lat, lon) > site_name).
  resolved <- .cacs_resolve_site_input(site_id, lat, lon, site_name, sites_df)

  # 2. Validate run_result schema.
  if (!inherits(run_result, c("tbl_df", "data.frame"))) {
    .cli_abort_schema(c(
      "{.arg run_result} must be a {.cls tbl_df} or {.cls data.frame}.",
      "x" = "Got {.cls {class(run_result)[[1L]]}}.",
      "i" = "Pass the output of {.fn cacs_run}."
    ))
  }
  required_cols <- c("site_id", "variable", "estimate", "moe",
                     "n_tracts_num", "n_tracts_den", "moe_fallback")
  missing_cols <- setdiff(required_cols, names(run_result))
  if (length(missing_cols) > 0L) {
    .cli_abort_schema(c(
      "{.arg run_result} missing required column{?s}: {.field {missing_cols}}.",
      "i" = "Pass the output of {.fn cacs_run} (27-col canonical long schema)."
    ))
  }
  resolved <- .cacs_promote_latlon_to_site(
    resolved = resolved,
    sites_df = sites_df,
    run_result = run_result,
    context = "cacs_plot_site_rates"
  )

  # 3. Filter run_result to (this site, rate rows only). v1.0 sanctioned
  #    rates are the 5 named members of cacs_acs_default_rates.
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

  # 4. Filter iso_sf to this site.
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

  # 5. Reproject to EPSG:5070, union all drive-time rings into a single
  #    outline polygon, then back to EPSG:4326 for leaflet rendering.
  iso_5070 <- sf::st_transform(site_iso, 5070)
  sf::st_agr(iso_5070) <- "constant"
  iso_union_5070 <- suppressWarnings(sf::st_union(iso_5070))
  iso_union <- sf::st_transform(sf::st_sfc(iso_union_5070, crs = 5070), 4326)

  # 6. Compute auto-bounds via shared helper.
  bbox <- .cacs_default_view(resolved$lat, resolved$lon,
                             padding_km = padding_km)

  # 7. Build the multi-row rate-summary popup HTML. Render one .cacs_popup_rate()
  #    block per rate row, then concatenate inside a single wrapper div so
  #    the marker popup has one anchor. Rows with moe_fallback = TRUE are
  #    visually flagged by prefixing the variable name with an asterisk
  #    (`*var`) before the popup is built.
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

  # 8. Build leaflet widget: tiles -> fitBounds -> isochrone outline ->
  #    site marker with sticky popup.
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


#' Build the full four-stage site walkthrough at once
#'
#' @description
#' Builds all four stage maps for a single site and returns them together
#' in one object, ready to display one after another in a Quarto chunk or
#' report. It calls [cacs_plot_site_isochrone()] (stage 1, the site and its
#' isochrone), [cacs_plot_site_intersection()] (stage 2, tract overlap),
#' [cacs_plot_site_weighted()] (stage 3, a census variable), and
#' [cacs_plot_site_rates()] (stage 4, headline rates) in turn, passing the
#' shared styling arguments (`tiles`, `padding_km`, `drive_time_min`, and
#' `...`) through to each. The site is resolved once up front, so a bad
#' site argument fails immediately rather than partway through.
#'
#' @section Resolving the site:
#' When you supply a direct `lat`/`lon` pair, the coordinates are matched
#' once to the nearest site in `run_result` and that `site_id` is reused
#' for all four stages. This prints one notice per call reporting the
#' match; if the matched site is more than 5 km away, it also warns you to
#' verify the coordinates.
#'
#' The companion [print.cacs_site_plot_pipeline()] method lists the four
#' maps and how to reach them. Display any single map by typing its
#' accessor (for example `x$isochrone`) at the console.
#'
#' @param site_id Site identifier to look up in `sites_df`. When omitted,
#'   `sites_df` defaults to the bundled `cacs_alabama_sites` table.
#' @param lat,lon Alternative to `site_id`: a direct latitude/longitude
#'   coordinate pair in WGS84 (`lat` in `[-90, 90]`, `lon` in `[-180, 180]`).
#' @param site_name Alternative to `site_id`: an exact match against the
#'   `site_name` column of `sites_df`.
#' @param iso_sf Isochrone polygons (an `sf` object) such as those returned
#'   by [cacs_isochrone()].
#' @param tract_sf Tract polygons (an `sf` object) with a `GEOID` column and
#'   polygon geometry, typically the same ACS data passed to
#'   [cacs_intersect_weight()]. Used by the overlap and variable maps.
#' @param acs_sf Long-format ACS data (an `sf` object) with columns `GEOID`,
#'   `variable`, `estimate`, and `moe`, typically the same object as
#'   `tract_sf`. Used by the variable map.
#' @param run_result Output of [cacs_run()]. Used by the rates map.
#' @param sites_df An `sf` table of sites. When `NULL`, the bundled
#'   `cacs_alabama_sites` table is used.
#' @param variable Single ACS variable code for the variable map
#'   (default `"B17001_002"`, the count of people below the poverty level).
#' @param variable_family Single string naming the variable's measurement
#'   family, used to pick the ColorBrewer palette for the variable map
#'   (see [cacs_plot_site_weighted()]). Default `"spatial_total"`.
#' @param tiles Name of the `leaflet` basemap provider tiles
#'   (default `"CartoDB.Positron"`).
#' @param padding_km Positive number giving the map padding, in kilometres,
#'   around the site point when setting the initial view. Default `5`.
#' @param drive_time_min Single integer, or `NULL`, naming which drive-time
#'   ring to use for the overlap and variable maps (the isochrone and rates
#'   maps always show all rings). When `NULL`, those maps use their default
#'   (the largest available ring for the site).
#' @param ... Reserved for future keyword arguments; passed through to each
#'   of the four stage functions.
#'
#' @return An object of class `"cacs_site_plot_pipeline"`: a list with four
#'   elements, `isochrone`, `intersection`, `weighted`, and `rates`, each a
#'   `leaflet` map. The resolved `site_id`, `lat`, `lon`, `name`, and the
#'   chosen `variable` are stored as attributes.
#' @family visual walkthrough helpers
#' @seealso The four stage maps it assembles:
#'   [cacs_plot_site_isochrone()], [cacs_plot_site_intersection()],
#'   [cacs_plot_site_weighted()], and [cacs_plot_site_rates()]; and
#'   [cacs_run()] for the end-to-end aggregation they illustrate.
#' @export
#' @examples
#' \dontrun{
#' iso <- readRDS(system.file("extdata", "legacy_2025_isochrones.rds",
#'                            package = "catchmentACS"))
#' acs <- readRDS(system.file("extdata", "sample_alabama_subset.rds",
#'                            package = "catchmentACS"))
#' result <- ... # cacs_run() output
#' pipe <- cacs_plot_site_pipeline(
#'   site_id    = "AL_SITE_01",
#'   iso_sf     = iso,
#'   tract_sf   = acs,
#'   acs_sf     = acs,
#'   run_result = result
#' )
#' pipe              # prints summary; doesn't auto-render
#' pipe$isochrone    # Stage 1 widget
#' pipe$rates        # Stage 4 widget
#' }
cacs_plot_site_pipeline <- function(site_id = NULL, lat = NULL, lon = NULL,
                                    site_name = NULL,
                                    iso_sf, tract_sf, acs_sf, run_result,
                                    sites_df = NULL,
                                    variable = "B17001_002",
                                    variable_family = "spatial_total",
                                    tiles = "CartoDB.Positron",
                                    padding_km = 5,
                                    drive_time_min = NULL, ...) {
  # 1. Fail-loud guard on leaflet first so missing-Suggests users get the
  #    standard install hint rather than a downstream stage's error.
  .assert_leaflet_available()

  # 2. Resolve site input up-front. Any bad-input abort happens here so we
  #    don't spend per-stage CPU/IO time before noticing the bad arg.
  resolved <- .cacs_resolve_site_input(site_id, lat, lon, site_name, sites_df)
  resolved <- .cacs_promote_latlon_to_site(
    resolved = resolved,
    sites_df = sites_df,
    run_result = run_result,
    context = "cacs_plot_site_pipeline"
  )

  # 3. Call each of the 4 stage fns with the resolved site_id and the
  #    relevant inputs. Pass shared kwargs (tiles, padding_km, ...) through
  #    so vignette / report authors can theme the whole pipeline uniformly.
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

  # 4. Wrap into classed list. Attributes carry the resolved site metadata
  #    so the print method can surface a one-line site header without
  #    re-resolving.
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


#' Print a site plot pipeline
#'
#' @description
#' Prints a compact summary of a four-map pipeline: the resolved site, the
#' variable used for the variable map, and the four accessors
#' (`x$isochrone`, `x$intersection`, `x$weighted`, `x$rates`). It does not
#' draw any map; type an accessor such as `x$isochrone` at the console, or
#' include it in a Quarto chunk, to view that map.
#'
#' @param x A site plot pipeline returned by [cacs_plot_site_pipeline()].
#' @param ... Currently unused.
#' @return `x`, invisibly.
#' @family visual walkthrough helpers
#' @seealso [cacs_plot_site_pipeline()], which creates the object this
#'   method prints.
#' @exportS3Method print cacs_site_plot_pipeline
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
    "{.code x$weighted}      - Stage 3: tract chloropleth on {.code {meta_var}}",
    "{.code x$rates}         - Stage 4: 5-rate popup summary"
  ))
  cli::cli_alert_info("Display any stage by typing its accessor in the console.")
  invisible(x)
}


#' Format a site plot pipeline as a one-line string
#'
#' @description
#' Returns a single-line summary string suitable for inline use, for
#' example when interpolating the object into report text. The fuller
#' multi-line summary is produced by [print.cacs_site_plot_pipeline()].
#'
#' @param x A site plot pipeline returned by [cacs_plot_site_pipeline()].
#' @param ... Currently unused.
#' @return A length-one character string.
#' @family visual walkthrough helpers
#' @seealso [cacs_plot_site_pipeline()], which creates the object this
#'   method formats.
#' @exportS3Method format cacs_site_plot_pipeline
format.cacs_site_plot_pipeline <- function(x, ...) {
  sprintf("<cacs_site_plot_pipeline: 4 widgets for site %s>",
          attr(x, "site_id") %||% "(unresolved)")
}


# Local %||% - mirrors the pattern used in intersect-weight.R / isochrone-*.R
# files so plot-site.R stays standalone (no cross-file import order dep).
`%||%` <- function(x, y) if (is.null(x)) y else x
