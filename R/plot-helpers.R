# R/plot-helpers.R - shared helpers for Phase 6 visualization suite.
# Step 5.4 introduces the assertion helper; Step 6.1 [CORE] adds the
# input resolver + auto-bounds. Steps 6.3-6.6 fill in the per-stage
# `cacs_plot_site_*()` bodies in R/plot-site.R using these helpers.

#' Assert leaflet is installed (fail-loud with helpful message)
#'
#' Internal helper used by every `cacs_plot_site_*()` function. Since
#' `leaflet` is in Suggests (not Imports), users who skip viz install
#' don't pay the dependency cost; this helper fails clearly when needed.
#'
#' @return TRUE invisibly if leaflet available; aborts via cli_abort otherwise.
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


#' Resolve site input across the 3 supported entry forms (internal)
#'
#' Shared input resolver for the F5 `cacs_plot_site_*()` family. Accepts
#' one of three mutually-permitted input forms, in precedence order:
#'   1. `site_id`           - lookup in `sites_df` by exact match.
#'   2. `(lat, lon)`        - direct coordinate pair (no lookup).
#'   3. `site_name`         - case-sensitive exact match against
#'                            `sites_df$site_name` (must resolve uniquely).
#'
#' By design,
#' all input-validation errors route through the existing
#' `.cli_abort_schema()` family rather than a new `plot_input` family.
#' If a downstream step (e.g. Step 6.4 variable-not-in-intersect_result)
#' wants finer-grained routing, register `"plot_input"` in
#' `.cacs_cond_classes()` at that time.
#'
#' `sites_df` defaults to the bundled `cacs_alabama_sites` when `NULL`.
#'
#' @param site_id character(1) or NULL; site identifier to look up.
#' @param lat,lon numeric(1) or NULL; direct WGS84 coordinates
#'   (lat in `[-90, 90]`, lon in `[-180, 180]`).
#' @param site_name character(1) or NULL; exact-match site label.
#' @param sites_df sf POINT object (default `cacs_alabama_sites`) with
#'   columns `site_id`, `site_name`, and POINT geometry.
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
  # All-NULL fail-loud
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
      help = "All cacs_plot_site_*() functions use the same site input resolver."
    )
  }

  # Default sites_df to bundled fixture when caller didn't supply one
  if (is.null(sites_df)) {
    sites_df <- get("cacs_alabama_sites",
                    envir = asNamespace("catchmentACS"))
  }

  # Precedence: site_id > (lat, lon) > site_name
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

  # site_name path (only reached when site_id and (lat, lon) both NULL)
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


#' Resolve direct plot coordinates to the nearest run-result site (internal)
#'
#' Stage 4 and the full plot pipeline need a concrete `site_id` because their
#' data products are keyed by `run_result$site_id`. This helper preserves the
#' documented `(lat, lon)` plot input form by matching those coordinates to
#' the nearest site row and returning that row's id/metadata.
#'
#' @param lat,lon numeric(1); WGS84 input coordinates.
#' @param sites_df sf/data.frame with `site_id` and either POINT geometry or
#'   `lon`/`lat` columns. Defaults to `cacs_alabama_sites` when NULL.
#' @param run_result cacs_run() long output with a `site_id` column.
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
      "i" = "Stage 4 and pipeline `(lat, lon)` inputs are resolved against the sites present in {.arg run_result}."
    ))
  }
  if (is.null(sites_df)) {
    sites_df <- get("cacs_alabama_sites",
                    envir = asNamespace("catchmentACS"))
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


#' Promote direct plot coordinates to a concrete site id when needed
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


#' Compute a default WGS84 view bbox for leaflet::fitBounds() (internal)
#'
#' Returns a bounding box centered at `(lat, lon)` and padded by
#' `padding_km` on each side. Uses small-angle approximations:
#'   * 1 degree of latitude  ~ 111 km  (constant across latitudes).
#'   * 1 degree of longitude ~ 111 * cos(lat * pi/180) km
#'     (shrinks toward the poles).
#'
#' Adequate for the per-site initial-zoom use case (isochrone radius
#' typically 5-30 km); not intended for cross-continental views.
#'
#' @param lat,lon numeric(1); center coordinates in WGS84.
#' @param padding_km numeric(1) > 0; padding distance in kilometres on
#'   each side of the center (so total bbox spans ~2*padding_km on
#'   the N-S axis).
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
  # Guard against cos(lat) underflow at the poles; clamp to a tiny
  # positive value so we never divide by zero. (Won't fire for any
  # plausible Alabama site, but keeps the helper safe for unit tests
  # near the poles.)
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


# ============================================================================
# Step 6.2 [CORE] - F5 color palettes + popup HTML builders.
#
# Palette design (hybrid scheme):
#
#   * Stage 2 area_wt surface  -> viridis continuous, FIXED domain [0, 1]
#     (cross-site comparable; 0.4 at site A = 0.4 at site B).
#   * Stage 3 variable surface -> ColorBrewer family-mapped:
#       spatial_total                        -> "Greens"   (sequential)
#       area_weighted_scalar_proxy           -> "YlOrRd"   (sequential)
#       population_weighted_scalar_proxy     -> "YlOrRd"   (same family)
#       area_weighted_rate_proxy             -> "YlOrRd"   (same family)
#       median_proxy                         -> "PuOr"     (diverging)
#       derived_rate                         -> "RdYlBu"   (diverging palette,
#                                                           observed-domain bins)
#       metadata_only                        -> abort (not plottable)
#       NA / unrecognized                    -> "YlGnBu"   (safe fallback)
#
# Binning: 5-bin quantile/jenks-style via `classInt::classIntervals()` when
# available (Suggests-gated; no new DESCRIPTION dep added), with a
# `pretty()` fallback that always works. When fewer than 5 finite values
# exist, linear-degrade to `n = length(unique(finite_values))`.
#
# XSS contract for popups: every user/data-origin string interpolation point
# goes through `htmltools::htmlEscape()`. Numeric fields use `formatC()` /
# `sprintf()` (deterministic R coercion - no escape needed). HTML literal
# entities (e.g. plus/minus glyph) are wrapped with `htmltools::HTML()` so
# they survive escape. Test fixture T-PLOT-H-22 verifies `<script>` payloads
# are escaped to `&lt;script&gt;`.
# ============================================================================


#' Build a viridis-on-fixed-domain palette closure for `area_wt` (internal)
#'
#' Returns a `leaflet::colorNumeric()` closure on the FIXED domain `[0, 1]`
#' so that the same `area_wt` value maps to the same color across sites
#' (cross-site visual comparability). The `values` argument is accepted
#' only to validate that the input is plausibly an `area_wt` vector
#' (numeric, finite values in `[0, 1]` after NA removal); it is NOT used
#' to derive the palette domain.
#'
#' Requires the `viridisLite` namespace (a transitive `leaflet` dep). If
#' unavailable, falls back to leaflet's built-in `"viridis"` palette name
#' (which leaflet resolves via its own viridisLite import).
#'
#' @param values numeric; an `area_wt` vector. Used only for input
#'   validation - the returned closure's domain is locked to `[0, 1]`.
#' @param na_color character(1); CSS color string for NA inputs.
#'   Default `"#CCCCCC"` (neutral mid-grey).
#'
#' @return a `leaflet::colorNumeric()` palette closure that maps a numeric
#'   vector to a character vector of hex colors. Calling the closure with
#'   a value outside `[0, 1]` (after the closure's internal clamp) yields
#'   the endpoint color rather than an error.
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
  # leaflet::colorNumeric expects either (a) a palette NAME string that it
  # resolves through scales/viridisLite, or (b) a colors-producing closure of
  # the form `function(n) -> character(n)`. Passing `viridisLite::viridis`
  # directly fails because that function's signature is `(n, alpha, begin,
  # end, ...)` and leaflet calls it with `f(x)` where x is a length>1 vector
  # of rescaled values. So we use the name string and let leaflet route
  # through its bundled viridisLite import.
  leaflet::colorNumeric(
    palette  = "viridis",
    domain   = c(0, 1),
    na.color = na_color
  )
}


#' Build a ColorBrewer chloropleth palette closure per estimand_family (internal)
#'
#' Returns a `leaflet::colorBin()` closure whose ColorBrewer family is
#' selected from `variable_family` per the palette table below.
#' Bin breaks are computed via `classInt::classIntervals(style = "jenks")`
#' when the suggest is available, else via `pretty()`. When fewer than 5
#' finite values exist, `n` linear-degrades to `length(unique(finite))`.
#'
#' @param values numeric; the variable's estimate vector (across tracts /
#'   sites for the chosen panel). NAs are dropped before binning; NA input
#'   to the returned closure renders as `na_color`.
#' @param variable_family character(1); one of `.ESTIMAND_FAMILIES` (or NA
#'   for the YlGnBu fallback). `"metadata_only"` aborts.
#' @param na_color character(1); CSS color for NA inputs. Default `"#CCCCCC"`.
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

  # Abort loudly on metadata_only (not a plottable family).
  if (!is.na(variable_family) &&
      identical(as.character(variable_family), "metadata_only")) {
    .cli_abort_schema(c(
      "Cannot build chloropleth palette for {.val metadata_only} family.",
      "x" = "{.val metadata_only} rows carry NA estimates and are not plottable.",
      "i" = "Filter rows where {.field estimand_family != \"metadata_only\"} first."
    ))
  }

  # Family -> ColorBrewer palette name lookup.
  brewer_pal <- switch(
    if (is.na(variable_family)) "_na_" else as.character(variable_family),
    "spatial_total"                    = "Greens",
    "area_weighted_scalar_proxy"       = "YlOrRd",
    "population_weighted_scalar_proxy" = "YlOrRd",
    "area_weighted_rate_proxy"         = "YlOrRd",
    "median_proxy"                     = "PuOr",
    "derived_rate"                     = "RdYlBu",
    # NA-family and any unrecognized family fall back to YlGnBu.
    "YlGnBu"
  )

  finite <- values[is.finite(values)]
  uniq_n <- length(unique(finite))

  # Bin count: 5 by default; linear-degrade for tiny vectors.
  n_bins <- if (uniq_n < 5L) max(uniq_n, 1L) else 5L

  bins <- NULL
  if (uniq_n >= 2L) {
    # Prefer classInt::classIntervals(style = "jenks") when the suggest is
    # installed; v0.2 deliberately does NOT add classInt to DESCRIPTION, so
    # gate on requireNamespace() and fall back to pretty().
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
      # Tiny-range pretty() can collapse to a single value; force a 2-bin
      # epsilon range so colorBin() does not abort.
      eps  <- max(abs(finite)) * 1e-6 + 1e-9
      bins <- c(finite[1L] - eps, finite[1L] + eps)
    }
  } else {
    # 0 or 1 unique finite value: degenerate degraded palette (single bin).
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


#' Fill `{{field}}` interpolation points in a mustache-style template (internal)
#'
#' Tiny hand-rolled whisker substitute. Replaces every `{{field}}` in
#' `template` with the matching value from the named-list `data`. Caller
#' is responsible for escaping (we do NOT auto-escape here so the popup
#' builders retain explicit control of which fields go through
#' `htmltools::htmlEscape()` vs. `htmltools::HTML()`).
#'
#' Unmatched `{{fields}}` are replaced with `""` (silent passthrough so
#' optional fields can be omitted from `data`).
#'
#' @param template character(1) raw HTML template.
#' @param data named list; values must coerce to character(1) via `as.character`.
#' @return character(1) - filled template.
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
  # Replace all known {{field}} markers.
  for (nm in names(data)) {
    pat <- paste0("\\{\\{\\s*", nm, "\\s*\\}\\}")
    val <- as.character(data[[nm]])
    if (length(val) == 0L || is.na(val)) val <- ""
    out <- gsub(pat, val, out, perl = TRUE)
  }
  # Drop any unfilled {{...}} markers so the popup never leaks the syntax.
  out <- gsub("\\{\\{\\s*[A-Za-z0-9_]+\\s*\\}\\}", "", out, perl = TRUE)
  out
}


#' Extract a named <!--BEGIN name-->...<!--END name--> block (internal)
#'
#' Pulls one mustache section out of the multi-section template file. The
#' shared `inst/templates/popup.html` ships 3 sections (`"site"`, `"tract"`,
#' `"rate"`) keyed by HTML comment fences. Section names are restricted to
#' `[A-Za-z0-9_-]+` to avoid regex injection from the caller.
#'
#' @param raw character(1) full template file contents.
#' @param section character(1) section name.
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
      "i" = "Edit {.file inst/templates/popup.html} to add the missing section."
    ))
  }
  m[[2L]]
}


#' Read + render a named popup template section (internal)
#'
#' Locates `inst/templates/<file>.html` via `system.file()`, extracts the
#' named section via `.extract_template_section()`, and fills it via
#' `.fill_template()`. Caller is expected to have already escaped every
#' user-supplied field via `htmltools::htmlEscape()` (see popup builders).
#'
#' Template file is fixed at `popup.html` for v0.2; the function signature
#' accepts `template_name` so a future version can add per-section files
#' without breaking the call sites.
#'
#' @param template_name character(1) section name (one of `"site"`, `"tract"`,
#'   `"rate"`).
#' @param data named list of already-escaped character(1) values.
#'
#' @return character(1) - rendered HTML safe to hand to
#'   `leaflet::addPopups()` / `leaflet::addMarkers(popup = ...)`.
#' @keywords internal
#' @noRd
.render_popup <- function(template_name, data) {
  path <- system.file("templates", "popup.html", package = "catchmentACS")
  if (!nzchar(path) || !file.exists(path)) {
    .cli_abort_schema(c(
      "Popup template file {.file inst/templates/popup.html} not found.",
      "i" = "Reinstall {.pkg catchmentACS} so the template is restaged."
    ))
  }
  raw     <- paste(readLines(path, warn = FALSE), collapse = "\n")
  section <- .extract_template_section(raw, template_name)
  .fill_template(section, data)
}


#' Build the site-marker popup HTML (internal)
#'
#' Routes through `.render_popup("site", ...)` with every string field
#' escaped via `htmltools::htmlEscape()` and numeric fields rendered via
#' `formatC()`.
#'
#' @param site_row named list / 1-row data.frame / 1-row tibble with at
#'   least `site_id`, `site_name`, `lat`, `lon`.
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


#' Build the tract polygon popup HTML (internal)
#'
#' Renders one HTML popup per tract row. The `area_wt` field is always
#' shown (Stage 2 surface); the per-variable section is conditionally
#' included when `vars` is non-empty and the tract row carries values
#' for them.
#'
#' @param tract_row named list / 1-row data.frame with at least `GEOID`,
#'   `area_wt`, and any variable columns named in `vars`. Optional
#'   `intersection_km2` is rendered if present.
#' @param vars character; variable column names to render (one row per
#'   variable in the popup body). Default `character(0)` (no variable rows).
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
    variable_rows    = rows_html  # already-escaped HTML; do NOT escape again
  )
  .render_popup("tract", data)
}


#' Build the per-site rate-result popup HTML (internal)
#'
#' Renders mean +/- MOE with `n_tracts_num`/`n_tracts_den` counts and the
#' `moe_fallback` indicator (a logical-ish field that we render as
#' "fallback applied" / "n/a" so the user does not have to know the
#' internal column name).
#'
#' @param rate_row named list / 1-row data.frame with `variable`,
#'   `estimate`, `moe`, `n_tracts_num`, `n_tracts_den`, `moe_fallback`,
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


#' NULL/NA-aware coalesce helper for the popup builders (internal)
#'
#' Distinct from `rlang::\\\`%||%\\\`` / the package's existing
#' `\\\`%||%\\\`` (which only catches NULL): this also collapses length-1
#' NA into `b`. Lives here as a regular-name function to avoid colliding
#' with the load-order-shadowed `%||%` operators defined in the other R
#' files (see `R/isochrone-normalize.R`, `R/intersect-weight.R`, etc.).
#'
#' @param a value to test.
#' @param b fallback if `a` is `NULL` or length-1 `NA`.
#' @keywords internal
#' @noRd
.coalesce_na <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 1L && is.na(a)) return(b)
  a
}
