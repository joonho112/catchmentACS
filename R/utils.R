# utils.R - geometry repair, a wrapper that turns an error in one site into a
# value the caller can carry on with, the row for a site and drive time with
# no estimates, and three helpers that format numbers for messages.


#' Repair invalid geometries
#'
#' Takes an `sf` object or an `sfc` that may hold invalid, empty,
#' GEOMETRYCOLLECTION, or mixed POLYGON and MULTIPOLYGON geometries, and
#' repairs what it can. Valid input is returned unchanged; otherwise
#' `sf::st_make_valid()` is tried, and then a buffer of width zero. Each try
#' is wrapped in `tryCatch()`, because GEOS raises an error on some inputs.
#' When some rows are still invalid, those rows of an `sf` object are given
#' an empty polygon and a warning names them, so the rest of the batch can go
#' on; only when every row is still invalid does the function give an error.
#'
#' @param x an `sf` object, an `sfc`, or `NULL`. Anything else gives an error.
#' @param collapse_collection logical; when TRUE (default) any GEOMETRYCOLLECTION
#'   feature is replaced by the union of its POLYGON components (others dropped).
#' @return an object of the same class as `x`; `NULL` if `x` is `NULL`.
#' @keywords internal
#' @noRd
.repair_geometry <- function(x, collapse_collection = TRUE) {

  if (is.null(x)) return(NULL)

  if (!inherits(x, c("sf", "sfc"))) {
    .cli_abort_schema(c(
      "{.fn .repair_geometry} requires {.cls sf} or {.cls sfc}.",
      "x" = "Got {.cls {class(x)[[1]]}}.",
      "i" = "Caller must validate input before invoking the repair chain."
    ))
  }

  # Nothing to repair in an empty sfc or a zero-row sf.
  geom <- if (inherits(x, "sf")) sf::st_geometry(x) else x
  if (length(geom) == 0L) return(x)

  # A GEOMETRYCOLLECTION is replaced by the union of its polygons; the
  # points and lines in it are dropped, which the warning says.
  is_gc <- vapply(geom, inherits, logical(1), what = "GEOMETRYCOLLECTION")
  if (collapse_collection && any(is_gc)) {
    geom_fixed <- geom
    for (i in which(is_gc)) {
      collapsed <- tryCatch(
        sf::st_union(sf::st_collection_extract(geom[i], "POLYGON")),
        error = function(e) sf::st_geometrycollection()  # empty placeholder
      )
      geom_fixed[i] <- collapsed
    }
    if (inherits(x, "sf")) {
      sf::st_geometry(x) <- geom_fixed
    } else {
      x <- geom_fixed
    }
    geom <- geom_fixed
    .cli_warn_runtime(c(
      "Collapsed {sum(is_gc)} GEOMETRYCOLLECTION feature{?s} to POLYGON union.",
      "i" = "Non-polygon members were dropped."
    ))
  }

  # A batch that mixes POLYGON and MULTIPOLYGON is cast to one type. If the
  # cast fails the batch is left as it is: sf::st_intersection() takes both.
  gtypes <- vapply(geom, function(g) class(g)[[1L]], character(1))
  if (length(setdiff(gtypes, c("POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION"))) == 0L &&
      length(unique(gtypes)) > 1L) {
    x <- tryCatch(
      sf::st_cast(x, "MULTIPOLYGON", warn = FALSE),
      error = function(e) x  # leave as-is if cast fails; downstream will catch
    )
    geom <- if (inherits(x, "sf")) sf::st_geometry(x) else x
  }

  valid_now <- function(g) {
    suppressWarnings(sf::st_is_valid(g, reason = FALSE))
  }
  is_valid <- valid_now(x)
  if (all(is_valid, na.rm = TRUE) && !anyNA(is_valid)) return(x)

  # First try: st_make_valid().
  x_mv <- tryCatch(sf::st_make_valid(x),
                   error = function(e) NULL,
                   warning = function(w) {
                     suppressWarnings(sf::st_make_valid(x))
                   })
  if (!is.null(x_mv)) {
    vv <- valid_now(x_mv)
    if (all(vv, na.rm = TRUE) && !anyNA(vv)) {
      .cli_warn_runtime(c(
        "Geometry repair applied for {sum(!is_valid | is.na(is_valid))} row{?s} via {.fn st_make_valid}."
      ))
      return(x_mv)
    }
  }

  # Second try: a buffer of width zero, which rebuilds the rings.
  base <- if (!is.null(x_mv)) x_mv else x
  x_bf <- tryCatch(sf::st_buffer(base, 0),
                   error = function(e) NULL)
  if (!is.null(x_bf)) {
    vb <- valid_now(x_bf)
    if (all(vb, na.rm = TRUE) && !anyNA(vb)) {
      .cli_warn_runtime(c(
        "Geometry repair applied via {.fn st_buffer} (0-width) fallback."
      ))
      return(x_bf)
    }
  }

  # Still invalid rows. An error only when every row is invalid; otherwise
  # the bad rows get an empty polygon (in an sf object) and a warning names
  # them, so the caller can drop or report just those sites.
  last  <- if (!is.null(x_bf)) x_bf else if (!is.null(x_mv)) x_mv else x
  bad   <- which(!valid_now(last) | is.na(valid_now(last)))
  n_bad <- length(bad)
  n_tot <- if (inherits(last, "sf")) nrow(last) else length(sf::st_geometry(last))

  if (n_bad == n_tot) {
    .cli_abort_geometry(c(
      "Geometry repair failed on all {n_tot} row{?s}.",
      "x" = "Rows: {.val {utils::head(bad, 10)}}{if (length(bad) > 10L) ' (first 10 shown)' else ''}.",
      "i" = "Inspect with {.code sf::st_is_valid(x, reason = TRUE)}."
    ))
  }

  .cli_warn_runtime(c(
    "Geometry repair failed on {n_bad} of {n_tot} row{?s}; these rows get an empty geometry.",
    "x" = "Rows: {.val {utils::head(bad, 10)}}{if (length(bad) > 10L) ' (first 10 shown)' else ''}.",
    "i" = "Rows with an empty geometry intersect nothing and add nothing to the results."
  ))
  # An empty polygon makes st_intersection() return nothing for these rows,
  # rather than raising an error on them.
  if (inherits(last, "sf")) {
    empties <- rep(list(sf::st_polygon()), n_bad)
    sf::st_geometry(last)[bad] <- sf::st_sfc(empties, crs = sf::st_crs(last))
  }
  last
}


#' Evaluate an expression for one site and keep its errors
#'
#' Evaluates `expr` in the environment of the caller and returns its value.
#' An error, of any class, is caught and returned instead as a list with
#' `.failure = TRUE`, the site and drive time given here, the message, the
#' condition itself, and its classes, so that a loop over sites can record
#' the failure and go on to the next site. Warnings and messages of the
#' package's own classes are raised again from here, so that a handler around
#' the loop still sees them; other warnings and messages are left alone.
#'
#' No code in the package calls this; the tests in
#' `tests/testthat/test-unit-utils.R` do.
#'
#' @keywords internal
#' @noRd
.safe_geom <- function(expr, site_id = NA_character_,
                       drive_time_min = NA_integer_, handler = NULL) {
  caller <- rlang::caller_env()
  expr_q <- substitute(expr)

  re_emit <- function(cond) {
    if (!is.null(handler)) handler(cond)
    rlang::cnd_signal(cond)
    invokeRestart("muffleWarning")
  }

  tryCatch(
    withCallingHandlers(
      eval(expr_q, envir = caller),
      warning = function(w) {
        if (inherits(w, "catchmentACS_warning")) re_emit(w)
      },
      message = function(m) {
        if (inherits(m, "catchmentACS_message")) re_emit(m)
      }
    ),
    catchmentACS_error = function(e) {
      list(
        .failure       = TRUE,
        site_id        = site_id,
        drive_time_min = drive_time_min,
        reason         = conditionMessage(e),
        cond           = e,
        class_chain    = class(e)
      )
    },
    error = function(e) {
      list(
        .failure       = TRUE,
        site_id        = site_id,
        drive_time_min = drive_time_min,
        reason         = conditionMessage(e),
        cond           = e,
        class_chain    = class(e)
      )
    }
  )
}


#' The row for a site and drive time with no estimates
#'
#' Returns a one-row tibble for a site and drive-time pair that ends with no
#' estimates. It has 16 columns: 15 of the 23 columns of the long table, and
#' `failure_reason`, which says what happened (`"no_tract_intersection"` and
#' three other values, or the message of an error) and which
#' `cacs_intersect_weight()` removes from the result. The eight columns left
#' out, among them `ring_topology`, `provider` and `acs_year`, are added as
#' `NA` when `dplyr::bind_rows()` puts this row with the rows that have
#' estimates.
#'
#' @keywords internal
#' @noRd
.empty_site_result <- function(site_id,
                               drive_time_min,
                               n_tracts = 0L,
                               failure_reason = "no_tract_intersection") {
  tibble::tibble(
    site_id                = as.character(site_id),
    drive_time_min         = as.integer(drive_time_min),
    variable               = NA_character_,
    estimate               = NA_real_,
    moe                    = NA_real_,
    weight_sum             = NA_real_,
    n_tracts               = as.integer(n_tracts),
    estimand_family        = NA_character_,
    weight_basis           = NA_character_,
    moe_formula_requested  = NA_character_,
    moe_formula_effective  = NA_character_,
    moe_fallback           = NA,
    moe_fallback_reason    = "n/a",
    failure_origin         = "intersection",
    failure_reason         = failure_reason,
    weight_uncertainty_propagated = FALSE
  )
}


# Three helpers that format a number for a message. Of the three, only
# pretty_bytes() is used in the package, in the message of cacs_clear_cache()
# (R/cache.R); all three have tests.

#' Format elapsed milliseconds
#' @keywords internal
#' @noRd
pretty_ms <- function(x) {
  if (!is.finite(x) || x < 0) return("n/a")
  if (x < 1000)        sprintf("%.0f ms",  x)
  else if (x < 60000)  sprintf("%.2f s",   x / 1000)
  else                 sprintf("%.1f min", x / 60000)
}

#' Format byte counts
#' @keywords internal
#' @noRd
pretty_bytes <- function(x) {
  if (!is.finite(x) || x < 0) return("n/a")
  units <- c("B", "KB", "MB", "GB", "TB")
  i <- min(length(units), floor(log(max(x, 1), 1024)) + 1L)
  sprintf("%.1f %s", x / (1024 ^ (i - 1L)), units[i])
}

#' Format row counts with thousands separator
#' @keywords internal
#' @noRd
pretty_n <- function(x) {
  if (!is.finite(x)) return("n/a")
  format(x, big.mark = ",", scientific = FALSE)
}
