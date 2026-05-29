# ============================================================================
# utils.R - geometry repair, per-site safe wrappers, empty-result constructor,
#           pretty printers. Sec. 15.3, Sec. 21.3 step 3, Sec. 19.3 step 7, Sec. 27.5 (EC-21..27).
# ============================================================================
#
# Edge cases covered:
#   * NULL input            -> early return NULL (caller decides)
#   * empty sfc / 0-row sf  -> return as-is, no repair attempted
#   * GEOMETRYCOLLECTION    -> collapse to POLYGON union via st_collection_extract
#   * mixed POLYGON + MULTIPOLYGON -> harmonize via st_cast (no abort)
#   * row-level repair fail in a batch -> mark just those rows EMPTY via warn,
#                                          do not abort the batch
#   * 3-retry chain exhausted on a row -> warn + return with EMPTY geometry
# ============================================================================


#' Repair geometry via a 3-step retry chain (internal)
#'
#' Defensive repair for sf input that may contain invalid, empty,
#' GEOMETRYCOLLECTION, or mixed POLYGON/MULTIPOLYGON geometries. The chain is
#' (1) early return when all valid, (2) `sf::st_make_valid()`,
#' (3) zero-width buffer fallback. Each retry is wrapped in `tryCatch()` to
#' contain GEOS exceptions. Per-row failures degrade to EMPTY-marked rows so
#' the caller can isolate the bad subset; a batch-level abort is only raised
#' when all rows fail all retries (cf. Sec. 21.9 E-21-15).
#'
#' @param x an `sf` object, an `sfc`, or `NULL`. Other classes abort schema.
#' @param collapse_collection logical; when TRUE (default) any GEOMETRYCOLLECTION
#'   feature is replaced by the union of its POLYGON components (others dropped).
#' @return repaired object of the same outer class as `x`; `NULL` if `x` is NULL.
#' @keywords internal
#' @noRd
.repair_geometry <- function(x, collapse_collection = TRUE) {

  ## --- Edge case A: NULL passthrough -------------------------------------
  if (is.null(x)) return(NULL)

  ## --- Edge case B: wrong class ------------------------------------------
  if (!inherits(x, c("sf", "sfc"))) {
    .cli_abort_schema(c(
      "{.fn .repair_geometry} requires {.cls sf} or {.cls sfc}.",
      "x" = "Got {.cls {class(x)[[1]]}}.",
      "i" = "Caller must validate input before invoking the repair chain."
    ))
  }

  ## --- Edge case C: zero-row / empty sfc ---------------------------------
  geom <- if (inherits(x, "sf")) sf::st_geometry(x) else x
  if (length(geom) == 0L) return(x)

  ## --- Edge case D: GEOMETRYCOLLECTION collapse --------------------------
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

  ## --- Edge case E: mixed POLYGON / MULTIPOLYGON -------------------------
  gtypes <- vapply(geom, function(g) class(g)[[1L]], character(1))
  if (length(setdiff(gtypes, c("POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION"))) == 0L &&
      length(unique(gtypes)) > 1L) {
    x <- tryCatch(
      sf::st_cast(x, "MULTIPOLYGON", warn = FALSE),
      error = function(e) x  # leave as-is if cast fails; downstream will catch
    )
    geom <- if (inherits(x, "sf")) sf::st_geometry(x) else x
  }

  ## --- Retry chain --------------------------------------------------------
  valid_now <- function(g) {
    suppressWarnings(sf::st_is_valid(g, reason = FALSE))
  }
  is_valid <- valid_now(x)
  if (all(is_valid, na.rm = TRUE) && !anyNA(is_valid)) return(x)

  # Retry 1: st_make_valid
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

  # Retry 2: zero-width buffer fallback
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

  # Exhausted: warn + return whatever we have so caller can NA-mark per-row
  last  <- if (!is.null(x_bf)) x_bf else if (!is.null(x_mv)) x_mv else x
  bad   <- which(!valid_now(last) | is.na(valid_now(last)))
  n_bad <- length(bad)
  n_tot <- if (inherits(last, "sf")) nrow(last) else length(sf::st_geometry(last))

  if (n_bad == n_tot) {
    .cli_abort_geometry(c(
      "Geometry repair exhausted on all {n_tot} row{?s}.",
      "x" = "Affected rows: {.val {utils::head(bad, 10)}}{?/ ...more}",
      "i" = "Inspect with {.code sf::st_is_valid(x, reason = TRUE)}."
    ))
  }

  .cli_warn_runtime(c(
    "Geometry repair exhausted on {n_bad}/{n_tot} row{?s}; marking row geometry as empty.",
    "x" = "Affected rows: {.val {utils::head(bad, 10)}}",
    "i" = "Downstream per-site loops will return {.fn .empty_site_result} for these."
  ))
  # Replace bad geometries with EMPTY so st_intersection short-circuits cleanly
  if (inherits(last, "sf")) {
    empties <- rep(list(sf::st_polygon()), n_bad)
    sf::st_geometry(last)[bad] <- sf::st_sfc(empties, crs = sf::st_crs(last))
  }
  last
}


#' Per-site safe wrapper around an sf-producing expression (internal)
#'
#' Wraps `expr` in `withCallingHandlers()` + `tryCatch()` so that:
#'   1. `cli_warn` from inside `expr` propagates up (captured by Sec. 24.7).
#'   2. Errors are caught and converted to a list of class
#'      `catchmentACS_site_failure` that the caller's `purrr::map()` body
#'      converts to `.empty_site_result()`.
#'   3. The original `call` is preserved for `rlang::trace_back()`.
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


#' Construct the canonical zero-intersection result row (internal)
#'
#' Returns a 1-row tibble matching the partial-column subset of the Sec. 21.7
#' 20-mandatory-column long schema that `cacs_intersect_weight()` produces
#' for empty-intersection sites. `dplyr::bind_rows()` in Sec. 21 step 14a will
#' pad missing columns with NA, which is intentional.
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


# ---- Pretty-printing helpers (used by cli.R cache/progress wrappers) -------

#' Format elapsed milliseconds (internal)
#' @keywords internal
#' @noRd
pretty_ms <- function(x) {
  if (!is.finite(x) || x < 0) return("n/a")
  if (x < 1000)        sprintf("%.0f ms",  x)
  else if (x < 60000)  sprintf("%.2f s",   x / 1000)
  else                 sprintf("%.1f min", x / 60000)
}

#' Format byte counts (internal)
#' @keywords internal
#' @noRd
pretty_bytes <- function(x) {
  if (!is.finite(x) || x < 0) return("n/a")
  units <- c("B", "KB", "MB", "GB", "TB")
  i <- min(length(units), floor(log(max(x, 1), 1024)) + 1L)
  sprintf("%.1f %s", x / (1024 ^ (i - 1L)), units[i])
}

#' Format row counts with thousands separator (internal)
#' @keywords internal
#' @noRd
pretty_n <- function(x) {
  if (!is.finite(x)) return("n/a")
  format(x, big.mark = ",", scientific = FALSE)
}
