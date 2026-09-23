# moe-helpers.R - margins of error. The formulas for a sum, a weighted sum, a
# weighted average, a proportion, and a ratio (the .moe_*_self() functions);
# cacs_se_to_moe() and cacs_moe_to_se(); the classifier that gives each ACS
# variable code its kind of estimate; and cacs_propagate_moe() with the
# helpers that choose a formula for each row, look up the numerator and
# denominator of a rate, and handle the rows where a formula gives no value.


# A denominator whose absolute value is below .DEN_EPS (about 1.5e-8) counts
# as zero. .is_zero_den(NA) is FALSE; callers check for NA separately.
.DEN_EPS <- sqrt(.Machine$double.eps)

.is_zero_den <- function(x) {
  !is.na(x) & abs(x) < .DEN_EPS
}

.check_equal_lengths <- function(..., labels = NULL) {
  vals <- list(...)
  lens <- vapply(vals, length, integer(1))
  if (length(unique(lens)) != 1L) {
    msg <- "MOE helper inputs must have equal lengths."
    if (!is.null(labels)) {
      msg <- paste0(msg, " Got ", paste(paste0(labels, "=", lens), collapse = ", "), ".")
    }
    .cli_abort_schema(msg)
  }
  invisible(TRUE)
}


# Margin-of-error formulas for one sum, weighted sum, weighted average,
# proportion, or ratio. The formulas for sums have no covariance terms: they
# treat the estimates as independent. .moe_prop_self() and .moe_ratio_self()
# are used by cacs_propagate_moe() and cacs_derive_rates(); the other three
# only by the tests, since the pipeline computes the variances of weighted
# sums and averages in .sum_weighted_variance() (R/intersect-weight.R).

#' Margin of error of a sum of estimates
#'
#' `z * sqrt(sum((moe / z)^2))`, which equals `sqrt(sum(moe^2))`: the
#' handbook's formula for a sum (U.S. Census Bureau 2020, chapter 8), at the
#' confidence level of the input margins of error. Returns `NA` if any margin
#' of error is `NA`.
#'
#' @keywords internal
#' @noRd
.moe_sum_self <- function(moe_vec, z = .Z_ACS_90) {
  if (anyNA(moe_vec)) return(NA_real_)
  z * sqrt(sum((moe_vec / z)^2))
}

#' Margin of error of a weighted sum of estimates
#'
#' `z * sqrt(sum((w * moe / z)^2))`, which equals `sqrt(sum((w * moe)^2))`:
#' the formula for a sum applied to the weighted estimates, with the weights
#' treated as fixed. Returns `NA` if any margin of error or weight is `NA`,
#' and stops with an error if `moe_vec` and `w_vec` differ in length.
#'
#' @keywords internal
#' @noRd
.moe_wsum_self <- function(moe_vec, w_vec, z = .Z_ACS_90) {
  .check_equal_lengths(moe_vec, w_vec, labels = c("moe_vec", "w_vec"))
  if (anyNA(moe_vec) || anyNA(w_vec)) return(NA_real_)
  z * sqrt(sum((w_vec * moe_vec / z)^2))
}

#' Margin of error of a weighted average of estimates
#'
#' The margin of error of the weighted sum divided by `sum(w)`, so the
#' weights need not sum to one. Returns `NA` if any margin of error or weight
#' is `NA` or if the weights sum to zero.
#'
#' @keywords internal
#' @noRd
.moe_wmean_self <- function(moe_vec, w_vec, z = .Z_ACS_90) {
  .check_equal_lengths(moe_vec, w_vec, labels = c("moe_vec", "w_vec"))
  if (anyNA(moe_vec) || anyNA(w_vec)) return(NA_real_)
  w_sum <- sum(w_vec)
  if (.is_zero_den(w_sum)) return(NA_real_)
  z * sqrt(sum((w_vec * moe_vec / z)^2)) / w_sum
}

#' Margin of error of a proportion
#'
#' The proportion formula (C1), for `p = num_est / den_est` when the
#' numerator is part of the denominator. With `Var_A = (num_moe / z)^2` and
#' `Var_B = (den_moe / z)^2`, the value is
#' `z * sqrt(Var_A - p^2 * Var_B) / den_est`, which equals
#' `sqrt(num_moe^2 - p^2 * den_moe^2) / den_est` (U.S. Census Bureau 2020,
#' chapter 8). The value under the square root can be negative; the callers
#' then use the ratio formula (C2) for that rate, as the handbook advises.
#'
#' @return A list with
#'   - `value`: the margin of error, or `NA` when one of the flags is `TRUE`;
#'   - `zero_denominator`: `den_est` counts as zero (`.is_zero_den()`);
#'   - `missing_moe`: `num_moe` or `den_moe` is `NA`;
#'   - `missing_estimate`: `num_est` or `den_est` is `NA`;
#'   - `inside_negative`: the value under the square root is negative.
#'
#' @keywords internal
#' @noRd
.moe_prop_self <- function(num_est, num_moe, den_est, den_moe, z = .Z_ACS_90) {
  # At most one flag is TRUE: the first that applies in the order below.
  if (is.na(num_est) || is.na(den_est)) {
    return(list(
      value             = NA_real_,
      zero_denominator  = FALSE,
      missing_moe       = FALSE,
      missing_estimate  = TRUE,
      inside_negative   = FALSE
    ))
  }
  if (.is_zero_den(den_est)) {
    return(list(
      value             = NA_real_,
      zero_denominator  = TRUE,
      missing_moe       = FALSE,
      missing_estimate  = FALSE,
      inside_negative   = FALSE
    ))
  }
  if (is.na(num_moe) || is.na(den_moe)) {
    return(list(
      value             = NA_real_,
      zero_denominator  = FALSE,
      missing_moe       = TRUE,
      missing_estimate  = FALSE,
      inside_negative   = FALSE
    ))
  }
  Var_A <- (num_moe / z)^2
  Var_B <- (den_moe / z)^2
  p     <- num_est / den_est
  inside <- Var_A - (p^2) * Var_B
  if (inside < 0) {
    return(list(
      value             = NA_real_,
      zero_denominator  = FALSE,
      missing_moe       = FALSE,
      missing_estimate  = FALSE,
      inside_negative   = TRUE
    ))
  }
  list(
    value             = z * sqrt(inside) / den_est,
    zero_denominator  = FALSE,
    missing_moe       = FALSE,
    missing_estimate  = FALSE,
    inside_negative   = FALSE
  )
}

#' Margin of error of a ratio
#'
#' The ratio formula (C2): `z * sqrt(Var_A + p^2 * Var_B) / abs(den_est)`,
#' with `Var_A`, `Var_B`, and `p` as in `.moe_prop_self()`, which equals
#' `sqrt(num_moe^2 + p^2 * den_moe^2) / abs(den_est)` (U.S. Census Bureau
#' 2020, chapter 8). The value under the square root is a sum of two
#' non-negative terms, so it is never negative. Returns `NA` if the
#' denominator is zero or an estimate or margin of error is `NA`.
#'
#' @keywords internal
#' @noRd
.moe_ratio_self <- function(num_est, num_moe, den_est, den_moe, z = .Z_ACS_90) {
  if (.is_zero_den(den_est)) return(NA_real_)
  if (is.na(num_est) || is.na(den_est)) return(NA_real_)
  if (is.na(num_moe) || is.na(den_moe)) return(NA_real_)
  Var_A <- (num_moe / z)^2
  Var_B <- (den_moe / z)^2
  p     <- num_est / den_est
  inside <- Var_A + (p^2) * Var_B  # always >= 0
  z * sqrt(inside) / abs(den_est)
}


# Converting between margins of error and standard errors.

#' Convert a standard error to a margin of error
#'
#' Multiplies a standard error (SE) by the normal quantile \eqn{z} for the
#' level, giving the margin of error (MOE):
#' \eqn{\mathrm{MOE} = z \cdot \mathrm{SE}}{MOE = z * SE}. A margin of
#' error is the half-width of a confidence interval. When `level` is `0.90`,
#' the level of published American Community Survey (ACS) margins of error,
#' the function uses \eqn{z = 1.645}, the value the Census Bureau uses. At
#' other levels it uses `qnorm(1 - (1 - level) / 2)`, for example about 1.96
#' at `level = 0.95`.
#'
#' @param se A numeric vector of standard errors.
#' @param level A single number between 0 and 1 (not a percentage) giving
#'   the confidence level of the margin of error. The default is `0.9`. The
#'   value is not checked: `level = 90`, for example, gives `NaN` with a
#'   warning.
#' @return A numeric vector of margins of error, the same length as `se`.
#' @references U.S. Census Bureau (2020). *Understanding and Using American
#'   Community Survey Data: What All Data Users Need to Know*. Chapter 7,
#'   Understanding Error and Determining Statistical Significance.
#' @seealso [cacs_propagate_moe()] computes margins of error for drive-time
#'   area estimates.
#' @family rates and margins of error
#' @export
#' @examples
#' cacs_se_to_moe(c(5, 10, NA), level = 0.90)
cacs_se_to_moe <- function(se, level = 0.90) {
  z <- if (identical(level, 0.90)) .Z_ACS_90 else stats::qnorm(1 - (1 - level) / 2)
  z * se
}

#' Convert a margin of error to a standard error
#'
#' Divides a margin of error (MOE) by the normal quantile \eqn{z} for the
#' level, giving the standard error (SE):
#' \eqn{\mathrm{SE} = \mathrm{MOE} / z}{SE = MOE / z}. A margin of error is
#' the half-width of a confidence interval. When `level` is `0.90`, the
#' level of published American Community Survey (ACS) margins of error, the
#' function uses \eqn{z = 1.645}, the value the Census Bureau uses. At other
#' levels it uses `qnorm(1 - (1 - level) / 2)`. The function is the inverse
#' of [cacs_se_to_moe()] at the same level.
#'
#' ACS 1-year margins of error for 2005 and earlier were published with
#' \eqn{z = 1.65}; dividing them by 1.65 gives the standard error. The
#' values of `moe` are not checked. The negative codes that the Census
#' Bureau's data API puts in place of some margins of error, such as
#' `-555555555`, are divided like any other number and give negative
#' standard errors. [cacs_acs_prefetch()] returns these codes as `NA`.
#'
#' @param moe A numeric vector of margins of error.
#' @param level A single number between 0 and 1 (not a percentage) giving
#'   the confidence level at which the margins of error were computed. The
#'   default is `0.9`. The value is not checked: `level = 90`, for example,
#'   gives `NaN` with a warning.
#' @return A numeric vector of standard errors, the same length as `moe`.
#' @references U.S. Census Bureau (2020). *Understanding and Using American
#'   Community Survey Data: What All Data Users Need to Know*. Chapter 7,
#'   Understanding Error and Determining Statistical Significance.
#' @seealso [cacs_propagate_moe()] computes margins of error for drive-time
#'   area estimates.
#' @family rates and margins of error
#' @export
#' @examples
#' cacs_moe_to_se(c(8.225, 16.45, NA), level = 0.90)
cacs_moe_to_se <- function(moe, level = 0.90) {
  z <- if (identical(level, 0.90)) .Z_ACS_90 else stats::qnorm(1 - (1 - level) / 2)
  moe / z
}


# Kind of estimate (estimand_family) of each ACS variable code.

# The 14 codes of cacs_acs_default_vars. B19013_001 is median household
# income and B19301_001 per capita income; the other twelve are counts.
.DEFAULT_VAR_FAMILY_MAP <- c(
  "B01003_001" = "spatial_total",
  "B17001_001" = "spatial_total",
  "B17001_002" = "spatial_total",
  "B11001_001" = "spatial_total",
  "B22003_001" = "spatial_total",
  "B22003_002" = "spatial_total",
  "B19056_001" = "spatial_total",
  "B19056_002" = "spatial_total",
  "B23025_001" = "spatial_total",
  "B23025_002" = "spatial_total",
  "B23025_003" = "spatial_total",
  "B23025_005" = "spatial_total",
  "B19013_001" = "median_proxy",
  "B19301_001" = "area_weighted_scalar_proxy"
)

#' Kind of estimate of one ACS variable code
#'
#' Returns `"spatial_total"` (a count), `"median_proxy"` (a median), or
#' `"area_weighted_scalar_proxy"` (a per-person value) for the codes in
#' `.DEFAULT_VAR_FAMILY_MAP` and for any cell of the tables `B19013` (median
#' household income), `B25077` (median home value), and `B19301` (per capita
#' income). Every other code of the form `B`, five digits, an underscore, and
#' three digits is treated as a count, so a median or per-person value from
#' another table is added up like a count. Other codes (tables whose names
#' begin with `C` or `S`, or have a letter after the digits, such as
#' `B17001A_002`), `NA`, and `""` give `"metadata_only"`: not combined by
#' `cacs_intersect_weight()`. The other three values of `.ESTIMAND_FAMILIES`
#' are never returned.
#'
#' @keywords internal
#' @noRd
.classify_acs_variable <- function(var_code) {
  if (is.na(var_code) || !nzchar(var_code)) return("metadata_only")

  if (var_code %in% names(.DEFAULT_VAR_FAMILY_MAP)) {
    return(unname(.DEFAULT_VAR_FAMILY_MAP[var_code]))
  }

  if (grepl("^B19013_\\d{3}$", var_code)) return("median_proxy")
  if (grepl("^B19301_\\d{3}$", var_code)) return("area_weighted_scalar_proxy")
  if (grepl("^B25077_\\d{3}$", var_code)) return("median_proxy")
  if (grepl("^B[0-9]{5}_\\d{3}$", var_code)) return("spatial_total")

  "metadata_only"
}

#' Kind of estimate of each code in a vector of ACS variable codes
#'
#' @keywords internal
#' @noRd
.classify_acs_variable_batch <- function(var_codes) {
  vapply(var_codes, .classify_acs_variable, character(1), USE.NAMES = FALSE)
}


# The formula for each row of cacs_propagate_moe(): the default for its kind
# of estimate, or the one chosen with the formula argument.

#' Default formula for each row, by kind of estimate
#'
#' Returns `"weighted_sum"` for counts; `"weighted_mean"` for medians,
#' per-person values, and the two other proxy kinds, which the classifier
#' never gives; `"general_ratio_conservative"` (the ratio formula, the
#' package default for every rate) for rates; and `NA` for
#' `"metadata_only"`, `NA`, and any other value. `R/intersect-weight.R` gives
#' the same values in `.dispatch_moe_formula_effective()`.
#'
#' @param estimand_family A character vector of kinds of estimate.
#' @return A character vector of formula names, `NA` where there is none.
#' @keywords internal
#' @noRd
.dispatch_formula_auto <- function(estimand_family) {
  out <- rep(NA_character_, length(estimand_family))
  out[estimand_family == "spatial_total"] <- "weighted_sum"
  out[estimand_family %in% c("area_weighted_scalar_proxy",
                             "population_weighted_scalar_proxy",
                             "area_weighted_rate_proxy",
                             "median_proxy")] <- "weighted_mean"
  out[estimand_family == "derived_rate"] <- "general_ratio_conservative"
  out[estimand_family == "metadata_only"] <- NA_character_
  out
}


#' Formula for each row from a named list
#'
#' Starts from the default formulas (`.dispatch_formula_auto()`) and gives
#' the rows with `data$variable == v` the formula `formula[[v]]`, for each
#' name `v` of `formula`. Does not check its input: `.resolve_formula()`
#' checks the names and values first.
#'
#' @param formula A list of formula names, named by values of
#'   `data$variable`.
#' @param data A tibble with columns `variable` and `estimand_family`.
#' @return A character vector with one formula per row of `data`.
#' @keywords internal
#' @noRd
.dispatch_formula_list <- function(formula, data) {
  out <- .dispatch_formula_auto(data$estimand_family)
  for (v in names(formula)) {
    idx <- which(data$variable == v)
    if (length(idx) > 0L) {
      out[idx] <- formula[[v]]
    }
  }
  out
}


#' Rows whose formula does not suit their kind of estimate
#'
#' Returns the indices of the rows whose pair of `estimand_family` and
#' formula is not one of these (an empty integer vector if all rows fit):
#'   - `"spatial_total"`: `"weighted_sum"`;
#'   - `"area_weighted_scalar_proxy"`, `"population_weighted_scalar_proxy"`,
#'     `"area_weighted_rate_proxy"`, and `"median_proxy"`: `"weighted_mean"`;
#'   - `"derived_rate"`: `"proportion_subset"` or
#'     `"general_ratio_conservative"`;
#'   - `"metadata_only"`: `NA` (no formula).
#' A row whose `estimand_family` is `NA` or not one of these values is
#' always returned. `cacs_propagate_moe()` checks the rows of pairs that did
#' not fail and stops with an error if any row is returned, so a formula
#' chosen for the wrong kind of estimate, or a row without a known kind,
#' gives an error instead of a margin of error.
#'
#' @param family A character vector of kinds of estimate.
#' @param formula A character vector of formula names of the same length
#'   (`NA` allowed).
#' @return An integer vector of row indices.
#' @keywords internal
#' @noRd
.check_family_formula_consistency <- function(family, formula) {
  if (length(family) != length(formula)) {
    .cli_abort_schema(c(
      "{.fn .check_family_formula_consistency} length mismatch.",
      "x" = "family length = {.val {length(family)}}; formula length = {.val {length(formula)}}."
    ))
  }
  n <- length(family)
  if (n == 0L) return(integer(0))

  ok <- vapply(seq_len(n), function(i) {
    fam <- family[[i]]
    frm <- formula[[i]]
    if (is.na(fam)) return(FALSE)
    if (identical(fam, "metadata_only")) {
      return(is.na(frm))
    }
    if (is.na(frm)) {
      return(FALSE)
    }
    switch(
      fam,
      "spatial_total"                    = identical(frm, "weighted_sum"),
      "area_weighted_scalar_proxy"       = identical(frm, "weighted_mean"),
      "population_weighted_scalar_proxy" = identical(frm, "weighted_mean"),
      "area_weighted_rate_proxy"         = identical(frm, "weighted_mean"),
      "median_proxy"                     = identical(frm, "weighted_mean"),
      "derived_rate"                     = frm %in% c("proportion_subset",
                                                       "general_ratio_conservative"),
      FALSE  # any other kind of estimate
    )
  }, logical(1L))

  which(!ok)
}


#' Formula for each row from the `formula` argument
#'
#' `NULL` gives every row its default (`.dispatch_formula_auto()`). A single
#' string is given to every row except the `"metadata_only"` rows, which keep
#' `NA`; a name on the string is ignored. A named list changes the default
#' only for the rows of the named variables (`.dispatch_formula_list()`). An
#' unknown formula, an unnamed list or element, a name that is not a value
#' of `data$variable`, and any other kind of value give a
#' `catchmentACS_error_schema` error. Whether each formula suits its row is
#' checked afterwards (`.check_family_formula_consistency()`).
#'
#' @param formula `NULL`, a single string from `.SANCTIONED_MOE_FORMULAS`, or
#'   a list of such strings named by values of `data$variable`.
#' @param data A tibble with columns `estimand_family` and `variable`.
#' @return A character vector with one formula per row of `data`.
#' @keywords internal
#' @noRd
.resolve_formula <- function(formula, data) {
  if (is.null(formula)) {
    return(.dispatch_formula_auto(data$estimand_family))
  }
  if (is.character(formula) && length(formula) == 1L) {
    if (!formula %in% .SANCTIONED_MOE_FORMULAS) {
      .cli_abort_schema(c(
        "{.arg formula} must be one of {.val {(.SANCTIONED_MOE_FORMULAS)}}.",
        "x" = "Got {.val {formula}}."
      ))
    }
    out <- rep(formula, nrow(data))
    # "metadata_only" rows keep NA: the consistency check rejects any formula
    # on them.
    out[data$estimand_family == "metadata_only"] <- NA_character_
    return(out)
  }
  if (is.list(formula)) {
    nm <- names(formula)
    if (is.null(nm) || any(!nzchar(nm))) {
      .cli_abort_schema(c(
        "{.arg formula} list must have non-empty names for every element.",
        "i" = "Names are matched to {.field variable}."
      ))
    }
    unknown_vars <- setdiff(nm, unique(data$variable))
    if (length(unknown_vars) > 0L) {
      .cli_abort_schema(c(
        "{.arg formula} list contains unknown variable name{?s}: {.field {unknown_vars}}.",
        "i" = "Each name in {.arg formula} must match a value in {.field data$variable}."
      ))
    }
    flat_vals <- unlist(formula, use.names = FALSE)
    bad_vals <- setdiff(flat_vals, .SANCTIONED_MOE_FORMULAS)
    if (length(bad_vals) > 0L) {
      .cli_abort_schema(c(
        "{.arg formula} ({.arg moe_formula} in {.fn cacs_run}) list values must be names of margin-of-error formulas.",
        "x" = "Not allowed: {.val {bad_vals}}.",
        "i" = "Allowed values: {.val {(.SANCTIONED_MOE_FORMULAS)}}."
      ))
    }
    return(.dispatch_formula_list(formula, data))
  }
  .cli_abort_schema(c(
    "{.arg formula} must be NULL, a character(1), or a named list.",
    "x" = "Got {.cls {class(formula)[[1]]}}."
  ))
}


#' z value for a confidence level
#'
#' At `level = 0.90` returns `.Z_ACS_90` (1.645), the value that
#' `cacs_intersect_weight()` uses to turn tract margins of error into
#' standard errors and back. With `qnorm(0.95)` (1.644854) the margins of
#' error of `cacs_propagate_moe()` at the default level would differ slightly
#' from those of `cacs_intersect_weight()`. `all.equal()` rather than `==`
#' lets a level with rounding error, such as `0.3 * 3`, also get 1.645. Other
#' levels use the two-sided normal quantile `qnorm(1 - (1 - level) / 2)`.
#'
#' @param level A single number strictly between 0 and 1; any other value
#'   gives a `catchmentACS_error_schema` error.
#' @return The z value.
#' @keywords internal
#' @noRd
.resolve_z_from_level <- function(level) {
  if (!is.numeric(level) || length(level) != 1L || is.na(level) ||
      !is.finite(level)) {
    .cli_abort_schema(c(
      "{.arg level} must be a finite numeric scalar.",
      "x" = "Got {.val {level}}."
    ))
  }
  if (level <= 0 || level >= 1) {
    .cli_abort_schema(c(
      "{.arg level} must be strictly in {.val (0, 1)}.",
      "x" = "Got {.val {level}}."
    ))
  }
  if (isTRUE(all.equal(level, 0.90))) .Z_ACS_90
  else stats::qnorm(1 - (1 - level) / 2)
}


# The weighted totals and means and their variances are not in the rows of
# data but in its cacs_aggregation_carriers attribute, the table that
# cacs_intersect_weight() attaches (one row per site, drive time, and
# variable, with the columns in .CARRIER_REQUIRED_COLS). .left_join_carriers()
# adds them to the rows of the same site, drive time, and variable; the
# lookup helpers below match on the numerator or denominator code of a rate
# instead of the row's own variable.

#' Add the weighted totals and means and their variances to the rows
#'
#' Joins `est_total`, `var_total_raw`, `est_mean`, and `var_mean_raw` of
#' `carrier` to `data` by site, drive time, and variable; rows without a
#' match get `NA`. `carrier` is `NULL` only when no row of `data` has a kind
#' of estimate other than `"metadata_only"` (`cacs_propagate_moe()` checks
#' this), and `data` is then returned unchanged.
#'
#' @param data A tibble with columns `site_id`, `drive_time_min`, and
#'   `variable`.
#' @param carrier The `cacs_aggregation_carriers` attribute of `data` (a
#'   table with the columns in `.CARRIER_REQUIRED_COLS`), or `NULL`.
#' @return `data` with the four columns added.
#' @keywords internal
#' @noRd
.left_join_carriers <- function(data, carrier) {
  if (is.null(carrier)) return(data)
  carrier_join <- carrier[, c("site_id", "drive_time_min", "variable",
                              "est_total", "var_total_raw",
                              "est_mean", "var_mean_raw"), drop = FALSE]
  # Drop the four columns first if data already has them (for example the
  # output of an earlier cacs_propagate_moe() call), so that the join does
  # not add .x and .y copies.
  drop_cols <- intersect(names(data),
                         c("est_total", "var_total_raw",
                           "est_mean", "var_mean_raw"))
  if (length(drop_cols) > 0L) {
    data <- data[, setdiff(names(data), drop_cols), drop = FALSE]
  }
  dplyr::left_join(
    data, carrier_join,
    by = c("site_id", "drive_time_min", "variable")
  )
}


# Lookups by rate name, for the rate rows of cacs_propagate_moe() and for
# cacs_derive_rates(). Each helper takes one rate name, the
# cacs_aggregation_carriers table, and vectors of site IDs and drive times,
# and returns one of its columns for the numerator or denominator code of
# that rate (from .SANCTIONED_RATES_V1), matched on site, drive time, and
# code, with NA where there is no match. Both callers pass one site and drive
# time at a time. The three parts are matched at once with `match()` on a
# single packed string rather than row by row, so the lookup stays fast on a
# large `carriers` table; the cost is that each call builds the keys of the
# whole table.

# The three parts of a key are joined with the ASCII unit separator (0x1F), a
# control character that site IDs, drive times, and ACS codes do not normally
# contain (site IDs are only checked to be non-empty strings; the bundled
# ones contain "_"), so different sites, drive times, and codes give
# different keys.
.CARRIER_KEY_SEP <- "\x1f"

.carrier_key_lookup <- function(rate_name, carriers, site_id, drive_time_min,
                                code_role, value_col) {
  if (!rate_name %in% names(.SANCTIONED_RATES_V1)) {
    .cli_abort_schema(c(
      "{.arg rate_name} must be one of {.val {names(.SANCTIONED_RATES_V1)}}.",
      "x" = "Got {.val {rate_name}}."
    ))
  }
  if (!code_role %in% c("num", "den")) {
    .cli_abort_schema(c(
      "{.arg code_role} must be {.val num} or {.val den}.",
      "x" = "Got {.val {code_role}}."
    ))
  }
  if (!value_col %in% names(carriers)) {
    .cli_abort_schema(c(
      "The {.attr cacs_aggregation_carriers} attribute has no column {.field {value_col}}.",
      "i" = "This attribute comes from {.fn cacs_intersect_weight}; keep it as it is."
    ))
  }
  if (length(site_id) != length(drive_time_min)) {
    .cli_abort_schema(c(
      "{.arg site_id} and {.arg drive_time_min} must have equal length.",
      "x" = "site_id length = {.val {length(site_id)}}; drive_time_min length = {.val {length(drive_time_min)}}."
    ))
  }
  acs_code <- .SANCTIONED_RATES_V1[[rate_name]][[code_role]]
  if (length(site_id) == 0L) return(numeric(0))

  query_key <- paste(site_id, drive_time_min, acs_code,
                     sep = .CARRIER_KEY_SEP)
  carrier_key <- paste(carriers$site_id, carriers$drive_time_min,
                       carriers$variable, sep = .CARRIER_KEY_SEP)
  idx <- match(query_key, carrier_key)
  out <- carriers[[value_col]][idx]
  # A column that is not numeric (such as a logical column of NA) gives
  # doubles; an integer column such as n_tracts stays integer.
  if (!is.numeric(out)) out <- as.numeric(out)
  out
}

#' Weighted total of the numerator, for rate rows
#'
#' `est_total` of the numerator code of `rate_name` for each site and drive
#' time, `NA` where `carriers` has no such row or the total is `NA`.
#' `cacs_derive_rates()` then gives the rate `failure_origin = "carrier"`;
#' `cacs_propagate_moe()` records `"zero_denominator"` in
#' `moe_fallback_reason`.
#'
#' @param rate_name A string, one of `names(.SANCTIONED_RATES_V1)`.
#' @param carriers The `cacs_aggregation_carriers` table, with columns
#'   `site_id`, `drive_time_min`, `variable`, and the column looked up.
#' @param site_id A character vector of site IDs.
#' @param drive_time_min A vector of drive times in minutes, the same length
#'   as `site_id`.
#' @return A numeric vector, `NA` where there is no match.
#' @keywords internal
#' @noRd
.lookup_carrier_num <- function(rate_name, carriers, site_id, drive_time_min) {
  .carrier_key_lookup(rate_name, carriers, site_id, drive_time_min,
                      code_role = "num", value_col = "est_total")
}

#' Weighted total of the denominator, for rate rows
#'
#' As `.lookup_carrier_num()`, for the denominator code.
#'
#' @inheritParams .lookup_carrier_num
#' @keywords internal
#' @noRd
.lookup_carrier_den <- function(rate_name, carriers, site_id, drive_time_min) {
  .carrier_key_lookup(rate_name, carriers, site_id, drive_time_min,
                      code_role = "den", value_col = "est_total")
}

#' Variance of the numerator, for rate rows
#'
#' `var_total_raw` of the numerator code: the variance (the square of the
#' standard error) of its weighted total. The callers turn it into a margin
#' of error at the chosen level, `z * sqrt(var_total_raw)`.
#'
#' @inheritParams .lookup_carrier_num
#' @keywords internal
#' @noRd
.lookup_carrier_num_var <- function(rate_name, carriers, site_id, drive_time_min) {
  .carrier_key_lookup(rate_name, carriers, site_id, drive_time_min,
                      code_role = "num", value_col = "var_total_raw")
}

#' Variance of the denominator, for rate rows
#'
#' As `.lookup_carrier_num_var()`, for the denominator code.
#'
#' @inheritParams .lookup_carrier_num
#' @keywords internal
#' @noRd
.lookup_carrier_den_var <- function(rate_name, carriers, site_id, drive_time_min) {
  .carrier_key_lookup(rate_name, carriers, site_id, drive_time_min,
                      code_role = "den", value_col = "var_total_raw")
}

#' Number of tracts behind the numerator, for rate rows
#'
#' `n_tracts` of the numerator code (for example `B17001_002` for
#' `poverty_rate`), the number of tracts combined for it, which
#' `cacs_derive_rates()` puts in the `n_tracts_num` column of rate rows.
#' The values come back as stored (integer).
#'
#' @inheritParams .lookup_carrier_num
#' @return An integer vector, `NA` where there is no match.
#' @keywords internal
#' @noRd
.lookup_carrier_n_tracts_num <- function(rate_name, carriers, site_id, drive_time_min) {
  .carrier_key_lookup(rate_name, carriers, site_id, drive_time_min,
                      code_role = "num", value_col = "n_tracts")
}

#' Number of tracts behind the denominator, for rate rows
#'
#' As `.lookup_carrier_n_tracts_num()`, for the denominator code
#' (`n_tracts_den`). The two counts differ when a tract has a row for one code
#' and not the other.
#'
#' @inheritParams .lookup_carrier_num
#' @return An integer vector, `NA` where there is no match.
#' @keywords internal
#' @noRd
.lookup_carrier_n_tracts_den <- function(rate_name, carriers, site_id, drive_time_min) {
  .carrier_key_lookup(rate_name, carriers, site_id, drive_time_min,
                      code_role = "den", value_col = "n_tracts")
}


# What cacs_propagate_moe() writes for groups of rate rows: a zero denominator
# or a missing estimate, a missing margin of error, a negative value under the
# square root of C1 (C2 is used instead), and C1 with a value. Each helper
# sets moe and the three columns that record how it was computed for the rows
# in idx and returns data. With an empty idx it returns data unchanged, so the
# caller does not test for empty groups (tests/testthat/test-unit-known-answer.R
# checks this). is_C1 tells .apply_zero_den() and .apply_missing_moe() which
# formula was chosen for the rows; the other two helpers accept it and do not
# use it.

#' Mark rate rows with a zero denominator
#'
#' Sets `moe` to `NA`, `moe_fallback` to `TRUE`, and `moe_fallback_reason` to
#' `"zero_denominator"`. `cacs_propagate_moe()` uses it also for rows whose
#' numerator or denominator estimate is missing. `moe_formula_effective` keeps
#' the formula chosen for the rows (`"proportion_subset"` if `is_C1` is
#' `TRUE`, else `"general_ratio_conservative"`) rather than a special label,
#' so that the column holds only formula names and `moe_fallback_reason` says
#' why there is no value.
#'
#' @param data A tibble with columns `moe`, `moe_formula_effective`,
#'   `moe_fallback`, and `moe_fallback_reason`.
#' @param idx An integer vector of the rows to change.
#' @param is_C1 `TRUE` if the formula chosen for the rows is C1, `FALSE` if
#'   it is C2.
#' @return `data` with those rows changed.
#' @keywords internal
#' @noRd
.apply_zero_den <- function(data, idx, is_C1 = TRUE) {
  if (length(idx) == 0L) return(data)
  data$moe[idx] <- NA_real_
  data$moe_formula_effective[idx] <- if (isTRUE(is_C1)) {
    "proportion_subset"
  } else {
    "general_ratio_conservative"
  }
  data$moe_fallback[idx] <- TRUE
  data$moe_fallback_reason[idx] <- "zero_denominator"
  data
}

#' Mark rate rows with a missing margin of error
#'
#' As `.apply_zero_den()`, with `moe_fallback_reason = "missing_moe"`, for
#' rows where the margin of error of the numerator or denominator is missing.
#'
#' @inheritParams .apply_zero_den
#' @keywords internal
#' @noRd
.apply_missing_moe <- function(data, idx, is_C1 = TRUE) {
  if (length(idx) == 0L) return(data)
  data$moe[idx] <- NA_real_
  data$moe_formula_effective[idx] <- if (isTRUE(is_C1)) {
    "proportion_subset"
  } else {
    "general_ratio_conservative"
  }
  data$moe_fallback[idx] <- TRUE
  data$moe_fallback_reason[idx] <- "missing_moe"
  data
}

#' Use C2 for rows where C1 has a negative value under the root
#'
#' Sets `moe` to the value of the ratio formula (C2, `.moe_ratio_self()`),
#' `moe_formula_effective` to `"general_ratio_conservative"`, `moe_fallback`
#' to `TRUE`, and `moe_fallback_reason` to `"negative_variance"`. The value
#' under the square root of the proportion formula (C1) can be negative; the
#' ratio formula is then used instead, as the handbook advises (U.S. Census
#' Bureau 2020, chapter 8). Only C1 rows come here: the value under the
#' square root of C2 is a sum of two non-negative terms and is never
#' negative.
#'
#' @param data A tibble with columns `moe`, `moe_formula_effective`,
#'   `moe_fallback`, and `moe_fallback_reason`.
#' @param idx An integer vector of the rows to change.
#' @param num_est A numeric vector with one numerator estimate per element of
#'   `idx`.
#' @param num_moe The margins of error of the numerator, likewise.
#' @param den_est The estimates of the denominator, likewise.
#' @param den_moe The margins of error of the denominator, likewise.
#' @param z The z value of the confidence level.
#' @param is_C1 Not used (`cacs_propagate_moe()` passes `TRUE`).
#' @return `data` with those rows changed.
#' @keywords internal
#' @noRd
.apply_c2_fallback <- function(data, idx, num_est, num_moe,
                               den_est, den_moe, z, is_C1 = TRUE) {
  if (length(idx) == 0L) return(data)
  c2_vals <- vapply(seq_along(idx), function(i) {
    .moe_ratio_self(num_est[i], num_moe[i], den_est[i], den_moe[i], z = z)
  }, numeric(1L))
  data$moe[idx] <- c2_vals
  data$moe_formula_effective[idx] <- "general_ratio_conservative"
  data$moe_fallback[idx] <- TRUE
  data$moe_fallback_reason[idx] <- "negative_variance"
  data
}

#' Write the value of the proportion formula (C1)
#'
#' Sets `moe` to the value from `.moe_prop_self()`, `moe_formula_effective` to
#' `"proportion_subset"`, `moe_fallback` to `FALSE`, and
#' `moe_fallback_reason` to `"n/a"`. `cacs_propagate_moe()` sends here the
#' rows for which `.moe_prop_self()` set no flag, so the value is computed a
#' second time from the same inputs.
#'
#' @inheritParams .apply_c2_fallback
#' @keywords internal
#' @noRd
.apply_c1_success <- function(data, idx, num_est, num_moe,
                              den_est, den_moe, z, is_C1 = TRUE) {
  if (length(idx) == 0L) return(data)
  c1_vals <- vapply(seq_along(idx), function(i) {
    res <- .moe_prop_self(num_est[i], num_moe[i], den_est[i], den_moe[i],
                          z = z)
    res$value
  }, numeric(1L))
  data$moe[idx] <- c1_vals
  data$moe_formula_effective[idx] <- "proportion_subset"
  data$moe_fallback[idx] <- FALSE
  data$moe_fallback_reason[idx] <- "n/a"
  data
}


# Margins of error from variances, z * sqrt(var), with NA where the variance
# is NA or negative. cacs_aggregation_carriers holds variances
# (var_total_raw), and .moe_prop_self() and .moe_ratio_self() take margins
# of error.
.var_to_moe <- function(var, z) {
  out <- rep(NA_real_, length(var))
  ok <- !is.na(var) & var >= 0
  out[ok] <- z * sqrt(var[ok])
  out
}


#' Compute margins of error for drive-time area estimates
#'
#' Computes a margin of error (MOE) for each estimate in the output of
#' [cacs_intersect_weight()], at the confidence level given by `level`. A
#' margin of error is the half-width of a confidence interval. The formula
#' depends on the kind of estimate and is recorded in the
#' `moe_formula_effective` column. American Community Survey (ACS) margins
#' of error are published at the 90 percent level; at the default
#' `level = 0.9`, the margins of error of counts, medians, and per-person
#' values are the same as those that [cacs_intersect_weight()] returns.
#'
#' The margins of error are combined as if the census tract estimates were
#' independent. The Census Bureau's handbook for ACS data users notes that
#' its approximation formulas leave out the covariance between estimates,
#' so a margin of error can be too small or too large depending on the
#' correlation between them (U.S. Census Bureau 2020, chapter 8). The
#' margin of error of a count, a median, or a per-person value is too small
#' if the tract estimates are positively correlated. For a rate, errors
#' that move the numerator and the denominator in the same direction partly
#' offset each other in the ratio, and the package does not compute the net
#' effect for a given rate. The area-based weights of the tracts are
#' treated as fixed, so the margins of error leave out two more sources of
#' error. One is the assumption that whatever a variable counts is spread
#' evenly over each tract's area. The other, for medians and per-person
#' values, is the weighting of tracts by area.
#'
#' @references U.S. Census Bureau (2020). *Understanding and Using American
#'   Community Survey Data: What All Data Users Need to Know*. Chapter 8,
#'   Calculating Measures of Error for Derived Estimates.
#'
#' @section MOE formula families:
#' By default, each row gets the formula for the kind of estimate in its
#' `estimand_family` column. The four formulas are called Families A, B, C1,
#' and C2; the letters also appear in warnings and in the row counts of the
#' `cacs_moe_provenance` attribute. The formulas give the margin of error at
#' the 90 percent level, the level of the tract margins of error \eqn{M_i};
#' at another `level`, the result is multiplied by \eqn{z / 1.645}, where
#' \eqn{z} is the normal quantile for the level,
#' `qnorm(1 - (1 - level) / 2)`.
#'
#' - Family A, recorded as `"weighted_sum"`, is used for counts
#'   (`"spatial_total"`). The margin of error is
#'   \eqn{\sqrt{\sum_i (w_i M_i)^2}}{sqrt(sum((w_i * M_i)^2))}, where
#'   \eqn{w_i} is the coverage weight of tract \eqn{i}, the share of its area
#'   inside the drive-time area. This is the handbook's formula for a sum,
#'   applied to the weighted tract estimates.
#' - Family B, recorded as `"weighted_mean"`, is used for medians and
#'   per-person values (`"median_proxy"` and `"area_weighted_scalar_proxy"`).
#'   It is the same formula, with \eqn{w_i} proportional to the area that
#'   tract \eqn{i} shares with the drive-time area and summing to one. This is
#'   an approximation made by the package rather than one given in the
#'   handbook.
#' - Family C1, recorded as `"proportion_subset"`, is the handbook's formula
#'   for a proportion, a ratio whose numerator is part of its denominator. For
#'   the rate \eqn{\hat p = \hat X_{\mathrm{num}} /
#'   \hat X_{\mathrm{den}}}{p = X_num / X_den} of two weighted counts with
#'   Family A margins of error \eqn{M_{\mathrm{num}}}{M_num} and
#'   \eqn{M_{\mathrm{den}}}{M_den}, the margin of error is
#'   \eqn{\sqrt{M_{\mathrm{num}}^2 - \hat p^2 M_{\mathrm{den}}^2} /
#'   \hat X_{\mathrm{den}}}{sqrt(M_num^2 - p^2 * M_den^2) / X_den}.
#' - Family C2, recorded as `"general_ratio_conservative"`, is the handbook's
#'   formula for a ratio whose numerator is not part of its denominator:
#'   \eqn{\sqrt{M_{\mathrm{num}}^2 + \hat p^2 M_{\mathrm{den}}^2} /
#'   \left| \hat X_{\mathrm{den}} \right|}{sqrt(M_num^2 + p^2 * M_den^2) /
#'   abs(X_den)}. The package divides by the absolute value of the
#'   denominator; for the five rates, whose denominators are counts, that is
#'   the denominator itself. Like the other formulas it leaves out the
#'   correlation between tract estimates (see Details).
#'
#' C1 and C2 are used for the rows of the five rates in
#' [`cacs_acs_default_rates`] (`"derived_rate"`). By default every rate uses
#' C2, the ratio formula. The numerator of each of the five rates is part of
#' its denominator, so the rates are proportions, for which the handbook
#' gives C1, the proportion formula. The two formulas differ by
#' \eqn{2 \hat p^2 M_{\mathrm{den}}^2}{2 * p^2 * M_den^2} under the square
#' root, so C2 never gives the narrower margin; how much wider it is depends
#' on \eqn{\hat p M_{\mathrm{den}}}{p * M_den} relative to
#' \eqn{M_{\mathrm{num}}}{M_num} and varies from rate to rate.
#'
#' If the value under the square root of C1 is negative, the row uses C2
#' instead, as the handbook advises (U.S. Census Bureau 2020, chapter 8).
#' The substitution is recorded in the row: `"general_ratio_conservative"`
#' in `moe_formula_effective`, `TRUE` in `moe_fallback`, and
#' `"negative_variance"` in `moe_fallback_reason`. How rates that cannot be
#' computed are marked is described in the "Rates that are NA" section of
#' [cacs_derive_rates()].
#'
#' The output of [cacs_intersect_weight()] has no rate rows, so this function
#' applies only Families A and B; [cacs_derive_rates()] applies C1 and C2 to
#' the rates, as chosen by its `formula_dispatch` argument, at the level set
#' here.
#'
#' @param data A tibble returned by [cacs_intersect_weight()], from which
#'   rows may have been removed (for example with `[` or `dplyr::filter()`)
#'   but whose columns and attributes are kept, including
#'   `cacs_aggregation_carriers` (a table of the weighted totals and means
#'   with their variances, which this function reads). Rows for ACS codes
#'   that [cacs_intersect_weight()] does not combine (`estimand_family` is
#'   `"metadata_only"`), such as codes of tables whose names begin with `C`
#'   or `S`, make the function stop with an error; it runs once those rows
#'   are removed. The row kept for a site and drive-time pair with no tracts
#'   is returned unchanged.
#' @param formula A string, a list of strings named by values of `variable`,
#'   or `NULL` (the default), giving the formula for each row:
#'   `"weighted_sum"`, `"weighted_mean"`, `"proportion_subset"`, or
#'   `"general_ratio_conservative"` (see the "MOE formula families" section).
#'   `NULL` gives each row the formula for its kind of estimate, and a string
#'   is used for every row. In a list, the rows not named keep the formula
#'   they would get with `NULL`. Each row accepts only the formula for its
#'   kind of estimate; any other formula, a name not found in `variable`, or
#'   an unknown formula gives an error.
#' @param level A single number between 0 and 1 (not a percentage) giving
#'   the confidence level of the margins of error. The default is `0.9`.
#'   Any other value, such as `0`, `1`, `90`, `NA`, `"0.9"`, or
#'   `c(0.9, 0.95)`, gives an error.
#' @param verbose A logical value. With `TRUE` (the default), the function
#'   shows a one-line summary when the step finishes (see the "Progress
#'   messages" section of [cacs_run()]).
#' @param ... Not used. The name `fallback_chain_max` is accepted with a
#'   warning and has no effect; any other argument gives an error.
#' @return The tibble `data` with the same rows in the same order, in which
#'   `moe` (the margin of error) is recomputed at `level`. The columns that
#'   record how it was computed are rewritten: `moe_formula_requested` and
#'   `moe_formula_effective` (the formula chosen and the one used), and
#'   `moe_fallback` and `moe_fallback_reason` (whether the chosen formula
#'   could not be used, and why). Count, median, and per-person rows keep
#'   `moe_fallback = FALSE` even when their margin of error is `NA`. Four
#'   columns are added from the `cacs_aggregation_carriers` attribute:
#'   `est_total` and `var_total_raw`, the weighted sum of the tract estimates
#'   and its variance (used for counts), and `est_mean` and `var_mean_raw`,
#'   the weighted average and its variance (used for medians and per-person
#'   values). With `options(cacs.return_se = TRUE)`, an `se` column is added
#'   too: the standard error, `moe` divided by 1.645 at the 90 percent level
#'   or by the normal quantile for another `level`.
#'
#'   The attributes of `data` are kept, and two are added.
#'   `cacs_confidence_level` is the value of `level`, which
#'   [cacs_derive_rates()] uses for the rates. `cacs_moe_provenance` is a
#'   list recording the confidence level, the `formula` argument, and the
#'   numbers of rows computed with Families A and B (see the "MOE formula
#'   families" section). Its counts for rates are always 0, because the
#'   rates are added later by [cacs_derive_rates()], which counts them in its
#'   `cacs_rate_provenance` attribute.
#' @seealso [cacs_se_to_moe()] and [cacs_moe_to_se()] convert between
#'   margins of error and standard errors. The mathematics behind these
#'   formulas is in
#'   `vignette("theory-moe-propagation", package = "catchmentACS")`
#'   (<https://joonho112.github.io/catchmentACS/articles/theory-moe-propagation.html>).
#' @family steps of the calculation
#' @export
#' @examples
#' # Turn the cache off while this example runs (see ?cacs_set_cache).
#' old <- options(catchmentACS.cache_enabled = FALSE)
#'
#' # Example data bundled with the package: the drive-time areas are circles
#' # with a radius of 1 km per minute, and the ACS data are made up.
#' library(sf)
#' iso <- readRDS(system.file("extdata", "legacy_2025_isochrones.rds",
#'                            package = "catchmentACS"))
#' acs <- readRDS(system.file("extdata", "sample_alabama_subset.rds",
#'                            package = "catchmentACS"))
#' iso_07 <- iso[iso$site_id == "AL_SITE_07" & iso$drive_time_min == 10, ]
#' agg <- cacs_intersect_weight(iso_sf = iso_07, acs_sf = acs,
#'                              verbose = FALSE)
#'
#' # Margins of error at the default 90 percent level and at 95 percent
#' prop <- cacs_propagate_moe(agg, verbose = FALSE)
#' prop_95 <- cacs_propagate_moe(agg, level = 0.95, verbose = FALSE)
#' tibble::tibble(variable = prop$variable, estimate = prop$estimate,
#'                moe_90 = prop$moe, moe_95 = prop_95$moe,
#'                formula = prop$moe_formula_effective)
#'
#' options(old)
cacs_propagate_moe <- function(data, formula = NULL, level = 0.90,
                               verbose = TRUE, ...) {

  dots <- list(...)
  if (length(dots) > 0L) {
    dot_names <- names(dots)
    if (is.null(dot_names) || any(!nzchar(dot_names))) {
      .cli_abort_schema(c(
        "{.arg ...} entries for {.fn cacs_propagate_moe} must be named.",
        "i" = "The only name accepted in {.arg ...} is {.val fallback_chain_max}."
      ))
    }
    allowed_dots <- "fallback_chain_max"
    unknown_dots <- setdiff(dot_names, allowed_dots)
    if (length(unknown_dots) > 0L) {
      .cli_abort_schema(c(
        "{.fn cacs_propagate_moe} received unknown {.arg ...} argument{?s}: {.arg {unknown_dots}}.",
        "i" = "The only name accepted in {.arg ...} is {.val {allowed_dots}}."
      ))
    }
    used_reserved <- intersect(dot_names, allowed_dots)
    if (length(used_reserved) > 0L) {
      .cli_warn_provenance(
        c(
          "{.fn cacs_propagate_moe} ignored {.arg {used_reserved}}.",
          "i" = "{.arg fallback_chain_max} is accepted but has no effect in this version of catchmentACS."
        ),
        phase = "moe"
      )
    }
  }

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    .cli_abort_schema(c(
      "{.arg verbose} must be a non-NA logical scalar.",
      "x" = "Got {.cls {class(verbose)[[1L]]}} of length {.val {length(verbose)}}."
    ))
  }

  if (!inherits(data, c("tbl_df", "data.table"))) {
    .cli_abort_schema(c(
      "{.arg data} must be a {.cls tbl_df} or {.cls data.table}.",
      "x" = "Got {.cls {class(data)[[1]]}}.",
      "i" = "Did you call {.fn cacs_intersect_weight}?"
    ))
  }

  # These columns are checked first so that a missing one gives an error
  # that names the data argument; .validate_intersect_output() below would
  # describe it as a column missing from the output of
  # cacs_intersect_weight().
  critical <- c("site_id", "drive_time_min", "variable",
                "estimand_family", "estimate", "moe",
                "weight_basis", "moe_formula_effective",
                "moe_fallback", "moe_fallback_reason")
  missing_cols <- setdiff(critical, names(data))
  if (length(missing_cols) > 0L) {
    .cli_abort_schema(c(
      "{.arg data} missing critical column{?s}: {.field {missing_cols}}.",
      "i" = "Pass the result of {.fn cacs_intersect_weight}."
    ))
  }

  # The cacs_aggregation_carriers attribute is required unless no row has a
  # kind of estimate other than "metadata_only". .validate_intersect_output()
  # then checks the columns in .LONG_REQUIRED_COLS, that attribute, and that
  # cacs_schema_version is "1.0".
  carrier <- attr(data, "cacs_aggregation_carriers")
  non_meta <- any(data$estimand_family != "metadata_only", na.rm = TRUE)
  if (is.null(carrier) && non_meta) {
    .cli_abort_schema(c(
      "{.arg data} missing {.attr cacs_aggregation_carriers}.",
      "i" = "Did you call {.fn cacs_intersect_weight}?"
    ))
  }
  .validate_intersect_output(data, abort = TRUE,
                              require_carrier = non_meta)

  moe_prog <- .cacs_progress_reporter(n = 1L, label = "MOE propagation",
                                      verbose = verbose)
  moe_done <- FALSE
  on.exit(
    moe_prog$finish(
      n_success = as.integer(moe_done),
      n_failed  = as.integer(!moe_done)
    ),
    add = TRUE
  )

  z <- .resolve_z_from_level(level)

  # The formula for each row. Rows of pairs that failed earlier
  # (failure_origin other than "none") are not checked: their
  # estimand_family is NA, and they are returned unchanged.
  formula_resolved <- .resolve_formula(formula, data)
  ordinary_rows <- if ("failure_origin" %in% names(data)) {
    is.na(data$failure_origin) | data$failure_origin == "none"
  } else {
    rep(TRUE, nrow(data))
  }
  bad_formula <- .check_family_formula_consistency(
    data$estimand_family[ordinary_rows],
    formula_resolved[ordinary_rows]
  )
  if (length(bad_formula) > 0L) {
    bad_rows <- which(ordinary_rows)[bad_formula]
    bad_preview <- utils::head(bad_rows, 8L)
    bad_variables <- data$variable[bad_preview]
    bad_families <- data$estimand_family[bad_preview]
    bad_formulas <- formula_resolved[bad_preview]
    .cli_abort_schema(c(
      "{.arg formula} is inconsistent with {.field estimand_family} for {length(bad_rows)} row{?s}.",
      "x" = "Offending variable{?s}: {.field {bad_variables}}.",
      "x" = "Family/formula pair{?s}: {.val {unique(paste0(bad_families, ' -> ', bad_formulas))}}.",
      "i" = "Leave {.arg formula} at {.code NULL} ({.arg moe_formula} in {.fn cacs_run}), or give {.val weighted_sum} for counts and {.val weighted_mean} for medians and per-person values."
    ))
  }

  # Record the chosen formula; the rate branches below change it where C2
  # replaces C1.
  data$moe_formula_effective <- formula_resolved

  # Counts (Family A) and medians and per-person values (Family B): the
  # margin of error is z * sqrt(variance), with the variance of the weighted
  # sum or average joined from cacs_aggregation_carriers by site, drive time,
  # and variable. At level = 0.90 this gives the margins of error of
  # cacs_intersect_weight(). Rate rows get their numerator and denominator
  # from the lookup helpers below.
  data <- .left_join_carriers(data, carrier)

  is_A <- !is.na(formula_resolved) & formula_resolved == "weighted_sum"
  is_B <- !is.na(formula_resolved) & formula_resolved == "weighted_mean"

  if (any(is_A)) {
    data$moe[is_A] <- z * sqrt(data$var_total_raw[is_A])
    data$moe_fallback[is_A] <- FALSE
    data$moe_fallback_reason[is_A] <- "n/a"
  }
  if (any(is_B)) {
    data$moe[is_B] <- z * sqrt(data$var_mean_raw[is_B])
    data$moe_fallback[is_B] <- FALSE
    data$moe_fallback_reason[is_B] <- "n/a"
  }

  # cacs_derive_rates() reads this attribute to compute the rates at the same
  # level. It is set again before the result is returned.
  attr(data, "cacs_confidence_level") <- level

  # Rate rows (estimand_family "derived_rate"). The output of
  # cacs_intersect_weight() has none, so the blocks below run only for rate
  # rows added to data by other means; in the pipeline the rates are
  # computed by cacs_derive_rates(). The variable of a rate row must be one
  # of the five rate names, because the lookup helpers take the numerator
  # and denominator codes from .SANCTIONED_RATES_V1; any other name stops
  # the function here instead of giving NA values.

  is_C1 <- !is.na(formula_resolved) & formula_resolved == "proportion_subset"
  is_C2 <- !is.na(formula_resolved) &
           formula_resolved == "general_ratio_conservative"

  if (any(is_C1) || any(is_C2)) {
    ratio_vars <- unique(data$variable[is_C1 | is_C2])
    bad_vars <- setdiff(ratio_vars, names(.SANCTIONED_RATES_V1))
    if (length(bad_vars) > 0L) {
      .cli_abort_operator(c(
        "Rows with a ratio formula name rate{?s} that {?is/are} not built in: {.field {bad_vars}}.",
        "i" = "Built-in rates: {.val {names(.SANCTIONED_RATES_V1)}}.",
        "x" = "The {.field variable} of each row with a ratio formula must be one of these rates."
      ))
    }
  }

  if (any(is_C1)) {
    idx_C1 <- which(is_C1)
    rate_names_C1 <- data$variable[idx_C1]
    sid_C1 <- data$site_id[idx_C1]
    dt_C1  <- data$drive_time_min[idx_C1]

    # The lookup helpers take one rate name, so they are called row by row.
    num_est_C1 <- vapply(seq_along(idx_C1), function(i) {
      .lookup_carrier_num(rate_names_C1[i], carrier, sid_C1[i], dt_C1[i])
    }, numeric(1L))
    den_est_C1 <- vapply(seq_along(idx_C1), function(i) {
      .lookup_carrier_den(rate_names_C1[i], carrier, sid_C1[i], dt_C1[i])
    }, numeric(1L))
    num_var_C1 <- vapply(seq_along(idx_C1), function(i) {
      .lookup_carrier_num_var(rate_names_C1[i], carrier, sid_C1[i], dt_C1[i])
    }, numeric(1L))
    den_var_C1 <- vapply(seq_along(idx_C1), function(i) {
      .lookup_carrier_den_var(rate_names_C1[i], carrier, sid_C1[i], dt_C1[i])
    }, numeric(1L))

    # .moe_prop_self() takes margins of error, at the level of this call.
    num_moe_C1 <- .var_to_moe(num_var_C1, z)
    den_moe_C1 <- .var_to_moe(den_var_C1, z)

    # The rate is the ratio of the two weighted totals. It is written only
    # where both totals are present and the denominator is not zero; other
    # rate rows keep the estimate they came with.
    has_estimate <- !is.na(num_est_C1) & !is.na(den_est_C1) &
                    !.is_zero_den(den_est_C1)
    if (any(has_estimate)) {
      data$estimate[idx_C1[has_estimate]] <-
        num_est_C1[has_estimate] / den_est_C1[has_estimate]
    }

    # The flags from .moe_prop_self() decide which helper handles each row.
    # The order follows what each case lacks: without both estimates or with
    # a zero denominator there is no rate; without both margins of error
    # there is no margin of error; only with all four can the value under
    # the square root be negative.
    probes <- lapply(seq_along(idx_C1), function(i) {
      .moe_prop_self(num_est_C1[i], num_moe_C1[i],
                     den_est_C1[i], den_moe_C1[i], z = z)
    })

    is_zero_den    <- vapply(probes, `[[`, logical(1L), "zero_denominator")
    is_missing_est <- vapply(probes, `[[`, logical(1L), "missing_estimate")
    is_missing_moe <- vapply(probes, `[[`, logical(1L), "missing_moe")
    is_neg_var     <- vapply(probes, `[[`, logical(1L), "inside_negative")

    # Each row goes to one helper only, the first in this order. A missing
    # estimate, which .moe_prop_self() flags separately, is recorded here as
    # "zero_denominator" too; cacs_derive_rates() gives such a rate
    # failure_origin = "carrier" instead.
    local_zero    <- which(is_zero_den | is_missing_est)
    local_missing <- setdiff(which(is_missing_moe), local_zero)
    local_neg     <- setdiff(which(is_neg_var),
                              c(local_zero, local_missing))
    local_success <- setdiff(seq_along(idx_C1),
                              c(local_zero, local_missing, local_neg))

    data <- .apply_zero_den(data,    idx_C1[local_zero],    is_C1 = TRUE)
    data <- .apply_missing_moe(data, idx_C1[local_missing], is_C1 = TRUE)
    data <- .apply_c2_fallback(
      data, idx_C1[local_neg],
      num_est = num_est_C1[local_neg],
      num_moe = num_moe_C1[local_neg],
      den_est = den_est_C1[local_neg],
      den_moe = den_moe_C1[local_neg],
      z       = z,
      is_C1   = TRUE
    )
    data <- .apply_c1_success(
      data, idx_C1[local_success],
      num_est = num_est_C1[local_success],
      num_moe = num_moe_C1[local_success],
      den_est = den_est_C1[local_success],
      den_moe = den_moe_C1[local_success],
      z       = z,
      is_C1   = TRUE
    )
  }

  # Rate rows with the ratio formula (C2) chosen. The value under its square
  # root is a sum of two non-negative terms and cannot be negative, so there
  # is no negative-variance branch: only a zero denominator (or a missing
  # estimate) and a missing margin of error give NA. The other rows get the
  # value of .moe_ratio_self() with moe_fallback = FALSE.
  if (any(is_C2)) {
    idx_C2 <- which(is_C2)
    rate_names_C2 <- data$variable[idx_C2]
    sid_C2 <- data$site_id[idx_C2]
    dt_C2  <- data$drive_time_min[idx_C2]

    num_est_C2 <- vapply(seq_along(idx_C2), function(i) {
      .lookup_carrier_num(rate_names_C2[i], carrier, sid_C2[i], dt_C2[i])
    }, numeric(1L))
    den_est_C2 <- vapply(seq_along(idx_C2), function(i) {
      .lookup_carrier_den(rate_names_C2[i], carrier, sid_C2[i], dt_C2[i])
    }, numeric(1L))
    num_var_C2 <- vapply(seq_along(idx_C2), function(i) {
      .lookup_carrier_num_var(rate_names_C2[i], carrier, sid_C2[i], dt_C2[i])
    }, numeric(1L))
    den_var_C2 <- vapply(seq_along(idx_C2), function(i) {
      .lookup_carrier_den_var(rate_names_C2[i], carrier, sid_C2[i], dt_C2[i])
    }, numeric(1L))

    num_moe_C2 <- .var_to_moe(num_var_C2, z)
    den_moe_C2 <- .var_to_moe(den_var_C2, z)

    has_estimate_C2 <- !is.na(num_est_C2) & !is.na(den_est_C2) &
                        !.is_zero_den(den_est_C2)
    if (any(has_estimate_C2)) {
      data$estimate[idx_C2[has_estimate_C2]] <-
        num_est_C2[has_estimate_C2] / den_est_C2[has_estimate_C2]
    }

    # The same order as for C1, without the negative-variance step. These
    # checks also treat an infinite denominator as zero; the C1 checks do not.
    local_zero_C2 <- which(is.na(num_est_C2) | is.na(den_est_C2) |
                            !is.finite(den_est_C2) |
                            .is_zero_den(den_est_C2))
    local_missing_C2 <- setdiff(
      which(is.na(num_moe_C2) | is.na(den_moe_C2)),
      local_zero_C2
    )
    local_success_C2 <- setdiff(seq_along(idx_C2),
                                 c(local_zero_C2, local_missing_C2))

    data <- .apply_zero_den(data,    idx_C2[local_zero_C2],    is_C1 = FALSE)
    data <- .apply_missing_moe(data, idx_C2[local_missing_C2], is_C1 = FALSE)

    # C2 is the formula chosen for these rows, so moe_fallback stays FALSE.
    # .apply_c2_fallback() is not used here because it sets it to TRUE: it is
    # for C1 rows that had to use C2.
    if (length(local_success_C2) > 0L) {
      i_ok <- idx_C2[local_success_C2]
      c2_vals <- vapply(local_success_C2, function(i) {
        .moe_ratio_self(num_est_C2[i], num_moe_C2[i],
                        den_est_C2[i], den_moe_C2[i], z = z)
      }, numeric(1L))
      data$moe[i_ok] <- c2_vals
      data$moe_formula_effective[i_ok] <- "general_ratio_conservative"
      data$moe_fallback[i_ok] <- FALSE
      data$moe_fallback_reason[i_ok] <- "n/a"
    }
  }

  # "metadata_only" rows get no margin of error and no formula. Their
  # failure_origin is "none", so the output check below stops with an error
  # when data has such a row (moe_formula_effective is NA on it); the
  # function returns a result only for data without them.
  is_meta <- !is.na(data$estimand_family) &
             data$estimand_family == "metadata_only"
  if (any(is_meta)) {
    data$moe[is_meta] <- NA_real_
    data$moe_formula_effective[is_meta] <- NA_character_
    data$moe_fallback[is_meta] <- NA
    data$moe_fallback_reason[is_meta] <- "n/a"
  }

  # moe_formula_requested keeps the formula chosen for each row (from formula
  # or the default), also where moe_formula_effective records C2 in place of
  # C1.
  data$moe_formula_requested <- formula_resolved

  # With options(cacs.return_se = TRUE), an se column (moe / z) is added. It
  # is an option rather than an argument so that it can be set once for the
  # session and also applies when cacs_run() calls this function.
  if (isTRUE(getOption("cacs.return_se", FALSE))) {
    data$se <- data$moe / z
  }

  # One warning gives the number of rows for each fallback reason, and none
  # is given when no row has a reason. Only rate rows can have one, so a
  # call on the output of cacs_intersect_weight() gives no warning. The phase
  # value is shown in the phase column of cacs_capture_conditions();
  # cacs_run() records the warning under its propagate_moe step either way.
  n_c1_to_c2 <- sum(data$moe_fallback_reason == "negative_variance",
                    na.rm = TRUE)
  n_zero_den <- sum(data$moe_fallback_reason == "zero_denominator",
                    na.rm = TRUE)
  n_missing  <- sum(data$moe_fallback_reason == "missing_moe",
                    na.rm = TRUE)
  if ((n_c1_to_c2 + n_zero_den + n_missing) > 0L) {
    msg <- "Some rate rows have no margin of error or use a replacement formula:"
    if (n_c1_to_c2 > 0L) {
      msg <- c(msg, "*" = "{.code moe_fallback_reason = \"negative_variance\"}: {n_c1_to_c2} row{?s} used the ratio formula (C2), because the proportion formula (C1) gave a negative variance.")
    }
    if (n_zero_den > 0L) {
      msg <- c(msg, "*" = "{.code moe_fallback_reason = \"zero_denominator\"}: {n_zero_den} row{?s} without a margin of error, because the denominator is zero.")
    }
    if (n_missing > 0L) {
      msg <- c(msg, "*" = "{.code moe_fallback_reason = \"missing_moe\"}: {n_missing} row{?s} without a margin of error, because a margin of error of the numerator or denominator is missing.")
    }
    msg <- c(msg, "i" = "The {.field moe_fallback_reason} column gives the reason on each row.")
    .cli_warn_runtime(msg, phase = "moe")
  }

  # Check the result: the checks made on the input, the se column when the
  # option asks for it, and that moe_formula_effective is not NA on rows
  # whose failure_origin is "none". Nothing gives "metadata_only" rows a
  # formula, so such a row stops the function here (see above). A failed
  # check is a catchmentACS_error_schema error.
  .validate_propagate_output(data, abort = TRUE)

  # The attributes of data, including cacs_aggregation_carriers and
  # cacs_aggregation_provenance, pass through the join and the column
  # assignments unchanged. The input check required cacs_schema_version
  # "1.0", so the next line keeps that value, and cacs_confidence_level gets
  # the value set above again.
  attr(data, "cacs_schema_version") <- "1.0"

  attr(data, "cacs_confidence_level") <- level

  # cacs_moe_provenance (12 fields): the level and z value, the formula
  # argument, row counts for each formula, for C1 rows computed with C2, and
  # for rows left NA by reason, the time, and the package version. The
  # counts for rates are 0 when data comes from cacs_intersect_weight(),
  # which has no rate rows.
  n_rows_family_a       <- sum(formula_resolved == "weighted_sum",
                               na.rm = TRUE)
  n_rows_family_b       <- sum(formula_resolved == "weighted_mean",
                               na.rm = TRUE)
  n_rows_c1_success     <- sum(
    formula_resolved == "proportion_subset" &
      data$moe_fallback_reason == "n/a",
    na.rm = TRUE
  )
  n_rows_c2_success     <- sum(
    formula_resolved == "general_ratio_conservative" &
      data$moe_fallback_reason == "n/a",
    na.rm = TRUE
  )
  n_rows_fallback_c1_c2 <- n_c1_to_c2
  n_rows_na_zero_den    <- n_zero_den
  n_rows_na_missing_moe <- n_missing
  attr(data, "cacs_moe_provenance") <- list(
    level                       = level,
    z                           = z,
    formula_argument            = formula,
    n_rows_family_a             = as.integer(n_rows_family_a),
    n_rows_family_b             = as.integer(n_rows_family_b),
    n_rows_family_c1_success    = as.integer(n_rows_c1_success),
    n_rows_family_c2_success    = as.integer(n_rows_c2_success),
    n_rows_fallback_c1_to_c2    = as.integer(n_rows_fallback_c1_c2),
    n_rows_na_zero_den          = as.integer(n_rows_na_zero_den),
    n_rows_na_missing_moe       = as.integer(n_rows_na_missing_moe),
    generated_at                = format(Sys.time(),
                                          "%Y-%m-%dT%H:%M:%S%z"),
    cacs_ver                    = as.character(
      utils::packageVersion("catchmentACS")
    )
  )

  moe_done <- TRUE
  data
}
