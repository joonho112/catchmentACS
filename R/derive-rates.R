# derive-rates.R - cacs_derive_rates(), which adds a row for each of the five
# rates (poverty_rate, snap_rate, ssi_rate, unemp_rate,
# labor_force_participation) to the table from cacs_propagate_moe(), and the
# exported object cacs_acs_default_rates that holds their ACS codes.
#
# A rate is a ratio of two weighted counts, and both counts and their
# variances come from the `cacs_aggregation_carriers` attribute that
# cacs_intersect_weight() attaches rather than from the rows of the input
# table. Removing the rows of a rate's ACS codes does not change the rate.
# The margins of error use the confidence level that cacs_propagate_moe()
# records in `cacs_confidence_level`, and the 90 percent level when the input
# has no such attribute.

#' @importFrom stats setNames qnorm
#' @keywords internal
#' @noRd
NULL


# cacs_acs_default_rates is an exported copy of the internal
# .SANCTIONED_RATES_V1 (R/aaa-globals.R), so that the `rates` default of
# cacs_derive_rates() and cacs_run() does not name an internal object.

#' Numerator and denominator ACS codes of the five built-in rates
#'
#' The numerator and denominator codes of each of the five rates that
#' [cacs_derive_rates()] and [cacs_run()] compute. The codes are American
#' Community Survey (ACS) variable codes. The list is the default, and the
#' only accepted value, of the `rates` argument of both functions.
#'
#' The five rates are:
#' \describe{
#'   \item{`poverty_rate`}{People whose income in the past 12 months was
#'     below the poverty level (`B17001_002`) divided by people for whom
#'     poverty status is determined (`B17001_001`).}
#'   \item{`snap_rate`}{Households that received Food Stamps or the
#'     Supplemental Nutrition Assistance Program (SNAP) in the past 12
#'     months (`B22003_002`) divided by all households (`B22003_001`).}
#'   \item{`ssi_rate`}{Households with Supplemental Security Income (SSI)
#'     in the past 12 months (`B19056_002`) divided by all households
#'     (`B19056_001`).}
#'   \item{`unemp_rate`}{Unemployed people in the civilian labor force
#'     (`B23025_005`) divided by the civilian labor force (`B23025_003`),
#'     among people 16 years and over.}
#'   \item{`labor_force_participation`}{People in the labor force, including
#'     the armed forces (`B23025_002`), divided by people 16 years and over
#'     (`B23025_001`).}
#' }
#'
#' @format A named list of five character vectors, one for each rate. Each
#'   vector has the elements `num` and `den`, the ACS codes of the numerator
#'   and the denominator.
#' @docType data
#' @seealso [cacs_derive_rates()] computes the rates and their margins of
#'   error, and `vignette("theory-derived-rates", package = "catchmentACS")`
#'   (<https://joonho112.github.io/catchmentACS/articles/theory-derived-rates.html>)
#'   explains the calculations.
#' @family rates and margins of error
#' @export
#' @examples
#' names(cacs_acs_default_rates)
#'
#' # The codes of the numerator and the denominator of the poverty rate
#' cacs_acs_default_rates$poverty_rate
cacs_acs_default_rates <- .SANCTIONED_RATES_V1


# Not used. cacs_derive_rates() checks `cacs_aggregation_carriers` with
# .validate_carrier_tbl(), which requires the ten columns named in
# .CARRIER_REQUIRED_COLS (R/checks.R).

.DERIVE_RATES_REQUIRED_CARRIER_COLS <- c(
  "site_id", "drive_time_min", "variable",
  "est_total", "var_total_raw", "weight_sum"
)


# One row: the rate for one site and drive time, and its margin of error.
#
# The four lookups below read the weighted numerator and denominator counts
# and their variances from `carriers`. They find them by the ACS codes of the
# rate rather than by the variable of a row, so the only way a rate goes
# missing is that one of its two codes has no row in `carriers` for the pair.
# Those rows come back from .empty_rate_row() with failure_origin =
# "carrier": no formula was applied to them, so they are not counted as a
# substituted formula.

.compute_one_rate <- function(template_row, sid, dt, rate_nm,
                              num_code, den_code, formula,
                              carriers, z) {

  num_est <- .lookup_carrier_num(rate_nm, carriers, sid, dt)
  den_est <- .lookup_carrier_den(rate_nm, carriers, sid, dt)
  num_var <- .lookup_carrier_num_var(rate_nm, carriers, sid, dt)
  den_var <- .lookup_carrier_den_var(rate_nm, carriers, sid, dt)

  # The numerator and the denominator can cover different tracts, so the sums
  # of the coverage weights are read for each of the two codes separately.
  ws_lookup <- function(code) {
    .carrier_key_lookup(rate_nm, carriers, sid, dt,
                        code_role = if (identical(code, num_code)) "num"
                                    else "den",
                        value_col = "weight_sum")
  }
  num_ws <- ws_lookup(num_code)
  den_ws <- ws_lookup(den_code)

  # How many tracts each of the two counts is made of. The two numbers differ
  # when a tract in the area has a row for one of the codes and not for the
  # other; the rate is then a ratio of totals over different tracts.
  n_tracts_num_val <- .lookup_carrier_n_tracts_num(rate_nm, carriers, sid, dt)
  n_tracts_den_val <- .lookup_carrier_n_tracts_den(rate_nm, carriers, sid, dt)

  if (is.na(num_est) || is.na(den_est) ||
      is.na(num_var) || is.na(den_var)) {
    return(.empty_rate_row(template_row, sid, dt, rate_nm, formula,
                            failure_origin = "carrier"))
  }

  # `carriers` holds variances; both formulas take margins of error.
  num_moe <- if (is.na(num_var) || num_var < 0) NA_real_
             else z * sqrt(num_var)
  den_moe <- if (is.na(den_var) || den_var < 0) NA_real_
             else z * sqrt(den_var)

  if (identical(formula, "proportion_subset")) {
    probe <- .moe_prop_self(num_est, num_moe, den_est, den_moe, z = z)
    # .moe_prop_self() reports why it could not use the proportion formula;
    # the three reasons become the value of moe_fallback_reason.
    if (isTRUE(probe$zero_denominator) || isTRUE(probe$missing_estimate)) {
      result <- list(moe = NA_real_,
                     formula_effective = "proportion_subset",
                     fallback = TRUE,
                     fallback_reason = "zero_denominator")
    } else if (isTRUE(probe$missing_moe)) {
      result <- list(moe = NA_real_,
                     formula_effective = "proportion_subset",
                     fallback = TRUE,
                     fallback_reason = "missing_moe")
    } else if (isTRUE(probe$inside_negative)) {
      # The value under the square root of the proportion formula is
      # negative, so this row uses the ratio formula instead.
      moe_c2 <- .moe_ratio_self(num_est, num_moe, den_est, den_moe, z = z)
      result <- list(moe = moe_c2,
                     formula_effective = "general_ratio_conservative",
                     fallback = TRUE,
                     fallback_reason = "negative_variance")
    } else {
      result <- list(moe = probe$value,
                     formula_effective = "proportion_subset",
                     fallback = FALSE,
                     fallback_reason = "n/a")
    }
  } else {  # "general_ratio_conservative"
    # Nothing under the square root of the ratio formula can be negative, so
    # this branch has no substitution of its own. `moe_fallback` records that
    # the formula chosen for the rate could not be used, not that the ratio
    # formula was used, so it stays FALSE on the rows this branch computes; a
    # zero denominator or a missing margin of error still sets it to TRUE and
    # writes "zero_denominator" or "missing_moe" in `moe_fallback_reason`.
    if (!is.finite(den_est) || .is_zero_den(den_est)) {
      result <- list(moe = NA_real_,
                     formula_effective = "general_ratio_conservative",
                     fallback = TRUE,
                     fallback_reason = "zero_denominator")
    } else if (is.na(num_moe) || is.na(den_moe)) {
      result <- list(moe = NA_real_,
                     formula_effective = "general_ratio_conservative",
                     fallback = TRUE,
                     fallback_reason = "missing_moe")
    } else {
      moe_value <- .moe_ratio_self(num_est, num_moe, den_est, den_moe,
                                    z = z)
      result <- list(moe = moe_value,
                     formula_effective = "general_ratio_conservative",
                     fallback = FALSE,
                     fallback_reason = "n/a")
    }
  }

  rate_estimate <- if (!is.finite(den_est) || .is_zero_den(den_est)) {
    NA_real_
  } else {
    num_est / den_est
  }

  # weight_sum on a rate row is the smaller of the two sums, so that it does
  # not overstate how much of the area either count covers.
  ws_vals <- c(num_ws, den_ws)
  rate_weight_sum <- if (all(is.na(ws_vals))) {
    NA_real_
  } else {
    min(ws_vals, na.rm = TRUE)
  }

  # The provider, profile, OSM snapshot date, ACS year and weighting of the
  # row are copied from template_row, a row of the input for the same pair.
  .build_rate_row(
    template_row     = template_row,
    sid              = sid,
    dt               = dt,
    rate_nm          = rate_nm,
    estimate         = rate_estimate,
    moe              = result$moe,
    weight_sum       = rate_weight_sum,
    formula_req      = formula,
    formula_eff      = result$formula_effective,
    fallback         = result$fallback,
    fallback_reason  = result$fallback_reason,
    failure_origin   = "none",
    n_tracts_num_val = n_tracts_num_val,
    n_tracts_den_val = n_tracts_den_val
  )
}


# The row for a rate when one of its two ACS codes has no row in
# `cacs_aggregation_carriers` for the pair. The rate, its margin of error,
# `weight_sum` and the two tract counts are NA and `failure_origin` is
# "carrier". `moe_fallback` stays FALSE and `moe_fallback_reason` "n/a"
# because no formula was applied, and `moe_formula_effective` repeats the
# formula chosen for the rate.

.empty_rate_row <- function(template_row, sid, dt, rate_nm, formula,
                            failure_origin = "carrier") {
  .build_rate_row(
    template_row     = template_row,
    sid              = sid,
    dt               = dt,
    rate_nm          = rate_nm,
    estimate         = NA_real_,
    moe              = NA_real_,
    weight_sum       = NA_real_,
    formula_req      = formula,
    formula_eff      = formula,  # requested but never applied
    fallback         = FALSE,
    fallback_reason  = "n/a",
    failure_origin   = failure_origin,
    n_tracts_num_val = NA_integer_,
    n_tracts_den_val = NA_integer_
  )
}


# Builds one rate row with the 23 columns that every row of the result must
# have (.LONG_REQUIRED_COLS in R/checks.R). Columns that the input has beyond
# those, such as the est_total and var_total_raw columns that
# cacs_propagate_moe() adds, are filled with NA when dplyr::bind_rows() puts
# the rate rows under the input rows.
#
# On a rate row, n_tracts is NA and the numbers of tracts behind the two
# counts are in n_tracts_num and n_tracts_den, so is.na(n_tracts) picks out
# the rate rows. weight_uncertainty_propagated is FALSE because the coverage
# weights are treated as fixed numbers throughout the package.

.build_rate_row <- function(template_row, sid, dt, rate_nm,
                            estimate, moe, weight_sum,
                            formula_req, formula_eff,
                            fallback, fallback_reason,
                            failure_origin,
                            n_tracts_num_val = NA_integer_,
                            n_tracts_den_val = NA_integer_) {

  # template_row is NULL only for a pair that has no row in the input, which
  # the loop in cacs_derive_rates() does not produce; the defaults then stand
  # in for the columns it would supply.
  tpl_get <- function(col, default) {
    if (is.null(template_row) || !col %in% names(template_row) ||
        length(template_row[[col]]) == 0L) {
      return(default)
    }
    template_row[[col]][[1L]]
  }

  # The two counts have to be integers in the result. The lookup hands back
  # the n_tracts values of `cacs_aggregation_carriers`, which are integers
  # there, so this only guards against a table that stores them as doubles.
  n_tracts_num_int <- if (is.na(n_tracts_num_val)) NA_integer_
                      else as.integer(n_tracts_num_val)
  n_tracts_den_int <- if (is.na(n_tracts_den_val)) NA_integer_
                      else as.integer(n_tracts_den_val)

  tibble::tibble(
    site_id                       = sid,
    drive_time_min                = as.integer(dt),
    ring_topology                 = tpl_get("ring_topology", NA_character_),
    variable                      = rate_nm,
    estimate                      = estimate,
    moe                           = moe,
    weight_sum                    = weight_sum,
    n_tracts                      = NA_integer_,
    n_tracts_num                  = n_tracts_num_int,
    n_tracts_den                  = n_tracts_den_int,
    provider                      = tpl_get("provider", NA_character_),
    profile                       = tpl_get("profile", NA_character_),
    osm_snapshot_date             = tpl_get("osm_snapshot_date",
                                            NA_character_),
    acs_year                      = tpl_get("acs_year", NA_integer_),
    weight_method                 = tpl_get("weight_method", "area"),
    estimand_family               = "derived_rate",
    weight_basis                  = "coverage",
    moe_formula_requested         = formula_req,
    moe_formula_effective         = formula_eff,
    moe_fallback                  = fallback,
    moe_fallback_reason           = fallback_reason,
    failure_origin                = failure_origin,
    weight_uncertainty_propagated = FALSE
  )
}


# The range check behind options(catchmentACS.audit_rates = TRUE). The ranges
# are rough plausibility bounds, not values a rate has to lie between, so the
# check is off by default, only warns, and leaves the rates as they are.

.rate_formula_audit_inline <- function(out, rates,
                                       debug = getOption(
                                         "catchmentACS.audit_rates", FALSE
                                       ),
                                       bounds = .SANCTIONED_RATE_BOUNDS_V1) {
  audit <- list(
    enabled = isTRUE(debug),
    n_out_of_bounds = 0L,
    rows = tibble::tibble(
      site_id = character(),
      drive_time_min = integer(),
      variable = character(),
      estimate = numeric(),
      min = numeric(),
      max = numeric()
    )
  )

  if (!isTRUE(debug)) {
    attr(out, "cacs_rate_audit") <- audit
    return(out)
  }

  if (!all(c("site_id", "drive_time_min", "variable", "estimate") %in%
           names(out))) {
    attr(out, "cacs_rate_audit") <- audit
    return(out)
  }

  rate_names <- intersect(names(rates), names(bounds))
  issue_rows <- list()

  for (rate_nm in rate_names) {
    bound <- bounds[[rate_nm]]
    vals <- out[out$variable == rate_nm, , drop = FALSE]
    if (nrow(vals) == 0L) next
    bad <- is.finite(vals$estimate) &
      (vals$estimate < bound[["min"]] | vals$estimate > bound[["max"]])
    if (!any(bad, na.rm = TRUE)) next

    bad_rows <- vals[bad, c("site_id", "drive_time_min", "variable",
                            "estimate"), drop = FALSE]
    bad_rows$min <- unname(bound[["min"]])
    bad_rows$max <- unname(bound[["max"]])
    issue_rows[[rate_nm]] <- bad_rows

    observed <- range(bad_rows$estimate, na.rm = TRUE)
    .cli_warn_rate_out_of_range(
      c(
        "Rate audit found out-of-range {.field {rate_nm}} value{?s}.",
        "x" = "{.val {nrow(bad_rows)}} row{?s} outside [{bound[['min']]}, {bound[['max']]}].",
        "i" = "Observed range: [{round(observed[[1L]], 6)}, {round(observed[[2L]], 6)}].",
        "i" = "Turn this check off with {.code options(catchmentACS.audit_rates = FALSE)}."
      ),
      phase = "rates"
    )
  }

  if (length(issue_rows) > 0L) {
    audit$rows <- dplyr::bind_rows(issue_rows)
    audit$n_out_of_bounds <- as.integer(nrow(audit$rows))
  }
  attr(out, "cacs_rate_audit") <- audit
  out
}


# One warning at the end of cacs_derive_rates() that counts the rate rows
# with no margin of error and the rows where the ratio formula replaced the
# proportion formula. When every counted row is one whose counts are missing,
# the warning also has the class catchmentACS_warning_carrier_missing, so a
# handler can tell that case from a substituted formula.

.warn_derive_rates_summary <- function(out, rate_names) {
  rate_rows <- out$variable %in% rate_names
  if (!any(rate_rows)) return(invisible(NULL))

  n_c1_to_c2 <- sum(
    rate_rows & out$moe_fallback_reason == "negative_variance",
    na.rm = TRUE
  )
  n_zero_den <- sum(
    rate_rows & out$moe_fallback_reason == "zero_denominator",
    na.rm = TRUE
  )
  n_missing_moe <- sum(
    rate_rows & out$moe_fallback_reason == "missing_moe",
    na.rm = TRUE
  )
  n_carrier <- sum(
    rate_rows & out$failure_origin == "carrier",
    na.rm = TRUE
  )

  if ((n_c1_to_c2 + n_zero_den + n_missing_moe + n_carrier) == 0L) {
    return(invisible(NULL))
  }

  # One line for each reason that some row has.
  msg <- "Some rates are {.code NA} or use a replacement formula:"
  if (n_carrier > 0L) {
    msg <- c(msg, "*" = "{.code failure_origin = \"carrier\"}: {n_carrier} rate row{?s}, {.code NA} because a count or margin of error that the rate needs is missing.")
  }
  if (n_zero_den > 0L) {
    msg <- c(msg, "*" = "{.code moe_fallback_reason = \"zero_denominator\"}: {n_zero_den} rate row{?s}, {.code NA} because the denominator is zero.")
  }
  if (n_missing_moe > 0L) {
    msg <- c(msg, "*" = "{.code moe_fallback_reason = \"missing_moe\"}: {n_missing_moe} rate row{?s} without a margin of error, because a margin of error of the numerator or denominator is missing.")
  }
  if (n_c1_to_c2 > 0L) {
    msg <- c(msg, "*" = "{.code moe_fallback_reason = \"negative_variance\"}: {n_c1_to_c2} rate row{?s} used the ratio formula (C2), because the proportion formula (C1) gave a negative variance.")
  }
  msg <- c(msg, "i" = "The {.field failure_origin} and {.field moe_fallback_reason} columns give the reason on each rate row.")

  warn_fn <- if (n_carrier > 0L &&
                 (n_c1_to_c2 + n_zero_den + n_missing_moe) == 0L) {
    .cli_warn_carrier_missing
  } else {
    .cli_warn_runtime
  }

  # The warning names the call of cacs_derive_rates(), which called this
  # function, when it was called by name, as in cacs_derive_rates(x). When
  # cacs_run() calls it through do.call(), that call holds the function itself
  # and would print its definition, so the warning then has no call. The
  # caller's environment is passed, not the call: .cacs_emit() gives its
  # arguments to do.call(), which would evaluate a call object.
  caller <- rlang::caller_env()
  caller_call <- sys.call(-1L)
  fn <- if (is.call(caller_call)) caller_call[[1L]] else NULL
  by_name <- is.symbol(fn) ||
    (is.call(fn) && identical(fn[[1L]], as.name("::")))
  warn_fn(msg, phase = "rates", call = if (by_name) caller else NULL)
}


#' Compute rates and their margins of error for drive-time areas
#'
#' Computes the five rates in [`cacs_acs_default_rates`], such as the
#' poverty rate, for each site and drive-time pair in the output of
#' [cacs_propagate_moe()], each with a margin of error (MOE). A margin of
#' error is the half-width of a confidence interval. The rates are added
#' after the input rows, as five new rows for each pair.
#'
#' Each rate is the ratio of two weighted counts of ACS estimates. The two
#' counts and their variances are read from the `cacs_aggregation_carriers`
#' attribute that [cacs_intersect_weight()] attaches. The margin of error is
#' computed from them at the confidence level that [cacs_propagate_moe()]
#' records in the `cacs_confidence_level` attribute (the 90 percent level if
#' there is none). The result does not keep `cacs_aggregation_carriers`, so
#' `cacs_derive_rates()` gives an error if it is run on its own result.
#'
#' The numerator of each of the five rates is part of its denominator, so
#' the rates are proportions, for which the Census Bureau's handbook gives
#' the proportion formula, C1 (U.S. Census Bureau 2020, chapter 8). By
#' default, the margin of error of every rate is computed with the ratio
#' formula, C2, which for the same rate is never narrower than the margin the
#' proportion formula gives. `formula_dispatch` can choose the proportion
#' formula for two of the five rates. The table gives the formula used for
#' each rate with each value of `formula_dispatch`.
#'
#' | Rate | Default | `"auto"` | `"proportion_subset"` |
#' |:--|:--|:--|:--|
#' | `poverty_rate` | ratio | proportion | proportion |
#' | `labor_force_participation` | ratio | proportion | proportion |
#' | `snap_rate` | ratio | ratio | ratio |
#' | `ssi_rate` | ratio | ratio | ratio |
#' | `unemp_rate` | ratio | ratio | ratio |
#'
#' `"auto"` and `"proportion_subset"` give the same rates and margins of
#' error. `"proportion_subset"` also gives a warning naming the three rates
#' that keep the ratio formula, and lists them in the
#' `formula_downgraded_rates` element of the `cacs_rate_provenance`
#' attribute, a list recording how the rates were computed. The warning calls
#' those three rates ineligible for the proportion formula. That wording
#' describes the package's list of rates and not the data: the numerator of
#' each of the three is part of its denominator. Both formulas are given in
#' the "MOE formula families" section of [cacs_propagate_moe()].
#'
#' Under `"auto"` and `"proportion_subset"`, the three rates in the table keep
#' the ratio formula whatever the values are; the choice is built into the
#' package and does not depend on the data. `unemp_rate` keeps it to
#' reproduce the 2025 analysis the package was first written for, and the
#' package gives no reason for `snap_rate` and `ssi_rate`.
#' `vignette("theory-derived-rates", package = "catchmentACS")` describes how
#' the proportion formula can be computed for them from the `estimate` and
#' `moe` of the rows of their two ACS codes.
#'
#' When the value under the square root of the proportion formula is
#' negative, the row uses the ratio formula instead (see
#' [cacs_propagate_moe()]), with `moe_fallback = TRUE` and
#' `moe_fallback_reason = "negative_variance"`.
#'
#' @references U.S. Census Bureau (2020). *Understanding and Using American
#'   Community Survey Data: What All Data Users Need to Know*. Chapter 8,
#'   Calculating Measures of Error for Derived Estimates.
#'
#' @section Rates that are NA:
#' A rate and its margin of error are `NA` when the numerator or the
#' denominator, or the margin of error of either, is missing for the pair.
#' This happens when a code of the rate has no rows in the ACS data, when a
#' tract in the area has a missing estimate or margin of error for one of
#' the two codes (including a Census Bureau code that
#' [cacs_intersect_weight()] sets to `NA`), and when no tract is left for the
#' pair. The codes of all
#' five rates are in [`cacs_acs_default_vars`], the default variables of
#' [cacs_acs_prefetch()] and [cacs_run()]. Rates that are `NA` for these
#' reasons have `"carrier"` in `failure_origin`, the column that records the
#' step at which a row failed; the value is named after the
#' `cacs_aggregation_carriers` attribute, which holds the weighted totals. A
#' rate whose denominator is zero is also `NA`, as is its margin of error,
#' with `"zero_denominator"` in `moe_fallback_reason` and `TRUE` in
#' `moe_fallback`.
#'
#' One warning at the end counts these rows and the rows where the ratio
#' formula replaced the proportion formula. Its class is
#' `catchmentACS_warning_runtime`; when it counts only rows with
#' `failure_origin = "carrier"`, it also has the class
#' `catchmentACS_warning_carrier_missing`.
#'
#' @section Tract counts for rates:
#' On rate rows, `n_tracts_num` and `n_tracts_den` give the numbers of
#' tracts combined for the numerator and for the denominator (the values of
#' `n_tracts` on the rows of those two ACS codes), and `n_tracts` is `NA`.
#' On the rows of the ACS variables, `n_tracts_num` and `n_tracts_den` are
#' `NA`, and they are also `NA` on rate rows with
#' `failure_origin = "carrier"`.
#'
#' When, in the ACS data, a tract in the area has a row for one of the two
#' codes of a rate and not for the other, for example after rows with a
#' missing estimate have been removed, the numerator and the denominator are
#' totals over different sets of tracts. The rate is then no longer the
#' share of one population: it can be much larger or smaller than the share
#' for the area, and even above 1, and no warning is given. The two counts
#' then differ unless the two codes lack the same number of tracts, so equal
#' counts do not rule this out. Rows whose estimate is missing, kept as `NA`
#' rows rather than removed, make the rate `NA`, with a warning (see the
#' "Rates that are NA" section). [cacs_acs_validate()] shows how to check
#' that every tract has one row for each variable.
#'
#' @section Checking rates against ranges:
#' With `options(catchmentACS.audit_rates = TRUE)`, each rate is compared
#' with a fixed range: 0 to 0.6 for `poverty_rate`, 0 to 0.5 for
#' `snap_rate`, 0 to 0.15 for `ssi_rate`, 0 to 0.3 for `unemp_rate`, and
#' 0.3 to 0.85 for `labor_force_participation`. For each rate with values
#' outside its range, a warning of class
#' `catchmentACS_warning_rate_out_of_range` gives the number of those rows
#' and the smallest and largest of their values; the rates are not changed.
#' The `cacs_rate_audit` attribute records whether the check was on
#' (`enabled`), the number of rate rows outside the ranges
#' (`n_out_of_bounds`), and those rows (`rows`). It is attached even when
#' the check is off (the default), with `enabled = FALSE` and no rows.
#'
#' @param weighted_acs A tibble returned by [cacs_propagate_moe()], from
#'   which rows may have been removed (for example with `[` or
#'   `dplyr::filter()`) but whose columns and attributes are kept, including
#'   `cacs_aggregation_carriers`. A tibble returned by
#'   [cacs_intersect_weight()] can also be used; the margins of error of the
#'   rates are then at the 90 percent level. The rates are computed from
#'   `cacs_aggregation_carriers` for each site and drive-time pair that still
#'   has a row, so removing the rows of a rate's codes does not change the
#'   rate.
#' @param rates A named list giving, for each rate, the American Community
#'   Survey (ACS) codes of the numerator and the denominator. Only
#'   [`cacs_acs_default_rates`] (the default), the list of the five built-in
#'   rates, is accepted; any other list, including a subset or a reordering
#'   of it, gives an error.
#' @param formula_dispatch A string choosing the margin-of-error formula for
#'   the rates: `"general_ratio_conservative"` (the default), `"auto"`, or
#'   `"proportion_subset"`. The last two give the same result: the proportion
#'   formula for `poverty_rate` and `labor_force_participation` only, and the
#'   ratio formula for the other three (see Details).
#' @param verbose A logical value. With `TRUE` (the default), the function
#'   shows a one-line summary when the step finishes (see the "Progress
#'   messages" section of [cacs_run()]).
#' @param ... Not used: any argument given here, such as `level = 0.95`,
#'   gives an error. The confidence level of the rates is set by the `level`
#'   argument of [cacs_propagate_moe()].
#' @return The tibble `weighted_acs` with its rows unchanged and in the same
#'   order, followed by the rate rows. On the rate rows:
#'   \describe{
#'     \item{`variable`, `estimand_family`}{The name of the rate, such as
#'       `"poverty_rate"`, and `"derived_rate"`.}
#'     \item{`estimate`, `moe`}{The rate (not a percentage) and its margin of
#'       error.}
#'     \item{`moe_formula_requested`, `moe_formula_effective`}{The formula
#'       chosen for the rate (see the table in Details) and the formula used,
#'       which differs only where the ratio formula replaced the proportion
#'       formula. With `formula_dispatch = "proportion_subset"`, the three
#'       rates that keep the ratio formula have `"general_ratio_conservative"`
#'       in both columns.}
#'     \item{`moe_fallback`, `moe_fallback_reason`}{`TRUE` with
#'       `"negative_variance"` or `"zero_denominator"` when the chosen formula
#'       could not be used (see Details and the "Rates that are NA" section),
#'       and `FALSE` with `"n/a"` otherwise, including when `failure_origin`
#'       is `"carrier"`.}
#'     \item{`weight_sum`}{The smaller of the sums of the coverage weights for
#'       the numerator and for the denominator (a tract's coverage weight is
#'       the share of its area inside the drive-time area); `NA` when
#'       `failure_origin` is `"carrier"`.}
#'     \item{`n_tracts`, `n_tracts_num`, `n_tracts_den`}{See the "Tract counts
#'       for rates" section.}
#'     \item{`failure_origin`}{`"none"`, or `"carrier"` when a count or margin
#'       of error that the rate needs is missing (see the "Rates that are NA"
#'       section).}
#'   }
#'   The other columns are described in the Value section of [cacs_run()].
#'
#'   The attributes of `weighted_acs` are kept, except
#'   `cacs_aggregation_carriers`, and `cacs_confidence_level` is set to the
#'   confidence level used. Two attributes are added:
#'   \describe{
#'     \item{`cacs_rate_provenance`}{A list recording how the rates were
#'       computed. It gives the rates and the formula chosen for each, the
#'       value of `formula_dispatch`, and the rates that
#'       `"proportion_subset"` left on the ratio formula. It also counts the
#'       rate rows: all of them, and those with a substituted formula, a
#'       zero denominator, `failure_origin = "carrier"`, or a rate outside
#'       the check ranges. The last fields are the confidence level and its
#'       z value, the time the rates were computed, and the package
#'       version.}
#'     \item{`cacs_rate_audit`}{The result of the range check described in
#'       the "Checking rates against ranges" section.}
#'   }
#' @seealso For the rates and their margins of error at more length, see
#'   `vignette("theory-derived-rates", package = "catchmentACS")`
#'   (<https://joonho112.github.io/catchmentACS/articles/theory-derived-rates.html>).
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
#' prop <- cacs_propagate_moe(agg, verbose = FALSE)
#'
#' # The five rates, with margins of error at the 90 percent level
#' out <- cacs_derive_rates(prop, verbose = FALSE)
#' is_rate <- out$estimand_family == "derived_rate"
#' out[is_rate, c("variable", "estimate", "moe", "moe_formula_effective")]
#'
#' # Margins of error with the default and with formula_dispatch = "auto"
#' out_auto <- cacs_derive_rates(prop, formula_dispatch = "auto",
#'                               verbose = FALSE)
#' tibble::tibble(variable = out$variable[is_rate],
#'                moe_default = out$moe[is_rate],
#'                moe_auto = out_auto$moe[is_rate],
#'                formula_auto = out_auto$moe_formula_effective[is_rate])
#'
#' options(old)
cacs_derive_rates <- function(weighted_acs,
                              rates = cacs_acs_default_rates,
                              formula_dispatch = "general_ratio_conservative",
                              verbose = TRUE,
                              ...) {

  # `...` takes no arguments. An argument given there, such as level = 0.95,
  # would otherwise be dropped without a word.
  if (...length() > 0L) {
    # The names are read without evaluating the arguments.
    dot_names <- names(substitute(list(...)))[-1L] %||% character(...length())
    dot_names[!nzchar(dot_names)] <- "<unnamed>"
    .cli_abort_schema(c(
      "{.fn cacs_derive_rates} does not take the argument{?s} {.arg {dot_names}}.",
      "i" = "The confidence level of the rates is set by the {.arg level} argument of {.fn cacs_propagate_moe}."
    ))
  }

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    .cli_abort_schema(c(
      "{.arg verbose} must be a non-NA logical scalar.",
      "x" = "Got {.cls {class(verbose)[[1L]]}} of length {.val {length(verbose)}}."
    ))
  }

  if (!inherits(weighted_acs, c("tbl_df", "data.table"))) {
    .cli_abort_schema(c(
      "{.arg weighted_acs} must be a {.cls tbl_df} or {.cls data.table}.",
      "x" = "Got {.cls {class(weighted_acs)[[1]]}}.",
      "i" = "Did you call {.fn cacs_propagate_moe}?"
    ))
  }
  critical <- c("site_id", "drive_time_min", "variable",
                "estimand_family", "estimate", "moe",
                "moe_fallback_reason", "failure_origin")
  missing_cols <- setdiff(critical, names(weighted_acs))
  if (length(missing_cols) > 0L) {
    .cli_abort_schema(c(
      "{.arg weighted_acs} missing critical column{?s}: {.field {missing_cols}}.",
      "i" = "Pass the result of {.fn cacs_propagate_moe}."
    ))
  }

  # The confidence level comes from the input. A table straight from
  # cacs_intersect_weight() has no such attribute, and the rates then get
  # margins of error at the 90 percent level, the level of the published ACS
  # margins of error.
  level <- attr(weighted_acs, "cacs_confidence_level") %||% 0.90
  z <- if (isTRUE(all.equal(level, 0.90))) .Z_ACS_90
       else stats::qnorm(1 - (1 - level) / 2)

  # `rates` is compared with identical(), so a subset of the five rates, a
  # different order, a renamed rate or a changed ACS code all give an error;
  # a list written out again with the same names, order and codes passes.
  if (!is.list(rates) || is.null(names(rates)) || any(!nzchar(names(rates)))) {
    .cli_abort_schema(c(
      "{.arg rates} must be a named {.cls list} of {.code c(num, den)} pairs.",
      "x" = "Got {.cls {class(rates)[[1]]}}.",
      "i" = "Leave {.arg rates} at its default, {.code cacs_acs_default_rates}."
    ))
  }
  if (!identical(rates, .SANCTIONED_RATES_V1)) {
    extras  <- setdiff(names(rates), names(.SANCTIONED_RATES_V1))
    missing <- setdiff(names(.SANCTIONED_RATES_V1), names(rates))
    msg <- c("{.arg rates} must be {.code cacs_acs_default_rates}, the five built-in rates, unchanged.")
    if (length(extras) > 0L) {
      msg <- c(msg,
               "x" = "Not among the built-in rates: {.field {extras}}.")
    }
    if (length(missing) > 0L) {
      msg <- c(msg,
               "x" = "Missing rate{?s}: {.field {missing}}.")
    }
    if (length(extras) == 0L && length(missing) == 0L) {
      msg <- c(msg,
               "x" = "The rate names match, but the ACS codes or the order of the rates differ.")
    }
    msg <- c(msg,
             "i" = "{.fn cacs_derive_rates} derives only these five rates; leave {.arg rates} at its default.")
    .cli_abort_schema(msg)
  }

  carriers <- attr(weighted_acs, "cacs_aggregation_carriers")
  if (is.null(carriers)) {
    .cli_abort_schema(c(
      "{.arg weighted_acs} has no {.attr cacs_aggregation_carriers} attribute, which holds the totals for the numerator and denominator of each rate.",
      "i" = "Pass the result of {.fn cacs_propagate_moe} or {.fn cacs_intersect_weight}; the result of {.fn cacs_derive_rates} does not keep this attribute."
    ))
  }
  .validate_carrier_tbl(carriers, x = NULL, abort = TRUE,
                        require_symmetric_keys = FALSE)

  rate_prog <- .cacs_progress_reporter(n = 1L, label = "Rate derivation",
                                       verbose = verbose)
  rate_done <- FALSE
  on.exit(
    rate_prog$finish(
      n_success = as.integer(rate_done),
      n_failed  = as.integer(!rate_done)
    ),
    add = TRUE
  )

  if (!is.character(formula_dispatch) || length(formula_dispatch) != 1L ||
      is.na(formula_dispatch)) {
    .cli_abort_schema(c(
      "{.arg formula_dispatch} must be a non-NA character scalar.",
      "x" = "Got {.cls {class(formula_dispatch)[[1]]}} of length {.val {length(formula_dispatch)}}."
    ))
  }
  sanctioned_dispatch <- c("general_ratio_conservative",
                           "proportion_subset", "auto")
  if (!formula_dispatch %in% sanctioned_dispatch) {
    .cli_abort_schema(c(
      "{.arg formula_dispatch} must be one of {.val {sanctioned_dispatch}}.",
      "x" = "Got {.val {formula_dispatch}}."
    ))
  }

  formula_per_rate <- if (identical(formula_dispatch, "auto")) {
    vapply(names(rates),
           function(r) .RATE_FORMULA_CATALOGUE[[r]],
           character(1L),
           USE.NAMES = TRUE)
  } else {
    setNames(rep(formula_dispatch, length(rates)), names(rates))
  }
  # The numerator of all five rates is part of its denominator, but only the
  # two rates that .RATE_C1_SUBSET_ELIGIBLE marks TRUE take the proportion
  # formula here; the warning below names the other three, which keep the
  # ratio formula.
  formula_downgraded_rates <- character(0)
  if (identical(formula_dispatch, "proportion_subset")) {
    eligible <- .RATE_C1_SUBSET_ELIGIBLE[names(rates)]
    formula_downgraded_rates <- names(rates)[!eligible]
    if (length(formula_downgraded_rates) > 0L) {
      formula_per_rate[formula_downgraded_rates] <- "general_ratio_conservative"
      .cli_warn_runtime(
        c(
          "{.arg formula_dispatch = 'proportion_subset'} applies only to subset-eligible rates.",
          "i" = "Downgraded rates not C1-eligible in this release to C2: {.field {formula_downgraded_rates}}."
        ),
        phase = "rates"
      )
    }
  }

  # The input has one row per site, drive time and variable, so the pairs are
  # taken with distinct(): five rate rows are added for each site and drive
  # time, however many variable rows that pair has.
  site_dt_pairs <- dplyr::distinct(
    weighted_acs[, c("site_id", "drive_time_min")]
  )
  n_pairs <- nrow(site_dt_pairs)
  n_rates <- length(rates)
  rate_rows <- vector("list", n_pairs * n_rates)
  k <- 0L

  for (i in seq_len(n_pairs)) {
    sid <- site_dt_pairs$site_id[[i]]
    dt  <- site_dt_pairs$drive_time_min[[i]]
    # Any row of the pair works as the template, because
    # cacs_intersect_weight() writes the same provider, profile, OSM snapshot
    # date, ACS year and weighting on every row of a pair.
    template_idx <- which(weighted_acs$site_id == sid &
                          weighted_acs$drive_time_min == dt)[1L]
    template_row <- if (is.na(template_idx)) NULL
                    else weighted_acs[template_idx, , drop = FALSE]

    for (rate_nm in names(rates)) {
      k <- k + 1L
      num_code <- rates[[rate_nm]][["num"]]
      den_code <- rates[[rate_nm]][["den"]]
      formula  <- formula_per_rate[[rate_nm]]
      rate_rows[[k]] <- .compute_one_rate(
        template_row = template_row,
        sid          = sid,
        dt           = dt,
        rate_nm      = rate_nm,
        num_code     = num_code,
        den_code     = den_code,
        formula      = formula,
        carriers     = carriers,
        z            = z
      )
    }
  }

  rate_bind <- if (length(rate_rows) > 0L) {
    dplyr::bind_rows(rate_rows)
  } else {
    weighted_acs[0L, ]
  }
  out <- dplyr::bind_rows(weighted_acs, rate_bind)

  .warn_derive_rates_summary(out, names(rates))
  out <- .rate_formula_audit_inline(out, rates)

  # Attributes of the input that the result keeps.
  schema_version <- attr(weighted_acs, "cacs_schema_version") %||% "1.0"
  agg_prov       <- attr(weighted_acs, "cacs_aggregation_provenance")
  moe_prov       <- attr(weighted_acs, "cacs_moe_provenance")

  attr(out, "cacs_schema_version") <- schema_version
  if (!is.null(agg_prov)) {
    attr(out, "cacs_aggregation_provenance") <- agg_prov
  }
  if (!is.null(moe_prov)) {
    attr(out, "cacs_moe_provenance") <- moe_prov
  }
  attr(out, "cacs_confidence_level") <- level

  # The record of how the rates were computed, for cacs_describe() and for
  # readers of the result.
  rate_mask <- out$variable %in% names(rates)
  n_rate_rows_appended  <- sum(rate_mask, na.rm = TRUE)
  n_fallback_c1_to_c2   <- sum(
    rate_mask & out$moe_fallback_reason == "negative_variance",
    na.rm = TRUE
  )
  n_zero_denominator    <- sum(
    rate_mask & out$moe_fallback_reason == "zero_denominator",
    na.rm = TRUE
  )
  n_missing_moe         <- sum(
    rate_mask & out$moe_fallback_reason == "missing_moe",
    na.rm = TRUE
  )
  n_carrier_missing     <- sum(
    rate_mask & out$failure_origin == "carrier",
    na.rm = TRUE
  )

  attr(out, "cacs_rate_provenance") <- list(
    rates_computed       = names(rates),
    formula_dispatch     = formula_dispatch,
    formula_per_rate     = formula_per_rate,
    n_rate_rows_appended = as.integer(n_rate_rows_appended),
    n_fallback_c1_to_c2  = as.integer(n_fallback_c1_to_c2),
    n_zero_denominator   = as.integer(n_zero_denominator),
    n_missing_moe        = as.integer(n_missing_moe),
    n_carrier_missing    = as.integer(n_carrier_missing),
    n_rate_audit_out_of_bounds = as.integer(
      attr(out, "cacs_rate_audit")$n_out_of_bounds %||% 0L
    ),
    formula_downgraded_rates = formula_downgraded_rates,
    n_formula_downgraded = as.integer(length(formula_downgraded_rates)),
    level                = level,
    z                    = z,
    generated_at         = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    cacs_ver             = as.character(
      utils::packageVersion("catchmentACS")
    )
  )

  # `cacs_aggregation_carriers` is dropped before the validator runs. A rate
  # row is built from two rows of that table but has none of its own, and its
  # variable is the name of the rate ("poverty_rate") rather than an ACS code
  # ("B17001_002"), so the validator would look for rows that cannot be
  # there. Dropping it also means that a second call of cacs_derive_rates()
  # on this result gives an error instead of reusing counts that no longer
  # match the rows.
  attr(out, "cacs_aggregation_carriers") <- NULL

  # abort = TRUE: a row built above that does not match the columns of the
  # long table gives an error here rather than being returned.
  .validate_derive_rates_output(out, abort = TRUE)

  rate_done <- TRUE
  out
}
