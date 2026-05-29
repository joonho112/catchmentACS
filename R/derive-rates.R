# ============================================================================
# derive-rates.R - cacs_derive_rates() (Sec. 23, Phase 6 Step 6.4)
#
# Computes the 5 v1.0 curated catchment ACS rates (poverty_rate, snap_rate,
# ssi_rate, unemp_rate, labor_force_participation) using the
# `cacs_aggregation_carriers` attribute preserved by cacs_intersect_weight()
# and the MOE-propagation results from cacs_propagate_moe().
#
# Also exports `cacs_acs_default_rates` — the package-level public alias for
# the v1.0 curated rate catalogue (`.SANCTIONED_RATES_V1`). Used as the
# `rates` default in `cacs_run()` (Sec. 24.2) so that the Sec. 11.2 LOCKED
# signature does not reach into an internal `:::` symbol at call-evaluation
# time. v1.1 will introduce a parallel `cacs_acs_default_rates_v1_1` if the
# catalogue is extended; the current symbol is value-frozen.
#
# 8-step algorithm (Sec. 23.3):
#   1. Validate weighted_acs schema (Phase 2.3 inherited).
#   2. CURATED WHITELIST — identical(rates, .SANCTIONED_RATES_V1) abort on
#      deviation (E-23-04) with cacs_derive_rates_custom() hint.
#   3. Validate carrier attribute presence + required columns.
#   4. Resolve formula_dispatch (3-mode enum: general_ratio_conservative /
#      proportion_subset / auto) to per-rate vector.
#   5. Per-(site, drive_time, rate) loop with carrier lookup + Sec. 22
#      MOE helpers (.moe_prop_self / .moe_ratio_self).
#   6. Bind rate rows beneath the input weighted_acs row set.
#   7. Aggregate fallback summary warning (W-23-04 single cli_warn).
#   8. Validate output + attach cacs_rate_provenance + consume-then-drop
#      cacs_aggregation_carriers.
#
# Cross-ref: Sec. 23 (spec), Sec. 22 (MOE helpers), Sec. 12.3.1 (long schema).

#' @importFrom stats setNames qnorm
#' @keywords internal
#' @noRd
NULL
# ============================================================================


# ----------------------------------------------------------------------------
# Public alias for the v1.0 curated rate catalogue (Sec. 23.2).
#
# Surfaces `.SANCTIONED_RATES_V1` under the name promised by Sec. 11.2
# function-signature lock and Sec. 24.2 `cacs_run()` default `rates` arg.
# Keeping the internal `.SANCTIONED_RATES_V1` as the single source of truth
# means the curated catalogue is value-frozen once at package load; a v1.1
# extension introduces a parallel symbol rather than mutating this one.
# ----------------------------------------------------------------------------

#' Default catalogue of curated catchment ACS rates
#'
#' The five derived rates that [cacs_derive_rates()] and [cacs_run()] compute
#' by default, supplied as a named `list` of length-2 character vectors. Each
#' element is a `c(num, den)` pair giving the ACS variable codes for a rate's
#' numerator and denominator. The catalogue is fixed for this release; a future
#' release will add a parallel `cacs_acs_default_rates_v1_1` object if the set
#' of rates is extended.
#'
#' Entries:
#' \itemize{
#'   \item `poverty_rate` --- `c(num = "B17001_002", den = "B17001_001")`
#'   \item `snap_rate` --- `c(num = "B22003_002", den = "B22003_001")`
#'   \item `ssi_rate` --- `c(num = "B19056_002", den = "B19056_001")`
#'   \item `unemp_rate` --- `c(num = "B23025_005", den = "B23025_003")`
#'   \item `labor_force_participation` ---
#'     `c(num = "B23025_002", den = "B23025_001")`
#' }
#'
#' @format A named `list` of length 5. Each element is a length-2 `character`
#'   vector with names `c("num", "den")` carrying the ACS variable code for the
#'   rate's numerator and denominator respectively.
#' @seealso [cacs_run()], [cacs_derive_rates()].
#' @family rate and MOE helpers
#' @export
cacs_acs_default_rates <- .SANCTIONED_RATES_V1


# ----------------------------------------------------------------------------
# Carrier-attribute key list (Sec. 23.3 step 3). Sec. 23.2 references a tighter
# 6-column subset (site_id, drive_time_min, variable, est_total, var_total_raw,
# weight_sum) than the Phase 5 contract; we honor the spec subset here so the
# error message names exactly what the spec requires.
# ----------------------------------------------------------------------------

.DERIVE_RATES_REQUIRED_CARRIER_COLS <- c(
  "site_id", "drive_time_min", "variable",
  "est_total", "var_total_raw", "weight_sum"
)


# ----------------------------------------------------------------------------
# Sec. 23.3 step 5 per-(site, drive_time, rate) cell computation.
#
# Returns a single-row tibble matching the canonical long schema
# (Sec. 12.3.1). The carrier lookup is delegated to the Phase 6.2 helpers
# (.lookup_carrier_num / .lookup_carrier_den / .lookup_carrier_num_var /
# .lookup_carrier_den_var) which translate the rate name into num/den ACS
# codes via .SANCTIONED_RATES_V1.
#
# Carrier-missing handling: when any of the four carrier lookups returns NA
# (i.e. the (site_id, drive_time_min, num_code|den_code) tuple is absent from
# the carrier tibble), the helper emits an empty-rate row with
# `failure_origin = "carrier"` and `moe_fallback_reason = "n/a"` per
# Sec. 23.5 — these rows are upstream carrier failures, not MOE fallbacks.
#
# Formula dispatch:
#   - "proportion_subset" -> .moe_prop_self() (Sec. 22.4.3 C1 with C2 fallback)
#   - "general_ratio_conservative" -> .moe_ratio_self() (Sec. 22.4.4 C2)
#
# The returned row's `weight_sum` is `min(num_ws, den_ws, na.rm = TRUE)` per
# Sec. 23.5 (Q23-3 routed to this minimum convention); `n_tracts` is
# `NA_integer_` because the rate is *derived*, not aggregated.
# ----------------------------------------------------------------------------

.compute_one_rate <- function(template_row, sid, dt, rate_nm,
                              num_code, den_code, formula,
                              carriers, z) {

  # 1. Carrier lookups (Phase 6.2 helpers).
  num_est <- .lookup_carrier_num(rate_nm, carriers, sid, dt)
  den_est <- .lookup_carrier_den(rate_nm, carriers, sid, dt)
  num_var <- .lookup_carrier_num_var(rate_nm, carriers, sid, dt)
  den_var <- .lookup_carrier_den_var(rate_nm, carriers, sid, dt)

  # weight_sum from carrier: use min(num_ws, den_ws) per Sec. 23.5 Q23-3.
  ws_lookup <- function(code) {
    .carrier_key_lookup(rate_nm, carriers, sid, dt,
                        code_role = if (identical(code, num_code)) "num"
                                    else "den",
                        value_col = "weight_sum")
  }
  num_ws <- ws_lookup(num_code)
  den_ws <- ws_lookup(den_code)

  # F4 v0.2 — per-rate tract-contribution carriers (bivariate transparency).
  # `n_tracts` on the carrier tibble is the number of tracts that contributed
  # to each ACS-code aggregation; the rate's num and den can come from
  # different ACS table rows (e.g. ssi_rate's B19056_002 vs B19056_001) so
  # the two counts can diverge. Returns numeric (NA_real_ on miss) from
  # `.carrier_key_lookup()`; we coerce to integer at the build-row step so
  # the output column type matches the canonical schema (Sec. 12.3.1).
  n_tracts_num_val <- .lookup_carrier_n_tracts_num(rate_nm, carriers, sid, dt)
  n_tracts_den_val <- .lookup_carrier_n_tracts_den(rate_nm, carriers, sid, dt)

  # 2. Carrier-missing branch — emit empty rate row, do NOT trigger
  # MOE-fallback bookkeeping. Sec. 23.5 routes these via failure_origin.
  # Both `n_tracts_num` and `n_tracts_den` collapse to NA_integer_ here
  # (F4 contract for the carrier-missing edge case).
  if (is.na(num_est) || is.na(den_est) ||
      is.na(num_var) || is.na(den_var)) {
    return(.empty_rate_row(template_row, sid, dt, rate_nm, formula,
                            failure_origin = "carrier"))
  }

  # 3. Lift carrier variance -> MOE (Sec. 22.4.3 / Sec. 22.4.4 inputs).
  num_moe <- if (is.na(num_var) || num_var < 0) NA_real_
             else z * sqrt(num_var)
  den_moe <- if (is.na(den_var) || den_var < 0) NA_real_
             else z * sqrt(den_var)

  # 4. Formula dispatch (Sec. 23.3 step 5; mirrors Sec. 22.3 step 6/7).
  if (identical(formula, "proportion_subset")) {
    probe <- .moe_prop_self(num_est, num_moe, den_est, den_moe, z = z)
    # C1 path with internal fallback chain (zero_den -> missing_moe ->
    # negative_variance -> C1 success). Translate the typed sentinel set
    # into the canonical output fields.
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
      # C1 -> C2 fallback per Sec. 22.5 priority "1 (neg var -> C2)".
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
  } else {  # "general_ratio_conservative" (C2 as-requested)
    # C2 has zero_den + missing_moe branches but no negative_variance branch
    # (Sec. 22.4.4: `inside` is provably non-negative). C2 dispatched as the
    # *requested* formula keeps `fallback = FALSE` on success — the fallback
    # flag means "the requested formula did not apply", not "a C2 formula
    # was used".
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

  # 5. Point estimate.
  rate_estimate <- if (!is.finite(den_est) || .is_zero_den(den_est)) {
    NA_real_
  } else {
    num_est / den_est
  }

  # 6. weight_sum: min over the carrier columns. Sec. 23.5 contract:
  # `min(num_ws, den_ws, na.rm = TRUE)` when at least one is non-NA; NA_real_
  # otherwise.
  ws_vals <- c(num_ws, den_ws)
  rate_weight_sum <- if (all(is.na(ws_vals))) {
    NA_real_
  } else {
    min(ws_vals, na.rm = TRUE)
  }

  # 7. Build the output row from `template_row` provenance to honor the
  # canonical 27-col schema (Sec. 12.3.1; v0.2 F4 grew 24 -> 26 by adding
  # `n_tracts_num` / `n_tracts_den`). The template provides
  # provider / profile / osm_snapshot_date / acs_year / weight_method.
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


# ----------------------------------------------------------------------------
# Empty rate row when a carrier is missing (Sec. 23.5 contract:
# `failure_origin = "carrier"`, `moe_fallback_reason = "n/a"`). The other 17
# columns mirror the canonical 20-col schema; numeric fields collapse to NA
# but the row is *not* a fallback (the function never tried to apply a MOE
# formula) — Sec. 22.7 reserves `moe_fallback_reason` for the MOE math step.
# ----------------------------------------------------------------------------

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
    n_tracts_num_val = NA_integer_,  # F4 v0.2: carrier-missing edge case
    n_tracts_den_val = NA_integer_
  )
}


# ----------------------------------------------------------------------------
# Canonical 27-col long-schema row builder for derived_rate rows. v0.2 F4
# grew the schema 24 -> 26 by adding `n_tracts_num` / `n_tracts_den`; v0.3
# adds `ring_topology`. Uses
# the upstream `template_row` (any row from `weighted_acs` that shares
# (site_id, drive_time_min)) to carry provider / profile / osm_snapshot_date
# / acs_year / weight_method provenance forward.
#
# Column policy (rate row):
#   - `n_tracts`        = NA_integer_ (preserves the v0.1 `is.na(n_tracts)`
#                          rate-row filter idiom; backward compat lock).
#   - `n_tracts_num`    = number of tracts contributing to the rate's
#                          numerator ACS code (from carrier lookup); NA when
#                          the carrier is missing (caller distinguishes via
#                          `failure_origin = "carrier"`).
#   - `n_tracts_den`    = same for the denominator ACS code. These two can
#                          differ when num and den come from separately
#                          suppressed ACS table cells (e.g. ssi_rate's
#                          B19056_002 vs B19056_001).
#
# `weight_basis` is "coverage" per Sec. 23.5, `weight_uncertainty_propagated`
# is FALSE (v1.1 unlocks).
# ----------------------------------------------------------------------------

.build_rate_row <- function(template_row, sid, dt, rate_nm,
                            estimate, moe, weight_sum,
                            formula_req, formula_eff,
                            fallback, fallback_reason,
                            failure_origin,
                            n_tracts_num_val = NA_integer_,
                            n_tracts_den_val = NA_integer_) {

  # Sanitize template defaults so empty templates (e.g. when weighted_acs
  # is itself empty, which should not occur in v0.1 but is a defensive
  # branch) still produce a valid row.
  tpl_get <- function(col, default) {
    if (is.null(template_row) || !col %in% names(template_row) ||
        length(template_row[[col]]) == 0L) {
      return(default)
    }
    template_row[[col]][[1L]]
  }

  # Coerce the new F4 carrier-lookup values to integer storage. The lookup
  # helper returns numeric (NA_real_ on miss) per Sec. 22.6 packed-key
  # contract; the canonical schema requires integer. NA-safe.
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


# ----------------------------------------------------------------------------
# Optional v0.3 Step 4.1 rate audit. This is deliberately opt-in because the
# bounds are warning heuristics, not mathematical validity constraints.
# ----------------------------------------------------------------------------

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
        "i" = "Disable this debug audit with {.code options(catchmentACS.audit_rates = FALSE)}."
      ),
      phase = "Sec. 23 rate derivation"
    )
  }

  if (length(issue_rows) > 0L) {
    audit$rows <- dplyr::bind_rows(issue_rows)
    audit$n_out_of_bounds <- as.integer(nrow(audit$rows))
  }
  attr(out, "cacs_rate_audit") <- audit
  out
}


# ----------------------------------------------------------------------------
# Sec. 23.3 step 7 aggregate fallback warning. A carrier-missing-only summary
# uses `catchmentACS_warning_carrier_missing` while inheriting from the runtime
# warning parent. Mixed fallback summaries keep the generic runtime class so
# Sec. 24's `cacs_run()` aggregation can attribute the warning origin to
# "Rate derivation" (Sec. 23) vs "MOE propagation" (Sec. 22).
# ----------------------------------------------------------------------------

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

  msg <- c(
    "Rate derivation fallback summary:",
    "*" = "{.val {n_c1_to_c2}} row{?s}: C1 -> C2 (negative variance)",
    "*" = "{.val {n_zero_den}} row{?s}: NA (zero denominator)",
    "*" = "{.val {n_missing_moe}} row{?s}: NA (missing input MOE)",
    "*" = "{.val {n_carrier}} row{?s}: carrier missing (failure_origin = 'carrier')",
    "i" = "Per-row reason in {.field moe_fallback_reason} / {.field failure_origin}."
  )

  warn_fn <- if (n_carrier > 0L &&
                 (n_c1_to_c2 + n_zero_den + n_missing_moe) == 0L) {
    .cli_warn_carrier_missing
  } else {
    .cli_warn_runtime
  }

  warn_fn(msg, phase = "Sec. 23 rate derivation")
}


# ============================================================================
# Public entry — cacs_derive_rates() (Sec. 23.2 LOCKED signature)
# ============================================================================

#' Derive curated catchment ACS rates from a propagated-MOE tibble
#'
#' Computes the five curated catchment rates as ratios of area-weighted ACS
#' estimates, attaching a margin of error (MOE, the half-width of a confidence
#' interval) to each. The rates are `poverty_rate`, `snap_rate`, `ssi_rate`,
#' `unemp_rate`, and `labor_force_participation`. The numerator and
#' denominator aggregates come from the `cacs_aggregation_carriers` attribute
#' that [cacs_intersect_weight()] preserves, and the per-variable MOEs come
#' from [cacs_propagate_moe()].
#'
#' The five rates are appended as new `"derived_rate"` rows beneath the input
#' `weighted_acs` rows. Output cardinality is therefore the input row count
#' plus one rate row per (site, drive-time, rate) cell, i.e.
#' `n_distinct_sites * n_distinct_drive_times * length(rates)`. The
#' `cacs_aggregation_carriers` attribute is *consumed and dropped* so that a
#' downstream re-invocation fails loudly rather than silently reusing stale
#' carriers.
#'
#' `formula_dispatch` selects the per-rate MOE formula. The two formula
#' families are the *proportion-of-subset* form (Family C1,
#' `"proportion_subset"`), valid only when the numerator is a true subset of
#' the denominator, and the *conservative general-ratio* form (Family C2,
#' `"general_ratio_conservative"`), valid for any ratio. See
#' [cacs_propagate_moe()] for the underlying family math.
#' - `"general_ratio_conservative"` (default) broadcasts the C2 formula to
#'   every rate, including the C1-eligible `poverty_rate` and
#'   `labor_force_participation`.
#' - `"proportion_subset"` requests C1 for rates that are C1-eligible in this
#'   release. Rates not eligible for C1 in the curated catalogue (`snap_rate`,
#'   `ssi_rate`, and `unemp_rate`) are downgraded to C2 with a warning and
#'   recorded in the `cacs_rate_provenance` attribute. `unemp_rate` remains C2
#'   by the v0.5.0/v1.0 baseline policy even though its numerator is a subset
#'   of the labor-force denominator.
#' - `"auto"` dispatches per rate: `poverty_rate` and
#'   `labor_force_participation` route to C1; `snap_rate`, `ssi_rate`, and
#'   `unemp_rate` route to C2.
#'
#' In every mode, a C1 rate whose subset variance comes out negative falls
#' back to the C2 formula for that cell, with the fallback flagged in
#' `moe_fallback`, `moe_fallback_reason`, and `moe_formula_effective`.
#'
#' The output carries two integer columns, `n_tracts_num` and `n_tracts_den`,
#' immediately after the existing `n_tracts` column. They are populated only
#' on derived-rate rows (`NA_integer_` on the inherited source-variable rows).
#' See the "Policy on n_tracts_num and n_tracts_den" section below for the
#' full contract.
#'
#' @section Policy on n_tracts_num and n_tracts_den:
#' These two integer columns expose the per-tract contribution counts
#' separately for each rate's numerator and denominator ACS codes, surfacing
#' ACS-suppression asymmetry that the single `n_tracts` slot cannot represent.
#'
#' Population rules:
#' \itemize{
#'   \item **Source-variable rows** (every row passed through from
#'     [cacs_propagate_moe()]): both `n_tracts_num` and `n_tracts_den` are
#'     `NA_integer_`. The `n_tracts` column already carries the per-variable
#'     tract count for these rows; the two new columns are reserved for
#'     derived rates only.
#'   \item **Derived-rate rows** (the appended rate rows for
#'     `poverty_rate`, `snap_rate`, `ssi_rate`, `unemp_rate`,
#'     `labor_force_participation`): both columns are populated
#'     **independently** from the carrier-tibble count of contributing tracts
#'     for the numerator ACS code and the denominator ACS code respectively.
#'   \item **Carrier-missing edge case** on a derived-rate row (the
#'     `failure_origin == "carrier"` branch): both columns collapse to
#'     `NA_integer_`. The diagnostic surface for that condition is
#'     `failure_origin`, not the count columns.
#' }
#'
#' The two counts are reported separately rather than collapsed to a single
#' `min(num, den)` because the numerator and denominator of a curated rate
#' often come from separately suppressed ACS table cells. For example,
#' `ssi_rate` divides `B19056_002` (households with Supplemental Security
#' Income) by `B19056_001` (the `B19056` total-household universe), and a
#' catchment can have different valid tract contributions for those two cells.
#' A bivariate exposure lets the caller spot that asymmetry directly; a
#' collapsed value would hide it.
#'
#' The `n_tracts` column itself remains `NA_integer_` on rate rows; only the
#' two `_num` / `_den` slots carry rate-row tract counts.
#'
#' @section Rate definitions and optional bounds audit:
#' `ssi_rate` follows the canonical ACS `B19056` formula
#' `B19056_002 / B19056_001`: households with Supplemental Security Income
#' divided by the `B19056` total-household universe.
#'
#' Set `options(catchmentACS.audit_rates = TRUE)` to enable a warning-only
#' debug audit against the sanctioned rate bounds
#' (`poverty_rate` `[0, 0.6]`, `snap_rate` `[0, 0.5]`, `ssi_rate`
#' `[0, 0.15]`, `unemp_rate` `[0, 0.30]`, and `labor_force_participation`
#' `[0.3, 0.85]`). These bounds are diagnostics, not hard validity rules.
#'
#' @section Carrier-missing warnings:
#' A derived rate requires both numerator and denominator carrier rows for
#' the same (`site_id`, `drive_time_min`) cell. The default ACS variable
#' catalogue includes every carrier required by the five sanctioned rates.
#' Direct pipelines or explicit custom variable sets that omit a carrier
#' still return the affected rate row with `estimate = NA`, `moe = NA`, and
#' `failure_origin = "carrier"`, and emit a
#' `catchmentACS_warning_carrier_missing` condition. That warning also
#' inherits from `catchmentACS_warning_runtime` for backward-compatible
#' warning capture.
#'
#' @param weighted_acs A `tbl_df` (or `data.table`) from
#'   [cacs_propagate_moe()] carrying the canonical long schema and the
#'   `cacs_aggregation_carriers` attribute.
#' @param rates A named `list` of `c(num, den)` ACS variable-code pairs.
#'   Must be exactly [cacs_acs_default_rates], the five sanctioned rates;
#'   any other set aborts. Arbitrary custom ratios are deferred to a future
#'   release.
#' @param formula_dispatch One of `"general_ratio_conservative"` (default;
#'   Family C2 for every rate), `"proportion_subset"` (Family C1 where
#'   eligible, else downgraded to C2), or `"auto"` (per-rate: C1 for
#'   `poverty_rate` and `labor_force_participation`, C2 for the rest).
#' @param verbose logical(1); default `TRUE`. Emits a single
#'   `catchmentACS_message_progress_summary` condition for orchestrator-level
#'   progress capture, in addition to any warning conditions.
#' @param ... Reserved for forward compatibility. The confidence level is
#'   taken from the `cacs_confidence_level` attribute of `weighted_acs`
#'   (default `0.90`); explicit pass-through is not yet supported.
#' @return The input `weighted_acs`, row-bound with one `"derived_rate"` row
#'   per (site, drive-time, rate) cell, in the canonical long schema. The
#'   `n_tracts_num` and `n_tracts_den` columns are populated on rate rows
#'   from the per-tract carrier counts for the numerator and denominator ACS
#'   codes respectively, and are `NA_integer_` on the inherited
#'   source-variable rows (see "Policy on n_tracts_num and n_tracts_den"
#'   below). A `cacs_rate_provenance` attribute is attached, and the
#'   `cacs_aggregation_carriers` attribute is consumed and removed.
#' @seealso [cacs_run()], [cacs_propagate_moe()], [cacs_acs_default_rates].
#' @family core pipeline
#' @export
#' @examples
#' \donttest{
#' iso <- readRDS(system.file("extdata", "legacy_2025_isochrones.rds",
#'                            package = "catchmentACS"))
#' acs <- readRDS(system.file("extdata", "sample_alabama_subset.rds",
#'                            package = "catchmentACS"))
#' iso_subset <- iso[iso$site_id == "AL_SITE_01" &
#'                   iso$drive_time_min == 5, ]
#' agg <- cacs_intersect_weight(iso_sf = iso_subset, acs_sf = acs,
#'                              weight_method = "area", verbose = FALSE)
#' prop <- cacs_propagate_moe(agg, verbose = FALSE)
#' rates <- suppressWarnings(cacs_derive_rates(prop, verbose = FALSE))
#' rate_rows <- rates[rates$estimand_family == "derived_rate", ]
#' rate_rows[, c("variable", "estimate", "moe", "moe_formula_effective")]
#' }
cacs_derive_rates <- function(weighted_acs,
                              rates = cacs_acs_default_rates,
                              formula_dispatch = "general_ratio_conservative",
                              verbose = TRUE,
                              ...) {

  # --------------------------------------------------------------------------
  # Step 1 — schema validation (Sec. 23.3 step 1a/b/c)
  # --------------------------------------------------------------------------
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
    ))                                                                  # E-23-01
  }
  critical <- c("site_id", "drive_time_min", "variable",
                "estimand_family", "estimate", "moe",
                "moe_fallback_reason", "failure_origin")
  missing_cols <- setdiff(critical, names(weighted_acs))
  if (length(missing_cols) > 0L) {
    .cli_abort_schema(c(
      "{.arg weighted_acs} missing critical column{?s}: {.field {missing_cols}}.",
      "i" = "Sec. 23.3 step 1 requires Sec. 12.3.1 canonical long schema + Sec. 22 MOE columns."
    ))                                                                  # E-23-02
  }

  # Level carry-through (Sec. 23.3 step 1c). Default 0.90 when the input
  # was produced outside `cacs_propagate_moe()` (e.g. legacy regression
  # fixture) — Sec. 22.8 ACS Bureau convention.
  level <- attr(weighted_acs, "cacs_confidence_level") %||% 0.90
  z <- if (isTRUE(all.equal(level, 0.90))) .Z_ACS_90
       else stats::qnorm(1 - (1 - level) / 2)

  # --------------------------------------------------------------------------
  # Step 2 — CURATED WHITELIST (Sec. 23.3 step 2)
  #
  # `identical()` rejects any deviation — added rate, missing rate, renamed
  # rate, swapped num/den code. The error message offers the
  # `cacs_derive_rates_custom()` v1.1+ migration path so users with
  # legitimate custom-rate needs are not stuck.
  # --------------------------------------------------------------------------
  if (!is.list(rates) || is.null(names(rates)) || any(!nzchar(names(rates)))) {
    .cli_abort_schema(c(
      "{.arg rates} must be a named {.cls list} of {.code c(num, den)} pairs.",
      "x" = "Got {.cls {class(rates)[[1]]}}.",
      "i" = "Use {.code cacs_derive_rates(rates = cacs_acs_default_rates)} or omit {.arg rates} for the curated catalogue."
    ))                                                                  # E-23-03
  }
  if (!identical(rates, .SANCTIONED_RATES_V1)) {
    extras  <- setdiff(names(rates), names(.SANCTIONED_RATES_V1))
    missing <- setdiff(names(.SANCTIONED_RATES_V1), names(rates))
    msg <- c("{.arg rates} must be exactly the v1.0 curated catalogue.")
    if (length(extras) > 0L) {
      msg <- c(msg,
               "x" = "Unsanctioned rate{?s}: {.field {extras}}.")
    }
    if (length(missing) > 0L) {
      msg <- c(msg,
               "x" = "Missing sanctioned rate{?s}: {.field {missing}}.")
    }
    if (length(extras) == 0L && length(missing) == 0L) {
      msg <- c(msg,
               "x" = "Rate name set matches but num/den ACS codes were modified.")
    }
    msg <- c(msg,
             "i" = "Use {.fn cacs_derive_rates_custom} (v1.1+) for arbitrary ratios.")
    .cli_abort_schema(msg)                                              # E-23-04
  }

  # --------------------------------------------------------------------------
  # Step 3 — carrier attribute validation (Sec. 23.3 step 3)
  # --------------------------------------------------------------------------
  carriers <- attr(weighted_acs, "cacs_aggregation_carriers")
  if (is.null(carriers)) {
    .cli_abort_schema(c(
      "{.arg weighted_acs} missing {.attr cacs_aggregation_carriers}.",
      "i" = "Did you call {.fn cacs_intersect_weight} -> {.fn cacs_propagate_moe}?",
      "x" = "Sec. 23.3 step 3 requires the carrier attribute for per-rate num/den lookup."
    ))                                                                  # E-23-07
  }
  .validate_carrier_tbl(carriers, x = NULL, abort = TRUE,
                        require_symmetric_keys = FALSE)                  # E-23-08

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

  # --------------------------------------------------------------------------
  # Step 4 — formula_dispatch 3-mode resolution (Sec. 23.3 step 4)
  # --------------------------------------------------------------------------
  if (!is.character(formula_dispatch) || length(formula_dispatch) != 1L ||
      is.na(formula_dispatch)) {
    .cli_abort_schema(c(
      "{.arg formula_dispatch} must be a non-NA character scalar.",
      "x" = "Got {.cls {class(formula_dispatch)[[1]]}} of length {.val {length(formula_dispatch)}}."
    ))                                                                  # E-23-10
  }
  sanctioned_dispatch <- c("general_ratio_conservative",
                           "proportion_subset", "auto")
  if (!formula_dispatch %in% sanctioned_dispatch) {
    .cli_abort_schema(c(
      "{.arg formula_dispatch} must be one of {.val {sanctioned_dispatch}}.",
      "x" = "Got {.val {formula_dispatch}}."
    ))                                                                  # E-23-09
  }

  formula_per_rate <- if (identical(formula_dispatch, "auto")) {
    vapply(names(rates),
           function(r) .RATE_FORMULA_CATALOGUE[[r]],
           character(1L),
           USE.NAMES = TRUE)
  } else {
    setNames(rep(formula_dispatch, length(rates)), names(rates))
  }
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
        phase = "Sec. 23 rate derivation"
      )
    }
  }

  # --------------------------------------------------------------------------
  # Step 5 — per-(site, drive_time, rate) loop with carrier lookup
  #
  # We use `dplyr::distinct()` on (site_id, drive_time_min) so the rate
  # cardinality contract `n_distinct_sites x n_distinct_drive_times x
  # length(rates)` (Sec. 23.5) holds even when the input has multiple rows
  # per (site, drive_time) pair (which it does — one per ACS variable).
  # --------------------------------------------------------------------------
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
    # Pull one template row from the input for provenance carry-through
    # (provider/profile/osm_snapshot_date/acs_year/weight_method). Any row
    # at this (site, drive_time) pair works because Phase 5 emits one
    # provenance bag per (site, drive_time) batch.
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

  # --------------------------------------------------------------------------
  # Step 6 — bind rate rows beneath input weighted_acs (Sec. 23.3 step 6)
  # --------------------------------------------------------------------------
  rate_bind <- if (length(rate_rows) > 0L) {
    dplyr::bind_rows(rate_rows)
  } else {
    weighted_acs[0L, ]
  }
  out <- dplyr::bind_rows(weighted_acs, rate_bind)

  # --------------------------------------------------------------------------
  # Step 7 — aggregate fallback summary warning (Sec. 23.3 step 7; W-23-04)
  # --------------------------------------------------------------------------
  .warn_derive_rates_summary(out, names(rates))
  out <- .rate_formula_audit_inline(out, rates)

  # --------------------------------------------------------------------------
  # Step 8 — validate output + attach provenance + consume-then-drop carrier
  # (Sec. 23.3 step 8)
  # --------------------------------------------------------------------------

  # Preserve upstream attributes (Sec. 23.5 carry-through contract).
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

  # Rate provenance (Sec. 23.5 new attribute).
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

  # Consume-then-drop carrier (Sec. 23.5 final contract) BEFORE the
  # validator so the inherited `.validate_intersect_output()` does NOT try
  # to key-match derived_rate row variables (e.g. `"poverty_rate"`) against
  # the carrier's ACS-code rows (e.g. `"B17001_002"`). The derived_rate
  # rows are *constructed from* the carriers, not *keyed by* them; the
  # carrier contract holds inside `.compute_one_rate()` and the validator
  # then runs in derived_rate-only mode (require_carrier = FALSE).
  # Downstream `cacs_run()` orchestrator re-invocation must fail loud
  # rather than silently re-use stale carriers; the carrier drop here
  # enforces that contract.
  attr(out, "cacs_aggregation_carriers") <- NULL

  # Phase 2.3 validator. abort = TRUE so a schema regression in Steps 5-6
  # lands as a hard `catchmentACS_error_schema` here.
  .validate_derive_rates_output(out, abort = TRUE)

  rate_done <- TRUE
  out
}
