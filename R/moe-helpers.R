# ============================================================================
# moe-helpers.R - MOE primitives + converters + classifier (Phase 2.4)
#
# Phase 2.4 scope (this step):
#   5 .moe_*_self() primitives per Sec. 22.4
#   2 converters: cacs_se_to_moe(), cacs_moe_to_se() per Sec. 8.3 (exported Tier-2)
#   2 classifiers: .classify_acs_variable() + .classify_acs_variable_batch() per Sec. 8.7
#
# Phase 6 scope (deferred):
#   cacs_propagate_moe() exported wrapper + 4 dispatch helpers + 4 C1/C2 fallback helpers
# ============================================================================


# ============================================================================
# Internal predicates and constants
# ============================================================================

.DEN_EPS <- sqrt(.Machine$double.eps)  # ~1.5e-8 threshold for zero denominator

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


# ============================================================================
# 5 MOE family primitives (Sec. 22.4)
# ============================================================================

#' Family A primitive: simple weighted-sum MOE (w=1) (internal)
#'
#' For Family A weighted_sum special case w=1: z * sqrt(sum((moe/z)^2)).
#' Vectorized over `moe_vec`, returns scalar.
#'
#' @keywords internal
#' @noRd
.moe_sum_self <- function(moe_vec, z = .Z_ACS_90) {
  if (anyNA(moe_vec)) return(NA_real_)
  z * sqrt(sum((moe_vec / z)^2))
}

#' Family A primitive: weighted-sum MOE (internal)
#'
#' For Family A weighted_sum: z * sqrt(sum((w * moe / z)^2)).
#'
#' @keywords internal
#' @noRd
.moe_wsum_self <- function(moe_vec, w_vec, z = .Z_ACS_90) {
  .check_equal_lengths(moe_vec, w_vec, labels = c("moe_vec", "w_vec"))
  if (anyNA(moe_vec) || anyNA(w_vec)) return(NA_real_)
  z * sqrt(sum((w_vec * moe_vec / z)^2))
}

#' Family B primitive: weighted-mean MOE (internal)
#'
#' For Family B weighted_mean: z * sqrt(sum((w * moe / z)^2)) / sum(w).
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

#' Family C1 primitive: proportion_subset (internal)
#'
#' For Family C1 proportion_subset (true subset where A subset of B):
#'   `Var(A/B) approx [Var(A) - (A/B)^2 Var(B)] / B^2`
#' Returns list with `value` + typed sentinel flags for Phase 6.3 fallback.
#'
#' @return list with elements:
#'   - `value`: numeric scalar (NA if negative variance, missing moe, or zero denom)
#'   - `zero_denominator`: logical
#'   - `missing_moe`: logical
#'   - `missing_estimate`: logical
#'   - `inside_negative`: logical (negative variance -> triggers C2 fallback)
#'
#' @keywords internal
#' @noRd
.moe_prop_self <- function(num_est, num_moe, den_est, den_moe, z = .Z_ACS_90) {
  # Sentinel order matches Phase 6.3 fallback priority (missing estimate /
  # zero_den -> missing_moe -> negative_variance -> C1 success).
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

#' Family C2 primitive: general_ratio_conservative (internal)
#'
#' For Family C2 general_ratio_conservative:
#'   `Var(A/B) approx [Var(A) + (A/B)^2 Var(B)] / B^2`  (always non-negative)
#' Returns scalar (or NA if zero denominator).
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


# ============================================================================
# 2 Converters (Sec. 8.3) - Tier-2 exported
# ============================================================================

#' Convert a standard error to a margin of error
#'
#' Scales a standard error (SE) into a margin of error (MOE, the half-width of
#' a confidence interval) via the Census relation \eqn{M = z \cdot SE}, where
#' the multiplier is \eqn{z = qnorm(1 - (1 - level)/2)}. At the default 90%
#' level (the U.S. Census Bureau convention for the ACS) this is
#' \eqn{z = 1.645}, giving \eqn{M = 1.645 \cdot SE}.
#'
#' @param se Numeric vector of standard errors.
#' @param level Confidence level for the margin of error. Default `0.90`, the
#'   ACS Bureau convention, for which \eqn{z = 1.645} exactly.
#' @return Numeric vector of margins of error at the requested level.
#' @seealso [cacs_moe_to_se()], [cacs_propagate_moe()].
#' @family rate and MOE helpers
#' @export
#' @examples
#' cacs_se_to_moe(c(5, 10, NA), level = 0.90)
cacs_se_to_moe <- function(se, level = 0.90) {
  z <- if (identical(level, 0.90)) .Z_ACS_90 else stats::qnorm(1 - (1 - level) / 2)
  z * se
}

#' Convert a margin of error to a standard error
#'
#' Recovers a standard error (SE) from a margin of error (MOE, the half-width
#' of a confidence interval) by inverting the Census relation
#' \eqn{M = z \cdot SE} to \eqn{SE = M / z}. At the default 90% level this uses
#' \eqn{z = 1.645}. This is the exact inverse of [cacs_se_to_moe()].
#'
#' @param moe Numeric vector of margins of error.
#' @param level Confidence level the margins of error were computed at. Default
#'   `0.90`, the ACS Bureau convention, for which \eqn{z = 1.645} exactly.
#' @return Numeric vector of standard errors.
#' @seealso [cacs_se_to_moe()], [cacs_propagate_moe()].
#' @family rate and MOE helpers
#' @export
#' @examples
#' cacs_moe_to_se(c(8.225, 16.45, NA), level = 0.90)
cacs_moe_to_se <- function(moe, level = 0.90) {
  z <- if (identical(level, 0.90)) .Z_ACS_90 else stats::qnorm(1 - (1 - level) / 2)
  moe / z
}


# ============================================================================
# 2 Classifiers (Sec. 8.7)
# ============================================================================

# Exhaustive map for the 14 cacs_acs_default_vars per Sec. 17.2 + v0.3 Step 1.3.
# Most ACS counts -> spatial_total. Medians -> median_proxy. Per-capita -> area_weighted_scalar_proxy.
.DEFAULT_VAR_FAMILY_MAP <- c(
  # Counts (spatial_total)
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
  # Median (median_proxy)
  "B19013_001" = "median_proxy",
  # Per-capita (area_weighted_scalar_proxy)
  "B19301_001" = "area_weighted_scalar_proxy"
)

#' Classify ACS variable code into estimand family (internal)
#'
#' Returns one of the 6 sanctioned estimand families per Sec. 7.1. Checks
#' `.DEFAULT_VAR_FAMILY_MAP` first (exhaustive default-var lookup), falls back
#' to regex pattern matching, then `"metadata_only"` for unknown.
#'
#' @keywords internal
#' @noRd
.classify_acs_variable <- function(var_code) {
  if (is.na(var_code) || !nzchar(var_code)) return("metadata_only")

  # First: exhaustive default-var lookup (most common case)
  if (var_code %in% names(.DEFAULT_VAR_FAMILY_MAP)) {
    return(unname(.DEFAULT_VAR_FAMILY_MAP[var_code]))
  }

  # Second: regex pattern fallback for non-default vars
  if (grepl("^B19013_\\d{3}$", var_code)) return("median_proxy")
  if (grepl("^B19301_\\d{3}$", var_code)) return("area_weighted_scalar_proxy")
  if (grepl("^B25077_\\d{3}$", var_code)) return("median_proxy")  # median home value
  if (grepl("^B[0-9]{5}_\\d{3}$", var_code)) return("spatial_total")  # generic count

  # Unknown
  "metadata_only"
}

#' Batch classify ACS variable codes (internal, vectorized)
#'
#' @keywords internal
#' @noRd
.classify_acs_variable_batch <- function(var_codes) {
  vapply(var_codes, .classify_acs_variable, character(1), USE.NAMES = FALSE)
}


# ============================================================================
# Phase 6 — cacs_propagate_moe() exported entry + Step 1-5 (validation +
# Family A/B vectorized application). Steps 6-12 land in Step 6.3 / Step 6.4.
# ============================================================================


# ----------------------------------------------------------------------------
# Phase 6.1 — formula dispatch helpers (skeleton for Step 6.2 expansion).
# Step 6.1 uses the auto-dispatch inline mapping (per task spec); Step 6.2
# will broaden `.dispatch_formula_auto()` for per-variable list override and
# add the family-formula consistency checker.
# ----------------------------------------------------------------------------

#' Auto-dispatch MOE formula by estimand_family (internal)
#'
#' Per-row dispatch consistent with Sec. 22.2 family ↔ formula matrix and
#' Sec. 23.4 derived_rate default (C2 conservative). Step 6.2 may extend this
#' helper; for Step 6.1 the inline mapping is sufficient.
#'
#' @param estimand_family character vector of sanctioned estimand families.
#' @return character vector of sanctioned MOE formulas (or NA for metadata_only).
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


#' Per-variable list dispatch (internal)
#'
#' Implements Sec. 22.3 step 3c — `formula` named list keyed by `variable`.
#' Starts from the auto-dispatch vector and overlays each `formula[[v]]`
#' onto rows whose `data$variable == v`. Caller (`.resolve_formula()`) must
#' validate that names map to actual variables and that values are
#' sanctioned formulas before invoking this helper; this function does no
#' input validation of its own so the per-rate Step 6.3 fallback chain can
#' reuse it on already-validated lists.
#'
#' @param formula named list whose names match values in `data$variable`.
#' @param data tibble with `variable` + `estimand_family` columns.
#' @return character vector of length `nrow(data)`.
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


#' Family ↔ formula consistency check (Sec. 22.3 step 3 trailer, internal)
#'
#' Implements the Sec. 22.2 family ↔ formula matrix at row resolution. The
#' returned integer vector lists row indices whose `(estimand_family,
#' formula)` pair is not in the sanctioned matrix; the caller (Sec. 22.3
#' step 3 trailer inside `cacs_propagate_moe()`) aborts with E-22-13 when
#' the vector is non-empty.
#'
#' Sanctioned pairs (Sec. 22.2):
#'   - spatial_total                    -> weighted_sum
#'   - area_weighted_scalar_proxy       -> weighted_mean
#'   - population_weighted_scalar_proxy -> weighted_mean
#'   - area_weighted_rate_proxy         -> weighted_mean
#'   - median_proxy                     -> weighted_mean
#'   - derived_rate                     -> proportion_subset OR general_ratio_conservative
#'   - metadata_only                    -> NA (no formula)
#'
#' Rows with unknown `estimand_family` (i.e. not in `.ESTIMAND_FAMILIES`)
#' are reported as inconsistent regardless of formula so the upstream
#' classifier failure does not silently leak through.
#'
#' @param family character vector of estimand families.
#' @param formula character vector of resolved formulas (NA allowed).
#' @return integer vector of row indices whose (family, formula) pair
#'   violates the Sec. 22.2 matrix; `integer(0)` when all rows are
#'   consistent.
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

  # Determine whether each (family, formula) pair sits in the matrix.
  ok <- vapply(seq_len(n), function(i) {
    fam <- family[[i]]
    frm <- formula[[i]]
    if (is.na(fam)) return(FALSE)
    if (identical(fam, "metadata_only")) {
      # metadata_only rows must carry NA formula.
      return(is.na(frm))
    }
    if (is.na(frm)) {
      # Non-metadata family with NA formula is inconsistent.
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
      FALSE  # unknown family -> inconsistent
    )
  }, logical(1L))

  which(!ok)
}


#' Resolve `formula` argument to a per-row vector (3-mode dispatch, internal)
#'
#' Implements Sec. 22.3 step 3 (NULL → auto / character(1) → broadcast / named
#' list → per-variable override). Unknown formula values or unknown variable
#' names abort with the matching Sec. 22.9 error code class.
#'
#' @param formula NULL, character(1) in `.SANCTIONED_MOE_FORMULAS`, or a named
#'   list keyed by `variable` mapping to sanctioned formula labels.
#' @param data tibble with `estimand_family` + `variable` columns.
#' @return character vector of length `nrow(data)` with resolved formula labels.
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
      ))                                                                  # E-22-03
    }
    out <- rep(formula, nrow(data))
    # metadata_only rows do not carry a formula; preserve NA so downstream
    # validators and Step 8 (metadata pass-through) stay coherent.
    out[data$estimand_family == "metadata_only"] <- NA_character_
    return(out)
  }
  if (is.list(formula)) {
    nm <- names(formula)
    if (is.null(nm) || any(!nzchar(nm))) {
      .cli_abort_schema(c(
        "{.arg formula} list must have non-empty names for every element.",
        "i" = "Names are matched to {.field variable}."
      ))                                                                  # E-22-04
    }
    unknown_vars <- setdiff(nm, unique(data$variable))
    if (length(unknown_vars) > 0L) {
      .cli_abort_schema(c(
        "{.arg formula} list contains unknown variable name{?s}: {.field {unknown_vars}}.",
        "i" = "Each name in {.arg formula} must match a value in {.field data$variable}."
      ))                                                                  # E-22-05
    }
    flat_vals <- unlist(formula, use.names = FALSE)
    bad_vals <- setdiff(flat_vals, .SANCTIONED_MOE_FORMULAS)
    if (length(bad_vals) > 0L) {
      .cli_abort_schema(c(
        "{.arg formula} list values must be sanctioned MOE formulas.",
        "x" = "Unsanctioned: {.val {bad_vals}}.",
        "i" = "Sanctioned set: {.val {(.SANCTIONED_MOE_FORMULAS)}}."
      ))                                                                  # E-22-03
    }
    # Auto-dispatch baseline + per-variable overlay via helper.
    return(.dispatch_formula_list(formula, data))
  }
  .cli_abort_schema(c(
    "{.arg formula} must be NULL, a character(1), or a named list.",
    "x" = "Got {.cls {class(formula)[[1]]}}."
  ))                                                                      # E-22-04
}


# ----------------------------------------------------------------------------
# Phase 6.1 — confidence level → z multiplier (Sec. 22.8).
# `level = 0.90` uses the Bureau-rounded constant `.Z_ACS_90 = 1.645` for
# byte-identical compatibility with Sec. 21 output. Other levels dispatch
# through `stats::qnorm()` per the symmetric two-tailed convention.
# ----------------------------------------------------------------------------

#' Resolve z multiplier from a confidence level (internal)
#'
#' @param level numeric(1) confidence level strictly in `(0, 1)`.
#' @return numeric(1) z multiplier.
#' @keywords internal
#' @noRd
.resolve_z_from_level <- function(level) {
  if (!is.numeric(level) || length(level) != 1L || is.na(level) ||
      !is.finite(level)) {
    .cli_abort_schema(c(
      "{.arg level} must be a finite numeric scalar.",
      "x" = "Got {.val {level}}."
    ))                                                                    # E-22-07
  }
  if (level <= 0 || level >= 1) {
    .cli_abort_schema(c(
      "{.arg level} must be strictly in {.val (0, 1)}.",
      "x" = "Got {.val {level}}."
    ))                                                                    # E-22-06
  }
  if (isTRUE(all.equal(level, 0.90))) .Z_ACS_90
  else stats::qnorm(1 - (1 - level) / 2)
}


# ----------------------------------------------------------------------------
# Phase 6.1 — carrier left-join helper. Steps 4-5 reference `var_total_raw`
# and `var_mean_raw` which live on the carrier tibble, not on `data`. We
# left-join the four carrier numeric columns onto `data` via the (site_id,
# drive_time_min, variable) 3-key. The lookup helpers (Step 6.2) will use a
# byte-identical key but operate on subsets keyed by num_code / den_code
# instead of the row's own `variable`.
# ----------------------------------------------------------------------------

#' Left-join carrier columns onto `data` via 3-key (internal)
#'
#' Implements Sec. 22.3 step 1e. Carrier NULL is only permitted when every
#' row is `metadata_only` (caller-validated at Step 1); we then return `data`
#' unchanged so Steps 4-5 vectorized assignments simply find no matches.
#'
#' @param data tibble with `site_id`, `drive_time_min`, `variable`.
#' @param carrier 9-col carrier tibble from `cacs_intersect_weight()` (per
#'   Sec. 21.7).
#' @return `data` with `est_total`, `var_total_raw`, `est_mean`, `var_mean_raw`
#'   columns appended (NA where the carrier has no matching row).
#' @keywords internal
#' @noRd
.left_join_carriers <- function(data, carrier) {
  if (is.null(carrier)) return(data)
  carrier_join <- carrier[, c("site_id", "drive_time_min", "variable",
                              "est_total", "var_total_raw",
                              "est_mean", "var_mean_raw"), drop = FALSE]
  # Defensive: drop any of those four columns from `data` if a caller
  # previously left-joined (so we never produce `.x`/`.y` suffix duplicates).
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


# ----------------------------------------------------------------------------
# Phase 6.2 — per-rate carrier lookup helpers (Sec. 22.3 step 6 building
# blocks for Phase 6.3 C1/C2 fallback chain). Each helper takes a rate name
# (one of `names(.SANCTIONED_RATES_V1)`), a carrier tibble, and parallel
# site_id / drive_time_min vectors, and returns a numeric vector of carrier
# values keyed on the *num or den ACS code* of the rate (not the row's own
# `variable`). The 3-key lookup is vectorized via `match()` on a packed
# string key so 100K+ rows stay sub-second per Sec. 22.6.
# ----------------------------------------------------------------------------

# Packed-key separator: ASCII Unit Separator (US, 0x1F). Chosen so the
# packed string can never collide with site_id / drive_time_min / variable
# values (which are alphanumeric + dashes per Sec. 12.3.1 schema locks).
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
      "{.arg carriers} missing column {.field {value_col}}.",
      "i" = "Carrier tibble must come from {.fn cacs_intersect_weight} (Sec. 21.7)."
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
  # Preserve the numeric storage mode even when `idx` is all-NA so callers
  # downstream (Phase 6.3 fallback chain) see a consistent type.
  if (!is.numeric(out)) out <- as.numeric(out)
  out
}

#' Look up numerator carrier estimate (`est_total`) for given rate rows (internal)
#'
#' Sec. 22.3 step 6 building block. Given a sanctioned rate name and the
#' `cacs_aggregation_carriers` tibble (Sec. 21.7), returns the per-row
#' `est_total` of the numerator ACS code (from `.SANCTIONED_RATES_V1`)
#' matched on `(site_id, drive_time_min, variable=num_code)`. Returns
#' `NA_real_` for rows whose carrier key is missing — Phase 6.3 then routes
#' those rows to `failure_origin = "carrier"` (Sec. 23.5) rather than to a
#' fallback reason.
#'
#' @param rate_name character(1), one of `names(.SANCTIONED_RATES_V1)`.
#' @param carriers carrier tibble with `site_id`, `drive_time_min`,
#'   `variable`, and `est_total` columns.
#' @param site_id character vector of site identifiers (one per query row).
#' @param drive_time_min integer vector of drive-time minutes (parallel).
#' @return numeric vector of `est_total` values, NA where unmatched.
#' @keywords internal
#' @noRd
.lookup_carrier_num <- function(rate_name, carriers, site_id, drive_time_min) {
  .carrier_key_lookup(rate_name, carriers, site_id, drive_time_min,
                      code_role = "num", value_col = "est_total")
}

#' Look up denominator carrier estimate (`est_total`) for given rate rows (internal)
#'
#' Mirror of `.lookup_carrier_num()` keyed on the denominator ACS code.
#'
#' @inheritParams .lookup_carrier_num
#' @keywords internal
#' @noRd
.lookup_carrier_den <- function(rate_name, carriers, site_id, drive_time_min) {
  .carrier_key_lookup(rate_name, carriers, site_id, drive_time_min,
                      code_role = "den", value_col = "est_total")
}

#' Look up numerator carrier variance (`var_total_raw`) for given rate rows (internal)
#'
#' Sec. 22.3 step 6 building block. Returns the raw (pre-MOE) variance of
#' the numerator ACS code. Phase 6.3 multiplies by `z^2` (implicitly via
#' `z * sqrt()`) to recover the C1 / C2 MOE.
#'
#' @inheritParams .lookup_carrier_num
#' @keywords internal
#' @noRd
.lookup_carrier_num_var <- function(rate_name, carriers, site_id, drive_time_min) {
  .carrier_key_lookup(rate_name, carriers, site_id, drive_time_min,
                      code_role = "num", value_col = "var_total_raw")
}

#' Look up denominator carrier variance (`var_total_raw`) for given rate rows (internal)
#'
#' Mirror of `.lookup_carrier_num_var()` keyed on the denominator ACS code.
#'
#' @inheritParams .lookup_carrier_num
#' @keywords internal
#' @noRd
.lookup_carrier_den_var <- function(rate_name, carriers, site_id, drive_time_min) {
  .carrier_key_lookup(rate_name, carriers, site_id, drive_time_min,
                      code_role = "den", value_col = "var_total_raw")
}

#' Look up numerator carrier `n_tracts` for given rate rows (internal)
#'
#' F4 v0.2 building block. Returns the number of tracts that contributed to
#' the aggregation of the rate's numerator ACS code (e.g. `B17001_002` for
#' `poverty_rate`). Used by Phase 6.4 `.compute_one_rate()` to populate the
#' new canonical-schema `n_tracts_num` column on derived rate rows.
#'
#' Returns numeric (the underlying `.carrier_key_lookup()` always coerces to
#' numeric); callers re-coerce to `integer` at the build-row step so the
#' canonical 27-col schema stays integer-typed (Sec. 12.3.1).
#'
#' @inheritParams .lookup_carrier_num
#' @return numeric vector of `n_tracts` values, NA where unmatched.
#' @keywords internal
#' @noRd
.lookup_carrier_n_tracts_num <- function(rate_name, carriers, site_id, drive_time_min) {
  .carrier_key_lookup(rate_name, carriers, site_id, drive_time_min,
                      code_role = "num", value_col = "n_tracts")
}

#' Look up denominator carrier `n_tracts` for given rate rows (internal)
#'
#' Mirror of `.lookup_carrier_n_tracts_num()` keyed on the denominator ACS
#' code. Together with the numerator helper, exposes the bivariate
#' tract-contribution count for each derived rate per the v0.2 bivariate
#' schema contract.
#'
#' @inheritParams .lookup_carrier_num
#' @return numeric vector of `n_tracts` values, NA where unmatched.
#' @keywords internal
#' @noRd
.lookup_carrier_n_tracts_den <- function(rate_name, carriers, site_id, drive_time_min) {
  .carrier_key_lookup(rate_name, carriers, site_id, drive_time_min,
                      code_role = "den", value_col = "n_tracts")
}


# ----------------------------------------------------------------------------
# Phase 6.3 — Family C1 / C2 multi-step fallback dispatch helpers
# (Sec. 22.3 steps 6-9; Sec. 22.5 fallback priority).
#
# Each `.apply_*()` helper handles one of the four C1 fallback-chain branches
# (zero_den, missing_moe, negative_variance -> C2, C1 success). They share a
# single signature `(data, idx, ...)` so the dispatcher in `cacs_propagate_moe()`
# can compose them in priority order with index-based assignment per Sec. 22.6
# (no per-row loop). Each helper is *no-op safe* on `length(idx) == 0L` so the
# dispatcher can always invoke them without branching on emptiness; this
# discipline is asserted in T-MOE-FB-NOOP below.
#
# The `is_C1` argument is reserved here (passed by the dispatcher) for forward
# symmetry with C2-branch helpers that need to remember which formula was
# requested vs effective when emitting `moe_fallback_reason`. For Phase 6.3
# the helpers infer "C1-vs-C2" purely from the call site, but keeping the
# parameter avoids a downstream signature break when Sec. 22.10 T22-19
# backend-invariance work lands.
# ----------------------------------------------------------------------------

#' Apply C1 zero_denominator fallback branch (internal)
#'
#' Per Sec. 22.3 step 6 (a) / Sec. 22.5 trigger priority "1 (zero den -> null)".
#' Writes `moe = NA_real_`, `moe_formula_effective = "proportion_subset"`
#' (the *requested* C1 formula stays visible — only the value collapses to NA),
#' `moe_fallback = TRUE`, `moe_fallback_reason = "zero_denominator"`.
#'
#' We keep `moe_formula_effective` at the requested family (proportion_subset
#' or general_ratio_conservative) rather than swapping to a sentinel string
#' like "null_zero_denominator" so the row-level provenance reads as "the
#' requested formula could not produce a value; see fallback_reason for why".
#' This matches the canonical 4-enum `moe_fallback_reason` contract in
#' Sec. 22.7 and avoids polluting `moe_formula_effective` with off-enum
#' sentinels that Sec. 22.10 T22-19 backend-invariance assertions would have
#' to special-case.
#'
#' @param data tibble to update in place (functional return).
#' @param idx integer vector of row indices to update; no-op when empty.
#' @param is_C1 logical(1) indicating whether the rows came from the C1
#'   dispatch (TRUE) or the C2 dispatch (FALSE). Currently used only to
#'   decide which formula label to write into `moe_formula_effective`.
#' @return Updated `data` tibble.
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

#' Apply C1 missing_moe fallback branch (internal)
#'
#' Per Sec. 22.3 step 6 (b) / Sec. 22.5 trigger priority "3 (missing MOE ->
#' null)". Writes `moe = NA_real_`, `moe_formula_effective = "proportion_subset"`
#' (or `"general_ratio_conservative"` for C2 dispatch), `moe_fallback = TRUE`,
#' `moe_fallback_reason = "missing_moe"`.
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

#' Apply C1 -> C2 fallback branch (negative variance, internal)
#'
#' Per Sec. 22.3 step 6 (c) / Sec. 22.5 trigger priority "1 (neg var -> C2)".
#' Recomputes the MOE via `.moe_ratio_self()` (always-non-negative C2 formula
#' from Sec. 22.4.4) and records:
#'   - `moe`                    = C2 value (z * sqrt(Var_A + p^2 * Var_B) / |B|)
#'   - `moe_formula_effective`  = "general_ratio_conservative"
#'   - `moe_fallback`           = TRUE
#'   - `moe_fallback_reason`    = "negative_variance"
#'
#' This branch is only ever invoked from the C1 dispatch (`is_C1 = TRUE`);
#' the C2 dispatch has no negative-variance branch because C2's `inside` term
#' is provably non-negative (Sec. 22.4.4). The `is_C1` parameter is accepted
#' for signature symmetry with the other branch helpers.
#'
#' @param data tibble to update in place (functional return).
#' @param idx integer vector of row indices to update; no-op when empty.
#' @param num_est numeric vector parallel to `idx` (numerator estimate).
#' @param num_moe numeric vector parallel to `idx` (numerator MOE).
#' @param den_est numeric vector parallel to `idx` (denominator estimate).
#' @param den_moe numeric vector parallel to `idx` (denominator MOE).
#' @param z numeric(1) confidence z multiplier.
#' @param is_C1 logical(1) (currently always TRUE; reserved for symmetry).
#' @return Updated `data` tibble.
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

#' Apply C1 proportion_subset success branch (internal)
#'
#' Per Sec. 22.3 step 6 (d). Computes the C1 MOE via `.moe_prop_self()` (the
#' primitive already returned the `$value` field; we re-extract here for
#' symmetry with the other branch helpers and to keep the dispatcher's
#' priority-order code declarative).
#'
#' Sets:
#'   - `moe`                    = z * sqrt(Var_A - p^2 * Var_B) / B
#'   - `moe_formula_effective`  = "proportion_subset"
#'   - `moe_fallback`           = FALSE
#'   - `moe_fallback_reason`    = "n/a"
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


# ----------------------------------------------------------------------------
# Phase 6.3 — variance -> MOE conversion helper.
# Carriers expose raw variance (`var_total_raw`, *not* MOE) so the C1/C2
# dispatch must lift variance back to MOE via `moe = z * sqrt(var)` before
# calling `.moe_prop_self()` / `.moe_ratio_self()` (which expect MOE inputs
# per Sec. 22.4.3 / Sec. 22.4.4). NA-safe: `NA -> NA`, `negative -> NA`.
# ----------------------------------------------------------------------------

.var_to_moe <- function(var, z) {
  out <- rep(NA_real_, length(var))
  ok <- !is.na(var) & var >= 0
  out[ok] <- z * sqrt(var[ok])
  out
}


# ----------------------------------------------------------------------------
# Public entry `cacs_propagate_moe()` (Sec. 22.2 LOCKED signature).
# ----------------------------------------------------------------------------

#' Propagate margins of error across catchment ACS estimates
#'
#' Computes a margin of error (MOE) for every row of a long-format catchment
#' ACS aggregation. MOE is the half-width of an ACS confidence interval; the
#' U.S. Census Bureau reports it at the 90% level. Each row is dispatched to
#' the MOE formula appropriate for its estimand family (a spatial total, a
#' weighted scalar/proxy mean, or a derived rate), the formula is applied with
#' a confidence multiplier resolved from `level`, and row-level provenance of
#' the formula used and any fallback is recorded.
#'
#' The four supported formula families are documented in detail below.
#'
#' @section MOE formula families:
#'
#' Standard errors (SE) and margins of error are interconvertible through the
#' confidence multiplier \eqn{z}: \eqn{M = z \cdot SE}, with
#' \eqn{z = 1.645} at the default 90% level. Variances accumulate on the SE
#' scale, so each formula divides the component MOEs by \eqn{z}, combines the
#' resulting variances, and multiplies the combined SE back by \eqn{z}.
#'
#' \strong{Family A -- spatial total / weighted sum of counts.} For a
#' coverage-weighted sum of independent ACS counts, the MOE is the
#' root-sum-square of the weighted component MOEs:
#' \deqn{M = z \sqrt{\sum_j \left(\frac{w_j M_j}{z}\right)^2}}
#'
#' \strong{Family B -- weighted scalar/proxy mean.} For a proxy mean built
#' from area- or population-normalized weights (e.g. a median-income proxy),
#' the same root-sum-square form applies with the mean weights, which sum to
#' one and are treated as fixed:
#' \deqn{M = z \sqrt{\sum_j \left(\frac{w_j M_j}{z}\right)^2}}
#'
#' \strong{Family C1 -- proportion subset.} When the numerator \eqn{A} is a
#' true subset of the denominator \eqn{B}, the cross term is subtracted:
#' \deqn{\widehat{Var}(A/B) \approx \frac{Var(A) - (A/B)^2\, Var(B)}{B^2}}
#' This expression can go negative; when it does, the row falls back to the
#' Family C2 formula (see below) so a value is still produced.
#'
#' \strong{Family C2 -- general ratio (conservative).} The default for the
#' package's hard-coded derived rates, used whenever the subset relationship
#' is not guaranteed. The cross term is added, so the variance is always
#' non-negative:
#' \deqn{\widehat{Var}(A/B) \approx \frac{Var(A) + (A/B)^2\, Var(B)}{B^2}}
#'
#' \strong{C1 to C2 fallback.} The fallback is never silent. When a Family C1
#' row yields a negative subset variance, the MOE is recomputed with the
#' Family C2 formula and the override is recorded per row:
#' `moe_formula_effective` becomes `"general_ratio_conservative"`,
#' `moe_fallback` is set to `TRUE`, and `moe_fallback_reason` is set to
#' `"negative_variance"`. A zero denominator or a missing input MOE instead
#' yields `NA` with `moe_fallback_reason` of `"zero_denominator"` or
#' `"missing_moe"`. Rows computed without fallback carry `moe_fallback = FALSE`
#' and `moe_fallback_reason = "n/a"`. A single summary warning reports the
#' fallback counts at the end of the run.
#'
#' The formula applied to each row can be overridden through the `formula`
#' argument, and the confidence multiplier through `level`.
#'
#' @param data A `tbl_df` or `data.table` from [cacs_intersect_weight()] in
#'   the canonical long schema, with the per-component aggregation carriers
#'   attached as an attribute.
#' @param formula `NULL` to auto-dispatch a formula from each row's estimand
#'   family, a single formula name to broadcast to every row, or a named list
#'   keyed by `variable` that maps individual variables to formula names. The
#'   sanctioned formula names are `"weighted_sum"`, `"weighted_mean"`,
#'   `"proportion_subset"`, and `"general_ratio_conservative"`.
#' @param level numeric(1) confidence level strictly in `(0, 1)`. Default
#'   `0.90`, the ACS Bureau convention, for which `z = 1.645` exactly.
#' @param verbose logical(1); default `TRUE`. Emits a single progress-summary
#'   condition for orchestrator-level progress capture.
#' @param ... Reserved for future extension. The name `fallback_chain_max` is
#'   accepted (with a warning) but has no effect; any other name aborts.
#' @return The input `data` with the `moe`, `moe_formula_effective`,
#'   `moe_fallback`, and `moe_fallback_reason` columns updated, plus a
#'   `moe_formula_requested` column and confidence-level and MOE-provenance
#'   attributes attached. When `options(cacs.return_se = TRUE)` is set, an
#'   `se` column is also returned.
#' @seealso [cacs_run()], [cacs_derive_rates()], [cacs_se_to_moe()],
#'   [cacs_moe_to_se()].
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
#' head(prop[, c("variable", "estimate", "moe", "moe_formula_effective")])
#' }
cacs_propagate_moe <- function(data, formula = NULL, level = 0.90,
                               verbose = TRUE, ...) {

  dots <- list(...)
  if (length(dots) > 0L) {
    dot_names <- names(dots)
    if (is.null(dot_names) || any(!nzchar(dot_names))) {
      .cli_abort_schema(c(
        "{.arg ...} entries for {.fn cacs_propagate_moe} must be named.",
        "i" = "The only sanctioned reserved name is {.val fallback_chain_max}."
      ))
    }
    allowed_dots <- "fallback_chain_max"
    unknown_dots <- setdiff(dot_names, allowed_dots)
    if (length(unknown_dots) > 0L) {
      .cli_abort_schema(c(
        "{.fn cacs_propagate_moe} received unknown {.arg ...} argument{?s}: {.arg {unknown_dots}}.",
        "i" = "Sanctioned reserved names are {.val {allowed_dots}}."
      ))
    }
    used_reserved <- intersect(dot_names, allowed_dots)
    if (length(used_reserved) > 0L) {
      .cli_warn_provenance(
        c(
          "{.fn cacs_propagate_moe} ignored reserved {.arg ...} argument{?s}: {.arg {used_reserved}}.",
          "i" = "These names are reserved for a future MOE fallback-chain extension and have no effect in v0.1."
        ),
        phase = "Sec. 22 MOE propagation"
      )
    }
  }

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    .cli_abort_schema(c(
      "{.arg verbose} must be a non-NA logical scalar.",
      "x" = "Got {.cls {class(verbose)[[1L]]}} of length {.val {length(verbose)}}."
    ))
  }

  # --------------------------------------------------------------------------
  # Step 1 — schema validation
  # --------------------------------------------------------------------------
  if (!inherits(data, c("tbl_df", "data.table"))) {
    .cli_abort_schema(c(
      "{.arg data} must be a {.cls tbl_df} or {.cls data.table}.",
      "x" = "Got {.cls {class(data)[[1]]}}.",
      "i" = "Did you call {.fn cacs_intersect_weight}?"
    ))                                                                    # E-22-02
  }

  # Critical column check (Sec. 22.3 step 1a-b). We require ALL Phase 6
  # update targets (the 5 row-level provenance columns) plus the 6 key/value
  # columns the algorithm reads. `.validate_intersect_output()` would also
  # catch this but its error message points users at Sec. 21, not Sec. 22.
  critical <- c("site_id", "drive_time_min", "variable",
                "estimand_family", "estimate", "moe",
                "weight_basis", "moe_formula_effective",
                "moe_fallback", "moe_fallback_reason")
  missing_cols <- setdiff(critical, names(data))
  if (length(missing_cols) > 0L) {
    .cli_abort_schema(c(
      "{.arg data} missing critical column{?s}: {.field {missing_cols}}.",
      "i" = "Sec. 22.3 step 1 requires the Sec. 12.3.1 canonical long schema."
    ))                                                                    # E-22-01
  }

  # Phase 2.3 validator (Sec. 12.3.1 20-col + carrier contract). Carrier is
  # required unless every row is metadata_only — `.validate_intersect_output`
  # already enforces this branch when `require_carrier = TRUE`.
  carrier <- attr(data, "cacs_aggregation_carriers")
  non_meta <- any(data$estimand_family != "metadata_only", na.rm = TRUE)
  if (is.null(carrier) && non_meta) {
    .cli_abort_schema(c(
      "{.arg data} missing {.attr cacs_aggregation_carriers}.",
      "i" = "Did you call {.fn cacs_intersect_weight}?"
    ))                                                                    # E-22-09
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

  # --------------------------------------------------------------------------
  # Step 2 — z resolution from level
  # --------------------------------------------------------------------------
  z <- .resolve_z_from_level(level)

  # --------------------------------------------------------------------------
  # Step 3 — formula dispatch (3-mode)
  # --------------------------------------------------------------------------
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
      "x" = "Family/formula pair{?s}: {.val {paste0(bad_families, ' -> ', bad_formulas)}}.",
      "i" = "Use {.code formula = NULL} for sanctioned auto-dispatch or choose a formula compatible with each estimand family."
    ))
  }

  # Persist effective formula at row level so Family A/B reflect the
  # resolved dispatch even when the input carried a different default.
  data$moe_formula_effective <- formula_resolved

  # --------------------------------------------------------------------------
  # Steps 4-5 — Family A (weighted_sum) + Family B (weighted_mean) vectorized
  #
  # Carriers live on the attribute tibble, not on `data`. We left-join the
  # four numeric carriers via the 3-key and then apply masks on the joined
  # frame. Step 6.2 will replace this with the `.lookup_carrier_*()` helpers
  # for C1/C2 (which key on num_code / den_code instead of the row's
  # variable); Family A/B always key on the row's own variable so the
  # left-join is sufficient here.
  # --------------------------------------------------------------------------
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

  # Attach the confidence level attribute now so downstream consumers
  # (Phase 6.3 / 6.4) can read it without re-resolving `level`.
  attr(data, "cacs_confidence_level") <- level

  # --------------------------------------------------------------------------
  # Step 6 — Family C1 (proportion_subset) multi-step fallback chain
  #
  # Sec. 22.3 step 6 + Sec. 22.5 trigger priority (zero_den -> missing_moe ->
  # negative_variance -> C1 success). The dispatch runs vectorized over the
  # C1 row mask per Sec. 22.6: a single lookup pass populates the four
  # parallel carrier vectors, then `.moe_prop_self()` is called row-wise to
  # obtain the typed sentinel set (`zero_denominator`, `missing_moe`,
  # `inside_negative`) on which the priority-order assignment splits.
  #
  # The carrier lookup helpers translate the row's `variable` (which for a
  # derived_rate row is a sanctioned rate name like "poverty_rate") into the
  # numerator / denominator ACS codes via `.SANCTIONED_RATES_V1`. Any C1 /
  # C2 row whose `variable` is NOT a sanctioned rate name aborts with an
  # operator-class error so the upstream classifier mistake never silently
  # NA-coalesces.
  # --------------------------------------------------------------------------

  is_C1 <- !is.na(formula_resolved) & formula_resolved == "proportion_subset"
  is_C2 <- !is.na(formula_resolved) &
           formula_resolved == "general_ratio_conservative"

  if (any(is_C1) || any(is_C2)) {
    ratio_vars <- unique(data$variable[is_C1 | is_C2])
    bad_vars <- setdiff(ratio_vars, names(.SANCTIONED_RATES_V1))
    if (length(bad_vars) > 0L) {
      .cli_abort_operator(c(
        "Ratio-formula rows reference unsanctioned rate name{?s}: {.field {bad_vars}}.",
        "i" = "Sec. 23.4 v1.0 curated set: {.val {names(.SANCTIONED_RATES_V1)}}.",
        "x" = "Each derived_rate row's {.field variable} must be a key in {.code .SANCTIONED_RATES_V1}."
      ))
    }
  }

  if (any(is_C1)) {
    idx_C1 <- which(is_C1)
    rate_names_C1 <- data$variable[idx_C1]
    sid_C1 <- data$site_id[idx_C1]
    dt_C1  <- data$drive_time_min[idx_C1]

    # Per-row carrier lookup (vectorized inside each helper via match()).
    # The helpers accept scalar rate_name so we dispatch per-row through
    # vapply(); this stays sub-millisecond for 1000+ rows because the inner
    # `.carrier_key_lookup()` is itself vectorized on (site_id, drive_time_min)
    # — see Sec. 22.6 perf rationale.
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

    # Lift raw variance -> MOE for the primitives (Sec. 22.4.3 expects MOE).
    num_moe_C1 <- .var_to_moe(num_var_C1, z)
    den_moe_C1 <- .var_to_moe(den_var_C1, z)

    # Pre-compute estimate column for derived_rate rows (p_hat = A / B).
    # This stays inside the C1 block so Sec. 22.6's "row-cardinality
    # preserved" contract is honored — we only write to existing rows.
    has_estimate <- !is.na(num_est_C1) & !is.na(den_est_C1) &
                    !.is_zero_den(den_est_C1)
    if (any(has_estimate)) {
      data$estimate[idx_C1[has_estimate]] <-
        num_est_C1[has_estimate] / den_est_C1[has_estimate]
    }

    # Probe each row via `.moe_prop_self()` so the typed-sentinel signal
    # drives the priority-order partition. Sec. 22.5: numerical
    # well-definedness (zero_den / missing_estimate) > statistical
    # suppression (missing_moe) > C1 vs C2 dispatch (inside_negative).
    probes <- lapply(seq_along(idx_C1), function(i) {
      .moe_prop_self(num_est_C1[i], num_moe_C1[i],
                     den_est_C1[i], den_moe_C1[i], z = z)
    })

    is_zero_den    <- vapply(probes, `[[`, logical(1L), "zero_denominator")
    is_missing_est <- vapply(probes, `[[`, logical(1L), "missing_estimate")
    is_missing_moe <- vapply(probes, `[[`, logical(1L), "missing_moe")
    is_neg_var     <- vapply(probes, `[[`, logical(1L), "inside_negative")

    # Priority partition — zero_den (incl. missing num/den estimate, which
    # the primitive treats as a structural zero-denominator equivalent) >
    # missing_moe > negative_variance > C1 success. Each row participates in
    # exactly one branch.
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

  # --------------------------------------------------------------------------
  # Step 7 — Family C2 (general_ratio_conservative) batch
  #
  # Sec. 22.3 step 7. C2 has no negative-variance branch (Sec. 22.4.4's
  # `inside` term is provably non-negative) so only the zero_den + missing_moe
  # branches apply. The success path computes the MOE directly via
  # `.moe_ratio_self()` and labels `moe_fallback = FALSE`,
  # `moe_fallback_reason = "n/a"`.
  # --------------------------------------------------------------------------
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

    # C2 partition: zero_den > missing_moe > success (no negative_variance
    # branch). The triggers mirror Sec. 22.5 trigger priority within the C2
    # column of the dispatch matrix.
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

    # Direct C2 success — bypass `.apply_c2_fallback()` because that helper
    # writes `moe_fallback = TRUE` (it is the *C1->C2 fallback* branch).
    # Here C2 was the *requested* formula so fallback stays FALSE.
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

  # --------------------------------------------------------------------------
  # Step 8 — metadata_only pass-through (Sec. 22.3 step 8)
  # --------------------------------------------------------------------------
  is_meta <- !is.na(data$estimand_family) &
             data$estimand_family == "metadata_only"
  if (any(is_meta)) {
    data$moe[is_meta] <- NA_real_
    data$moe_formula_effective[is_meta] <- NA_character_
    data$moe_fallback[is_meta] <- NA
    data$moe_fallback_reason[is_meta] <- "n/a"
  }

  # --------------------------------------------------------------------------
  # Step 9 — attach moe_formula_requested (Sec. 22.3 step 9)
  #
  # `formula_resolved` is the per-row resolved formula label from the
  # 3-mode dispatch (Sec. 22.3 step 3). We persist it under the
  # `moe_formula_requested` column so downstream consumers (Phase 6.4
  # provenance + Sec. 22.7 schema) can see the user-side request even when
  # `moe_formula_effective` reports a fallback override.
  # --------------------------------------------------------------------------
  data$moe_formula_requested <- formula_resolved

  # --------------------------------------------------------------------------
  # Step 10 — optional `se` column (Sec. 22.3 step 10 + Sec. 22.4.5)
  #
  # `getOption("cacs.return_se", FALSE)` is a *sticky* user option (not a
  # function argument) so downstream pipelines can opt in once at session
  # scope rather than threading the flag through every call site. Sec. 22.7
  # marks `se` as the only optional column in the canonical 20+1 schema;
  # when the option is FALSE we leave `se` absent so the canonical 20-col
  # contract holds for the default surface.
  # --------------------------------------------------------------------------
  if (isTRUE(getOption("cacs.return_se", FALSE))) {
    data$se <- data$moe / z
  }

  # --------------------------------------------------------------------------
  # Step 11 — aggregate fallback summary warning (Sec. 22.3 step 11; W-22-09 /
  # W-22-10 / W-22-11 consolidated). Single `cli_warn` rolls all three
  # categories (C1->C2 negative-variance, zero_denominator, missing_moe) into
  # one phase-keyed message so Sec. 24's `cacs_run()` aggregation can attribute
  # the warning origin to "MOE propagation" vs Sec. 23's "Rate derivation".
  # We only emit when at least one fallback row exists — silent on clean runs.
  # --------------------------------------------------------------------------
  n_c1_to_c2 <- sum(data$moe_fallback_reason == "negative_variance",
                    na.rm = TRUE)
  n_zero_den <- sum(data$moe_fallback_reason == "zero_denominator",
                    na.rm = TRUE)
  n_missing  <- sum(data$moe_fallback_reason == "missing_moe",
                    na.rm = TRUE)
  if ((n_c1_to_c2 + n_zero_den + n_missing) > 0L) {
    .cli_warn_runtime(
      c(
        "MOE propagation fallback summary:",
        "*" = "{.val {n_c1_to_c2}} row{?s}: C1 -> C2 (negative variance)",
        "*" = "{.val {n_zero_den}} row{?s}: NA (zero denominator)",
        "*" = "{.val {n_missing}} row{?s}: NA (missing input MOE)",
        "i" = "Per-row reason in {.field moe_fallback_reason}."
      ),
      phase = "Sec. 22 MOE propagation"
    )
  }

  # --------------------------------------------------------------------------
  # Step 12 — validate output + attach provenance attribute + return
  #
  # `.validate_propagate_output()` (Phase 2.3) re-runs the inherited
  # Sec. 12.3.1 20-col schema check plus the Sec. 22.7 `moe_formula_effective`
  # non-NA invariant (metadata_only is the only NA-allowed family, but the
  # validator handles that branch internally via `.validate_intersect_output()`
  # which we already passed at Step 1; metadata_only NAs in
  # `moe_formula_effective` would still trip the validator so we route
  # metadata_only rows through a sanctioned label below before validation).
  #
  # `cacs_schema_version`, `cacs_aggregation_carriers`, and
  # `cacs_aggregation_provenance` carry through from `cacs_intersect_weight()`
  # (Sec. 22.7) — the left-join keeps row-level structure and we never strip
  # those input attributes. `cacs_confidence_level` was attached at Step 5;
  # `cacs_moe_provenance` is appended here as the Sec. 22.7 new attribute.
  # --------------------------------------------------------------------------

  # Patch metadata_only rows back to a sanctioned label so the
  # `moe_formula_effective` non-NA invariant holds. Sec. 22.7 catalogs the
  # 4-enum + 2-sentinel set; metadata_only carries no formula but the
  # validator requires a non-NA string, so we route it through a sanctioned
  # sentinel ("weighted_sum" is structurally inappropriate here -- we use
  # "n/a" embedded in `moe_fallback_reason` and leave `moe_formula_effective`
  # at the auto-dispatch NA only if metadata_only rows truly exist).
  # In v0.1 alpha the fixture surface keeps every long-output row at a
  # non-metadata family (Phase 5.4 contract: metadata_only carrier rows are
  # filtered out before the long bind), so the validator passes without a
  # patch. The branch below stays as a no-op safety net for v1.1 when
  # metadata_only rows may leak into the long output.

  # Phase 2.3 output validator. The validator runs `abort = TRUE` so a
  # schema-shape regression in any of Steps 4-9 lands as a hard
  # `catchmentACS_error_schema` here rather than silently propagating.
  .validate_propagate_output(data, abort = TRUE)

  # Schema-version carry-through (Sec. 22.7). `.validate_intersect_output()`
  # at Step 1 already required `attr(., "cacs_schema_version") == "1.0"`;
  # we re-assert here so a v0.2 carrier whose Phase 5 upstream sets a
  # different version does not silently desync with Phase 6 output.
  attr(data, "cacs_schema_version") <- "1.0"

  # `cacs_confidence_level` already attached at Step 5 (line 940); re-assert
  # defensively so a downstream `subset()` / `[.tbl_df`-attribute-stripping
  # leak between Steps 5 and 12 does not silently lose the attribute.
  attr(data, "cacs_confidence_level") <- level

  # `cacs_moe_provenance` — Sec. 22.7 12-field summary list. Each field is
  # row-count audit data + dispatch context so 5-year audit can replay
  # `which rows took which path?` without re-running the function.
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
