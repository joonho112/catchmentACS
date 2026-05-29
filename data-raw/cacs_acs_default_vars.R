# ----------------------------------------------------------------------
# Generate cacs_acs_default_vars (v0.3 dev)
#
# §17.2 + v0.3 Step 1.3 — 14 source ACS variables, named with
# human-readable aliases.
#
# These 14 source variables produce the 9 v0.1 output indicators
# (§23 .SANCTIONED_RATES_V1 + Family A/B context vars) via
# numerator / denominator / direct-estimate combinations. See §17.2
# Table for the mapping. The vector is consumed by cacs_acs_prefetch()
# when variables = NULL (§6.3).
#
# v0.3 adds B19056_002 so Phase 4 can correct ssi_rate from the
# B19056 table-specific numerator while preserving B19056_001 as the
# B19056 universe/denominator source row.
#
# Run: Rscript data-raw/cacs_acs_default_vars.R
# ----------------------------------------------------------------------

library(usethis)

cacs_acs_default_vars <- c(
  total_pop   = "B01003_001",  # total population — context, denominator candidate
  pov_denom   = "B17001_001",  # poverty universe — poverty denominator
  pov_below   = "B17001_002",  # below poverty — poverty numerator
  med_hh_inc  = "B19013_001",  # median household income — scalar/proxy estimate
  per_cap_inc = "B19301_001",  # per-capita income — scalar/proxy estimate
  total_hh    = "B11001_001",  # total households — household denominator
  snap_denom  = "B22003_001",  # SNAP universe — SNAP denominator
  snap_recv   = "B22003_002",  # SNAP households — SNAP numerator
  ssi_total   = "B19056_001",  # SSI table universe — denominator/context
  ssi_hh      = "B19056_002",  # households with SSI income — SSI numerator
  emp_denom   = "B23025_001",  # employment universe — employment denominator
  labor_force = "B23025_002",  # labor force — employment context
  civ_labor   = "B23025_003",  # civilian labor force — unemployment denominator
  unemployed  = "B23025_005"   # unemployed — unemployment numerator
)

# Generation-time invariants — catch transcription errors before binary write.
stopifnot(
  length(cacs_acs_default_vars) == 14L,
  !any(duplicated(cacs_acs_default_vars)),
  !any(duplicated(names(cacs_acs_default_vars))),
  all(grepl("^B[0-9]{5}_[0-9]{3}$", cacs_acs_default_vars))
)

usethis::use_data(cacs_acs_default_vars, compress = "xz", overwrite = TRUE)
