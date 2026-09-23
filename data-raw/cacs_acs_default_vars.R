# data-raw/cacs_acs_default_vars.R - writes data/cacs_acs_default_vars.rda, the
# 14 ACS variable codes that cacs_acs_prefetch() downloads when `variables` is
# NULL, each with a short name.
#
# The 14 codes are the numerators and denominators of the five rates that
# cacs_derive_rates() computes, plus two counts, a median, and a per-person
# value that the package reports on their own. The help page
# ?cacs_acs_default_rates gives the numerator and the denominator of each rate.
#
# Run: Rscript data-raw/cacs_acs_default_vars.R

library(usethis)

cacs_acs_default_vars <- c(
  total_pop   = "B01003_001",  # total population: context, denominator candidate
  pov_denom   = "B17001_001",  # poverty universe: poverty denominator
  pov_below   = "B17001_002",  # below poverty: poverty numerator
  med_hh_inc  = "B19013_001",  # median household income: scalar/proxy estimate
  per_cap_inc = "B19301_001",  # per-capita income: scalar/proxy estimate
  total_hh    = "B11001_001",  # total households: household denominator
  snap_denom  = "B22003_001",  # SNAP universe: SNAP denominator
  snap_recv   = "B22003_002",  # SNAP households: SNAP numerator
  ssi_total   = "B19056_001",  # SSI table universe: denominator/context
  ssi_hh      = "B19056_002",  # households with SSI income: SSI numerator
  emp_denom   = "B23025_001",  # employment universe: employment denominator
  labor_force = "B23025_002",  # labor force: employment context
  civ_labor   = "B23025_003",  # civilian labor force: unemployment denominator
  unemployed  = "B23025_005"   # unemployed: unemployment numerator
)

# Checks before the data are written: 14 different codes, 14 different names,
# and every code in the form Bnnnnn_nnn.
stopifnot(
  length(cacs_acs_default_vars) == 14L,
  !any(duplicated(cacs_acs_default_vars)),
  !any(duplicated(names(cacs_acs_default_vars))),
  all(grepl("^B[0-9]{5}_[0-9]{3}$", cacs_acs_default_vars))
)

usethis::use_data(cacs_acs_default_vars, compress = "xz", overwrite = TRUE)
