# catchmentACS <img src="https://raw.githubusercontent.com/joonho112/catchmentACS/main/man/figures/logo.png" align="right" height="139" alt="catchmentACS hex sticker: nested drive-time isochrone rings over a census-tract mesh" />

[![R-CMD-check](https://github.com/joonho112/catchmentACS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/joonho112/catchmentACS/actions/workflows/R-CMD-check.yaml)
[![regression](https://github.com/joonho112/catchmentACS/actions/workflows/regression.yml/badge.svg)](https://github.com/joonho112/catchmentACS/actions/workflows/regression.yml)
[![test-coverage](https://github.com/joonho112/catchmentACS/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/joonho112/catchmentACS/actions/workflows/test-coverage.yaml)
[![Lifecycle: beta](https://img.shields.io/badge/lifecycle-beta-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#beta)
[![License: MIT](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/joonho112/catchmentACS/blob/main/LICENSE.md)

catchmentACS describes the neighborhoods around a set of sites, such as
pre-kindergarten (Pre-K) classrooms, with data from the American Community
Survey (ACS). For each site
and each drive time, such as 5, 10, and 15 minutes, it finds the area that can
be reached by car within that time, the site's catchment. It then combines the
ACS estimates of the census tracts that overlap the catchment into estimates
for it: how many people live there, how many are below the poverty level, what
share of households receive benefits from the Supplemental Nutrition Assistance
Program (SNAP), and so on. Every estimate comes with a margin of error, the
half-width of its 90 percent confidence interval, and with a record of the
settings and inputs behind it.

For a count, each tract contributes the share of its own area that lies inside
the drive-time area: a tract half inside contributes half of its count. This
assumes that whatever a variable counts is spread evenly over a tract: the
people below the poverty level, and not only the population as a whole. Each rate is
the ratio of two counts computed this way. Medians and per-person values from
three ACS tables (median household income, median home value, and per capita
income) are averaged over the overlapping tracts instead, with weights
proportional to the area each tract shares with the area. Those weights ignore
how many people live in each tract, so such an average can be far from the
median of the area itself. A median or per-person value from any other ACS table
is added up like a count. The articles listed below describe the calculations
and what they assume.

## Installation

When catchmentACS is available on CRAN, `install.packages("catchmentACS")`
installs the released version. The development version is on GitHub:

```r
# install.packages("pak")
pak::pak("joonho112/catchmentACS")
```

`pak::pak()` does not build the articles; they are on the package website,
<https://joonho112.github.io/catchmentACS/articles/>. Building drive-time areas
with the default routing service needs the osrm package, and the package's maps
need the leaflet package. Neither is installed with catchmentACS:

```r
install.packages(c("osrm", "leaflet"))
```

Downloading ACS data needs a free Census API key, which can be requested at
<https://api.census.gov/data/key_signup.html> and saved with:

```r
tidycensus::census_api_key("YOUR_KEY_HERE", install = TRUE)
readRenviron("~/.Renviron")
```

The first line saves the key in `~/.Renviron` for later sessions; the second
makes it usable in the session that is running. A key already saved there needs
`overwrite = TRUE`.

The key is needed only when catchmentACS downloads estimates: when no saved result
of them matches the call, or when `force_refresh = TRUE`. A run that supplies estimates through
`acs =`, like the example below, needs no key.

This product uses the Census Bureau Data API but is not endorsed or certified by
the Census Bureau.

Downloaded estimates and drive-time areas are saved for the rest of the R
session, so a repeated call neither downloads nor routes again. To keep them
between sessions, add this line to the R startup file, for example with
`usethis::edit_r_profile()`:

```r
options(catchmentACS.cache_dir = tools::R_user_dir("catchmentACS", "cache"))
```

Saved results in that folder that go unused for 30 days are deleted.
`?cacs_cache_dir` gives the details.

## An example that runs offline

Everything the example needs comes from three files that ship with the package.
They hold made-up data: 20 sites on a grid over a rectangle around Alabama,
drive-time areas drawn as circles around the sites, and random ACS estimates
for 911 small squares that stand in for census tracts. Given the areas and the
estimates, `cacs_run()` skips the steps that build the areas and download the
estimates, so the example contacts no service and needs no key.

```r
library(catchmentACS)
library(dplyr)
library(sf)  # needed to subset the bundled sf objects with [

sites <- readRDS(system.file(
  "extdata", "legacy_2025_sites.rds", package = "catchmentACS"
))
iso <- readRDS(system.file(
  "extdata", "legacy_2025_isochrones.rds", package = "catchmentACS"
))
acs <- readRDS(system.file(
  "extdata", "sample_alabama_subset.rds", package = "catchmentACS"
))

one_site <- "AL_SITE_07"

result <- cacs_run(
  sites                  = sites[sites$site_id == one_site, , drop = FALSE],
  state                  = "AL",
  precomputed_isochrones = iso[iso$site_id == one_site, , drop = FALSE],
  acs                    = acs,
  verbose                = FALSE
)

tibble::as_tibble(result) |>
  filter(variable %in% names(cacs_acs_default_rates)) |>
  select(site_id, drive_time_min, variable, estimate, moe)
```

The call returns one row for each drive time and rate, with an estimate and its
margin of error, and the same for each ACS variable in `acs`. Down the column
for this site the poverty rate is 0.62 for the 5-minute area and 0.36 for the
15-minute area, and the share of households receiving SNAP benefits is 0.04 and
0.11. These numbers describe no real place. The bundled ACS values are random,
and the made-up squares are spaced apart rather than tiling the state, so the
5-minute area overlaps one square and the two wider areas overlap three: the
change down the column is the change from one square's values to an average of
three. The file ships with the package, so every reader gets the same numbers.
For some other sites a rate has no value, because one made-up estimate in
twenty is missing; the run then warns and leaves `NA` in `estimate` and `moe`.
`AL_SITE_07` has no such row.

For a run on your own sites, the call is the same without
`precomputed_isochrones =` and `acs =`. The sites go in as a data frame with
the columns `site_id`, `lon`, and `lat`, or as an sf object of points with a
`site_id` column. The run then needs a routing service, a Census API key, and
an internet connection.
[Getting started with catchmentACS](https://joonho112.github.io/catchmentACS/articles/getting-started.html)
goes through the result column by column, and
[Routing services, API keys, and offline use](https://joonho112.github.io/catchmentACS/articles/providers.html)
covers the routing services and their keys.

## What version 0.6.0 does

- Area weighting of the tracts that overlap each drive-time area. It is the
  only weighting method in this version.
- ACS 5-year estimates for one state at a time, with end years from 2009
  through 2024. The state has to be in the contiguous United States or the
  District of Columbia; sites or tracts outside that give an error.
- Drive-time areas from the Open Source Routing Machine (OSRM) or from
  openrouteservice, which needs an API key: the `ORS_API_KEY` environment
  variable, `ors_api_key =` in `cacs_isochrone()`, or
  `iso_args = list(ors_api_key = ...)` in `cacs_run()`. The openrouteservice
  route has been checked only with simulated responses from openrouteservice.
- Five rates, each with a margin of error at the 90 percent level: the poverty
  rate, the share of households receiving SNAP benefits, the share of households
  with Supplemental Security Income, the unemployment rate, and labor force
  participation.
- A cache for the downloaded estimates, the drive-time areas, and the weighted
  results, kept for the R session or in a folder you choose; checks on ACS data
  and drive-time areas from other sources;
  summaries of a result; and leaflet maps of one site's areas, tracts,
  estimates, and rates.

Not implemented yet: Mapbox and r5r as routing services, weighting by
population, rates other than the five above, and ACS pulls of 1-year estimates,
block groups, counties, or several states at once. Asking for any of these
stops the run with an error naming what is missing. Weighting by population, a
different `rates` list, and several states in `state =` give the error no
matter what else is supplied. The other five give it only when the step that
would use them runs. With `precomputed_isochrones =` supplied the routing step
does not run, so `provider = "mapbox"` is accepted and left unused; with
`acs =` supplied the download step does not run, so
`acs_args = list(survey = "acs1")` is too.

## Articles

The articles below are on the package website. `vignette("name", package =
"catchmentACS")` opens one locally when the installed package includes the
articles. A version installed from CRAN includes them. A version installed from
GitHub includes them only when they are built during installation, as with
`remotes::install_github("joonho112/catchmentACS", build_vignettes = TRUE)`, or
with `devtools::install(build_vignettes = TRUE)` from a local copy.

### Using the package

| Article | What it covers |
|---------|----------------|
| [Getting started with catchmentACS](https://joonho112.github.io/catchmentACS/articles/getting-started.html) | A first run on the example data, and how to read the result. |
| [A worked example with Alabama Pre-K sites](https://joonho112.github.io/catchmentACS/articles/alabama-tutorial.html) | The five steps one at a time, then tables for a report. |
| [Routing services, API keys, and offline use](https://joonho112.github.io/catchmentACS/articles/providers.html) | Which services are implemented, what keys they need, and how to run without a network. |
| [Mapping each step for one Birmingham site](https://joonho112.github.io/catchmentACS/articles/visual-walkthrough.html) | Leaflet maps of the drive-time areas, the tracts, the weighted estimates, and the rates. |

### Methods

| Article | What it covers |
|---------|----------------|
| [How the estimates are computed](https://joonho112.github.io/catchmentACS/articles/methodology.html) | The calculation from tracts to catchment estimates, and its assumptions. |
| [Area weighting of tract estimates](https://joonho112.github.io/catchmentACS/articles/theory-spatial-aggregation.html) | The two weights, and when an area-weighted average stands in for a median. |
| [Margins of error for combined estimates](https://joonho112.github.io/catchmentACS/articles/theory-moe-propagation.html) | How the margins of error of the tracts combine, and what the formulas leave out. |
| [Rates and their margins of error](https://joonho112.github.io/catchmentACS/articles/theory-derived-rates.html) | The five rate definitions and the two formulas for the margin of error of a rate. |

### Updating from older versions

| Article | What it covers |
|---------|----------------|
| [Updating code written for versions 0.1 and 0.2](https://joonho112.github.io/catchmentACS/articles/porting-v01-to-v03.html) | The ten changes that reach code written for 0.1 or 0.2. |
| [Updating code written for version 0.3](https://joonho112.github.io/catchmentACS/articles/porting-v03-to-v04.html) | What changed in 0.4. |
| [Updating code written for version 0.4](https://joonho112.github.io/catchmentACS/articles/porting-v04-to-v05.html) | What changed in 0.5.0, including the corrected medians and per-person values. |

Help pages for the functions are on the website as well. `?cacs_run` describes
every argument of the one call that runs the whole calculation.

## Citation

catchmentACS is written by JoonHo Lee
([ORCID: 0009-0006-4019-8703](https://orcid.org/0009-0006-4019-8703)).
`citation("catchmentACS")` gives the citation for the installed version, and
`toBibtex(citation("catchmentACS"))` gives it as a BibTeX entry.

## License

MIT © [JoonHo Lee](https://github.com/joonho112)

## Bug reports

Bug reports and feature requests are welcome at
<https://github.com/joonho112/catchmentACS/issues>.
