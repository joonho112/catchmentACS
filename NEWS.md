# catchmentACS 0.6.0

## Changed defaults

* `cacs_isochrone()`, `cacs_acs_prefetch()`, and `cacs_intersect_weight()`,
  and so `cacs_run()`, now save their results in a folder inside the
  temporary folder of the R session, which R deletes when the session ends.
  Versions 0.5.1 and earlier saved them in the user cache folder of the
  operating system and kept them until they were deleted. To keep saved
  results between sessions, add this line to the R startup file:
  `options(catchmentACS.cache_dir = tools::R_user_dir("catchmentACS", "cache"))`.
  A cache folder outside the temporary folder is tidied once per session:
  results not used for 30 days are deleted, and
  `options(catchmentACS.cache_max_age_days = )` sets another number of days.
  The package no longer reads or deletes the folder used by earlier
  versions; `?cacs_cache_dir` gives its location, and it can be deleted by
  hand. The packages memoise and rappdirs are no longer needed.
* `cacs_set_cache(scope = "global")` no longer writes to the R startup file.
  It sets the option for the session and shows the line to add to the
  startup file. `confirm` is kept so that code written for earlier versions
  still runs, but it is not used.
* `cacs_cache_dir()` no longer creates the cache folder unless
  `create = TRUE`.
* `as_tibble()` on a `cacs_run()` result keeps the rows in their order by
  default. `rate_first = TRUE` puts the rate rows first within each site and
  drive time, and `options(catchmentACS.rate_first_default = TRUE)` does the
  same for conversions called from your own code, while conversions made
  inside other packages keep the order. As a result, `dplyr::semi_join()`,
  `dplyr::anti_join()`, and summaries after `dplyr::rowwise()` return the
  right rows.
* The map functions (`cacs_plot_site_isochrone()`,
  `cacs_plot_site_intersection()`, `cacs_plot_site_weighted()`,
  `cacs_plot_site_rates()`, and `cacs_plot_site_pipeline()`) now draw the
  standard OpenStreetMap map by default (`tiles = "OpenStreetMap"`). The
  CARTO tiles used before need an API key, which these functions do not
  send, and they showed a notice asking for one. Any other provider in
  `leaflet::providers` can still be chosen with `tiles`.

## Bug fixes

* `cacs_intersect_weight()` and `cacs_run(acs = )` now treat the Census
  Bureau's annotation codes (-222222222, -333333333, -555555555, -666666666,
  -888888888, and -999999999) in the estimates and margins of error of ACS
  data given to them as missing, with a warning that names the codes found;
  they used to enter the sums. A code in place of a margin of error therefore
  makes that margin of error `NA`, and a rate that uses the variable `NA` as
  well. Data from `cacs_acs_prefetch()` and the bundled data give the same
  results as before.
* `cacs_acs_validate()`, `cacs_intersect_weight()`, `cacs_run(acs = )`, and
  `cacs_acs_prefetch()` now stop with an error when a tract has two rows for
  the same variable; such a row used to be added twice.
* `cacs_intersect_weight()`, and `cacs_run()` through it, now recognize saved
  results by the drive-time areas themselves, so a subset or an edited copy
  of a `cacs_isochrone()` result no longer returns rows saved for another
  subset. The key also no longer depends on how the row names of the areas
  are stored. Results saved by earlier versions are computed again once.
* `cacs_isochrone()` now stops at the first site that gets HTTP status 429,
  401, or 403 and sends no request for the remaining sites; before, it tried
  every site first.
* `cacs_isochrone()` no longer reads a port number in a connection error,
  such as "port 443", as an HTTP status. A connection failure or a timeout is
  therefore tried again, up to three times, as the help page describes;
  before, the site was not tried again.
* `cacs_isochrone()` no longer saves a result in which routing failed for a
  site in a way that may be temporary (no answer, a timeout, or an HTTP 5xx
  status), and it does not use such a result saved by an earlier version, so
  a temporary failure is not returned again from the cache. A site that the
  routing service rejects with another HTTP 4xx status is saved with the
  result, and a warning says so when the saved result is used.
* A server set with `options(osrm.server = )` now stays in use over repeated
  `cacs_isochrone()` calls; before, loading the osrm package during the first
  call replaced it with the public demo server for later calls, and the
  record kept with the result named that server.
* `cacs_acs_prefetch()`, and so `cacs_run()`, no longer stops with "package
  lwgeom required" when spherical geometry is turned off with
  `sf::sf_use_s2(FALSE)`: the check for tracts without area now measures the
  tracts in EPSG:5070 in that case, so the lwgeom package is not needed.
* `cacs_acs_prefetch()` now reads back a result it saved together with its
  message about removed water tracts also when the package is installed with
  its sources kept (for example with `R_KEEP_PKG_SOURCE=yes`). The saved
  message kept a reference to the package source that changed when the file
  was read, so the saved copy never matched its checksum file and each call
  downloaded the data again.
* The map functions now find the bundled `cacs_alabama_sites` table when they
  are called as `catchmentACS::` without `library(catchmentACS)`, and an
  object of the same name in the global environment no longer takes its
  place.
* `cacs_derive_rates()` now gives an error for an argument passed through
  `...`, such as `level = 0.95`, which it used to ignore; the confidence level
  is set in `cacs_propagate_moe()`.
* `cacs_intersect_weight()`, and so `cacs_run()`, now stops with an error of
  class `catchmentACS_error_geometry` when the geometry of no row of the
  drive-time areas or of the tracts can be repaired; before, an error in
  writing the message took its place.
* `cacs_summary_as_markdown()` now names the confidence level of the result
  in the caption of `rates_per_site_moe`, which always said 90%; `summary()`
  records the level on that table.

## Other changes

* `cacs_intersect_weight()` has a new argument `cache_dir`, as
  `cacs_isochrone()` and `cacs_acs_prefetch()` have, and
  `cacs_run(cache_dir = )` now applies to all three steps.
* `cacs_clear_cache()` and `cacs_cache_status()` now handle only the files
  named the way the package names its saved files, and they include the
  GeoPackage files written by `cacs_acs_prefetch(write_gpkg = TRUE)`.
  `cacs_clear_cache(confirm = TRUE)` in a non-interactive session now says
  that it deleted nothing because it could not ask for confirmation.
* The `phase` values that `cacs_capture_conditions()` returns for conditions
  from `cacs_run()`, `cacs_derive_rates()`, and `cacs_propagate_moe()` are now
  `"run"`, `"rates"`, and `"moe"`; before, they were longer labels that began
  with a section number of an internal design document. Several warnings and
  messages that had no `phase` now have the name of their step, such as
  `"isochrone"` or `"intersect"`.
* Error, warning, and progress messages were rewritten to say what happened
  and what to do. They no longer refer to development versions, to sections
  of an internal design document, or to functions and arguments that do not
  exist (such as `cacs_derive_rates_custom()` and `crs_area`). The messages
  about the OSRM `res` value used when `res` is not given say which value was
  used and how to choose another. The condition classes are unchanged.
* `cacs_validate_iso()` gives plainer `fix_hint` and `expected` text for ring
  topology, provider values, and missing columns. The `check` values and the
  columns of the table are unchanged.
* The suggested packages mapboxapi, r5r, httptest2, covr, lintr, pkgdown, and
  roxygen2 were dropped; none of them was used by the package.

## Documentation

* The vignettes are R Markdown documents built with knitr instead of Quarto
  documents, so building the package no longer needs the Quarto command-line
  tool. Their formulas are written as MathML: in the vignettes opened with
  `vignette()`, longer formulas were shown as TeX code before and are now
  displayed as formulas, also without an internet connection.
* Every exported topic has an example that runs; `?cacs_isochrone` and
  `?cacs_acs_prefetch` show results of those functions stored in the package.
* The help pages describe the changes above and, where they apply, the
  limitations below.

## Known limitations

* `cacs_isochrone()` and `cacs_run()` do not build drive-time areas with
  `provider = "mapbox"` or `provider = "r5r"`, which are not implemented and
  give an error, and `cacs_derive_rates()` accepts only the five built-in
  rates.
* `cacs_isochrone(provider = "ors")` has been checked only with simulated
  responses from openrouteservice.
* `cacs_acs_prefetch()` downloads American Community Survey (ACS) 5-year
  estimates for the census tracts of one state per call.
* `cacs_intersect_weight()` and `cacs_run()` decide how to combine a variable
  from its ACS code alone. Codes in tables B19013 and B25077 are treated as
  medians, codes in table B19301 as per-person values, and every other code
  made of "B", five digits, an underscore, and three digits as a count, which
  is added up over the tracts. A median or per-person value with a code of
  that form from any other table therefore comes out as a sum of tract
  values, with `weight_basis = "coverage"` and no warning.
* A rate from `cacs_derive_rates()` or `cacs_run()` divides the total of its
  numerator by the total of its denominator, even when the two totals come
  from different tracts. This happens when the ACS data have a tract's row for
  one of the two codes but not for the other, for example after rows with a
  missing estimate were removed. The rate can then be far from the share for
  the area, even above 1, without a warning; `n_tracts_num` and
  `n_tracts_den` then differ, unless the two codes lack the same number of
  tracts.
* When the ACS data have no row for a tract and a variable,
  `cacs_intersect_weight()` and `cacs_run()` leave that tract out of the
  variable's total or average without a warning, so a total comes out too
  small, and `cacs_acs_validate()` does not report the missing row.
  `n_tracts` on that variable's row is then smaller than on the rows of the
  variables that have the tract.
* The part of a drive-time area that has no tracts in the ACS data, such as
  the part across a state line, adds nothing: counts come out too small, and
  rates and medians come from the remaining tracts. The only warning compares
  the bounding box of all the drive-time areas with the bounding box of all
  the tracts, so it is not given when the uncovered part lies inside that box
  or reaches less than 0.05 degrees (about 5 km) beyond it.
* The drive-time areas in the example file `legacy_2025_isochrones.rds` are
  circles with a radius of 1 km per minute of driving, not areas computed by
  a routing service, yet their routing columns hold fixed values such as
  `provider = "osrm"`, `routing_engine_version = "OSRM 5.27.1"`, and
  `osm_snapshot_status = "explicit"`. Results computed from them therefore
  name OSRM as the provider, in their columns and in the output of `print()`
  and `cacs_describe()`.

# catchmentACS 0.5.1

* The help pages, the articles, NEWS, the README, the package description in
  `DESCRIPTION`, and the description of the data files installed with the
  package are revised to use plain language; function behavior is unchanged.
  The articles have new titles, and the package website groups them as "Using
  the package", "Methods", and "Updating from older versions".

# catchmentACS 0.5.0

## Bug fixes

* `cacs_intersect_weight()` now computes the weights that average medians and
  per-person values (the area shares) separately for each variable.
  Previously, they were computed over all the variables of a site and drive
  time together, so these estimates and their margins of error were too small.
  When every tract has a row for each variable, they were divided by the
  number of variables in the call. They now lie within the range of the tract
  values. Counts and the five built-in rates, which use coverage weights, were
  not affected.

## Other changes

* `cacs_run()` now fills the `isochrone` column of `output = "list_column"`
  and `output = "both"` results also when it builds the drive-time areas
  itself. In 0.4.0 and 0.4.5 the column was filled only when the areas were
  supplied through `precomputed_isochrones`.
* `cacs_isochrone()` now accepts sites as a base data frame, as well as a
  tibble or an sf object of points.
* `cacs_acs_prefetch()` now accepts only whole-number years from 2009 to
  2024, and a year with a fraction gives an error instead of being
  truncated. It now looks for a saved result of the data in the cache before it
  sends any request, and without `CENSUS_API_KEY` a download stops with an
  error before any request is sent. Copies saved by earlier versions are not
  read, so the first call after the upgrade downloads the data again and needs
  the key.
* `cacs_run(weight_method = "population")` now stops with an error before
  any download, routing request, or spatial calculation. Area weighting is
  the only weighting method implemented.
* `cacs_intersect_weight()` now finds a saved result in the cache faster,
  because it computes the cache key from the tract boundaries more quickly.
  The new key does not change the results. Results saved by earlier versions
  are not found and are computed again once.

## Documentation

* The help pages, the README, and the articles were rewritten in English.
  New articles describe the methods:
  `vignette("theory-spatial-aggregation", package = "catchmentACS")`,
  `vignette("theory-moe-propagation", package = "catchmentACS")`, and
  `vignette("theory-derived-rates", package = "catchmentACS")`.
  Three more are new as well:
  `vignette("methodology", package = "catchmentACS")` gives the whole
  calculation in one place, `vignette("alabama-tutorial", package =
  "catchmentACS")` runs the five steps one at a time, and
  `vignette("providers", package = "catchmentACS")` covers the routing
  services, the API keys, and running without a network.
  `vignette("porting-v04-to-v05", package = "catchmentACS")` describes the
  changes for code written for 0.4.0 or 0.4.5.
* `citation("catchmentACS")` gives a citation for the package. The source
  code is at <https://github.com/joonho112/catchmentACS> and the
  documentation site at <https://joonho112.github.io/catchmentACS/>.

## Known limitations

* `cacs_isochrone()` and `cacs_run()` do not build drive-time areas with
  `provider = "mapbox"` or `provider = "r5r"`, which are not implemented and
  give an error, and `cacs_derive_rates()` accepts only the five built-in
  rates.
* `cacs_isochrone(provider = "ors")` has been checked only with simulated
  responses from openrouteservice.
* `cacs_acs_prefetch()` downloads American Community Survey (ACS) 5-year
  estimates for the census tracts of one state per call.
* `dplyr::semi_join()`, `dplyr::anti_join()`, and `dplyr::summarise()` or
  `dplyr::reframe()` after `dplyr::rowwise()` with grouping variables give
  wrong results for a `cacs_run()` result, without a warning, unless
  `options(catchmentACS.rate_first_default = FALSE)` is set (see
  `?as_tibble.cacs_run_result`).

# catchmentACS 0.4.5

* `cacs_validate_osrm_endpoint()` now sends its request to the path that
  the public Open Source Routing Machine (OSRM) demo server
  `routing.openstreetmap.de` expects, which starts with the profile (for
  example `/routed-car/route/v1/driving/...`); other servers keep the path
  `/route/v1/driving/...`. It also returns the HTTP status of an error
  response in `http_status` instead of treating the response as a failed
  request.
* `cacs_isochrone()` works with version 5 of the osrm package and keeps the
  areas and the number of requests of each `res` value unchanged. The osrm
  package may then show the message "'res' is deprecated, use 'n' instead"
  for each site, and the `res` value is still used.
* `cacs_acs_prefetch()` no longer passes the deprecated argument
  `cache = TRUE` to `tidycensus::load_variables()`.
* `print()` for `cacs_run()` results writes its header lines as messages,
  which `capture.output(print(x))` does not capture. `?print.cacs_run_result`
  now says so and shows `capture.output(print(x), type = "message")`.

# catchmentACS 0.4.0

## Changed defaults

* `as_tibble()` for `cacs_run()` results now puts the rows of the five rates
  before the ACS variables within each site and drive time. Code that relies
  on row positions gets a different order; `rate_first = FALSE` or
  `options(catchmentACS.rate_first_default = FALSE)` keeps the order of the
  result. A message of class `catchmentACS_message_rate_first_changed`
  reports the change once per session.
* `cacs_isochrone()` with the public OSRM demo server (`osrm_mode = "demo"`,
  the default) now uses `res = 30L` when `res` is not given, instead of
  `70L`. Fewer requests are sent, so the server is less likely to refuse
  them with HTTP status 429 (too many requests). A message of class
  `catchmentACS_message_demo_budget_protected` reports this once per
  session. `options(catchmentACS.osrm_demo_budget_protect = FALSE)` restores
  `70L`, which `osrm_mode = "docker"` keeps, and a `res` that is given is
  used as it is.

## New features

* `summary()` for `cacs_run()` results gains the elements `rates_per_site`
  and `rates_per_site_moe`: tables of the five rates with one row for each
  site and drive time, the second with margins of error. The new argument
  `breakdown` (`"cross_site"`, the default, `"per_site"`, or `"both"`)
  chooses the tables that are printed.
* `print()` for `cacs_run()` results now shows all five rates, with the mean
  and standard deviation of each over all site and drive-time pairs ("Top 5
  rates (cross-site mean +/- sd)"), instead of the top three. It also shows a
  "Rates per site" table when the result has at most
  `getOption("catchmentACS.summary_per_site_max")` sites (12 by default).
* `cacs_summary_as_markdown()` recognizes the per-site rate tables and
  formats them with the columns in a fixed order and a caption that says
  whether the cells include margins of error.
* `cacs_run(output = "list_column")` now fills the `isochrone` column with
  the drive-time area of each site and drive time when the areas are
  supplied through `precomputed_isochrones`; in 0.3.0 the cells were
  `NULL`. A message of class `catchmentACS_message_listcol_iso_filled`
  reports the fill.
* `cacs_capture_conditions()` gains the argument `return_value`. With
  `"both"`, it returns a list of the value of the expression (`result`) and
  the table of captured conditions (`conditions`), so the value can be kept
  without assigning it with `<<-`. The default, `"conditions"`, returns the
  table as before, and `options(catchmentACS.capture_return_value = "both")`
  changes the default.
* `cacs_validate_osrm_endpoint()` is a new function that sends one small
  request to an OSRM server and returns a one-row tibble with the columns
  `endpoint`, `quota_ok`, `response_ms`, and `http_status`. When the server
  responds with HTTP status 429, it gives a warning of class
  `catchmentACS_warning_provider_quota_exhausted`.
* `cacs_isochrone()` now suggests three alternatives in its error for HTTP
  status 429: a lower `res` (such as `iso_args = list(res = 30L)`),
  `osrm_mode = "docker"`, or another routing service.

## Bug fixes

* `summary()` for `cacs_run()` results now matches the values in each row of
  `rates_breakdown` to the rate named in its `variable` column. In 0.2.0 and
  0.3.0 each row showed the mean, standard deviation, and number of missing
  values of the next rate in alphabetical order, so `rates_breakdown` values
  computed with those versions should be recomputed. `dplyr::group_by()` on a
  `cacs_run()` result now uses the new method `group_by.cacs_run_result()`,
  which removes the class `cacs_run_result` before grouping.

## Documentation

* `vignette("porting-v03-to-v04", package = "catchmentACS")` describes the
  changes for code written for 0.3.0.

# catchmentACS 0.3.0

## Breaking changes

* `cacs_intersect_weight()` and `cacs_run()` now accept only cumulative
  drive-time areas, in which each area contains the areas of the shorter
  drive times. Bands between two drive times (for example 5 to 10 minutes)
  give an error of class `catchmentACS_error_annulus_input`, and the new
  function `cacs_rings_to_cumulative()` converts them. `cacs_isochrone()`
  and `cacs_run()` record this in a column `ring_topology`, always
  `"cumulative"`.
* `cacs_isochrone()` with OSRM joins the bands that the osrm package returns
  for several drive times into cumulative areas, so the area for each drive
  time contains the shorter ones.
* `cacs_isochrone()` with OSRM now uses `res = 70L` when `res` is not given
  (the effective value in 0.2.0 was 50) and reports this once per session
  with a message of class `catchmentACS_message_res_default_changed`.
  `iso_args = list(res = 50L)` gives the resolution of 0.2.0, and a `res`
  that is given produces no message.

## Bug fixes

* `cacs_derive_rates()` now computes `ssi_rate` as
  `B19056_002 / B19056_001`, the share of households with Supplemental
  Security Income among the households of table B19056. Previously, it divided
  `B19056_001` by `B11001_001`, two counts of all households.
  `cacs_acs_default_vars` gains `B19056_002` (`ssi_hh`), so it now includes
  the numerator and the denominator of every built-in rate.
* `cacs_derive_rates()` gives a warning of class
  `catchmentACS_warning_carrier_missing` when the numerator or the
  denominator of a rate is missing from its input, and sets only that rate
  to `NA`.

## Cache

* `cacs_acs_prefetch()` keeps ACS data saved in test mode (for example while
  testthat runs) in the cache subfolder `acs_test`, apart from other saved
  data in `acs`, and outside test mode it does not read `acs_test`.
  Previously, data saved by tests could later be read as real ACS data.
* `cacs_intersect_weight()` now builds its cache key from the content of
  `acs_sf`, so a saved result is not reused after the ACS estimates or tract
  boundaries change.
* `cacs_isochrone()`, `cacs_acs_prefetch()`, and `cacs_intersect_weight()`
  now save a checksum file (`.fingerprint`, SHA-256) next to each saved
  result. A saved result whose checksum file is missing or damaged, whose
  file cannot be read, or whose checksum does not match is deleted and
  computed again, so results saved by 0.2.0 are computed again once.
* `cacs_set_cache()` is a new function that turns the cache off or on for
  the session or, with `scope = "global"`, also for later sessions.
  `cacs_get_cache_state()` is a new function that shows the cache settings
  and, in `counters`, the numbers of hits and misses for each kind of saved
  result, which `cacs_clear_cache()` resets.
* `cacs_acs_prefetch()` gives a warning of class
  `catchmentACS_warning_cache_stale_suspect` when ACS data read from the
  cache have fewer rows than expected, with steps to clear the cache. The
  option `catchmentACS.stale_threshold_rows` sets the number of rows (1000
  by default; 0 turns the check off), and an option such as
  `catchmentACS.stale_threshold_WY` sets it for one state.

## Messages and warnings

* `?catchmentACS-conditions` is a new help page that lists the condition
  classes of the package. Messages and warnings are now given through rlang
  with these classes, so code can select them by class.
  `cacs_capture_conditions()` is a new function that runs an expression and
  returns a table of the package's messages and warnings with the columns
  `class`, `message`, `phase`, `timestamp`, and `call`.
* `cacs_acs_prefetch()` now gives the message about removed water tracts
  (class `catchmentACS_message_water_tract_filter`) also in non-interactive
  sessions such as `Rscript`, and repeats it when ACS data saved by 0.3.0 or
  later are read from the cache.
* `cacs_run(verbose = TRUE)` now passes `verbose` to all five steps, and
  each step reports a progress summary. Progress messages (classes
  `catchmentACS_message_progress_tick` and
  `catchmentACS_message_progress_summary`) now appear under `Rscript` too,
  and the option `catchmentACS.progress_throttle` thins the progress lines
  of large batches, always keeping the last one.
* `cacs_intersect_weight()` and `cacs_run()` also give the older classes
  `cacs_error_annulus_input` and `cacs_error_schema` to their errors for
  drive-time bands, so code written for those classes still works.

## Routing with OSRM

* `cacs_isochrone()` records an estimate of the requests that the osrm
  package sends for each site (`osrm_request_budget` in the attribute
  `cacs_isochrone_provenance`).
* `cacs_isochrone(osrm_mode = "docker")` sends its requests to a local OSRM
  server, `http://0.0.0.0:5000/` unless another address is given, and gives
  a message of class `catchmentACS_message_perf_fix_applied` that there is
  no wait between requests. The OSRM server and profile are now part of the
  cache key.

## New functions and other changes

* `cacs_describe()` is a new function that prints a description of how a
  result was produced, for the result of `cacs_run()` or of one of its
  steps, without changing the object.
* `cacs_validate_iso()` is a new function that checks a table of drive-time
  areas and lists the problems it finds, each of which would make
  `cacs_intersect_weight()` or `cacs_run()` stop with an error. `cacs_run()`
  now checks the columns of `precomputed_isochrones` before it uses them,
  and several error messages about input tables now suggest a fix.
* `cacs_summary_as_markdown()` is a new function that formats the tables of
  `summary()` for a `cacs_run()` result as Markdown pipe tables.
* `cacs_intersect_weight(keep_tract_audit = TRUE)` adds the attribute
  `cacs_tract_audit`, a table of the tracts in each drive-time area with the
  columns `site_id`, `drive_time_min`, `GEOID`, `area_wt`, `int_area_m2`,
  and `tract_area_m2`.
* `cacs_derive_rates()` compares each rate with a fixed range when
  `options(catchmentACS.audit_rates = TRUE)` is set, and gives a warning of
  class `catchmentACS_warning_rate_out_of_range` for rates outside it. The
  check is off by default and does not change the rates.
* `cacs_plot_site_rates()` and `cacs_plot_site_pipeline()` now accept `lat`
  and `lon` as an alternative to `site_id`. They map the site in `sites_df`
  nearest to the point, with a message of class
  `catchmentACS_message_resolve_site`, and give a warning of class
  `catchmentACS_warning_resolve_site_distant` when that site is more than
  5 km away.
* `cacs_intersect_weight()` hides two notices from sf in its own intersection
  steps: that coordinates are treated as planar, and that attributes are
  assumed to be constant over the geometries. The package's own warnings, such
  as those about skipped tracts, are still shown.

## Documentation

* `vignette("porting-v01-to-v03", package = "catchmentACS")` describes, with
  examples, the changes for code written for 0.1.0 or 0.2.0.

# catchmentACS 0.2.0

## New features

* `cacs_plot_site_isochrone()`, `cacs_plot_site_intersection()`,
  `cacs_plot_site_weighted()`, and `cacs_plot_site_rates()` are new
  functions that draw leaflet maps of one site at each step of the
  calculation, and `cacs_plot_site_pipeline()` draws all four. A site is
  chosen by `site_id` and `sites_df`, by `lat` and `lon`, or by
  `site_name`. `vignette("visual-walkthrough", package = "catchmentACS")`
  shows the maps. The functions need the leaflet package, which
  catchmentACS lists under Suggests.
* `cacs_run()` now returns an object of class `cacs_run_result`, which is
  still a tibble, so code that uses the result as a tibble works as before.
  Its `print()` method shows a summary of the run before the rows, and
  `summary()` returns a `cacs_run_summary` list with the mean, standard
  deviation, and number of missing values of each rate. Both use the
  attribute `cacs_run_result_metadata`; without it, `print()` shows a plain
  tibble.
* `cacs_isochrone()`, `cacs_acs_prefetch()`, and `cacs_intersect_weight()`
  now report progress when `verbose = TRUE`: a summary for small jobs, and
  progress for each site or pair when there are five or more.
  `options(catchmentACS.progress = "off")` or the environment variable
  `CACS_QUIET=1` turns this off, and
  `options(catchmentACS.progress = "force")` shows the progress for each
  site or pair at any size. `cacs_run(verbose = TRUE)` passes `verbose` to
  `cacs_intersect_weight()`.
* `cacs_derive_rates()` fills two new columns on rate rows, `n_tracts_num`
  and `n_tracts_den`: the numbers of tracts combined for the numerator and
  for the denominator. `n_tracts` stays `NA` on rate rows.

## Bug fixes

* `cacs_acs_prefetch()` now removes, by default (`drop_water_tracts = TRUE`),
  water and special-purpose tracts (tract numbers 9900 and above) and tracts
  whose boundary has no area. A message of class
  `catchmentACS_message_water_tract_filter` lists up to five of the removed
  `GEOID`s. Previously, such tracts made the area-weighted intersection stop
  with an error.
* `cacs_intersect_weight()` now skips tracts with no area, with a warning of
  class `catchmentACS_warning_geometry_skip`, and lists them in the
  attribute `skipped_geoids`. It still stops with an error of class
  `catchmentACS_error_geometry` when no tract has an area.
* `cacs_isochrone()` no longer gives three deprecation warnings from tigris
  on each call, because it no longer passes the argument `returnclass` to
  `tigris::tracts()`.

# catchmentACS 0.1.0

* Initial alpha version.
* `cacs_run()` runs the five steps `cacs_acs_prefetch()`, `cacs_isochrone()`,
  `cacs_intersect_weight()`, `cacs_propagate_moe()`, and
  `cacs_derive_rates()`, each of which can also be called on its own.
* `cacs_se_to_moe()` and `cacs_moe_to_se()` convert between standard errors
  and margins of error; `cacs_cache_dir()`, `cacs_cache_status()`, and
  `cacs_clear_cache()` find, summarize, and clear the cache folder; and
  `cacs_acs_validate()` checks the form of ACS data.
* `cacs_alabama_sites`, `cacs_acs_default_vars`, and
  `cacs_acs_default_rates` are the package's datasets.
* `vignette("getting-started", package = "catchmentACS")` introduces the
  package.
