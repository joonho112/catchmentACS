# Data files installed with catchmentACS

The files in this folder are installed with the package and are read with
`system.file("extdata", "<file>", package = "catchmentACS")`. They are not
datasets that `data()` loads; the articles, the examples, and the package tests
read them from this folder.

The folder holds six data files and this description. Five of the six hold
made-up data; only `visual_walkthrough_fixture.rds` holds American Community
Survey (ACS) estimates downloaded from the Census Bureau and a drive-time area
from a routing server. Numbers read out of the made-up files describe no real
place.

| File | Size | What it holds |
|---|---:|---|
| `legacy_2025_sites.rds` | 1.3 KB | 20 made-up sites: an sf object of points (EPSG:4326) with the columns `site_id`, `site_name`, `county_fips`, and `region_label` |
| `legacy_2025_isochrones.rds` | 81 KB | 60 circles (20 sites by three drive times) as an sf object of polygons (EPSG:4326) with 16 columns describing a routing run |
| `legacy_2025_golden_output.rds` | 9.0 KB | 1,080 rows by 20 columns of class `cacs_run_result`: the saved output that one regression test compares against |
| `sample_alabama_subset.rds` | 44 KB | Made-up ACS values: 911 squares by 14 variables (12,754 rows by 6 columns, EPSG:4269) |
| `visual_walkthrough_fixture.rds` | 25 KB | A list of six objects holding real data for one Birmingham site, used by the mapping article |
| `osm_snapshot_2025-04-01.txt` | 810 B | A note on the fixed routing values in `legacy_2025_isochrones.rds`, written by the script that builds the three `legacy_2025_*` files |

## The made-up ACS values

`data-raw/al_acs_fetch.R` writes `sample_alabama_subset.rds` without contacting
the Census Bureau. It draws each estimate from a uniform distribution over a
range chosen for that variable and each margin of error as 5 to 25 percent of
the estimate, from a fixed random seed, so the file is the same every time the
script runs. One row in twenty, 638 of 12,754, has a missing estimate and the
margin of error `-555555555`, the value the Census Bureau uses when a margin of
error is not appropriate because the estimate is controlled to an independent
population or housing estimate. The 14 variables are those of
`cacs_acs_default_vars`.

The geometries are 0.03-degree squares, not tract boundaries. Each sits at one
position of a lattice over a rectangle around Alabama, and the positions are
0.12 degrees apart from west to east and 0.16 from south to north, so the
squares do not touch: together they cover about a twentieth of the rectangle.
About a quarter of the 911 lie entirely outside the state. The `GEOID` values
are built from real Alabama county FIPS codes and made-up six-digit tract
numbers, so they are not the GEOIDs of real tracts, and none of them appears in
the one file here that holds real data. The file records no ACS year; the
scripts and tests that read it pass `year = 2023`.

## The made-up sites and drive-time areas

`data-raw/regression_al_prek_2025.R` writes the three `legacy_2025_*` files.
The 20 sites are the centers of 20 of the squares above, so six of them
(`AL_SITE_01`, `AL_SITE_02`, `AL_SITE_03`, `AL_SITE_10`, `AL_SITE_15`, and
`AL_SITE_18`) fall outside Alabama, in the Gulf or across the Florida, Georgia,
and Mississippi lines. Their `county_fips` column is cut from the made-up
`GEOID` values, so it does not say where a point is.

The same `site_id` values appear in the bundled dataset `cacs_alabama_sites`
for ten other points near Alabama city centers. The two objects share
`AL_SITE_01` through `AL_SITE_10` and place them between 134 and 436 km apart,
so a `site_id` alone does not identify a location across the two.

The drive-time areas are circles, not routes: the script buffers each point by
one kilometer per minute, giving areas of 78.5, 314.0, and 706.5 square
kilometers for 5, 10, and 15 minutes. The 16 columns record a routing run that
never happened: the script writes them as fixed values, including
`provider = "osrm"`, `routing_engine_version = "OSRM 5.27.1"`,
`osm_snapshot_date = "2025-04-01"`, and `osm_snapshot_status = "explicit"`,
without contacting a server. `osm_snapshot_2025-04-01.txt` is a note the same
script writes next to them. Results computed from these areas repeat some of
the values, such as `provider`, and the list-column form of a `cacs_run()`
result keeps all of them with the areas.

## The saved output the regression test compares against

`legacy_2025_golden_output.rds` is the result of running `cacs_run()` with
`weight_method = "area"` on the three made-up inputs above. It records what the
package computed at the time. It reproduces no earlier analysis, and it
describes no place in Alabama.

Its 1,080 rows are 540 rows for `weight_method = "area"`, which the test uses,
and 540 placeholder rows for `weight_method = "population"`, kept from an
earlier version that accepted the name without computing anything from it. On
the placeholder rows:

- `estimate`, `moe`, `weight_sum`, and `n_tracts` are `NA`;
- `weight_basis` is `"block_group_pop"` and `moe_fallback_reason` is `"n/a"`;
- the `population_stub_status` field of the file's `provenance` attribute
  records that nothing was computed for them.

Weighting by population is still not implemented.

The nine values of `variable` are the seven ACS codes `B01003_001`,
`B17001_001`, `B17001_002`, `B19013_001`, `B19301_001`, `B22003_001`, and
`B22003_002`, and the two rates `poverty_rate` and `snap_rate`.

Missing values are not confined to the placeholder rows. In the 540 area rows,
63 have a missing `estimate` and `moe`, and 17 have a missing `weight_sum`,
because the missing estimates in `sample_alabama_subset.rds` carry through the
calculation; those 17 rows have `failure_origin = "carrier"`, which marks a rate
whose numerator or denominator was missing, and the rest have `"none"`. The
`acs_year` column is `NA` in all 1,080 rows, because that column is copied from
the record that `cacs_acs_prefetch()` attaches to downloaded estimates and this
run read a plain file instead. The year is in the file's `provenance`
attribute.

The 20 columns are the layout of the version that produced the file. A result
from the current version has 27 columns. Rebuilding the file changes the output
that the regression test compares against, so it is not part of ordinary
maintenance: a rebuild should say why the saved output no longer applies and
what moved in the 540 area rows.

### The test

`tests/testthat/test-regression-2025-alabama.R` reads the file. It runs only
when the environment variable `CACS_REGRESSION` is `"true"`, and never on CRAN.
It keeps the 540 area rows, runs `cacs_run()` on the same three inputs, joins
the two on `site_id`, `drive_time_min`, `weight_method`, and `variable`, and
compares four columns:

| Column | Allowed difference | Why |
|---|---|---|
| `estimate` | 1e-6 | two ways of writing the same arithmetic |
| `moe` | 1e-5 | rounding accumulates through the sum of squares and the square root |
| `weight_sum` | 1e-9 | areas come from `sf::st_area()` alone |
| `n_tracts` | none; the values must be identical | a count |

Rows where both values are missing count as equal, which is why the 63 missing
estimates above do not fail the test. The other 12 columns are carried through
the join but are not compared.

### The record attached to the file

`attr(x, "provenance")` is a list of 15 fields describing the run that produced
the file, in three groups:

- what it was built from: `source_scripts` (the script named above),
  `script_hash_sha256` (a SHA-256 hash of the version of that script that wrote
  the file), `input_hashes_sha256` (a SHA-256 hash of each of the three input
  files), `generation_seed`, `generated_on`, and `package_git`;
- what the run assumed or found: `acs_year`, `osm_snapshot`,
  `acs_missing_estimate_policy`, `acs_input_missing_estimate_count`,
  `geos_version`, and `proj_version` (a change in either can move an area
  slightly);
- the placeholder rows: `population_stub_status`, `population_stub_rows`, and
  `v01_synthetic_note`.

The file also carries the attributes that `cacs_run()` attaches to any result,
such as `cacs_run_provenance`; `?cacs_run` describes them.

## The real Birmingham data for the mapping article

`visual_walkthrough_fixture.rds` holds real data for one site, `AL_BHM_01`, in
Birmingham, so that the article "Mapping each step for one Birmingham site"
(<https://joonho112.github.io/catchmentACS/articles/visual-walkthrough.html>,
or `vignette("visual-walkthrough", package = "catchmentACS")`) can draw its
maps without a network connection. It is a list of six elements:

- `iso_sf`: the 10-minute drive-time area, built on 2026-05-27 through the
  public demo server of the Open Source Routing Machine (OSRM) with
  `profile = "car"`, the default, and `res = 30L`,
  the lightest setting, which asks the server for a grid of 30 by 30 points.
  That value is not a column; it is in the area's `cacs_isochrone_provenance`
  attribute, as `res_param`. The date of the OpenStreetMap road data the server
  used is not recorded, and the area says so in
  `osm_snapshot_status = "unknown_best_effort"`.
- `acs_sf` and `tract_sf`, the same object under two names: the 2019–2023 ACS
  5-year estimates and margins of error of the 60 census tracts that meet the
  area or lie within about 2 km of it, for the 14 variables of
  `cacs_acs_default_vars`, downloaded with tidycensus 1.8.1, with tract
  boundaries from tigris for 2023.
- `run_result`: the result of `cacs_run()` for that site, in the current
  27-column layout.
- `sites_df`: the site as a point.
- `anchor_site`: the string `"AL_BHM_01"`.

These are the only real ACS, boundary, and routing data in this folder. They
are there for the maps; the regression test above uses the made-up files
instead, so that it does not depend on what a routing server answers today.

The drive-time area was computed from OpenStreetMap data, © OpenStreetMap
contributors, available under the Open Database License
(<https://www.openstreetmap.org/copyright>). The ACS estimates and the tract
boundaries are from the U.S. Census Bureau.
