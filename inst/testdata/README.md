# Test data installed with catchmentACS

The package tests read the files in this folder with
`system.file("testdata", "<file>", package = "catchmentACS")`, and the article
on updating code written for versions 0.1 and 0.2 reads
`fixture_032_3site_fresh.rds`.

Four files hold data from a routing server and the Census Bureau:

- `fixture_032_3site_fresh.rds`: the inputs and results of a run of version
  0.2 for three points at landmarks in Birmingham, Mobile, and Huntsville,
  Alabama: 10-minute drive-time areas from the public OSRM (Open Source Routing
  Machine) demo server, 2019–2023 American Community Survey (ACS) 5-year
  estimates for the census tracts of Alabama, and the estimates computed from
  them.
- `iso_3site_demo.rds`: 10-minute drive-time areas for the same three points
  from the same server.
- `run_result_3site_demo.rds`: a result of `cacs_run()` for the same three
  points, computed from OSRM drive-time areas and 2019–2023 ACS 5-year
  estimates.
- `fixture_water_tract_baldwin.rds`: 2019–2023 ACS 5-year estimates and the
  boundaries of three census tracts in Baldwin County, Alabama.

In these four files, the drive-time areas were computed from OpenStreetMap
data, © OpenStreetMap contributors, available under the Open Database License
(<https://www.openstreetmap.org/copyright>), and the ACS estimates and the
tract boundaries are from the U.S. Census Bureau.

`fixture_cache_poison.rds` holds made-up ACS values and values computed from
them and the drive-time areas in `fixture_032_3site_fresh.rds`.
`al_10_site_sample.rds`, `fixture_annulus_input.rds`, and the six
`synthetic_*.rds` files hold made-up data.
