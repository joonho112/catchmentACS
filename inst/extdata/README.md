# `inst/extdata/` — Frozen fixtures for catchmentACS

This directory holds *frozen* fixture files that ship with the installed
package and are retrieved via `system.file("extdata", ...)`. These files
are **not** lazy-loaded as datasets (no `@docType data`); their schemas
and provenance are documented in this file.

## File inventory (v0.5.0)

| File | Size | Purpose | Schema |
|---|---:|---|---|
| `legacy_2025_sites.rds` | ~1 KB | 20-site regression input (sf POINT, EPSG:4326) | site points |
| `legacy_2025_isochrones.rds` | ~80 KB | 60 synthetic cumulative isochrone polygons (20 sites x 3 drive_times) with OSRM-style provenance metadata | 16-col isochrone |
| `legacy_2025_golden_output.rds` | ~9 KB | 1,080-row frozen regression golden (540 area rows + 540 historical population-stub rows) | 20-col canonical long |
| `sample_alabama_subset.rds` | ~44 KB | Synthetic AL ACS 2023 tract panel (911 tracts x 14 vars) | ACS tract panel |
| `visual_walkthrough_fixture.rds` | ~324 KB | Offline visual-vignette fixture for one anchor site, regenerated with the current v0.5.0 schema | visual walkthrough |
| `osm_snapshot_2025-04-01.txt` | ~1 KB | Historical routing-provenance metadata carried by synthetic legacy fixtures | text |
| `README.md` | this file | Inventory + schema + provenance | - |

## Provenance — synthetic golden

The synthetic regression golden is produced by the canonical `cacs_run()`
pipeline (see below), not by reproducing any private administrative dataset.
The generation script (`data-raw/regression_al_prek_2025.R`) records pipeline
provenance — package and geometry-engine versions, the pinned random seed, and
input hashes — in the golden's `attr(., "provenance")` list.

## Synthetic regression golden

`legacy_2025_golden_output.rds` is a **synthetic regression golden** rather
than a 1:1 reproduction of the 2025 baseline output. This is by design:

1. The 5 baseline `.rds` files use a **wide-form ring-based schema**
   (annular rings `isomin/isomax` = 0-5, 5-10, 10-15 with wide columns
   `total_pop_E`, `total_pop_M`, etc.) that does not match the canonical
   20-col long schema used by `cacs_run()` (cumulative
   isochrones at 5, 10, 15 min + long-format with `variable` column).
2. Re-deriving a canonical golden from the baseline tibbles requires
   re-running the 2025 scripts with new aggregation logic — that is
   itself a dedicated golden-regeneration effort, not part of building this
   fixture.
3. v0.5.0 still ships without an administrative ground-truth fixture or
   a supported population-weighting path, so ground-truth regeneration remains
   deferred.

The synthetic golden is instead produced by running `cacs_run()`
end-to-end with `weight_method = "area"` on a 20-site input + synthetic
buffered-circle isochrones + the `sample_alabama_subset.rds` ACS fixture
(also synthetic). This makes the regression test a *frozen output of the
canonical pipeline* rather than a 1:1 reproduction of the 2025 ad-hoc
scripts. It is useful for idempotence, schema, and drift checks; it should not
be interpreted as live analytical truth for Alabama policy reporting.

### Golden output preservation

`legacy_2025_golden_output.rds` preserves the **FULL 1,080 rows**:
- 540 rows for `weight_method = "area"` — produced by `cacs_run()`
- 540 rows for `weight_method = "population"` — STUB with
  `estimate = NA`, `moe = NA`, `weight_sum = NA`, `n_tracts = NA`,
  `moe_fallback_reason = "n/a"`, `failure_origin = "none"`.
  The inactive population-path status is recorded in
  `attr(golden, "provenance")$population_stub_status`, not in canonical
  row-level enum columns.

The v0.5.0 regression test filters to `weight_method == "area"` (540 rows).
The population-stub block remains a historical compatibility artifact while
`weight_method = "population"` is a reserved fail-loud surface. A future
population-weighting release may replace that block by a dedicated regeneration
step, but should not change the area block without an explicit
algorithmic-drift review.

## Visual walkthrough fixture

`visual_walkthrough_fixture.rds` is a bundled offline fixture for
`vignettes/visual-walkthrough.qmd`. It packages one anchor site's inputs and
rendering output so the vignette can build without network calls. Its embedded
ACS and run-result objects use the current v0.5.0 long-output schema, including
explicit `ring_topology` provenance and corrected normalized mean-weight proxy
rows. The fixture is appropriate for plotting examples, not for the v0.5.0
golden regression contract. The authoritative current ACS sample for
analysis-style examples is `sample_alabama_subset.rds`.

## Schema (`legacy_2025_golden_output.rds`)

20 columns (canonical long schema), 1,080 rows:

| Column | Type | Notes |
|---|---|---|
| `site_id` | chr | 20 unique values (`"AL_SITE_01"` ... `"AL_SITE_20"`) |
| `drive_time_min` | int | `{5, 10, 15}` |
| `variable` | chr | 9 distinct: 7 source vars + 2 derived rates |
| `estimate` | dbl | area weighted sum / NA in pop stub |
| `moe` | dbl | RSS propagation / NA in pop stub |
| `weight_sum` | dbl | area-weighted sum of overlap fraction / NA in pop |
| `n_tracts` | int | overlap > 0 tract count / NA for derived rates and pop |
| `provider` | chr | `"osrm"` |
| `profile` | chr | `"car"` |
| `osm_snapshot_date` | chr | `"2025-04-01"` |
| `acs_year` | int | `2023` |
| `weight_method` | chr | `"area"` (540) or `"population"` (540 stub) |
| `estimand_family` | chr | `area_weighted_scalar_proxy` / `median_proxy` / `spatial_total` |
| `weight_basis` | chr | `area_mean` / `coverage` |
| `moe_formula_requested` | chr | sanctioned MOE-formula label |
| `moe_formula_effective` | chr | sanctioned MOE-formula label |
| `moe_fallback` | lgl | primary-to-fallback MOE formula flag |
| `moe_fallback_reason` | chr | fallback-reason enum |
| `failure_origin` | chr | upstream/row-construction status |
| `weight_uncertainty_propagated` | lgl | v0.1 always `FALSE` |

The 9 distinct `variable` values:

| Type | Code/name | Source |
|---|---|---|
| Source | `B01003_001` | Total population |
| Source | `B17001_001` | Poverty denominator |
| Source | `B17001_002` | Below poverty |
| Source | `B19013_001` | Median HH income |
| Source | `B19301_001` | Per-capita income |
| Source | `B22003_001` | SNAP denominator |
| Source | `B22003_002` | SNAP receiving |
| Derived rate | `poverty_rate` | `B17001_002 / B17001_001` |
| Derived rate | `snap_rate` | `B22003_002 / B22003_001` |

## Provenance attribute (`attr(golden, "provenance")`)

The golden carries a provenance list with audit fields including:

- `source_scripts` (chr[4]): 2025 baseline R script filenames
- `baseline_artifacts` (chr[5]): the 5 baseline `.rds` filenames
- `baseline_status` (named lgl[5]): existence audit per baseline file
- `acs_year` (int): `2023L`
- `osm_snapshot` (chr): `"2025-04-01"`
- `geos_version` (chr): GEOS minor version (geometry drift detector)
- `proj_version` (chr): PROJ version (geometry drift detector)
- `generated_on` (Date): generation date
- `generation_seed` (int): `20260522L` (commit-pinned random seed)
- `acs_missing_estimate_policy` (chr): `"fail_safe_propagate_na"`
- `acs_input_missing_estimate_count` (int): missing ACS estimate count in
  `sample_alabama_subset.rds`
- `population_stub_status` (chr): `"v0.1_population_path_inactive"`
- `population_stub_rows` (int): `540L`
- `script_hash_sha256` (chr): SHA-256 of `data-raw/regression_al_prek_2025.R`
- `input_hashes_sha256` (list): SHA-256 hashes for the ACS, site, and
  isochrone fixture inputs used by the golden
- `package_git` (list): git worktree status when available, or
  `"not_git_worktree"` when this package snapshot is not inside git
- `baseline_audit_status` (chr): baseline-existence audit status
- `v01_synthetic_note` (chr): documentation of the historical v0.1 alpha synthetic path label

This attribute is fixed for the current synthetic regression lifecycle; changes
occur only via logged drift repair or golden regeneration work.

## Regeneration policy

Do not regenerate `legacy_2025_golden_output.rds` as routine maintenance. A
regeneration requires a dedicated drift/regeneration PR or logged repair step
that records why the old fixture is invalid, preserves the 540-row area block
unless numeric drift has been reviewed, and updates the provenance hashes. A
future population-weighting release may replace the 540-row population stub
block; that update should not change the area block without an explicit
algorithmic-drift review.

## Tolerance contract

Per-column absolute tolerance for the regression test:

| Column | epsilon | Justification |
|---|---|---|
| `estimate` | 1e-6 (abs) | algorithmic equivalence |
| `moe` | 1e-5 (abs) | RSS sqrt one-ulp accumulation |
| `weight_sum` | 1e-9 (abs) | pure sf st_area() — provider noise zero |
| `n_tracts` | exact int | discrete count, no tolerance |

If the regression test fails, follow the drift triage playbook in the
regeneration policy above before regenerating the golden.
