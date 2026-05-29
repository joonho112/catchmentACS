# catchmentACS 0.5.0

> v0.5.0 is a beta-readiness release focused on clearer offline examples,
> provider/status documentation, safer ACS preflight behavior, fuller
> list-column outputs, refreshed regression fixtures, and faster intersect
> cache reuse.

## Bug fixes

* **Corrected the normalized mean weight in `cacs_intersect_weight()`.** The
  mean weight is now normalized within each (site, drive-time, *variable*) cell,
  so it sums to one per variable. Previously it was normalized over the whole
  (site, drive-time) cell, whose long-format rows span every requested variable;
  that summed each tract's intersection area once per variable and **deflated
  every `area_weighted_scalar_proxy` and `median_proxy` estimate (and its margin
  of error) by the number of variables** in the call. Scalar/median proxy
  estimates are now proper area-weighted averages that lie within the range of
  the contributing tract values. **`spatial_total` counts and `derived_rate`
  values are unaffected** (they use the per-row coverage weight), so the five
  curated rates and all count totals were always correct. The frozen regression
  golden and the v0.2 Phase-2 weighted baseline were updated for the corrected
  proxy rows; a unit test now locks the per-variable normalization.

## Public workflow and API polish

* `cacs_run(output = "list_column")` and `output = "both"` now fill the
  `$isochrone` list-column from the resolved isochrone object on all supported
  execution paths, including computed 5-call and 4-call-B runs. The existing
  `catchmentACS_message_listcol_iso_filled` condition class is preserved, with
  wording broadened from v0.4 bypass-only behavior to v0.5 resolved-iso
  behavior.
* `cacs_isochrone()` now accepts base `data.frame` site inputs in addition to
  tibbles and `sf` point objects. `cacs_run()` documentation and validation
  hints were aligned with the same input contract.
* `cacs_acs_prefetch()` now pins ACS5 end-year support to 2009-2024 for this
  release, rejects fractional years instead of truncating them, and checks the
  persistent ACS cache before requiring `CENSUS_API_KEY`. Cache hits and
  explicit `acs =` bypasses remain usable without a Census key; live cache
  misses and `force_refresh = TRUE` require one before tidycensus metadata/data
  calls.
* Provider and weighting status is now explicit across docs and fail-loud
  paths: OSRM and ORS are implemented routing providers; Mapbox, r5r,
  population weighting, and custom-rate surfaces remain reserved/deferred in
  this beta instead of silently returning placeholders.
* `cacs_run(weight_method = "population")` now fails immediately before ACS,
  provider, or spatial work begins. Area weighting remains the only
  implemented weighting method in v0.5.0.

## Documentation and fixture contract

* Corrected the package author identity and home repository: maintainer and
  author is now JoonHo Lee (`jlee296@ua.edu`, ORCID 0009-0006-4019-8703), and
  the canonical repository is `joonho112/catchmentACS` with the documentation
  site at `https://joonho112.github.io/catchmentACS`. A hex-sticker logo and
  matching favicons were added.
* Converted all reader-facing documentation to English, enabled roxygen
  markdown, and rewrote every exported object's help page to publication grade.
  The reference index is now complete and grouped with `@family` tags.
* Reorganized the vignettes into a two-track-plus-migration structure. The
  Applied track covers getting-started, the Alabama tutorial, providers, and
  the visual walkthrough. The Method track covers the methodology overview plus
  three new theory articles: `theory-spatial-aggregation`,
  `theory-moe-propagation`, and `theory-derived-rates`. The Migration track
  collects the v0.1->v0.3 and v0.3->v0.4 porting guides and a new v0.4->v0.5
  migration guide.
* Rebuilt the README with a vignette-track table, a verified offline
  quick-start, a citation block, and funding/affiliation details. An
  `inst/CITATION` file is now provided.
* Restructured the pkgdown site with Applied, Method, and Migration navbar
  tracks and enabled KaTeX so the method-track math renders.
* Added and refreshed the v0.5 documentation surface: root README with a
  verified no-network offline example, pkgdown scaffold, methodology vignette,
  Alabama tutorial, provider status article, and visual walkthrough.
* Offline examples now consistently state that bundled `legacy_2025_*`
  isochrones are synthetic buffered-circle fixtures with OSRM-style provenance
  metadata. They do not call Census, OSRM, ORS, or any other live service while
  package docs render.
* Bundled fixture docs now distinguish synthetic regression evidence, visual
  walkthrough support, packaged water-tract regression data, and live
  analytical workflows. Fixture values are examples/regression baselines, not
  current Alabama reporting truth.
* High-value Rd examples and cross-links were refreshed for `cacs_describe()`,
  `cacs_rings_to_cumulative()`, and the `cacs_run_result` S3 methods, keeping
  examples network-free.

## Regression, data, and performance

* Repaired `inst/extdata/legacy_2025_golden_output.rds` with a controlled
  golden-only regeneration so the 1,080-row synthetic Alabama regression
  baseline matches the current packaged ACS/sites/isochrone inputs. Sites and
  isochrone fixtures were not copied back from the regeneration dry run.
* The gated `CACS_REGRESSION=true` regression suite now passes against the
  repaired golden baseline, with population-weighting rows preserved only as
  documented historical stubs.
* Intersection cache keys are substantially faster: ACS geometry content
  hashing switched from row-wise WKT text hashing to WKB raw hashing with a WKT
  fallback. In the 20-site fixture benchmark, warm intersect cache hits improved
  from about 1.98 seconds to 0.188 seconds, and cache-key construction from
  about 1.97 seconds to 0.180 seconds. Existing analytical outputs are
  unchanged, but old intersect-cache entries may be missed because the internal
  cache key changed.
* Added dev-only no-network performance harnesses for 1-site, 10-site, and
  20-site fixture-scale runs.
* Coverage is now recorded as a v0.5 measurement-only baseline. The local
  offline `covr::package_coverage()` run measured 86.09% total coverage; no
  hard coverage threshold is enforced in CI for this release.

## Live-test and CI posture

* Live provider/Census smoke tests are documented as opt-in gates in
  `tests/testthat/README.md`. Routine package checks and vignettes remain
  deterministic and no-network by default.
* The live OSRM smoke test now reflects the v0.5 public-demo omitted-`res`
  policy (`30L` under demo-budget protection), and no longer writes trace
  output into local development log folders.
* The coverage workflow now prints the measured coverage percentage while
  keeping Codecov upload failures non-blocking and avoiding a numeric
  percentage gate.

## Caveats

* Intersect cache keys changed because of the WKB hashing update, so existing
  on-disk intersect cache entries may be missed once and recomputed.
* No real ORS live smoke test is included; ORS is covered by mocked
  integration tests and credential validation.
* ACS support is release-pinned to ACS5 tract pulls for one state at a time,
  2009-2024.

---

# catchmentACS 0.4.5

> v0.4.5 is a **small patch release** over v0.4.0. It keeps public APIs stable
> while tightening OSRM compatibility, documenting print capture behavior, and
> aligning tests/docs with the behavior observed in the v0.4 workflow tests.

## OSRM compatibility and pre-flight checks

* `cacs_validate_osrm_endpoint()` now builds the public
  `routing.openstreetmap.de` probe URL with the required profile-prefixed
  service path (for example `/routed-car/route/v1/driving/...` for the default
  car profile). Custom/self-hosted endpoints continue to use the standard
  `/route/v1/driving/...` path.
* HTTP statuses from the OSRM pre-flight probe are preserved for classification
  instead of being converted into generic request errors before
  `http_status`/`quota_ok` can be reported.
* The internal OSRM isochrone wrapper now forwards upstream `n` only when a
  public `res` value has an exact `osrm` 5.x mapping (for example
  `res = 27L -> n = 500L`). Non-exact catchmentACS public/compatibility values
  such as `res = 30L`, `50L`, and `70L` continue to forward `res` so geometry
  and request-budget semantics do not silently change.

## Documentation and tests

* `as_tibble(cacs_run_result, rate_first = FALSE)` is documented and tested as
  an opt-out from the v0.4 rate-first lift. It returns the underlying
  long-format row order as-is; it is not a separate alphabetical sorter.
* `print.cacs_run_result()` documentation now explains that `cli` headers are
  emitted on the message/condition stream. Tests that assert on headers should
  use `testthat::capture_messages(print(x))` or
  `capture.output(print(x), type = "message")`; bare
  `capture.output(print(x))` can miss headers because it captures stdout.
* Stale print documentation now says "Top 5 rates (cross-site mean +/- sd)"
  rather than "top-3 rates preview".

## Dependency-noise cleanup

* `cacs_acs_prefetch()` no longer passes the deprecated/ignored
  `cache = TRUE` argument to `tidycensus::load_variables()`. Test mocks now
  accept `...`, and a source-scan guard prevents reintroducing that deprecated
  argument in `R/`.

---

# catchmentACS 0.4.0

> v0.4.0 is an **adoption-readiness patch release** closing four
> hands-on workflow-test issues surfaced by v0.3.0. The four
> watchwords are **Discoverable / Demo-safe / Foundation-first /
> Backward-locked**. Every v0.3 script continues to work unchanged.

## Discoverability

* **[Issue #004]** Per-site/per-drive-time rate matrix is now first-class across all four
  `cacs_run_result` S3 surfaces:
  - `summary(run_result)` gains two new unconditional slots
    `rates_per_site` (numeric tibble) and `rates_per_site_moe` (cells like
    `"0.192 ± 0.024"`), plus a `breakdown = c("cross_site", "per_site",
    "both")` argument.
  - `print(run_result)` cli card renames "Top-3 rates" to "Top 5 rates
    (cross-site mean ± sd)" and auto-appends a "Rates per site" mini-block
    when `n_sites <= getOption("catchmentACS.summary_per_site_max", 12L)`.
  - `as_tibble(run_result)` is now a registered S3 method that defaults to
    `rate_first = TRUE`, surfacing rate rows above source ACS B-codes
    within each `(site_id, drive_time_min)` block. Disable the v0.4
    rate-first lift via `rate_first = FALSE` or
    `options(catchmentACS.rate_first_default = FALSE)`; this returns the
    underlying long-format row order as-is and does not apply a separate
    alphabetical re-sort.
  - `cacs_summary_as_markdown()` detects per-site shapes by column-name
    signature and renders rate-first pipe Markdown with an MOE-aware
    caption.
  - All four surfaces share one internal pivot helper
    (`.cacs_rates_per_site_pivot()`), making the keyed table
    (`site_id`, optional `drive_time_min`, rate columns) byte-identical
    across entry points by construction.
* **[Issue #003]** `cacs_run(output = "list_column", precomputed_isochrones = iso)`
  now fills the `$isochrone` list-column with per-site `sf` rows on the
  3-call and 4-call bypass paths, replacing the v0.3 `<NULL>` placeholder.
  Emits `catchmentACS_message_listcol_iso_filled` per `cacs_run()` call
  when the fill happens.

## Safety-by-default

* **[Issue #002]** `cacs_isochrone(provider = "osrm")` with `osrm_mode =
  "demo"` (default) and omitted `res` now auto-downgrades to `res = 30L`
  (instead of the v0.3 70L), preventing HTTP 429 quota-exhaustion on the
  public demo endpoint. Emits a once-per-session classed message
  `catchmentACS_message_demo_budget_protected`. Disable via
  `options(catchmentACS.osrm_demo_budget_protect = FALSE)`. The
  `osrm_mode = "docker"` path keeps the v0.3 70L default.
* The HTTP 429 abort message now lists three remediation paths in priority
  order: lower `res` (e.g., `iso_args = list(res = 30L)`), switch to
  `osrm_mode = "docker"`, or switch provider.
* New exported `cacs_validate_osrm_endpoint(server, timeout)` performs a
  lightweight `/route` ping returning a 1-row tibble
  (`endpoint`, `quota_ok`, `response_ms`, `http_status`). On HTTP 429
  emits `catchmentACS_warning_provider_quota_exhausted`.

## Issue closures

* **[Issue #001]** `cacs_capture_conditions()` adds `return_value =
  c("conditions", "both")` argument. When `"both"`, returns
  `list(result, conditions)` so the natural pattern
  `out <- cacs_capture_conditions(cacs_acs_prefetch(...), return_value =
  "both"); acs <- out$result` works without `<<-` super-assignment. Default
  stays `"conditions"` (v0.3 byte-identical). Session-wide override via
  `options(catchmentACS.capture_return_value = "both")`.
* **[Latent v0.2/v0.3 correctness fix]** `summary(cacs_run(...))$rates_breakdown`
  values were silently mis-aligned (mean values circularly shifted by one
  alphabetical position relative to the `variable` label column) due to a
  `cacs_run_result` S3 class interaction with dplyr `group_by + summarise`.
  v0.4 strips the class before internal aggregation and registers a
  dplyr-safe `group_by.cacs_run_result()` method, producing correct values.
  **Users with published v0.3 rates_breakdown values should re-compute under
  v0.4.**

## API additions

* `cacs_capture_conditions(..., return_value = c("conditions", "both"))` —
  new argument; default preserves v0.3 behavior (Issue #001).
* `cacs_isochrone()` omitted-`res` auto-downgrade on demo (Issue #002).
* `cacs_validate_osrm_endpoint(server = NULL, timeout = 5)` — new exported
  pre-flight helper (Issue #002).
* `summary.cacs_run_result(..., breakdown = c("cross_site", "per_site", "both"))` —
  new argument; default preserves v0.3 print (Issue #004 Track 1).
* `summary(run_result)$rates_per_site` and `$rates_per_site_moe` — new
  unconditional list slots keyed by `site_id` plus `drive_time_min` when
  present (Issue #004 Track 1).
* `as_tibble.cacs_run_result(..., rate_first = NULL)` — new registered
  S3 method; default `TRUE` via option `catchmentACS.rate_first_default`
  (Issue #004 Track 3).
* `cacs_summary_as_markdown()` per-site shape detection (Issue #004 Track 4).
* 4 new package options:
  - `catchmentACS.summary_per_site_max` (default `12L`) — auto-print gate.
  - `catchmentACS.osrm_demo_budget_protect` (default `TRUE`) — Issue #002.
  - `catchmentACS.capture_return_value` (default `"conditions"`) — Issue #001.
  - `catchmentACS.rate_first_default` (default `TRUE`) — Issue #004.
* 4 new classed conditions:
  - `catchmentACS_message_demo_budget_protected` (Issue #002).
  - `catchmentACS_message_rate_first_changed` (Issue #004).
  - `catchmentACS_message_listcol_iso_filled` (Issue #003).
  - `catchmentACS_warning_provider_quota_exhausted` (Issue #002 pre-flight).

## Documentation

* New vignette `porting-v03-to-v04` — 6-section migration guide covering
  the 4 issues plus the latent rates_breakdown correctness fix.
* `getting-started` refreshed with a "v0.4's new deliverable — per-site
  rate matrix" section showcasing the four-track surface.
* `?cacs_capture_conditions` gains an `@section Capturing both the result
  and the conditions` covering the `return_value = "both"` pattern and
  the `<<-` workaround (still supported).
* `?cacs_isochrone` expanded to list three remediation paths in priority order.
* `?summary.cacs_run_result` documents the `breakdown` argument and the
  two new list slots.
* `?as_tibble.cacs_run_result` (new Rd) documents the rate-first sort.
* `?cacs_validate_osrm_endpoint` (new Rd) documents the pre-flight probe.

## Backward compatibility

Every v0.3 script continues to work without modification on v0.4:

* `cacs_capture_conditions(expr, classes)` with the default returns the
  identical tibble shape as v0.3 (the `return_value = "conditions"`
  default is preserved verbatim).
* `cacs_isochrone(..., res = 70L)` explicit calls remain silent and
  resolve to 70L regardless of `osrm_mode`. Only the omitted-`res` +
  `osrm_mode = "demo"` combination triggers the downgrade.
* `cacs_run()` long output is byte-identical to v0.3 schema. The
  list-column `$isochrone` cell becomes populated where it was `<NULL>`
  in v0.3 (strictly more data; no v0.3 doc or test promised NULL).
* `summary(run_result)$rates_breakdown` slot is preserved; values are
  *more correct* in v0.4 (latent v0.3 bug fixed — users should re-compute
  any published values).
* `as_tibble(run_result, rate_first = FALSE)` and
  `options(catchmentACS.rate_first_default = FALSE)` both disable the v0.4
  rate-first lift and return the underlying long-format row order as-is. A
  once-per-session migration notice `catchmentACS_message_rate_first_changed`
  announces the default change.
* All v0.3 classed conditions still emit; v0.4 only adds new classes.
* All new options except `catchmentACS.rate_first_default` default to
  v0.3-compatible behavior; set `catchmentACS.rate_first_default = FALSE`
  to opt out of the v0.4 row-order change globally.

The single behavior-visible-on-positional-access change in v0.4 is
`as_tibble(run_result)` row order. Three safety nets guard it: per-call
`rate_first = FALSE` opt-out, global
`catchmentACS.rate_first_default = FALSE` opt-out, and the migration notice.

---

# catchmentACS 0.3.0

## Breaking changes
* v0.3 accepts cumulative isochrone rings only for pipeline
  ingestion. Annulus-style inputs must be converted explicitly with
  `cacs_rings_to_cumulative()` before validation, intersection, or
  `cacs_run(precomputed_isochrones = ...)`.
* Omitted OSRM `res` now resolves to `70L` instead of the previous
  effective behavior. Use `iso_args = list(res = 50L)` when intentionally
  reproducing v0.2-style OSRM geometry.

## Architectural changes
* Released the v0.3.0 track. `DESCRIPTION` now records `Version: 0.3.0`;
  archived project logs preserve the development-track history.
* Added explicit `ring_topology` provenance to canonical isochrone and long
  outputs, hard-reject annulus inputs, and exported
  `cacs_rings_to_cumulative()` for intentional annulus-to-cumulative
  conversion.
* Changed the effective omitted OSRM `res` default from 50 to 70 and added a
  classed once-per-session migration notice; explicit `res = 50L` and
  `res = 70L` remain silent.

## Critical fixes
* Prepared the default ACS variable catalogue for the SSI formula fix by adding
  `B19056_002` (`ssi_hh`, households with Supplemental Security Income) while
  retaining `B19056_001` (`ssi_total`), `B11001_001`, and `B23025_001`.
* **Critical:** Corrected `ssi_rate` from
  `B19056_001 / B11001_001` to `B19056_002 / B19056_001`, so it now measures
  households with Supplemental Security Income over the B19056 household
  universe instead of dividing two total-household rows. All five sanctioned
  rate formulas were audited against ACS 2023 B-table labels.
* Added opt-in derived-rate audit warnings via
  `options(catchmentACS.audit_rates = TRUE)`. Out-of-range diagnostic values
  emit `catchmentACS_warning_rate_out_of_range`; the audit is warning-only and
  off by default.
* Locked all five sanctioned rate formulas with hand-computed golden tests,
  SSI-specific regression tests, default-carrier no-NA checks, provenance/audit
  assertions, and an env-gated three-site SSI live-smoke test.
* Refreshed `inst/extdata/sample_alabama_subset.rds` to the 14-variable v0.3
  default ACS catalogue so examples and fixtures include the new SSI numerator
  carrier.
* Closed the carrier-missing default-path regression: the default
  ACS catalogue now covers every sanctioned rate numerator and denominator,
  and direct/custom pipelines that omit a carrier emit
  `catchmentACS_warning_carrier_missing` while limiting `NA` output to the
  affected derived-rate row.
* Added offline replay fixtures from the v0.2 walkthroughs, including
  cache-poison and annulus-input fixtures for regression tests.

## Cache fixes
* **Critical:** Test-fixture ACS cache poisoning is now structurally
  isolated: test namespace mode writes ACS entries under `acs_test/`, while
  production reads use `acs/` and never fall back to `acs_test/`.
* **Critical:** Intersect cache keys now hash current `acs_sf` content,
  so changed ACS estimates or geometry cannot silently reuse a stale weighted
  output even when upstream provenance keys or row counts are unchanged.
* Added v0.3 cache fingerprint sidecars. Every persisted cache entry now writes
  a sibling `<key>.fingerprint` metadata file using `sha256`; missing legacy
  sidecars, malformed sidecars, corrupt RDS files, and digest mismatches
  invalidate to a cache miss instead of returning stale data.
* Added `cacs_set_cache()` and `cacs_get_cache_state()` for session/global cache
  opt-out, diagnostics, namespace status, and hit/miss counters.
* Hardened cache payload identity: isochrone keys are now 16-dimensional with
  `ring_topology`; intersect keys are now 14-dimensional with
  `iso_ring_topology`, `iso_res_param`, and content-aware ACS input hashing.
* Added `catchmentACS_warning_cache_stale_suspect` for suspicious ACS cache hits
  with a clear cache recovery recipe. Tune with
  `options(catchmentACS.stale_threshold_rows = 1000L)`, disable with `0L`, or
  override by state, e.g. `options(catchmentACS.stale_threshold_WY = 500L)`.
* Added 39 bug-grade cache regression tests for #10, #11, v0.2 legacy cache
  invalidation, cache API behavior, and cross-namespace integration.

## Condition emit
* Water-tract filtering now emits
  `catchmentACS_message_water_tract_filter` under both interactive and
  non-interactive `Rscript` sessions. Cached ACS hits written with v0.3
  condition metadata replay the water-filter condition so cache use does not
  hide dropped GEOIDs.
* Progress reporting now emits classed
  `catchmentACS_message_progress_tick` and
  `catchmentACS_message_progress_summary` conditions under `Rscript`.
  Large batches throttle tick conditions via
  `options(catchmentACS.progress_throttle = N)` while preserving the final
  tick.
* `cacs_run(verbose = TRUE)` now forwards `verbose`
  to all five sub-calls. ACS prefetch, isochrone, intersect/weight, MOE
  propagation, and rate derivation each expose progress summary conditions.
* Added the internal `.cacs_emit()` classed emit helper. Messages and warnings
  are formatted with `cli` and signaled through `rlang::inform()` /
  `rlang::warn()`, making the condition class tree the stable capture
  contract.
* Added exported `cacs_capture_conditions(expr, classes = NULL)`, which
  returns a tibble of captured catchmentACS conditions with `class`, `message`,
  `phase`, `timestamp`, and `call`.
* Added `?catchmentACS-conditions`, a top-level help page for the current
  condition class tree, custom handlers, and muting/capture patterns.
* Added `cacs_intersect_weight(keep_tract_audit = FALSE)`. When `TRUE`, the
  returned weighted long tibble carries `attr(out, "cacs_tract_audit")`, a
  six-column per-tract audit table (`site_id`, `drive_time_min`, `GEOID`,
  `area_wt`, `int_area_m2`, `tract_area_m2`). The default keeps ordinary
  outputs audit-free.

## Performance
* Root-caused the 117 s/site OSRM cold-path
  slowdown to the upstream `osrm::osrmIsochrone()` public-demo request budget:
  at `res = 70L`, `routing.openstreetmap.de` implies 4,900 grid destinations,
  66 table chunks, and about 65 seconds of forced sleep per site before network
  and geometry overhead.
* Made OSRM endpoint/profile resolution explicit and cache-keyed. The
  `osrm_mode = "docker"` path now resolves to a local OSRM endpoint by default
  (`http://0.0.0.0:5000/`, overrideable) and emits
  `catchmentACS_message_perf_fix_applied` when the fast-server path avoids the
  public-demo forced-sleep budget.
* Added OSRM request-budget provenance to `cacs_isochrone()` outputs so users
  can diagnose whether cold-run time is dominated by the public demo endpoint
  or by their own geometry/workflow choices.
* Added `cacs_get_cache_state()$counters[[1]]`, a public three-row hit/miss
  diagnostic table (`isochrone`, `acs`, `intersect`) with hit rates and
  namespace-scoped reset behavior when `cacs_clear_cache()` is called.
* Added deterministic performance-boundary tests for OSRM request
  budgets, fast-path endpoint provenance, warm-cache `cacs_run()` behavior,
  cache-counter overhead, and geometry byte identity. Live OSRM timing remains
  env-gated and diagnostic because the public demo endpoint is intentionally
  classified as over-budget at `res = 70L`.

## API polish
* `cacs_plot_site_rates()` and `cacs_plot_site_pipeline()` now honor
  the documented direct `(lat, lon)` input form by resolving coordinates to the
  nearest available `site_id` in `run_result`. Resolutions emit
  `catchmentACS_message_resolve_site`; matches more than 5 km away also emit
  `catchmentACS_warning_resolve_site_distant`.
* Documented per-urbanicity isochrone area-divergence expectations:
  urban sites should stay tight, mid-density sites get moderate headroom, and
  sparse rural sites are interpreted through tract-membership/Jaccard checks
  before raw polygon-area divergence is treated as a bug.
* Locked the urbanicity interpretation with fixture-backed urban/mid/rural
  area-tolerance bands and seam-mode tract Jaccard checks, including Greene
  County as a documented rural outlier rather than a downstream aggregation
  bug.
* Narrowed internal `sf` planar-CRS noise handling around
  catchmentACS-managed intersection calls while preserving caller-facing
  classed warnings such as geometry skips and provenance hints.
* Added exported `cacs_summary_as_markdown()` for stable
  Quarto/GitHub pipe-Markdown rendering of `summary(cacs_run(...))`
  breakdown tables without coupling to tibble's terminal-width internals.
* Added `cacs_describe()` as a read-only provenance map for `cacs_run_result`,
  long weighted/propagated/derived tibbles, isochrone/ACS `sf` objects, and
  `output = "both"` result lists. The helper reports schema, carrier, MOE,
  rate, cache, and spatial provenance without mutating the object.
* Added `cacs_validate_iso()` as a non-aborting preflight helper for v0.3
  isochrone schema issues, and rewrote the highest-friction schema/plot
  validator messages with concrete fix hints and copy-paste examples.
* Expanded `vignette("porting-v01-to-v03")` into the v0.3 migration guide,
  including executable examples for the eight v0.1 -> v0.2 script-side
  transitions and the v0.2 -> v0.3 topology/cache/formula/API changes.
* Added schema verification with offline integration tests and a
  live-OSRM smoke gate; `cacs_run(precomputed_isochrones = ...)` now validates
  the resolved isochrone schema before reading canonical status columns.

## Documentation and maintenance polish
* Documented legacy condition aliases `cacs_error_annulus_input` and
  `cacs_error_schema` as backward-compatible shims; new handlers should prefer
  canonical `catchmentACS_*` classes.
* Clarified that `cacs_get_cache_state()$counters[[1]]` exposes one public `acs`
  row that aggregates production `acs/` and test-isolated `acs_test/` cache
  activity.
* Kept the installed-package vignette-render skip self-clearing after v0.3.0
  installation; no release code change was needed.
* Confirmed R CMD check posture as `0 errors | 0 warnings | 0 notes` in the
  release environment. Hosts that cannot verify current time may emit
  `checking for future file timestamps ... NOTE unable to verify current time`;
  this is an environmental host/NTP note and is not introduced by package code.
* Preserved runnable code chunks and tables in the v0.3 porting vignette while
  deferring broader documentation-style refinements to the v0.5 documentation
  overhaul.
* Centralized the annulus-input error class chain in `.ANNULUS_ERROR_CLASSES`
  while preserving exact class order and legacy aliases.

## Upgrade guide
* For migration examples, run
  `vignette("porting-v01-to-v03", package = "catchmentACS")`. The guide covers
  the eight v0.1 -> v0.2 script-side gotchas and the v0.2 -> v0.3 changes for
  topology, cache identity, formulas, conditions, performance, and API polish.

---

# catchmentACS 0.2.0

> v0.2.0 user-feedback patch + visual-walkthrough feature release on top
> of the v0.1.0 alpha terminal state. Six feedback items closed plus a
> round of external-review concerns repaired. Hits the v0.2 coverage target
> of 80% (line) after several external review cycles.

## New features

### Leaflet visualization suite
* New leaflet visualization suite -- 5 functions for
  single-site step-by-step exploration of catchmentACS outputs:
  - `cacs_plot_site_isochrone()` (Stage 1)
  - `cacs_plot_site_intersection()` (Stage 2)
  - `cacs_plot_site_weighted()` (Stage 3)
  - `cacs_plot_site_rates()` (Stage 4)
  - `cacs_plot_site_pipeline()` (wrapper)
  + `visual-walkthrough.qmd` vignette.

  Input forms: `site_id` + `sites_df`, OR `(lat, lon)` direct, OR `site_name`
  match. Resolved via shared `.cacs_resolve_site_input()` helper.

  `leaflet` package stays in Suggests for v0.2.0; install via
  `install.packages("leaflet")` if not already present.

### Step-by-step UX devices
* `cacs_run()` return value now has class chain
  `c("cacs_run_result", "tbl_df", "tbl", "data.frame")` (preserves tibble +
  dplyr dispatch). New S3 methods:
  - `print.cacs_run_result()` -- rich cli-formatted output with provider,
    drive_times, site counts, top-3 rates with mean +/- MOE, dropped water
    GEOIDs count, wall-clock.
  - `summary.cacs_run_result()` -- returns classed `cacs_run_summary` list
    with per-rate mean / sd / n_NA, `n_tracts` fivenum, MOE-fallback
    coverage rate.
  - `print.cacs_run_summary()` -- structured summary display.
  All 11 metadata fields accessible via
  `attr(out, "cacs_run_result_metadata")`: `generated_at`, `cacs_version`,
  `provider`, `profile`, `drive_times`, `n_sites_total / success / failed`,
  `wall_clock_seconds`, `skipped_geoids`, `moe_fallback_summary` (the last
  carrying `c1_to_c2_count`, `zero_den_count`, `missing_moe_count`).
* No breaking change: all v0.1 workflows (`cacs_run(...) %>% filter(...)`,
  `ggplot(...)`, `write_csv(...)`, `View(...)`) work unchanged. The
  print method defensively degrades to the generic tibble preview when the
  metadata attribute has been stripped by an upstream dplyr verb.

## Bug fixes

### Water tract auto-filter [CRITICAL]
* **Critical:** `cacs_acs_prefetch()` now auto-drops water/special-purpose
  tracts (GEOID pattern `99xxxx` + degenerate-area fallback) by default; new
  arg `drop_water_tracts = TRUE`. Emits an inform message (condition class
  `catchmentACS_message_water_tract_filter`) listing dropped GEOIDs (capped
  at 5).
* `cacs_intersect_weight()` validator demoted from error to warn-and-skip on
  degenerate-area tracts (condition class `catchmentACS_warning_geometry_skip`);
  preserves fail-loud `catchmentACS_error_geometry` when ALL tracts are
  degenerate. Skipped GEOIDs available via `attr(result, "skipped_geoids")`.
* Fixes a water-tract filtering bug in which water tract `01003990000` aborted
  Baldwin Co. catchment computations. Concrete impact: the new default filter
  catches at least two known Alabama water tracts in
  `tigris::tracts("AL", year = 2023)` — `01003990000` (Baldwin Co. coastal) and
  `01097990000` (Mobile Co. coastal) — that previously aborted area-weighted
  intersection.

## Improvements

### Verbose progress reporting
* New verbose progress reporting across the 3 long-running tier-2
  functions: `cacs_isochrone()`, `cacs_acs_prefetch()`, `cacs_intersect_weight()`.
  Three modes auto-selected based on N + user options:
  - **silent**: when `verbose = FALSE` OR `CACS_QUIET=1` OR `options(catchmentACS.progress = "off")`
  - **bookend**: start + end summary only (default for N < 5 or N == 1)
  - **bar**: per-tick `cli` progress with ETA + rate (default for N >= 5; force via `options(catchmentACS.progress = "force")`)
  Emitted as classed conditions (`catchmentACS_message_progress_tick`,
  `catchmentACS_message_progress_summary`) for downstream capture.
* `cacs_run(verbose=TRUE)` now propagates to the long-running
  `cacs_intersect_weight()` sub-call. Users running batch site computations via
  `cacs_run()` now see per-pair progress, not just the orchestrator-level
  phase summary.

### `n_tracts_num` / `n_tracts_den` on derived rate rows
* Derived rate rows now expose `n_tracts_num` and `n_tracts_den` as
  two new integer columns showing how many census tracts contributed to each
  rate's numerator and denominator separately. The existing `n_tracts` column
  remains NA on rate rows (preserves backward compat for the `is.na(n_tracts)`
  filter idiom). Canonical long output schema grows from 24 to 26 columns
  (22 mandatory per `.LONG_REQUIRED_COLS` -- up from 20 in v0.1 -- plus
  4 carrier numeric columns `est_total` / `var_total_raw` / `est_mean` /
  `var_mean_raw` that `cacs_propagate_moe()` left-joins for MOE math).
  Carrier tibble grows from 9 to 10 columns (`n_tracts` added -- internal).

## Cleanups

### `tigris::tracts(returnclass=)` deprecation
* Removed deprecated `returnclass = "sf"` argument from internal
  `tigris::tracts()` call in `R/isochrone-osrm.R`. Tigris 2.0+ always returns
  sf objects; the argument is a no-op that emits 3 deprecation warnings per
  `cacs_isochrone()` call. Lock-in via `tests/testthat/test-unit-no-deprecated-args.R`.

## Internal
* `weight_sum` column was already exposed in v0.1 long output;
  v0.2 added an explicit regression test (`test-unit-weight-sum-exposed.R`)
  to lock it as part of the v0.2.0 contract.
* New condition classes: `catchmentACS_message_water_tract_filter`, `catchmentACS_warning_geometry_skip`, `catchmentACS_message_progress_tick`, `catchmentACS_message_progress_summary`. Plot input failures use the existing schema condition family (`catchmentACS_error_schema`) in v0.2.0 rather than a separate plot-input class.
* `leaflet` remains a Suggests dependency for v0.2.0 (the visualization
  suite); v0.5 may revisit promotion to Imports based on adoption data.
  Internal `.assert_leaflet_available()` helper provides fail-loud guard
  for the 5 `cacs_plot_site_*` functions in the visualization suite.

## Schema changes
* Canonical long output: 24 -> 26 cols (22 mandatory per
  `.LONG_REQUIRED_COLS`, up from 20 in v0.1, + 4 carrier numeric columns
  left-joined by `cacs_propagate_moe()` for MOE math). Added
  `n_tracts_num` + `n_tracts_den` after `n_tracts`.
* Carrier tibble: 9 -> 10 cols (`n_tracts` added for derive-rates lookup)
* `cacs_run()` return value gains class chain `c("cacs_run_result", "tbl_df", "tbl", "data.frame")` (preserves tibble dispatch)

## Breaking changes
* None planned.

---

# catchmentACS 0.1.0

* Initial alpha release, developed over several external review cycles.
* Tier-1 (6 fns): `cacs_isochrone`, `cacs_acs_prefetch`, `cacs_intersect_weight`, `cacs_propagate_moe`, `cacs_derive_rates`, `cacs_run`.
* Tier-2 (6 fns): `cacs_se_to_moe`, `cacs_moe_to_se`, `cacs_cache_dir`, `cacs_clear_cache`, `cacs_cache_status`, `cacs_acs_validate`.
* Data: `cacs_alabama_sites`, `cacs_acs_default_vars`, `cacs_acs_default_rates`.
* `R CMD check` 0E / 0W / 0N on `--no-manual`.
* 827 PASS tests + 3 expected env-gated SKIP.
* Coverage 76.95% (line) / 78.68% (expression).
* Sample vignette `vignettes/getting-started.qmd`.
* 3 GitHub Actions YAML workflows (R-CMD-check, regression, test-coverage).
