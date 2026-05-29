# testthat ID taxonomy

Most `test_that()` labels in this package start with a short ID prefix. The
prefix is an audit aid: it tells reviewers which contract, bug family, or
implementation phase the test protects. Prefer adding new tests to an existing
prefix family when the new assertion extends the same contract.

## Current prefix families

| Prefix | Scope | Typical files |
|---|---|---|
| `T-P2-SCHEMA-` | Phase 2 schema locks: canonical isochrone/long schemas, condition classes, preflight issue shape, replay upgrade checks. | `test-phase2-schema-locks.R` |
| `T-P2-GOLDEN-` | Phase 2 golden compatibility checks against v0.2 weighted-output baselines. | `test-phase2-schema-locks.R` |
| `T-P2-COVERAGE-` | Phase 2 branch coverage for validator and rings-helper edge cases. | `test-phase2-schema-locks.R` |
| `T-P2-PIPE-` | Phase 2 pipeline integration checks for `cacs_run()` bypass paths and topology propagation. | `test-integration-phase2-pipeline.R` |
| `T-P2-LIVE-OSRM-` | Env-gated live OSRM smoke tests for the v0.5 demo-safe omitted `res = 30L` policy. | `test-smoke-live-osrm-demo-default.R` |
| `T-UF1-RINGS-` | UF-1 annulus-to-cumulative conversion helper tests. | `test-unit-cacs-rings-to-cumulative-helper.R` |
| `T-UF1-VAL-` | UF-1 cumulative-only validator tests and annulus hard-rejection behavior. | `test-unit-cumulative-rings-validator.R` |
| `T-P3-CACHE-FP-` | Phase 3 cache fingerprint sidecar contract. | `test-unit-cache-fingerprint.R` |
| `T-P3-CACHE-ISO-` | Phase 3 ACS `acs_test/` namespace isolation. | `test-unit-cache-isolation.R` |
| `T-P3-CACHE-KEY-` | Phase 3 cache-key payload dimensions and content-aware invalidation. | `test-unit-cache-key-payload.R` |
| `T-CACHE-EDGE-` | Cache fingerprint and payload edge cases. | `test-unit-cache-fingerprint-edge-cases.R` |
| `T-CACHE-OPT-` | Public cache-control API behavior and options/env switches. | `test-unit-cache-opt-out.R` |
| `T-CACHE-STALE-` | Suspicious ACS cache-hit warning behavior. | `test-unit-cache-stale-warning.R` |
| `T-CACHE-` | Original cache unit contract. | `test-unit-cache.R` |
| `CACHE-API-` | Public cache API tests introduced before the `T-CACHE-*` split. | `test-unit-cache-api.R` |
| `BUG010-` | Critical cache-poisoning regression family. | `test-bug010-cache-poisoning.R` |
| `BUG011-` | Critical stale-intersect regression family. | `test-bug011-stale-intersect.R` |
| `P4-SSI-` | Phase 4 SSI formula correction and old-formula exclusion. | `test-bug012-ssi-formula.R` |
| `C09-` | Phase 4 C-09 carrier-missing closure and side-effect checks. | `test-c09-carrier-missing-derive-rates.R` |
| `P4-GOLD-` | Phase 4 hand-computed five-rate golden tests. | `test-golden-5rate-hand-computed.R` |
| `P4-CANON-` | Phase 4 sanctioned rate catalogue and audit-bound contract. | `test-integration-rate-formulas-canonical.R` |
| `P4-NONA-` | Phase 4 carrier-complete default-path no-NA contract. | `test-integration-derive-rates-no-na.R` |
| `P4-PROV-` | Phase 4 rate provenance and audit metadata. | `test-phase4-rate-provenance-audit.R` |
| `P4-SIDE-` | Phase 4 side-effect tests proving SSI/carrier changes do not mutate unrelated rates. | `test-side-effect-other-rates-unchanged.R` |
| `P4-SMOKE-SSI-` | Env-gated live SSI smoke tests. | `test-smoke-3sites-ssi-bounds.R` |
| `T-DESC-` | `cacs_describe()` exported helper and method behavior. | `test-unit-cacs-describe.R` |
| `T-VALIDATOR-MSG-` | User-facing schema/plot validator message rewrites. | `test-unit-validator-messages-rewrite.R` |

## Fixture and gate roles

The packaged offline fixtures are deliberately split by purpose. The
`legacy_2025_*` files and `sample_alabama_subset.rds` protect the synthetic
regression contract without network calls. `visual_walkthrough_fixture.rds`
supports map documentation only and should not be treated as a golden baseline.
Live provider/Census smoke tests stay env-gated so routine checks remain
deterministic. Current opt-in gates are:

| Gate | Scope | Used by |
|---|---|---|
| `CACS_LIVE_PROVIDER=true` | Generic live-provider helper; paired with provider-specific credentials when needed. | `helper-skip.R`, `test-unit-acs-validate.R` |
| `CACS_LIVE_OSRM=1` | Live OSRM isochrone smoke tests. | `test-smoke-live-osrm-demo-default.R`, `test-smoke-live-osrm-budget-boundary.R`, `test-smoke-3sites-*.R` |
| `CATCHMENTACS_LIVE_OSRM=1` | Live OSRM endpoint preflight probe. | `test-issue002-osrm-endpoint-validate.R` |
| `CACS_LIVE_CENSUS=1` plus `CENSUS_API_KEY` | Live Census + OSRM workflow smoke tests. | `test-smoke-3sites-3cachemodes.R`, `test-smoke-3sites-ssi-bounds.R` |
| `CATCHMENTACS_LIVE_SMOKE=true` | Broader legacy live smoke opt-in. | `test-emit-smoke-real-world.R`, `test-smoke-live-osrm-budget-boundary.R` |
| `CENSUS_API_KEY` | Required for live Census cache misses and `force_refresh = TRUE`; not enough by itself for tests that also require `CACS_LIVE_PROVIDER` or `CACS_LIVE_CENSUS`. | `helper-skip.R`, `test-unit-acs-validate.R`, live smoke tests |

## Legacy section prefixes

The older `T19-`, `T20-`, `T21-`, `T22-`, `T23-`, and `T24-` families map to
the original numbered design sections:

| Prefix | Scope |
|---|---|
| `T19-` | Isochrone dispatch, provider passthrough, normalization, and provider-side provenance. |
| `T20-` | ACS prefetch validation, cache behavior, suppression warnings, and provenance. |
| `T21-` | Intersection/weighting, spatial guards, carrier preservation, and composite cache hits. |
| `T22-` | MOE propagation families and fallback semantics. |
| `T23-` | Derived-rate validation, formula dispatch, carrier lookup, and row cardinality. |
| `T24-` | `cacs_run()` orchestration, bypass paths, partial success, output modes, and run provenance. |

`V02-CACHE-` marks v0.2 legacy cache compatibility tests. Keep this prefix for
tests that specifically preserve migration behavior for old cache files.

Other older or narrow prefixes remain valid where they already exist, including
`PERF-CACHE-` for local performance budgets, `T-STUB-MAPBOX` / `T-STUB-R5R` for
deferred provider stubs, and `Testcase N:` labels in the original water-tract
regression file. Treat these as legacy/allowed forms; new tests should prefer a
normalized phase, bug, or domain prefix from the tables above.

## Choosing a prefix for new tests

Use the narrowest existing family that matches the behavior under test:

- New schema/topology lock: `T-P2-SCHEMA-`, `T-UF1-RINGS-`, or `T-UF1-VAL-`.
- New cache fingerprint, namespace, key, stale-warning, or API assertion:
  `T-P3-CACHE-*`, `T-CACHE-*`, `BUG010-`, or `BUG011-`.
- New formula, rate, carrier, or SSI assertion: `P4-*` or `C09-`.
- New describe helper behavior: `T-DESC-`.
- New user-facing diagnostic text: `T-VALIDATOR-MSG-`.
- New legacy section behavior that predates v0.3 phase labels: continue the
  relevant `T19-` through `T24-` family.

Within a family, keep IDs monotone and do not renumber older tests. Suffixes
such as `b` or `B` are acceptable for small follow-on assertions that belong
next to an existing test. For new bug-grade regressions, prefer a named bug
family such as `BUG012-` over adding an unrelated assertion to a broad unit
file.
