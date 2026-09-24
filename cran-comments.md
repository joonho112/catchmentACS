## Submission

This is a new submission.

## Test environments

* local: macOS 26.6.2 (aarch64), R 4.6.0
* win-builder: R-devel (2026-09-21 r90579 ucrt) and R 4.6.1
* mac builder: macOS 26.6 (arm64), R 4.6.1 Patched
* R-hub, R-devel: Ubuntu 24.04 (linux), Ubuntu 22.04 (donttest), Windows
  (windows), and Fedora 42 with only the packages in Depends, Imports, and
  VignetteBuilder, and testthat for the tests (nosuggests)
* GitHub Actions:
  * ubuntu-latest: R 4.6.1 and R-devel
  * windows-latest: R 4.6.1
  * macos-latest: R 4.6.1
  * ubuntu-latest, R 4.6.1, with the packages in Depends and Imports and only
    those packages in Suggests that R CMD check needs (testthat, knitr, and
    rmarkdown)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

On win-builder, the note also lists words in DESCRIPTION as possibly
misspelled. These are correct: ACS (American Community Survey, written out in
the Description), isochrone and Isochrone (the area reachable within a given
drive time, defined in the Description), and pre (from "pre-kindergarten").

On the local machine only, a second note says that its HTML Tidy is too old to
validate the HTML help pages. The HTML manual passes on win-builder.

## Examples

The examples in six help pages download data from the Census Bureau, for which
an API key is needed; they are wrapped in `\dontrun{}`. So are the examples in
`?cacs_isochrone` and `?cacs_validate_osrm_endpoint` that send requests to the
public OSRM demo server: they need no key, but the server is a shared service
that limits the requests it accepts, and the example in `?cacs_isochrone`
alone sends several dozen. Each help page with such an example also has
examples that run without a key or an internet connection.

## Files written by the package

By default, the package saves results for reuse in a folder inside
`tempdir()`, which R deletes at the end of the session. A folder that lasts
between sessions, such as `tools::R_user_dir("catchmentACS", "cache")`, is
used only when the user sets the option `catchmentACS.cache_dir` or the
environment variable `CACS_CACHE_DIR`. The examples, tests, and vignettes
write only inside `tempdir()`.

## Other notes

* The values `"mapbox"` and `"r5r"` of `provider` in `cacs_isochrone()` and
  `cacs_run()`, and `"population"` of `weight_method` in
  `cacs_intersect_weight()` and `cacs_run()`, together with the arguments used
  only with them, are not implemented in this version. With one of these
  values, the functions stop with an error that says so, before any request
  is sent; the help pages and NEWS.md say the same. A few arguments that no
  longer have an effect, such as `confirm` in `cacs_set_cache()`, are kept so
  that code written for the earlier versions on GitHub still runs; their help
  says that they have no effect.
* `cacs_describe()` shows its description by calling `print()` on the object
  it returns. The print method writes with the cli package, as messages that
  `suppressMessages()` hides.
* `cacs_set_cache()` is the function for setting the option
  `catchmentACS.cache_enabled`; its example restores the option.

## Reverse dependencies

This is a new package, so there are no reverse dependencies.
