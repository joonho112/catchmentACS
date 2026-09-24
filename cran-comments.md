## Submission

This is a new submission.

## Test environments

* local: macOS 26.6.2 (aarch64), R 4.6.0
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

On the local machine only, a second note says that its HTML Tidy is too old to
validate the HTML help pages.

## Examples

The examples that download data from the Census Bureau, which needs an API
key, or that send requests to the public OSRM demo server, which limits the
requests it accepts, are wrapped in `\dontrun{}`. Each help page with such an
example also has examples that run, using data included in the package.

## Files written by the package

By default, the package saves results for reuse in a folder inside
`tempdir()`, which R deletes at the end of the session. A folder that lasts
between sessions, such as `tools::R_user_dir("catchmentACS", "cache")`, is
used only when the user sets the option `catchmentACS.cache_dir` or the
environment variable `CACS_CACHE_DIR`. The examples, tests, and vignettes
write only inside `tempdir()`.

## Reverse dependencies

This is a new package, so there are no reverse dependencies.
