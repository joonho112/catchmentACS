# catchmentACS-conditions.R - the help page that lists the classes of the
# package's messages, warnings, and errors. No code but the NULL below.


#' Condition classes for messages, warnings, and errors
#'
#' The messages, warnings, and errors of catchmentACS are conditions (R's
#' term for all three) with classes that say what happened. Code can
#' therefore select them by class instead of by the text of the message,
#' with [cacs_capture_conditions()] or with the base R functions described
#' in the "Handling conditions by class" section.
#'
#' The class vector of a condition starts with a class of its own, such as
#' `catchmentACS_warning_partial`. It is followed by the class of its kind
#' (`catchmentACS_message`, `catchmentACS_warning`, or
#' `catchmentACS_error`), then `catchmentACS_condition`, and last the
#' classes that rlang and R add, such as `rlang_warning`, `warning`, and
#' `condition`. Some conditions also have a class shared by a group of
#' related conditions, between their own class and the class of their kind
#' (see the lists below). A handler for a class receives every condition
#' that has it, so a handler for `catchmentACS_warning` receives every
#' warning of the package.
#'
#' A few conditions do not have these classes. Some argument checks give a
#' plain R error, such as a value of `output` in [cacs_run()] that is not
#' one of the choices, and a missing osrm or openrouteservice package gives
#' an error of class `rlib_error_package_not_found` from rlang. Messages and
#' warnings from other packages keep their own classes.
#'
#' @section Messages:
#' \describe{
#'   \item{`catchmentACS_message_progress`}{Messages that report on the
#'     steps as they run. Progress lines also have the class
#'     `catchmentACS_message_progress_tick`, and summary lines
#'     `catchmentACS_message_progress_summary` (see the "Progress messages"
#'     section of [cacs_run()]).}
#'   \item{`catchmentACS_message_cache`}{Messages about the cache, such as
#'     those saying that a saved result was read or saved, the message that
#'     [cacs_acs_prefetch()] shows before it downloads, the messages of
#'     [cacs_set_cache()], and the messages of [cacs_clear_cache()] that
#'     report its result. When a saved result fails the check described in
#'     [cacs_cache_dir()] and is
#'     deleted, the message also has the class
#'     `catchmentACS_message_cache_legacy_invalidated` if its checksum file
#'     is missing, and `catchmentACS_message_cache_fingerprint_mismatch`
#'     otherwise. This message is given while the saved result is looked
#'     for, inside a call to `suppressMessages()`, so it is never shown, and
#'     neither a handler nor [cacs_capture_conditions()] receives it.
#'     `catchmentACS_message_cache_enabled_announce` is for a notice, off by
#'     default, that the cache is on. It is hidden in the same way when it
#'     is given during a lookup.}
#'   \item{`catchmentACS_message_water_tract_filter`}{The message of
#'     [cacs_acs_prefetch()] that lists the water tracts it removed (see its
#'     "Water tracts" section).}
#'   \item{`catchmentACS_message_demo_budget_protected`}{The message of
#'     [cacs_isochrone()] that `res` is 30 because it was not given (see its
#'     "OSRM grid resolution" section).}
#'   \item{`catchmentACS_message_res_default_changed`}{The same message when
#'     `res` is 70.}
#'   \item{`catchmentACS_message_perf_fix_applied`}{The message of
#'     [cacs_isochrone()], with `verbose = TRUE`, that the Open Source
#'     Routing Machine (OSRM) server in use is not the public demo server, so
#'     there is no wait between requests.}
#'   \item{`catchmentACS_message_listcol_iso_filled`}{The message of
#'     [cacs_run()] about the `isochrone` column, with
#'     `output = "list_column"` or `"both"`.}
#'   \item{`catchmentACS_message_rate_first_changed`}{The message, once per R
#'     session, that [as_tibble.cacs_run_result()] has put the rate rows first
#'     because of `options(catchmentACS.rate_first_default = TRUE)`. A call
#'     with `rate_first = TRUE` gives no such message.}
#'   \item{`catchmentACS_message_resolve_site`}{The message of
#'     [cacs_plot_site_rates()] and [cacs_plot_site_pipeline()] that, given
#'     `lat` and `lon` instead of `site_id`, they use the nearest site.}
#' }
#'
#' @section Warnings:
#' \describe{
#'   \item{`catchmentACS_warning_runtime`}{A warning about a problem that
#'     does not stop the step, for example sites whose routing failed,
#'     geometry that was repaired, or a Census download that is tried again.
#'     The warnings of [cacs_propagate_moe()] and [cacs_derive_rates()] that
#'     count the rows whose margin of error used another formula or is `NA`
#'     also have this class. The "Rates that are NA" section of
#'     [cacs_derive_rates()] says when its warning also has the class
#'     `catchmentACS_warning_carrier_missing`.}
#'   \item{`catchmentACS_warning_rate_out_of_range`}{The warnings of the
#'     range check on rates that `options(catchmentACS.audit_rates = TRUE)`
#'     turns on (see [cacs_derive_rates()]).}
#'   \item{`catchmentACS_warning_partial`}{The warning of [cacs_run()] when
#'     some, but not all, site and drive-time pairs have no drive-time area
#'     because routing failed or gave an empty area. The rows of those pairs
#'     are `NA`, and the `cacs_run_warnings` attribute of the result lists
#'     the pairs.}
#'   \item{`catchmentACS_warning_geometry_skip`}{The warning of
#'     [cacs_intersect_weight()] that it skipped tracts whose area is zero or
#'     not finite (step 3 in its Details).}
#'   \item{`catchmentACS_warning_provenance`}{A warning that an argument or a
#'     column was dropped, ignored, or renamed, such as an unknown name in
#'     `iso_args` or another list of arguments of [cacs_run()].
#'     [cacs_intersect_weight()] also gives this class to its warning that
#'     the drive-time areas extend beyond the tracts in the American
#'     Community Survey (ACS) data (step 1 in its Details).}
#'   \item{`catchmentACS_warning_variable`}{The warning of
#'     [cacs_acs_prefetch()] that it skipped variable codes that are not in
#'     the ACS variable list for the year.}
#'   \item{`catchmentACS_warning_cache_stale_suspect`}{The warning that saved
#'     ACS data read from the cache have fewer rows than expected (see the
#'     "Cache behavior" section of [cacs_acs_prefetch()]). It also has the
#'     class `catchmentACS_warning_cache`.}
#'   \item{`catchmentACS_warning_resolve_site_distant`}{The warning, given
#'     with `catchmentACS_message_resolve_site`, that the nearest site is
#'     more than 5 km from `lat` and `lon`.}
#'   \item{`catchmentACS_warning_provider_quota_exhausted`}{The warning of
#'     [cacs_validate_osrm_endpoint()] that the OSRM server answered that its
#'     request limit has been reached (HTTP status 429).}
#' }
#'
#' @section Errors:
#' \describe{
#'   \item{`catchmentACS_error_schema`}{An argument or an input table is not
#'     in the expected form, such as a value of the wrong type, a missing
#'     column, or a value out of range. Most errors from argument checks
#'     have this class.}
#'   \item{`catchmentACS_error_annulus_input`}{The drive-time areas are bands
#'     between two drive times, with `isomin` above 0 or
#'     `ring_topology = "annulus"`, which [cacs_intersect_weight()] and
#'     [cacs_run()] do not accept ([cacs_rings_to_cumulative()] converts
#'     them). These errors also have the class `catchmentACS_error_schema`
#'     and two older classes, `cacs_error_annulus_input` and
#'     `cacs_error_schema`, kept so that code written with them still works.
#'     No other condition has the older classes.}
#'   \item{`catchmentACS_error_credential`}{A Census or openrouteservice API
#'     key is missing, or a routing service refused the requests as
#'     unauthorized (HTTP status 401 or 403). The same class is given for
#'     choices that are not implemented yet: `provider = "mapbox"` or
#'     `"r5r"`, and `weight_method = "population"`.}
#'   \item{`catchmentACS_error_network`}{The Census download failed three
#'     times.}
#'   \item{`catchmentACS_error_variable`}{None of the requested variable
#'     codes is in the ACS variable list for the year.}
#'   \item{`catchmentACS_error_operator`}{The work cannot go on for a reason
#'     other than the form of the arguments. For example, the Census API or
#'     the routing service refused the requests, the cache folder cannot be
#'     created, the data lie outside the area that [cacs_intersect_weight()]
#'     accepts, or no site and drive-time pair has a drive-time area in
#'     [cacs_run()].}
#'   \item{`catchmentACS_error_geometry`}{The area weights cannot be
#'     computed: every tract has zero or non-finite area, the overlap of a
#'     tract and a drive-time area cannot be computed, or invalid geometry
#'     cannot be repaired.}
#'   \item{`catchmentACS_error_missing_suggest`}{A package that the function
#'     needs, but that catchmentACS only suggests, is not installed: leaflet
#'     for the `cacs_plot_site_*()` map functions, or knitr for
#'     [cacs_summary_as_markdown()].}
#' }
#'
#' @section Handling conditions by class:
#' [cacs_capture_conditions()] records the messages and warnings of the
#' package in a table instead of showing them. The base R functions for
#' conditions take the same class names (see [conditions][base::conditions]).
#' A handler for a class in `withCallingHandlers()` lets the code go on, and
#' one in `tryCatch()` stops the code at the first condition of the class.
#' `suppressMessages()` and `suppressWarnings()` can hide only the messages
#' or warnings of the classes named in their `classes` argument, as in
#' `suppressMessages(expr, classes = "catchmentACS_message_progress")`.
#' [cacs_run()] also keeps the warnings of each step in the
#' `cacs_run_warnings` attribute of its result.
#'
#' @examples
#' # The error that cacs_acs_validate() gives when the ACS data are not an sf
#' # object, kept here to show its classes
#' err <- tryCatch(cacs_acs_validate(data.frame(GEOID = "01001020100")),
#'                 error = function(e) e)
#' class(err)
#'
#' # A handler for one class of the package
#' tryCatch(
#'   cacs_acs_validate(data.frame(GEOID = "01001020100")),
#'   catchmentACS_error_schema = function(e) "not in the expected form"
#' )
#' @family validation and conditions
#' @name catchmentACS-conditions
#' @aliases catchmentACS_condition catchmentACS_message catchmentACS_warning
#' @aliases catchmentACS_error catchmentACS-conditions
NULL
