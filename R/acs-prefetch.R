# acs-prefetch.R - cacs_acs_prefetch() and the internal helpers it calls.
#
# The function takes one state, the ACS 5-year estimates, and census tracts;
# other values stop with an error. It checks its arguments, resolves the
# variable codes, and returns a saved result when the request matches one. With
# no saved result it needs a Census API key: it reads the ACS variable list,
# downloads through tidycensus (up to three attempts), checks the columns,
# removes water tracts, sets negative margins of error to NA, attaches the
# record of the request, and saves the result.


# Water tracts are numbered 9900 and higher, so their six-digit tract code
# begins with "99". An 11-digit GEOID is STATE(2) + COUNTY(3) + TRACT(6),
# which puts those two digits in positions 6 and 7. `.drop_water_tracts()`
# uses this pattern together with an area test, so a tract is still found
# when either signal alone misses it: a changed Census numbering, or a
# boundary that sf reports with no area.

.WATER_TRACT_REGEX <- "^[0-9]{2}[0-9]{3}99[0-9]{4}$"


#' Download ACS estimates for the census tracts of one state
#'
#' Downloads American Community Survey (ACS) 5-year estimates for the census
#' tracts of one state, with the tract boundaries, using
#' `tidycensus::get_acs()`. Each estimate comes with the margin of error
#' published with it, the half-width of its 90 percent confidence interval.
#' The function removes water tracts unless `drop_water_tracts = FALSE`, and
#' it returns negative margins of error as `NA` whatever the other arguments
#' are (see the "Water tracts" and "Missing estimates and margins of error"
#' sections). Unless the cache is turned off, the result is also saved in the
#' cache folder and reused by later calls with the same request (see the
#' "Cache behavior" section).
#'
#' @param state A string giving one state, the District of Columbia, or
#'   Puerto Rico, as a two-letter USPS abbreviation (such as `"AL"`) or a
#'   two-digit FIPS code (such as `"01"`). A vector of states, or a lowercase
#'   code such as `"al"`, gives an error.
#' @param year A single whole number giving the last year of the ACS 5-year
#'   estimates, from 2009 to 2024; the default is 2023 (the 2019-2023
#'   estimates).
#' @param variables A character vector of ACS variable codes, or `NULL` (the
#'   default) for [`cacs_acs_default_vars`]. Each code must have the form of
#'   `"B19013_001"` (the letter B, five digits, an underscore, and three
#'   digits); other codes, such as `"B01001A_001"` or `"C17002_001"`, give an
#'   error. `"core"` and `"extended"` select the same variables as `NULL`. For
#'   codes that are not in the ACS variable list for `year`, see the
#'   "Downloading from the Census Bureau" section. The Details of
#'   [cacs_intersect_weight()] say how each code is combined; a median or
#'   per-person value from a table other than `B19013`, `B25077`, or `B19301`
#'   is added up like a count.
#' @param survey A string giving the survey. Only `"acs5"` (the default), the
#'   ACS 5-year estimates, is accepted.
#' @param geography A string giving the geographic level. Only `"tract"` (the
#'   default), census tracts, is accepted.
#' @param cache_dir A path to the cache folder, or `NULL` (the default) to
#'   use [cacs_cache_dir()]; see the "Cache behavior" section. A folder
#'   outside the temporary folder of the R session is tidied as described in
#'   [cacs_cache_dir()]. [cacs_clear_cache()] clears only the folder that
#'   [cacs_cache_dir()] returns.
#' @param write_gpkg A logical value, `FALSE` (the default) or `TRUE`. If
#'   `TRUE`, a downloaded result is also written to a GeoPackage file
#'   (`.gpkg`) in the `acs` folder of the cache folder (`cache_dir`, or
#'   [cacs_cache_dir()] when `cache_dir` is `NULL`), for use in other GIS
#'   software. The file is written even when the cache is turned off, but not
#'   when a saved result is read. It is named after the cache key,
#'   `attr(result, "cacs_provenance")$cache_key`, with the extension `.gpkg`.
#'   By default the cache folder is inside the temporary folder of the R
#'   session, so the file is deleted when the session ends; in a cache folder
#'   that lasts between sessions it is deleted together with the saved result
#'   (see [cacs_cache_dir()]), and [cacs_clear_cache()] also deletes it. To
#'   keep a copy, write the result with [sf::st_write()].
#' @param force_refresh A logical value. If `TRUE`, the data are downloaded
#'   even when a saved result exists, and the new result replaces it unless
#'   the cache is turned off. With `FALSE` (the default), a saved result is
#'   read when there is one; see the "Cache behavior" section.
#' @param drop_water_tracts A logical value. If `TRUE` (the default), tracts
#'   numbered 9900 or higher and tracts whose boundary has no area are
#'   removed; if `FALSE`, they are kept unless they were already removed from
#'   a saved result. See the "Water tracts" section.
#' @param verbose A logical value. With `TRUE` (the default), the function
#'   shows a message before the download and a summary line when it
#'   finishes, and other messages report reading a saved result, writing
#'   files, and removing water tracts. `FALSE` turns off these messages;
#'   warnings are still given. See the "Progress messages" section of
#'   [cacs_run()] for how to turn the summary line off.
#'
#' @return An `sf` data frame with one row for each tract and variable, and
#'   the columns `GEOID` (the 11-digit tract identifier: two digits for the
#'   state, three for the county, and six for the tract), `NAME` (the tract
#'   name), `variable` (the ACS variable code), `estimate`, `moe` (the margin
#'   of error), and `geometry` (the tract boundary, in NAD83, EPSG:4269).
#'
#'   The attributes `cacs_provenance` and `cacs_acs_provenance` hold the
#'   same list, a record of how the result was produced. It gives the
#'   request: the state, the year, the survey, the geography, and the
#'   variable codes. It counts the variables, tracts, rows, missing
#'   estimates, and margins of error set to `NA`. It also holds the cache
#'   key, the time the result was created (in UTC), and the tidycensus and
#'   sf versions. A result read from the cache keeps the list from the
#'   original download. The attribute `cacs_schema_version` is the version
#'   label (`"1.0"`) of the column layout.
#'
#' @section Downloading from the Census Bureau:
#' Reading a saved result needs neither a Census API key nor an internet
#' connection. The data are downloaded when no saved result is found or when
#' `force_refresh = TRUE`. A download needs a Census API key in the
#' `CENSUS_API_KEY` environment variable, which
#' `tidycensus::census_api_key()` can set; if it is missing or empty, the
#' function stops before any request is made. Variable codes that are not in
#' the ACS variable list for the year are skipped with a warning, and the
#' function stops if none is left. A failed download is tried up to three
#' times, except that an HTTP 4xx error, such as a bad request, stops the
#' function at once. The downloaded table is checked with the same rules as
#' [cacs_acs_validate()].
#'
#' @section Water tracts:
#' When `drop_water_tracts = TRUE` (the default), two kinds of tracts are
#' removed right after the download. The first are tracts numbered 9900 or
#' higher (a `GEOID` whose last six digits begin with `99`), which the
#' package treats as water or special-purpose tracts. The second are tracts
#' whose boundary has zero, negative, or non-finite area, such as an empty
#' boundary, because an area weight cannot be computed for them. A message
#' lists the removed `GEOID`s (the first five, followed by the number of
#' others) unless `verbose = FALSE`.
#'
#' The same rule is applied again when a saved result is read from the cache,
#' but tracts removed before the result was saved stay removed even with
#' `drop_water_tracts = FALSE`. They come back only with
#' `force_refresh = TRUE` or after the saved result is deleted, for example
#' with [cacs_clear_cache()]. With
#' `drop_water_tracts = FALSE`, [cacs_intersect_weight()] skips tracts with
#' zero area, with a warning.
#'
#' @section Missing estimates and margins of error:
#' Negative values in the margin-of-error column, which the Census Bureau's
#' data API uses as codes rather than margins of error, are returned as
#' `NA`; the function does not change estimates. For example, `-555555555`
#' means that a margin of error is not appropriate because the estimate is
#' controlled to an independent population or housing estimate (U.S. Census
#' Bureau, "Notes on ACS Estimate and Annotation Values"). A warning is
#' given when more than 10 percent of the rows have a missing estimate, and
#' another when more than 10 percent have `-555555555` in place of a margin
#' of error. Step 5 in the Details of [cacs_intersect_weight()] describes how
#' these `NA` values carry into the estimates for the drive-time areas.
#'
#' @section Cache behavior:
#' By default the result is saved in the `acs` folder of the cache folder
#' (`cache_dir`, or [cacs_cache_dir()] when `cache_dir` is `NULL`), with a
#' small checksum file next to it. The default cache folder lasts only for
#' the R session; [cacs_cache_dir()] says how to keep saved results between
#' sessions and how long they are kept. A later call reads the saved result when it
#' asks for the same `state` value, year, survey, geography, and variables
#' (in any order) and the installed versions of R, catchmentACS, tidycensus,
#' tigris, and sf have not changed. Otherwise the data are downloaded again
#' (`state = "AL"` and `state = "01"`, for example, are saved separately).
#' A saved result that fails its checksum check is deleted and downloaded
#' again.
#'
#' A saved result that is read back with fewer than 1,000 rows gives a warning
#' that it may be incomplete, once per session and only for copies with at
#' least six variables (the `catchmentACS.stale_min_variables` option). The
#' row limit is set with the option `catchmentACS.stale_threshold_rows` (0
#' turns the check off) or for one `state` value with an option such as
#' `catchmentACS.stale_threshold_WY`.
#'
#' @seealso [cacs_acs_validate()] checks ACS data obtained another way, and
#'   [cacs_get_cache_state()], [cacs_set_cache()], and [cacs_clear_cache()]
#'   show, turn on or off, and clear the cache.
#' @family steps of the calculation
#' @export
#' @examples
#' library(sf)
#' # The rows of 60 census tracts around a site in Birmingham, Alabama, taken
#' # from the 2019-2023 estimates that cacs_acs_prefetch() downloaded for the
#' # state and kept in a file that comes with the package: one row for each
#' # tract and variable
#' acs_bhm <- readRDS(system.file("extdata", "visual_walkthrough_fixture.rds",
#'                                package = "catchmentACS"))$acs_sf
#' head(acs_bhm)
#'
#' # Downloads the estimates for every tract in Alabama, which needs a Census
#' # API key.
#' \dontrun{
#' al <- cacs_acs_prefetch(state = "AL", year = 2023)
#' }
cacs_acs_prefetch <- function(state,
                              year = 2023L,
                              variables = NULL,
                              survey = "acs5",
                              geography = "tract",
                              cache_dir = NULL,
                              write_gpkg = FALSE,
                              force_refresh = FALSE,
                              drop_water_tracts = TRUE,
                              verbose = TRUE) {

  if (!is.character(state)) {
    .cli_abort_schema(c(
      "{.arg state} must be {.cls character(1)} USPS code or 2-digit FIPS.",
      "x" = "Got class {.cls {class(state)[1L]}}.",
      "i" = "Pass a single string such as {.val AL} or {.val 01}."
    ))
  }

  if (length(state) != 1L) {
    .cli_abort_schema(c(
      "{.arg state} must be a single string in the current release.",
      "x" = "Got {.cls character} of length {.val {length(state)}}: {.val {utils::head(state, 5L)}}.",
      "i" = "For several states, call {.fn cacs_acs_prefetch} once for each state and combine the results with {.fn dplyr::bind_rows}."
    ))
  }

  if (is.na(state) || nchar(state) != 2L) {
    .cli_abort_schema(c(
      "{.arg state} must be exactly 2 characters wide (USPS code or 2-digit FIPS).",
      "x" = "Got {.val {state}} (nchar = {.val {if (is.na(state)) NA_integer_ else nchar(state)}})."
    ))
  }

  # The cache key holds `state` as it was given, so "al" and "AL" would be
  # saved separately; the function asks for the upper-case form instead of
  # changing it.
  if (state != toupper(state)) {
    .cli_abort_schema(c(
      "{.arg state} must be uppercase.",
      "i" = "Use {.val {toupper(state)}} instead."
    ))
  }

  .validate_state_in_fips(state)

  year_range <- .cacs_acs5_year_range()
  if (length(year) != 1L || is.na(year) ||
      !is.numeric(year) || !is.finite(year)) {
    .cli_abort_schema(c(
      "{.arg year} must be a finite numeric scalar.",
      "x" = "Got {.cls {class(year)[1L]}} of length {.val {length(year)}}."
    ))
  }
  if (!isTRUE(year == floor(year))) {
    .cli_abort_schema(c(
      "{.arg year} must be an integer-valued ACS5 end year.",
      "x" = "Got {.val {year}}.",
      "i" = "Use a whole-number year such as 2024."
    ))
  }
  if (year < year_range[["min"]] || year > year_range[["max"]]) {
    .cli_abort_schema(c(
      "{.arg year} must be between {year_range[['min']]} and {year_range[['max']]}.",
      "x" = "Got {.val {year}}.",
      "i" = "ACS 5-year estimates begin with {year_range[['min']]}; {year_range[['max']]} is the latest year tried with this version of catchmentACS."
    ))
  }
  year <- as.integer(year)

  if (!is.character(survey) || length(survey) != 1L || is.na(survey)) {
    .cli_abort_schema(c(
      "{.arg survey} must be a {.cls character(1)} value."
    ))
  }
  if (!identical(survey, "acs5")) {
    .cli_abort_schema(c(
      "{.arg survey} must be {.val acs5} in the current release.",
      "x" = "Got {.val {survey}}.",
      "i" = "ACS 1-year estimates ({.val acs1}) are not published for census tracts."
    ))
  }

  if (!is.character(geography) || length(geography) != 1L || is.na(geography)) {
    .cli_abort_schema(c(
      "{.arg geography} must be a {.cls character(1)} value."
    ))
  }
  if (!identical(geography, "tract")) {
    .cli_abort_schema(c(
      "{.arg geography} must be {.val tract} in the current release.",
      "x" = "Got {.val {geography}}.",
      "i" = "{.val block group} and {.val county} support are deferred to future releases."
    ))
  }

  for (nm in c("write_gpkg", "force_refresh", "drop_water_tracts", "verbose")) {
    val <- get(nm)
    if (!is.logical(val) || length(val) != 1L || is.na(val)) {
      .cli_abort_schema(c(
        "{.arg {nm}} must be {.cls logical(1)} (TRUE or FALSE).",
        "x" = "Got {.cls {class(val)[1L]}} of length {.val {length(val)}}."
      ))
    }
  }

  if (!is.null(cache_dir)) {
    if (!is.character(cache_dir) || length(cache_dir) != 1L || is.na(cache_dir)) {
      .cli_abort_schema(c(
        "{.arg cache_dir} must be {.cls character(1)} or {.code NULL}."
      ))
    }
  }


  resolved <- .resolve_variables(variables)
  variables_resolved <- resolved$vars
  vars_source        <- resolved$source
  variables_effective <- variables_resolved
  variables_skipped   <- character(0)


  # A saved result is found by a key built from the request and from the versions
  # of R and of the packages that shape the result, so a copy written under
  # other versions is not read back.
  make_cache_key <- function(vars) {
    cache_payload <- list(
      state              = state,
      year               = year,
      survey             = survey,
      geography          = geography,
      variables          = sort(unique(vars)),
      geometry_vintage   = .acs_geometry_vintage(year, geography),
      tidycensus_version = as.character(utils::packageVersion("tidycensus")),
      tigris_version     = as.character(utils::packageVersion("tigris")),
      sf_version         = as.character(utils::packageVersion("sf")),
      schema_version     = "1.0",
      package_version    = as.character(utils::packageVersion("catchmentACS")),
      r_version          = paste(R.version$major, R.version$minor, sep = ".")
    )
    .cacs_cache_key(cache_payload, namespace = "acs")
  }

  read_cached_acs <- function(key) {
    if (isTRUE(force_refresh)) {
      return(NULL)
    }
    cached <- suppressMessages(.cacs_cache_get(
      key, "acs", cache_dir = cache_dir, replay_conditions = FALSE
    ))
    if (is.null(cached)) {
      return(NULL)
    }
    cached_conditions <- .cacs_cache_conditions(cached)
    if (isTRUE(verbose)) {
      .cli_inform_cache(
        c("Cache hit for ACS {.val {year}} {.val {survey}} {.val {geography}} state {.val {state}}.",
          "i" = "{.val {nrow(cached)}} row{?s} from cached snapshot."),
        phase = "acs"
      )
    }
    # The saved result carries the record of the download it came from, and the
    # row subset below keeps it, so the result of a cache hit reports that
    # download rather than this call.
    #
    # The cache key does not include `drop_water_tracts`, because a copy per
    # value would mean a new download for each one. The filter runs again here
    # instead, so a copy saved before the filter existed also loses its water
    # tracts. Tracts removed before the copy was saved do not come back with
    # `drop_water_tracts = FALSE`; they come back with `force_refresh = TRUE`
    # or after the saved result is deleted.
    filtered <- .drop_water_tracts(
      acs_sf            = cached,
      drop_water_tracts = drop_water_tracts,
      verbose           = verbose
    )
    out_cached <- .cacs_cache_strip_conditions(filtered$kept_sf)
    if (isTRUE(verbose) && isTRUE(drop_water_tracts) &&
        length(cached_conditions) > 0L &&
        length(filtered$dropped_geoids) == 0L) {
      .cacs_cache_signal_conditions(cached_conditions)
    }
    out_cached
  }

  cache_key <- make_cache_key(variables_effective)
  cached_out <- read_cached_acs(cache_key)
  if (!is.null(cached_out)) {
    return(cached_out)
  }


  # A saved result is returned above without a Census API key. From here on the
  # function asks the Census Bureau for the variable list and then for the
  # data, so it stops now when the key is missing. If the variable list itself
  # cannot be read (no connection, or a test that replaces the download), the
  # codes are not checked and the download goes ahead; a year whose endpoint
  # does not exist is the exception and stops the function.

  if (!nzchar(Sys.getenv("CENSUS_API_KEY"))) {
    .cli_abort_credential(c(
      "{.envvar CENSUS_API_KEY} is not set.",
      "i" = "Set via {.code Sys.setenv(CENSUS_API_KEY = '<key>')} or add to {.path ~/.Renviron}.",
      "*" = "Request a key at {.url https://api.census.gov/data/key_signup.html}."
    ))
  }

  codebook_error <- NULL
  codebook <- tryCatch(
    tidycensus::load_variables(year, survey),
    error = function(e) {
      codebook_error <<- e
      NULL
    }
  )

  if (!is.null(codebook_error) &&
      isTRUE(.is_tidycensus_endpoint_not_found(codebook_error))) {
    .cli_abort_schema(c(
      "tidycensus could not load the list of ACS5 variables for {.val {year}}.",
      "x" = "Message: {.val {conditionMessage(codebook_error)}}.",
      "i" = "The Census Bureau may not publish this list for the year, or the request did not reach it; check {.code tidycensus::load_variables({year}, \"acs5\")}."
    ))
  }

  if (!is.null(codebook) && "name" %in% names(codebook)) {
    valid_mask <- variables_resolved %in% codebook$name
    n_invalid  <- sum(!valid_mask)

    if (!any(valid_mask)) {
      .cli_abort_variable(c(
        "{.arg variables} contains 0 codes valid in the tidycensus codebook for ({.val {year}}, {.val {survey}}).",
        "x" = "All {.val {length(variables_resolved)}} requested invalid: {.val {utils::head(variables_resolved, 5L)}}.",
        "i" = "Check the codes with {.code tidycensus::load_variables({year}, \"{survey}\")}."
      ))
    }

    if (!all(valid_mask)) {
      skipped <- variables_resolved[!valid_mask]
      .cli_warn_variable(c(
        "Skipping {.val {n_invalid}} invalid variable{?s} in codebook ({.val {year}}, {.val {survey}}).",
        "i" = "Skipped: {.val {utils::head(skipped, 5L)}}."
      ), phase = "acs")
      variables_effective <- variables_resolved[valid_mask]
      variables_skipped   <- skipped
    }
  }

  # Skipped codes change the request, so the key is built again and the cache
  # looked in once more.
  cache_key_after_codebook <- make_cache_key(variables_effective)
  if (!identical(cache_key_after_codebook, cache_key)) {
    cache_key <- cache_key_after_codebook
    cached_out <- read_cached_acs(cache_key)
    if (!is.null(cached_out)) {
      return(cached_out)
    }
  }


  if (isTRUE(verbose)) {
    .cli_inform_cache(
      c("i" = "Fetching ACS {.val {year}} {.val {survey}} {.val {geography}} for state {.val {state}}..."),
      phase = "acs"
    )
  }

  # One request, so the reporter prints only the summary line at the end,
  # unless `catchmentACS.progress` is set to "force". on.exit() prints that
  # line even when the attempts below end in an error.
  acs_prog <- .cacs_progress_reporter(n = 1L, label = "ACS prefetch",
                                      verbose = verbose)
  acs_prog_state <- new.env(parent = emptyenv())
  acs_prog_state$n_success <- 0L
  acs_prog_state$n_failed  <- 1L
  on.exit(
    acs_prog$finish(
      n_success = acs_prog_state$n_success,
      n_failed  = acs_prog_state$n_failed
    ),
    add = TRUE
  )

  result <- NULL
  for (attempt in seq_len(3L)) {
    result <- tryCatch(
      .tidycensus_get_acs_call(
        geography = geography,
        variables = variables_effective,
        state     = state,
        year      = year,
        survey    = survey
      ),
      error = function(e) .classify_tidycensus_error(e)
    )

    if (!inherits(result, "tidycensus_classified_error")) break

    if (inherits(result, "tidycensus_4xx")) {
      .cli_abort_operator(c(
        "The Census Bureau's data API refused the request (HTTP 4xx).",
        "x" = "Message: {.val {attr(result, 'cacs_msg')}}.",
        "i" = "Check {.arg year}, {.arg survey}, and {.arg variables}; a key that has reached its request limit is refused as well."
      ))
    }

    # A 5xx error is tried again after 1 second and then after 2 seconds. The
    # third attempt has no wait after it: it stops the function below.
    if (inherits(result, "tidycensus_5xx") && attempt < 3L) {
      backoff <- 2L^(attempt - 1L)
      .cli_warn_runtime(c(
        "The Census Bureau's data API answered with an HTTP 5xx error on attempt {attempt} of 3; trying again in {backoff} second{?s}.",
        "i" = "Message: {.val {attr(result, 'cacs_msg')}}."
      ), phase = "acs")
      Sys.sleep(backoff)
      next
    }

    if (attempt == 3L) {
      .cli_abort_network(c(
        "The Census Bureau's data API did not give the data after 3 attempts.",
        "x" = "Last message: {.val {attr(result, 'cacs_msg')}}."
      ))
    }
  }

  # The loop above ends with a result, with the 4xx error, or with the network
  # error of the third attempt, which also catches an error that matched
  # neither 4xx nor 5xx. Nothing reaches this line today; it stops the function
  # rather than returning the error object as if it were data.
  if (inherits(result, "tidycensus_classified_error")) {
    .cli_abort_network(c(
      "tidycensus fetch failed with unclassified error.",
      "x" = "Message: {.val {attr(result, 'cacs_msg')}}."
    ))
  }

  out <- result

  # The download worked, so the summary line reports one success. tick() adds
  # a line of its own only when `catchmentACS.progress` is "force".
  acs_prog$tick()
  acs_prog_state$n_success <- 1L
  acs_prog_state$n_failed  <- 0L


  # The same checks as `cacs_acs_validate()`, whose body is this one call.
  ok <- .validate_acs_schema(out, abort = TRUE)
  stopifnot(isTRUE(ok))


  # The water-tract filter runs before the counts below, so `n_tracts`,
  # `n_total_rows`, and the counts of missing values describe the rows that
  # are returned.

  cached_conditions <- list()
  drop_result <- withCallingHandlers(
    .drop_water_tracts(
      acs_sf            = out,
      drop_water_tracts = drop_water_tracts,
      verbose           = verbose
    ),
    catchmentACS_message_water_tract_filter = function(cnd) {
      cached_conditions[[length(cached_conditions) + 1L]] <<- cnd
    }
  )
  out <- drop_result$kept_sf


  # A negative value in `moe` is one of the Census Bureau's annotation codes,
  # not a margin of error. -555555555 means that a margin of error is not
  # appropriate because the estimate is controlled to an independent
  # population or housing estimate. Such a value has to become NA before the
  # margins of error are combined, because that step squares it
  # (`.sum_weighted_variance()` in R/intersect-weight.R): -555555555 would add
  # 1.1e+17 to the variance of a drive-time area, while NA makes the margin of
  # error of that area NA.
  #
  # tidycensus 1.8.1 already returns NA for these codes, so with that version
  # the two conversions and the two warnings below have nothing to do; they
  # still hold for data that arrives with the codes in place.

  total_rows <- nrow(out)
  estimate_suppressed <- is.na(out$estimate)
  moe_sentinel <- !is.na(out$moe) & (out$moe == -555555555)
  negative_moe <- !is.na(out$moe) & out$moe < 0 & !moe_sentinel

  n_estimate_suppressed <- sum(estimate_suppressed)
  n_moe_sentinel <- sum(moe_sentinel)
  n_negative_moe <- sum(negative_moe)
  n_suppressed <- n_estimate_suppressed

  if (n_moe_sentinel > 0L) {
    out$moe[moe_sentinel] <- NA_real_
  }
  # Any other negative value in `moe` is an annotation code as well.
  out$moe[!is.na(out$moe) & out$moe < 0] <- NA_real_

  if (total_rows > 0L && (n_estimate_suppressed / total_rows) > 0.10) {
    .cli_warn_runtime(c(
      "More than 10% of rows have a missing estimate ({n_estimate_suppressed} of {total_rows}).",
      "i" = "These estimates stay {.code NA}. The estimate and margin of error of such a variable are {.code NA} for the drive-time areas that include the tract, and so is a rate that uses it."
    ), phase = "acs")
  }

  if (total_rows > 0L && (n_moe_sentinel / total_rows) > 0.10) {
    .cli_warn_runtime(c(
      "More than 10% of rows have the code {.val {-555555555}} in place of a margin of error ({n_moe_sentinel} of {total_rows}).",
      "i" = "These margins of error are set to {.code NA}. The margin of error of such a variable is {.code NA} for the drive-time areas that include the tract, and a rate that uses it is {.code NA}."
    ), phase = "acs")
  }


  # The result has one row per tract and variable, so columns holding the
  # request would repeat the same values thousands of times; the record is
  # attached to the object instead. `cacs_intersect_weight()` reads `year`
  # from it for its `acs_year` column (`.extract_acs_year()`).

  prov_versions <- tryCatch(
    list(
      tidycensus = as.character(utils::packageVersion("tidycensus")),
      sf         = as.character(utils::packageVersion("sf"))
    ),
    error = function(e) list(tidycensus = NA_character_, sf = NA_character_)
  )

  vars_named <- variables_effective
  names(vars_named) <- variables_effective

  prov <- list(
    state                 = state,
    year                  = year,
    survey                = survey,
    geography             = geography,
    variables             = vars_named,
    variable_source       = vars_source,
    n_variables_requested = length(variables_resolved),
    n_variables_received  = length(variables_effective),
    n_variables_skipped   = length(variables_skipped),
    n_tracts              = length(unique(out$GEOID)),
    n_suppressed          = n_suppressed,
    n_estimate_suppressed = n_estimate_suppressed,
    n_moe_sentinel        = n_moe_sentinel,
    n_negative_moe        = n_negative_moe,
    n_total_rows          = total_rows,
    cache_key             = cache_key,
    cache_namespace       = "acs",
    generated_at          = as.POSIXct(Sys.time(), tz = "UTC"),
    tidycensus_version    = prov_versions$tidycensus,
    sf_version            = prov_versions$sf,
    acs_geometry_vintage  = .acs_geometry_vintage(year, geography)
  )

  attr(out, "cacs_provenance") <- prov
  attr(out, "cacs_acs_provenance") <- prov
  attr(out, "cacs_schema_version") <- "1.0"

  # Nothing in the package sets these three attributes, so these lines have
  # nothing to remove from a downloaded result; the tests check that the
  # result carries none of them.
  attr(out, "cacs_cache_key_pending") <- NULL
  attr(out, "cacs_vars_source")       <- NULL
  attr(out, "cacs_variables_skipped") <- NULL


  # `.cacs_cache_put()` writes `<key>.rds.tmp` and renames it to `<key>.rds`,
  # so an interrupted write leaves no half-written copy.
  # The GeoPackage is an extra file for other GIS software; the `.rds` file is
  # the one read back. `cache_dir` names the cache folder, and both files go
  # under its `acs` subfolder.

  put_ok <- .cacs_cache_put(
    out, cache_key, "acs", cache_dir = cache_dir,
    conditions = cached_conditions
  )
  if (isTRUE(verbose) && isTRUE(put_ok)) {
    .cli_inform_cache(
      c("Saved the ACS data ({total_rows} row{?s}) in the cache folder.",
        "i" = "A later call for the same data reads this copy instead of downloading it again."),
      phase = "acs"
    )
  }

  if (isTRUE(write_gpkg)) {
    # The GeoPackage goes next to the saved result, in the same subfolder.
    base_dir <- tryCatch(
      .cacs_cache_base_dir(cache_dir = cache_dir, create = TRUE),
      catchmentACS_error_operator = function(e) NULL
    )
    gpkg_dir   <- if (is.null(base_dir)) NA_character_ else
      file.path(base_dir, .cacs_cache_effective_namespace("acs", cache_dir = cache_dir))
    if (!is.na(gpkg_dir) && !dir.exists(gpkg_dir)) {
      dir.create(gpkg_dir, recursive = TRUE, showWarnings = FALSE)
    }
    gpkg_path  <- file.path(gpkg_dir, paste0(cache_key, ".gpkg"))
    gpkg_ok <- if (is.na(gpkg_dir)) {
      .cli_warn_runtime(c(
        "The GeoPackage file was not written: the cache folder cannot be created.",
        "i" = "The downloaded data are returned; see {.fn cacs_cache_dir}."
      ), phase = "acs")
      FALSE
    } else tryCatch({
      suppressWarnings(suppressMessages(
        sf::st_write(out, gpkg_path, driver = "GPKG",
                     delete_dsn = TRUE, quiet = TRUE)
      ))
      TRUE
    },
    error = function(e) {
      .cli_warn_runtime(c(
        "The GeoPackage file was not written; the saved copy of the data is kept.",
        "x" = "Path: {.path {gpkg_path}}",
        "i" = "Error: {.val {conditionMessage(e)}}"
      ), phase = "acs")
      FALSE
    })
    if (isTRUE(verbose) && isTRUE(gpkg_ok)) {
      .cli_inform_cache(
        c("Also saved the ACS data as a GeoPackage file:",
          "i" = "{.path {gpkg_path}}"),
        phase = "acs"
      )
    }
  }

  out
}


#' The one call to `tidycensus::get_acs()`
#'
#' The download sits in its own function so that tests can replace it with
#' `testthat::local_mocked_bindings(.tidycensus_get_acs_call = mockfn,
#' .package = "catchmentACS")`. Only the smoke test in
#' `tests/testthat/test-unit-acs-validate.R`, which needs both a Census API
#' key and an opt-in environment variable, makes the request for real.
#'
#' @keywords internal
#' @noRd
.tidycensus_get_acs_call <- function(geography, variables, state, year, survey) {
  tidycensus::get_acs(
    geography    = geography,
    variables    = variables,
    state        = state,
    year         = year,
    survey       = survey,
    output       = "tidy",
    geometry     = TRUE
  )
}


#' Check `state` against the USPS codes and FIPS codes in tigris
#'
#' @keywords internal
#' @noRd
.validate_state_in_fips <- function(state) {
  fips <- tryCatch(tigris::fips_codes, error = function(e) NULL)
  if (is.null(fips)) {
    # tigris is an Imports dependency, so this branch runs only when its data
    # cannot be read. The list below holds the 50 states and DC; unlike
    # `tigris::fips_codes` it leaves out Puerto Rico and the other
    # territories, which `cacs_acs_prefetch()` otherwise accepts.
    usps <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI",
              "ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN",
              "MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH",
              "OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA",
              "WV","WI","WY")
    fips_codes_hardcoded <- sprintf("%02d", c(1,2,4,5,6,8,9,10,11,12,13,15,
                                              16,17,18,19,20,21,22,23,24,25,
                                              26,27,28,29,30,31,32,33,34,35,
                                              36,37,38,39,40,41,42,44,45,46,
                                              47,48,49,50,51,53,54,55,56))
    ok <- state %in% usps || state %in% fips_codes_hardcoded
  } else {
    ok <- state %in% unique(fips$state) ||
          state %in% unique(fips$state_code)
  }
  if (!isTRUE(ok)) {
    .cli_abort_schema(c(
      "{.arg state} is not a known USPS code or 2-digit FIPS.",
      "x" = "Got {.val {state}}.",
      "i" = "Use a code such as {.val AL} or {.val 01}; {.code tigris::fips_codes} lists them."
    ))
  }
  invisible(TRUE)
}


#' The ACS 5-year end years the package accepts
#'
#' The upper year is written out rather than computed from the current date,
#' because the Census Bureau publishes each year of the data on its own
#' calendar. It moves when a new year has been tried with the package.
#'
#' @return named integer vector `c(min = 2009L, max = 2024L)`
#' @keywords internal
#' @noRd
.cacs_acs5_year_range <- function() {
  c(min = 2009L, max = 2024L)
}


#' Does this tidycensus error say that the year has no endpoint?
#'
#' @keywords internal
#' @noRd
.is_tidycensus_endpoint_not_found <- function(e) {
  msg <- tolower(conditionMessage(e))
  grepl("api endpoint not found|does this data set exist|404|not[[:space:]_.-]*found",
        msg)
}


#' Turn the `variables` argument into a sorted vector of codes and a label
#'
#' The label goes into the record of the request as `variable_source`.
#'
#' @return list(vars = character, source = character(1))
#' @keywords internal
#' @noRd
.resolve_variables <- function(variables) {
  if (is.null(variables)) {
    e <- new.env(parent = emptyenv())
    utils::data("cacs_acs_default_vars", package = "catchmentACS", envir = e)
    vars <- unname(e$cacs_acs_default_vars)
    return(list(vars = sort(unique(vars)), source = "default_catalogue"))
  }

  if (is.character(variables) && length(variables) == 1L &&
      variables %in% c("core", "extended")) {
    e <- new.env(parent = emptyenv())
    utils::data("cacs_acs_default_vars", package = "catchmentACS", envir = e)
    vars <- unname(e$cacs_acs_default_vars)
    # "core" and "extended" both give the default list; only the label that
    # goes into the record tells them apart.
    src  <- if (identical(variables, "core")) "core_alias" else "extended_alias"
    return(list(vars = sort(unique(vars)), source = src))
  }

  if (is.character(variables) && length(variables) > 0L) {
    if (any(is.na(variables)) || any(!nzchar(variables))) {
      .cli_abort_schema(c(
        "{.arg variables} must not contain {.code NA} or empty strings."
      ))
    }
    pat <- "^B[0-9]{5}_[0-9]{3}$"
    if (!all(grepl(pat, variables))) {
      bad <- variables[!grepl(pat, variables)]
      .cli_abort_schema(c(
        "{.arg variables} has {cli::qty(length(bad))}{?a code/codes} that {?does/do} not look like an ACS code such as {.val B19013_001}.",
        "x" = "Got {.val {utils::head(bad, 5L)}}.",
        "i" = "A code is {.val B}, five digits, an underscore, and three digits."
      ))
    }
    return(list(vars = sort(unique(variables)), source = "user_supplied"))
  }

  .cli_abort_schema(c(
    "{.arg variables} must be {.code NULL}, {.val core}, {.val extended}, or a non-empty character vector.",
    "x" = "Got {.cls {class(variables)[1L]}}."
  ))
}


#' Sort a tidycensus error by what its message says
#'
#' Returns a plain list, not a condition, with one of the classes
#' `c("tidycensus_4xx", "tidycensus_classified_error")`,
#' `c("tidycensus_5xx", "tidycensus_classified_error")`, or
#' `c("tidycensus_unknown", "tidycensus_classified_error")`, and the message
#' in the attribute `cacs_msg`. The loop in `cacs_acs_prefetch()` asks for the
#' first two: a 4xx error stops it at once and a 5xx error is tried again.
#' Nothing asks for `tidycensus_unknown`, so such an error uses up the three
#' attempts and ends in the same network error as an exhausted 5xx retry.
#'
#' @keywords internal
#' @noRd
.classify_tidycensus_error <- function(e) {
  msg <- conditionMessage(e)
  msg_lc <- tolower(msg)

  bucket <- if (grepl("\\b5[0-9]{2}\\b|server error|timeout|timed.out|504|502|503", msg_lc)) {
    "tidycensus_5xx"
  } else if (grepl("\\b4[0-9]{2}\\b|bad request|forbidden|unauthorized|not.found|400|401|403|404", msg_lc)) {
    "tidycensus_4xx"
  } else {
    "tidycensus_unknown"
  }

  obj <- structure(
    list(parent = e),
    class      = c(bucket, "tidycensus_classified_error")
  )
  attr(obj, "cacs_msg") <- msg
  obj
}


#' Return the ACS cache folder, creating it when it is missing
#'
#' Nothing in the package calls this; the folder that `cacs_acs_prefetch()`
#' writes to comes from `.cacs_cache_put()` and `.cacs_cache_get()`. The tests
#' in `tests/testthat/test-phase5-acs-helper-coverage.R` are its only callers.
#'
#' @keywords internal
#' @noRd
.resolve_cache_dir <- function(cache_dir = NULL) {
  if (!is.null(cache_dir)) {
    if (!dir.exists(dirname(cache_dir))) {
      .cli_abort_operator(c(
        "Parent of {.path {cache_dir}} does not exist.",
        "i" = "Create the parent directory first."
      ))
    }
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
    return(normalizePath(cache_dir, mustWork = FALSE, winslash = "/"))
  }
  file.path(cacs_cache_dir(create = TRUE), "acs")
}


#' Name the tract boundaries that come with an ACS year and geography
#'
#' The label goes into the cache key and into the record of the request. It is
#' built from the year and the geography alone. If a later tidycensus release
#' pairs an ACS year with boundaries of another year, this function is the one
#' place to change.
#'
#' @keywords internal
#' @noRd
.acs_geometry_vintage <- function(year, geography) {
  paste0("tigris_", as.integer(year), "_", geography)
}


#' Drop water and special-purpose tracts
#'
#' Two checks, so that a tract is still dropped when one of them misses it:
#' the GEOID pattern of the Census numbering (digits 6 and 7 are `"99"`), and
#' an area that is zero, negative, or not a finite number. Each dropped row
#' keeps the reason it was dropped (`"geoid_pattern"`, `"zero_area"`, or
#' `"geoid_pattern+zero_area"`).
#'
#' @param acs_sf An `sf` data frame of ACS rows, with `GEOID` and a geometry
#'   column.
#' @param drop_water_tracts A single `TRUE` or `FALSE`; with `FALSE` the input
#'   is returned unchanged, with empty vectors of dropped GEOIDs and reasons.
#' @param verbose A single `TRUE` or `FALSE`; with `TRUE`, and when any rows
#'   are dropped, `catchmentACS_message_water_tract_filter` is emitted.
#'
#' @return A list with three elements:
#'   * `kept_sf`: the rows that are kept.
#'   * `dropped_geoids`: the `GEOID` of every dropped row, so a tract appears
#'     once per variable.
#'   * `dropped_reasons`: one reason per element of `dropped_geoids`.
#'
#' `cacs_acs_prefetch()` uses `kept_sf`, and on a cache hit it also asks
#' whether `dropped_geoids` is empty; only the tests read `dropped_reasons`.
#'
#' @keywords internal
#' @noRd
.drop_water_tracts <- function(acs_sf,
                               drop_water_tracts = TRUE,
                               verbose = TRUE) {
  if (!isTRUE(drop_water_tracts)) {
    return(list(
      kept_sf         = acs_sf,
      dropped_geoids  = character(0),
      dropped_reasons = character(0)
    ))
  }
  if (is.null(acs_sf) || nrow(acs_sf) == 0L) {
    return(list(
      kept_sf         = acs_sf,
      dropped_geoids  = character(0),
      dropped_reasons = character(0)
    ))
  }

  pat_match <- grepl(.WATER_TRACT_REGEX, acs_sf$GEOID)

  # Only the sign of the area is asked, and a positive area stays positive in
  # any projection, so measuring on longitude and latitude is enough. With
  # sf::sf_use_s2(FALSE), though, sf measures longitude and latitude only with
  # the lwgeom package, which is not installed with sf; the tracts are then
  # measured in EPSG:5070 (NAD83 / Conus Albers) instead.
  # suppressWarnings() keeps a warning from sf out of the result; sf 1.1.1
  # gives none here.
  area_sf <- if (!isTRUE(sf::sf_use_s2()) && isTRUE(sf::st_is_longlat(acs_sf))) {
    sf::st_transform(acs_sf, 5070)
  } else {
    acs_sf
  }
  area_m <- suppressWarnings(as.numeric(sf::st_area(area_sf)))
  area_bad <- !is.finite(area_m) | area_m <= 0

  drop_mask <- pat_match | area_bad

  # One reason per row of the input; the dropped rows are taken out of it
  # below.
  reasons <- character(length(acs_sf$GEOID))
  reasons[pat_match & !area_bad] <- "geoid_pattern"
  reasons[area_bad & !pat_match] <- "zero_area"
  reasons[pat_match & area_bad]  <- "geoid_pattern+zero_area"

  if (any(drop_mask) && isTRUE(verbose)) {
    .cli_inform_water_tract_filter(
      geoids  = unique(acs_sf$GEOID[drop_mask]),
      reasons = reasons[drop_mask]
    )
  }

  list(
    kept_sf         = acs_sf[!drop_mask, , drop = FALSE],
    dropped_geoids  = acs_sf$GEOID[drop_mask],
    dropped_reasons = reasons[drop_mask]
  )
}
