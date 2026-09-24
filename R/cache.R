# cache.R - the folder that holds saved results, the setting that turns saving
# on and off, and the writing, reading, and checking of the saved files.
#
# A saved result is an .rds file in the subfolder for its kind of result
# (`isochrone`, `acs`, `intersect`), with a checksum file next to it. Writing
# goes to <key>.rds.tmp and then renames, so an interrupted write leaves a .tmp
# file instead of a half-written .rds; reading never looks at a .tmp file, and
# cacs_cache_status() counts the ones left behind. Cache keys and checksums
# are SHA-256 digests, from digest::digest(algo = "sha256").


# How many values each function puts in its cache key. .cacs_cache_key()
# checks the length, so a payload that gains or loses a value gives an error
# here instead of a key that quietly stops matching the saved results.

.CACS_CACHE_NAMESPACE_DIMS <- list(
  isochrone = 16L,   # cache_payload in R/isochrone-dispatch.R
  acs       = 12L,   # cache_payload in R/acs-prefetch.R
  intersect = 14L    # payload in R/intersect-weight.R
)

.CACS_CACHE_NAMESPACES <- c("isochrone", "acs", "acs_test", "intersect")
.CACS_CACHE_PUBLIC_NAMESPACES <- c("isochrone", "acs", "intersect")
.CACS_CACHE_FINGERPRINT_ALGO <- "sha256"
.CACS_CACHE_FINGERPRINT_SCHEMA <- "cache-v0.3"
.CACS_CACHE_CONDITIONS_ATTR <- "cacs_cached_conditions"

.cacs_cache_state <- new.env(parent = emptyenv())


# State kept for the length of the R session: the hit and miss counts that
# cacs_get_cache_state() reports, the keys already warned about as suspicious,
# whether the notice that the cache is on has been given, and the time at
# which the package was loaded.

.cacs_cache_reset_state <- function() {
  .cacs_cache_state$hits <- stats::setNames(
    rep.int(0L, length(.CACS_CACHE_NAMESPACES)),
    .CACS_CACHE_NAMESPACES
  )
  .cacs_cache_state$misses <- stats::setNames(
    rep.int(0L, length(.CACS_CACHE_NAMESPACES)),
    .CACS_CACHE_NAMESPACES
  )
  .cacs_cache_state$stale_warned <- character(0)
  .cacs_cache_state$announced <- FALSE
  .cacs_cache_state$pruned <- character(0)
  .cacs_cache_state$session_started_at <- as.POSIXct(Sys.time(), tz = "UTC")
  invisible(TRUE)
}

.cacs_cache_ensure_state <- function() {
  if (is.null(.cacs_cache_state$hits) || is.null(.cacs_cache_state$misses)) {
    .cacs_cache_reset_state()
  }
  invisible(TRUE)
}

.cacs_cache_bump <- function(kind = c("hits", "misses"), namespace) {
  kind <- match.arg(kind)
  .cacs_cache_ensure_state()
  if (!namespace %in% names(.cacs_cache_state[[kind]])) {
    .cacs_cache_state[[kind]][[namespace]] <- 0L
  }
  .cacs_cache_state[[kind]][[namespace]] <-
    as.integer(.cacs_cache_state[[kind]][[namespace]] + 1L)
  invisible(TRUE)
}

.cacs_cache_reset_counters <- function(namespace = "all") {
  .cacs_cache_ensure_state()
  ns <- if (identical(namespace, "all")) {
    .CACS_CACHE_NAMESPACES
  } else {
    namespace
  }
  ns <- intersect(ns, .CACS_CACHE_NAMESPACES)
  for (kind in c("hits", "misses")) {
    .cacs_cache_state[[kind]][ns] <- 0L
  }
  invisible(TRUE)
}

.cacs_cache_counter_tibble <- function() {
  .cacs_cache_ensure_state()
  hits <- .cacs_cache_state$hits
  misses <- .cacs_cache_state$misses
  get_count <- function(x, nm) {
    present <- intersect(nm, names(x))
    if (length(present) == 0L) return(0L)
    as.integer(sum(unname(x[present]), na.rm = TRUE))
  }

  # One `acs` row for the user, adding up the subfolders `acs` and `acs_test`,
  # so this table has the same three rows in an ordinary session and while
  # tests run.
  hit <- c(
    isochrone = get_count(hits, "isochrone"),
    acs = get_count(hits, c("acs", "acs_test")),
    intersect = get_count(hits, "intersect")
  )
  miss <- c(
    isochrone = get_count(misses, "isochrone"),
    acs = get_count(misses, c("acs", "acs_test")),
    intersect = get_count(misses, "intersect")
  )
  tibble::tibble(
    namespace = names(hit),
    hit = as.integer(unname(hit)),
    miss = as.integer(unname(miss)),
    hit_rate = hit / (hit + miss)
  )
}

.cacs_cache_mark_stale_warned <- function(id) {
  .cacs_cache_ensure_state()
  if (id %in% .cacs_cache_state$stale_warned) return(FALSE)
  .cacs_cache_state$stale_warned <- c(.cacs_cache_state$stale_warned, id)
  TRUE
}


# Messages and warnings can be saved with a result, in the attribute named by
# .CACS_CACHE_CONDITIONS_ATTR, and given again when the result is read back, so
# that a saved run says the same things as the run that produced it.
#
# A condition keeps the call that raised it. When the package is installed
# with its sources kept (R_KEEP_PKG_SOURCE=yes), that call has a "srcref"
# attribute, which holds environments of the installed package. Such a value
# is not the same after it is written and read back, so the saved file would
# never match its fingerprint file and each call would compute the result
# again. The attribute is dropped; giving the condition again does not use it.

.cacs_cache_prepare_conditions <- function(conditions) {
  if (is.null(conditions)) return(list())
  if (inherits(conditions, "condition")) {
    conditions <- list(conditions)
  }
  if (!is.list(conditions)) return(list())
  conditions <- Filter(function(cnd) inherits(cnd, "condition"), conditions)
  lapply(conditions, function(cnd) {
    call <- cnd[["call"]]
    if (!is.null(attr(call, "srcref", exact = TRUE))) {
      attr(call, "srcref") <- NULL
      cnd[["call"]] <- call
    }
    cnd
  })
}

.cacs_cache_conditions <- function(value) {
  .cacs_cache_prepare_conditions(
    attr(value, .CACS_CACHE_CONDITIONS_ATTR, exact = TRUE)
  )
}

.cacs_cache_strip_conditions <- function(value) {
  attr(value, .CACS_CACHE_CONDITIONS_ATTR) <- NULL
  value
}

.cacs_cache_signal_conditions <- function(conditions) {
  conditions <- .cacs_cache_prepare_conditions(conditions)
  for (cnd in conditions) {
    rlang::cnd_signal(cnd)
  }
  invisible(length(conditions) > 0L)
}


# The folder is worked out again at every call, so a change to the option or
# the environment variable takes effect at once and saving, reading, and
# deleting always use the same folder. Without either setting, the folder is
# inside the temporary folder of the R session, which R deletes when the
# session ends.

.cacs_cache_dir_impl <- function(create = TRUE) {
  override_opt <- getOption("catchmentACS.cache_dir", NULL)
  override_env <- Sys.getenv("CACS_CACHE_DIR", unset = NA)

  path <- if (!is.null(override_opt) && nzchar(override_opt)) {
    override_opt
  } else if (!is.na(override_env) && nzchar(override_env)) {
    override_env
  } else {
    file.path(tempdir(), "catchmentACS")
  }

  if (create && !dir.exists(path)) {
    ok <- tryCatch(
      dir.create(path, recursive = TRUE, showWarnings = FALSE),
      error = function(e) FALSE,
      warning = function(w) FALSE
    )
    if (!isTRUE(ok) || !dir.exists(path)) {
      .cli_abort_operator(c(
        "Cannot create cache directory {.path {path}}.",
        "x" = "{.fn dir.create} failed (permission denied, read-only filesystem, or parent missing).",
        "i" = "Override with {.code options(catchmentACS.cache_dir = \"<writable path>\")} or env var {.envvar CACS_CACHE_DIR}."
      ))
    }
  }

  normalizePath(path, mustWork = FALSE, winslash = "/")
}


#' Get the path of the cache folder
#'
#' Returns the path of the cache folder, where the package saves results on
#' disk for reuse. By default, [cacs_isochrone()], [cacs_acs_prefetch()], and
#' [cacs_intersect_weight()] each save their results in a subfolder of it
#' (`isochrone`, `acs`, and `intersect`), and a later call that matches an
#' earlier one reads the saved result instead of computing or downloading it
#' again. The help page of each function says what must match.
#'
#' The cache folder is the first of these that is set and not empty:
#'
#' 1. the option `catchmentACS.cache_dir`;
#' 2. the environment variable `CACS_CACHE_DIR`;
#' 3. a folder named `catchmentACS` inside the temporary folder of the R
#'    session (see [tempdir()]), which R deletes when the session ends.
#'
#' The folder is worked out again at every call, so a change to the option or
#' the environment variable takes effect at once.
#'
#' @section How saved results are checked:
#' Each saved result is an `.rds` file with a checksum file (`.fingerprint`)
#' next to it. When a saved result is read, its checksum is computed again
#' and compared with the one in the checksum file. If the checksum file is
#' missing or not in the expected form, if the `.rds` file cannot be read, or
#' if the checksums differ, both files are deleted and the result is computed
#' or downloaded again. When saved American Community Survey (ACS) data are
#' read, [cacs_acs_prefetch()] also warns if they look incomplete.
#'
#' @section A cache folder that lasts between sessions:
#' Results saved in the default folder are deleted when the R session ends.
#' To keep them for later sessions, set the option or the environment
#' variable to a folder that lasts, for example with this line in the R
#' startup file (see [Startup][base::Startup]):
#'
#' ```
#' options(catchmentACS.cache_dir = tools::R_user_dir("catchmentACS", "cache"))
#' ```
#'
#' [tools::R_user_dir()] gives a folder for the cache files of one package,
#' inside the user's cache folder for R; where that is depends on the
#' operating system.
#'
#' A cache folder outside the temporary folder of the session is tidied once
#' per session, the first time a result is read from it or saved in it while
#' the cache is on. These files are deleted:
#'
#' - results not used for 30 days, with their checksum and GeoPackage files.
#'   Reading a saved result counts as using it. The option
#'   `catchmentACS.cache_max_age_days` sets another number of days, and `Inf`
#'   keeps results however old they are. Its value when the folder is first
#'   used in the session is the one that counts, so set it in the R startup
#'   file next to `catchmentACS.cache_dir`, for example
#'   `options(catchmentACS.cache_max_age_days = 90)`; a value that is not a
#'   single positive number counts as 30;
#' - GeoPackage files as old as that without a result;
#' - files left by an interrupted write, and results or checksum files
#'   without their partner, when they are more than a day old.
#'
#' Only files named the way the package names its saved files, in the
#' subfolders `isochrone`, `acs`, `acs_test`, and `intersect`, are deleted.
#'
#' Versions 0.5.1 and earlier saved results in the user cache folder of the
#' operating system: `~/Library/Caches/catchmentACS` on macOS,
#' `~/.cache/catchmentACS` on Linux, and `catchmentACS/catchmentACS/Cache`
#' inside the folder named by the environment variable `LOCALAPPDATA` on
#' Windows.
#' The package no longer reads or deletes that folder; delete it yourself if
#' it is not needed.
#'
#' @param create A logical value. If `FALSE` (the default), the path is
#'   returned without creating the folder. If `TRUE`, the folder is created,
#'   together with any missing parent folders, when it does not exist, and an
#'   error is given if it cannot be created.
#' @return A string giving the path of the cache folder.
#' @family cache and configuration
#' @export
#' @examples
#' # The path of the cache folder, without creating the folder
#' cacs_cache_dir(create = FALSE)
cacs_cache_dir <- function(create = FALSE) {
  .cacs_cache_dir_impl(create = create)
}


#' Turn the cache on or off
#'
#' Turns on or off the saving and reuse of results on disk (the cache; see
#' [cacs_cache_dir()]) by [cacs_isochrone()], [cacs_acs_prefetch()], and
#' [cacs_intersect_weight()], and so by [cacs_run()]. Turning the cache off
#' does not delete saved results.
#'
#' `cacs_set_cache()` sets the option `catchmentACS.cache_enabled` for the
#' rest of the R session and changes no file. With `scope = "global"`, it also
#' shows a line to add to the R startup file (see [Startup][base::Startup]) so
#' that later sessions start with the same setting. For `enabled = FALSE`, the
#' line sets this option. For `enabled = TRUE`, the line sets a cache folder
#' that lasts between sessions instead, because the cache is on by default
#' and its default folder is deleted when the session ends.
#'
#' If an environment variable decides whether the cache is on (see the next
#' section), the message says so.
#'
#' Versions 0.3.0 to 0.5.1 wrote the setting to the startup file themselves,
#' between the lines `# catchmentACS v0.3 cache control` and
#' `# end catchmentACS v0.3 cache control`. R still runs these lines when a
#' session starts, so the setting in them still applies. The package no
#' longer reads or removes them; delete them by hand if they are not
#' wanted.
#'
#' @section Environment variables and options:
#' These environment variables and options are read at every call, so a
#' change takes effect at once. The rules are checked in this order, so
#' `CACS_NO_CACHE` and `CACS_CACHE_ENABLED` take precedence over
#' `cacs_set_cache()`:
#'
#' 1. If the environment variable `CACS_NO_CACHE` is `"1"`, `"true"`,
#'    `"yes"`, or `"on"` (ignoring case), the cache is off.
#' 2. If the environment variable `CACS_CACHE_ENABLED` is set and not empty,
#'    the cache is on when its value is one of those four and off otherwise.
#' 3. If the option `catchmentACS.cache_enabled` is `TRUE` or `FALSE`, it
#'    decides. Other values are ignored.
#' 4. Otherwise, the cache is on (the default).
#'
#' When the cache is on, one kind of saved result can be turned off on its
#' own. The option `catchmentACS.cache_isochrone`, `catchmentACS.cache_acs`,
#' or `catchmentACS.cache_intersect` set to `FALSE` turns off the cache of
#' [cacs_isochrone()], [cacs_acs_prefetch()], or [cacs_intersect_weight()].
#' The environment variables `CACS_CACHE_ISOCHRONE`, `CACS_CACHE_ACS`, and
#' `CACS_CACHE_INTERSECT` do the same when set to a value other than the four
#' above, unless the matching option is `TRUE`.
#'
#' @param enabled A logical value: `TRUE` (the default) turns the cache on
#'   and `FALSE` turns it off. `NA` or any other value gives an error.
#' @param scope A string: `"session"` (the default) sets the option for the
#'   current R session, and `"global"` also shows the line to add to the R
#'   startup file for later sessions (see Details).
#' @param confirm Not used. It is kept so that code written for earlier
#'   versions still runs.
#' @return `TRUE`, invisibly.
#' @family cache and configuration
#' @export
#' @examples
#' # Turn the cache off for this session, check the setting, and restore it
#' old <- options("catchmentACS.cache_enabled")
#' cacs_set_cache(FALSE)
#' cacs_get_cache_state()$enabled
#'
#' # Also show the line that keeps the cache off in later sessions;
#' # no file is changed
#' cacs_set_cache(FALSE, scope = "global")
#' options(old)
cacs_set_cache <- function(enabled = TRUE,
                           scope = c("session", "global"),
                           confirm = interactive()) {
  if (!is.logical(enabled) || length(enabled) != 1L || is.na(enabled)) {
    .cli_abort_schema("{.arg enabled} must be {.cls logical(1)}.")
  }
  scope <- match.arg(scope)
  options(catchmentACS.cache_enabled = enabled)

  if (identical(scope, "global")) {
    # An environment variable can keep the cache in the other state; say so.
    now_on <- .cacs_cache_enabled()
    state_line <- if (identical(now_on, enabled)) {
      if (isTRUE(enabled)) {
        "The cache is on for this R session."
      } else {
        "The cache is off for this R session."
      }
    } else {
      envvar <- .cacs_cache_env_override()
      paste0(
        "The option is set, but the environment variable {.envvar ", envvar,
        "} keeps the cache ", if (isTRUE(now_on)) "on" else "off",
        " for this R session."
      )
    }
    if (isTRUE(enabled)) {
      folder <- cacs_cache_dir()
      if (!.cacs_cache_in_tempdir(folder)) {
        lines <- c(
          state_line,
          "i" = "Saved results are kept in {.path {folder}}, which lasts between sessions.",
          "i" = paste(
            "If the R startup file has a line that sets",
            "{.code catchmentACS.cache_enabled} to {.code FALSE}, remove it."
          )
        )
      } else {
        lines <- c(
          state_line,
          "i" = paste(
            "Without a cache folder, saved results are deleted when the session",
            "ends. To keep them for later sessions, add this line to the R",
            "startup file (see {.code ?Startup}):"
          ),
          " " = "options(catchmentACS.cache_dir = tools::R_user_dir(\"catchmentACS\", \"cache\"))",
          "i" = paste(
            "If the R startup file has a line that sets",
            "{.code catchmentACS.cache_enabled} to {.code FALSE}, remove it."
          )
        )
      }
    } else {
      lines <- c(
        state_line,
        "i" = paste(
          "To keep it off in later sessions, add this line to the R startup",
          "file (see {.code ?Startup}), and remove any other line there that",
          "sets {.code catchmentACS.cache_enabled}:"
        ),
        " " = "options(catchmentACS.cache_enabled = FALSE)"
      )
    }
    .cli_inform_cache(lines, phase = "cache")
  }
  invisible(TRUE)
}

#' Show the cache settings and how often saved results were used
#'
#' Returns a one-row table with the cache settings, the number of times a
#' saved result was used (a hit) or not used (a miss) in this R session, and
#' a summary of the files in the cache folder.
#'
#' @return A tibble with one row and these columns:
#'   \describe{
#'     \item{`enabled`}{Whether the cache is on (see [cacs_set_cache()]).
#'       This column does not show a kind of saved result that is turned off
#'       on its own.}
#'     \item{`cache_dir`}{The path of the cache folder, as returned by
#'       [cacs_cache_dir()].}
#'     \item{`namespace_mode`}{`"test"` in test mode, which is on, for
#'       example, while testthat runs tests and no cache folder is set with
#'       the option `catchmentACS.cache_dir` or the environment variable
#'       `CACS_CACHE_DIR`. American Community Survey (ACS) data are then kept
#'       in the subfolder `acs_test` instead of `acs`. Otherwise
#'       `"production"`.}
#'     \item{`fingerprint_algorithm`}{The checksum method of the checksum
#'       files, `"sha256"` (see [cacs_cache_dir()]).}
#'     \item{`counters`}{A list holding a tibble with one row for each kind of
#'       saved result (`isochrone`, `acs`, and `intersect`; see
#'       [cacs_clear_cache()]). Its columns are `namespace`, `hit`, `miss`,
#'       and `hit_rate`, which is `hit / (hit + miss)` or `NaN` before the
#'       first lookup. Its `acs` row adds up the subfolders `acs` and
#'       `acs_test`.}
#'     \item{`hits`, `misses`}{Lists each holding a named integer vector of
#'       the same counts for each subfolder: `isochrone`, `acs`, `acs_test`,
#'       and `intersect`.}
#'     \item{`status`}{A list holding the table returned by
#'       [cacs_cache_status()].}
#'     \item{`session_started_at`}{The time, in UTC, at which the package was
#'       loaded in this session.}
#'   }
#'
#' @section Hits and misses:
#' Each time [cacs_isochrone()], [cacs_acs_prefetch()], or
#' [cacs_intersect_weight()] looks for a saved result, the lookup counts as a
#' hit if the saved result is used and as a miss if the result is computed
#' or downloaded instead. A miss happens when nothing is saved for the inputs
#' or the saved result fails its checksum check (see [cacs_cache_dir()]),
#' and, for the first two functions, when the cache is off.
#' [cacs_intersect_weight()] does not look while the cache is off, and
#' [cacs_acs_prefetch()] does not look with `force_refresh = TRUE`. Lookups
#' in a folder given in a `cache_dir` argument are counted too. The counts
#' start when the package is loaded, and [cacs_clear_cache()] sets them back
#' to zero for the subfolders it clears.
#'
#' @family cache and configuration
#' @export
#' @examples
#' cacs_get_cache_state()
#'
#' # The numbers of hits and misses for each kind of saved result
#' cacs_get_cache_state()$counters[[1]]
cacs_get_cache_state <- function() {
  .cacs_cache_ensure_state()
  tibble::tibble(
    enabled = .cacs_cache_enabled(),
    cache_dir = cacs_cache_dir(create = FALSE),
    namespace_mode = .cacs_cache_namespace_mode(),
    fingerprint_algorithm = .CACS_CACHE_FINGERPRINT_ALGO,
    counters = list(.cacs_cache_counter_tibble()),
    hits = list(.cacs_cache_state$hits),
    misses = list(.cacs_cache_state$misses),
    status = list(cacs_cache_status()),
    session_started_at = .cacs_cache_state$session_started_at
  )
}


#' Delete saved results from the cache folder
#'
#' Deletes the saved results chosen by `namespace`, with their checksum files,
#' any temporary files left by an interrupted write, and the GeoPackage files
#' written by `cacs_acs_prefetch(write_gpkg = TRUE)`, from the cache folder
#' returned by [cacs_cache_dir()]. Other files in the folder and the
#' subfolders themselves are kept. A folder given in the `cache_dir` argument
#' of another function is not changed; to clear such a folder, first set
#' `options(catchmentACS.cache_dir = )` to it.
#'
#' After deleting, `cacs_clear_cache()` sets the hit and miss counts of the
#' cleared subfolders to zero (see [cacs_get_cache_state()]). If the cache
#' folder does not exist, a message says so and nothing is deleted.
#'
#' @param namespace A string naming the kind of saved result to delete, which
#'   is also the name of its subfolder:
#'   \describe{
#'     \item{`"all"` (the default)}{All four kinds.}
#'     \item{`"isochrone"`}{Drive-time areas saved by [cacs_isochrone()].}
#'     \item{`"acs"`}{American Community Survey (ACS) data saved by
#'       [cacs_acs_prefetch()].}
#'     \item{`"acs_test"`}{ACS data saved in test mode (see `namespace_mode`
#'       in [cacs_get_cache_state()]).}
#'     \item{`"intersect"`}{Results saved by [cacs_intersect_weight()].}
#'   }
#' @param confirm A logical value. If `TRUE`, a question is asked first, and
#'   files are deleted only if the answer is yes; in a non-interactive session
#'   nobody can answer, so nothing is deleted. If `FALSE`, files are deleted
#'   without a question. The default, `interactive()`, is `TRUE` only in an
#'   interactive session, so a script run with `Rscript` deletes without
#'   asking. The question is also skipped when the environment variable
#'   `CACS_NO_CONFIRM` is `"1"`.
#' @return A tibble, returned invisibly, with one row for each subfolder
#'   cleared and the columns `namespace`, `n_removed` (the number of files
#'   deleted, counting a saved result, its checksum file, and its GeoPackage
#'   file as separate files), and
#'   `bytes_freed` (their total size in bytes). It has no rows when the cache
#'   folder does not exist or the answer to the question is no.
#' @family cache and configuration
#' @export
#' @examples
#' # Use a temporary cache folder, so that a cache folder you have set is
#' # left alone
#' old <- options(catchmentACS.cache_dir = file.path(tempdir(), "cacs-example"))
#' cacs_cache_status()
#' cacs_clear_cache("isochrone", confirm = FALSE)
#' options(old)
cacs_clear_cache <- function(namespace = c("all", "isochrone", "acs", "acs_test", "intersect"),
                             confirm = interactive()) {
  namespace <- match.arg(namespace)
  base <- cacs_cache_dir(create = FALSE)
  ns_list <- if (namespace == "all") {
    .CACS_CACHE_NAMESPACES
  } else {
    namespace
  }

  if (!dir.exists(base)) {
    .cli_inform_cache(c(
      "Cache directory does not exist; nothing to clear.",
      "i" = "Path: {.path {base}}"
    ), phase = "cache")
    .cacs_cache_reset_counters(ns_list)
    return(invisible(tibble::tibble(
      namespace   = character(0),
      n_removed   = integer(0),
      bytes_freed = double(0)
    )))
  }

  if (isTRUE(confirm) && !identical(Sys.getenv("CACS_NO_CONFIRM"), "1")) {
    cli::cli_inform(c(
      "About to delete cache files under {.path {base}}.",
      "i" = "Namespaces: {.val {ns_list}}"
    ))
    answer <- tryCatch(
      utils::askYesNo("Proceed with deletion?", default = FALSE),
      error = function(e) FALSE
    )
    if (!isTRUE(answer)) {
      .cli_inform_cache(if (interactive()) {
        "No files removed."
      } else {
        c("No files removed: {.arg confirm} is {.code TRUE}, and this R session cannot ask for confirmation.",
          "i" = "Use {.code confirm = FALSE} to delete without asking.")
      }, phase = "cache")
      return(invisible(tibble::tibble(
        namespace   = character(0),
        n_removed   = integer(0),
        bytes_freed = double(0)
      )))
    }
  }

  summary <- lapply(ns_list, function(ns) {
    ns_dir <- file.path(base, ns)
    if (!dir.exists(ns_dir)) {
      return(tibble::tibble(namespace = ns, n_removed = 0L, bytes_freed = 0))
    }
    files <- list.files(ns_dir, pattern = .CACS_CACHE_FILE_PATTERN, full.names = TRUE)
    if (length(files) == 0L) {
      return(tibble::tibble(namespace = ns, n_removed = 0L, bytes_freed = 0))
    }
    sizes <- file.info(files)$size
    unlink(files, force = TRUE)
    removed <- !file.exists(files)
    tibble::tibble(namespace = ns, n_removed = sum(removed),
                   bytes_freed = sum(sizes[removed], na.rm = TRUE))
  })

  out <- do.call(rbind, summary)
  .cacs_cache_reset_counters(ns_list)

  .cli_inform_cache(c(
    "Cleared cache namespaces: {.val {ns_list}}.",
    "i" = "Removed {.val {sum(out$n_removed)}} file{?s}, freeing {pretty_bytes(sum(out$bytes_freed))}."
  ), phase = "cache")

  invisible(out)
}


#' Summarize the files in the cache folder
#'
#' Counts the files in each subfolder of the cache folder returned by
#' [cacs_cache_dir()] and reports their total size and their oldest and
#' newest modification times. The folder is not created if it does not
#' exist. For a folder given in the `cache_dir` argument of another function,
#' first set `options(catchmentACS.cache_dir = )` to it.
#'
#' @return A tibble with one row for each subfolder (`isochrone`, `acs`,
#'   `acs_test`, and `intersect`; see [cacs_clear_cache()]) and these
#'   columns:
#'   \describe{
#'     \item{`namespace`}{The subfolder.}
#'     \item{`n_entries`}{The number of saved results (`.rds` files).}
#'     \item{`orphan_tmp`}{The number of temporary `.rds.tmp` files, which
#'       are left when a write is interrupted and are never read.}
#'     \item{`n_fingerprints`}{The number of checksum files (`.fingerprint`
#'       files; see [cacs_cache_dir()]).}
#'     \item{`orphan_fingerprint_tmp`}{The number of temporary
#'       `.fingerprint.tmp` files, left in the same way.}
#'     \item{`legacy_entries`}{The number of `.rds` files without a checksum
#'       file, such as those saved by versions of the package before 0.3.0.
#'       Such a file is deleted, and the result computed again, when a later
#'       call looks for it.}
#'     \item{`total_size_mb`}{The total size of these files and of the
#'       GeoPackage files written by `cacs_acs_prefetch(write_gpkg = TRUE)`,
#'       in megabytes (`1024^2` bytes).}
#'     \item{`oldest`, `newest`}{The earliest and latest modification times of
#'       the same files, or `NA` when there are none. Reading a saved result
#'       updates its modification time.}
#'   }
#'   Other files are not counted.
#' @family cache and configuration
#' @export
#' @examples
#' cacs_cache_status()
cacs_cache_status <- function() {
  base <- cacs_cache_dir(create = FALSE)
  ns_list <- .CACS_CACHE_NAMESPACES

  rows <- lapply(ns_list, function(ns) {
    ns_dir <- file.path(base, ns)
    if (!dir.exists(ns_dir)) {
      return(tibble::tibble(
        namespace     = ns,
        n_entries     = 0L,
        orphan_tmp    = 0L,
        n_fingerprints = 0L,
        orphan_fingerprint_tmp = 0L,
        legacy_entries = 0L,
        total_size_mb = 0,
        oldest        = as.POSIXct(NA),
        newest        = as.POSIXct(NA)
      ))
    }
    own <- list.files(ns_dir, pattern = .CACS_CACHE_FILE_PATTERN, full.names = TRUE)
    rds <- grep("[.]rds$", own, value = TRUE)
    tmp <- grep("[.]rds[.]tmp$", own, value = TRUE)
    fingerprint <- grep("[.]fingerprint$", own, value = TRUE)
    fingerprint_tmp <- grep("[.]fingerprint[.]tmp$", own, value = TRUE)
    gpkg <- grep("[.]gpkg$", own, value = TRUE)
    all_files <- c(rds, tmp, fingerprint, fingerprint_tmp, gpkg)
    info <- file.info(all_files)
    tibble::tibble(
      namespace     = ns,
      n_entries     = length(rds),
      orphan_tmp    = length(tmp),
      n_fingerprints = length(fingerprint),
      orphan_fingerprint_tmp = length(fingerprint_tmp),
      legacy_entries = length(setdiff(
        sub("\\.rds$", "", basename(rds)),
        sub("\\.fingerprint$", "", basename(fingerprint))
      )),
      total_size_mb = sum(info$size, na.rm = TRUE) / (1024^2),
      oldest        = if (nrow(info) == 0L) as.POSIXct(NA) else min(info$mtime, na.rm = TRUE),
      newest        = if (nrow(info) == 0L) as.POSIXct(NA) else max(info$mtime, na.rm = TRUE)
    )
  })

  do.call(rbind, rows)
}


#' Build the cache key of one call
#'
#' Used by `cacs_isochrone()`, `cacs_acs_prefetch()`, and
#' `cacs_intersect_weight()`, each with its own `namespace`.
#'
#' @param payload A named list of the values that identify the call.
#' @param namespace One of `"isochrone"`, `"acs"`, and `"intersect"`. It also
#'   gives the number of values the payload must have
#'   (`.CACS_CACHE_NAMESPACE_DIMS`).
#' @return A string: the SHA-256 digest of the payload, 64 hex characters.
#' @keywords internal
#' @noRd
.cacs_cache_key <- function(payload,
                            namespace = c("isochrone", "acs", "intersect")) {
  namespace <- match.arg(namespace)
  expected_dims <- .CACS_CACHE_NAMESPACE_DIMS[[namespace]]

  if (!is.list(payload)) {
    .cli_abort_schema(c(
      "{.arg payload} must be a {.cls list}.",
      "x" = "Got {.cls {class(payload)[[1]]}}."
    ))
  }
  if (length(payload) != expected_dims) {
    .cli_abort_schema(c(
      "{.arg payload} length mismatch for namespace {.val {namespace}}.",
      "x" = "Expected {.val {expected_dims}} components, got {.val {length(payload)}}."
    ))
  }
  nm <- names(payload)
  if (is.null(nm) || any(!nzchar(nm)) || anyDuplicated(nm)) {
    .cli_abort_schema(c(
      "{.arg payload} must have unique non-empty names on every element."
    ))
  }

  # Sort the names at every level of the payload first, so two calls that put
  # the same values together in a different order get the same key.
  payload_sorted <- .cacs_cache_normalize_payload(payload[order(nm)])
  digest::digest(payload_sorted, algo = "sha256", serialize = TRUE)
}

.cacs_cache_normalize_payload <- function(x) {
  if (is.list(x) && !inherits(x, c("data.frame", "sf", "sfc"))) {
    nm <- names(x)
    if (!is.null(nm) && all(nzchar(nm)) && !anyDuplicated(nm)) {
      x <- x[order(nm)]
    }
    return(lapply(x, .cacs_cache_normalize_payload))
  }
  if (is.factor(x)) return(as.character(x))
  x
}


# The helpers below answer one question each about a saved result: which
# folder it goes in, whether saving is on for its kind, what its checksum file
# is called, and whether saved ACS data look too small to be complete.

#' The cache folder to use for one call
#'
#' Returns `cache_dir` when the caller gave one and `cacs_cache_dir()`
#' otherwise. With `create = TRUE` a missing folder is created, and an error is
#' given when it cannot be created.
#'
#' @keywords internal
#' @noRd
.cacs_cache_base_dir <- function(cache_dir = NULL, create = TRUE) {
  if (is.null(cache_dir) || identical(cache_dir, "")) {
    return(cacs_cache_dir(create = create))
  }
  stopifnot(is.character(cache_dir), length(cache_dir) == 1L, !is.na(cache_dir))
  if (create && !dir.exists(cache_dir)) {
    ok <- tryCatch(
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE),
      error = function(e) FALSE,
      warning = function(w) FALSE
    )
    if (!isTRUE(ok) || !dir.exists(cache_dir)) {
      .cli_abort_operator(c(
        "Cannot create cache directory {.path {cache_dir}}.",
        "x" = "{.fn dir.create} failed.",
        "i" = "Pass a writable {.arg cache_dir} or use {.fn cacs_cache_dir} defaults."
      ))
    }
  }
  normalizePath(cache_dir, mustWork = FALSE, winslash = "/")
}

#' Is the cache on for one kind of saved result?
#'
#' Answers for the whole package when `namespace` is `NULL`, and for one kind
#' of saved result otherwise. The package-wide settings come first, so that a
#' machine or a test run can turn the cache off whatever a script asks for:
#' `CACS_NO_CACHE`, then `CACS_CACHE_ENABLED`, then
#' `options(catchmentACS.cache_enabled = )`. Then, for a namespace,
#' `options(catchmentACS.cache_<namespace> = )` decides when it is `TRUE` or
#' `FALSE`, and `CACS_CACHE_<NAMESPACE>` turns the cache on only for `1`,
#' `true`, `yes`, or `on`. Here the option beats the environment variable, the
#' other way round from the package-wide settings above. The "Environment
#' variables and options" section of `?cacs_set_cache` says the same for
#' users.
#'
#' @keywords internal
#' @noRd
.cacs_cache_enabled <- function(namespace = NULL) {
  global_enabled <- TRUE
  opt <- getOption("catchmentACS.cache_enabled", NULL)
  if (is.logical(opt) && length(opt) == 1L && !is.na(opt)) {
    global_enabled <- isTRUE(opt)
  }
  no_cache <- Sys.getenv("CACS_NO_CACHE", unset = "")
  if (nzchar(no_cache) && tolower(no_cache) %in% c("1", "true", "yes", "on")) {
    return(FALSE)
  }
  env <- Sys.getenv("CACS_CACHE_ENABLED", unset = "")
  if (nzchar(env)) {
    global_enabled <- tolower(env) %in% c("1", "true", "yes", "on")
  }
  if (!isTRUE(global_enabled)) {
    return(FALSE)
  }
  if (!is.null(namespace) && is.character(namespace) && length(namespace) == 1L) {
    opt_ns <- getOption(paste0("catchmentACS.cache_", namespace), NULL)
    if (is.logical(opt_ns) && length(opt_ns) == 1L && !is.na(opt_ns)) {
      return(isTRUE(opt_ns))
    }
    env_ns <- Sys.getenv(paste0("CACS_CACHE_", toupper(namespace)), unset = "")
    if (nzchar(env_ns)) {
      return(tolower(env_ns) %in% c("1", "true", "yes", "on"))
    }
  }
  TRUE
}

# The environment variable that decides whether the cache is on, when one does.
.cacs_cache_env_override <- function() {
  no_cache <- Sys.getenv("CACS_NO_CACHE", unset = "")
  if (nzchar(no_cache) && tolower(no_cache) %in% c("1", "true", "yes", "on")) {
    return("CACS_NO_CACHE")
  }
  if (nzchar(Sys.getenv("CACS_CACHE_ENABLED", unset = ""))) {
    return("CACS_CACHE_ENABLED")
  }
  NA_character_
}

.cacs_cache_namespace_mode <- function(cache_dir = NULL) {
  if (.cacs_cache_test_mode(cache_dir)) "test" else "production"
}

.cacs_cache_test_mode <- function(cache_dir = NULL) {
  mode <- getOption("catchmentACS.cache_namespace_mode", NULL)
  if (is.character(mode) && length(mode) == 1L && !is.na(mode)) {
    if (identical(mode, "test")) return(TRUE)
    if (identical(mode, "production")) return(FALSE)
  }
  opt <- getOption("catchmentACS.cache_test_namespace", NULL)
  if (is.logical(opt) && length(opt) == 1L && !is.na(opt)) return(isTRUE(opt))
  opt2 <- getOption("catchmentACS.test_cache", NULL)
  if (is.logical(opt2) && length(opt2) == 1L && !is.na(opt2)) return(isTRUE(opt2))
  if (identical(Sys.getenv("CACS_CACHE_TEST_MODE", unset = ""), "1")) return(TRUE)
  if (identical(tolower(Sys.getenv("TESTTHAT", unset = "")), "true") &&
      is.null(cache_dir) &&
      is.null(getOption("catchmentACS.cache_dir", NULL)) &&
      !nzchar(Sys.getenv("CACS_CACHE_DIR", unset = ""))) {
    return(TRUE)
  }
  if (!is.null(cache_dir) && is.character(cache_dir) && length(cache_dir) == 1L) {
    path <- normalizePath(cache_dir, mustWork = FALSE, winslash = "/")
    if (grepl("(^|/)(tests|testthat)(/|$)", path)) return(TRUE)
  }
  FALSE
}

.cacs_cache_effective_namespace <- function(namespace, cache_dir = NULL) {
  stopifnot(namespace %in% .CACS_CACHE_PUBLIC_NAMESPACES)
  if (identical(namespace, "acs") && .cacs_cache_test_mode(cache_dir)) {
    return("acs_test")
  }
  namespace
}

.cacs_cache_namespace_dir <- function(namespace, cache_dir = NULL, create = TRUE) {
  base <- .cacs_cache_base_dir(cache_dir = cache_dir, create = create)
  ns <- .cacs_cache_effective_namespace(namespace, cache_dir = cache_dir)
  ns_dir <- file.path(base, ns)
  if (create && !dir.exists(ns_dir)) {
    ok <- tryCatch(
      dir.create(ns_dir, recursive = TRUE, showWarnings = FALSE),
      error = function(e) FALSE,
      warning = function(w) FALSE
    )
    if (!isTRUE(ok) || !dir.exists(ns_dir)) {
      return(NULL)
    }
  }
  ns_dir
}

# A cache folder outside the temporary folder of the R session lasts between
# sessions, so it is tidied once per session, the first time a saved result is
# read from it or written to it while the cache is on. Only files with the
# names the package gives -- a 64-character key and `.rds`, `.fingerprint`, or
# `.gpkg`, with `.tmp` for an unfinished write -- in the four subfolders are
# looked at. Deleted are:
#   - unfinished writes, and results or checksum files without their partner,
#     when they are more than a day old;
#   - results not used for getOption("catchmentACS.cache_max_age_days", 30)
#     days, with their checksum and GeoPackage files, and GeoPackage files
#     without a result that are as old. Reading a result resets its age
#     (.cacs_cache_get()).

.CACS_CACHE_FILE_PATTERN <- "^[0-9a-f]{64}[.](rds|fingerprint|gpkg)([.]tmp)?$"

.cacs_cache_max_age_days <- function() {
  days <- getOption("catchmentACS.cache_max_age_days", 30)
  if (!is.numeric(days) || length(days) != 1L || is.na(days) || days <= 0) {
    return(30)
  }
  as.numeric(days)
}

# On Windows, tempdir() and tempfile() separate folders with a backslash, and
# normalizePath() keeps the short form of a folder name (such as RUNNER~1) in
# a path that does not exist yet, so the paths are compared with forward
# slashes, both as given and normalized.

.cacs_cache_in_tempdir <- function(path) {
  slashes <- function(x) {
    if (.Platform$OS.type == "windows") gsub("\\", "/", x, fixed = TRUE) else x
  }
  tmp <- unique(slashes(c(tempdir(), normalizePath(tempdir(), winslash = "/", mustWork = FALSE))))
  candidates <- unique(slashes(c(path, normalizePath(path, winslash = "/", mustWork = FALSE))))
  any(vapply(candidates, function(p) {
    any(p == tmp | startsWith(p, paste0(tmp, "/")))
  }, logical(1)))
}

.cacs_cache_prune <- function(base, force = FALSE, now = Sys.time()) {
  if (!isTRUE(force)) {
    .cacs_cache_ensure_state()
    id <- normalizePath(base, winslash = "/", mustWork = FALSE)
    if (id %in% .cacs_cache_state$pruned) return(invisible(0L))
    .cacs_cache_state$pruned <- c(.cacs_cache_state$pruned, id)
    if (.cacs_cache_in_tempdir(base)) return(invisible(0L))
  }
  if (!dir.exists(base)) return(invisible(0L))

  max_age <- .cacs_cache_max_age_days()
  n_removed <- 0L
  for (ns in .CACS_CACHE_NAMESPACES) {
    ns_dir <- file.path(base, ns)
    if (!dir.exists(ns_dir)) next
    files <- list.files(ns_dir, pattern = .CACS_CACHE_FILE_PATTERN, full.names = TRUE)
    if (length(files) == 0L) next
    name <- basename(files)
    key <- substr(name, 1L, 64L)
    ext <- substring(name, 66L)
    age <- as.numeric(difftime(now, file.info(files)$mtime, units = "days"))
    has_rds <- key %in% key[ext == "rds"]
    has_fingerprint <- key %in% key[ext == "fingerprint"]
    old_keys <- key[ext == "rds" & age > max_age]
    drop <- (grepl("[.]tmp$", ext) & age > 1) |
      (ext == "rds" & !has_fingerprint & age > 1) |
      (ext == "fingerprint" & !has_rds & age > 1) |
      (key %in% old_keys & ext %in% c("rds", "fingerprint", "gpkg")) |
      (ext == "gpkg" & !has_rds & age > max_age)
    drop[is.na(drop)] <- FALSE
    if (any(drop)) {
      unlink(files[drop], force = TRUE)
      n_removed <- n_removed + sum(drop)
    }
  }
  invisible(n_removed)
}

.cacs_cache_fingerprint_path <- function(ns_dir, key) {
  file.path(ns_dir, paste0(key, ".fingerprint"))
}

.cacs_cache_value_digest <- function(value, algo = .CACS_CACHE_FINGERPRINT_ALGO) {
  digest::digest(value, algo = algo, serialize = TRUE)
}

.cacs_cache_fingerprint_meta <- function(value) {
  list(
    algorithm = .CACS_CACHE_FINGERPRINT_ALGO,
    digest = .cacs_cache_value_digest(value),
    schema_version = .CACS_CACHE_FINGERPRINT_SCHEMA,
    serialize = TRUE
  )
}

.cacs_cache_read_fingerprint <- function(path) {
  meta <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(meta)) return(NULL)
  if (!identical(meta$schema_version, .CACS_CACHE_FINGERPRINT_SCHEMA)) return(NULL)
  if (!identical(meta$algorithm, .CACS_CACHE_FINGERPRINT_ALGO)) return(NULL)
  if (!is.character(meta$digest) || length(meta$digest) != 1L ||
      !grepl("^[0-9a-f]{64}$", meta$digest)) {
    return(NULL)
  }
  if (!identical(meta$serialize, TRUE)) return(NULL)
  meta
}

.cacs_cache_invalidate_pair <- function(path, fingerprint_path) {
  unlink(c(path, paste0(path, ".tmp"), fingerprint_path, paste0(fingerprint_path, ".tmp")),
         force = TRUE)
  invisible(TRUE)
}

.cacs_cache_maybe_announce <- function(base) {
  .cacs_cache_ensure_state()
  if (!isTRUE(getOption("catchmentACS.cache_announce", FALSE))) return(invisible(FALSE))
  if (isTRUE(.cacs_cache_state$announced)) return(invisible(FALSE))
  .cacs_cache_state$announced <- TRUE
  .cli_inform_cache_enabled_announce(
    c("catchmentACS cache is enabled.",
      "i" = "Cache root: {.path {base}}.",
      "i" = "Disable for this session with {.code cacs_set_cache(FALSE)}."),
    phase = "cache"
  )
  invisible(TRUE)
}

.cacs_cache_stale_threshold <- function(value) {
  prov <- attr(value, "cacs_provenance") %||%
    attr(value, "cacs_acs_provenance")
  state <- if (is.list(prov)) prov$state else NA_character_
  if (is.character(state) && length(state) == 1L && !is.na(state) && nzchar(state)) {
    opt_name <- paste0("catchmentACS.stale_threshold_", toupper(state))
    opt_state <- getOption(opt_name, NULL)
    if (is.numeric(opt_state) && length(opt_state) == 1L && !is.na(opt_state)) {
      return(as.integer(opt_state))
    }
  }
  as.integer(getOption("catchmentACS.stale_threshold_rows", 1000L))
}

.cacs_cache_warn_stale_if_needed <- function(value, key, effective_namespace) {
  if (!effective_namespace %in% c("acs", "acs_test")) return(invisible(FALSE))
  if (is.null(value) || !is.data.frame(value)) return(invisible(FALSE))
  threshold <- .cacs_cache_stale_threshold(value)
  if (!is.finite(threshold) || threshold <= 0L || nrow(value) >= threshold) {
    return(invisible(FALSE))
  }
  prov <- attr(value, "cacs_provenance") %||%
    attr(value, "cacs_acs_provenance")
  n_variables <- if (is.list(prov) && length(prov$n_variables_received) == 1L) {
    suppressWarnings(as.integer(prov$n_variables_received))
  } else if ("variable" %in% names(value)) {
    length(unique(value$variable))
  } else {
    NA_integer_
  }
  min_variables <- as.integer(getOption("catchmentACS.stale_min_variables", 6L))
  if (!identical(nrow(value), 39L) &&
      !is.na(n_variables) &&
      n_variables < min_variables) {
    return(invisible(FALSE))
  }
  id <- paste0(effective_namespace, ":", key)
  if (!.cacs_cache_mark_stale_warned(id)) return(invisible(FALSE))
  state <- if (is.list(prov)) prov$state %||% NA_character_ else NA_character_
  year <- if (is.list(prov)) prov$year %||% NA_integer_ else NA_integer_
  geography <- if (is.list(prov)) prov$geography %||% NA_character_ else NA_character_
  .cli_warn_cache_stale_suspect(
    c("The ACS data read from the cache have only {nrow(value)} row{?s}.",
      "x" = "A saved copy with fewer than {threshold} row{?s} may be incomplete or out of date.",
      "i" = "State/year/geography: {.val {state}} / {.val {year}} / {.val {geography}}.",
      "i" = "If this is unexpected, remove the saved copy with",
      " " = "{.code cacs_clear_cache(namespace = \"{effective_namespace}\", confirm = FALSE)}",
      "i" = "or download the data again with {.code force_refresh = TRUE}.",
      "i" = "Set {.code options(catchmentACS.stale_threshold_rows = 0)} to turn this check off."),
    phase = effective_namespace
  )
  invisible(TRUE)
}


#' Read a saved result
#'
#' Returns `NULL` when there is nothing to use: saving is off for this kind of
#' result, no file has this key, or the file fails a check. A file that fails
#' is deleted with its checksum file, so the next call writes it again. A
#' `<key>.rds.tmp` file left by an interrupted write is never read.
#'
#' @keywords internal
#' @noRd
.cacs_cache_get <- function(key, namespace, cache_dir = NULL,
                            replay_conditions = TRUE) {
  stopifnot(is.character(key), length(key) == 1L, nzchar(key))
  stopifnot(namespace %in% .CACS_CACHE_PUBLIC_NAMESPACES)

  effective_namespace <- .cacs_cache_effective_namespace(namespace, cache_dir = cache_dir)
  if (!.cacs_cache_enabled(namespace)) {
    .cacs_cache_bump("misses", effective_namespace)
    return(NULL)
  }

  base   <- .cacs_cache_base_dir(cache_dir = cache_dir, create = FALSE)
  .cacs_cache_maybe_announce(base)
  .cacs_cache_prune(base)
  ns_dir <- file.path(base, effective_namespace)
  path   <- file.path(ns_dir, paste0(key, ".rds"))
  fp_path <- .cacs_cache_fingerprint_path(ns_dir, key)

  if (!file.exists(path)) {
    .cacs_cache_bump("misses", effective_namespace)
    return(NULL)
  }

  if (!file.exists(fp_path)) {
    .cli_inform_cache_legacy_invalidated(
      c("Legacy cache entry invalidated: {.val {effective_namespace}}/{substr(key, 1, 8)}...",
        "i" = "It was saved by an older version of catchmentACS and has no fingerprint file, so it will be computed again."),
      phase = effective_namespace
    )
    .cacs_cache_invalidate_pair(path, fp_path)
    .cacs_cache_bump("misses", effective_namespace)
    return(NULL)
  }

  meta <- .cacs_cache_read_fingerprint(fp_path)
  if (is.null(meta)) {
    .cli_inform_cache_fingerprint_mismatch(
      c("Cache fingerprint invalidated: {.val {effective_namespace}}/{substr(key, 1, 8)}...",
        "i" = "Its fingerprint file is missing, damaged, or of an unknown kind, so it will be computed again."),
      phase = effective_namespace
    )
    .cacs_cache_invalidate_pair(path, fp_path)
    .cacs_cache_bump("misses", effective_namespace)
    return(NULL)
  }

  read_ok <- TRUE
  obj <- tryCatch(
    readRDS(path),
    error = function(e) {
      read_ok <<- FALSE
      NULL
    }
  )
  if (!isTRUE(read_ok)) {
    .cli_inform_cache_fingerprint_mismatch(
      c("Cache fingerprint invalidated: {.val {effective_namespace}}/{substr(key, 1, 8)}...",
        "i" = "The saved file could not be read, so it will be computed again."),
      phase = effective_namespace
    )
    .cacs_cache_invalidate_pair(path, fp_path)
    .cacs_cache_bump("misses", effective_namespace)
    return(NULL)
  }

  actual <- .cacs_cache_value_digest(obj, algo = meta$algorithm)
  if (!identical(actual, meta$digest)) {
    .cli_inform_cache_fingerprint_mismatch(
      c("Cache fingerprint invalidated: {.val {effective_namespace}}/{substr(key, 1, 8)}...",
        "i" = "The saved file does not match its fingerprint file, so it will be computed again."),
      phase = effective_namespace
    )
    .cacs_cache_invalidate_pair(path, fp_path)
    .cacs_cache_bump("misses", effective_namespace)
    return(NULL)
  }

  # A result that is used counts as new for the tidying of lasting folders.
  suppressWarnings(try(Sys.setFileTime(c(path, fp_path), Sys.time()), silent = TRUE))

  cached_conditions <- .cacs_cache_conditions(obj)
  if (isTRUE(replay_conditions) && length(cached_conditions) > 0L) {
    obj <- .cacs_cache_strip_conditions(obj)
    on.exit(.cacs_cache_signal_conditions(cached_conditions), add = TRUE)
  }

  .cacs_cache_bump("hits", effective_namespace)
  .cacs_cache_warn_stale_if_needed(obj, key, effective_namespace)
  .cli_inform_cache(c("Cache hit: {.val {effective_namespace}}/{substr(key, 1, 8)}..."),
                    phase = effective_namespace)
  obj
}


#' Save one result
#'
#' Writes `<key>.rds.tmp` and renames it to `<key>.rds`, so a process that is
#' killed leaves a `.tmp` file rather than a half-written `.rds`. The checksum
#' file is written the same way, and both files are deleted when that fails,
#' rather than leaving a result no later call can use. A failed write gives a
#' warning of class `catchmentACS_warning_runtime` and returns `FALSE`; the
#' caller keeps the result it has just computed.
#'
#' @keywords internal
#' @noRd
.cacs_cache_put <- function(value, key, namespace, cache_dir = NULL,
                            conditions = NULL) {
  stopifnot(is.character(key), length(key) == 1L, nzchar(key))
  stopifnot(namespace %in% .CACS_CACHE_PUBLIC_NAMESPACES)

  effective_namespace <- .cacs_cache_effective_namespace(namespace, cache_dir = cache_dir)
  if (!.cacs_cache_enabled(namespace)) {
    return(invisible(FALSE))
  }

  base <- tryCatch(
    .cacs_cache_base_dir(cache_dir = cache_dir, create = TRUE),
    catchmentACS_error_operator = function(e) NULL
  )
  if (is.null(base)) {
    .cli_warn_runtime(c(
      "Cache write failed: the cache folder cannot be created.",
      "i" = "The result is returned but not saved; see {.fn cacs_cache_dir}."
    ), phase = effective_namespace)
    return(invisible(FALSE))
  }
  .cacs_cache_maybe_announce(base)
  .cacs_cache_prune(base)
  ns_dir <- file.path(base, effective_namespace)
  if (!dir.exists(ns_dir)) {
    ok <- tryCatch(
      dir.create(ns_dir, recursive = TRUE, showWarnings = FALSE),
      error = function(e) FALSE
    )
    if (!isTRUE(ok)) {
      .cli_warn_runtime(c(
        "Cache write failed: cannot create namespace directory {.path {ns_dir}}.",
        "i" = "Result will be recomputed on next call (no caching applied)."
      ), phase = effective_namespace)
      return(invisible(FALSE))
    }
  }

  path     <- file.path(ns_dir, paste0(key, ".rds"))
  path_tmp <- paste0(path, ".tmp")
  fp_path  <- .cacs_cache_fingerprint_path(ns_dir, key)
  fp_tmp   <- paste0(fp_path, ".tmp")
  cached_conditions <- .cacs_cache_prepare_conditions(conditions)
  value <- .cacs_cache_strip_conditions(value)
  if (length(cached_conditions) > 0L) {
    attr(value, .CACS_CACHE_CONDITIONS_ATTR) <- cached_conditions
  }
  fp_meta  <- .cacs_cache_fingerprint_meta(value)

  written <- tryCatch({
    saveRDS(value, path_tmp, compress = "xz")
    if (file.exists(path)) unlink(path, force = TRUE)
    file.rename(path_tmp, path)
  }, error = function(e) FALSE,
     warning = function(w) FALSE)

  if (!isTRUE(written)) {
    if (file.exists(path_tmp)) try(unlink(path_tmp), silent = TRUE)
    .cli_warn_runtime(c(
      "Cache write failed for {.val {effective_namespace}}/{substr(key, 1, 8)}...",
      "i" = "The result is returned as computed; it was not saved."
    ), phase = effective_namespace)
    return(invisible(FALSE))
  }

  fp_written <- tryCatch({
    saveRDS(fp_meta, fp_tmp, compress = FALSE)
    if (file.exists(fp_path)) unlink(fp_path, force = TRUE)
    file.rename(fp_tmp, fp_path)
  }, error = function(e) FALSE,
     warning = function(w) FALSE)

  if (!isTRUE(fp_written)) {
    .cacs_cache_invalidate_pair(path, fp_path)
    .cli_warn_runtime(c(
      "Cache fingerprint write failed for {.val {effective_namespace}}/{substr(key, 1, 8)}...",
      "i" = "The result is returned as computed; the saved copy was removed."
    ), phase = effective_namespace)
    return(invisible(FALSE))
  }

  invisible(TRUE)
}

if (is.null(.cacs_cache_state$hits)) {
  .cacs_cache_reset_state()
}
