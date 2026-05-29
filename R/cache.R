# ============================================================================
# cache.R - Layer 1 (memoise) + Layer 2 (rappdirs + SHA-256) cache.
#           Sec. 9.2 three layers, Sec. 9.5 hash utility, Sec. 19.6 / Sec. 20.4 / Sec. 21.8
#           namespace dims. Single dispatch factory.
#
# NOTE: Sec. 9.5 specifies SHA-256, but the `digest` package (in Imports)
#       does not expose SHA-256 - only sha256, sha512, blake3, etc.
#       Using sha256 (64-char hex, functionally equivalent for cache-key
#       stability). Blueprint v1.0.2 patch candidate: Sec. 9.5 should be
#       updated to "SHA-256" or move openssl from Suggests to Imports.
#
# Robustness value-adds:
#   * memoise wraps cacs_cache_dir() for L1 in-session lookup
#   * Permission failure on dir.create() -> .cli_abort_operator() (Sec. 13.5)
#   * Atomic write: <key>.rds.tmp -> file.rename() to <key>.rds
#   * Stale .rds.tmp tolerance in .cacs_cache_get() + audit in cacs_cache_status()
# ============================================================================


# ---- Internal: namespace-to-dims lookup (single source) --------------------

.CACS_CACHE_NAMESPACE_DIMS <- list(
  isochrone = 16L,   # Sec. 19.6 + v0.3 ring_topology
  acs       = 12L,   # Sec. 20.4
  intersect = 14L    # Sec. 21.8 composite + v0.3 iso topology/res
)

.CACS_CACHE_NAMESPACES <- c("isochrone", "acs", "acs_test", "intersect")
.CACS_CACHE_PUBLIC_NAMESPACES <- c("isochrone", "acs", "intersect")
.CACS_CACHE_FINGERPRINT_ALGO <- "sha256"
.CACS_CACHE_FINGERPRINT_SCHEMA <- "cache-v0.3"
.CACS_CACHE_CONDITIONS_ATTR <- "cacs_cached_conditions"

.cacs_cache_state <- new.env(parent = emptyenv())


# ---- Internal: diagnostic cache state -------------------------------------

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

  # Public counters intentionally expose one ACS row: it aggregates production
  # `acs/` activity and test-isolated `acs_test/` activity for a stable user
  # surface across ordinary and testthat sessions.
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


# ---- Internal: cached condition metadata ----------------------------------

.cacs_cache_prepare_conditions <- function(conditions) {
  if (is.null(conditions)) return(list())
  if (inherits(conditions, "condition")) {
    conditions <- list(conditions)
  }
  if (!is.list(conditions)) return(list())
  Filter(function(cnd) inherits(cnd, "condition"), conditions)
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


# ---- Internal: Layer 1 memoise wrapper for cache-dir lookup ----------------

.cacs_cache_dir_impl <- function(create = TRUE) {
  # Resolution: option -> env -> rappdirs default
  override_opt <- getOption("catchmentACS.cache_dir", NULL)
  override_env <- Sys.getenv("CACS_CACHE_DIR", unset = NA)

  path <- if (!is.null(override_opt) && nzchar(override_opt)) {
    override_opt
  } else if (!is.na(override_env) && nzchar(override_env)) {
    override_env
  } else {
    rappdirs::user_cache_dir("catchmentACS")
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

.cacs_cache_dir_memo <- memoise::memoise(.cacs_cache_dir_impl)


# ---- Exported: cacs_cache_dir() --------------------------------------------

#' Get (or create) the catchmentACS cache directory
#'
#' Returns the absolute path to the user-level directory where catchmentACS
#' stores its persistent cache of isochrone, ACS, and intersect-weight
#' artifacts. The lookup is memoised so repeated calls within a session are
#' cheap. The directory is created on first use unless `create = FALSE`.
#'
#' The path is resolved in this order, with the first non-empty value winning:
#' 1. `options(catchmentACS.cache_dir)`, used mainly for test isolation.
#' 2. The `CACS_CACHE_DIR` environment variable, for a user override.
#' 3. `rappdirs::user_cache_dir("catchmentACS")`, the OS-native default.
#'
#' @section Cache contract:
#' A cached `<key>.rds` file is only trusted when it has a sibling
#' `<key>.fingerprint` sidecar. The sidecar is a small metadata object holding
#' the digest of the serialized value and a schema version tag. Missing
#' sidecars, malformed sidecars, unreadable `.rds` files, and digest mismatches
#' all fall back to a cache miss rather than returning stale data.
#'
#' ACS test fixtures are isolated in a separate `acs_test/` subdirectory while
#' test namespace mode is active; ordinary ACS reads use `acs/` and never fall
#' back to the test fixtures. Use [cacs_set_cache()] to turn caching on or off,
#' and [cacs_get_cache_state()] to inspect the enabled flag, namespace mode,
#' hit/miss counters, and sidecar status.
#'
#' Suspiciously small ACS cache hits emit a stale-cache warning. The row
#' threshold defaults to `options(catchmentACS.stale_threshold_rows = 1000L)`,
#' can be turned off with `0L`, and can be set per state, for example
#' `options(catchmentACS.stale_threshold_WY = 500L)`. Very small variable sets
#' are ignored unless they match a known stale shape; that guard is controlled
#' by `options(catchmentACS.stale_min_variables = 6L)`.
#'
#' @param create Logical (default `TRUE`). When `TRUE` and the directory does
#'   not yet exist, the directory is created recursively. A failure to create
#'   it (for example a read-only filesystem) raises an error.
#' @return A length-one character vector: the absolute path to the cache
#'   directory.
#' @seealso [cacs_set_cache()], [cacs_get_cache_state()], [cacs_clear_cache()],
#'   [cacs_cache_status()]; [cacs_acs_prefetch()] and [cacs_run()] are the main
#'   consumers of this cache.
#' @family cache and configuration
#' @export
#' @examples
#' \donttest{
#' cache_dir <- cacs_cache_dir(create = FALSE)
#' nzchar(cache_dir)
#' }
cacs_cache_dir <- function(create = TRUE) {
  .cacs_cache_dir_memo(create = create)
}


# ---- Exported: cacs_set_cache() / cacs_get_cache_state() ------------------

#' Enable or disable the catchmentACS cache
#'
#' Turns the package's persistent cache on or off. With `scope = "session"` the
#' change applies to the current R session only, via an option. With
#' `scope = "global"` it writes an idempotent settings block to your
#' `~/.Rprofile` so future sessions inherit the same setting.
#'
#' Caching is `enabled = TRUE` by default. Use session scope for reversible
#' debugging and reproducibility checks. Use global scope only when you want
#' the setting to persist across sessions; that write is guarded by a
#' confirmation prompt unless `confirm = FALSE` or the environment variable
#' `CACS_NO_CONFIRM = "1"` is set.
#'
#' @param enabled Logical scalar. `TRUE` enables cache reads and writes;
#'   `FALSE` disables them.
#' @param scope Either `"session"` (current session only) or `"global"`
#'   (persisted to `~/.Rprofile`).
#' @param confirm Logical; defaults to `interactive()`. Global writes prompt
#'   for confirmation unless `confirm = FALSE` or `CACS_NO_CONFIRM = "1"`.
#' @return Invisibly `TRUE` when the requested change was applied, or `FALSE`
#'   when a confirmation prompt was declined.
#' @seealso [cacs_get_cache_state()], [cacs_clear_cache()], [cacs_cache_dir()];
#'   [cacs_acs_prefetch()] and [cacs_run()] honor this setting.
#' @family cache and configuration
#' @export
#' @examples
#' cacs_set_cache(TRUE, scope = "session")
#' \dontrun{
#' cacs_set_cache(FALSE, scope = "global")
#' }
cacs_set_cache <- function(enabled = TRUE,
                           scope = c("session", "global"),
                           confirm = interactive()) {
  if (!is.logical(enabled) || length(enabled) != 1L || is.na(enabled)) {
    .cli_abort_schema("{.arg enabled} must be {.cls logical(1)}.")
  }
  scope <- match.arg(scope)

  if (identical(scope, "session")) {
    options(catchmentACS.cache_enabled = enabled)
    return(invisible(TRUE))
  }

  if (isTRUE(confirm) && !identical(Sys.getenv("CACS_NO_CONFIRM"), "1")) {
    cli::cli_inform(c(
      "About to update {.path ~/.Rprofile} with catchmentACS cache settings.",
      "i" = "Requested cache state: {.val {enabled}}."
    ))
    answer <- tryCatch(
      utils::askYesNo("Proceed with global cache setting?", default = FALSE),
      error = function(e) FALSE
    )
    if (!isTRUE(answer)) return(invisible(FALSE))
  }

  rprofile <- getOption("catchmentACS.rprofile_path", NULL)
  if (!is.character(rprofile) || length(rprofile) != 1L || !nzchar(rprofile)) {
    rprofile <- Sys.getenv("R_PROFILE_USER", unset = "~/.Rprofile")
  }
  rprofile <- path.expand(rprofile)
  parent <- dirname(rprofile)
  if (!dir.exists(parent)) {
    .cli_abort_operator(c(
      "Cannot update {.path {rprofile}}.",
      "x" = "Parent directory does not exist.",
      "i" = "Use {.code cacs_set_cache({enabled}, scope = \"session\")} instead."
    ))
  }
  if (file.exists(rprofile) && file.access(rprofile, mode = 2L) != 0L) {
    .cli_abort_operator(c(
      "Cannot update {.path {rprofile}}.",
      "x" = "File is not writable.",
      "i" = "Use {.code cacs_set_cache({enabled}, scope = \"session\")} instead."
    ))
  }

  old <- if (file.exists(rprofile)) readLines(rprofile, warn = FALSE) else character(0)
  start <- "# catchmentACS v0.3 cache control"
  end <- "# end catchmentACS v0.3 cache control"
  block <- c(
    start,
    sprintf("options(catchmentACS.cache_enabled = %s)", toupper(as.character(enabled))),
    end
  )
  start_i <- which(old == start)
  end_i <- which(old == end)
  if (length(start_i) > 0L && length(end_i) > 0L && start_i[[1L]] < end_i[[1L]]) {
    s <- start_i[[1L]]
    e <- end_i[end_i > s][[1L]]
    pre <- if (s > 1L) old[seq_len(s - 1L)] else character(0)
    post <- if (e < length(old)) old[seq.int(e + 1L, length(old))] else character(0)
    new <- c(pre, block, post)
  } else {
    new <- c(old, if (length(old) > 0L) "" else character(0), block)
  }

  ok <- tryCatch({
    writeLines(new, rprofile, useBytes = TRUE)
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) {
    .cli_abort_operator(c(
      "Cannot update {.path {rprofile}}.",
      "x" = "{.fn writeLines} failed.",
      "i" = "Use {.code cacs_set_cache({enabled}, scope = \"session\")} instead."
    ))
  }
  options(catchmentACS.cache_enabled = enabled)
  invisible(TRUE)
}

#' Inspect cache configuration and session counters
#'
#' Returns a snapshot of how the catchmentACS cache is currently configured
#' together with the read counters accumulated so far this session. The
#' snapshot reports whether caching is enabled, where the cache lives, the
#' active namespace mode, the digest algorithm, and per-namespace hit/miss
#' tallies. Use it to confirm your cache settings and to diagnose why a repeat
#' workflow is or is not benefiting from the cache.
#'
#' @return A one-row tibble describing whether caching is enabled, the cache
#'   root directory, the namespace mode, the digest algorithm, public hit-rate
#'   counters, the raw per-subdirectory hit/miss counters, the namespace
#'   summary from [cacs_cache_status()], and the session start time the
#'   counters are measured from. The `counters` list-column holds a three-row
#'   tibble (`isochrone`, `acs`, `intersect`) with `hit`, `miss`, and
#'   `hit_rate`; the `acs` row combines the ordinary `acs/` and test-isolated
#'   `acs_test/` activity so the table keeps a stable three-row shape in both
#'   ordinary and test sessions.
#'
#' @section Hit-rate observability:
#' Cache counters are session-local diagnostics for the read path. A successful
#' cache read increments `hit`; a missing entry, a disabled namespace, an entry
#' without a fingerprint sidecar, an unreadable `.rds` file, a malformed
#' sidecar, or a digest mismatch increments `miss`, because the caller then has
#' to recompute the artifact. `hit_rate` is `hit / (hit + miss)` and is `NaN`
#' before a namespace has had any reads. Counters start at package load and are
#' reset for a namespace when [cacs_clear_cache()] removes its files. If a
#' repeat workflow is still slow, inspect `cacs_get_cache_state()$counters[[1]]`:
#' zero hits in a namespace usually means the second run is not reading the
#' cache entries you expect.
#' @seealso [cacs_set_cache()], [cacs_clear_cache()], [cacs_cache_status()],
#'   [cacs_cache_dir()]; [cacs_acs_prefetch()] and [cacs_run()] populate these
#'   counters.
#' @family cache and configuration
#' @export
#' @examples
#' cacs_get_cache_state()
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


# ---- Exported: cacs_clear_cache() ------------------------------------------

#' Clear cached catchmentACS artifacts
#'
#' Removes the `.rds` files, their `.fingerprint` sidecars, and any leftover
#' temporary files from the requested namespace subdirectories of
#' [cacs_cache_dir()], then resets the in-session counters and memoised path
#' lookup so the next call starts from a clean slate.
#'
#' @param namespace Which cache to clear: one of `"all"`, `"isochrone"`,
#'   `"acs"`, `"acs_test"`, or `"intersect"`.
#' @param confirm Logical; defaults to `interactive()`. When `TRUE`, prompts
#'   before deletion. The prompt is skipped in non-interactive sessions and
#'   when the environment variable `CACS_NO_CONFIRM = "1"` is set.
#' @return Invisibly, a tibble with one row per cleared namespace and columns
#'   `namespace`, `n_removed`, and `bytes_freed`.
#' @seealso [cacs_cache_status()], [cacs_cache_dir()], [cacs_set_cache()],
#'   [cacs_get_cache_state()]; [cacs_acs_prefetch()] and [cacs_run()] write the
#'   entries this removes.
#' @family cache and configuration
#' @export
#' @examples
#' \dontrun{
#' cacs_clear_cache("isochrone", confirm = FALSE)
#' }
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
    ))
    memoise::forget(.cacs_cache_dir_memo)
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
      .cli_inform_cache("Aborted by user; no files removed.")
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
    files <- list.files(
      ns_dir,
      pattern = "\\.(rds|fingerprint)(\\.tmp)?$",
      full.names = TRUE
    )
    if (length(files) == 0L) {
      return(tibble::tibble(namespace = ns, n_removed = 0L, bytes_freed = 0))
    }
    sz <- sum(file.info(files)$size, na.rm = TRUE)
    unlink(files, force = TRUE)
    tibble::tibble(namespace = ns, n_removed = length(files), bytes_freed = sz)
  })

  out <- do.call(rbind, summary)
  memoise::forget(.cacs_cache_dir_memo)
  .cacs_cache_reset_counters(ns_list)

  .cli_inform_cache(c(
    "Cleared cache namespaces: {.val {ns_list}}.",
    "i" = "Removed {.val {sum(out$n_removed)}} file{?s}, freed {.val {pretty_bytes(sum(out$bytes_freed))}}."
  ))

  invisible(out)
}


# ---- Exported: cacs_cache_status() -----------------------------------------

#' Summarize cached catchmentACS artifacts on disk
#'
#' Reports what is currently stored in each cache namespace: how many `.rds`
#' entries and fingerprint sidecars exist, how many orphaned temporary files
#' remain, how many entries lack a sidecar, the total size on disk, and the
#' oldest and newest modification times. Use it to size and audit the cache.
#'
#' @return A tibble with one row per namespace and the columns `namespace`,
#'   `n_entries`, `orphan_tmp`, `n_fingerprints`, `orphan_fingerprint_tmp`,
#'   `legacy_entries`, `total_size_mb`, `oldest`, and `newest`.
#' @seealso [cacs_clear_cache()], [cacs_cache_dir()], [cacs_get_cache_state()],
#'   [cacs_set_cache()]; [cacs_acs_prefetch()] and [cacs_run()] create these
#'   entries.
#' @family cache and configuration
#' @export
#' @examples
#' \donttest{
#' cacs_cache_status()
#' }
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
    rds <- list.files(ns_dir, pattern = "\\.rds$", full.names = TRUE)
    tmp <- list.files(ns_dir, pattern = "\\.rds\\.tmp$", full.names = TRUE)
    fingerprint <- list.files(ns_dir, pattern = "\\.fingerprint$", full.names = TRUE)
    fingerprint_tmp <- list.files(ns_dir, pattern = "\\.fingerprint\\.tmp$", full.names = TRUE)
    all_files <- c(rds, tmp, fingerprint, fingerprint_tmp)
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


# ---- Internal: .cacs_cache_key() - single dispatch factory -----------------

#' SHA-256 cache key factory (internal single dispatch)
#'
#' Used by Sec. 19.6 (isochrone, 16-dim), Sec. 20.4 (ACS, 12-dim),
#' and Sec. 21.8 (intersect composite, 14-dim). Payload ordering is
#' normalized recursively for call-order invariance.
#'
#' @param payload named list of cache key components.
#' @param namespace one of `"isochrone"`, `"acs"`, `"intersect"`; expected
#'   payload length is looked up via `.CACS_CACHE_NAMESPACE_DIMS`.
#' @return character(1) SHA-256 hex digest.
#' @keywords internal
#' @noRd
.cacs_cache_key <- function(payload,
                            namespace = c("isochrone", "acs", "intersect")) {
  namespace <- match.arg(namespace)
  expected_dims <- .CACS_CACHE_NAMESPACE_DIMS[[namespace]]

  if (!is.list(payload)) {
    .cli_abort_schema(c(
      "{.arg payload} must be a {.cls list}.",
      "x" = "Got {.cls {class(payload)[[1]]}}.",
      "i" = "Cache key factory contract."
    ))
  }
  if (length(payload) != expected_dims) {
    .cli_abort_schema(c(
      "{.arg payload} length mismatch for namespace {.val {namespace}}.",
      "x" = "Expected {.val {expected_dims}} components, got {.val {length(payload)}}.",
      "i" = "isochrone = 16L, ACS = 12L, intersect = 14L in the v0.3 cache contract."
    ))
  }
  nm <- names(payload)
  if (is.null(nm) || any(!nzchar(nm)) || anyDuplicated(nm)) {
    .cli_abort_schema(c(
      "{.arg payload} must have unique non-empty names on every element."
    ))
  }

  payload_sorted <- .cacs_cache_normalize_payload(payload[order(nm)])
  # NOTE: Sec. 9.5 calls for SHA-256; digest package does not expose it,
  # so we use SHA-256 (64-char hex, deterministic, equivalent for cache
  # key stability). Blueprint v1.0.2 patch candidate.
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


# ---- Internal: .cacs_cache_get() - read with stale-tmp tolerance -----------

#' Read an object from the cache (internal)
#'
#' Returns `NULL` on cache miss. Stale `<key>.rds.tmp` files are ignored.
#'
#' @keywords internal
#' @noRd
.cacs_cache_base_dir <- function(cache_dir = NULL, create = TRUE) {
  if (is.null(cache_dir)) {
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
    c("Suspicious ACS cache hit: only {.val {nrow(value)}} row{?s}.",
      "x" = "Threshold for stale-cache suspicion is {.val {threshold}} row{?s}.",
      "i" = "State/year/geography: {.val {state}} / {.val {year}} / {.val {geography}}.",
      "i" = "If this is unexpected, run {.code cacs_clear_cache(namespace = \"{effective_namespace}\", confirm = FALSE)} or retry with {.arg force_refresh = TRUE}.",
      "i" = "Set {.code options(catchmentACS.stale_threshold_rows = 0)} to disable this heuristic."),
    phase = effective_namespace
  )
  invisible(TRUE)
}


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
        "i" = "Missing v0.3 fingerprint sidecar; entry will be recomputed."),
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
        "i" = "Sidecar metadata is missing, malformed, or uses an unsupported algorithm."),
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
        "i" = "Cached RDS could not be read."),
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
        "i" = "Stored object digest no longer matches its sidecar."),
      phase = effective_namespace
    )
    .cacs_cache_invalidate_pair(path, fp_path)
    .cacs_cache_bump("misses", effective_namespace)
    return(NULL)
  }

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


# ---- Internal: .cacs_cache_put() - atomic write ----------------------------

#' Write an object to the cache (internal, atomic)
#'
#' Writes to `<key>.rds.tmp` then `file.rename()` to `<key>.rds` so a killed
#' process leaves an orphan `.tmp` rather than a partial `.rds`. On write
#' failure emits W-12 per Sec. 9.5 and returns invisible(FALSE).
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

  base   <- .cacs_cache_base_dir(cache_dir = cache_dir, create = TRUE)
  .cacs_cache_maybe_announce(base)
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
      "i" = "Result is fine; cache layer was bypassed."
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
      "i" = "Result is fine; cache entry was rolled back."
    ), phase = effective_namespace)
    return(invisible(FALSE))
  }

  invisible(TRUE)
}

if (is.null(.cacs_cache_state$hits)) {
  .cacs_cache_reset_state()
}
