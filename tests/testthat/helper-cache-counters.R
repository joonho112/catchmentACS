# Shared helpers for cache-counter observability tests.

reset_cache_counters <- function(namespace = "all") {
  .cacs_cache_reset_counters(namespace)
}

cache_counter_state <- function() {
  cacs_get_cache_state()$counters[[1]]
}

cache_counter_row <- function(namespace) {
  rows <- cache_counter_state()
  rows[rows$namespace == namespace, , drop = FALSE]
}
