# ============================================================================
# Phase 5 per-class condition tests.
# ============================================================================


.phase5_condition_cases <- list(
  list(
    name = "water_tract_filter",
    leaf = "catchmentACS_message_water_tract_filter",
    parent = "catchmentACS_message",
    emit = function() .cacs_emit("inform", "water_tract_filter",
                                 "Synthetic water", phase = "phase5")
  ),
  list(
    name = "progress_tick",
    leaf = "catchmentACS_message_progress_tick",
    parent = "catchmentACS_message_progress",
    emit = function() .cacs_emit("inform", "progress_tick",
                                 "Synthetic tick", phase = "phase5")
  ),
  list(
    name = "progress_summary",
    leaf = "catchmentACS_message_progress_summary",
    parent = "catchmentACS_message_progress",
    emit = function() .cacs_emit("inform", "progress_summary",
                                 "Synthetic summary", phase = "phase5")
  ),
  list(
    name = "res_default_changed",
    leaf = "catchmentACS_message_res_default_changed",
    parent = "catchmentACS_message",
    emit = function() .cacs_emit("inform", "res_default_changed",
                                 "Synthetic res notice", phase = "phase5")
  ),
  list(
    name = "cache_legacy_invalidated",
    leaf = "catchmentACS_message_cache_legacy_invalidated",
    parent = "catchmentACS_message_cache",
    emit = function() .cacs_emit("inform", "cache_legacy_invalidated",
                                 "Synthetic legacy cache", phase = "phase5")
  ),
  list(
    name = "cache_fingerprint_mismatch",
    leaf = "catchmentACS_message_cache_fingerprint_mismatch",
    parent = "catchmentACS_message_cache",
    emit = function() .cacs_emit("inform", "cache_fingerprint_mismatch",
                                 "Synthetic fingerprint", phase = "phase5")
  ),
  list(
    name = "cache_enabled_announce",
    leaf = "catchmentACS_message_cache_enabled_announce",
    parent = "catchmentACS_message_cache",
    emit = function() .cacs_emit("inform", "cache_enabled_announce",
                                 "Synthetic cache enabled", phase = "phase5")
  ),
  list(
    name = "cache_stale_suspect",
    leaf = "catchmentACS_warning_cache_stale_suspect",
    parent = "catchmentACS_warning_cache",
    emit = function() .cacs_emit("warn", "cache_stale_suspect",
                                 "Synthetic stale cache", phase = "phase5")
  ),
  list(
    name = "rate_out_of_range",
    leaf = "catchmentACS_warning_rate_out_of_range",
    parent = "catchmentACS_warning",
    emit = function() .cacs_emit("warn", "rate_out_of_range",
                                 "Synthetic rate warning", phase = "phase5")
  )
)


.phase5_catch_condition <- function(emit) {
  rlang::catch_cnd(emit())
}


for (case in .phase5_condition_cases) {
  local({
    case <- case

    test_that(paste0("P5-CLASS-", case$name, "-01 class chain is locked"), {
      cnd <- .phase5_catch_condition(case$emit)

      expect_s3_class(cnd, case$leaf)
      expect_s3_class(cnd, case$parent)
      expect_s3_class(cnd, "catchmentACS_condition")
    })

    test_that(paste0("P5-CLASS-", case$name, "-02 message is formatted"), {
      cnd <- .phase5_catch_condition(case$emit)

      expect_type(conditionMessage(cnd), "character")
      expect_true(nzchar(conditionMessage(cnd)))
      expect_match(conditionMessage(cnd), "Synthetic", fixed = TRUE)
    })

    test_that(paste0("P5-CLASS-", case$name, "-03 leaf subscription fires"), {
      out <- cacs_capture_conditions(case$emit(), classes = case$leaf)

      expect_equal(nrow(out), 1L)
      expect_equal(out$class, case$leaf)
      expect_equal(out$phase, "phase5")
    })

    test_that(paste0("P5-CLASS-", case$name, "-04 parent subscription fires"), {
      out <- cacs_capture_conditions(case$emit(), classes = case$parent)

      expect_equal(nrow(out), 1L)
      expect_equal(out$class, case$leaf)
    })

    test_that(paste0("P5-CLASS-", case$name, "-05 registry includes class"), {
      known <- .cacs_known_classes(include_parents = TRUE)

      expect_true(case$leaf %in% known)
      expect_true(case$parent %in% known)
    })
  })
}


test_that("P5-CLASS-progress-throttle uses configured sparse tick cadence", {
  withr::with_envvar(c(CACS_QUIET = ""), {
    withr::with_options(list(catchmentACS.progress = "force",
                             catchmentACS.progress_throttle = 20L), {
      out <- cacs_capture_conditions({
        prog <- .cacs_progress_reporter(75L, "Phase5Throttle",
                                        verbose = TRUE)
        for (i in seq_len(75L)) {
          prog$tick(detail = paste0("i", i))
        }
      }, classes = "catchmentACS_message_progress_tick")
    })
  })

  expect_equal(nrow(out), 5L) # 1, 21, 41, 61, 75
})
