# =============================================================================
# PIPELINE SCHEMA CONTRACT CHECK
#
# WHY THIS EXISTS
# On 2026-08-09 three separate output whitelists silently dropped a newly
# computed column, each without raising an error:
#   R/29  the NSE/schema select
#   R/29  update_projection_with_ytd()  -- its own comment warns about this
#   R/33  the final "Select final schema columns" block
# Each cost a debugging cycle. dplyr::select() drops silently by design, so a
# column computed upstream that is not named in a downstream select simply
# ceases to exist with no diagnostic.
#
# WHAT THIS DOES
# Declares the columns each artifact is CONTRACTED to carry, then checks the
# artifacts on disk. It changes no pipeline code and cannot break a run. Run it
# after any pipeline change that adds or renames a column.
#
# USAGE
#   source("R/52_schema_validation.R")
#   validate_pipeline_schema()                       # production cache
#   validate_pipeline_schema(cache_dir = ..., prefix = "bt2025_")
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(glue)
})

# ------------------------------------------------------------------------------
# CONTRACTS
# ------------------------------------------------------------------------------
# Add a column here at the same time you add it to the pipeline. If it is here
# and missing from the artifact, this check fails loudly, which is the entire
# point.

SCHEMA_CONTRACTS <- list(

  r29_projections = list(
    file = "player_projections.csv",
    type = "csv",
    required = c(
      "player_uid", "nfl_gsis_id", "player_name", "position", "team",
      "prior_mu", "prior_sigma", "posterior_mu", "posterior_sigma",
      "prior_source", "n_nfl_seasons", "current_age",
      "prior_mu_pre_age", "aging_delta_ppg",
      "injury_mu_adj", "ramp_mu_adj", "volume_implied_ppg", "volume_blend_adj",
      # added 2026-08-09, dropped twice before reaching the output
      "w_translation", "translation_observed_fraction", "hist_mean_used"
    )
  ),

  r32_reconciled = list(
    file = "reconciled_projections.rds",
    type = "rds",
    required = c(
      "player_uid", "nfl_gsis_id", "player_name", "position", "team",
      "prior_source",
      "r29_posterior_mu", "r29_posterior_sigma",
      "r32_posterior_mu", "r32_posterior_sigma", "r32_delta_from_r29",
      "r32_projection_lower_80", "r32_projection_upper_80",
      "volume_implied_ppg_v2", "blend_weight_r31", "blend_weight_r31_static",
      "qb_anchor_pass_ppg", "qb_anchor_rush_ppg",
      "qb_discount_factor",
      "boom_probability", "bust_probability",
      "r32_omega", "r32_sigma_week"
    )
  ),

  r31_allocation = list(
    file = "player_volume_allocation.rds",
    type = "rds",
    required = c(
      "nfl_gsis_id", "player_name", "team", "position",
      "depth_position", "depth_rank", "is_rookie",
      "target_share", "rush_share",
      "expected_targets_pg", "expected_carries_pg",
      "carryover_applied", "carryover_prior_share",
      "talent_source", "talent_multiplier"
    )
  ),

  r30_team_volumes = list(
    file = "team_volumes.rds",
    type = "rds",
    required = c(
      "team", "season", "qb_quality_score", "qb_id",
      "projected_pass_pg", "projected_rush_pg",
      "projected_pass_yds_pg", "projected_pass_tds_pg",
      "pass_sos_factor", "rush_sos_factor"
    )
  )
)

# Columns produced by R/33 rather than read from disk. Checked separately by
# validate_vorp_schema() because compute_vorp_rankings() returns a frame.
VORP_REQUIRED <- c(
  "player_uid", "nfl_gsis_id", "player_name", "team", "position",
  "r32_posterior_mu", "r32_posterior_mu_ppg", "vorp_availability",
  "replacement_ppg", "vorp_base", "adjusted_vorp",
  "boom_modifier", "bust_modifier", "ceiling_modifier",
  "overall_rank", "position_rank"
)

# ------------------------------------------------------------------------------

.read_artifact <- function(path, type) {
  if (!file.exists(path)) return(NULL)
  if (identical(type, "rds")) readRDS(path)
  else readr::read_csv(path, n_max = 5, show_col_types = FALSE,
                       progress = FALSE)
}

#' Check pipeline artifacts against their declared column contracts
#'
#' @param cache_dir Directory holding the artifacts.
#' @param prefix Filename prefix, e.g. "s2_week15_" or "bt2025_".
#' @param stop_on_fail Logical. TRUE raises an error; FALSE reports and returns.
#' @return Invisibly, a tibble of results.
validate_pipeline_schema <- function(
    cache_dir = here::here("data", "season2_cache"),
    prefix = "s2_week15_",
    stop_on_fail = FALSE) {

  cat(glue("
=== PIPELINE SCHEMA CHECK ===
dir   : {cache_dir}
prefix: {prefix}
"), "\n\n")

  out <- purrr::imap_dfr(SCHEMA_CONTRACTS, function(spec, nm) {
    path <- file.path(cache_dir, paste0(prefix, spec$file))
    dat  <- .read_artifact(path, spec$type)

    if (is.null(dat)) {
      cat(glue("  {nm}: FILE NOT FOUND -- {basename(path)}"), "\n")
      return(tibble::tibble(artifact = nm, status = "missing_file",
                            n_missing = NA_integer_, missing = NA_character_))
    }

    miss <- setdiff(spec$required, names(dat))
    if (length(miss) == 0) {
      cat(glue("  {nm}: OK ({length(spec$required)} contracted columns present, ",
               "{ncol(dat)} total)"), "\n")
      tibble::tibble(artifact = nm, status = "ok",
                     n_missing = 0L, missing = NA_character_)
    } else {
      cat(glue("  {nm}: MISSING {length(miss)} -- ",
               "{paste(miss, collapse = ', ')}"), "\n")
      tibble::tibble(artifact = nm, status = "missing_columns",
                     n_missing = length(miss),
                     missing = paste(miss, collapse = ", "))
    }
  })

  bad <- out %>% dplyr::filter(.data$status != "ok")
  cat("\n")
  if (nrow(bad) == 0) {
    cat("ALL CONTRACTS SATISFIED\n")
  } else {
    msg <- glue("{nrow(bad)} artifact(s) failed the schema contract. A column ",
                "computed upstream is not reaching the output; check the ",
                "dplyr::select() whitelists between where it is created and ",
                "where it is written.")
    if (isTRUE(stop_on_fail)) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }
  invisible(out)
}

#' Check the frame returned by compute_vorp_rankings()
#'
#' @param vorp_df Output of compute_vorp_rankings().
#' @param stop_on_fail Logical.
validate_vorp_schema <- function(vorp_df, stop_on_fail = FALSE) {
  miss <- setdiff(VORP_REQUIRED, names(vorp_df))
  cat("=== VORP SCHEMA CHECK ===\n")
  if (length(miss) == 0) {
    cat(glue("  OK ({length(VORP_REQUIRED)} contracted columns present, ",
             "{ncol(vorp_df)} total)"), "\n")
  } else {
    cat(glue("  MISSING {length(miss)} -- {paste(miss, collapse = ', ')}"), "\n")
    msg <- "compute_vorp_rankings() output is missing contracted columns."
    if (isTRUE(stop_on_fail)) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }
  invisible(miss)
}
