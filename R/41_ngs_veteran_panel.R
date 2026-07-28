# ==============================================================================
# 41_ngs_veteran_panel.R
# ==============================================================================
#
# PURPOSE
# -------
# Slot 2 NGS data layer. Turns the season-level Next Gen Stats panel from
# R/23 into a single trailing-window, volume-weighted efficiency read per
# player, keyed on GSIS. This is a SCORING-NEUTRAL DATA LAYER only: it emits
# the volume-weighted level, the backing window volume, and the qualifying
# season count for each candidate NGS metric. It applies NO floor and NO
# shrinkage. Those belong to the consumer (the Item 6.5 residual gate first,
# then R/31's veteran talent-z and R/32 Level 2), which already own the
# per-position volume floors and the empirical-Bayes machinery. Baking either
# in here would duplicate that machinery and pre-empt the gate.
#
# WHY VOLUME-WEIGHTED
# -------------------
# For efficiency metrics a high-volume season is a more trustworthy read than
# a thin one. Weighting each season by its own NGS volume (attempts / targets /
# rush attempts) down-weights small-sample seasons and injury-shortened years
# without a separate shrinkage pass. It is NOT recency-weighted: no season-to-
# season decay curve exists in the pipeline (compute_prior_weight is week-based,
# in-season only), and inventing one would add an unvalidated knob.
#
# WINDOW
# ------
# Trailing `window_seasons` COMPLETED seasons before `target_season`. For a
# 2026 draft that is 2023-2025. `window_seasons` defaults to 3L to mirror
# R/31::VETERAN_WINDOW_SEASONS, but callers inside the pipeline (R/31) should
# pass VETERAN_WINDOW_SEASONS explicitly so the window has a single source of
# truth at the call site.
#
# CANDIDATE METRICS (per position, for the 6.5 gate to choose among)
# ------------------------------------------------------------------
#   QB    : ngs_cpoe_vw            (completion pct above expectation)
#   RB    : ngs_ryoe_att_vw        (rush yards over expected per attempt)
#           ngs_rush_eff_vw        (NGS rushing efficiency; LOWER = more direct)
#   WR/TE : ngs_separation_vw      (avg separation)
#           ngs_yac_oe_vw          (YAC above expectation)
# The WR/TE separation and YAC pair is the intended fill for R/31's missing
# TE veteran signal (Item 6), pending the 6.5 gate's validation.
#
# NAVIGATION
# ----------
#   Line ~90  : Libraries, guarded source of R/23, constants
#   Line ~120 : Internal helpers (.wmean_ngs, .wvol_ngs, .last_by_season)
#   Line ~150 : build_ngs_veteran_panel()
#
# DEPENDENCIES
# ------------
#   R/23_aging_curves.R  -- load_ngs_season_panel()  (the NGS reader)
#   nflreadr (transitively, via R/23) : load_nextgen_stats()
#   dplyr, glue, here, readr
#
# DATA SOURCES
# ------------
#   nflreadr::load_nextgen_stats() via R/23 : NGS season summaries, 2016-2025.
#     week == 0 REG rows only (NGS pre-aggregated season totals).
#     Coverage is minimum-attempt gated: only qualifying (draftable-volume)
#     players appear. Rookies and any player without prior NFL snaps are absent
#     by construction, so this is a returning-veteran signal only.
#
# OUTPUT (one row per player_id)
# ------------------------------
#   player_id            chr   GSIS id (trimmed; joins to nfl_gsis_id)
#   ngs_position         chr   position at the latest qualifying season
#   ngs_display_name     chr   name at the latest qualifying season
#   ngs_cpoe_vw          dbl   VW CPOE            | ngs_cpoe_vol, ngs_cpoe_n
#   ngs_separation_vw    dbl   VW separation      | ngs_rec_vol,  ngs_rec_n
#   ngs_yac_oe_vw        dbl   VW YAC over exp     (shares rec vol / n)
#   ngs_ryoe_att_vw      dbl   VW RYOE per att    | ngs_rush_vol, ngs_rush_n
#   ngs_rush_eff_vw      dbl   VW rushing eff      (shares rush vol / n)
#   target_season        int   season the window was built for
#
#   attr(, "schema_tag")     "s2_w15_ngs_vet_panel_v1"
#   attr(, "window_seasons") the trailing window used
#
# VERSION
# -------
# 1.0  Initial build. Volume-weighted trailing-window NGS layer on R/23's
#      reader. Data layer only: no floor, no shrinkage (deferred to consumer).
# ==============================================================================


# ------------------------------------------------------------------------------
# LIBRARIES
# ------------------------------------------------------------------------------

library(dplyr)
library(glue)
library(here)
library(readr)
library(nflreadr)

source(here::here("R", "23_aging_curves.R"))

# load_ngs_season_panel() must arrive from R/23. Fail loudly rather than
# silently returning an empty panel if the source chain is refactored.
if (!exists("load_ngs_season_panel")) {
  stop(
    "build_ngs_veteran_panel() requires load_ngs_season_panel() from ",
    "R/23_aging_curves.R, which was not found after sourcing. Check the ",
    "source path."
  )
}

# NGS data availability floor. This is a hard data fact (NGS begins in 2016),
# not a tunable fallback.
NGS_MIN_SEASON <- 2016L


# ------------------------------------------------------------------------------
# INTERNAL HELPERS
# ------------------------------------------------------------------------------

# Volume-weighted mean over the trailing window, ignoring seasons where the
# metric or its weight is missing or the weight is non-positive.
.wmean_ngs <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  if (!any(keep)) return(NA_real_)
  sum(x[keep] * w[keep]) / sum(w[keep])
}

# Total window volume backing a metric (only seasons that actually contributed
# to the weighted mean).
.wvol_ngs <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  sum(w[keep])
}

# Value at the latest season with a non-missing entry (for position / name).
.last_by_season <- function(x, s) {
  ok <- !is.na(x)
  if (!any(ok)) return(NA)
  x[ok][which.max(s[ok])]
}


# ------------------------------------------------------------------------------
# FUNCTION: build_ngs_veteran_panel
# ------------------------------------------------------------------------------

#' Build the trailing-window, volume-weighted NGS veteran-efficiency panel.
#'
#' @param target_season Integer scalar. Projection season the window is built
#'   for (e.g. 2026). The window is the `window_seasons` completed seasons
#'   before it.
#' @param window_seasons Integer scalar. Trailing completed seasons to pool.
#'   Default 3L (mirrors R/31::VETERAN_WINDOW_SEASONS). Pipeline callers should
#'   pass VETERAN_WINDOW_SEASONS explicitly.
#' @param verbose Logical. Print progress. Default TRUE.
#' @param save_output Logical. Write the panel to `cache_dir`. Default FALSE.
#' @param cache_dir Character or NULL. Directory for the cached .rds. Defaults
#'   to here::here("data", "season2_cache") when save_output is TRUE.
#'
#' @return A tibble, one row per player_id, with the columns documented in the
#'   file header. Carries `schema_tag` and `window_seasons` attributes.
build_ngs_veteran_panel <- function(target_season,
                                    window_seasons = 3L,
                                    verbose        = TRUE,
                                    save_output    = FALSE,
                                    cache_dir      = NULL) {

  if (length(target_season) != 1L || is.na(target_season)) {
    stop("target_season must be a single non-missing integer.")
  }
  if (length(window_seasons) != 1L || is.na(window_seasons) || window_seasons < 1L) {
    stop("window_seasons must be a single integer >= 1.")
  }
  target_season  <- as.integer(target_season)
  window_seasons <- as.integer(window_seasons)

  # Trailing completed seasons before the target.
  window <- (target_season - window_seasons):(target_season - 1L)

  # Clip to NGS availability. Warn loudly about any dropped season; stop if
  # nothing usable remains (never silently proceed on an empty window).
  usable <- window[window >= NGS_MIN_SEASON]
  dropped <- setdiff(window, usable)
  if (length(dropped) > 0L) {
    warning(glue(
      "build_ngs_veteran_panel(): {length(dropped)} requested season(s) predate ",
      "NGS coverage ({NGS_MIN_SEASON}+) and were dropped: ",
      "{paste(sort(dropped), collapse = ', ')}."
    ))
  }
  if (length(usable) == 0L) {
    stop(glue(
      "build_ngs_veteran_panel(): no requested window season is within NGS ",
      "coverage ({NGS_MIN_SEASON}+). Window was ",
      "{min(window)}-{max(window)} for target {target_season}."
    ))
  }

  if (verbose) message(glue(
    "build_ngs_veteran_panel(): target {target_season}, window ",
    "{min(usable)}-{max(usable)} ({length(usable)} seasons)."
  ))

  # --- Pull the season-level NGS panel from R/23 ---
  raw <- load_ngs_season_panel(seasons = usable, verbose = verbose)

  if (is.null(raw) || nrow(raw) == 0L) {
    stop("build_ngs_veteran_panel(): load_ngs_season_panel() returned no rows.")
  }

  # Guard against the historical leading-space GSIS padding bug (R/19) so the
  # downstream join key is clean.
  raw <- raw %>% dplyr::mutate(player_id = trimws(player_id))

  # Any candidate metric column that a given NGS pull omits (e.g. an empty
  # stat type) is created as all-NA so the summarise below never errors.
  needed <- c("cpoe", "ngs_attempts",
              "avg_separation", "avg_yac_above_expectation", "ngs_targets",
              "rush_yards_over_expected_per_att", "ngs_efficiency",
              "ngs_rush_attempts",
              "player_position", "player_display_name")
  for (col in needed) {
    if (!col %in% names(raw)) raw[[col]] <- NA_real_
  }

  # --- Collapse the window to one volume-weighted read per player ---
  panel <- raw %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(
      ngs_position      = .last_by_season(player_position, season),
      ngs_display_name  = .last_by_season(player_display_name, season),

      # QB passing accuracy
      ngs_cpoe_vw       = .wmean_ngs(cpoe, ngs_attempts),
      ngs_cpoe_vol      = .wvol_ngs(cpoe, ngs_attempts),
      ngs_cpoe_n        = sum(!is.na(cpoe)),

      # WR/TE receiving (separation + YAC share the target-volume backing)
      ngs_separation_vw = .wmean_ngs(avg_separation, ngs_targets),
      ngs_yac_oe_vw     = .wmean_ngs(avg_yac_above_expectation, ngs_targets),
      ngs_rec_vol       = .wvol_ngs(avg_separation, ngs_targets),
      ngs_rec_n         = sum(!is.na(avg_separation)),

      # RB rushing (RYOE + efficiency share the rush-attempt backing)
      ngs_ryoe_att_vw   = .wmean_ngs(rush_yards_over_expected_per_att, ngs_rush_attempts),
      ngs_rush_eff_vw   = .wmean_ngs(ngs_efficiency, ngs_rush_attempts),
      ngs_rush_vol      = .wvol_ngs(rush_yards_over_expected_per_att, ngs_rush_attempts),
      ngs_rush_n        = sum(!is.na(rush_yards_over_expected_per_att)),

      .groups = "drop"
    ) %>%
    dplyr::mutate(target_season = target_season) %>%
    dplyr::arrange(ngs_position, dplyr::desc(ngs_cpoe_vol + ngs_rec_vol + ngs_rush_vol))

  attr(panel, "schema_tag")     <- "s2_w15_ngs_vet_panel_v1"
  attr(panel, "window_seasons") <- length(usable)

  if (verbose) {
    pos_tbl <- panel %>%
      dplyr::count(ngs_position, name = "n") %>%
      dplyr::arrange(dplyr::desc(n))
    message(glue(
      "build_ngs_veteran_panel(): {nrow(panel)} players | ",
      "cpoe {sum(!is.na(panel$ngs_cpoe_vw))} | ",
      "separation {sum(!is.na(panel$ngs_separation_vw))} | ",
      "ryoe {sum(!is.na(panel$ngs_ryoe_att_vw))}"
    ))
    message(paste(
      apply(pos_tbl, 1L, function(r) glue("  {r[['ngs_position']]}: {r[['n']]}")),
      collapse = "\n"
    ))
  }

  # --- Optional cache write ---
  if (isTRUE(save_output)) {
    if (is.null(cache_dir)) cache_dir <- here::here("data", "season2_cache")
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    out_path <- file.path(cache_dir, "s2_week15_ngs_veteran_panel.rds")
    saveRDS(panel, out_path)
    if (verbose) message(glue("build_ngs_veteran_panel(): cached -> {out_path}"))
  }

  panel
}
