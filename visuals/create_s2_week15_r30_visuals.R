# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 15
# Visualization Script: R/30 Team Volume Projections
# File: examples/create_s2_week15_r30_visuals.R
#
# PURPOSE
# -------
# Three publication-quality visualizations derived from R/30 output:
#
#   V1: Projected pass vs rush volume scatter (all 32 teams, quadrant view)
#   V2: QB quality index ranking (horizontal lollipop, all 32 teams)
#   V3: Coach change blend impact -- historical vs blended pass volume
#       (dumbbell chart, coach-change teams only; skipped gracefully if none)
#
# DEPENDENCIES
# ------------
#   data/season2_cache/s2_week15_team_volumes.csv  (R/30 output)
#
# OUTPUTS (all to output/plots/)
# --------------------------------
#   s2_week15_r30_pass_rush_quadrant.png
#   s2_week15_r30_qb_quality_ranking.png
#   s2_week15_r30_coach_blend_impact.png  (skipped if no coach changes)
#
# SCHEMA TAG: s2_w15_team_vol_v1
# ==============================================================================

# ==============================================================================
# LIBRARIES
# ==============================================================================

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ggrepel)
library(scales)
library(glue)
library(here)
library(readr)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

SEASON      <- 2026L
WEEK        <- 15L
OUTPUT_DIR  <- here::here("output", "plots")
DATA_DIR    <- here::here("data", "season2_cache")
PLOT_DPI    <- 300L
FILE_PREFIX <- "s2_week15_r30"

# V2: lollipop color scheme
COLOR_ABOVE_AVG <- "#2A9D8F"   # positive QB quality
COLOR_BELOW_AVG <- "#E63946"   # negative QB quality

# V3: dumbbell colors
COLOR_HISTORICAL <- "gray45"
COLOR_BLENDED    <- "#457B9D"

# ==============================================================================
# SETUP
# ==============================================================================

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  cat(glue("Created output directory: {OUTPUT_DIR}\n\n"))
}

cat(paste0(strrep("=", 70), "\n"))
cat(glue("R/30 TEAM VOLUME PROJECTIONS -- VISUALIZATION SCRIPT\n"))
cat(glue("Season {SEASON} | Week {WEEK}\n"))
cat(paste0(strrep("=", 70), "\n\n"))

# ==============================================================================
# DATA LOAD
# ==============================================================================

team_vol_path <- file.path(DATA_DIR, "s2_week15_team_volumes.csv")

if (!file.exists(team_vol_path)) {
  stop(glue(
    "Required input not found: {team_vol_path}\n",
    "Run R/30_team_volume_projections.R first to generate this file."
  ))
}

tv <- readr::read_csv(team_vol_path, show_col_types = FALSE)

cat(glue("Loaded: s2_week15_team_volumes.csv ({nrow(tv)} rows)\n\n"))

# Validate required columns
required_cols <- c(
  "team", "coach_change_flag", "prior_hc_team",
  "historical_pass_pg", "historical_rush_pg",
  "historical_proe", "blended_pass_pg", "blended_rush_pg",
  "blended_proe", "qb_quality_score",
  "projected_pass_pg", "projected_rush_pg", "projected_plays_pg",
  "projected_pass_tds_pg", "projected_rush_tds_pg"
)

missing_cols <- setdiff(required_cols, names(tv))
if (length(missing_cols) > 0) {
  stop(glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
}

cat(glue("Column validation: all {length(required_cols)} required columns present\n"))

n_teams        <- nrow(tv)
n_coach_change <- sum(tv$coach_change_flag, na.rm = TRUE)

cat(glue("Teams: {n_teams} | Coach changes: {n_coach_change}\n\n"))

# Pre-compute league averages used across plots
league_avg_pass <- mean(tv$projected_pass_pg, na.rm = TRUE)
league_avg_rush <- mean(tv$projected_rush_pg, na.rm = TRUE)

# ==============================================================================
# VISUALIZATION 1: Projected Pass vs Rush Volume -- 32-Team Quadrant View
# ==============================================================================
# Each point is one team. Reference lines at league average divide the chart
# into four quadrants: pass-heavy, balanced, run-heavy, high-pace outliers.
# Coach-change teams are flagged with a triangle to show where new schemes land.

cat("Building V1: Pass vs rush volume quadrant scatter...\n")

v1_data <- tv %>%
  dplyr::mutate(
    team_shape = if_else(coach_change_flag, "Coach change", "No change")
  )

# Quadrant labels -- placed in corners at 90% of axis range
pass_lo <- min(tv$projected_pass_pg, na.rm = TRUE)
pass_hi <- max(tv$projected_pass_pg, na.rm = TRUE)
rush_lo <- min(tv$projected_rush_pg, na.rm = TRUE)
rush_hi <- max(tv$projected_rush_pg, na.rm = TRUE)

# Add a small margin for annotation placement
pass_margin <- (pass_hi - pass_lo) * 0.06
rush_margin <- (rush_hi - rush_lo) * 0.06

quadrant_labels <- tibble::tibble(
  x     = c(pass_lo + pass_margin, pass_hi - pass_margin,
            pass_lo + pass_margin, pass_hi - pass_margin),
  y     = c(rush_hi - rush_margin, rush_hi - rush_margin,
            rush_lo + rush_margin, rush_lo + rush_margin),
  label = c("Run-Heavy /\nPass-Light",
            "High Pace\n(Pass + Run)",
            "Low Pace /\nRun-Focused",
            "Pass-Heavy /\nRun-Light"),
  hjust = c(0, 1, 0, 1)
)

p1 <- ggplot2::ggplot(
  v1_data,
  ggplot2::aes(
    x     = projected_pass_pg,
    y     = projected_rush_pg,
    shape = team_shape,
    color = team_shape
  )
) +
  ggplot2::geom_vline(
    xintercept = league_avg_pass,
    linetype   = "dashed",
    color      = "gray60",
    linewidth  = 0.7
  ) +
  ggplot2::geom_hline(
    yintercept = league_avg_rush,
    linetype   = "dashed",
    color      = "gray60",
    linewidth  = 0.7
  ) +
  ggplot2::annotate(
    "text",
    x      = league_avg_pass + 0.2,
    y      = rush_hi,
    label  = glue("League avg\n{round(league_avg_pass, 1)} att/gm"),
    hjust  = 0,
    size   = 2.8,
    color  = "gray50"
  ) +
  ggplot2::annotate(
    "text",
    x      = pass_hi,
    y      = league_avg_rush + 0.3,
    label  = glue("League avg\n{round(league_avg_rush, 1)} att/gm"),
    hjust  = 1,
    size   = 2.8,
    color  = "gray50"
  ) +
  ggplot2::geom_text(
    data    = quadrant_labels,
    mapping = ggplot2::aes(x = x, y = y, label = label, hjust = hjust),
    inherit.aes = FALSE,
    size    = 2.8,
    color   = "gray75",
    fontface = "italic",
    lineheight = 0.9
  ) +
  ggplot2::geom_point(size = 3.5, alpha = 0.85) +
  ggrepel::geom_text_repel(
    ggplot2::aes(label = team),
    size          = 2.8,
    max.overlaps  = 32,
    segment.color = "gray70",
    segment.size  = 0.3,
    min.segment.length = 0.15,
    box.padding   = 0.3
  ) +
  ggplot2::scale_shape_manual(
    name   = NULL,
    values = c("Coach change" = 17L, "No change" = 16L)
  ) +
  ggplot2::scale_color_manual(
    name   = NULL,
    values = c("Coach change" = "#E9C46A", "No change" = "#457B9D")
  ) +
  ggplot2::scale_x_continuous(
    name   = glue(
      "Projected Pass Attempts per Game (Season {SEASON})"
    ),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  ggplot2::scale_y_continuous(
    name   = glue(
      "Projected Rush Attempts per Game (Season {SEASON})"
    ),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  ggplot2::labs(
    title    = glue(
      "Every Team's Projected Pass/Rush Budget for Season {SEASON}"
    ),
    subtitle = glue(
      "Reference lines at league average | ",
      "Triangles = coach change teams ({n_coach_change} teams) | ",
      "{n_teams} teams total"
    ),
    caption  = glue(
      "NFL Analytics Toolkit | Season {SEASON} Week {WEEK} | ",
      "R/30 Team Volume Projections\n",
      "Pass avg: {round(league_avg_pass, 1)} att/gm | ",
      "Rush avg: {round(league_avg_rush, 1)} att/gm | ",
      "Blended: 70% team history + 30% coach prior (where applicable)"
    )
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", size = 13),
    plot.subtitle    = ggplot2::element_text(size = 9, color = "#555555"),
    plot.caption     = ggplot2::element_text(size = 8, color = "#888888"),
    legend.position  = "bottom",
    panel.grid.minor = ggplot2::element_blank()
  )

v1_path <- file.path(OUTPUT_DIR, glue("{FILE_PREFIX}_pass_rush_quadrant.png"))
ggplot2::ggsave(v1_path, p1, width = 11, height = 9, dpi = PLOT_DPI)
cat(glue("  Saved: {FILE_PREFIX}_pass_rush_quadrant.png\n\n"))

# ==============================================================================
# VISUALIZATION 2: QB Quality Index Ranking -- All 32 Teams
# ==============================================================================
# Horizontal lollipop sorted by qb_quality_score. Center line at 0 (league
# average). Captures how QB talent moves the team's projected volume and
# efficiency -- the key adjustment layer unique to R/30.

cat("Building V2: QB quality index ranking...\n")

if (requireNamespace("forcats", quietly = TRUE)) {
  v2_data <- tv %>%
    dplyr::mutate(
      qb_direction = if_else(qb_quality_score >= 0, "Above average", "Below average"),
      team         = forcats::fct_reorder(team, qb_quality_score)
    )
} else {
  v2_data <- tv %>%
    dplyr::mutate(
      qb_direction = if_else(qb_quality_score >= 0, "Above average", "Below average")
    ) %>%
    dplyr::arrange(qb_quality_score) %>%
    dplyr::mutate(team = factor(team, levels = unique(team)))
}

top_qb_team    <- tv$team[which.max(tv$qb_quality_score)]
bottom_qb_team <- tv$team[which.min(tv$qb_quality_score)]
top_qb_score   <- round(max(tv$qb_quality_score, na.rm = TRUE), 2)
bottom_qb_score <- round(min(tv$qb_quality_score, na.rm = TRUE), 2)

p2 <- ggplot2::ggplot(
  v2_data,
  ggplot2::aes(x = qb_quality_score, y = team, color = qb_direction)
) +
  ggplot2::geom_vline(
    xintercept = 0,
    color      = "gray55",
    linewidth  = 0.8,
    linetype   = "solid"
  ) +
  ggplot2::geom_segment(
    ggplot2::aes(x = 0, xend = qb_quality_score, y = team, yend = team),
    linewidth = 0.8,
    alpha     = 0.7
  ) +
  ggplot2::geom_point(size = 3.2) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = round(qb_quality_score, 2),
      hjust = if_else(qb_quality_score >= 0, -0.35, 1.35)
    ),
    size  = 2.6,
    color = "gray35"
  ) +
  ggplot2::scale_color_manual(
    name   = "QB quality",
    values = c("Above average" = COLOR_ABOVE_AVG, "Below average" = COLOR_BELOW_AVG)
  ) +
  ggplot2::scale_x_continuous(
    name   = "QB Quality Score (z-score: CPOE + EPA/dropback, equal weight)",
    breaks = scales::pretty_breaks(n = 8)
  ) +
  ggplot2::labs(
    title    = glue(
      "QB Quality Index: Which Teams Have the Pass-Volume Edge in Season {SEASON}?"
    ),
    subtitle = glue(
      "Composite z-score of CPOE and EPA/dropback | ",
      "Positive = above-average QB, adjusts team pass volume upward\n",
      "Highest: {top_qb_team} ({top_qb_score}) | ",
      "Lowest: {bottom_qb_team} ({bottom_qb_score})"
    ),
    y        = NULL,
    caption  = glue(
      "NFL Analytics Toolkit | Season {SEASON} Week {WEEK} | ",
      "R/30 Team Volume Projections\n",
      "Min {200L} dropbacks per season required | ",
      "Teams with no qualifying QB default to 0 (league average)"
    )
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", size = 12),
    plot.subtitle    = ggplot2::element_text(size = 9, color = "#555555"),
    plot.caption     = ggplot2::element_text(size = 8, color = "#888888"),
    legend.position  = "bottom",
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.y      = ggplot2::element_text(size = 9),
    plot.margin      = ggplot2::margin(t = 8, r = 30, b = 8, l = 8)
  )

v2_path <- file.path(OUTPUT_DIR, glue("{FILE_PREFIX}_qb_quality_ranking.png"))
ggplot2::ggsave(v2_path, p2, width = 10, height = 11, dpi = PLOT_DPI)
cat(glue("  Saved: {FILE_PREFIX}_qb_quality_ranking.png\n\n"))

# ==============================================================================
# VISUALIZATION 3: Coach Change Blend Impact
# ==============================================================================
# Dumbbell chart for coach-change teams only. Left dot = historical_pass_pg
# (team's own 3-year pattern). Right dot = blended_pass_pg (after 70/30 mix
# with the new HC's prior scheme). Labels show which prior team influenced
# the blend. Skips gracefully if n_coach_change == 0.

cat("Building V3: Coach change blend impact...\n")

if (n_coach_change == 0L) {

  cat(glue(
    "  No coach changes detected in s2_week15_team_volumes.csv\n",
    "  Skipping V3 -- {FILE_PREFIX}_coach_blend_impact.png not written\n\n"
  ))

} else {

  v3_data <- tv %>%
    dplyr::filter(coach_change_flag) %>%
    dplyr::mutate(
      pass_delta    = blended_pass_pg - historical_pass_pg,
      blend_dir     = if_else(pass_delta >= 0, "New scheme: more pass", "New scheme: less pass"),
      prior_label   = if_else(
        is.na(prior_hc_team),
        "(prior team unknown)",
        glue("from {prior_hc_team}")
      )
    ) %>%
    dplyr::arrange(pass_delta) %>%
    dplyr::mutate(
      team = factor(team, levels = unique(team))
    )

  cat(glue(
    "  Coach-change teams: {nrow(v3_data)} | ",
    "More pass after blend: {sum(v3_data$pass_delta >= 0)} | ",
    "Less pass: {sum(v3_data$pass_delta < 0)}\n"
  ))

  # Compute x range for annotation positioning
  x_lo <- min(c(v3_data$historical_pass_pg, v3_data$blended_pass_pg), na.rm = TRUE)
  x_hi <- max(c(v3_data$historical_pass_pg, v3_data$blended_pass_pg), na.rm = TRUE)
  x_margin <- (x_hi - x_lo) * 0.04

  p3 <- ggplot2::ggplot(v3_data) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x     = historical_pass_pg,
        xend  = blended_pass_pg,
        y     = team,
        yend  = team,
        color = blend_dir
      ),
      linewidth = 1.2,
      arrow = ggplot2::arrow(
        length = ggplot2::unit(0.10, "inches"),
        type   = "closed"
      )
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = historical_pass_pg, y = team),
      color = COLOR_HISTORICAL,
      size  = 3.2,
      shape = 16
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = blended_pass_pg, y = team, color = blend_dir),
      size  = 3.2,
      shape = 16
    ) +
    # Prior team label on the right margin
    ggplot2::geom_text(
      ggplot2::aes(
        x     = x_hi + x_margin,
        y     = team,
        label = prior_label
      ),
      hjust = 0,
      size  = 2.8,
      color = "gray45"
    ) +
    ggplot2::scale_color_manual(
      name   = "Blend direction",
      values = c(
        "New scheme: more pass" = COLOR_ABOVE_AVG,
        "New scheme: less pass" = COLOR_BELOW_AVG
      )
    ) +
    ggplot2::scale_x_continuous(
      name   = "Projected Pass Attempts per Game",
      breaks = scales::pretty_breaks(n = 6)
    ) +
    ggplot2::coord_cartesian(
      xlim = c(x_lo - x_margin, x_hi + x_margin * 14)
    ) +
    ggplot2::labs(
      title    = glue(
        "How Much Did a New Coach Move the Needle? ",
        "({n_coach_change} Teams with Season {SEASON} HC Changes)"
      ),
      subtitle = paste0(
        "Gray dot = team's own 3-year historical pass rate | ",
        "Arrow endpoint = 70/30 blended projection\n",
        "Right label = prior team whose scheme was blended in"
      ),
      y        = NULL,
      caption  = glue(
        "NFL Analytics Toolkit | Season {SEASON} Week {WEEK} | ",
        "R/30 Team Volume Projections\n",
        "Blend: 70% team history + 30% new HC prior team pattern"
      )
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = 12),
      plot.subtitle    = ggplot2::element_text(size = 9, color = "#555555"),
      plot.caption     = ggplot2::element_text(size = 8, color = "#888888"),
      legend.position  = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y      = ggplot2::element_text(size = 10),
      plot.margin      = ggplot2::margin(t = 8, r = 80, b = 8, l = 8)
    )

  v3_path <- file.path(OUTPUT_DIR, glue("{FILE_PREFIX}_coach_blend_impact.png"))
  ggplot2::ggsave(v3_path, p3,
                  width  = 10,
                  height = max(5, nrow(v3_data) * 0.45 + 3),
                  dpi    = PLOT_DPI)
  cat(glue("  Saved: {FILE_PREFIX}_coach_blend_impact.png\n\n"))
}

# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

v3_note <- if (n_coach_change == 0L) {
  " (skipped -- no coach changes in data)"
} else {
  ""
}

cat(paste0(strrep("=", 70), "\n"))
cat("R/30 VISUALIZATION SUMMARY\n")
cat(paste0(strrep("=", 70), "\n"))
cat(glue("  Season: {SEASON} | Week: {WEEK}\n"))
cat(glue("  Teams in output:         {n_teams}\n"))
cat(glue("  Coach changes detected:  {n_coach_change}\n"))
cat(glue(
  "  League avg pass/gm:      {round(league_avg_pass, 1)}\n"
))
cat(glue(
  "  League avg rush/gm:      {round(league_avg_rush, 1)}\n"
))
cat(glue(
  "  Highest QB quality:      {top_qb_team} (z = {top_qb_score})\n"
))
cat(glue(
  "  Lowest QB quality:       {bottom_qb_team} (z = {bottom_qb_score})\n"
))
cat("\n  Plots generated:\n")
cat(glue("  1. {FILE_PREFIX}_pass_rush_quadrant.png\n"))
cat(glue("  2. {FILE_PREFIX}_qb_quality_ranking.png\n"))
cat(glue("  3. {FILE_PREFIX}_coach_blend_impact.png{v3_note}\n"))
cat(glue("  Output: output/plots/\n"))
cat(glue("  Resolution: {PLOT_DPI} dpi\n"))
cat(paste0(strrep("=", 70), "\n"))

# ==============================================================================
# END OF FILE
# ==============================================================================
