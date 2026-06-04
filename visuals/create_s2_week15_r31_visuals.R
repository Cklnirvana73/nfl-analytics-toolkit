# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 15
# Visualization Script: R/31 Player Volume Allocation
# File: examples/create_s2_week15_r31_visuals.R
#
# PURPOSE
# -------
# Three publication-quality visualizations derived from R/31 output:
#
#   V1: Expected volume leaders by position (top 10 per position)
#       WR/TE/QB = expected_targets_pg | RB = expected_carries_pg
#
#   V2: Share adjustment chain -- depth prior vs final allocated share
#       (dumbbell, top 40 by expected_targets_pg)
#
#   V3: Rookie capital allocation -- all rookies by expected targets,
#       colored by draft round
#
# DEPENDENCIES
# ------------
#   data/season2_cache/s2_week15_player_volume_allocation.csv  (R/31 output)
#
# OUTPUTS (all to output/plots/)
# --------------------------------
#   s2_week15_r31_volume_leaders.png
#   s2_week15_r31_share_adjustment_chain.png
#   s2_week15_r31_rookie_capital_allocation.png
#
# SCHEMA TAG: s2_w15_player_alloc_v1
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
FILE_PREFIX <- "s2_week15_r31"

# Top N per position for V1
TOP_N_PER_POSITION <- 10L

# Top N for V2 dumbbell (by expected_targets_pg)
TOP_N_DUMBBELL <- 40L

# Consistent position colors
POSITION_ORDER  <- c("QB", "RB", "WR", "TE")
POSITION_COLORS <- c(QB = "#E63946", RB = "#457B9D", WR = "#2A9D8F", TE = "#E9C46A")

# Draft round color palette for V3 (R1 darkest, UDFA lightest)
ROUND_COLORS <- c(
  "R1"   = "#1D3557",
  "R2"   = "#457B9D",
  "R3"   = "#2A9D8F",
  "R4"   = "#E9C46A",
  "R5"   = "#F4A261",
  "R6"   = "#E76F51",
  "R7"   = "#C77DFF",
  "UDFA" = "#AAAAAA"
)

# ==============================================================================
# SETUP
# ==============================================================================

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  cat(glue("Created output directory: {OUTPUT_DIR}\n\n"))
}

cat(paste0(strrep("=", 70), "\n"))
cat(glue("R/31 PLAYER VOLUME ALLOCATION -- VISUALIZATION SCRIPT\n"))
cat(glue("Season {SEASON} | Week {WEEK}\n"))
cat(paste0(strrep("=", 70), "\n\n"))

# ==============================================================================
# DATA LOAD
# ==============================================================================

alloc_path <- file.path(DATA_DIR, "s2_week15_player_volume_allocation.csv")

if (!file.exists(alloc_path)) {
  stop(glue(
    "Required input not found: {alloc_path}\n",
    "Run R/31_player_volume_allocation.R first to generate this file."
  ))
}

alloc <- readr::read_csv(alloc_path, show_col_types = FALSE)

cat(glue(
  "Loaded: s2_week15_player_volume_allocation.csv ",
  "({format(nrow(alloc), big.mark = ',')} rows)\n\n"
))

# Validate required columns
required_cols <- c(
  "nfl_gsis_id", "player_name", "team", "position",
  "depth_position", "depth_rank",
  "is_rookie", "draft_round", "rookie_capital_mult",
  "score_final", "talent_z", "talent_multiplier",
  "target_share_base", "target_share_adjusted", "target_share",
  "rush_share_base", "rush_share_adjusted", "rush_share",
  "projected_team_pass_pg", "projected_team_rush_pg",
  "expected_targets_pg", "expected_carries_pg"
)

missing_cols <- setdiff(required_cols, names(alloc))
if (length(missing_cols) > 0) {
  stop(glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
}

cat(glue("Column validation: all {length(required_cols)} required columns present\n"))

n_players <- nrow(alloc)
n_rookies <- sum(alloc$is_rookie, na.rm = TRUE)
n_teams   <- dplyr::n_distinct(alloc$team)

cat(glue(
  "Players: {n_players} | Rookies: {n_rookies} | Teams: {n_teams}\n\n"
))

# Factor position with consistent ordering
alloc <- alloc %>%
  dplyr::mutate(
    position = factor(position, levels = POSITION_ORDER)
  )

# ==============================================================================
# VISUALIZATION 1: Expected Volume Leaders by Position
# ==============================================================================
# Top TOP_N_PER_POSITION players per position by primary volume metric.
# WR/TE/QB use expected_targets_pg; RB uses expected_carries_pg.
# Tells the headline fantasy story: who does R/31 think actually gets the ball?

cat("Building V1: Expected volume leaders by position...\n")

v1_data <- alloc %>%
  dplyr::mutate(
    primary_volume = dplyr::if_else(
      position == "RB",
      expected_carries_pg,
      expected_targets_pg
    ),
    volume_label = dplyr::if_else(
      position == "RB",
      "Expected Carries per Game",
      "Expected Targets per Game"
    )
  ) %>%
  dplyr::filter(!is.na(primary_volume), primary_volume > 0) %>%
  dplyr::group_by(position) %>%
  dplyr::slice_max(order_by = primary_volume, n = TOP_N_PER_POSITION) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    player_label = glue("{player_name} ({team})")
  )

# Sort within each position: worst to best so ggplot bottom-to-top puts best at top
v1_data <- v1_data %>%
  dplyr::arrange(position, primary_volume) %>%
  dplyr::mutate(
    player_label = factor(player_label, levels = unique(player_label))
  )

# Facet label: include metric name per position
position_metric_labels <- c(
  QB = glue("QB\n(Expected Targets/Gm)"),
  RB = glue("RB\n(Expected Carries/Gm)"),
  WR = glue("WR\n(Expected Targets/Gm)"),
  TE = glue("TE\n(Expected Targets/Gm)")
)

v1_data <- v1_data %>%
  dplyr::mutate(
    facet_label = dplyr::recode(
      as.character(position),
      QB = "QB  (targets/gm)",
      RB = "RB  (carries/gm)",
      WR = "WR  (targets/gm)",
      TE = "TE  (targets/gm)"
    ),
    facet_label = factor(
      facet_label,
      levels = c(
        "QB  (targets/gm)",
        "RB  (carries/gm)",
        "WR  (targets/gm)",
        "TE  (targets/gm)"
      )
    )
  )

p1 <- ggplot2::ggplot(
  v1_data,
  ggplot2::aes(x = primary_volume, y = player_label, fill = position)
) +
  ggplot2::geom_col(alpha = 0.85, width = 0.7) +
  ggplot2::geom_text(
    ggplot2::aes(label = round(primary_volume, 1)),
    hjust  = -0.2,
    size   = 2.8,
    color  = "gray25"
  ) +
  ggplot2::scale_fill_manual(values = POSITION_COLORS) +
  ggplot2::scale_x_continuous(
    name   = "Volume per Game (targets or carries depending on position)",
    expand = ggplot2::expansion(mult = c(0, 0.15))
  ) +
  ggplot2::facet_wrap(
    ~ facet_label,
    scales = "free",
    nrow   = 2L
  ) +
  ggplot2::labs(
    title    = glue(
      "R/31 Volume Leaders: Who Gets the Ball in Season {SEASON}?"
    ),
    subtitle = glue(
      "Top {TOP_N_PER_POSITION} per position | ",
      "WR/TE/QB = expected targets/gm | RB = expected carries/gm\n",
      "Allocated against team budget from R/30 | Talent + depth + rookie capital combined"
    ),
    y        = NULL,
    fill     = NULL,
    caption  = glue(
      "NFL Analytics Toolkit | Season {SEASON} Week {WEEK} | ",
      "R/31 Player Volume Allocation\n",
      "Volume = team projected att/gm x final allocated share | ",
      "Soft constraint: shares rescaled if team total outside [0.95, 1.05]"
    )
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", size = 13),
    plot.subtitle    = ggplot2::element_text(size = 9, color = "#555555"),
    plot.caption     = ggplot2::element_text(size = 8, color = "#888888"),
    legend.position  = "none",
    strip.text       = ggplot2::element_text(face = "bold", size = 10),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    axis.text.y      = ggplot2::element_text(size = 8)
  )

v1_path <- file.path(OUTPUT_DIR, glue("{FILE_PREFIX}_volume_leaders.png"))
ggplot2::ggsave(v1_path, p1, width = 14, height = 12, dpi = PLOT_DPI)
cat(glue("  Saved: {FILE_PREFIX}_volume_leaders.png\n\n"))

# ==============================================================================
# VISUALIZATION 2: Share Adjustment Chain -- Depth Prior vs Final Share
# ==============================================================================
# Dumbbell for top TOP_N_DUMBBELL by expected_targets_pg.
# Left dot = target_share_base (pure depth-position prior).
# Right dot = target_share (after rookie capital + talent multiplier + constraint).
# The gap is the actual work R/31 is doing beyond a naive depth chart read.

cat("Building V2: Share adjustment chain dumbbell...\n")

v2_data <- alloc %>%
  dplyr::filter(!is.na(target_share_base), !is.na(target_share)) %>%
  dplyr::slice_max(order_by = expected_targets_pg, n = TOP_N_DUMBBELL) %>%
  dplyr::mutate(
    adj_delta     = target_share - target_share_base,
    adj_direction = dplyr::if_else(adj_delta >= 0, "Boosted", "Reduced"),
    player_label  = glue("{player_name} ({position}, {team})")
  ) %>%
  dplyr::arrange(expected_targets_pg) %>%
  dplyr::mutate(
    player_label = factor(player_label, levels = unique(player_label))
  )

n_boosted <- sum(v2_data$adj_direction == "Boosted")
n_reduced <- sum(v2_data$adj_direction == "Reduced")

cat(glue(
  "  Dumbbell players: {nrow(v2_data)} | ",
  "Boosted vs depth: {n_boosted} | Reduced: {n_reduced}\n"
))

p2 <- ggplot2::ggplot(v2_data) +
  ggplot2::geom_segment(
    ggplot2::aes(
      x     = target_share_base,
      xend  = target_share,
      y     = player_label,
      yend  = player_label,
      color = adj_direction
    ),
    linewidth = 0.9,
    alpha     = 0.75
  ) +
  ggplot2::geom_point(
    ggplot2::aes(x = target_share_base, y = player_label),
    color = "gray50",
    size  = 2.2,
    shape = 16
  ) +
  ggplot2::geom_point(
    ggplot2::aes(x = target_share, y = player_label, color = adj_direction),
    size  = 2.2,
    shape = 16
  ) +
  ggplot2::scale_color_manual(
    name   = "Adjustment direction",
    values = c(Boosted = "#2A9D8F", Reduced = "#E63946")
  ) +
  ggplot2::scale_x_continuous(
    name   = "Target Share (proportion of team pass attempts)",
    labels = scales::percent_format(accuracy = 0.1),
    breaks = scales::pretty_breaks(n = 7)
  ) +
  ggplot2::labs(
    title    = glue(
      "How Much Did Talent + Draft Capital Move Each Player? ",
      "(Top {TOP_N_DUMBBELL} by Projected Targets)"
    ),
    subtitle = paste0(
      "Gray dot = depth-position prior (target_share_base) | ",
      "Colored dot = final allocated share (target_share)\n",
      glue(
        "Boosted: {n_boosted} | Reduced: {n_reduced} | ",
        "Gap = R/28 talent score + rookie capital effect"
      )
    ),
    y        = NULL,
    caption  = glue(
      "NFL Analytics Toolkit | Season {SEASON} Week {WEEK} | ",
      "R/31 Player Volume Allocation\n",
      "Talent multiplier = 1 + 0.10 * z-score(score_final), capped [0.70, 1.30] | ",
      "Rookie capital multipliers: R1=1.15x down to UDFA=0.45x"
    )
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", size = 12),
    plot.subtitle    = ggplot2::element_text(size = 9, color = "#555555"),
    plot.caption     = ggplot2::element_text(size = 8, color = "#888888"),
    legend.position  = "bottom",
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.y      = ggplot2::element_text(size = 8),
    plot.margin      = ggplot2::margin(t = 8, r = 15, b = 8, l = 8)
  )

v2_path <- file.path(OUTPUT_DIR, glue("{FILE_PREFIX}_share_adjustment_chain.png"))
ggplot2::ggsave(v2_path, p2, width = 11, height = 14, dpi = PLOT_DPI)
cat(glue("  Saved: {FILE_PREFIX}_share_adjustment_chain.png\n\n"))

# ==============================================================================
# VISUALIZATION 3: Rookie Capital Allocation
# ==============================================================================
# All rookies, sorted by expected_targets_pg descending.
# Colored by draft round -- tests whether capital multiplier translates
# to actual volume. UDFA rookies (NA draft_round) shown as a separate tier.

cat("Building V3: Rookie capital allocation...\n")

v3_data <- alloc %>%
  dplyr::filter(is_rookie) %>%
  dplyr::mutate(
    round_label = dplyr::case_when(
      is.na(draft_round)     ~ "UDFA",
      draft_round == 1L      ~ "R1",
      draft_round == 2L      ~ "R2",
      draft_round == 3L      ~ "R3",
      draft_round == 4L      ~ "R4",
      draft_round == 5L      ~ "R5",
      draft_round == 6L      ~ "R6",
      draft_round >= 7L      ~ "R7",
      TRUE                   ~ "UDFA"
    ),
    round_label  = factor(
      round_label,
      levels = c("R1", "R2", "R3", "R4", "R5", "R6", "R7", "UDFA")
    ),
    player_label = glue("{player_name} ({position}, {team})")
  ) %>%
  dplyr::filter(!is.na(expected_targets_pg) | !is.na(expected_carries_pg)) %>%
  dplyr::mutate(
    primary_volume = dplyr::if_else(
      position == "RB",
      expected_carries_pg,
      expected_targets_pg
    )
  ) %>%
  dplyr::filter(!is.na(primary_volume), primary_volume > 0) %>%
  dplyr::arrange(dplyr::desc(primary_volume)) %>%
  dplyr::mutate(
    player_label = factor(player_label, levels = rev(unique(player_label)))
  )

cat(glue("  Rookie pool: {nrow(v3_data)} players with volume > 0\n"))

if (nrow(v3_data) == 0L) {

  cat("  No rookies with positive volume found -- skipping V3\n\n")

} else {

  # Round counts for subtitle
  round_counts <- v3_data %>%
    dplyr::count(round_label, .drop = FALSE) %>%
    dplyr::filter(n > 0) %>%
    dplyr::summarise(
      txt = paste(glue("{round_label}: {n}"), collapse = " | ")
    ) %>%
    dplyr::pull(txt)

  p3 <- ggplot2::ggplot(
    v3_data,
    ggplot2::aes(x = primary_volume, y = player_label, color = round_label)
  ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = primary_volume, y = player_label, yend = player_label),
      linewidth = 0.8,
      alpha     = 0.6
    ) +
    ggplot2::geom_point(size = 3.0) +
    ggplot2::scale_color_manual(
      name   = "Draft round",
      values = ROUND_COLORS,
      drop   = FALSE
    ) +
    ggplot2::scale_x_continuous(
      name   = "Volume per Game (targets for WR/TE/QB, carries for RB)",
      expand = ggplot2::expansion(mult = c(0, 0.08)),
      breaks = scales::pretty_breaks(n = 6)
    ) +
    ggplot2::labs(
      title    = glue(
        "Rookie Volume Allocation: Does Draft Capital Translate ",
        "to Projected Opportunity?"
      ),
      subtitle = glue(
        "{nrow(v3_data)} rookies with projected volume > 0 | ",
        "{round_counts}\n",
        "Sorted by primary volume (targets for WR/TE/QB, carries for RB)"
      ),
      y        = NULL,
      caption  = glue(
        "NFL Analytics Toolkit | Season {SEASON} Week {WEEK} | ",
        "R/31 Player Volume Allocation\n",
        "Capital multipliers: R1=1.15x, R2=1.05x, R3=0.95x, ",
        "R4=0.85x, R5=0.75x, R6=0.65x, R7=0.55x, UDFA=0.45x"
      )
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold", size = 12),
      plot.subtitle      = ggplot2::element_text(size = 9, color = "#555555"),
      plot.caption       = ggplot2::element_text(size = 8, color = "#888888"),
      legend.position    = "right",
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y        = ggplot2::element_text(size = 7.5)
    )

  v3_height <- max(6, nrow(v3_data) * 0.28 + 3)
  v3_path <- file.path(OUTPUT_DIR, glue("{FILE_PREFIX}_rookie_capital_allocation.png"))
  ggplot2::ggsave(v3_path, p3, width = 11, height = v3_height, dpi = PLOT_DPI)
  cat(glue("  Saved: {FILE_PREFIX}_rookie_capital_allocation.png\n\n"))
}

# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

top_target <- alloc %>%
  dplyr::slice_max(expected_targets_pg, n = 1L, with_ties = FALSE)
top_carry <- alloc %>%
  dplyr::slice_max(expected_carries_pg, n = 1L, with_ties = FALSE)

cat(paste0(strrep("=", 70), "\n"))
cat("R/31 VISUALIZATION SUMMARY\n")
cat(paste0(strrep("=", 70), "\n"))
cat(glue("  Season: {SEASON} | Week: {WEEK}\n"))
cat(glue("  Players allocated:          {n_players}\n"))
cat(glue("  Rookies with volume > 0:    {nrow(v3_data)}\n"))
cat(glue("  Teams covered:              {n_teams}\n"))
cat(glue(
  "  Top target leader:          ",
  "{top_target$player_name} ({top_target$team}) ",
  "{round(top_target$expected_targets_pg, 1)} tgt/gm\n"
))
cat(glue(
  "  Top carry leader:           ",
  "{top_carry$player_name} ({top_carry$team}) ",
  "{round(top_carry$expected_carries_pg, 1)} car/gm\n"
))
cat("\n  Plots generated:\n")
cat(glue("  1. {FILE_PREFIX}_volume_leaders.png\n"))
cat(glue("  2. {FILE_PREFIX}_share_adjustment_chain.png\n"))
cat(glue(
  "  3. {FILE_PREFIX}_rookie_capital_allocation.png",
  if (nrow(v3_data) == 0L) " (skipped -- no rookies)\n" else "\n"
))
cat(glue("  Output: output/plots/\n"))
cat(glue("  Resolution: {PLOT_DPI} dpi\n"))
cat(paste0(strrep("=", 70), "\n"))

# ==============================================================================
# END OF FILE
# ==============================================================================
