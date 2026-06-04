# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 15
# Visualization Script: R/33 VORP Rankings Engine
# File: examples/create_s2_week15_r33_visuals.R
#
# PURPOSE
# -------
# Three publication-quality visualizations derived from R/33 output:
#
#   V1: Overall VORP big board -- top 40 players in the representative league
#       (horizontal bar, colored by position)
#
#   V2: PPG vs adjusted VORP -- who punches above their raw projection weight?
#       (scatter, colored by position, labels for outliers)
#
#   V3: Boom and bust modifier distributions by position
#       (paired box plots showing how modifiers reshape raw VORP)
#
# DEPENDENCIES
# ------------
#   data/season2_cache/s2_week15_vorp_rankings.csv  (R/33 output)
#
# LEAGUE SELECTION
# ----------------
# All three plots filter to a single representative league.
# Priority order: (1) first PPR-format league, (2) first standard league,
# (3) first league in the file regardless of format.
#
# OUTPUTS (all to output/plots/)
# --------------------------------
#   s2_week15_r33_vorp_big_board.png
#   s2_week15_r33_ppg_vs_vorp.png
#   s2_week15_r33_boom_bust_modifiers.png
#
# SCHEMA TAG: s2_w15_vorp_v1
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
FILE_PREFIX <- "s2_week15_r33"

# Big board: number of players to show
TOP_N_BOARD <- 40L

# PPG vs VORP: label players whose adjusted_vorp deviates from expected
# by more than this many points from the position median fit
LABEL_VORP_OUTLIER_DELTA <- 2.5

# Consistent position ordering and colors
POSITION_ORDER  <- c("QB", "RB", "WR", "TE")
POSITION_COLORS <- c(QB = "#E63946", RB = "#457B9D", WR = "#2A9D8F", TE = "#E9C46A")

# ==============================================================================
# SETUP
# ==============================================================================

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  cat(glue("Created output directory: {OUTPUT_DIR}\n\n"))
}

cat(paste0(strrep("=", 70), "\n"))
cat(glue("R/33 VORP RANKINGS -- VISUALIZATION SCRIPT\n"))
cat(glue("Season {SEASON} | Week {WEEK}\n"))
cat(paste0(strrep("=", 70), "\n\n"))

# ==============================================================================
# DATA LOAD
# ==============================================================================

vorp_path <- file.path(DATA_DIR, "s2_week15_vorp_rankings.csv")

if (!file.exists(vorp_path)) {
  stop(glue(
    "Required input not found: {vorp_path}\n",
    "Run R/33_vorp_rankings.R first to generate this file."
  ))
}

vorp <- readr::read_csv(vorp_path, show_col_types = FALSE)

cat(glue(
  "Loaded: s2_week15_vorp_rankings.csv ",
  "({format(nrow(vorp), big.mark = ',')} rows)\n\n"
))

# Validate required columns
required_cols <- c(
  "nfl_gsis_id", "player_name", "team", "position",
  "league_name", "league_format", "league_teams",
  "r32_posterior_mu", "r32_projection_upper_80",
  "boom_probability", "bust_probability",
  "replacement_ppg", "vorp_base",
  "boom_modifier", "bust_modifier", "ceiling_modifier",
  "adjusted_vorp", "overall_rank", "position_rank"
)

missing_cols <- setdiff(required_cols, names(vorp))
if (length(missing_cols) > 0) {
  stop(glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
}

cat(glue("Column validation: all {length(required_cols)} required columns present\n"))

n_leagues <- dplyr::n_distinct(vorp$league_name)
n_players <- dplyr::n_distinct(vorp$nfl_gsis_id)
leagues_present <- unique(vorp$league_name)
formats_present <- unique(vorp$league_format)

cat(glue(
  "Leagues: {n_leagues} | ",
  "Unique players: {n_players} | ",
  "Formats: {paste(formats_present, collapse = ', ')}\n"
))

# ------------------------------------------------------------------------------
# Select representative league: PPR first, then standard, then first available
# ------------------------------------------------------------------------------
ppr_leagues    <- vorp %>%
  dplyr::filter(league_format == "ppr") %>%
  dplyr::pull(league_name) %>%
  unique()

std_leagues <- vorp %>%
  dplyr::filter(league_format %in% c("standard", "half_ppr")) %>%
  dplyr::pull(league_name) %>%
  unique()

rep_league <- if (length(ppr_leagues) > 0L) {
  ppr_leagues[1]
} else if (length(std_leagues) > 0L) {
  std_leagues[1]
} else {
  leagues_present[1]
}

rep_format <- vorp %>%
  dplyr::filter(league_name == rep_league) %>%
  dplyr::pull(league_format) %>%
  dplyr::first()

rep_teams <- vorp %>%
  dplyr::filter(league_name == rep_league) %>%
  dplyr::pull(league_teams) %>%
  dplyr::first()

cat(glue(
  "Representative league: '{rep_league}' ",
  "({rep_format}, {rep_teams}-team)\n\n"
))

# Filter to representative league for all plots
vorp_rep <- vorp %>%
  dplyr::filter(league_name == rep_league) %>%
  dplyr::mutate(
    position = factor(position, levels = POSITION_ORDER)
  )

# Replacement levels for subtitle reference
rep_levels <- vorp_rep %>%
  dplyr::group_by(position) %>%
  dplyr::summarise(
    replacement_ppg = dplyr::first(replacement_ppg),
    .groups = "drop"
  ) %>%
  dplyr::arrange(position)

rep_level_str <- paste(
  glue("{rep_levels$position}: {round(rep_levels$replacement_ppg, 1)}"),
  collapse = " | "
)

# ==============================================================================
# VISUALIZATION 1: Overall VORP Big Board -- Top 40
# ==============================================================================
# The final draft output. Top TOP_N_BOARD by adjusted_vorp in the
# representative league. Position mix in the top 40 shows which positions
# the iterative FLEX algorithm determines are most scarce.

cat("Building V1: VORP big board...\n")

v1_data <- vorp_rep %>%
  dplyr::filter(!is.na(adjusted_vorp)) %>%
  dplyr::slice_max(order_by = adjusted_vorp, n = TOP_N_BOARD) %>%
  dplyr::arrange(adjusted_vorp) %>%
  dplyr::mutate(
    player_label = glue("{player_name} ({team})"),
    player_label = factor(player_label, levels = unique(player_label)),
    rank_label   = glue(
      "#{overall_rank} | {round(adjusted_vorp, 1)}"
    )
  )

# Position counts in top 40
pos_counts_v1 <- v1_data %>%
  dplyr::count(position) %>%
  dplyr::mutate(txt = glue("{position}: {n}")) %>%
  dplyr::pull(txt) %>%
  paste(collapse = " | ")

cat(glue("  Top {TOP_N_BOARD} composition: {pos_counts_v1}\n"))

p1 <- ggplot2::ggplot(
  v1_data,
  ggplot2::aes(x = adjusted_vorp, y = player_label, fill = position)
) +
  ggplot2::geom_col(alpha = 0.85, width = 0.72) +
  ggplot2::geom_text(
    ggplot2::aes(label = rank_label),
    hjust  = -0.08,
    size   = 2.6,
    color  = "gray25"
  ) +
  ggplot2::geom_vline(
    xintercept = 0,
    color      = "gray60",
    linewidth  = 0.5,
    linetype   = "solid"
  ) +
  ggplot2::scale_fill_manual(
    values = POSITION_COLORS,
    name   = "Position"
  ) +
  ggplot2::scale_x_continuous(
    name   = "Adjusted VORP (PPR pts/game above replacement)",
    expand = ggplot2::expansion(mult = c(0, 0.18)),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  ggplot2::labs(
    title    = glue(
      "Season {SEASON} VORP Big Board: Top {TOP_N_BOARD} Players"
    ),
    subtitle = glue(
      "League: {rep_league} ({rep_format}, {rep_teams}-team) | ",
      "{pos_counts_v1}\n",
      "Replacement PPG -- {rep_level_str}"
    ),
    y        = NULL,
    caption  = glue(
      "NFL Analytics Toolkit | Season {SEASON} Week {WEEK} | ",
      "R/33 VORP Rankings\n",
      "adjusted_vorp = vorp_base + boom_modifier + bust_modifier + ceiling_modifier"
    )
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title         = ggplot2::element_text(face = "bold", size = 13),
    plot.subtitle      = ggplot2::element_text(size = 9, color = "#555555"),
    plot.caption       = ggplot2::element_text(size = 8, color = "#888888"),
    legend.position    = "right",
    panel.grid.minor   = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    axis.text.y        = ggplot2::element_text(size = 8)
  )

v1_path <- file.path(OUTPUT_DIR, glue("{FILE_PREFIX}_vorp_big_board.png"))
ggplot2::ggsave(v1_path, p1, width = 12, height = 14, dpi = PLOT_DPI)
cat(glue("  Saved: {FILE_PREFIX}_vorp_big_board.png\n\n"))

# ==============================================================================
# VISUALIZATION 2: PPG vs Adjusted VORP -- Positional Scarcity Premium
# ==============================================================================
# r32_posterior_mu on x, adjusted_vorp on y. The spread between positions
# at the same PPG level shows the scarcity premium. QBs cluster below the
# cross-position trend (high PPG, limited VORP because QB pool is deep).
# Elite TEs cluster above (moderate PPG, high VORP because TE pool drops off).

cat("Building V2: PPG vs adjusted VORP scatter...\n")

v2_data <- vorp_rep %>%
  dplyr::filter(!is.na(r32_posterior_mu), !is.na(adjusted_vorp)) %>%
  dplyr::group_by(position) %>%
  dplyr::mutate(
    # Label top 5 per position and any outlier with high vorp vs low ppg
    pos_vorp_rank = dplyr::dense_rank(dplyr::desc(adjusted_vorp)),
    label_player  = pos_vorp_rank <= 5L | overall_rank <= 15L
  ) %>%
  dplyr::ungroup()

n_labeled_v2 <- sum(v2_data$label_player, na.rm = TRUE)
cat(glue("  Scatter: {nrow(v2_data)} players | {n_labeled_v2} labeled\n"))

p2 <- ggplot2::ggplot(
  v2_data,
  ggplot2::aes(x = r32_posterior_mu, y = adjusted_vorp, color = position)
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype   = "dashed",
    color      = "gray55",
    linewidth  = 0.7
  ) +
  ggplot2::annotate(
    "text",
    x     = max(v2_data$r32_posterior_mu, na.rm = TRUE) * 0.85,
    y     = 0.3,
    label = "Replacement level",
    size  = 3.0,
    color = "gray55",
    fontface = "italic"
  ) +
  ggplot2::geom_point(alpha = 0.65, size = 2.0) +
  ggrepel::geom_text_repel(
    data         = dplyr::filter(v2_data, label_player),
    mapping      = ggplot2::aes(label = glue("{player_name} ({team})")),
    size         = 2.5,
    max.overlaps = 25,
    segment.color = "gray65",
    segment.size  = 0.3,
    min.segment.length = 0.2,
    box.padding  = 0.3
  ) +
  ggplot2::scale_color_manual(
    values = POSITION_COLORS,
    name   = "Position"
  ) +
  ggplot2::scale_x_continuous(
    name   = "R/32 Posterior (PPR pts/game)",
    breaks = scales::pretty_breaks(n = 7)
  ) +
  ggplot2::scale_y_continuous(
    name   = "Adjusted VORP (pts/game above replacement)",
    breaks = scales::pretty_breaks(n = 7)
  ) +
  ggplot2::facet_wrap(~ position, scales = "free", nrow = 2L) +
  ggplot2::labs(
    title    = glue(
      "Scarcity Premium: Same PPG, Very Different Draft Value"
    ),
    subtitle = glue(
      "League: {rep_league} ({rep_format}) | ",
      "Top 5 per position labeled plus top 15 overall\n",
      "Replacement PPG -- {rep_level_str}"
    ),
    caption  = glue(
      "NFL Analytics Toolkit | Season {SEASON} Week {WEEK} | ",
      "R/33 VORP Rankings\n",
      "VORP = PPG above replacement | ",
      "Points near y=0 are borderline starters"
    )
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", size = 13),
    plot.subtitle    = ggplot2::element_text(size = 9, color = "#555555"),
    plot.caption     = ggplot2::element_text(size = 8, color = "#888888"),
    legend.position  = "none",
    strip.text       = ggplot2::element_text(face = "bold", size = 11),
    panel.grid.minor = ggplot2::element_blank()
  )

v2_path <- file.path(OUTPUT_DIR, glue("{FILE_PREFIX}_ppg_vs_vorp.png"))
ggplot2::ggsave(v2_path, p2, width = 12, height = 10, dpi = PLOT_DPI)
cat(glue("  Saved: {FILE_PREFIX}_ppg_vs_vorp.png\n\n"))

# ==============================================================================
# VISUALIZATION 3: Boom and Bust Modifier Distributions by Position
# ==============================================================================
# Paired box plots: boom_modifier (positive, adds value) and bust_modifier
# (negative, subtracts value) side by side per position. Shows how the
# probability-weighted modifiers reshape rankings beyond raw VORP.
# Bust modifier is shown as absolute value for visual symmetry then signed.

cat("Building V3: Boom and bust modifier distributions...\n")

v3_data <- vorp_rep %>%
  dplyr::filter(
    !is.na(boom_modifier),
    !is.na(bust_modifier),
    position %in% POSITION_ORDER
  ) %>%
  dplyr::select(
    player_name, team, position,
    boom_modifier, bust_modifier, ceiling_modifier,
    adjusted_vorp, vorp_base
  ) %>%
  tidyr::pivot_longer(
    cols      = c(boom_modifier, bust_modifier),
    names_to  = "modifier_type",
    values_to = "modifier_value"
  ) %>%
  dplyr::mutate(
    modifier_label = dplyr::recode(
      modifier_type,
      boom_modifier = "Boom modifier\n(adds value)",
      bust_modifier = "Bust modifier\n(subtracts value)"
    ),
    modifier_label = factor(
      modifier_label,
      levels = c("Boom modifier\n(adds value)", "Bust modifier\n(subtracts value)")
    )
  )

# Median per group for annotation
v3_medians <- v3_data %>%
  dplyr::group_by(position, modifier_label) %>%
  dplyr::summarise(
    med = median(modifier_value, na.rm = TRUE),
    n   = dplyr::n(),
    .groups = "drop"
  )

p3 <- ggplot2::ggplot(
  v3_data,
  ggplot2::aes(x = modifier_label, y = modifier_value, fill = position)
) +
  ggplot2::geom_hline(
    yintercept = 0,
    color      = "gray55",
    linewidth  = 0.6,
    linetype   = "dashed"
  ) +
  ggplot2::geom_boxplot(
    alpha         = 0.75,
    outlier.shape = 16,
    outlier.size  = 1.2,
    outlier.alpha = 0.40,
    linewidth     = 0.45,
    width         = 0.55
  ) +
  ggplot2::scale_fill_manual(values = POSITION_COLORS) +
  ggplot2::scale_x_discrete(name = NULL) +
  ggplot2::scale_y_continuous(
    name   = "Modifier value (PPR pts/game)",
    breaks = scales::pretty_breaks(n = 6)
  ) +
  ggplot2::facet_wrap(~ position, nrow = 1L) +
  ggplot2::labs(
    title    = glue(
      "How Much Do Boom and Bust Modifiers Reshape the Rankings?"
    ),
    subtitle = glue(
      "League: {rep_league} ({rep_format}) | ",
      "Modifiers = probability * format weight\n",
      "boom_modifier = boom_weight * boom_probability | ",
      "bust_modifier = -bust_weight * bust_probability"
    ),
    caption  = glue(
      "NFL Analytics Toolkit | Season {SEASON} Week {WEEK} | ",
      "R/33 VORP Rankings\n",
      "boom_modifier = boom_weight * boom_probability | ",
      "bust_modifier = -bust_weight * bust_probability"
    ),
    fill     = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", size = 13),
    plot.subtitle    = ggplot2::element_text(size = 9, color = "#555555"),
    plot.caption     = ggplot2::element_text(size = 8, color = "#888888"),
    legend.position  = "none",
    strip.text       = ggplot2::element_text(face = "bold", size = 11),
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.x      = ggplot2::element_text(size = 8.5, lineheight = 0.9)
  )

v3_path <- file.path(OUTPUT_DIR, glue("{FILE_PREFIX}_boom_bust_modifiers.png"))
ggplot2::ggsave(v3_path, p3, width = 13, height = 6, dpi = PLOT_DPI)
cat(glue("  Saved: {FILE_PREFIX}_boom_bust_modifiers.png\n\n"))

# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

top_overall <- vorp_rep %>%
  dplyr::filter(overall_rank == 1L)

top_per_pos <- vorp_rep %>%
  dplyr::group_by(position) %>%
  dplyr::slice_min(position_rank, n = 1L, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(position)

cat(paste0(strrep("=", 70), "\n"))
cat("R/33 VISUALIZATION SUMMARY\n")
cat(paste0(strrep("=", 70), "\n"))
cat(glue("  Season: {SEASON} | Week: {WEEK}\n"))
cat(glue("  Leagues in file:    {n_leagues}\n"))
cat(glue("  Unique players:     {n_players}\n"))
cat(glue("  Representative:     {rep_league} ({rep_format}, {rep_teams}-team)\n"))
cat(glue("  #1 overall:         {top_overall$player_name[1]} ",
         "({top_overall$team[1]} {top_overall$position[1]}) ",
         "VORP {round(top_overall$adjusted_vorp[1], 1)}\n"))
cat(glue("  Position leaders:\n"))
for (i in seq_len(nrow(top_per_pos))) {
  r <- top_per_pos[i, ]
  cat(glue(
    "    {r$position}: {r$player_name} ({r$team}) ",
    "VORP {round(r$adjusted_vorp, 1)}\n"
  ))
}
cat(glue("  Replacement PPG:    {rep_level_str}\n"))
cat("\n  Plots generated:\n")
cat(glue("  1. {FILE_PREFIX}_vorp_big_board.png\n"))
cat(glue("  2. {FILE_PREFIX}_ppg_vs_vorp.png\n"))
cat(glue("  3. {FILE_PREFIX}_boom_bust_modifiers.png\n"))
cat(glue("  Output: output/plots/\n"))
cat(glue("  Resolution: {PLOT_DPI} dpi\n"))
cat(paste0(strrep("=", 70), "\n"))

# ==============================================================================
# END OF FILE
# ==============================================================================
