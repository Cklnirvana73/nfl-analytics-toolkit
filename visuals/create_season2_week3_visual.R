# ==============================================================================
# SEASON 2 WEEK 3: EXTENDED FANTASY SCORING -- VISUALIZATIONS
# File: examples/create_season2_week3_visual.R
# ==============================================================================
#
# Produces three publication-quality static PNGs:
#
#   Plot 1: Scoring System Repricing -- Dumbbell Chart
#           Top 25 players by Sleeper total, Standard PPG vs Sleeper PPG.
#           Shows who gains and loses when Sleeper bonuses are applied.
#
#   Plot 2: New Parameter Value by Position -- Grouped Bar
#           Average bonus PPG from first down, long TD, and 100-yard bonus
#           broken out by position (QB/RB/WR/TE).
#
#   Plot 3: Superflex QB Value Gap -- Horizontal Bar
#           Season point gain for top 12 QBs moving from 4pt to 6pt pass TDs.
#           Quantifies a number most fantasy players have never calculated.
#
# OUTPUT
# ------
#   output/plots/s2_week3_scoring_repricing.png        (300 dpi)
#   output/plots/s2_week3_new_param_by_position.png    (300 dpi)
#   output/plots/s2_week3_superflex_qb_gap.png         (300 dpi)
#
# DENSITY CHECK (per visualization-patterns.md)
#   Plot 1: 25 labeled players -- static only (under 50 threshold)
#   Plot 2: 12 bars -- static only
#   Plot 3: 12 labeled QBs -- static only
#
# DEPENDENCIES
# ------------
#   ggplot2, dplyr, tidyr, glue, here, scales
#   R/15_multi_season_pbp.R
#   R/17_extended_scoring.R
#
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(glue)
library(here)
library(scales)

source(here::here("R", "15_multi_season_pbp.R"))
source(here::here("R", "17_extended_scoring.R"))

# ==============================================================================
# CONFIGURATION
# ==============================================================================

SEASON        <- 2025L
MIN_GAMES     <- 8L
OUTPUT_DIR    <- here::here("output", "plots")
ATTRIBUTION   <- "Data: nflfastR | Analysis: NFL Analytics Toolkit"

# Position colors -- colorblind-safe palette (Wong 2011)
POS_COLORS <- c(
  "QB" = "#E69F00",
  "RB" = "#56B4E9",
  "WR" = "#009E73",
  "TE" = "#CC79A7"
)

# Consistent theme applied to all plots
theme_toolkit <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 14, hjust = 0),
      plot.subtitle    = element_text(size = 11, color = "gray40", hjust = 0),
      plot.caption     = element_text(size = 8,  color = "gray50", hjust = 1),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", size = 10),
      axis.title       = element_text(size = 10),
      strip.text       = element_text(face = "bold", size = 10)
    )
}

# Create output directory if needed
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  message(glue("Created output directory: {OUTPUT_DIR}"))
}

# ==============================================================================
# DATA LOADING
# ==============================================================================

cat("Loading 2025 play-by-play data...\n")
pbp_2025 <- load_normalized_season(
  season    = SEASON,
  cache_dir = here::here("data", "season2_cache")
)

cat("Loading roster data...\n")
roster_2025 <- nflreadr::load_rosters(seasons = SEASON)

# ==============================================================================
# COMPUTE SCORING SYSTEMS
# All three plots draw from the same compare_scoring_systems() call.
# Run once, reuse across all plots.
# ==============================================================================

cat("Computing scoring systems...\n")

system_comparison <- compare_scoring_systems(
  pbp_data    = pbp_2025,
  roster_data = roster_2025,
  season      = SEASON,
  min_games   = MIN_GAMES,
  systems     = list(
    Standard = list(
      use_tiered_ppr = FALSE,
      te_premium     = FALSE,
      rush_att_bonus = 0,
      ppr            = 0,
      pass_td        = 4
    ),
    PPR = list(
      use_tiered_ppr = FALSE,
      te_premium     = FALSE,
      ppr            = 1.0
    ),
    Sleeper = list(
      use_tiered_ppr       = TRUE,
      te_premium           = TRUE,
      rush_att_bonus       = 0.25,
      first_down_points    = 0.5,
      long_td_bonus        = 2.0,
      long_td_threshold    = 40L,
      hundred_yard_bonus   = 3.0,
      two_point_conversion = 2.0,
      sack_penalty         = -1.0
    )
  )
)

# Verify required columns exist before any plot
stopifnot("Standard_ppg"  %in% names(system_comparison))
stopifnot("Standard_games" %in% names(system_comparison))
stopifnot("Sleeper_ppg"   %in% names(system_comparison))
stopifnot("Sleeper_total" %in% names(system_comparison))

# Restrict to skill positions with enough games
plot_base <- system_comparison %>%
  dplyr::filter(
    position %in% c("QB", "RB", "WR", "TE"),
    !is.na(Standard_ppg),
    !is.na(Sleeper_ppg),
    !is.na(Standard_games),
    Standard_games >= MIN_GAMES
  )

cat(glue("  Qualifying players for plots: {nrow(plot_base)}\n\n"))

# ==============================================================================
# PLOT 1: Scoring System Repricing -- Dumbbell Chart
# ==============================================================================
# Top 25 players by Sleeper total. Standard PPG (left dot) to Sleeper PPG
# (right dot) connected by a line. Color = position. Sorted by Sleeper PPG.
# Segment color shows direction: gained (green) vs lost (red).
# ==============================================================================

cat("Building Plot 1: Scoring System Repricing...\n")

# Verify columns before use
stopifnot("Standard_ppg" %in% names(plot_base))
stopifnot("Sleeper_ppg"  %in% names(plot_base))
stopifnot("Sleeper_total" %in% names(plot_base))

p1_data <- plot_base %>%
  dplyr::arrange(desc(Sleeper_total)) %>%
  dplyr::slice_head(n = 25L) %>%
  dplyr::mutate(
    ppg_gain      = Sleeper_ppg - Standard_ppg,
    gained        = ppg_gain >= 0,
    # y-axis: sorted by Sleeper PPG, best at top
    player_label  = factor(player_name,
                           levels = rev(player_name))
  )

# Density check: 25 labeled players -- static only, no interactive needed
cat(glue("  Plot 1 labeled points: {nrow(p1_data)} (threshold: 50 -- static only)\n"))

p1 <- ggplot(p1_data) +
  # Connecting segment -- color by gain/loss direction
  geom_segment(
    aes(x = Standard_ppg, xend = Sleeper_ppg,
        y = player_label,  yend = player_label,
        color = gained),
    linewidth = 1.2, alpha = 0.7
  ) +
  # Standard dot (hollow)
  geom_point(
    aes(x = Standard_ppg, y = player_label, fill = position),
    shape = 21, size = 3, color = "white", stroke = 0.8
  ) +
  # Sleeper dot (filled)
  geom_point(
    aes(x = Sleeper_ppg, y = player_label, fill = position),
    shape = 21, size = 3.5, color = "gray30", stroke = 0.5
  ) +
  # Segment direction colors
  scale_color_manual(
    values = c("TRUE" = "#009E73", "FALSE" = "#D55E00"),
    labels = c("TRUE" = "Gained PPG", "FALSE" = "Lost PPG"),
    name   = "Direction"
  ) +
  # Position fill colors
  scale_fill_manual(
    values = POS_COLORS,
    name   = "Position"
  ) +
  # X axis: PPG
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.1),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  labs(
    title    = "Sleeper Scoring Reprices the Top 25",
    subtitle = glue(
      "Hollow dot = Standard scoring | Filled dot = Sleeper scoring | ",
      "Min {MIN_GAMES} games | 2025 season"
    ),
    x       = "Fantasy Points Per Game",
    y       = NULL,
    caption = glue(
      "Sleeper settings: Tiered PPR, TE premium, 0.5 first down, ",
      "2pt long TD (40+ yds), 3pt 100-yd game, -1pt sack\n{ATTRIBUTION}"
    )
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(linewidth = 2)),
    fill  = guide_legend(order = 2,
                         override.aes = list(shape = 21, size = 3.5,
                                             color = "gray30"))
  ) +
  theme_toolkit() +
  theme(
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.major.y = element_blank()
  )

ggsave(
  filename = file.path(OUTPUT_DIR, "s2_week3_scoring_repricing.png"),
  plot     = p1,
  width    = 10, height = 9, dpi = 300, bg = "white"
)
cat("  Saved: s2_week3_scoring_repricing.png\n\n")

# ==============================================================================
# PLOT 2: New Parameter Value by Position -- Grouped Bar
# ==============================================================================
# Average bonus PPG from each new Sleeper parameter by position.
# Positions on x-axis, three grouped bars per position (first down / long TD /
# 100-yd bonus). Shows which position benefits most from which bonus type.
# ==============================================================================

cat("Building Plot 2: New Parameter Value by Position...\n")

# Need Sleeper scoring broken down by component to get the position-level means.
# Re-run with Sleeper settings to get the component columns.
fp_sleeper <- calculate_fantasy_points_ext(
  pbp_data             = pbp_2025,
  roster_data          = roster_2025,
  season               = SEASON,
  use_tiered_ppr       = TRUE,
  te_premium           = TRUE,
  rush_att_bonus       = 0.25,
  first_down_points    = 0.5,
  long_td_bonus        = 2.0,
  long_td_threshold    = 40L,
  hundred_yard_bonus   = 3.0,
  two_point_conversion = 2.0,
  sack_penalty         = -1.0
)

# Verify component columns exist
stopifnot("first_down_fantasy_points"   %in% names(fp_sleeper))
stopifnot("long_td_fantasy_points"      %in% names(fp_sleeper))
stopifnot("hundred_yard_fantasy_points" %in% names(fp_sleeper))

# Season totals per player, then average across qualified players per position
sleeper_season <- fp_sleeper %>%
  dplyr::group_by(player_id, player_name, position, team) %>%
  dplyr::summarise(
    games          = dplyr::n_distinct(game_id),
    first_down_ppg = sum(first_down_fantasy_points,   na.rm = TRUE) /
                     dplyr::if_else(dplyr::n_distinct(game_id) > 0,
                                    as.double(dplyr::n_distinct(game_id)), 1),
    long_td_ppg    = sum(long_td_fantasy_points,      na.rm = TRUE) /
                     dplyr::if_else(dplyr::n_distinct(game_id) > 0,
                                    as.double(dplyr::n_distinct(game_id)), 1),
    hundred_yd_ppg = sum(hundred_yard_fantasy_points, na.rm = TRUE) /
                     dplyr::if_else(dplyr::n_distinct(game_id) > 0,
                                    as.double(dplyr::n_distinct(game_id)), 1),
    .groups        = "drop"
  ) %>%
  dplyr::filter(
    games >= MIN_GAMES,
    position %in% c("QB", "RB", "WR", "TE")
  )

# Position-level averages
p2_data <- sleeper_season %>%
  dplyr::group_by(position) %>%
  dplyr::summarise(
    n_players        = dplyr::n(),
    `First Down\n(0.5 pts)` = mean(first_down_ppg, na.rm = TRUE),
    `Long TD\n(2 pts, 40+ yds)` = mean(long_td_ppg, na.rm = TRUE),
    `100-Yard Game\n(3 pts)`  = mean(hundred_yd_ppg, na.rm = TRUE),
    .groups          = "drop"
  ) %>%
  tidyr::pivot_longer(
    cols      = c(`First Down\n(0.5 pts)`,
                  `Long TD\n(2 pts, 40+ yds)`,
                  `100-Yard Game\n(3 pts)`),
    names_to  = "bonus_type",
    values_to = "avg_ppg"
  ) %>%
  dplyr::mutate(
    bonus_type = factor(bonus_type,
                        levels = c("First Down\n(0.5 pts)",
                                   "Long TD\n(2 pts, 40+ yds)",
                                   "100-Yard Game\n(3 pts)")),
    position   = factor(position, levels = c("QB", "RB", "WR", "TE"))
  )

# Bonus type color palette (colorblind-safe sequential)
BONUS_COLORS <- c(
  "First Down\n(0.5 pts)"       = "#0072B2",
  "Long TD\n(2 pts, 40+ yds)"   = "#E69F00",
  "100-Yard Game\n(3 pts)"      = "#D55E00"
)

# League average total bonus reference line (all positions combined)
avg_total_bonus <- sleeper_season %>%
  dplyr::summarise(
    avg = mean(first_down_ppg + long_td_ppg + hundred_yd_ppg, na.rm = TRUE)
  ) %>%
  dplyr::pull(avg)

p2 <- ggplot(p2_data,
             aes(x = position, y = avg_ppg, fill = bonus_type)) +
  geom_col(
    position = position_dodge(width = 0.75),
    width    = 0.65,
    color    = "white",
    linewidth = 0.3
  ) +
  # Value labels on each bar
  geom_text(
    aes(label = round(avg_ppg, 2)),
    position = position_dodge(width = 0.75),
    vjust    = -0.4,
    size     = 3,
    color    = "gray30"
  ) +
  scale_fill_manual(values = BONUS_COLORS, name = "Bonus Type") +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title    = "Which Position Benefits Most from Sleeper Bonuses?",
    subtitle = glue(
      "Average bonus points per game by type | ",
      "Qualified players: min {MIN_GAMES} games | 2025 season"
    ),
    x       = "Position",
    y       = "Average Bonus Points Per Game",
    caption = glue(
      "First down bonus applies to rusher/receiver only (not passer).\n",
      "Long TD bonus applies to scorer (receiver on pass plays, rusher on run plays).\n",
      "{ATTRIBUTION}"
    )
  ) +
  theme_toolkit() +
  theme(legend.position = "bottom")

ggsave(
  filename = file.path(OUTPUT_DIR, "s2_week3_new_param_by_position.png"),
  plot     = p2,
  width    = 9, height = 6, dpi = 300, bg = "white"
)
cat("  Saved: s2_week3_new_param_by_position.png\n\n")

# ==============================================================================
# PLOT 3: Superflex QB Value Gap -- Horizontal Bar
# ==============================================================================
# For the top 12 QBs (by Sleeper total), the season point gain from moving
# from 4pt to 6pt passing TDs. Annotated with actual gap value.
# Reference line at the average gap across all qualified QBs.
# ==============================================================================

cat("Building Plot 3: Superflex QB Value Gap...\n")

# Compute both systems for QBs only
fp_4pt <- calculate_fantasy_points_ext(
  pbp_data          = pbp_2025,
  roster_data       = roster_2025,
  season            = SEASON,
  superflex_pass_td = 4.0,
  te_premium        = FALSE
)

fp_6pt <- calculate_fantasy_points_ext(
  pbp_data          = pbp_2025,
  roster_data       = roster_2025,
  season            = SEASON,
  superflex_pass_td = 6.0,
  te_premium        = FALSE
)

# Season totals
qb_4pt <- fp_4pt %>%
  dplyr::filter(position == "QB") %>%
  dplyr::group_by(player_id, player_name, team) %>%
  dplyr::summarise(
    games     = dplyr::n_distinct(game_id),
    total_4pt = sum(total_fantasy_points, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  dplyr::filter(games >= MIN_GAMES)

qb_6pt <- fp_6pt %>%
  dplyr::filter(position == "QB") %>%
  dplyr::group_by(player_id, player_name, team) %>%
  dplyr::summarise(
    games     = dplyr::n_distinct(game_id),
    total_6pt = sum(total_fantasy_points, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  dplyr::filter(games >= MIN_GAMES)

# Verify column names before join
stopifnot("player_id" %in% names(qb_4pt))
stopifnot("player_id" %in% names(qb_6pt))

p3_data <- qb_4pt %>%
  dplyr::inner_join(
    qb_6pt %>% dplyr::select(player_id, total_6pt),
    by = "player_id"
  ) %>%
  dplyr::mutate(
    gap = total_6pt - total_4pt
  ) %>%
  dplyr::arrange(desc(total_6pt)) %>%
  dplyr::slice_head(n = 12L) %>%
  dplyr::mutate(
    # Sorted worst to best so ggplot bottom-to-top puts best at top
    player_label = factor(player_name,
                          levels = player_name[order(gap)])
  )

# League average gap across ALL qualified QBs (not just top 12)
avg_gap_all_qbs <- qb_4pt %>%
  dplyr::inner_join(
    qb_6pt %>% dplyr::select(player_id, total_6pt),
    by = "player_id"
  ) %>%
  dplyr::mutate(gap = total_6pt - total_4pt) %>%
  dplyr::summarise(avg_gap = mean(gap, na.rm = TRUE)) %>%
  dplyr::pull(avg_gap)

cat(glue("  Average gap across all qualified QBs: {round(avg_gap_all_qbs, 1)} pts\n"))
cat(glue("  Plot 3 labeled QBs: {nrow(p3_data)} (threshold: 50 -- static only)\n"))

# Precompute qualified QB count for caption
n_qualified_qbs <- qb_4pt %>%
  dplyr::inner_join(qb_6pt %>% dplyr::select(player_id), by = "player_id") %>%
  nrow()

p3 <- ggplot(p3_data,
             aes(x = gap, y = player_label)) +
  # Reference line: average gap across all qualified QBs
  geom_vline(
    xintercept = avg_gap_all_qbs,
    linetype   = "dashed",
    color      = "gray50",
    linewidth  = 0.8
  ) +
  annotate(
    "text",
    x     = avg_gap_all_qbs,
    y     = Inf,
    label = glue("Avg all QBs\n({round(avg_gap_all_qbs, 1)} pts)"),
    vjust = 1.3,
    hjust = -0.08,
    size  = 3,
    color = "gray40"
  ) +
  geom_col(
    fill  = "#E69F00",
    width = 0.65,
    color = "white",
    linewidth = 0.3
  ) +
  # Value label inside/outside each bar
  geom_text(
    aes(label = glue("+{round(gap, 0)} pts")),
    hjust  = -0.15,
    size   = 3.2,
    color  = "gray30",
    fontface = "bold"
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.2)),
    labels = scales::number_format(accuracy = 1, prefix = "+")
  ) +
  labs(
    title    = "How Much Does 6-Point Pass TD Value Add Over a Full Season?",
    subtitle = glue(
      "Season point gain for top 12 QBs by Sleeper total: 6pt vs 4pt passing TDs | ",
      "2025 regular season | Min {MIN_GAMES} games"
    ),
    x       = "Season Points Gained (6pt minus 4pt)",
    y       = NULL,
    caption = glue(
      "Dashed line = average gain across all {n_qualified_qbs} ",
      "qualified QBs.\n",
      "One passing TD = 2 additional points. A QB with 43 TDs gains 86 pts -- ",
      "equivalent to ~2.5 extra PPG.\n{ATTRIBUTION}"
    )
  ) +
  theme_toolkit() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90")
  )

ggsave(
  filename = file.path(OUTPUT_DIR, "s2_week3_superflex_qb_gap.png"),
  plot     = p3,
  width    = 9, height = 6, dpi = 300, bg = "white"
)
cat("  Saved: s2_week3_superflex_qb_gap.png\n\n")

# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

cat("==============================================================================\n")
cat("Season 2 Week 3 Visualizations Complete\n")
cat("==============================================================================\n")
cat(glue("Output directory: {OUTPUT_DIR}\n\n"))

cat("Files saved:\n")
cat("  s2_week3_scoring_repricing.png     -- Top 25 players, Standard vs Sleeper PPG\n")
cat("  s2_week3_new_param_by_position.png -- Avg bonus PPG by position and type\n")
cat("  s2_week3_superflex_qb_gap.png      -- Top 12 QB season gain, 6pt vs 4pt TDs\n\n")

cat("Key findings from 2025 data:\n")

# Plot 1 summary
top_gainer <- p1_data %>%
  dplyr::arrange(desc(ppg_gain)) %>%
  dplyr::slice_head(n = 1L)
top_loser <- p1_data %>%
  dplyr::arrange(ppg_gain) %>%
  dplyr::slice_head(n = 1L)

cat(glue(
  "  Plot 1: Biggest gainer = {top_gainer$player_name} ",
  "(+{round(top_gainer$ppg_gain, 1)} PPG)\n"
))
cat(glue(
  "  Plot 1: Biggest loser  = {top_loser$player_name} ",
  "({round(top_loser$ppg_gain, 1)} PPG)\n"
))

# Plot 2 summary
rb_total <- p2_data %>%
  dplyr::filter(position == "RB") %>%
  dplyr::summarise(total = sum(avg_ppg)) %>%
  dplyr::pull(total)
qb_total <- p2_data %>%
  dplyr::filter(position == "QB") %>%
  dplyr::summarise(total = sum(avg_ppg)) %>%
  dplyr::pull(total)
cat(glue(
  "  Plot 2: RB avg total bonus = {round(rb_total, 2)} PPG | ",
  "QB avg total bonus = {round(qb_total, 2)} PPG\n"
))

# Plot 3 summary
top_qb <- p3_data %>%
  dplyr::arrange(desc(gap)) %>%
  dplyr::slice_head(n = 1L)
cat(glue(
  "  Plot 3: Largest QB gap = {top_qb$player_name} ",
  "(+{round(top_qb$gap, 0)} pts, {top_qb$games} games)\n"
))
cat(glue("  Plot 3: Avg gap all QBs = {round(avg_gap_all_qbs, 1)} pts\n"))

cat("==============================================================================\n")
