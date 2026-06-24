# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 17
# Visualization Script
# File: examples/create_season2_week17_visuals.R
#
# Three charts, each derived from a specific finding produced by R/37:
#
#   Plot 1: s2_week17_dynasty_value_swing.png
#           Diverging bar chart -- dynasty trade_value minus redraft
#           trade_value for the 8 biggest gainers and 8 biggest fallers.
#           Finding: dynasty systematically rewards youth and penalizes
#           age in a way that pure VORP rankings do not capture.
#
#   Plot 2: s2_week17_aging_multiplier_curves.png
#           Faceted line chart (QB / RB / WR / TE) -- aging multiplier
#           vs player age, at a reference projection of 10 fppg, for
#           both the 1-season redraft horizon and the 3-season dynasty
#           horizon. Finding: the dynasty horizon amplifies the positional
#           aging curve effect significantly, especially for RBs past 28.
#
#   Plot 3: s2_week17_dynasty_top30.png
#           Horizontal lollipop -- top 30 players by dynasty trade value,
#           colored by position. Finding: after aging adjustments, the
#           top dynasty assets combine young skill players with peak-age
#           stars, and the tier system distributes them as expected.
#
# Density check (per visualization-patterns.md section 17):
#   Plot 1: 16 labeled players -- static PNG only (under 50 threshold)
#   Plot 2: 2 lines x 4 facets -- static PNG only
#   Plot 3: 30 labeled players -- static PNG only (under 50 threshold)
#
# Data source:
#   Visualization builds trade value tables directly from the cached VORP
#   rankings, prospect scores, and aging curves. This bypasses the
#   interactive Sleeper league-selection menu from build_trade_value_table()
#   and is appropriate for visualization purposes. The working league is
#   the first league present in the VORP cache.
#
#   If s2_week17_trade_values.rds already exists from a prior interactive
#   run it is loaded instead, and only the dynasty version is recomputed
#   for the swing comparison.
#
# Run:
#   source(here::here("examples", "create_season2_week17_visuals.R"))
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(glue)
library(here)
library(scales)

source(here::here("R", "37_trade_value_model.R"))


# ==============================================================================
# CONFIGURATION
# ==============================================================================

SEASON               <- 2026L
AGE_GRID             <- 22:36
REF_MU_ILLUSTRATION  <- 10.0   # fppg reference for multiplier curve plots
N_SWING_EACH_SIDE    <- 8L     # top gainers AND top fallers per swing chart
N_TOP_DYNASTY        <- 30L    # players in the dynasty ranking chart

OUTPUT_DIR   <- here::here("output", "plots")
ATTRIBUTION  <- "Data: nflfastR + Sleeper | NFL Analytics Toolkit"

# Colorblind-safe Okabe-Ito palette (Wong 2011)
POS_COLORS <- c(
  "QB" = "#E69F00",
  "RB" = "#56B4E9",
  "WR" = "#009E73",
  "TE" = "#CC79A7"
)

# Diverging palette for format swing (positive = dynasty gain, negative = loss)
SWING_COLORS <- c(
  "Dynasty gain" = "#009E73",  # teal (Okabe-Ito green)
  "Dynasty loss" = "#D55E00"   # brick (Okabe-Ito vermillion)
)

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  message(glue("Created output directory: {OUTPUT_DIR}"))
}


# ==============================================================================
# SHARED THEME
# ==============================================================================

theme_toolkit <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = rel(1.15), hjust = 0),
      plot.subtitle    = element_text(color = "gray40", size = rel(0.88), hjust = 0),
      plot.caption     = element_text(color = "gray55", size = rel(0.72), hjust = 1),
      plot.background  = element_rect(fill = "white", color = NA),
      plot.margin      = margin(10, 15, 10, 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray92"),
      panel.grid.major.x = element_line(color = "gray92"),
      axis.title       = element_text(color = "gray35", size = rel(0.88)),
      axis.text        = element_text(color = "gray40"),
      strip.text       = element_text(face = "bold", size = rel(0.95)),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", size = rel(0.85))
    )
}


# ==============================================================================
# DATA LOADING
# ==============================================================================

cat("\nLoading inputs...\n")

if (!file.exists(VORP_CACHE_PATH)) {
  stop(glue("VORP cache not found: {VORP_CACHE_PATH}. Run R/33 first."),
       call. = FALSE)
}
if (!file.exists(PROSPECT_CSV_PATH)) {
  stop(glue("Prospect scores not found: {PROSPECT_CSV_PATH}. Run R/28 first."),
       call. = FALSE)
}

vorp_all <- readRDS(VORP_CACHE_PATH)
vorp_all <- .attach_player_ages(vorp_all, season = SEASON)

curves <- .load_aging_curves_with_fallback(season = AGING_PANEL_LAST_SEASON,
                                            verbose = FALSE)

prospects_raw <- readr::read_csv(PROSPECT_CSV_PATH, show_col_types = FALSE)
if (!"nfl_gsis_id" %in% names(prospects_raw)) {
  prospects_raw$nfl_gsis_id <- NA_character_
}
rookies <- prospects_raw %>%
  dplyr::filter(draft_class_type == "prediction", !is.na(score_final)) %>%
  dplyr::transmute(nfl_gsis_id, player_name = cfb_player_name,
                   position, score_final)
rookies$interval_width <- NA_real_

example_league <- unique(vorp_all$league_name)[1]
vorp_one       <- dplyr::filter(vorp_all, league_name == example_league)

cat(glue("  Working league: {example_league}\n"))
cat(glue("  Veterans in VORP cache: {nrow(vorp_one)}\n"))
cat(glue("  Rookies (prediction cohort): {nrow(rookies)}\n\n"))

# Build redraft and dynasty tables for this league.
tv_redraft <- compute_player_trade_values(vorp_one, rookies, curves, FALSE)
tv_redraft <- .assign_tiers_and_ranks(tv_redraft)
tv_redraft$league_name <- example_league

tv_dynasty <- compute_player_trade_values(vorp_one, rookies, curves, TRUE)
tv_dynasty <- .assign_tiers_and_ranks(tv_dynasty)
tv_dynasty$league_name <- example_league

cat(glue("  Redraft: {nrow(tv_redraft)} players valued ",
         "({sum(tv_redraft$rookie_flag)} rookies)\n"))
cat(glue("  Dynasty: {nrow(tv_dynasty)} players valued ",
         "({sum(tv_dynasty$rookie_flag)} rookies)\n\n"))


# ==============================================================================
# PLOT 1: Dynasty Value Swing (diverging bar)
# ==============================================================================
# Shows the players whose trade value changes most when switching from redraft
# to dynasty. Dynasty rewards youth (long time horizon amplifies the aging
# climb); it penalizes older veterans (3-season decay discounts their value).
# ==============================================================================

cat("Building Plot 1: Dynasty Value Swing...\n")

swing_data <- tv_redraft %>%
  dplyr::filter(!rookie_flag, !is.na(nfl_gsis_id),
                trade_value > 0 | !is.na(age_at_season_start)) %>%
  dplyr::select(nfl_gsis_id, player_name, position,
                age_at_season_start, tv_redraft = trade_value) %>%
  dplyr::inner_join(
    tv_dynasty %>%
      dplyr::filter(!rookie_flag, !is.na(nfl_gsis_id)) %>%
      dplyr::select(nfl_gsis_id, tv_dynasty = trade_value),
    by = "nfl_gsis_id"
  ) %>%
  dplyr::mutate(delta = tv_dynasty - tv_redraft)

# Pre-delivery assertion
stopifnot("Swing data is empty -- join or filter produced zero rows" =
            nrow(swing_data) > 0)

top_gainers <- swing_data %>%
  dplyr::slice_max(order_by = delta, n = N_SWING_EACH_SIDE)
top_fallers <- swing_data %>%
  dplyr::slice_min(order_by = delta, n = N_SWING_EACH_SIDE)

plot1_data <- dplyr::bind_rows(top_gainers, top_fallers) %>%
  dplyr::distinct(nfl_gsis_id, .keep_all = TRUE) %>%
  dplyr::mutate(
    direction = if_else(delta >= 0, "Dynasty gain", "Dynasty loss"),
    # Factor: worst-to-best (ascending delta) so ggplot draws best at top
    player_label = glue("{player_name} ({position}, {age_at_season_start})"),
    player_label = factor(player_label,
                          levels = player_label[order(delta)])
  )

p1 <- ggplot(plot1_data,
             aes(x = delta, y = player_label, fill = direction)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_vline(xintercept = 0, color = "gray30", linewidth = 0.5) +
  geom_text(
    aes(label = format(round(delta, 2), nsmall = 2),
        hjust = if_else(delta >= 0, -0.15, 1.15)),
    size = 3.0, color = "gray20"
  ) +
  scale_fill_manual(values = SWING_COLORS) +
  scale_x_continuous(
    labels = function(x) format(x, big.mark = ","),
    expand = expansion(mult = c(0.15, 0.20))
  ) +
  labs(
    title    = "Dynasty Recalibrates Value: Youth Up, Age Down",
    subtitle = glue("Trade value change (dynasty minus redraft) | {example_league} | ",
                    "Top {N_SWING_EACH_SIDE} gainers and {N_SWING_EACH_SIDE} fallers"),
    x        = "Dynasty trade value minus redraft trade value",
    y        = NULL,
    caption  = ATTRIBUTION
  ) +
  theme_toolkit() +
  theme(panel.grid.major.y = element_blank())

ggsave(
  filename = here::here(OUTPUT_DIR, "s2_week17_dynasty_value_swing.png"),
  plot     = p1,
  width    = 9, height = 7, dpi = 300
)
cat("  Saved: s2_week17_dynasty_value_swing.png\n")


# ==============================================================================
# PLOT 2: Aging Multiplier Curves by Position (faceted line)
# ==============================================================================
# The aging multiplier is the model's most distinctive component. This chart
# makes it legible by showing how the multiplier moves across the career arc
# for each skill position, comparing the 1-season redraft horizon to the
# 3-season dynasty horizon. The gap between the lines is the dynasty premium
# for youth and the dynasty discount for age.
# Reference player: 10 fppg posterior_mu (a mid-tier starter).
# ==============================================================================

cat("Building Plot 2: Aging Multiplier Curves...\n")

mult_grid <- purrr::map_dfr(CURVE_POSITIONS_TV, function(pos) {
  purrr::map_dfr(AGE_GRID, function(age) {
    tibble::tibble(
      position = pos,
      age      = age,
      Redraft  = .project_aging_multiplier(age, pos, curves, N_SEASONS_REDRAFT,
                                            REF_MU_ILLUSTRATION, FALSE),
      Dynasty  = .project_aging_multiplier(age, pos, curves, N_SEASONS_DYNASTY,
                                            REF_MU_ILLUSTRATION, TRUE)
    )
  })
}) %>%
  tidyr::pivot_longer(cols = c(Redraft, Dynasty),
                      names_to  = "Format",
                      values_to = "multiplier") %>%
  dplyr::mutate(
    Format   = factor(Format, levels = c("Redraft", "Dynasty")),
    position = factor(position, levels = CURVE_POSITIONS_TV)
  )

stopifnot("Multiplier grid is empty" = nrow(mult_grid) > 0)

p2 <- ggplot(mult_grid, aes(x = age, y = multiplier,
                             color = Format, linetype = Format)) +
  geom_hline(yintercept = 1.00, color = "gray60",
             linetype = "dashed", linewidth = 0.4) +
  geom_hline(yintercept = AGING_MULTIPLIER_FLOOR, color = "gray80",
             linetype = "dotted", linewidth = 0.4) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.6) +
  facet_wrap(~ position, ncol = 2) +
  annotate("text", x = max(AGE_GRID) - 0.5, y = 1.02,
           label = "Neutral (1.0)", hjust = 1, vjust = -0.3,
           size = 2.8, color = "gray50", fontface = "italic") +
  annotate("text", x = max(AGE_GRID) - 0.5, y = AGING_MULTIPLIER_FLOOR + 0.02,
           label = glue("Floor ({AGING_MULTIPLIER_FLOOR})"),
           hjust = 1, vjust = -0.3,
           size = 2.8, color = "gray65", fontface = "italic") +
  scale_color_manual(values = c("Redraft" = "#0072B2", "Dynasty" = "#D55E00")) +
  scale_linetype_manual(values = c("Redraft" = "solid", "Dynasty" = "dashed")) +
  scale_x_continuous(breaks = seq(22, 36, by = 2)) +
  scale_y_continuous(
    labels = function(x) format(round(x, 2), nsmall = 2),
    limits = c(AGING_MULTIPLIER_FLOOR - 0.03, 1.38)
  ) +
  labs(
    title    = "Dynasty Amplifies Aging: Position and Horizon Comparison",
    subtitle = glue("Aging multiplier applied to VORP | Reference projection: ",
                    "{REF_MU_ILLUSTRATION} fppg | ",
                    "Redraft = 1-season horizon, Dynasty = 3-season horizon"),
    x        = "Player age (as of September 1, season start)",
    y        = "Aging multiplier",
    color    = "Format horizon",
    linetype = "Format horizon",
    caption  = ATTRIBUTION
  ) +
  theme_toolkit()

ggsave(
  filename = here::here(OUTPUT_DIR, "s2_week17_aging_multiplier_curves.png"),
  plot     = p2,
  width    = 10, height = 7, dpi = 300
)
cat("  Saved: s2_week17_aging_multiplier_curves.png\n")


# ==============================================================================
# PLOT 3: Dynasty Top 30 Rankings (lollipop)
# ==============================================================================
# The trade value rankings after applying VORP + aging curve adjustments.
# Colored by position to reveal whether the model surfaces expected
# position-by-age patterns: young skill players and peak-age stars at the top,
# older players and depth pieces further down.
# ==============================================================================

cat("Building Plot 3: Dynasty Top 30 Rankings...\n")

top30_data <- tv_dynasty %>%
  dplyr::filter(trade_value > 0) %>%
  dplyr::arrange(overall_tv_rank) %>%
  dplyr::slice_head(n = N_TOP_DYNASTY) %>%
  dplyr::mutate(
    # Use rookie_flag (not is.na(age)) to assign the R label.
    # A veteran whose age did not resolve from rosters gets "?" so the
    # mismatch is visible rather than silently mislabeled as a rookie.
    age_label = dplyr::case_when(
      rookie_flag                     ~ "R",
      !is.na(age_at_season_start)     ~ as.character(age_at_season_start),
      TRUE                            ~ "?"
    ),
    player_label = glue("{player_name} ({age_label})"),
    # Factor: ascending trade_value so ggplot draws highest value at top
    player_label = factor(player_label,
                          levels = player_label[order(trade_value)])
  )

stopifnot("Top 30 data is empty -- all trade values may be zero" =
            nrow(top30_data) > 0)

p3 <- ggplot(top30_data,
             aes(x = trade_value, y = player_label, color = position)) +
  geom_segment(aes(x = 0, xend = trade_value, y = player_label,
                   yend = player_label),
               color = "gray80", linewidth = 0.5) +
  geom_point(size = 3.2) +
  scale_color_manual(values = POS_COLORS, name = "Position") +
  scale_x_continuous(
    labels = function(x) format(round(x, 1), nsmall = 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title    = glue("Dynasty Trade Value: Top {N_TOP_DYNASTY} Assets"),
    subtitle = glue("{example_league} | Aging-adjusted VORP | ",
                    "Age at season start in parentheses; R = rookie"),
    x        = "Dynasty trade value",
    y        = NULL,
    caption  = ATTRIBUTION
  ) +
  theme_toolkit() +
  theme(
    legend.position = "right",
    panel.grid.major.y = element_blank()
  )

ggsave(
  filename = here::here(OUTPUT_DIR, "s2_week17_dynasty_top30.png"),
  plot     = p3,
  width    = 10, height = 9, dpi = 300
)
cat("  Saved: s2_week17_dynasty_top30.png\n")


# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

cat("\n================================================================\n")
cat(glue("VISUALIZATION SUMMARY: Week 17 Trade Value Model\n"))
cat("================================================================\n")
cat(glue("League: {example_league}\n"))
cat(glue("Players in dynasty table: {nrow(tv_dynasty)} ",
         "({sum(tv_dynasty$rookie_flag)} rookies)\n"))
cat(glue("Plots written to: {OUTPUT_DIR}\n"))
cat("\n")
cat("  s2_week17_dynasty_value_swing.png\n")
cat(glue("    Dynasty vs redraft delta | ",
         "{N_SWING_EACH_SIDE} gainers + {N_SWING_EACH_SIDE} fallers\n"))
cat("  s2_week17_aging_multiplier_curves.png\n")
cat(glue("    Multiplier by age and position | ",
         "Redraft vs dynasty horizon | Ref mu = {REF_MU_ILLUSTRATION} fppg\n"))
cat("  s2_week17_dynasty_top30.png\n")
cat(glue("    Top {N_TOP_DYNASTY} dynasty assets | Colored by position\n"))
cat("================================================================\n\n")
