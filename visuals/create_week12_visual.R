# ==============================================================================
# create_week12_visual.R
# NFL Analytics Toolkit - Week 12: Projection System Visualizations
# ==============================================================================
#
# Produces 6 publication-quality plots:
#   1. Prior weight decay curve -- how the system transitions from prior to
#      observed data across the 18-week regular season
#   2. Preseason vs week-14 projection comparison -- how the Bayesian updating
#      changed individual player projections across the season
#   3. Backtest RMSE by week and position -- ensemble vs naive baselines
#   4. [INTERACTIVE HTML] Projection report scatter -- boom probability vs
#      projected PPR, coloured by matchup flag, with full hover tooltips
#   5. 2026 top projected players by position -- lollipop chart with 95% CIs
#   6. [INTERACTIVE HTML] 2026 projection confidence vs production -- quadrant
#      scatter identifying draft targets vs volatile upside plays
#
# NFL SEASON SCOPE
# ----------------
# All plots are bounded to weeks 1-18 (regular season).
# Week 19 = Wild Card, week 22 = Super Bowl -- outside fantasy scope.
# Interactive plot density: 3 positions x ~30 players = 90+ points -> HTML required.
#
# OUTPUT
# ------
# output/plots/week12_prior_weight_decay.png
# output/plots/week12_projection_shift.png
# output/plots/week12_backtest_rmse.png
# output/plots/week12_projection_report_interactive.html
# output/plots/week12_2026_top_projections.png
# output/plots/week12_2026_confidence_scatter.png
# output/plots/week12_2026_confidence_interactive.html
#
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(glue)

source(here::here("R", "14_projection_system.R"))

# Create output directories
plots_dir <- here::here("output", "plots")
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

# ==============================================================================
# LOAD ARTIFACTS
# ==============================================================================

cat("Loading artifacts...\n")

prior_stats_path <- here::here("output", "player_stats_2024.rds")

if (file.exists(prior_stats_path)) {
  prior_stats <- readRDS(prior_stats_path)
  cat("Loaded prior season stats from cache.\n")
} else {
  cat("player_stats_2024.rds not found -- building from nflreadr...\n")
  weekly_2024 <- nflreadr::load_player_stats(seasons = 2024) %>%
    dplyr::filter(
      position %in% c("QB", "RB", "WR", "TE"),
      season_type == "REG"
    )

  prior_stats <- weekly_2024 %>%
    dplyr::group_by(player_id, player_display_name, position) %>%
    dplyr::summarise(
      games_played          = dplyr::n_distinct(week),
      total_ppr             = sum(fantasy_points_ppr, na.rm = TRUE),
      avg_ppr_per_game      = total_ppr / games_played,
      avg_pass_epa_per_game = sum(passing_epa, na.rm = TRUE) / games_played,
      avg_rush_epa_per_game = sum(rushing_epa, na.rm = TRUE) / games_played,
      .groups = "drop"
    ) %>%
    dplyr::rename(player_name = player_display_name) %>%
    dplyr::select(player_id, player_name, position, games_played,
                  avg_ppr_per_game, avg_pass_epa_per_game, avg_rush_epa_per_game)

  saveRDS(prior_stats, prior_stats_path)
  cat(glue("Saved {nrow(prior_stats)} players to {prior_stats_path}\n"))
}
feature_matrix  <- readRDS(FEATURE_MATRIX_PATH) %>%
  dplyr::filter(week <= NFL_REGULAR_SEASON_MAX_WEEK)
ensemble_models <- load_ensemble_models()
boom_bust_models <- load_boom_bust_models()

# Generate preseason prior (needed for plots 2 and 3)
preseason_proj <- generate_preseason_projections(
  prior_season_stats = prior_stats,
  prior_season       = 2024
)

# Build week-14 projection for comparison plot
# NOTE: In production this would use the real backtest loop.
# Here we run it inline for the visual.
cat("Running update loop through week 14 (this takes ~2 minutes)...\n")

proj <- preseason_proj
bye_schedule <- c(
  "00-0030506" = 11L,
  "00-0033873" = 9L,
  "00-0035676" = 7L
)

for (wk in 1:14) {
  feat_wk <- feature_matrix %>% dplyr::filter(week == wk)
  if (nrow(feat_wk) == 0) next
  bye_wk <- names(bye_schedule[bye_schedule == wk])
  proj <- update_weekly_projections(
    prior_projections = proj,
    current_week      = wk,
    ensemble_models   = ensemble_models,
    feature_matrix    = feat_wk,
    bye_week_players  = bye_wk
  )
}

cat("Update loop complete.\n\n")

# ==============================================================================
# VISUALIZATION 1: Prior Weight Decay Curve
# ==============================================================================
# Shows how the system transitions from prior to observed data across the season.
# Annotated with NFL season phases: early (1-3), mid (4-8), late (9-14),
# playoff push (15-18).
# No density issue -- this is a single smooth curve, no labels needed.

cat("Creating Visualization 1: Prior Weight Decay Curve...\n")

weight_df <- tibble::tibble(
  week            = 1:NFL_REGULAR_SEASON_MAX_WEEK,
  prior_weight    = purrr::map_dbl(1:NFL_REGULAR_SEASON_MAX_WEEK, compute_prior_weight),
  observed_weight = 1 - prior_weight
) %>%
  tidyr::pivot_longer(
    cols      = c(prior_weight, observed_weight),
    names_to  = "component",
    values_to = "weight"
  ) %>%
  dplyr::mutate(
    component = dplyr::case_when(
      component == "prior_weight"    ~ "Prior (preseason)",
      component == "observed_weight" ~ "Observed (ensemble)"
    )
  )

# Season phase annotations
phases <- tibble::tibble(
  xmin  = c(1, 4, 9, 15),
  xmax  = c(3, 8, 14, 18),
  label = c("Early\n(prior\ndominates)",
            "Mid\nseason",
            "Late season\n(observed\ndominates)",
            "Playoff\npush"),
  y     = c(0.95, 0.65, 0.35, 0.10)
)

p1 <- ggplot(weight_df, aes(x = week, y = weight, colour = component)) +
  # Phase shading
  annotate("rect", xmin = 1,  xmax = 3.5,  ymin = 0, ymax = 1,
           fill = "#F5F5F5", alpha = 0.6) +
  annotate("rect", xmin = 8.5, xmax = 14.5, ymin = 0, ymax = 1,
           fill = "#EAF4FB", alpha = 0.4) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 2.5, alpha = 0.8) +
  # Crossover annotation (where observed weight overtakes prior)
  geom_vline(xintercept = 9.4, linetype = "dashed",
             colour = "#999999", linewidth = 0.8) +
  annotate("text", x = 9.6, y = 0.52,
           label = "Crossover\n(~week 9)",
           hjust = 0, size = 3.2, colour = "#666666") +
  scale_x_continuous(
    breaks = c(1, 3, 5, 7, 9, 11, 13, 15, 17, 18),
    labels = c(1, 3, 5, 7, 9, 11, 13, 15, 17, 18)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_colour_manual(
    values = c("Prior (preseason)" = "#C0392B",
               "Observed (ensemble)" = "#2980B9")
  ) +
  labs(
    title    = "Bayesian Prior Weight Decay Across the NFL Regular Season",
    subtitle = glue(
      "Prior dominates early season (weeks 1-3); observed data dominates by week 9+. ",
      "Scope: weeks 1-{NFL_REGULAR_SEASON_MAX_WEEK} (regular season only)."
    ),
    x        = "NFL Week",
    y        = "Weight in Posterior Projection",
    colour   = NULL,
    caption  = "Data: nflfastR | Analysis: NFL Analytics Toolkit | Week 12"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 10, colour = "#555555"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  here::here("output", "plots", "week12_prior_weight_decay.png"),
  p1, width = 10, height = 6, dpi = 300
)
cat("  Saved: output/plots/week12_prior_weight_decay.png\n\n")

# ==============================================================================
# VISUALIZATION 2: Preseason vs Week-14 Projection Shift
# ==============================================================================
# Shows how individual player projections shifted from the preseason prior
# to the week-14 posterior. Large shifts indicate players who dramatically
# outperformed or underperformed their prior.
#
# NFL context: The players furthest from the diagonal are the most
# interesting -- they represent the system learning something the preseason
# prior missed (injury returns, role changes, scheme changes).

cat("Creating Visualization 2: Projection Shift (Preseason vs Week 14)...\n")

shift_df <- preseason_proj %>%
  dplyr::select(player_id, player_name, position,
                preseason_proj = projected_ppr_per_game) %>%
  dplyr::inner_join(
    proj %>%
      dplyr::select(player_id, position, wk14_proj = projected_ppr_per_game),
    by = c("player_id", "position")
  ) %>%
  dplyr::mutate(
    shift     = wk14_proj - preseason_proj,
    shift_abs = abs(shift),
    direction = dplyr::if_else(shift >= 0, "Increased", "Decreased")
  ) %>%
  dplyr::filter(!is.na(preseason_proj), !is.na(wk14_proj))

# Label only the most-shifted players (top 5 per direction per position)
label_threshold <- shift_df %>%
  dplyr::group_by(position) %>%
  dplyr::slice_max(order_by = shift_abs, n = 5) %>%
  dplyr::pull(player_id)

shift_labelled <- shift_df %>%
  dplyr::mutate(
    label = dplyr::if_else(player_id %in% label_threshold, player_name, NA_character_)
  )

p2 <- ggplot(shift_labelled,
             aes(x = preseason_proj, y = wk14_proj,
                 colour = direction)) +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed", colour = "#999999", linewidth = 0.8) +
  geom_point(alpha = 0.65, size = 2.0) +
  ggrepel::geom_text_repel(
    aes(label = label),
    size = 3.0, max.overlaps = 12, na.rm = TRUE,
    segment.colour = "#AAAAAA", segment.size = 0.3
  ) +
  scale_colour_manual(
    values = c("Increased" = "#27AE60", "Decreased" = "#E74C3C")
  ) +
  facet_wrap(~position, scales = "free") +
  labs(
    title    = "Projection Shift: Preseason Prior vs Week 14 Posterior",
    subtitle = paste(
      "Points above the diagonal improved relative to preseason expectations.",
      "Volume metrics carry 70% of the prior weight (Week 4 finding).",
      sep = "\n"
    ),
    x        = "Preseason Projected PPR/Game",
    y        = "Week 14 Projected PPR/Game",
    colour   = "Direction",
    caption  = "Data: nflfastR | Analysis: NFL Analytics Toolkit | Week 12"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(size = 9, colour = "#555555"),
    legend.position = "bottom",
    strip.text      = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# ggrepel needed for labels -- degrade gracefully if not installed
if (!requireNamespace("ggrepel", quietly = TRUE)) {
  p2 <- p2 + geom_text(aes(label = label), size = 2.8, na.rm = TRUE,
                        hjust = -0.1, vjust = 0.5)
  warning("ggrepel not installed -- using geom_text, labels may overlap.")
}

ggsave(
  here::here("output", "plots", "week12_projection_shift.png"),
  p2, width = 12, height = 6, dpi = 300
)
cat("  Saved: output/plots/week12_projection_shift.png\n\n")

# ==============================================================================
# VISUALIZATION 3: Backtest RMSE by Week and Position
# ==============================================================================
# The core validation plot. Three lines per position: ensemble, last week
# actual, season-to-date average. Ensemble should converge toward or below
# baselines by late season (weeks 9+).
#
# If it does not, that is a documented finding. The plot makes it visible.

cat("Creating Visualization 3: Backtest RMSE by Week...\n")

backtest_results <- backtest_projections(
  feature_matrix    = feature_matrix,
  preseason_stats   = prior_stats,
  ensemble_models   = ensemble_models,
  boom_bust_models  = boom_bust_models,
  backtest_season   = 2024,
  bye_week_schedule = bye_schedule
)

rmse_long <- backtest_results$weekly_errors %>%
  tidyr::pivot_longer(
    cols      = c(rmse_ensemble, rmse_last_week, rmse_season_avg),
    names_to  = "model",
    values_to = "rmse"
  ) %>%
  dplyr::mutate(
    model = dplyr::case_when(
      model == "rmse_ensemble"   ~ "Ensemble (Week 11)",
      model == "rmse_last_week"  ~ "Baseline: Last Week Actual",
      model == "rmse_season_avg" ~ "Baseline: Season Avg to Date"
    )
  )

# Week 4 finding annotation: volume metrics are strong -- annotate where
# season-avg baseline is most competitive (early season, high prior weight)
p3 <- ggplot(rmse_long, aes(x = week, y = rmse, colour = model)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.0, alpha = 0.8) +
  # Highlight late season where ensemble should win
  annotate("rect", xmin = 8.5, xmax = 18.5, ymin = 0, ymax = Inf,
           fill = "#EAF4FB", alpha = 0.3) +
  annotate("text", x = 13, y = Inf,
           label = "Observed data dominates\n(prior weight < 50%)",
           vjust = 1.5, size = 3.0, colour = "#2980B9") +
  scale_x_continuous(
    breaks = c(1, 3, 5, 7, 9, 11, 13, 15, 17, 18)
  ) +
  scale_colour_manual(
    values = c(
      "Ensemble (Week 11)"              = "#2980B9",
      "Baseline: Last Week Actual"      = "#E67E22",
      "Baseline: Season Avg to Date"    = "#8E44AD"
    )
  ) +
  facet_wrap(~position, scales = "free_y") +
  labs(
    title    = "Projection System Backtest: RMSE by Week vs Naive Baselines",
    subtitle = paste(
      "Ensemble should match or beat both baselines by week 9+ (late season).",
      "Early-season RMSE is expected to be high -- prior dominates, wide CIs are correct.",
      sep = "\n"
    ),
    x        = "NFL Week (Regular Season: Weeks 1-18)",
    y        = "RMSE (PPR Points)",
    colour   = NULL,
    caption  = paste(
      glue(
        "Season {backtest_results$summary_stats$backtest_season} backtest | ",
        "Late-season ensemble vs last-week: ",
        "{dplyr::if_else(backtest_results$summary_stats$beats_last_week, 'BEATS', 'DOES NOT BEAT')} | ",
        "vs season-avg: ",
        "{dplyr::if_else(backtest_results$summary_stats$beats_season_avg, 'BEATS', 'DOES NOT BEAT')}"
      ),
      "Data: nflfastR | Analysis: NFL Analytics Toolkit | Week 12",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(size = 9, colour = "#555555"),
    legend.position = "bottom",
    strip.text      = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  here::here("output", "plots", "week12_backtest_rmse.png"),
  p3, width = 13, height = 7, dpi = 300
)
cat("  Saved: output/plots/week12_backtest_rmse.png\n\n")

# ==============================================================================
# VISUALIZATION 4: Interactive -- Projection Report Scatter (Plotly)
# ==============================================================================
# Boom probability vs projected PPR/game, coloured by matchup flag,
# sized by bust probability, labelled by player name on hover.
#
# WHY INTERACTIVE:
# 3 positions x ~30 startable players per position = 90+ labeled points.
# Density threshold (50+ labeled points) requires interactive HTML per
# project standards (Week 8+).
#
# HOVER TOOLTIPS include:
#   - Player name
#   - Projected PPR/game with 80% CI
#   - Boom probability and bust probability
#   - Matchup flag
#   - ROS projection
#   - Sample size (remaining_games)
#
# Static PNG is saved first -- interactive is additive, not a replacement.

cat("Creating Visualization 4: Interactive Projection Report Scatter...\n")

# Generate report at week 14
feat_wk14 <- feature_matrix %>% dplyr::filter(week == 14)

# Derive opponent difficulty from feature matrix (defensive quality of week-14 opponents)
opp_diff_wk14 <- derive_opponent_difficulty(feature_matrix, target_week = 14L)

ros_wk14 <- generate_ros_projections(
  current_projections = proj,
  current_week        = 14L,
  bye_week_schedule   = bye_schedule,
  opponent_difficulty = opp_diff_wk14
)

report_wk14 <- create_projection_report(
  weekly_projections  = proj,
  ros_projections     = ros_wk14,
  boom_bust_models    = boom_bust_models,
  feature_matrix      = feat_wk14,
  roster_season       = 2025L,
  export_csv          = FALSE,
  opponent_difficulty = opp_diff_wk14
)

# Filter to startable players only (non-bye, qualified PPR projection)
plot_data <- report_wk14 %>%
  dplyr::filter(
    !is_bye_week,
    !is.na(boom_probability),
    !is.na(bust_probability),
    projected_ppr_per_game >= 5
  ) %>%
  dplyr::mutate(
    matchup_colour = dplyr::case_when(
      matchup_flag == "favorable" ~ "#27AE60",
      matchup_flag == "tough"     ~ "#E74C3C",
      TRUE                        ~ "#7F8C8D"
    ),
    hover_text = glue(
      "<b>{player_name}</b> ({player_position})<br>",
      "Projected: {round(projected_ppr_per_game, 1)} pts ",
      "({round(projected_ppr_lower_80, 1)}-{round(projected_ppr_upper_80, 1)} 80% CI)<br>",
      "Boom prob: {scales::percent(boom_probability, accuracy = 1)}<br>",
      "Bust prob: {scales::percent(bust_probability, accuracy = 1)}<br>",
      "Matchup: {matchup_flag}<br>",
      "ROS total: {round(projected_ros_ppr, 1)} pts ({remaining_games} games)<br>",
      "Recommendation: {boom_bust_recommendation}"
    )
  )

cat(glue(
  "  {nrow(plot_data)} players in interactive plot ",
  "(density threshold met: {nrow(plot_data)} > 50).\n\n"
))

# --- Static PNG first (always required -- interactive is additive) -----------
p4_static <- ggplot(
  plot_data,
  aes(x = projected_ppr_per_game, y = boom_probability,
      colour = matchup_flag, size = bust_probability)
) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0.35, linetype = "dashed",
             colour = "#27AE60", linewidth = 0.7) +
  geom_vline(xintercept = median(plot_data$projected_ppr_per_game, na.rm = TRUE),
             linetype = "dashed", colour = "#999999", linewidth = 0.7) +
  annotate("text", x = Inf, y = 0.36,
           label = "Boom threshold (35%)",
           hjust = 1.05, vjust = -0.3, size = 3.0, colour = "#27AE60") +
  scale_colour_manual(
    values = c("favorable" = "#27AE60", "neutral" = "#7F8C8D", "tough" = "#E74C3C"),
    name   = "Matchup"
  ) +
  scale_size_continuous(
    range = c(1.5, 6),
    name  = "Bust probability"
  ) +
  facet_wrap(~position, scales = "free_x") +
  labs(
    title    = "Week 14 Projections: Boom Probability vs Projected PPR",
    subtitle = paste(
      "Point size = bust probability. Favourable matchups (green) generally have lower bust risk.",
      "Interactive HTML version includes full player details on hover.",
      sep = "\n"
    ),
    x        = "Projected PPR Points per Game (Week 14)",
    y        = "Boom Probability (top 10% at position)",
    caption  = paste(
      "Week 14 | Regular season only (weeks 1-18) | 90+ players -- see HTML for labels",
      "Data: nflfastR | Analysis: NFL Analytics Toolkit | Week 12",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(size = 9, colour = "#555555"),
    strip.text      = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave(
  here::here("output", "plots", "week12_projection_report.png"),
  p4_static, width = 13, height = 7, dpi = 300
)
cat("  Saved: output/plots/week12_projection_report.png (static)\n\n")

# --- Interactive HTML (plotly) -----------------------------------------------
if (requireNamespace("plotly", quietly = TRUE) &&
    requireNamespace("htmlwidgets", quietly = TRUE)) {

  p4_interactive <- plotly::plot_ly(
    data    = plot_data,
    x       = ~projected_ppr_per_game,
    y       = ~boom_probability,
    color   = ~matchup_flag,
    size    = ~bust_probability,
    text    = ~hover_text,
    type    = "scatter",
    mode    = "markers",
    colors  = c(
      "favorable" = "#27AE60",
      "neutral"   = "#7F8C8D",
      "tough"     = "#E74C3C"
    ),
    marker  = list(
      opacity   = 0.75,
      sizemode  = "diameter",
      sizeref   = 0.3,
      sizemin   = 5,
      line      = list(width = 0.5, color = "white")
    ),
    hoverinfo = "text",
    # One trace per position (legend toggles visibility)
    split   = ~position
  ) %>%
    plotly::layout(
      title = list(
        text = paste(
          "<b>Week 14 Projection Report: Boom Probability vs Projected PPR</b>",
          "<br><sup>Size = bust probability | Colour = matchup difficulty | ",
          "Toggle positions via legend | Regular season weeks 1-18 only</sup>"
        ),
        font = list(size = 14)
      ),
      xaxis = list(
        title     = "Projected PPR Points per Game (Week 14)",
        gridcolor = "#EEEEEE"
      ),
      yaxis = list(
        title      = "Boom Probability (top 10% at position)",
        tickformat = ".0%",
        gridcolor  = "#EEEEEE",
        range      = c(0, 1)
      ),
      # Boom threshold line
      shapes = list(
        list(
          type    = "line",
          x0      = 0, x1 = 1, xref = "paper",
          y0      = 0.35, y1 = 0.35,
          line    = list(color = "#27AE60", dash = "dash", width = 1.5)
        )
      ),
      annotations = list(
        list(
          x         = 1, xref = "paper",
          y         = 0.36, yref = "y",
          text      = "Boom threshold (35%)",
          xanchor   = "right",
          showarrow = FALSE,
          font      = list(size = 11, color = "#27AE60")
        )
      ),
      legend = list(
        title        = list(text = "<b>Position / Matchup</b>"),
        orientation  = "v",
        x            = 1.02, y = 0.5
      ),
      paper_bgcolor = "white",
      plot_bgcolor  = "white",
      font          = list(family = "Arial, sans-serif")
    ) %>%
    plotly::config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d")
    )

  html_path <- here::here(
    "output", "plots", "week12_projection_report_interactive.html"
  )
  htmlwidgets::saveWidget(
    p4_interactive,
    file       = html_path,
    selfcontained = TRUE,   # No server required -- fully portable HTML
    title      = "NFL Analytics Toolkit - Week 14 Projection Report"
  )
  cat(glue("  Saved: output/plots/week12_projection_report_interactive.html\n\n"))
  cat("  NOTE: HTML is for exploration only. Static PNG is the shareable artifact.\n\n")

} else {
  cat("  plotly or htmlwidgets not installed -- skipping interactive HTML.\n")
  cat("  Install with: install.packages(c('plotly', 'htmlwidgets'))\n\n")
}

# ==============================================================================
# 2026 PRESEASON PROJECTIONS (forward-looking visuals)
# ==============================================================================
# Uses 2025 season as the prior to generate 2026 preseason projections.
# These are the most actionable plots for fantasy football players --
# they show who the system likes heading into next season.
#
# TEs are routed through the receiver model (same position_group), then
# real positions are restored for display.
# ==============================================================================

cat("Building 2026 preseason projections for forward-looking visuals...\n")

prior_2025_path <- here::here("output", "player_stats_2025.rds")

if (file.exists(prior_2025_path)) {
  prior_2025 <- readRDS(prior_2025_path)
  cat("  Loaded 2025 prior stats from cache.\n")
} else {
  cat("  Building 2025 prior stats from nflreadr...\n")
  weekly_2025 <- nflreadr::load_player_stats(seasons = 2025) %>%
    dplyr::filter(
      position %in% c("QB", "RB", "WR", "TE"),
      season_type == "REG"
    )

  prior_2025 <- weekly_2025 %>%
    dplyr::group_by(player_id, player_display_name, position) %>%
    dplyr::summarise(
      games_played          = dplyr::n_distinct(week),
      total_ppr             = sum(fantasy_points_ppr, na.rm = TRUE),
      avg_ppr_per_game      = total_ppr / games_played,
      avg_pass_epa_per_game = sum(passing_epa, na.rm = TRUE) / games_played,
      avg_rush_epa_per_game = sum(rushing_epa, na.rm = TRUE) / games_played,
      .groups = "drop"
    ) %>%
    dplyr::rename(player_name = player_display_name) %>%
    dplyr::select(player_id, player_name, position, games_played,
                  avg_ppr_per_game, avg_pass_epa_per_game, avg_rush_epa_per_game)

  saveRDS(prior_2025, prior_2025_path)
  cat(glue("  Saved {nrow(prior_2025)} players to cache.\n"))
}

# Save real positions, remap TE -> WR for model routing
real_pos_2025 <- prior_2025 %>% dplyr::select(player_id, real_position = position)

prior_2025_mapped <- prior_2025 %>%
  dplyr::mutate(position = dplyr::if_else(position == "TE", "WR", position))

proj_2026 <- generate_preseason_projections(
  prior_2025_mapped,
  prior_season    = 2025,
  min_prior_games = 4L
)

# Restore real positions
proj_2026 <- proj_2026 %>%
  dplyr::left_join(real_pos_2025, by = "player_id") %>%
  dplyr::mutate(
    model_position = position,
    position       = real_position
  ) %>%
  dplyr::select(-real_position)

cat(glue("  2026 projections: {nrow(proj_2026)} qualified players\n\n"))


# ==============================================================================
# VISUALIZATION 5: 2026 Top Projected Players by Position (Lollipop + CI)
# ==============================================================================
# Fantasy football players want to know: who should I target in drafts?
# Lollipop chart with 95% CI bands shows both the point projection and the
# uncertainty. Faceted by position so each group is on its own scale.
#
# Selection: top 10 QB, top 15 RB, top 15 WR, top 10 TE.

cat("Creating Visualization 5: 2026 Top Projected Players by Position...\n")

# Build the display data with rank filtering per position
top_counts <- c("QB" = 10, "RB" = 15, "WR" = 15, "TE" = 10)

proj_top <- purrr::map_dfr(names(top_counts), function(pos) {
  proj_2026 %>%
    dplyr::filter(position == pos) %>%
    dplyr::arrange(dplyr::desc(projected_ppr_per_game)) %>%
    dplyr::slice_head(n = top_counts[pos]) %>%
    dplyr::mutate(rank = dplyr::row_number())
})

# Clean up names for display (first initial + last name if too long)
proj_top <- proj_top %>%
  dplyr::mutate(
    display_name = dplyr::case_when(
      nchar(player_name) > 15 ~ paste0(substr(player_name, 1, 2), ".",
                                        sub("^\\S+\\s+", "", player_name)),
      TRUE ~ player_name
    ),
    # Confidence width for colour scale
    ci_width = projected_ppr_upper_95 - projected_ppr_lower_95
  )

# Position colours
pos_colours <- c("QB" = "#E41A1C", "RB" = "#377EB8", "WR" = "#4DAF4A", "TE" = "#FF7F00")

p5 <- ggplot(proj_top, aes(x = projected_ppr_per_game,
                             y = reorder(display_name, projected_ppr_per_game))) +
  geom_errorbarh(aes(xmin = projected_ppr_lower_95,
                      xmax = projected_ppr_upper_95),
                  height = 0.3, colour = "grey60", linewidth = 0.5) +
  geom_point(aes(colour = position), size = 3) +
  geom_text(aes(label = round(projected_ppr_per_game, 1)),
            hjust = -0.4, size = 2.8, colour = "grey30") +
  facet_wrap(~ position, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = pos_colours, guide = "none") +
  labs(
    title    = "2026 Preseason Projections: Top Players by Position",
    subtitle = "PPR points per game with 95% confidence intervals | Prior: 2025 season",
    x        = "Projected PPR per Game",
    y        = NULL,
    caption  = "Data: nflfastR | Analysis: NFL Analytics Toolkit\nNote: Projections based on 2025 performance only. Does not account for trades, free agency, or draft picks."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    plot.caption     = element_text(colour = "grey50", size = 8, hjust = 0),
    strip.text       = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(size = 9)
  )

ggsave(
  here::here("output", "plots", "week12_2026_top_projections.png"),
  p5, width = 14, height = 10, dpi = 300
)
cat("  Saved: output/plots/week12_2026_top_projections.png\n\n")


# ==============================================================================
# VISUALIZATION 6: 2026 Projection Confidence vs Production (Interactive)
# ==============================================================================
# Fantasy players need to know which projections to trust. This scatter plots
# projected PPR (x) vs confidence interval width (y) -- players in the
# bottom-right are high-value high-certainty. Players in the top-right are
# high-upside but volatile (exactly the kind of information that drives
# draft strategy in best-ball and dynasty leagues).
#
# Density: 4 positions x ~50 players = 200+ points -> interactive HTML required.

cat("Creating Visualization 6: 2026 Projection Confidence Scatter...\n")

proj_scatter <- proj_2026 %>%
  dplyr::mutate(
    ci_width = projected_ppr_upper_95 - projected_ppr_lower_95,
    # Quadrant labels for annotation
    quadrant = dplyr::case_when(
      projected_ppr_per_game >= median(projected_ppr_per_game) &
        ci_width <= median(ci_width) ~ "High Floor (target)",
      projected_ppr_per_game >= median(projected_ppr_per_game) &
        ci_width > median(ci_width)  ~ "High Upside (volatile)",
      projected_ppr_per_game < median(projected_ppr_per_game) &
        ci_width <= median(ci_width) ~ "Low Ceiling (avoid)",
      TRUE                            ~ "Boom or Bust"
    )
  )

# Position colours for the static version
p6_static <- ggplot(proj_scatter,
                     aes(x = projected_ppr_per_game, y = ci_width,
                         colour = position)) +
  geom_point(aes(size = prior_games_played), alpha = 0.6) +
  # Quadrant medians
  geom_vline(xintercept = median(proj_scatter$projected_ppr_per_game),
             linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  geom_hline(yintercept = median(proj_scatter$ci_width),
             linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  # Quadrant labels
  annotate("text", x = max(proj_scatter$projected_ppr_per_game) * 0.95,
           y = min(proj_scatter$ci_width) + 0.5,
           label = "High Floor\n(draft targets)", colour = "#2E7D32",
           size = 3, fontface = "bold", hjust = 1) +
  annotate("text", x = max(proj_scatter$projected_ppr_per_game) * 0.95,
           y = max(proj_scatter$ci_width) * 0.95,
           label = "High Upside\n(volatile)", colour = "#E65100",
           size = 3, fontface = "bold", hjust = 1) +
  annotate("text", x = min(proj_scatter$projected_ppr_per_game) + 0.5,
           y = min(proj_scatter$ci_width) + 0.5,
           label = "Low Ceiling\n(avoid)", colour = "#B71C1C",
           size = 3, fontface = "bold", hjust = 0) +
  scale_colour_manual(values = pos_colours) +
  scale_size_continuous(range = c(1.5, 5), name = "Games\nPlayed") +
  labs(
    title    = "2026 Projections: Confidence vs Production",
    subtitle = "Bottom-right quadrant = high value, high certainty | Point size = 2025 games played",
    x        = "Projected PPR per Game",
    y        = "95% CI Width (wider = less certain)",
    colour   = "Position",
    caption  = "Data: nflfastR | Analysis: NFL Analytics Toolkit\nDashed lines = median projection / median CI width"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(colour = "grey40", size = 10),
    plot.caption  = element_text(colour = "grey50", size = 8, hjust = 0),
    legend.position = "right"
  )

ggsave(
  here::here("output", "plots", "week12_2026_confidence_scatter.png"),
  p6_static, width = 12, height = 8, dpi = 300
)
cat("  Saved: output/plots/week12_2026_confidence_scatter.png\n")

# Interactive version -- 200+ points, density threshold triggered
if (requireNamespace("plotly", quietly = TRUE) &&
    requireNamespace("htmlwidgets", quietly = TRUE)) {

  proj_scatter_hover <- proj_scatter %>%
    dplyr::mutate(
      hover_text = glue(
        "<b>{player_name}</b> ({position})",
        "<br>Projected PPR/gm: {round(projected_ppr_per_game, 1)}",
        "<br>95% CI: [{round(projected_ppr_lower_95, 1)}, ",
        "{round(projected_ppr_upper_95, 1)}]",
        "<br>CI Width: {round(ci_width, 1)}",
        "<br>2025 Games: {prior_games_played}",
        "<br>Regression Alpha: {round(regression_alpha, 2)}",
        "<br>Quadrant: {quadrant}"
      )
    )

  p6_interactive <- plotly::plot_ly(
    proj_scatter_hover,
    x      = ~projected_ppr_per_game,
    y      = ~ci_width,
    color  = ~position,
    colors = pos_colours,
    size   = ~prior_games_played,
    sizes  = c(40, 300),
    text   = ~hover_text,
    hoverinfo = "text",
    type   = "scatter",
    mode   = "markers",
    marker = list(opacity = 0.7)
  ) %>%
    plotly::layout(
      title = list(text = "<b>2026 Preseason Projections: Confidence vs Production</b>",
                   font = list(size = 16)),
      xaxis = list(title = "Projected PPR per Game"),
      yaxis = list(title = "95% CI Width (wider = less certain)"),
      legend = list(title = list(text = "<b>Position</b>")),
      # Quadrant lines
      shapes = list(
        list(type = "line",
             x0 = median(proj_scatter$projected_ppr_per_game),
             x1 = median(proj_scatter$projected_ppr_per_game),
             y0 = 0, y1 = max(proj_scatter$ci_width) * 1.05,
             line = list(color = "grey", dash = "dash", width = 1)),
        list(type = "line",
             x0 = 0,
             x1 = max(proj_scatter$projected_ppr_per_game) * 1.05,
             y0 = median(proj_scatter$ci_width),
             y1 = median(proj_scatter$ci_width),
             line = list(color = "grey", dash = "dash", width = 1))
      ),
      paper_bgcolor = "white",
      plot_bgcolor  = "white",
      font          = list(family = "Arial, sans-serif")
    ) %>%
    plotly::config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d")
    )

  html_path_2026 <- here::here(
    "output", "plots", "week12_2026_confidence_interactive.html"
  )
  htmlwidgets::saveWidget(
    p6_interactive,
    file          = html_path_2026,
    selfcontained = TRUE,
    title         = "NFL Analytics Toolkit - 2026 Preseason Projection Confidence"
  )
  cat(glue("  Saved: output/plots/week12_2026_confidence_interactive.html\n\n"))

} else {
  cat("  plotly or htmlwidgets not installed -- skipping interactive HTML.\n\n")
}


# ==============================================================================
# SUMMARY
# ==============================================================================

cat("==========================================================\n")
cat("VISUALIZATION SUMMARY\n")
cat("==========================================================\n\n")

outputs <- c(
  "output/plots/week12_prior_weight_decay.png",
  "output/plots/week12_projection_shift.png",
  "output/plots/week12_backtest_rmse.png",
  "output/plots/week12_projection_report.png",
  "output/plots/week12_projection_report_interactive.html",
  "output/plots/week12_2026_top_projections.png",
  "output/plots/week12_2026_confidence_scatter.png",
  "output/plots/week12_2026_confidence_interactive.html"
)

for (f in outputs) {
  path <- here::here(f)
  exists_flag <- if (file.exists(path)) "EXISTS" else "MISSING"
  cat(glue("  [{exists_flag}] {f}\n\n"))
}

cat("\n")
cat("ANALYTICAL FINDINGS IN THESE PLOTS\n")
cat("-----------------------------------\n")
cat("Plot 1: Prior weight crossover occurs near week 9. Before week 9,\n")
cat("        preseason expectations carry more weight than observed data.\n\n")
cat("Plot 2: Players with the largest projection shifts from preseason are\n")
cat("        those with role changes, injuries, or scheme changes. The system\n")
cat("        captures this through the ensemble feature set.\n\n")
cat("Plot 3: Ensemble RMSE should converge toward or below baselines by\n")
cat("        week 9. If it does not, investigate feature leakage and\n")
cat("        role_stability_flag application (audit checklist issues 4 and 6).\n\n")
cat("Plot 4: Interactive scatter shows the full trade-off space for lineup\n")
cat("        decisions. A player with high boom probability and favorable\n")
cat("        matchup is the optimal start target.\n\n")
cat("Plot 5: 2026 top projections by position. CI width reveals which\n")
cat("        projections carry real conviction vs which are just repeating\n")
cat("        last year's noise. Players with wide CIs and few games played\n")
cat("        have the most regression alpha -- the model trusts them least.\n\n")
cat("Plot 6: Confidence vs production quadrant plot. Bottom-right = high\n")
cat("        value, high certainty (draft targets). Top-right = high upside\n")
cat("        but volatile (best-ball targets). Small points = fewer games\n")
cat("        played = less stable projection.\n\n")
cat("Data: nflfastR | Analysis: NFL Analytics Toolkit | Week 12\n")
