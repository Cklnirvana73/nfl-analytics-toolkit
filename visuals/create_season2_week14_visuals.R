# ==============================================================================
# examples/create_season2_week14_visuals.R
# Season 2, Week 14 -- Dynasty Prospect Scoring Visualizations
# NFL Analytics Toolkit
#
# PURPOSE
#   Generates 7 publication-quality plots from the Week 14 scoring artifacts.
#   Run AFTER run_week14_scoring() has produced all three output CSVs.
#
# PLOTS PRODUCED
#
#   Plot 1  Score Distribution by Position
#           Density overlay: 2015-2023 training set vs 2024-2026 prospects.
#           Shows where current prospects fall relative to the historical baseline.
#           output/plots/s2_week14_score_distribution.png
#
#   Plot 2  Signal Strength by Metric (Dumbbell)
#           Tier 1 (Elite) vs lowest-tier hit rate per metric, faceted by position.
#           Reveals which metrics most reliably separate NFL hits from busts.
#           output/plots/s2_week14_signal_strength.png
#
#   Plot 3  Top 2026 Rookie Rankings
#           Horizontal bar: top 12 per position by score_final, score_v1 overlay.
#           output/plots/s2_week14_top_2026_rookies.png
#
#   Plots 4a-d  Metric Correlation Heatmaps (one per position)
#           Full pairwise Pearson r matrix. High-collinearity pairs outlined.
#           output/plots/s2_week14_corr_heatmap_QB.png
#           output/plots/s2_week14_corr_heatmap_RB.png
#           output/plots/s2_week14_corr_heatmap_WR.png
#           output/plots/s2_week14_corr_heatmap_TE.png
#
# DENSITY THRESHOLD CHECK (50-point rule)
#   Plot 1: density curves, no labeled points -- static only
#   Plot 2: metric names on y-axis (axis labels, not geom_text) -- static only
#   Plot 3: up to 48 labeled bars (12 x 4 positions) -- static only
#   Plots 4a-d: heatmap tiles, no point labels -- static only
#   No interactive HTML required for any plot.
#
# PREREQUISITES
#   data/season2_cache/s2_week14_final_prospect_scores.csv
#   data/season2_cache/s2_week14_tier_reference.csv
#   data/season2_cache/s2_week14_metric_correlations.csv
#
# SCHEMA TAG: s2_w14_v1
# ==============================================================================


library(ggplot2)
library(dplyr)
library(tidyr)
library(glue)
library(here)
library(scales)
library(readr)


# ==============================================================================
# CONFIGURATION
# ==============================================================================

CUT_YEAR   <- 2023L          # Training/prediction split year (matches R/28)
TOP_N      <- 12L            # Players shown per position in Plot 3
OUTPUT_DIR <- here::here("output", "plots")
DPI        <- 300L
PREFIX     <- "s2_week14_"
ATTRIBUTION <- "Data: cfbfastR 2014-2025 | Analysis: NFL Analytics Toolkit S2W14"
DENSITY_THRESHOLD <- 50L

# Position colors -- colorblind-safe palette (Wong 2011)
POS_COLORS <- c(
  QB = "#E69F00",
  RB = "#56B4E9",
  WR = "#009E73",
  TE = "#CC79A7"
)

# Score bucket thresholds and display labels (for Plot 3 bar coloring)
SCORE_BREAKS  <- c(0, 50, 70, 100)
SCORE_LABELS  <- c("Developing (<50)", "Strong (50-69)", "Elite (70+)")
SCORE_COLORS  <- c("Developing (<50)" = "#FFC000",
                   "Strong (50-69)"   = "#92D050",
                   "Elite (70+)"      = "#00B050")

# Human-readable metric labels (full names for Plot 2)
METRIC_LABELS <- c(
  age_centered                    = "Age (centered)",
  breakout_age                    = "Breakout Age",
  seasons_since_breakout          = "Seasons Since Breakout",
  recruiting_rating               = "Recruiting Rating",
  recruiting_stars                = "Recruiting Stars",
  games_played                    = "Games Played",
  cfb_final_season                = "CFB Final Season",
  n_cfb_seasons                   = "CFB Seasons",
  sos_opp_def_epa_per_play        = "SOS (Def EPA/Play)",
  sos_opp_def_success_rate_allowed= "SOS (Def Success Rate)",
  sos_n_opponents                 = "SOS Opponents",
  sos_x_age                       = "SOS x Age (interaction)",
  pass_att_pg                     = "Pass Att/G",
  pass_yd_pg                      = "Pass Yd/G",
  pass_td_pg                      = "Pass TD/G",
  int_pg                          = "INT/G",
  completion_pct                  = "Completion %",
  pass_epa_per_attempt            = "Pass EPA/Att",
  pass_epa_slope                  = "Pass EPA Slope",
  success_rate                    = "Success Rate",
  rush_att_pg                     = "Rush Att/G",
  rush_yd_pg                      = "Rush Yd/G",
  rush_td_pg                      = "Rush TD/G",
  rush_epa_per_attempt            = "Rush EPA/Att",
  rush_epa_slope                  = "Rush EPA Slope",
  rush_to_rec_ratio               = "Rush/Rec Ratio",
  rec_role_share                  = "Rec Role Share",
  rec_yd_pg                       = "Rec Yd/G",
  rec_td_pg                       = "Rec TD/G",
  rec_epa_per_target              = "Rec EPA/Tgt",
  rec_yd_per_team_pass_att        = "Rec Yd/Team Pass Att",
  rec_epa_slope                   = "Rec EPA Slope",
  tgt_pg                          = "Targets/G",
  catch_rate                      = "Catch Rate",
  forty                           = "40-Yard Dash",
  vertical                        = "Vertical Jump",
  broad_jump                      = "Broad Jump",
  wt                              = "Weight (lbs)"
)

# Short labels for heatmap axes (abbreviated to fit)
SHORT_LABELS <- c(
  age_centered                    = "Age",
  breakout_age                    = "Brkout Age",
  seasons_since_breakout          = "Brkout Recency",
  recruiting_rating               = "Recruiting",
  recruiting_stars                = "Stars",
  games_played                    = "Games",
  cfb_final_season                = "Final Season",
  n_cfb_seasons                   = "CFB Seasons",
  sos_opp_def_epa_per_play        = "SOS EPA",
  sos_opp_def_success_rate_allowed= "SOS SR",
  sos_n_opponents                 = "SOS N",
  sos_x_age                       = "SOS x Age",
  pass_att_pg                     = "Pass Att",
  pass_yd_pg                      = "Pass Yd",
  pass_td_pg                      = "Pass TD",
  int_pg                          = "INT",
  completion_pct                  = "Comp%",
  pass_epa_per_attempt            = "Pass EPA",
  pass_epa_slope                  = "Pass EPA Slope",
  success_rate                    = "Succ Rate",
  rush_att_pg                     = "Rush Att",
  rush_yd_pg                      = "Rush Yd",
  rush_td_pg                      = "Rush TD",
  rush_epa_per_attempt            = "Rush EPA",
  rush_epa_slope                  = "Rush EPA Slope",
  rush_to_rec_ratio               = "Rush/Rec",
  rec_role_share                  = "Rec Share",
  rec_yd_pg                       = "Rec Yd",
  rec_td_pg                       = "Rec TD",
  rec_epa_per_target              = "Rec EPA",
  rec_yd_per_team_pass_att        = "Rec/TmPA",
  rec_epa_slope                   = "Rec EPA Slope",
  tgt_pg                          = "Tgt/G",
  catch_rate                      = "Catch%",
  forty                           = "40yd",
  vertical                        = "Vertical",
  broad_jump                      = "Broad Jmp",
  wt                              = "Weight"
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
      plot.title       = element_text(face = "bold", size = base_size + 2, hjust = 0),
      plot.subtitle    = element_text(size = base_size, color = "gray40", hjust = 0),
      plot.caption     = element_text(size = 8, color = "gray55", hjust = 1),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray92", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold", size = base_size),
      strip.background = element_rect(fill = "gray95", color = NA),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", size = 10),
      plot.margin      = margin(12, 16, 8, 12)
    )
}

theme_heatmap <- function(base_size = 9) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 3, hjust = 0),
      plot.subtitle    = element_text(size = base_size + 1, color = "gray40", hjust = 0),
      plot.caption     = element_text(size = 7, color = "gray55", hjust = 1),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid       = element_blank(),
      axis.text.x      = element_text(angle = 45, hjust = 1, size = base_size - 1),
      axis.text.y      = element_text(size = base_size - 1),
      axis.title       = element_blank(),
      legend.position  = "right",
      legend.key.height = unit(1.2, "cm"),
      plot.margin      = margin(10, 10, 10, 10)
    )
}


# ==============================================================================
# DATA LOADING
# ==============================================================================

scores_path <- here::here("data", "season2_cache", "s2_week14_final_prospect_scores.csv")
tiers_path  <- here::here("data", "season2_cache", "s2_week14_tier_reference.csv")
corr_path   <- here::here("data", "season2_cache", "s2_week14_metric_correlations.csv")

for (p in c(scores_path, tiers_path, corr_path)) {
  if (!file.exists(p)) {
    stop(
      "Required artifact missing: ", p,
      "\nRun run_week14_scoring() first to generate scoring artifacts.",
      call. = FALSE
    )
  }
}

cat("Loading scoring artifacts...\n")
scores <- read_csv(scores_path, show_col_types = FALSE)
tiers  <- read_csv(tiers_path,  show_col_types = FALSE)
corr   <- read_csv(corr_path,   show_col_types = FALSE)
cat(glue("  scores: {nrow(scores)} rows\n"))
cat(glue("  tiers:  {nrow(tiers)} rows\n"))
cat(glue("  corr:   {nrow(corr)} rows\n\n"))

# Overall training hit rate -- computed from data, never hardcoded
train_scores     <- filter(scores, is_training_player == TRUE, !is.na(is_hit))
overall_hit_rate <- mean(train_scores$is_hit, na.rm = TRUE) * 100


# ==============================================================================
# PLOT 1: Score Distribution by Position
# Question: Where do 2024-2026 prospects cluster vs the historical training set?
# ==============================================================================

cat("Building Plot 1: Score distribution by position...\n")

score_dist_data <- scores |>
  mutate(
    player_class = if_else(
      draft_class_type == "training",
      glue("Training Set ({TRAINING_FLOOR}-{CUT_YEAR})",
           TRAINING_FLOOR = 2015L, CUT_YEAR = CUT_YEAR),
      "Prospects (2024-2026)"
    ),
    position = factor(position, levels = c("QB", "RB", "WR", "TE"))
  )

# Compute median score for prediction players per position (for reference lines)
pred_medians <- score_dist_data |>
  filter(player_class == "Prospects (2024-2026)") |>
  group_by(position) |>
  summarise(med_score = median(score_final, na.rm = TRUE), .groups = "drop")

p1 <- ggplot(score_dist_data, aes(x = score_final, fill = player_class,
                                   color = player_class)) +
  geom_density(alpha = 0.35, linewidth = 0.8) +
  geom_vline(
    data = pred_medians,
    aes(xintercept = med_score),
    linetype = "dashed", color = "gray30", linewidth = 0.5
  ) +
  geom_text(
    data = pred_medians,
    aes(x = med_score, y = Inf,
        label = glue("Median:\n{round(med_score, 1)}")),
    hjust = -0.1, vjust = 1.3, size = 2.8, color = "gray30", inherit.aes = FALSE
  ) +
  facet_wrap(~position, nrow = 2, ncol = 2) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  scale_fill_manual(values = c(
    "Prospects (2024-2026)"  = "#2E75B6",
    "Training Set (2015-2023)" = "gray60"
  )) +
  scale_color_manual(values = c(
    "Prospects (2024-2026)"  = "#1F4E79",
    "Training Set (2015-2023)" = "gray45"
  )) +
  labs(
    title    = "Prospect Score Distribution by Position",
    subtitle = "2024-2026 prospects (blue) vs 2015-2023 training population (gray)\nDashed line = 2024-2026 median per position",
    x        = "Final Score (0-100)",
    y        = "Density",
    fill     = NULL,
    color    = NULL,
    caption  = ATTRIBUTION
  ) +
  theme_toolkit() +
  theme(legend.position = "top")

p1_path <- file.path(OUTPUT_DIR, glue("{PREFIX}score_distribution.png"))
ggsave(p1_path, plot = p1, width = 10, height = 7, dpi = DPI, bg = "white")
cat(glue("  Saved: {p1_path}\n"))


# ==============================================================================
# PLOT 2: Signal Strength Dumbbell (Tier 1 vs lowest-tier hit rates)
# Question: Which metrics most reliably separate NFL hits from busts?
# ==============================================================================

cat("Building Plot 2: Signal strength dumbbell...\n")

# Get highest and lowest hit-rate tier per metric/position
tier_extremes <- tiers |>
  group_by(position, metric_name) |>
  summarise(
    elite_hr  = max(hit_rate,  na.rm = TRUE) * 100,
    elite_n   = n_in_bin[which.max(hit_rate)],
    low_hr    = min(hit_rate,  na.rm = TRUE) * 100,
    low_n     = n_in_bin[which.min(hit_rate)],
    .groups   = "drop"
  ) |>
  mutate(
    gap          = elite_hr - low_hr,
    metric_label = dplyr::coalesce(METRIC_LABELS[metric_name], metric_name),
    position     = factor(position, levels = c("QB", "RB", "WR", "TE"))
  ) |>
  # Sort within each position by gap descending (largest gap = most predictive)
  group_by(position) |>
  arrange(gap, .by_group = TRUE) |>
  mutate(metric_label = factor(metric_label, levels = unique(metric_label))) |>
  ungroup()

# Pivot to long for geom_point layer
tier_long <- tier_extremes |>
  select(position, metric_label, elite_hr, low_hr) |>
  pivot_longer(
    cols      = c(elite_hr, low_hr),
    names_to  = "tier_type",
    values_to = "hit_rate_pct"
  ) |>
  mutate(
    tier_type = if_else(tier_type == "elite_hr", "Elite Signal (Tier 1)", "Lowest Tier")
  )

p2 <- ggplot(tier_extremes,
             aes(y = metric_label)) +
  # Reference line at overall hit rate
  geom_vline(
    xintercept = overall_hit_rate,
    linetype   = "dashed",
    color      = "gray50",
    linewidth  = 0.5
  ) +
  # Segment connecting elite to low
  geom_segment(
    aes(x = low_hr, xend = elite_hr,
        y = metric_label, yend = metric_label,
        color = gap),
    linewidth = 1.0, show.legend = FALSE
  ) +
  # Low-tier dot (red)
  geom_point(aes(x = low_hr),  color = "#D73027", size = 2.5) +
  # Elite-tier dot (green)
  geom_point(aes(x = elite_hr), color = "#00B050", size = 2.5) +
  # Overall hit rate label (first facet only via dummy data)
  annotate(
    "text",
    x = overall_hit_rate,
    y = -Inf,
    label = glue("Overall\n{round(overall_hit_rate, 1)}%"),
    hjust = -0.1, vjust = -0.3,
    size  = 2.5, color = "gray40"
  ) +
  scale_color_gradient(low = "gray75", high = "#1F4E79") +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%")
  ) +
  facet_wrap(~position, scales = "free_y", ncol = 2) +
  labs(
    title    = "Signal Strength by Metric: Tier 1 vs Lowest Tier Hit Rates",
    subtitle = paste0(
      "Green dot = Elite Signal hit rate  |  Red dot = Lowest tier hit rate\n",
      "Dashed line = overall training hit rate (",
      round(overall_hit_rate, 1), "%)"
    ),
    x       = "NFL Hit Rate (%)",
    y       = NULL,
    caption = ATTRIBUTION
  ) +
  theme_toolkit(base_size = 11) +
  theme(
    legend.position = "none",
    axis.text.y     = element_text(size = 8)
  )

p2_path <- file.path(OUTPUT_DIR, glue("{PREFIX}signal_strength.png"))
ggsave(p2_path, plot = p2, width = 12, height = 14, dpi = DPI, bg = "white")
cat(glue("  Saved: {p2_path}\n"))


# ==============================================================================
# PLOT 3: Top 2026 Rookie Rankings
# Question: Who are the top dynasty targets in the 2026 draft class?
# ==============================================================================

cat("Building Plot 3: Top 2026 rookie rankings...\n")

rookies_2026 <- scores |>
  filter(draft_year == 2026L) |>
  group_by(position) |>
  slice_max(score_final, n = TOP_N, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    position    = factor(position, levels = c("QB", "RB", "WR", "TE")),
    score_bucket = cut(
      score_final,
      breaks         = SCORE_BREAKS,
      labels         = SCORE_LABELS,
      include.lowest = TRUE,
      right          = FALSE
    )
  )

# Set within-position factor levels so best player appears at top of each facet
player_order <- rookies_2026 |>
  arrange(position, score_final) |>          # ascending = worst first -> top of chart
  mutate(name_pos = paste0(position, ":", cfb_player_name)) |>
  pull(name_pos)

rookies_2026 <- rookies_2026 |>
  mutate(name_pos = paste0(position, ":", cfb_player_name),
         name_pos = factor(name_pos, levels = player_order))

p3 <- ggplot(rookies_2026, aes(x = score_final, y = name_pos, fill = score_bucket)) +
  geom_col(width = 0.7) +
  # Score v1 overlay dot
  geom_point(
    aes(x = score_v1),
    color = "#1F4E79", size = 2.0, shape = 21,
    fill = "white", stroke = 1.2
  ) +
  # Score label on bar
  geom_text(
    aes(label = round(score_final, 1)),
    hjust = -0.2, size = 2.8, color = "gray20"
  ) +
  scale_x_continuous(limits = c(0, 110), breaks = seq(0, 100, 25)) +
  scale_y_discrete(labels = function(x) sub("^[^:]+:", "", x)) +
  scale_fill_manual(values = SCORE_COLORS, drop = FALSE) +
  facet_wrap(~position, scales = "free_y", ncol = 2) +
  labs(
    title    = glue("Top {TOP_N} 2026 Dynasty Rookie Rankings by Position"),
    subtitle = "Bar = Score (Final) | White dot = Score (V1) | Scores scaled 0-100 within position",
    x        = "Score (0-100)",
    y        = NULL,
    fill     = "Score Range",
    caption  = ATTRIBUTION
  ) +
  theme_toolkit() +
  theme(legend.position = "top")

p3_path <- file.path(OUTPUT_DIR, glue("{PREFIX}top_2026_rookies.png"))
ggsave(p3_path, plot = p3, width = 12, height = 10, dpi = DPI, bg = "white")
cat(glue("  Saved: {p3_path}\n"))


# ==============================================================================
# PLOTS 4a-d: Correlation Heatmaps (one per position)
# Question: Which metrics are collinear? What clusters form within each position?
# ==============================================================================

cat("Building Plots 4a-d: Correlation heatmaps...\n")

# Remove self-correlations
corr_clean <- corr |>
  filter(metric_x != metric_y) |>
  mutate(
    label_x = dplyr::coalesce(SHORT_LABELS[metric_x], metric_x),
    label_y = dplyr::coalesce(SHORT_LABELS[metric_y], metric_y)
  )

build_corr_heatmap <- function(pos) {
  df <- filter(corr_clean, position == pos)

  if (nrow(df) == 0L) {
    warning(glue("No correlation data found for position: {pos}"))
    return(NULL)
  }

  # Order metrics by hierarchical clustering for visual grouping
  # Pivot to wide matrix, compute distance, extract order
  mat_wide <- df |>
    select(label_x, label_y, pearson_r) |>
    pivot_wider(names_from = label_y, values_from = pearson_r, values_fill = 0) |>
    tibble::column_to_rownames("label_x") |>
    as.matrix()

  # Fill diagonal with 1 (self-correlations removed earlier)
  shared_names <- intersect(rownames(mat_wide), colnames(mat_wide))
  for (nm in shared_names) {
    mat_wide[nm, nm] <- 1
  }

  # Hierarchical clustering order
  if (nrow(mat_wide) >= 2) {
    dist_mat   <- as.dist(1 - mat_wide[shared_names, shared_names])
    hc         <- hclust(dist_mat, method = "complete")
    metric_ord <- shared_names[hc$order]
  } else {
    metric_ord <- shared_names
  }

  df <- df |>
    mutate(
      label_x = factor(label_x, levels = metric_ord),
      label_y = factor(label_y, levels = metric_ord)
    )

  # Compute n_labeled_points -- heatmap tiles are not labeled, always static
  n_cells <- nrow(df)
  cat(glue("  [{pos}] {n_cells} correlation cells -- static PNG (no point labels)\n"))

  # High collinearity border annotation
  df_high <- filter(df, abs(pearson_r) >= 0.7)

  p <- ggplot(df, aes(x = label_x, y = label_y, fill = pearson_r)) +
    geom_tile(color = "white", linewidth = 0.2) +
    # High collinearity outline
    geom_tile(
      data      = df_high,
      fill      = NA,
      color     = "#1F4E79",
      linewidth = 0.6
    ) +
    scale_fill_gradient2(
      low      = "#D73027",
      mid      = "white",
      high     = "#4575B4",
      midpoint = 0,
      limits   = c(-1, 1),
      name     = "Pearson r",
      breaks   = c(-1, -0.5, 0, 0.5, 1),
      labels   = c("-1.0", "-0.5", "0", "0.5", "1.0")
    ) +
    coord_fixed() +
    labs(
      title    = glue("{pos} -- Metric Correlation Heatmap"),
      subtitle = "Blue outline = |r| >= 0.70 (high collinearity). Metrics ordered by hierarchical clustering.",
      x        = NULL,
      y        = NULL,
      caption  = ATTRIBUTION
    ) +
    theme_heatmap()

  p
}

heatmap_plots <- list()
for (pos in c("QB", "RB", "WR", "TE")) {
  p <- build_corr_heatmap(pos)
  if (!is.null(p)) {
    heatmap_plots[[pos]] <- p
    out_path <- file.path(OUTPUT_DIR, glue("{PREFIX}corr_heatmap_{pos}.png"))
    ggsave(out_path, plot = p, width = 9, height = 8, dpi = DPI, bg = "white")
    cat(glue("  Saved: {out_path}\n"))
  }
}


# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

outputs <- c(
  glue("{PREFIX}score_distribution.png"),
  glue("{PREFIX}signal_strength.png"),
  glue("{PREFIX}top_2026_rookies.png"),
  glue("{PREFIX}corr_heatmap_QB.png"),
  glue("{PREFIX}corr_heatmap_RB.png"),
  glue("{PREFIX}corr_heatmap_WR.png"),
  glue("{PREFIX}corr_heatmap_TE.png")
)

n_prospects_2026 <- sum(scores$draft_year == 2026L, na.rm = TRUE)
n_training       <- sum(scores$is_training_player == TRUE, na.rm = TRUE)
n_prediction     <- sum(scores$is_training_player == FALSE, na.rm = TRUE)
top_2026_player  <- scores |>
  filter(draft_year == 2026L) |>
  slice_max(score_final, n = 1L, with_ties = FALSE) |>
  pull(cfb_player_name)
top_2026_score   <- scores |>
  filter(draft_year == 2026L) |>
  slice_max(score_final, n = 1L, with_ties = FALSE) |>
  pull(score_final) |>
  round(1)
n_high_corr_pairs <- sum(abs(corr$pearson_r) >= 0.7 &
                           corr$metric_x != corr$metric_y, na.rm = TRUE) %/% 2L

cat("\n=== Week 14 Visual Script Complete (schema: s2_w14_v1) ===\n\n")
cat(glue("Players scored: {n_training} training + {n_prediction} prediction = {nrow(scores)} total\n"))
cat(glue("2026 rookies:   {n_prospects_2026} players  |  Top overall: {top_2026_player} ({top_2026_score})\n"))
cat(glue("Overall training hit rate: {round(overall_hit_rate, 1)}%\n"))
cat(glue("High-collinearity metric pairs (|r| >= 0.70): {n_high_corr_pairs}\n\n"))

cat("Outputs saved to", OUTPUT_DIR, ":\n")
for (f in outputs) {
  status <- if (file.exists(file.path(OUTPUT_DIR, f))) "[OK]" else "[MISSING]"
  cat(glue("  {status}  {f}\n"))
}

cat(glue("\nDensity check: all plots under {DENSITY_THRESHOLD} labeled points -- no interactive HTML required.\n"))
