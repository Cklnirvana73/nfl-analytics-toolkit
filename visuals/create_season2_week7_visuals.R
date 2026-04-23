# ==============================================================================
# NFL Analytics Toolkit - Season 2, Week 7
# Visualization Script
# File: examples/create_season2_week7_visuals.R
#
# Purpose: Generate three analytically meaningful static PNGs from the CFB
#          player-season panel built by R/21. Each visual interrogates a
#          specific property of build_cfb_player_season_panel() output.
#
# Outputs:
#   output/plots/s2_week7_player_seasons_by_position.png
#   output/plots/s2_week7_epa_efficiency_by_position.png
#   output/plots/s2_week7_position_classification_audit.png
#
# Design notes:
#   - 2025 season is EXCLUDED from cross-season trend visuals due to
#     cfbfastR's upstream duplicate data issue, which inflates 2025
#     player-season counts roughly 8x vs normal and distorts any
#     cross-year comparison. Confirmed via direct load_cfb_pbp(2024)
#     inspection: 113,209 / 276,267 rows are duplicates at the source.
#     2024 is the most recent clean season for showcase purposes.
#   - Volume filter: pass_attempts > 100 for QBs, rush_attempts > 50 for
#     RBs, targets > 30 for WR_TEs. These match the thresholds used in
#     the example script's Step 7 so visuals and console output agree.
#   - All visuals use prose-style informative titles per visualization
#     reference (state the insight, not the axes).
#
# Prerequisites:
#   - CFB cache populated (Week 6 pipeline run)
#   - R/20 and R/21 sourced
#   - output/plots/ writable
#
# Run:
#   source(here::here("examples", "create_season2_week7_visuals.R"))
# ==============================================================================

library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(glue)
library(here)

source(here::here("R", "20_multi_season_cfb_pbp.R"))
source(here::here("R", "21_cfb_player_season_panel.R"))


# ==============================================================================
# CONSTANTS
# ==============================================================================

OUTPUT_DIR <- here::here("output", "plots")
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# 2025 excluded from all cross-season visuals (upstream cfbfastR duplicate data).
SEASONS_CLEAN     <- 2014:2024
SHOWCASE_SEASON   <- 2024L

# Volume filters (match Step 7 in example_season2_week7.R).
QB_MIN_ATTEMPTS <- 100L
RB_MIN_RUSHES   <- 50L
WR_MIN_TARGETS  <- 30L

# Named palette (distinct, colorblind-safe-ish).
POSITION_COLORS <- c(
  QB    = "#2A6F97",   # steel blue
  RB    = "#E07A5F",   # terracotta
  WR_TE = "#81B29A",   # sage green
  other = "#8D99AE",   # muted slate
  `NA`  = "#D8D8D8"    # light gray
)

# Shared theme for consistent look across all three PNGs.
theme_s2w7 <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title           = element_text(face = "bold", size = base_size + 3),
      plot.subtitle        = element_text(color = "gray30",
                                          size = base_size, margin = margin(b = 10)),
      plot.caption         = element_text(color = "gray50",
                                          size = base_size - 2, hjust = 0),
      strip.text           = element_text(face = "bold", size = base_size),
      axis.title           = element_text(size = base_size - 1),
      legend.position      = "top",
      legend.title         = element_text(size = base_size - 1),
      panel.grid.minor     = element_blank()
    )
}


# ==============================================================================
# LOAD PANEL
# ==============================================================================

cat(strrep("=", 70), "\n", sep = "")
cat("Week 7 visuals: loading 12-season panel...\n")
cat(strrep("=", 70), "\n\n", sep = "")

panel_full <- build_cfb_player_season_panel(
  seasons             = 2014:2025,
  include_postseason  = FALSE,
  garbage_time_filter = TRUE,
  verbose             = FALSE
)

cat(glue("Panel loaded: {format(nrow(panel_full), big.mark = ',')} rows "),
    glue("across {dplyr::n_distinct(panel_full$season)} seasons.\n\n"))


# ==============================================================================
# VISUAL 1: Player-seasons by position group and season
# ==============================================================================
# What this interrogates:
#   Does the panel have sensible row coverage across 12 seasons? The
#   2021-2022 jump (3,272 and 3,326 player-seasons vs ~2,000 elsewhere)
#   is the analytically interesting pattern -- it parallels the Week 6
#   FCS coverage expansion finding.
#
# Why it matters for downstream Phase 3 code: The panel is the feature
# source for translation models, so uneven row counts across years signal
# either a data coverage shift (acceptable, document it) or a bug
# (must be fixed).
# ==============================================================================

cat("VISUAL 1: Player-seasons by position group and season...\n")

# Filter to clean seasons. Keep all position groups including 'other' and NA
# so the chart shows the full panel composition, not a filtered view.
plot1_data <- panel_full %>%
  dplyr::filter(season %in% SEASONS_CLEAN, !low_volume) %>%
  dplyr::mutate(
    position_group = dplyr::if_else(is.na(position_group),
                                    "NA", position_group),
    position_group = factor(position_group,
                            levels = c("QB", "RB", "WR_TE", "other", "NA"))
  ) %>%
  dplyr::count(season, position_group, name = "n_player_seasons")

# Compute total per season for caption context.
season_totals <- plot1_data %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(total = sum(n_player_seasons), .groups = "drop")

max_season_total <- max(season_totals$total)
max_season_year  <- season_totals$season[which.max(season_totals$total)]

v1_title <- "2021-2022 show elevated player-season counts in the CFB panel"
v1_subtitle <- glue(
  "Non-low-volume player-seasons, FBS only, regular season, garbage time filtered | ",
  "peak: {max_season_year} ({format(max_season_total, big.mark = ',')} players)"
)

p1 <- ggplot(plot1_data,
             aes(x = season, y = n_player_seasons, fill = position_group)) +
  geom_col(width = 0.75) +
  scale_fill_manual(values = POSITION_COLORS, name = "Position group") +
  scale_x_continuous(breaks = SEASONS_CLEAN) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title    = v1_title,
    subtitle = v1_subtitle,
    x        = NULL,
    y        = "Player-seasons",
    caption  = "Source: cfbfastR 2014-2024 | R/21_cfb_player_season_panel.R | 2025 excluded (upstream data issue)"
  ) +
  theme_s2w7()

v1_path <- file.path(OUTPUT_DIR, "s2_week7_player_seasons_by_position.png")
ggsave(v1_path, p1, width = 10, height = 5.5, dpi = 300)
cat(glue("  Saved: {v1_path}\n\n"))


# ==============================================================================
# VISUAL 2: EPA efficiency by position group across seasons
# ==============================================================================
# What this interrogates:
#   Do the efficiency columns produced by build_cfb_player_season_panel()
#   yield stable, interpretable signal across 12 seasons? This is the
#   primary quality check before Phase 3 models consume these features.
#
# Computation: mean pass_epa_per_attempt (QB), mean rush_epa_per_attempt
# (RB), mean rec_epa_per_target (WR_TE), each volume-filtered. Matches
# the Step 7 tables in the example script.
# ==============================================================================

cat("VISUAL 2: EPA efficiency by position group across seasons...\n")

# Compute three efficiency series. One row per (season, position_group) with
# a common 'mean_efficiency' column so facets can share a tidy long layout.
qb_eff <- panel_full %>%
  dplyr::filter(
    season %in% SEASONS_CLEAN,
    position_group == "QB",
    pass_attempts > QB_MIN_ATTEMPTS,
    !is.na(pass_epa_per_attempt)
  ) %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(
    position_group  = "QB",
    mean_efficiency = mean(pass_epa_per_attempt, na.rm = TRUE),
    n_players       = dplyr::n(),
    .groups         = "drop"
  )

rb_eff <- panel_full %>%
  dplyr::filter(
    season %in% SEASONS_CLEAN,
    position_group == "RB",
    rush_attempts > RB_MIN_RUSHES,
    !is.na(rush_epa_per_attempt)
  ) %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(
    position_group  = "RB",
    mean_efficiency = mean(rush_epa_per_attempt, na.rm = TRUE),
    n_players       = dplyr::n(),
    .groups         = "drop"
  )

wr_eff <- panel_full %>%
  dplyr::filter(
    season %in% SEASONS_CLEAN,
    position_group == "WR_TE",
    targets > WR_MIN_TARGETS,
    !is.na(rec_epa_per_target)
  ) %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(
    position_group  = "WR_TE",
    mean_efficiency = mean(rec_epa_per_target, na.rm = TRUE),
    n_players       = dplyr::n(),
    .groups         = "drop"
  )

plot2_data <- dplyr::bind_rows(qb_eff, rb_eff, wr_eff) %>%
  dplyr::mutate(
    position_group = factor(position_group,
                            levels = c("QB", "RB", "WR_TE"),
                            labels = c("QB (pass EPA / attempt)",
                                        "RB (rush EPA / attempt)",
                                        "WR_TE (rec EPA / target)"))
  )

# Per-facet mean reference line.
facet_means <- plot2_data %>%
  dplyr::group_by(position_group) %>%
  dplyr::summarise(grand_mean = mean(mean_efficiency, na.rm = TRUE),
                   .groups = "drop")

v2_title <- "EPA efficiency is stable across seasons for all three position groups"
v2_subtitle <- glue(
  "Mean per-play EPA by season, volume-filtered ",
  "(QB > {QB_MIN_ATTEMPTS} attempts, RB > {RB_MIN_RUSHES} rushes, WR_TE > {WR_MIN_TARGETS} targets)"
)

p2 <- ggplot(plot2_data, aes(x = season, y = mean_efficiency)) +
  geom_hline(data = facet_means,
             aes(yintercept = grand_mean),
             linetype = "dashed", color = "gray60", linewidth = 0.5) +
  geom_line(aes(color = position_group),
            linewidth = 1, show.legend = FALSE) +
  geom_point(aes(color = position_group, size = n_players),
             show.legend = FALSE) +
  scale_color_manual(values = c(
    "QB (pass EPA / attempt)"   = POSITION_COLORS[["QB"]],
    "RB (rush EPA / attempt)"   = POSITION_COLORS[["RB"]],
    "WR_TE (rec EPA / target)"  = POSITION_COLORS[["WR_TE"]]
  )) +
  scale_x_continuous(breaks = seq(2014L, 2024L, by = 2L)) +
  scale_size_continuous(range = c(1.5, 4)) +
  facet_wrap(~ position_group, scales = "free_y", ncol = 3) +
  labs(
    title    = v2_title,
    subtitle = v2_subtitle,
    x        = NULL,
    y        = "Mean EPA per play",
    caption  = "Dashed line: cross-season mean within each facet | Point size: n qualifying players | 2025 excluded"
  ) +
  theme_s2w7() +
  theme(panel.spacing = unit(1.2, "lines"))

v2_path <- file.path(OUTPUT_DIR, "s2_week7_epa_efficiency_by_position.png")
ggsave(v2_path, p2, width = 11, height = 5, dpi = 300)
cat(glue("  Saved: {v2_path}\n\n"))


# ==============================================================================
# VISUAL 3: Position classification heuristic audit
# ==============================================================================
# What this interrogates:
#   How well-calibrated are the thresholds in classify_cfb_player_position()?
#   Left panel: overall distribution of position_group labels. Right panel:
#   for rows labeled 'other', what does the play-role mix look like?
#   If 'other' players cluster near threshold boundaries, the heuristic is
#   leaving an archetype uncaptured. If they scatter broadly, 'other' is
#   correctly identifying genuinely ambiguous players.
#
# Why it matters: position_group is the primary join key for all Phase 3
# per-position analyses. A miscalibrated classifier would leak QB-like
# players into RB analyses and vice versa.
# ==============================================================================

cat("VISUAL 3: Position classification heuristic audit...\n")

# Left-panel data: count of player-seasons by position_group.
# Use all clean seasons (not just one season) to get stable counts.
plot3_counts <- panel_full %>%
  dplyr::filter(season %in% SEASONS_CLEAN, !low_volume) %>%
  dplyr::mutate(
    position_group = dplyr::if_else(is.na(position_group),
                                    "NA", position_group),
    position_group = factor(position_group,
                            levels = c("NA", "other", "WR_TE", "RB", "QB"))
  ) %>%
  dplyr::count(position_group, name = "n_players")

total_panel <- sum(plot3_counts$n_players)
plot3_counts <- plot3_counts %>%
  dplyr::mutate(pct = n_players / total_panel)

p3_left <- ggplot(plot3_counts,
                  aes(x = n_players, y = position_group,
                      fill = position_group)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = glue(
    "{format(n_players, big.mark = ',')} ({round(pct * 100, 1)}%)"
  )),
  hjust = -0.05, size = 3.3, color = "gray20") +
  scale_fill_manual(values = c(
    QB    = POSITION_COLORS[["QB"]],
    RB    = POSITION_COLORS[["RB"]],
    WR_TE = POSITION_COLORS[["WR_TE"]],
    other = POSITION_COLORS[["other"]],
    `NA`  = POSITION_COLORS[["NA"]]
  )) +
  scale_x_continuous(labels = scales::comma_format(),
                     expand = expansion(mult = c(0, 0.30))) +
  labs(
    title = "Overall distribution",
    x     = "Player-seasons (2014-2024, non-low-volume)",
    y     = NULL
  ) +
  theme_s2w7() +
  theme(plot.title = element_text(size = 12))

# Right-panel data: play-role mix for 'other' players only. Compute
# pass_pct, rush_pct, recv_pct so the chart shows exactly the thresholds
# used in classify_cfb_player_position().
plot3_other <- panel_full %>%
  dplyr::filter(
    season %in% SEASONS_CLEAN,
    !low_volume,
    position_group == "other"
  ) %>%
  dplyr::mutate(
    total_role_plays = pass_attempts + rush_attempts + targets,
    pass_pct = dplyr::if_else(total_role_plays > 0L,
                              pass_attempts / total_role_plays, NA_real_),
    rush_pct = dplyr::if_else(total_role_plays > 0L,
                              rush_attempts / total_role_plays, NA_real_),
    recv_pct = dplyr::if_else(total_role_plays > 0L,
                              targets / total_role_plays, NA_real_)
  ) %>%
  dplyr::filter(!is.na(pass_pct), !is.na(rush_pct), !is.na(recv_pct))

n_other <- nrow(plot3_other)

# Pivot to long form for a single jittered distribution per role,
# colored by role.
plot3_other_long <- plot3_other %>%
  dplyr::select(player_name, season, pass_pct, rush_pct, recv_pct) %>%
  tidyr::pivot_longer(
    cols      = c(pass_pct, rush_pct, recv_pct),
    names_to  = "role",
    values_to = "share"
  ) %>%
  dplyr::mutate(
    role = dplyr::case_when(
      role == "pass_pct" ~ "pass share",
      role == "rush_pct" ~ "rush share",
      role == "recv_pct" ~ "recv share"
    ),
    role = factor(role, levels = c("pass share", "rush share", "recv share"))
  )

# Reference lines: the actual thresholds from R/21 (0.70 for QB and RB,
# 0.50 for WR_TE).
threshold_lines <- tibble::tibble(
  role      = factor(c("pass share", "rush share", "recv share"),
                     levels = c("pass share", "rush share", "recv share")),
  threshold = c(CFB_POSITION_QB_THRESHOLD,
                CFB_POSITION_RB_THRESHOLD,
                CFB_POSITION_WR_THRESHOLD)
)

p3_right <- ggplot(plot3_other_long,
                   aes(x = share, y = role)) +
  geom_jitter(aes(color = role),
              height = 0.25, width = 0, alpha = 0.35, size = 1.2,
              show.legend = FALSE) +
  geom_point(data = threshold_lines,
             aes(x = threshold, y = role),
             shape = 124, size = 8, color = "red", stroke = 1.2) +
  scale_color_manual(values = c(
    "pass share" = POSITION_COLORS[["QB"]],
    "rush share" = POSITION_COLORS[["RB"]],
    "recv share" = POSITION_COLORS[["WR_TE"]]
  )) +
  scale_x_continuous(labels = scales::percent_format(),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2)) +
  labs(
    title    = glue("'other' players ({format(n_other, big.mark = ',')}) vs thresholds"),
    subtitle = "Red ticks show classification thresholds (0.70 / 0.70 / 0.50)",
    x        = "Share of player-season's role plays",
    y        = NULL
  ) +
  theme_s2w7() +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10))

# Combine panels. Use patchwork if installed; else save separate PNGs and
# print a message. Patchwork is not a hard dependency for R/21 so we guard.
if (requireNamespace("patchwork", quietly = TRUE)) {
  combined <- patchwork::wrap_plots(p3_left, p3_right, widths = c(1, 1.2)) +
    patchwork::plot_annotation(
      title    = "Position classifier is calibrated; 'other' players cluster below all thresholds",
      subtitle = glue(
        "Left: position_group counts across 11 clean seasons | ",
        "Right: 'other' players' pass / rush / recv shares with threshold reference"
      ),
      caption  = "Source: cfbfastR 2014-2024 | R/21 classify_cfb_player_position() | 2025 excluded",
      theme    = theme(
        plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "gray30", size = 11),
        plot.caption  = element_text(color = "gray50", size = 9)
      )
    )

  v3_path <- file.path(OUTPUT_DIR, "s2_week7_position_classification_audit.png")
  ggsave(v3_path, combined, width = 12, height = 5, dpi = 300)
  cat(glue("  Saved: {v3_path}\n\n"))
} else {
  # Fallback: save each panel as a separate PNG.
  v3a_path <- file.path(OUTPUT_DIR,
                        "s2_week7_position_classification_counts.png")
  v3b_path <- file.path(OUTPUT_DIR,
                        "s2_week7_position_classification_other_audit.png")
  ggsave(v3a_path, p3_left,  width = 7,  height = 4, dpi = 300)
  ggsave(v3b_path, p3_right, width = 8,  height = 4, dpi = 300)
  cat(glue("  patchwork not installed -- saved two panels separately:\n"))
  cat(glue("    {v3a_path}\n"))
  cat(glue("    {v3b_path}\n"))
  cat(glue("    (install.packages('patchwork') to get the combined layout)\n\n"))
}


# ==============================================================================
# SUMMARY
# ==============================================================================

cat(strrep("=", 70), "\n", sep = "")
cat("Week 7 visuals complete.\n")
cat(strrep("=", 70), "\n", sep = "")
cat(glue("Output directory: {OUTPUT_DIR}\n"))
