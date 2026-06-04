# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 16
# Visuals Script: DEF/ST Forward Projection
# File: examples/create_season2_week16_visuals.R
#
# Produces three publication-quality PNG plots (300 dpi):
#
#   1. s2_week16_def_projection_ranking.png
#      32 teams ranked by preseason projected DEF/ST PPG. The headline output
#      the optimizer consumes. Loaded from the saved projection artifact.
#
#   2. s2_week16_regression_shrinkage.png
#      Each team's raw historical DEF PPG vs its regressed prior, as a dumbbell.
#      Shows the 0.50 shrinkage pulling the extremes toward the league mean,
#      the design decision validated in the assumptions script.
#
#   3. s2_week16_inseason_convergence.png
#      For the biggest in-season riser and faller (2025 used as a stand-in live
#      season), how the projection migrates from the prior toward observed
#      across weeks 1-18. Makes the in-season update behavior concrete.
#
# DENSITY CHECK (per visualization-patterns.md)
#   Plot 1: 32 bars                       -- static PNG sufficient
#   Plot 2: 32 dumbbells (64 points)      -- static PNG sufficient
#   Plot 3: 2 teams x 18 weeks (36 pts)   -- static PNG sufficient
#   None exceed the 50 labeled-point interactivity threshold.
#
# DATA SOURCES
#   Plot 1 loads data/season2_cache/s2_week16_def_st_projections.rds (run
#   project_def_st() first). Plots 2 and 3 compute pieces not present in that
#   artifact (the raw prior, and the per-week trajectory), reusing R/34's own
#   functions so they stay faithful to the projection.
#
# OUTPUT
#   output/plots/s2_week16_def_projection_ranking.png
#   output/plots/s2_week16_regression_shrinkage.png
#   output/plots/s2_week16_inseason_convergence.png
#
# RUN
#   source(here::here("examples", "create_season2_week16_visuals.R"))
#
# DEPENDENCIES
#   ggplot2, dplyr, tidyr, glue, here
#   R/34_def_st_projection.R (sources R/15, R/19, R/29)
# ==============================================================================

# ==============================================================================
# SECTION 1: SETUP
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(glue)
library(here)

source(here::here("R", "34_def_st_projection.R"))

OUTPUT_DIR  <- here::here("output", "plots")
DPI         <- 300L
PREFIX      <- "s2_week16_"
ATTRIBUTION <- "Data: nflfastR 2010-2025  |  Analysis: NFL Analytics Toolkit S2W16"

# Colorblind-safe palette (Wong 2011)
COL_PRIMARY   <- "#0072B2"   # blue   -- projected / regressed
COL_RAW       <- "#E69F00"   # orange -- raw historical
COL_RISER     <- "#009E73"   # green  -- in-season riser
COL_FALLER    <- "#D55E00"   # vermillion -- in-season faller
COL_REFERENCE <- "gray55"

theme_toolkit <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = 14, hjust = 0),
      plot.subtitle    = ggplot2::element_text(size = 11, color = "gray40", hjust = 0),
      plot.caption     = ggplot2::element_text(size = 8,  color = "gray55", hjust = 1),
      plot.background   = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid.major  = ggplot2::element_line(color = "gray90", linewidth = 0.4),
      panel.grid.minor  = ggplot2::element_blank(),
      axis.title        = ggplot2::element_text(size = 10),
      legend.position   = "bottom",
      legend.title      = ggplot2::element_text(face = "bold", size = 10),
      plot.margin       = ggplot2::margin(12, 16, 8, 12)
    )
}

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  message(glue("Created output directory: {OUTPUT_DIR}"))
}

# Guard: stop with an informative message rather than render a blank plot.
assert_rows <- function(df, label, min_rows = 1L) {
  if (is.null(df) || nrow(df) < min_rows) {
    stop(glue("[create_season2_week16_visuals] '{label}' has ",
              "{if (is.null(df)) 0 else nrow(df)} rows ",
              "(need >= {min_rows}). Check the cache and upstream functions."),
         call. = FALSE)
  }
  invisible(df)
}


# ==============================================================================
# SECTION 2: PLOT 1 -- PRESEASON PROJECTION RANKING
# ==============================================================================

proj_path <- OUTPUT_RDS_PATH   # s2_week16_def_st_projections.rds (from R/34)
if (!file.exists(proj_path)) {
  stop(glue("Projection artifact not found: {proj_path}\n",
            "Run project_def_st() first to generate it."), call. = FALSE)
}

proj <- readRDS(proj_path)
assert_rows(proj, "projection artifact", min_rows = 32L)

league_avg_proj <- mean(proj$def_proj_ppg, na.rm = TRUE)

p1 <- ggplot2::ggplot(
  proj,
  ggplot2::aes(x = def_proj_ppg, y = stats::reorder(team, def_proj_ppg))
) +
  ggplot2::geom_vline(xintercept = league_avg_proj, linetype = "dashed",
                      color = COL_REFERENCE, linewidth = 0.5) +
  ggplot2::geom_col(fill = COL_PRIMARY, width = 0.7) +
  ggplot2::annotate("text", x = league_avg_proj, y = 1.5,
                    label = glue("league avg {format(round(league_avg_proj, 1), nsmall = 1)}"),
                    hjust = -0.05, vjust = 0, size = 3, color = COL_REFERENCE) +
  ggplot2::labs(
    title    = "Projected DEF/ST fantasy points per game, 2026",
    subtitle = "Preseason projection, standard Sleeper scoring",
    x        = "Projected DEF/ST PPG",
    y        = NULL,
    caption  = ATTRIBUTION
  ) +
  theme_toolkit() +
  ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

p1_path <- file.path(OUTPUT_DIR, glue("{PREFIX}def_projection_ranking.png"))
ggplot2::ggsave(p1_path, plot = p1, width = 8, height = 8, dpi = DPI, bg = "white")
message(glue("  Saved: {p1_path}"))


# ==============================================================================
# SECTION 3: PLOT 2 -- REGRESSION SHRINKAGE (RAW vs REGRESSED PRIOR)
# ==============================================================================
# .compute_def_prior returns both the raw multi-season blend and the regressed
# prior, which the saved artifact does not carry, so we compute it here.

prior_tbl <- .compute_def_prior(PRIOR_SEASONS, DEF_SCORING_DEFAULT,
                                CACHE_DIR_DEFAULT)
assert_rows(prior_tbl, "prior table", min_rows = 32L)

league_mean_raw <- mean(prior_tbl$prior_raw_ppg, na.rm = TRUE)

# Long form for the two endpoints of each dumbbell
shrink_long <- prior_tbl %>%
  dplyr::select(team, prior_raw_ppg, def_prior_ppg) %>%
  tidyr::pivot_longer(
    cols      = c(prior_raw_ppg, def_prior_ppg),
    names_to  = "kind",
    values_to = "ppg"
  ) %>%
  dplyr::mutate(kind = dplyr::if_else(kind == "prior_raw_ppg",
                                      "Raw historical", "Regressed prior"))

team_order <- prior_tbl %>% dplyr::arrange(prior_raw_ppg) %>% dplyr::pull(team)
shrink_long$team <- factor(shrink_long$team, levels = team_order)
prior_tbl$team   <- factor(prior_tbl$team, levels = team_order)

p2 <- ggplot2::ggplot() +
  ggplot2::geom_vline(xintercept = league_mean_raw, linetype = "dashed",
                      color = COL_REFERENCE, linewidth = 0.5) +
  ggplot2::geom_segment(
    data = prior_tbl,
    ggplot2::aes(x = prior_raw_ppg, xend = def_prior_ppg,
                 y = team, yend = team),
    color = "gray75", linewidth = 0.6
  ) +
  ggplot2::geom_point(
    data = shrink_long,
    ggplot2::aes(x = ppg, y = team, color = kind),
    size = 2.2
  ) +
  ggplot2::scale_color_manual(values = c("Raw historical" = COL_RAW,
                                         "Regressed prior" = COL_PRIMARY)) +
  ggplot2::labs(
    title    = "Shrinkage pulls extreme defenses toward the mean",
    subtitle = glue("Raw 2023-2025 DEF PPG vs the regressed prior ",
                    "(strength 0.50). League mean ",
                    "{format(round(league_mean_raw, 1), nsmall = 1)}"),
    x        = "DEF/ST PPG",
    y        = NULL,
    color    = NULL,
    caption  = ATTRIBUTION
  ) +
  theme_toolkit() +
  ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

p2_path <- file.path(OUTPUT_DIR, glue("{PREFIX}regression_shrinkage.png"))
ggplot2::ggsave(p2_path, plot = p2, width = 8, height = 8, dpi = DPI, bg = "white")
message(glue("  Saved: {p2_path}"))


# ==============================================================================
# SECTION 4: PLOT 3 -- IN-SEASON CONVERGENCE (2025 STAND-IN)
# ==============================================================================
# Demonstrates the in-season update on real data by treating 2025 as the live
# season with a 2023-2024 prior. We compute the prior once and the 2025 per-game
# scores once, then blend cumulatively week by week using R/34's own helpers, so
# the trajectory matches what project_def_st(as_of_week = w) would produce.

STANDIN_SEASON <- 2025L
STANDIN_PRIORS <- 2023:2024
MAX_WEEK       <- 18L

prior_si <- .compute_def_prior(STANDIN_PRIORS, DEF_SCORING_DEFAULT,
                               CACHE_DIR_DEFAULT) %>%
  dplyr::select(team, def_prior_ppg)
assert_rows(prior_si, "stand-in prior", min_rows = 32L)

pbp_si <- load_normalized_season(STANDIN_SEASON, cache_dir = CACHE_DIR_DEFAULT)
games_si <- .score_def_games(calculate_def_st_points(pbp_si), DEF_SCORING_DEFAULT)
assert_rows(games_si, "stand-in per-game scores", min_rows = 1L)

# Cumulative observed PPG and blended projection for one team across weeks.
team_trajectory <- function(tm) {
  prior_val <- prior_si$def_prior_ppg[prior_si$team == tm]
  glog <- games_si %>% dplyr::filter(team == tm) %>% dplyr::arrange(week)
  purrr::map_dfr(seq_len(MAX_WEEK), function(w) {
    played <- glog %>% dplyr::filter(week <= w)
    n_obs  <- nrow(played)
    obs    <- if (n_obs > 0L) mean(played$def_st_points, na.rm = TRUE) else NA_real_
    pw     <- compute_prior_weight(w)
    proj   <- .blend_prior_observed(prior_val, obs, pw, n_obs > 0L)
    tibble::tibble(team = tm, week = w, prior = prior_val,
                   observed_cum = obs, projection = proj)
  })
}

# Pick the biggest riser and faller at full season (data-driven, not hardcoded).
full_obs <- games_si %>%
  dplyr::group_by(team) %>%
  dplyr::summarise(obs_full = mean(.data$def_st_points, na.rm = TRUE),
                   .groups = "drop")

movers <- prior_si %>%
  dplyr::inner_join(full_obs, by = "team") %>%
  dplyr::mutate(delta = obs_full - def_prior_ppg)

riser  <- movers$team[which.max(movers$delta)]
faller <- movers$team[which.min(movers$delta)]

traj <- dplyr::bind_rows(team_trajectory(riser), team_trajectory(faller)) %>%
  dplyr::mutate(
    role = dplyr::if_else(team == riser,
                          glue("{riser} (biggest riser)"),
                          glue("{faller} (biggest faller)"))
  )
assert_rows(traj, "trajectory", min_rows = 2L)

# Prior reference lines, one per team
prior_lines <- traj %>%
  dplyr::distinct(team, role, prior)

p3 <- ggplot2::ggplot(traj, ggplot2::aes(x = week, y = projection, color = role)) +
  ggplot2::geom_hline(
    data = prior_lines,
    ggplot2::aes(yintercept = prior, color = role),
    linetype = "dashed", linewidth = 0.5, show.legend = FALSE
  ) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::scale_color_manual(
    values = setNames(c(COL_RISER, COL_FALLER),
                      c(glue("{riser} (biggest riser)"),
                        glue("{faller} (biggest faller)")))
  ) +
  ggplot2::scale_x_continuous(breaks = seq(2, MAX_WEEK, by = 2)) +
  ggplot2::labs(
    title    = "Projections migrate from prior toward observed as the season runs",
    subtitle = glue("2025 used as a stand-in live season, 2023-2024 prior. ",
                    "Dashed lines are each team's preseason prior."),
    x        = "Week (as_of_week)",
    y        = "Projected DEF/ST PPG",
    color    = NULL,
    caption  = ATTRIBUTION
  ) +
  theme_toolkit()

p3_path <- file.path(OUTPUT_DIR, glue("{PREFIX}inseason_convergence.png"))
ggplot2::ggsave(p3_path, plot = p3, width = 9, height = 5.5, dpi = DPI, bg = "white")
message(glue("  Saved: {p3_path}"))


# ==============================================================================
# SECTION 5: CONFIRMATION + KEY INSIGHTS (computed from data, not hardcoded)
# ==============================================================================

cat("\n", strrep("=", 64), "\n", sep = "")
cat("Week 16 DEF/ST visuals complete.\n")
cat(strrep("=", 64), "\n\n", sep = "")

for (p in c(p1_path, p2_path, p3_path)) {
  size_kb <- round(file.info(p)$size / 1024, 0)
  cat(glue("  {basename(p)}  ({size_kb} KB)\n"))
}

top_team <- proj$team[which.max(proj$def_proj_ppg)]
top_ppg  <- max(proj$def_proj_ppg, na.rm = TRUE)
max_pull <- prior_tbl %>%
  dplyr::mutate(pull = abs(def_prior_ppg - prior_raw_ppg)) %>%
  dplyr::slice_max(pull, n = 1L)
riser_final  <- traj$projection[traj$team == riser  & traj$week == MAX_WEEK]
faller_final <- traj$projection[traj$team == faller & traj$week == MAX_WEEK]

cat("\nKEY INSIGHTS\n")
cat(glue("  Plot 1: top projected defense {top_team} at ",
         "{format(round(top_ppg, 2), nsmall = 2)} PPG\n"))
cat(glue("  Plot 2: largest shrinkage pull {max_pull$team} ",
         "{format(round(max_pull$prior_raw_ppg, 2), nsmall = 2)} -> ",
         "{format(round(max_pull$def_prior_ppg, 2), nsmall = 2)} PPG\n"))
cat(glue("  Plot 3: by week {MAX_WEEK}, {riser} at ",
         "{format(round(riser_final, 2), nsmall = 2)} and {faller} at ",
         "{format(round(faller_final, 2), nsmall = 2)} PPG\n"))
cat(strrep("=", 64), "\n")
