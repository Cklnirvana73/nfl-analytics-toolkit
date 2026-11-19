# ==============================================================================
# Statistics Series - Post 19: Standard Error
# Standalone data pull
#
# PURPOSE
# -------
# Reproduces the per-age standard error computed inside fit_aging_curves()
# in R/23_aging_curves.R (line 864: se_delta = sd_delta / sqrt(n_obs)) and
# prints the components separately so the post can answer one question:
#
#   When the error bars widen at older ages, is that because older players
#   are more erratic (sd_delta rises), or simply because there are fewer of
#   them (n_obs falls)?
#
# Nothing here re-implements the production logic. It calls the real
# functions and prints their intermediate values.
#
# HOW TO RUN
# ----------
#   From the project root:
#     source("stats_posts/scripts/pull_stats_post19_standard_error_data.R")
#
# Requires data/season2_cache to be populated by R/15, same as R/23.
# ==============================================================================

library(nflreadr)
library(dplyr)
library(purrr)
library(tidyr)
library(glue)
library(here)

# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------

R23_PATH        <- here::here("R", "23_aging_curves.R")
FOCUS_POSITION  <- "RB"          # primary position for the post's table
ALL_POSITIONS   <- c("QB", "RB", "WR", "TE")

cat("\n")
cat("==============================================================\n")
cat("  Statistics Series Post 19: Standard Error - data pull\n")
cat("==============================================================\n\n")

# ------------------------------------------------------------------------------
# STEP 1: Source R/23 (pulls in its constants and functions)
# ------------------------------------------------------------------------------

if (!file.exists(R23_PATH)) {
  stop(glue("R/23_aging_curves.R not found at: {R23_PATH}"))
}
cat("Step 1: Sourcing R/23_aging_curves.R ...\n")
source(R23_PATH)

stopifnot(
  exists("compute_age_deltas",  mode = "function"),
  exists("compute_player_ages", mode = "function"),
  exists("compute_season_ppg",  mode = "function"),
  exists("split_wr_te_positions", mode = "function")
)
cat("  Functions available.\n\n")

# ------------------------------------------------------------------------------
# STEP 2: Build the player-season panel (same path R/23 uses)
# ------------------------------------------------------------------------------

cat("Step 2: Building player-season panel ...\n")

if (!exists("build_player_season_panel", mode = "function")) {
  r16_path <- here::here("R", "16_player_season_panel.R")
  if (!file.exists(r16_path)) {
    stop(glue("R/16_player_season_panel.R not found at: {r16_path}"))
  }
  source(r16_path)
}

panel <- build_player_season_panel(
  seasons   = PANEL_SEASONS,
  cache_dir = CACHE_DIR,
  verbose   = TRUE
)

cat(glue("  Panel rows: {format(nrow(panel), big.mark = ',')}\n\n"))

# ------------------------------------------------------------------------------
# STEP 3: Ages and fantasy points per game
# ------------------------------------------------------------------------------

cat("Step 3: Computing ages and PPR per game ...\n")

panel <- compute_player_ages(panel, verbose = FALSE)
panel <- compute_season_ppg(panel, ppr_value = 1)

# R/16 collapses receivers and tight ends into a combined WR_TE group.
# R/23's pipeline splits them at Step 3b before computing deltas. Without
# this call, compute_age_deltas() filters on c("QB","RB","WR","TE"), matches
# no WR_TE row, and silently drops every receiver and tight end.
panel <- split_wr_te_positions(panel, verbose = TRUE)

cat("\n  Position groups after split:\n")
print(table(panel$position_group))

stopifnot(
  sum(panel$position_group == "WR", na.rm = TRUE) > 0L,
  sum(panel$position_group == "TE", na.rm = TRUE) > 0L
)

cat("\n  Done.\n\n")

# ------------------------------------------------------------------------------
# STEP 4: Year-over-year deltas
# ------------------------------------------------------------------------------

cat("Step 4: Computing year-over-year deltas ...\n")

deltas <- compute_age_deltas(
  panel_with_ages    = panel,
  metric_col         = "fp_per_game",
  positions          = ALL_POSITIONS,
  age_min            = AGE_MIN,
  age_max            = AGE_MAX,
  min_career_seasons = MIN_CAREER_SEASONS,
  filter_low_volume  = TRUE
)

cat(glue("  Total transitions (all positions): {format(nrow(deltas), big.mark = ',')}\n"))
cat(glue("  Seasons covered: {min(deltas$season)} to {max(deltas$season)}\n\n"))

# ------------------------------------------------------------------------------
# STEP 5: Per-age summary, replicating fit_aging_curves() line 858-874
# ------------------------------------------------------------------------------

age_summary_for <- function(pos) {
  deltas %>%
    dplyr::filter(position_group == pos) %>%
    dplyr::group_by(age_at_season_start) %>%
    dplyr::summarise(
      mean_delta = mean(delta, na.rm = TRUE),
      sd_delta   = sd(delta, na.rm = TRUE),
      n_obs      = dplyr::n(),
      se_delta   = dplyr::if_else(n_obs > 1L, sd_delta / sqrt(n_obs), NA_real_),
      ci_lower   = dplyr::if_else(!is.na(se_delta), mean_delta - 1.96 * se_delta, NA_real_),
      ci_upper   = dplyr::if_else(!is.na(se_delta), mean_delta + 1.96 * se_delta, NA_real_),
      .groups    = "drop"
    ) %>%
    dplyr::arrange(age_at_season_start) %>%
    dplyr::mutate(
      position  = pos,
      sparse    = n_obs < MIN_AGE_OBS,
      ci_width  = ci_upper - ci_lower
    )
}

cat("--------------------------------------------------------------\n")
cat(glue("  PER-AGE SUMMARY: {FOCUS_POSITION}\n"))
cat("--------------------------------------------------------------\n")

focus <- age_summary_for(FOCUS_POSITION)

focus %>%
  dplyr::select(
    age = age_at_season_start,
    n_obs, mean_delta, sd_delta, se_delta, ci_width, sparse
  ) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\n")

# ------------------------------------------------------------------------------
# STEP 6: The question - is the widening driven by sd or by n?
# ------------------------------------------------------------------------------

cat("--------------------------------------------------------------\n")
cat(glue("  DRIVER CHECK: {FOCUS_POSITION}\n"))
cat("--------------------------------------------------------------\n")

nonsparse <- focus %>% dplyr::filter(!is.na(se_delta))

sd_min  <- min(nonsparse$sd_delta)
sd_max  <- max(nonsparse$sd_delta)
n_min   <- min(nonsparse$n_obs)
n_max   <- max(nonsparse$n_obs)
se_min  <- min(nonsparse$se_delta)
se_max  <- max(nonsparse$se_delta)

cat(glue("  sd_delta range : {round(sd_min, 3)} to {round(sd_max, 3)}  (ratio {round(sd_max / sd_min, 2)}x)\n"))
cat(glue("  n_obs range    : {n_min} to {n_max}  (ratio {round(n_max / n_min, 2)}x)\n"))
cat(glue("  se_delta range : {round(se_min, 3)} to {round(se_max, 3)}  (ratio {round(se_max / se_min, 2)}x)\n"))
cat(glue("  sqrt(n) ratio  : {round(sqrt(n_max / n_min), 2)}x\n\n"))

# Youngest well-populated age vs oldest well-populated age, side by side
age_lo <- nonsparse %>% dplyr::slice_max(n_obs, n = 1)
age_hi <- nonsparse %>% dplyr::filter(!sparse) %>% dplyr::slice_max(age_at_season_start, n = 1)

cat("  Two buckets side by side:\n")
cat(glue(
  "    Age {age_lo$age_at_season_start}: n = {age_lo$n_obs}, sd = {round(age_lo$sd_delta, 2)}, ",
  "se = {round(age_lo$se_delta, 3)}\n"
))
cat(glue(
  "    Age {age_hi$age_at_season_start}: n = {age_hi$n_obs}, sd = {round(age_hi$sd_delta, 2)}, ",
  "se = {round(age_hi$se_delta, 3)}\n\n"
))

# Counterfactual: hold sd at the older bucket's value, give it the younger n
cf_se <- age_hi$sd_delta / sqrt(age_lo$n_obs)
cat(glue(
  "  Counterfactual: age {age_hi$age_at_season_start}'s spread ({round(age_hi$sd_delta, 2)}) ",
  "with age {age_lo$age_at_season_start}'s sample size ({age_lo$n_obs})\n"
))
cat(glue("    se would be {round(cf_se, 3)} instead of {round(age_hi$se_delta, 3)}\n\n"))

# ------------------------------------------------------------------------------
# STEP 7: sd vs n correlation across ages (is spread actually rising?)
# ------------------------------------------------------------------------------

cat("--------------------------------------------------------------\n")
cat("  SPREAD VS AGE\n")
cat("--------------------------------------------------------------\n")

cor_sd_age <- cor(nonsparse$age_at_season_start, nonsparse$sd_delta)
cor_n_age  <- cor(nonsparse$age_at_season_start, nonsparse$n_obs)

cat(glue("  Correlation, age vs sd_delta : {round(cor_sd_age, 3)}\n"))
cat(glue("  Correlation, age vs n_obs    : {round(cor_n_age, 3)}\n\n"))

# ------------------------------------------------------------------------------
# STEP 8: All four positions, compact
# ------------------------------------------------------------------------------

cat("--------------------------------------------------------------\n")
cat("  ALL POSITIONS: n and se at the extremes\n")
cat("--------------------------------------------------------------\n")

all_summ <- purrr::map_dfr(ALL_POSITIONS, age_summary_for)

all_summ %>%
  dplyr::filter(!is.na(se_delta)) %>%
  dplyr::group_by(position) %>%
  dplyr::summarise(
    ages         = dplyr::n(),
    transitions  = sum(n_obs),
    n_min        = min(n_obs),
    n_max        = max(n_obs),
    sd_min       = round(min(sd_delta), 2),
    sd_max       = round(max(sd_delta), 2),
    se_min       = round(min(se_delta), 3),
    se_max       = round(max(se_delta), 3),
    .groups      = "drop"
  ) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\n")

# ------------------------------------------------------------------------------
# STEP 9: Sanity assertions
# ------------------------------------------------------------------------------

stopifnot(
  nrow(focus) > 0L,
  setequal(unique(all_summ$position), ALL_POSITIONS),
  all(nonsparse$se_delta > 0),
  all(nonsparse$se_delta <= nonsparse$sd_delta),
  isTRUE(all.equal(
    nonsparse$se_delta,
    nonsparse$sd_delta / sqrt(nonsparse$n_obs)
  ))
)

cat("All assertions passed.\n")
cat("==============================================================\n")
cat("  Pull complete. Paste the full console output back.\n")
cat("==============================================================\n\n")
