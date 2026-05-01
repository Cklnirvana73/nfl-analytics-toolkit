# ==============================================================================
# examples/example_season2_week8.R
# Worked examples for R/22_sos_reconciliation.R
#
# Execution order (project standard): example -> tests -> visuals
# Run this script first to confirm the pipeline works end-to-end.
#
# Prerequisites:
#   - R/19_sleeper_api.R exists and is sourced by R/22
#   - R/20_multi_season_cfb_pbp.R exists and CFB cache is populated
#   - R/21 panel saved at data/season2_cfb_cache/s2_week7_cfb_panel.rds
#   - Sleeper league IDs available (use get_user_leagues() to discover)
#
# This script does NOT run the full pipeline (which requires live API calls
# and a populated cache). It demonstrates each function in isolation using
# a subset of seasons and documents expected outputs.
# ==============================================================================

library(dplyr)
library(glue)
library(here)

source(here::here("R", "22_sos_reconciliation.R"))


# ==============================================================================
# STEP 1: Confirm source guards loaded correctly
# ==============================================================================

cat("\n--- Step 1: Source guard verification ---\n")

stopifnot(
  "match_sleeper_players must be available from R/19" =
    exists("match_sleeper_players", mode = "function"),
  "load_normalized_cfb_season must be available from R/20" =
    exists("load_normalized_cfb_season", mode = "function")
)
cat("Source guards OK: R/19 and R/20 functions available.\n")

stopifnot(
  exists("build_cfb_sos_features",      mode = "function"),
  exists("build_sleeper_gsis_crosswalk", mode = "function"),
  exists("validate_sos_crosswalk",       mode = "function"),
  exists("run_week8_pipeline",           mode = "function")
)
cat("R/22 exported functions available.\n")


# ==============================================================================
# STEP 2: Load R/21 CFB panel -- build if not found
# ==============================================================================

cat("\n--- Step 2: Load R/21 CFB panel ---\n")

panel_path <- here::here("output", "season2_week7", "s2_week7_cfb_player_panel.rds")

if (file.exists(panel_path)) {
  panel <- readRDS(panel_path)
  cat(glue("Loaded from cache: {panel_path}\n"))
} else {
  cat(glue(
    "Panel not found at: {panel_path}\n",
    "Building panel via build_cfb_player_season_panel()...\n"
  ))

  # Source R/21 if not already loaded
  if (!exists("build_cfb_player_season_panel", mode = "function")) {
    r21_path <- here::here("R", "21_cfb_player_season_panel.R")
    if (!file.exists(r21_path)) {
      stop(glue(
        "R/21_cfb_player_season_panel.R not found at: {r21_path}\n",
        "This file is required to build the CFB player-season panel."
      ))
    }
    source(r21_path)
  }

  panel <- build_cfb_player_season_panel(
    seasons   = CFB_PANEL_SEASONS_DEFAULT,
    cache_dir = here::here("data", "season2_cfb_cache"),
    verbose   = TRUE
  )

  # Save to the same location R/21 uses
  out_dir <- here::here("output", "season2_week7")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  saveRDS(panel, panel_path)
  cat(glue("Panel built and saved: {panel_path}\n"))
}
cat(glue(
  "Panel loaded: {format(nrow(panel), big.mark=',')} player-seasons | ",
  "{dplyr::n_distinct(panel$player_name)} unique players | ",
  "seasons: {min(panel$season)}-{max(panel$season)}\n"
))

# Spot check: required columns for SOS join
stopifnot(
  "player_name must be in panel"  = "player_name"  %in% names(panel),
  "season must be in panel"       = "season"       %in% names(panel),
  "primary_team must be in panel" = "primary_team" %in% names(panel)
)
cat("Panel column check: OK\n")


# ==============================================================================
# STEP 3: Compute SOS features (recent seasons for speed)
# ==============================================================================

cat("\n--- Step 3: Build SOS features (2022-2024) ---\n")

# Use 3 recent seasons for demonstration. Full run uses 2014:2025.
EXAMPLE_SEASONS <- 2022:2024

panel_sos <- build_cfb_sos_features(
  panel     = panel %>% dplyr::filter(season %in% EXAMPLE_SEASONS),
  seasons   = EXAMPLE_SEASONS,
  cache_dir = here::here("data", "season2_cfb_cache"),
  verbose   = TRUE
)

# Verify output
stopifnot(
  "sos_computed column must exist"                    = "sos_computed" %in% names(panel_sos),
  "sos_opp_def_epa_per_play column must exist"        = "sos_opp_def_epa_per_play" %in% names(panel_sos),
  "sos_opp_def_success_rate_allowed column must exist" = "sos_opp_def_success_rate_allowed" %in% names(panel_sos),
  "sos_n_opponents column must exist"                 = "sos_n_opponents" %in% names(panel_sos),
  "row count unchanged by SOS join"                   = nrow(panel_sos) == nrow(panel %>% dplyr::filter(season %in% EXAMPLE_SEASONS))
)
cat("SOS feature join: OK\n")

n_computed <- sum(panel_sos$sos_computed, na.rm = TRUE)
pct_computed <- round(n_computed / nrow(panel_sos) * 100, 1)
cat(glue(
  "SOS computed: {format(n_computed, big.mark=',')} of ",
  "{format(nrow(panel_sos), big.mark=',')} player-seasons ({pct_computed}%)\n"
))

# KEY INSIGHT: computed from data, not hardcoded
cat("\n--- KEY INSIGHT: SOS distribution by position ---\n")
sos_summary <- panel_sos %>%
  dplyr::filter(sos_computed, !is.na(position_group)) %>%
  dplyr::group_by(position_group) %>%
  dplyr::summarise(
    n_players      = dplyr::n(),
    mean_opp_epa   = round(mean(sos_opp_def_epa_per_play, na.rm = TRUE), 4),
    sd_opp_epa     = round(sd(sos_opp_def_epa_per_play, na.rm = TRUE), 4),
    mean_n_opps    = round(mean(sos_n_opponents, na.rm = TRUE), 1),
    .groups        = "drop"
  )
print(sos_summary)


# ==============================================================================
# STEP 4: Hardest and easiest schedules -- QBs
# ==============================================================================

cat("\n--- Step 4: QB SOS extremes ---\n")

qb_sos <- panel_sos %>%
  dplyr::filter(
    position_group == "QB",
    !low_volume,
    sos_computed,
    sos_n_opponents >= 6L
  ) %>%
  dplyr::arrange(sos_opp_def_epa_per_play) %>%
  dplyr::select(player_name, season, primary_team,
                pass_epa_per_attempt, sos_opp_def_epa_per_play, sos_n_opponents)

cat("Hardest schedules (lowest opp EPA allowed = toughest defense):\n")
print(head(qb_sos, 5))

cat("\nEasiest schedules (highest opp EPA allowed = weakest defense):\n")
print(tail(qb_sos, 5))


# ==============================================================================
# STEP 5: Discover Sleeper leagues and build crosswalk
# ==============================================================================

cat("\n--- Step 5: Sleeper crosswalk ---\n")
cat("NOTE: Requires Sleeper API access. Skipping in dry-run mode.\n")
cat("To build the crosswalk, run:\n")
cat("  my_leagues <- get_user_leagues('your_sleeper_username', season = 2025L)\n")
cat("  crosswalk  <- build_sleeper_gsis_crosswalk(my_leagues$league_id)\n\n")

# Example inspection of a pre-built crosswalk (if it exists)
crosswalk_path <- here::here("data", "season2_cache", "s2_week8_id_crosswalk.rds")

if (file.exists(crosswalk_path)) {
  crosswalk <- readRDS(crosswalk_path)
  cat(glue(
    "Crosswalk found: {format(nrow(crosswalk), big.mark=',')} players\n"
  ))

  match_summary <- crosswalk %>%
    dplyr::count(match_method) %>%
    dplyr::mutate(pct = round(n / sum(n) * 100, 1))
  print(match_summary)

  skill_rate <- crosswalk %>%
    dplyr::filter(toupper(sleeper_position) %in% c("QB", "RB", "WR", "TE")) %>%
    dplyr::summarise(
      n_skill   = dplyr::n(),
      n_matched = sum(match_method != "unmatched"),
      rate      = round(n_matched / n_skill * 100, 1)
    )
  cat(glue("Skill position match rate: {skill_rate$rate}%\n"))
} else {
  cat("No crosswalk found at expected path. Run build_sleeper_gsis_crosswalk() first.\n")
}


# ==============================================================================
# STEP 6: Validate outputs
# ==============================================================================

cat("\n--- Step 6: Validate ---\n")

crosswalk_for_val <- if (file.exists(crosswalk_path)) {
  readRDS(crosswalk_path)
} else {
  NULL
}

vr <- validate_sos_crosswalk(
  sos_panel        = panel_sos,
  crosswalk        = crosswalk_for_val,
  expected_seasons = as.integer(EXAMPLE_SEASONS)
)

cat(glue("Validation overall valid: {vr$valid}\n"))
print(vr$summary[, c("check_name", "severity", "passed", "detail")])


# ==============================================================================
# STEP 7: Save example SOS panel (subset)
# ==============================================================================

cat("\n--- Step 7: Save example SOS panel ---\n")

example_output_path <- here::here(
  "data", "season2_cfb_cache", "s2_week8_cfb_sos_panel_example.rds"
)
saveRDS(panel_sos, example_output_path)
cat(glue("Saved example SOS panel: {example_output_path}\n"))

cat("\n=== example_season2_week8.R complete ===\n")
