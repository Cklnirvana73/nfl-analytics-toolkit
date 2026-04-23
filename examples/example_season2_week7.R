# ==============================================================================
# NFL Analytics Toolkit - Season 2, Week 7
# Usage Examples
# File: examples/example_season2_week7.R
#
# Purpose: Self-contained examples demonstrating all Week 7 functions.
#          Each step can be inspected independently. Steps 1-3 use a
#          single recent season for speed; Steps 4-7 build and validate
#          the full 12-season panel.
#
# Prerequisites:
#   - R/20_multi_season_cfb_pbp.R sourced (Week 6)
#   - CFB cache populated at data/season2_cfb_cache/ via
#     load_multi_season_cfb_pbp(seasons = 2014:2025)
#   - R/21_cfb_player_season_panel.R present
#
# Execution order (Season 2 standard):
#   1. examples/example_season2_week6.R  (Week 6 pipeline must have run)
#   2. THIS FILE
#   3. tests/test_season2_week7_functions.R
#   4. examples/create_season2_week7_visuals.R
#
# Schema tag: s2cfbv1_panel
# ==============================================================================

library(cfbfastR)
library(dplyr)
library(tibble)
library(glue)
library(here)

# Source Week 6 (loads load_normalized_cfb_season and constants).
source(here::here("R", "20_multi_season_cfb_pbp.R"))

# Source Week 7 (loads build_cfb_player_season_panel and companions).
source(here::here("R", "21_cfb_player_season_panel.R"))


# ==============================================================================
# STEP 1: Single-season smoke test
# ==============================================================================
# Load one recent season and confirm the panel builds without error.
# This is the fastest development check: one season takes seconds vs
# the full 12-season run that takes several minutes.
# ==============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("STEP 1: Single-season smoke test (2024)\n")
cat(strrep("=", 70), "\n\n", sep = "")

SEASON_CHECK <- 2024L

panel_single <- build_cfb_player_season_panel(
  seasons            = SEASON_CHECK,
  include_postseason = FALSE,
  garbage_time_filter = TRUE,
  verbose            = TRUE
)

cat(glue("\nPanel rows      : {format(nrow(panel_single), big.mark = ',')}"), "\n")
cat(glue("Panel columns   : {ncol(panel_single)}"), "\n")
cat(glue("Schema version  : {unique(panel_single$panel_version)}"), "\n")
cat(glue("Season present  : {unique(panel_single$season)}"), "\n")
cat(glue("Unique players  : {format(dplyr::n_distinct(panel_single$player_name), big.mark = ',')}"), "\n\n")

# Verify canonical column presence.
expected_cols <- c(
  "player_name", "season", "primary_team", "n_teams", "has_name_collision",
  "position_group", "games_played", "total_plays", "low_volume",
  "pass_attempts", "completions", "completion_pct",
  "passing_yards", "pass_tds", "interceptions",
  "pass_epa", "pass_epa_per_attempt",
  "rush_attempts", "rushing_yards", "rush_tds",
  "rush_epa", "rush_epa_per_attempt",
  "targets", "receptions", "catch_rate",
  "receiving_yards", "rec_tds",
  "rec_epa", "rec_epa_per_target",
  "success_rate", "garbage_time_plays_excluded", "panel_version"
)

missing_cols <- setdiff(expected_cols, names(panel_single))
if (length(missing_cols) == 0L) {
  cat("All 31 canonical columns present.\n")
} else {
  cat(glue("MISSING columns: {paste(missing_cols, collapse = ', ')}"), "\n")
}

# KEY INSIGHT: Play counts and player counts computed from data.
n_above_vol <- sum(!panel_single$low_volume, na.rm = TRUE)
cat(glue(
  "\nPlayers above min_plays threshold : {format(n_above_vol, big.mark = ',')}",
  " ({round(n_above_vol / nrow(panel_single) * 100, 1)}% of panel)\n"
))

rm(panel_single)
gc(verbose = FALSE)


# ==============================================================================
# STEP 2: Position classification spot check
# ==============================================================================
# Verify that classify_cfb_player_position() assigns sensible groups to
# known high-profile players. These checks catch threshold miscalibration
# and play_type attribution errors early.
# ==============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("STEP 2: Position classification spot check (2024)\n")
cat(strrep("=", 70), "\n\n", sep = "")

panel_pos <- build_cfb_player_season_panel(
  seasons = SEASON_CHECK,
  verbose = FALSE
)

pos_distribution <- table(panel_pos$position_group, useNA = "ifany")
cat("Position group distribution (2024):\n")
for (nm in names(pos_distribution)) {
  label <- if (is.na(nm)) "NA" else nm
  cat(glue("  {label}: {pos_distribution[[nm]]}"), "\n")
}

cat("\nTop QBs by pass_epa (2024, > 100 pass attempts):\n")
top_qb <- panel_pos %>%
  filter(
    position_group == "QB",
    pass_attempts > 100L,
    !low_volume
  ) %>%
  arrange(desc(pass_epa)) %>%
  select(player_name, primary_team, pass_attempts, pass_epa,
         pass_epa_per_attempt, pass_tds, interceptions) %>%
  slice_head(n = 10)
print(top_qb, n = 10)

cat("\nTop RBs by rush_epa (2024, > 50 rush attempts):\n")
top_rb <- panel_pos %>%
  filter(
    position_group == "RB",
    rush_attempts > 50L,
    !low_volume
  ) %>%
  arrange(desc(rush_epa)) %>%
  select(player_name, primary_team, rush_attempts, rush_epa,
         rush_epa_per_attempt, rush_tds) %>%
  slice_head(n = 10)
print(top_rb, n = 10)

cat("\nTop receivers by rec_epa_per_target (2024, > 30 targets):\n")
top_wr <- panel_pos %>%
  filter(
    position_group == "WR_TE",
    targets > 30L,
    !low_volume
  ) %>%
  arrange(desc(rec_epa_per_target)) %>%
  select(player_name, primary_team, targets, receptions,
         catch_rate, receiving_yards, rec_epa_per_target) %>%
  slice_head(n = 10)
print(top_wr, n = 10)

rm(panel_pos, top_qb, top_rb, top_wr, pos_distribution)
gc(verbose = FALSE)


# ==============================================================================
# STEP 3: Multi-team player (transfer) handling check
# ==============================================================================
# Confirm that players who transferred mid-season are collapsed to one row
# and that n_teams > 1 is correctly flagged.
# ==============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("STEP 3: Transfer / multi-team player handling (2024)\n")
cat(strrep("=", 70), "\n\n", sep = "")

panel_transfer <- build_cfb_player_season_panel(
  seasons = SEASON_CHECK,
  verbose = FALSE
)

multi_team <- panel_transfer %>%
  filter(n_teams > 1L) %>%
  arrange(desc(n_teams), player_name) %>%
  select(player_name, season, primary_team, n_teams,
         has_name_collision, position_group, total_plays)

cat(glue(
  "Players with n_teams > 1 in {SEASON_CHECK}: ",
  "{nrow(multi_team)}\n"
))

if (nrow(multi_team) > 0L) {
  cat("\nSample multi-team rows:\n")
  print(head(multi_team, 15), n = 15)

  n_collision_flagged <- sum(multi_team$has_name_collision)
  cat(glue(
    "\nhas_name_collision flagged (n_teams >= 3): {n_collision_flagged}\n"
  ))
}

# Confirm no duplicate player_name + season combinations exist.
dup_check <- panel_transfer %>%
  count(player_name, season) %>%
  filter(n > 1L)

if (nrow(dup_check) == 0L) {
  cat("\nDuplicate check: PASS -- one row per player_name+season.\n")
} else {
  cat(glue(
    "\nDuplicate check: FAIL -- {nrow(dup_check)} duplicates found.\n"
  ))
  print(head(dup_check, 10))
}

rm(panel_transfer, multi_team, dup_check)
gc(verbose = FALSE)


# ==============================================================================
# STEP 4: Garbage time filter impact
# ==============================================================================
# Compare play counts with and without the garbage time filter to confirm
# it is removing a meaningful share of plays without gutting the data.
# ==============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("STEP 4: Garbage time filter impact (2024)\n")
cat(strrep("=", 70), "\n\n", sep = "")

panel_filtered   <- build_cfb_player_season_panel(
  seasons             = SEASON_CHECK,
  garbage_time_filter = TRUE,
  verbose             = FALSE
)

panel_unfiltered <- build_cfb_player_season_panel(
  seasons             = SEASON_CHECK,
  garbage_time_filter = FALSE,
  verbose             = FALSE
)

# Use the garbage_time_plays_excluded column computed during aggregation.
n_excluded <- unique(panel_filtered$garbage_time_plays_excluded)
cat(glue(
  "Garbage time plays excluded ({SEASON_CHECK}): ",
  "{format(n_excluded, big.mark = ',')}\n"
))

# Compare EPA distributions between filtered and unfiltered for receivers.
epa_filtered   <- mean(panel_filtered$rec_epa_per_target,   na.rm = TRUE)
epa_unfiltered <- mean(panel_unfiltered$rec_epa_per_target, na.rm = TRUE)

cat(glue(
  "Mean rec_epa_per_target (filtered)  : {round(epa_filtered,   4)}\n"
))
cat(glue(
  "Mean rec_epa_per_target (unfiltered): {round(epa_unfiltered, 4)}\n"
))
cat(glue(
  "Expected direction: filtered > unfiltered (garbage time inflates targets\n",
  "  for backups playing against prevent defense, suppressing efficiency).\n"
))

rm(panel_filtered, panel_unfiltered)
gc(verbose = FALSE)


# ==============================================================================
# STEP 5: Full 12-season panel build
# ==============================================================================
# Build the complete panel across all 12 cached seasons. This is the
# primary deliverable for Week 7 and the input to Phase 3 models.
# First run: several minutes. Subsequent: reads from cache in seconds.
# ==============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("STEP 5: Full 12-season panel (2014-2025)\n")
cat(strrep("=", 70), "\n\n", sep = "")

panel_full <- build_cfb_player_season_panel(
  seasons             = 2014:2025,
  include_postseason  = FALSE,
  garbage_time_filter = TRUE,
  verbose             = TRUE
)

cat(glue("\n--- Full panel summary ---\n"))
cat(glue("Total rows       : {format(nrow(panel_full), big.mark = ',')}\n"))
cat(glue(
  "Seasons covered  : {min(panel_full$season, na.rm = TRUE)}-",
  "{max(panel_full$season, na.rm = TRUE)}\n"
))
cat(glue(
  "Unique players   : ",
  "{format(dplyr::n_distinct(panel_full$player_name), big.mark = ',')}\n"
))

# Season-by-season row counts to confirm no season was silently dropped.
cat("\nRows per season:\n")
season_counts <- panel_full %>%
  count(season, name = "n_player_seasons") %>%
  arrange(season)
print(season_counts, n = nrow(season_counts))


# ==============================================================================
# STEP 6: Run validate_cfb_panel_integrity()
# ==============================================================================
# Runs all integrity checks on the full panel. All critical checks must
# pass before the panel is used in downstream Phase 3 models.
# ==============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("STEP 6: Panel integrity validation\n")
cat(strrep("=", 70), "\n\n", sep = "")

validation <- validate_cfb_panel_integrity(
  panel            = panel_full,
  expected_seasons = 2014:2025
)

cat(glue("\nvalidation$valid: {validation$valid}\n\n"))

# Print any non-passing checks at detail level.
failures <- validation$summary %>% filter(!passed)
if (nrow(failures) == 0L) {
  cat("All checks passed.\n")
} else {
  cat(glue("{nrow(failures)} check(s) did not pass:\n"))
  for (j in seq_len(nrow(failures))) {
    row <- failures[j, ]
    cat(glue("  [{row$severity}] {row$check_name}: {row$detail}\n"))
  }
}

stopifnot(
  "validate_cfb_panel_integrity() failed a critical check." =
    isTRUE(validation$valid)
)
cat("\nstopifnot: PASS -- all critical checks passed.\n")


# ==============================================================================
# STEP 7: Cross-season efficiency signal quality check
# ==============================================================================
# Confirm that EPA efficiency columns produce stable, interpretable
# distributions across the 12-season range. This is a sanity check that
# the aggregation logic and garbage time filter are working as expected.
# ==============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("STEP 7: Cross-season EPA signal quality\n")
cat(strrep("=", 70), "\n\n", sep = "")

# QB pass EPA per attempt by season -- should be near-zero each year
# (EPA is zero-sum across the league; slight positive expected because
# FBS offenses face FBS defenses but only FBS-offensive plays are kept).
cat("QB pass_epa_per_attempt by season (volume-filtered QBs, > 100 attempts):\n")
qb_epa_by_season <- panel_full %>%
  filter(
    position_group == "QB",
    pass_attempts  > 100L,
    !is.na(pass_epa_per_attempt)
  ) %>%
  group_by(season) %>%
  summarise(
    n_qbs              = n(),
    mean_pass_epa_pa   = round(mean(pass_epa_per_attempt, na.rm = TRUE), 4),
    median_pass_epa_pa = round(median(pass_epa_per_attempt, na.rm = TRUE), 4),
    sd_pass_epa_pa     = round(sd(pass_epa_per_attempt, na.rm = TRUE), 4),
    .groups = "drop"
  ) %>%
  arrange(season)
print(qb_epa_by_season, n = nrow(qb_epa_by_season))

# Receiver catch_rate by season -- should be broadly stable.
cat("\nWR_TE catch_rate by season (> 30 targets):\n")
wr_catch_by_season <- panel_full %>%
  filter(
    position_group == "WR_TE",
    targets > 30L,
    !is.na(catch_rate)
  ) %>%
  group_by(season) %>%
  summarise(
    n_receivers      = n(),
    mean_catch_rate  = round(mean(catch_rate,  na.rm = TRUE), 3),
    mean_rec_epa_pt  = round(mean(rec_epa_per_target, na.rm = TRUE), 4),
    .groups = "drop"
  ) %>%
  arrange(season)
print(wr_catch_by_season, n = nrow(wr_catch_by_season))

# Success rate distribution (all positions, above min_plays).
cat("\nSuccess rate summary (all positions, !low_volume):\n")
sr_summary <- panel_full %>%
  filter(!low_volume, !is.na(success_rate)) %>%
  summarise(
    n            = n(),
    mean_sr      = round(mean(success_rate),   3),
    median_sr    = round(median(success_rate), 3),
    sd_sr        = round(sd(success_rate),     3),
    min_sr       = round(min(success_rate),    3),
    max_sr       = round(max(success_rate),    3),
    pct_above_50 = round(mean(success_rate > 0.50) * 100, 1)
  )
print(sr_summary)

cat(glue(
  "\nKEY INSIGHT: success_rate above 50% for ",
  "{sr_summary$pct_above_50}% of non-low-volume player-seasons.\n",
  "Mean success_rate: {sr_summary$mean_sr}. ",
  "EPA > 0 on roughly half of all plays is expected; the distribution\n",
  "is not centered at 0.50 because EPA itself is not zero-sum at the\n",
  "play level after garbage time filtering.\n"
))


# ==============================================================================
# STEP 8: Save full panel to output
# ==============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("STEP 8: Save full panel to output\n")
cat(strrep("=", 70), "\n\n", sep = "")

output_dir <- here::here("output", "season2_week7")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

panel_rds <- file.path(output_dir, "s2_week7_cfb_player_panel.rds")
saveRDS(panel_full, panel_rds)
cat(glue("Saved panel RDS : {panel_rds}\n"))

# Also write a lightweight CSV summary for quick inspection.
summary_csv <- file.path(output_dir, "s2_week7_panel_summary.csv")
panel_summary <- panel_full %>%
  filter(!low_volume) %>%
  select(
    player_name, season, primary_team, position_group, games_played,
    total_plays, pass_attempts, pass_epa, pass_epa_per_attempt,
    rush_attempts, rush_epa, rush_epa_per_attempt,
    targets, receptions, catch_rate, receiving_yards, rec_epa_per_target,
    success_rate
  ) %>%
  arrange(season, desc(total_plays))
write.csv(panel_summary, summary_csv, row.names = FALSE)
cat(glue("Saved summary CSV: {summary_csv}\n"))

cat("\n", strrep("=", 70), "\n", sep = "")
cat("Example script complete.\n")
cat("Next: tests/test_season2_week7_functions.R\n")
cat(strrep("=", 70), "\n\n", sep = "")
