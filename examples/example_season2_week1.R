# ==============================================================================
# NFL Analytics Toolkit - Season 2, Week 1
# Usage Examples
# File: examples/example_season2_week1.R
#
# Purpose: 7 self-contained examples demonstrating all Week 1 functions.
#          Each section can be run independently after sourcing the
#          production file and running the pipeline at least once.
#
# Prerequisites:
#   - R/15_multi_season_pbp.R sourced
#   - All 16 seasons cached via load_multi_season_pbp() or run_week1_pipeline()
#
# Production-grade for portfolio display
# Built for personal analytics use
# ==============================================================================

library(here)
library(dplyr)
library(glue)

# Source production file
source(here::here("R", "15_multi_season_pbp.R"))


# ==============================================================================
# EXAMPLE 1: Load a Single Season and Inspect Structure
# ==============================================================================
# The most common downstream use case. Load one season of normalized data
# and verify it has the expected structure before doing any analysis.
# ==============================================================================

cat("\n================================================================\n")
cat("EXAMPLE 1: Load a Single Season and Inspect Structure\n")
cat("================================================================\n\n")

pbp_2025 <- load_normalized_season(2025)

cat("Rows:", format(nrow(pbp_2025), big.mark = ","), "\n")
cat("Columns:", ncol(pbp_2025), "\n")
cat("Games:", length(unique(pbp_2025$game_id)), "\n")
cat("Weeks:", paste(sort(unique(pbp_2025$week)), collapse = ", "), "\n")
cat("Teams:", length(unique(pbp_2025$posteam[!is.na(pbp_2025$posteam)])), "\n")
cat("Schema version:", unique(pbp_2025$season_norm_version), "\n\n")

# Verify core columns exist
core_check <- c("epa", "success", "wp", "play_type", "posteam", "defteam",
                "passer_player_id", "rusher_player_id", "receiver_player_id")
for (col in core_check) {
  cat(glue("  {col}: {ifelse(col %in% names(pbp_2025), 'present', 'MISSING')}"), "\n")
}

# Key insight: Every normalized season has the same column interface.
# You can write downstream functions once and they work on any season
# from 2010 to 2025 without column-existence guards.

rm(pbp_2025)
gc(verbose = FALSE)


# ==============================================================================
# EXAMPLE 2: Compare Pass vs Run EPA Across Two Seasons
# ==============================================================================
# Demonstrates loading two seasons and comparing a fundamental metric.
# EPA (Expected Points Added = EP_after - EP_before) should be higher
# for pass plays than run plays on average -- this is one of the most
# robust findings in NFL analytics.
# ==============================================================================

cat("\n================================================================\n")
cat("EXAMPLE 2: Compare Pass vs Run EPA Across Two Seasons\n")
cat("================================================================\n\n")

for (s in c(2015, 2025)) {
  pbp <- load_normalized_season(s)

  epa_by_type <- pbp %>%
    filter(
      !is.na(epa),
      !is.na(posteam),
      play_type %in% c("pass", "run"),
      # Exclude garbage time
      !is.na(wp), wp >= 0.10, wp <= 0.90
    ) %>%
    group_by(play_type) %>%
    summarise(
      n_plays = n(),
      mean_epa = round(mean(epa, na.rm = TRUE), 4),
      success_rate = round(mean(success == 1, na.rm = TRUE), 3),
      .groups = "drop"
    )

  cat(glue("Season {s}:"), "\n")
  for (i in seq_len(nrow(epa_by_type))) {
    cat(glue("  {epa_by_type$play_type[i]}: mean EPA = {epa_by_type$mean_epa[i]}, ",
             "success rate = {epa_by_type$success_rate[i]}, ",
             "n = {format(epa_by_type$n_plays[i], big.mark = ',')}"), "\n")
  }
  cat("\n")

  rm(pbp, epa_by_type)
  gc(verbose = FALSE)
}

# Key insight: Pass EPA consistently exceeds run EPA across seasons.
# This underpins the "passing is more efficient" finding in modern analytics.
# The gap size may vary year to year but the direction is stable.


# ==============================================================================
# EXAMPLE 3: Validate Season Coverage
# ==============================================================================
# Run the built-in validation on a subset of seasons.
# Checks game count, team count, plays per game, and EPA centering.
# ==============================================================================

cat("\n================================================================\n")
cat("EXAMPLE 3: Validate Season Coverage\n")
cat("================================================================\n\n")

coverage <- validate_season_coverage(seasons = 2020:2025)

cat("Coverage validation results:\n\n")
for (i in seq_len(nrow(coverage))) {
  row <- coverage[i, ]
  status <- ifelse(row$all_pass, "PASS", "FAIL")
  cat(glue("  {row$season}: {status} | games={row$n_games} (expected {row$expected_games}) | ",
           "teams={row$n_teams} | plays/game={row$median_plays_per_game} | ",
           "mean EPA={row$mean_epa}"), "\n")
}

# Key insight: The 2020 season had COVID-related scheduling changes.
# The 2021 season was the first with 272 games (17-game schedule).
# Both should still pass validation thresholds if the pipeline is correct.

cat("\n")
rm(coverage)


# ==============================================================================
# EXAMPLE 4: Check Schema Differences Across Eras
# ==============================================================================
# Uses get_schema_differences() to find columns that are NOT available
# in all 16 seasons. Critical for Phase 3 modeling decisions: if a feature
# only exists from 2016 onward, aging curves cannot use it for the full span.
# ==============================================================================

cat("\n================================================================\n")
cat("EXAMPLE 4: Check Schema Differences Across Eras\n")
cat("================================================================\n\n")

diffs <- get_schema_differences()

total_cols <- nrow(diffs)
season_cols <- names(diffs)[grepl("^s\\d{4}$", names(diffs))]
n_seasons <- length(season_cols)

# Columns present in ALL seasons
all_seasons <- diffs %>%
  mutate(n_present = rowSums(across(all_of(season_cols)))) %>%
  filter(n_present == n_seasons)

# Columns present in FEWER than all seasons
partial <- diffs %>%
  mutate(n_present = rowSums(across(all_of(season_cols)))) %>%
  filter(n_present < n_seasons) %>%
  arrange(n_present)

cat(glue("Total unique columns across all seasons: {total_cols}"), "\n")
cat(glue("Columns available in all {n_seasons} seasons: {nrow(all_seasons)}"), "\n")
cat(glue("Columns with partial coverage: {nrow(partial)}"), "\n\n")

if (nrow(partial) > 0) {
  cat("Partial coverage columns (available in fewest seasons first):\n")
  for (i in seq_len(min(15, nrow(partial)))) {
    row <- partial[i, ]
    n_present <- sum(row[season_cols] == TRUE)
    cat(glue("  {row$column_name}: {n_present}/{n_seasons} seasons"), "\n")
  }
}

# Key insight: normalize_schema() backfills missing optional columns as NA.
# So all columns technically "exist" in every season, but their VALUES
# are NA for earlier seasons. The schema differences output shows true
# native availability before normalization.

cat("\n")
rm(diffs, all_seasons, partial)


# ==============================================================================
# EXAMPLE 5: Validate EPA Distributions
# ==============================================================================
# EPA should be centered near zero in aggregate for every season.
# A non-centered distribution would indicate a problem with nflfastR's
# EP model for that season or a data quality issue in our pipeline.
# ==============================================================================

cat("\n================================================================\n")
cat("EXAMPLE 5: Validate EPA Distributions\n")
cat("================================================================\n\n")

epa_check <- validate_epa_distribution(seasons = 2010:2025)

cat("EPA distribution validation:\n\n")
cat(glue("  Seasons within tolerance (|mean| < 0.05): ",
         "{sum(epa_check$within_tolerance)}/{nrow(epa_check)}"), "\n\n")

# Show any seasons outside tolerance
outside <- epa_check %>% filter(!within_tolerance)
if (nrow(outside) > 0) {
  cat("  Seasons outside tolerance:\n")
  for (i in seq_len(nrow(outside))) {
    cat(glue("    {outside$season[i]}: mean EPA = {outside$mean_epa[i]}"), "\n")
  }
} else {
  cat("  All seasons within tolerance. EP model is stable across the full span.\n")
}

# Show range of means
cat(glue("\n  EPA mean range: {min(epa_check$mean_epa, na.rm = TRUE)} to ",
         "{max(epa_check$mean_epa, na.rm = TRUE)}"), "\n")
cat(glue("  EPA SD range: {min(epa_check$sd_epa, na.rm = TRUE)} to ",
         "{max(epa_check$sd_epa, na.rm = TRUE)}"), "\n")

# Key insight: If EPA drifts away from zero over time, it could mean
# nflfastR's EP model was retrained on different data for different
# season ranges. For our purposes, we need this to be stable so that
# cross-season comparisons are valid in Phase 3 aging curves.

cat("\n")
rm(epa_check, outside)


# ==============================================================================
# EXAMPLE 6: Team Relocation Verification
# ==============================================================================
# Three NFL teams relocated during the 2010-2025 window:
#   STL -> LA (2016), SD -> LAC (2017), OAK -> LV (2020)
# normalize_schema() maps old abbreviations to new ones.
# This example confirms no old abbreviations survive in any season.
# ==============================================================================

cat("\n================================================================\n")
cat("EXAMPLE 6: Team Relocation Verification\n")
cat("================================================================\n\n")

old_abbrs <- c("OAK", "SD", "STL")
problems_found <- FALSE

for (s in SEASON_RANGE_DEFAULT) {
  pbp <- load_normalized_season(s)

  old_in_posteam <- intersect(old_abbrs, unique(pbp$posteam))
  old_in_defteam <- intersect(old_abbrs, unique(pbp$defteam))

  if (length(old_in_posteam) > 0 || length(old_in_defteam) > 0) {
    cat(glue("  {s}: FOUND old abbreviations - posteam: [{paste(old_in_posteam, collapse=', ')}], ",
             "defteam: [{paste(old_in_defteam, collapse=', ')}]"), "\n")
    problems_found <- TRUE
  }

  rm(pbp)
  gc(verbose = FALSE)
}

if (!problems_found) {
  cat("  All 16 seasons clean. No OAK, SD, or STL found in any season.\n")
  cat("  Relocations mapped correctly: OAK->LV, SD->LAC, STL->LA\n")
}

# Key insight: This matters for any team-level aggregation downstream.
# If old abbreviations survived, a query for LV's history would miss
# their Oakland years. The normalization ensures team identity is
# consistent across the full 16-year span.

cat("\n")


# ==============================================================================
# EXAMPLE 7: Memory-Efficient Multi-Season Analysis Pattern
# ==============================================================================
# When analyzing across multiple seasons, do NOT load all 16 into memory
# at once. Instead, iterate one season at a time, extract what you need,
# free memory, then combine results. This is the pattern used by every
# downstream Phase 3+ script.
# ==============================================================================

cat("\n================================================================\n")
cat("EXAMPLE 7: Memory-Efficient Multi-Season Analysis Pattern\n")
cat("================================================================\n\n")

# Example: calculate league-wide completion percentage per season
# without ever holding more than one season in memory

season_results <- list()

for (s in SEASON_RANGE_DEFAULT) {
  pbp <- load_normalized_season(s)

  result <- pbp %>%
    filter(
      play_type == "pass",
      !is.na(complete_pass),
      !is.na(posteam),
      # Exclude spikes and throwaways from completion rate
      !grepl("spike", tolower(desc))
    ) %>%
    summarise(
      season = first(season),
      n_attempts = n(),
      completions = sum(complete_pass == 1, na.rm = TRUE),
      completion_pct = round(completions / n_attempts * 100, 1),
      .groups = "drop"
    )

  season_results[[as.character(s)]] <- result

  # Free memory before next season
  rm(pbp, result)
  gc(verbose = FALSE)
}

comp_pct <- bind_rows(season_results)

cat("League-wide completion percentage by season:\n\n")
for (i in seq_len(nrow(comp_pct))) {
  row <- comp_pct[i, ]
  bar <- paste(rep("#", round(row$completion_pct / 2)), collapse = "")
  cat(glue("  {row$season}: {row$completion_pct}% ({format(row$completions, big.mark = ',')} / ",
           "{format(row$n_attempts, big.mark = ',')}) {bar}"), "\n")
}

# Key insight: Completion percentage has trended upward across the NFL
# from roughly 57% in 2010 to about 60% in recent seasons. This is driven
# by rule changes favoring receivers, scheme evolution toward short
# passing, and improved QB development. This trend is important context
# for any cross-era player comparison in Phase 3.

cat("\n")
rm(season_results, comp_pct)
gc(verbose = FALSE)


# ==============================================================================
# BEST PRACTICES SUMMARY
# ==============================================================================

cat("================================================================\n")
cat("Best Practices from Week 1 Examples\n")
cat("================================================================\n\n")
cat("1. Always use load_normalized_season() for single-season access.\n")
cat("   It reads from cache and throws a clear error if missing.\n\n")
cat("2. Filter garbage time (WP 10%-90%) for any efficiency metric.\n")
cat("   Raw EPA includes desperation plays that distort averages.\n\n")
cat("3. Iterate one season at a time for multi-season work.\n")
cat("   rm() + gc() between seasons keeps memory under control.\n\n")
cat("4. Verify column existence before use, but after normalization\n")
cat("   all key columns are guaranteed present (values may be NA\n")
cat("   for optional columns in earlier seasons).\n\n")
cat("5. Check validate_season_coverage() and validate_epa_distribution()\n")
cat("   before trusting cross-season comparisons.\n\n")
cat("6. Old team abbreviations (OAK, SD, STL) are mapped automatically.\n")
cat("   Use current abbreviations (LV, LAC, LA) in all downstream code.\n\n")
cat("7. The schema normalization version tag (season_norm_version)\n")
cat("   lets you detect if cached data was built by an older pipeline.\n")
cat("================================================================\n")
