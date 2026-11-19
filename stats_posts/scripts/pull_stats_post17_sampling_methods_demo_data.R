# ============================================================
# Statistics Series - Post 17: Sampling Methods
# Path A: Random vs. Stratified Sampling by Position
# ============================================================
#
# Purpose: Build the population of 2025 skill-position players, then
# compare simple random sampling against stratified sampling on
# position representation. Concept 17 is tagged [GAP] in the master
# outline (no NFL or WGU coverage), so this is a purpose-built demo,
# not an anchor pulled from an existing project script.
#
# Output: Console summary with population proportions, simple random
# sampling drift across repeated draws, one concrete example draw,
# and the stratified sample result for direct comparison.

library(dplyr)
library(nflreadr)

SEASON <- 2025L
N_SAMPLE <- 40L
N_DRAWS <- 1000L

# ---- Load player stats (defensive against nflreadr signature changes) ----
player_stats <- tryCatch(
  nflreadr::load_player_stats(seasons = SEASON, stat_type = "offense"),
  error = function(e) {
    message("stat_type argument failed, retrying without it: ", conditionMessage(e))
    nflreadr::load_player_stats(seasons = SEASON)
  }
)

# ---- Filter to regular season ----
player_stats <- player_stats %>%
  filter(season_type == "REG")

# ---- Resolve the team column name (varies across nflreadr versions) ----
team_col <- intersect(c("recent_team", "team"), names(player_stats))
if (length(team_col) == 0) {
  stop("Could not find a team column in player_stats. Columns present: ",
       paste(names(player_stats), collapse = ", "))
}
team_col <- team_col[1]
cat("Using team column:", team_col, "\n\n")

# ---- Build the population: one row per player ----
# Group by player_id only (never player_id + team) so a player traded
# mid-season does not get counted twice.
population <- player_stats %>%
  filter(position %in% c("QB", "RB", "WR", "TE")) %>%
  group_by(player_id) %>%
  summarise(
    player_name = dplyr::last(player_display_name),
    position = dplyr::last(position),
    team = dplyr::last(.data[[team_col]]),
    games_played = dplyr::n_distinct(week),
    .groups = "drop"
  ) %>%
  filter(games_played >= 1)

cat("=== POPULATION ===\n")
cat("Season:", SEASON, "\n")
cat("Total skill-position players (>=1 regular season game):", nrow(population), "\n\n")

pop_counts <- population %>%
  count(position) %>%
  mutate(proportion = round(n / sum(n), 4))

cat("Population by position:\n")
print(pop_counts)
cat("\n")

# Expected count per position if a sample of N_SAMPLE matched the
# population proportions exactly.
expected_counts <- pop_counts %>%
  mutate(expected_n = round(proportion * N_SAMPLE, 2)) %>%
  select(position, expected_n)

cat("Expected count per position at n =", N_SAMPLE, "(population proportion x sample size):\n")
print(expected_counts)
cat("\n")

# ---- Simple random sampling: repeated draws ----
set.seed(2017)
random_draws <- lapply(seq_len(N_DRAWS), function(i) {
  s <- population[sample(nrow(population), N_SAMPLE), ]
  s %>% count(position, name = "n") %>% mutate(draw = i)
})
random_draws_df <- bind_rows(random_draws)

all_combos <- expand.grid(
  draw = seq_len(N_DRAWS),
  position = c("QB", "RB", "WR", "TE"),
  stringsAsFactors = FALSE
)

random_draws_full <- all_combos %>%
  left_join(random_draws_df, by = c("draw", "position")) %>%
  mutate(n = ifelse(is.na(n), 0, n))

drift_summary <- random_draws_full %>%
  left_join(expected_counts, by = "position") %>%
  mutate(abs_dev = abs(n - expected_n)) %>%
  group_by(position) %>%
  summarise(
    expected_n = dplyr::first(expected_n),
    mean_n = round(mean(n), 2),
    min_n = min(n),
    max_n = max(n),
    mean_abs_dev = round(mean(abs_dev), 2),
    .groups = "drop"
  )

cat("=== SIMPLE RANDOM SAMPLING (n =", N_SAMPLE, ",", N_DRAWS, "repeated draws) ===\n")
print(drift_summary)
cat("\n")

# One concrete example draw, held out from the simulation loop, for
# narrative use in the post.
set.seed(99)
example_draw <- population[sample(nrow(population), N_SAMPLE), ] %>%
  count(position, name = "n") %>%
  right_join(expected_counts, by = "position") %>%
  mutate(
    n = ifelse(is.na(n), 0, n),
    deviation = round(n - expected_n, 2)
  )

cat("=== ONE EXAMPLE RANDOM DRAW (seed 99, n =", N_SAMPLE, ") ===\n")
print(example_draw)
cat("\n")

# ---- Stratified sampling: proportional allocation ----
strata_alloc <- pop_counts %>%
  mutate(
    exact_n = proportion * N_SAMPLE,
    stratum_n = round(exact_n)
  )

alloc_diff <- N_SAMPLE - sum(strata_alloc$stratum_n)
if (alloc_diff != 0) {
  idx <- which.max(strata_alloc$stratum_n)
  strata_alloc$stratum_n[idx] <- strata_alloc$stratum_n[idx] + alloc_diff
}

cat("=== STRATIFIED SAMPLE ALLOCATION (proportional, n =", N_SAMPLE, ") ===\n")
print(strata_alloc %>% select(position, proportion, stratum_n))
cat("\n")

set.seed(2017)
stratified_sample <- population %>%
  group_by(position) %>%
  group_modify(~ {
    k <- strata_alloc$stratum_n[strata_alloc$position == .y$position]
    .x[sample(nrow(.x), k), ]
  }) %>%
  ungroup()

stratified_counts <- stratified_sample %>%
  count(position, name = "n") %>%
  left_join(expected_counts, by = "position") %>%
  mutate(deviation = round(n - expected_n, 2))

cat("=== STRATIFIED SAMPLE RESULT ===\n")
print(stratified_counts)
cat("\n")

cat("=== SUMMARY ===\n")
cat("Population size:", nrow(population), "\n")
cat("Sample size per draw:", N_SAMPLE, "\n")
cat("Random draws simulated:", N_DRAWS, "\n")
cat(
  "Mean absolute deviation from expected count, averaged across positions (random):",
  round(mean(drift_summary$mean_abs_dev), 2), "\n"
)
cat(
  "Mean absolute deviation from expected count, averaged across positions (stratified):",
  round(mean(abs(stratified_counts$deviation)), 2), "\n"
)
