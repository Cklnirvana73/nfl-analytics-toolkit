# =============================================================================
# examples/example_season2_week2.R
# =============================================================================
# Season 2, Week 2: Player-Season Panel Construction -- Usage Examples
# NFL Analytics Toolkit | Production-grade for portfolio display
# =============================================================================
#
# PREREQUISITES
# -------------
# 1. R/15_multi_season_pbp.R must have been run to populate season2_cache/.
#    If the cache is empty:
#      source(here::here("R", "15_multi_season_pbp.R"))
#      load_multi_season_pbp(seasons = 2010:2025)
#
# 2. Source Week 2 functions before running any example:
#      source(here::here("R", "16_player_season_panel.R"))
#
# EXAMPLES IN THIS FILE
# ---------------------
#   Example 1: Classify positions for known problem cases
#   Example 2: Build a single-season panel (development mode)
#   Example 3: QB efficiency ranking from the panel
#   Example 4: Dual-threat RB analysis (rushing + receiving in one row)
#   Example 5: Position group distribution across 16 seasons
#   Example 6: Validate panel integrity after a full build
#   Example 7: Cross-season efficiency trend for one player
#
# =============================================================================

library(nflfastR)
library(nflreadr)
library(dplyr)
library(here)
library(glue)

source(here::here("R", "16_player_season_panel.R"))


# =============================================================================
# Example 1: Classify Positions for Known Problem Cases + Name Collision Flag
# =============================================================================
# The whole point of anchoring to nflreadr roster data is to correctly
# classify players like Lamar Jackson (QB who rushes more than many RBs)
# and Christian McCaffrey (RB who gets targeted like a WR).
#
# This example also demonstrates has_name_collision: a flag added to every
# player-season where the display name maps to more than one player_id in
# the same season. This is a recurring problem in football data -- two WRs
# named Michael Thomas were active simultaneously in 2019-2020; two players
# named Lamar Jackson exist in 2023 nflreadr data.
#
# The flag does not fix the problem. player_id (GSIS ID) is always the
# correct join key. The flag tells you when player_name alone is unsafe.

cat("\n--- Example 1: Position classification + name collision flag ---\n")

positions_2023 <- classify_player_position(seasons = 2023, verbose = FALSE)

# Show all name collisions in 2023 -- both players, both rows, flag visible
cat("Name collisions detected in 2023 (has_name_collision = TRUE):\n")
collisions_2023 <- positions_2023 %>%
  filter(has_name_collision == TRUE) %>%
  select(player_name, player_id, position, position_group, team) %>%
  arrange(player_name)
print(collisions_2023)

# Show the Lamar Jackson case explicitly -- two different people, same name
cat("\nThe Lamar Jackson case: two distinct player_ids, one shared name.\n")
lamar_rows <- positions_2023 %>%
  filter(player_name == "Lamar Jackson") %>%
  select(player_name, player_id, position, team, has_name_collision)
print(lamar_rows)

# Correct pattern: always filter by player_id, not player_name.
# To look up a player_id by name, use the collision-aware pattern below.
# Step 1: find all rows matching the name
# Step 2: disambiguate by position + team context
# Step 3: extract the player_id and use that for all downstream work
lamar_id <- positions_2023 %>%
  filter(player_name == "Lamar Jackson", position == "QB") %>%
  pull(player_id)

cat(glue("\nLamar Jackson (QB) GSIS ID: {lamar_id}\n"))
cat("Use this player_id for all panel joins, not the name string.\n")

# Verify position classifications for key complex cases
# Using player_id for the QB checks, team for simpler unambiguous cases
lamar_pos <- positions_2023 %>%
  filter(player_id == lamar_id) %>%
  pull(position)

cmc_pos <- positions_2023 %>%
  filter(player_name == "Christian McCaffrey", team == "SF") %>%
  pull(position)

kelce_group <- positions_2023 %>%
  filter(player_name == "Travis Kelce", team == "KC") %>%
  pull(position_group)

stopifnot(
  "Lamar Jackson (QB) must be QB, not RB" = length(lamar_pos) == 1L && lamar_pos == "QB",
  "McCaffrey (SF) must be RB, not WR"     = length(cmc_pos) == 1L && cmc_pos == "RB",
  "Kelce (KC) must be in WR_TE group"     = length(kelce_group) == 1L && kelce_group == "WR_TE"
)

cat("\nAll position checks passed.\n")
cat("NFL context: Roster position is the anchor, not play-level role.\n")
cat("Lamar's rush yards appear in rush_* columns but position_group = 'QB'.\n")


# =============================================================================
# Example 2: Build a Single-Season Panel (Development Mode)
# =============================================================================
# Always build and validate a single recent season before running the full
# 16-season panel. Faster feedback loop and catches issues early.

cat("\n--- Example 2: Single-season panel build (2024) ---\n")

panel_2024 <- build_player_season_panel(
  seasons   = 2024,
  min_plays = 10,
  verbose   = TRUE
)

cat(glue("\nPanel dimensions: {nrow(panel_2024)} rows x {ncol(panel_2024)} columns\n"))
cat(glue("Unique players  : {n_distinct(panel_2024$player_id)}\n"))
cat(glue("Low volume flag : {sum(panel_2024$low_volume, na.rm = TRUE)} player-seasons\n"))

# Verify output columns exist before using them
required_output_cols <- c(
  "player_id", "player_name", "season", "team",
  "position", "position_group",
  "games_played", "total_plays", "low_volume",
  "attempts", "completions", "passing_yards", "pass_tds",
  "rush_attempts", "rushing_yards", "rush_tds",
  "targets", "receptions", "receiving_yards", "rec_tds",
  "pass_epa", "rush_epa", "rec_epa",
  "total_yards", "total_epa", "panel_version"
)

missing_cols <- setdiff(required_output_cols, names(panel_2024))
if (length(missing_cols) > 0) {
  warning(glue("Missing expected columns: {paste(missing_cols, collapse = ', ')}"))
} else {
  cat("All expected output columns present.\n")
}

cat(glue("\nPanel version tag: {unique(panel_2024$panel_version)}\n"))


# =============================================================================
# Example 3: QB Efficiency Ranking
# =============================================================================
# EPA per dropback is the primary QB efficiency metric. It accounts for
# completions, incompletions, sacks, and scrambles on a per-play basis.
# Minimum 200 dropbacks filters out backups and spot starters.
#
# NFL context: League average pass EPA per dropback is approximately +0.04
# to +0.08. QBs above +0.15 are top-tier. QBs below 0.00 are replacement level.

cat("\n--- Example 3: QB efficiency ranking (2024) ---\n")

MIN_DROPBACKS <- 200L  # Roughly a quarter-season starter minimum

qb_ranking <- panel_2024 %>%
  filter(
    position_group == "QB",
    !is.na(pass_epa_per_dropback),
    qb_dropbacks >= MIN_DROPBACKS
  ) %>%
  select(
    player_name, team, games_played,
    qb_dropbacks, attempts, completions,
    pass_epa_per_dropback, pass_success_rate,
    mean_cpoe,
    rushing_yards, scrambles
  ) %>%
  arrange(desc(pass_epa_per_dropback)) %>%
  mutate(
    rank              = row_number(),
    completion_pct    = round(completions / attempts * 100, 1),
    pass_epa_per_db   = round(pass_epa_per_dropback, 3),
    pass_success_pct  = round(pass_success_rate * 100, 1),
    cpoe_display      = round(mean_cpoe, 1)
  ) %>%
  select(rank, player_name, team, games_played, qb_dropbacks,
         completion_pct, pass_epa_per_db, pass_success_pct, cpoe_display,
         rushing_yards, scrambles)

cat(glue("QBs with >= {MIN_DROPBACKS} dropbacks: {nrow(qb_ranking)}\n\n"))
print(head(qb_ranking, 10))

cat("\nNFL benchmark: League avg pass EPA/dropback ~ +0.04 to +0.08\n")
cat("Above +0.15 = top-tier. Below 0.00 = replacement level.\n")
cat(glue(
  "QBs above league avg (+0.04): {sum(qb_ranking$pass_epa_per_db >= 0.04)}\n"
))

# Surface rushing contribution for dual-threat QBs
dual_threat_qbs <- qb_ranking %>%
  filter(rushing_yards >= 300) %>%
  arrange(desc(rushing_yards))

cat(glue("\nDual-threat QBs (300+ rushing yards): {nrow(dual_threat_qbs)}\n"))
if (nrow(dual_threat_qbs) > 0) print(dual_threat_qbs)
cat("Note: These rush yards appear in rush_* columns despite position_group = 'QB'.\n")


# =============================================================================
# Example 4: Dual-Threat RB Analysis
# =============================================================================
# The panel captures both rushing and receiving stats in the same row,
# making it straightforward to identify RBs with meaningful roles in both
# the run game and passing game. This is not possible with single-stat tables.
#
# NFL context: Pass-catching RBs are disproportionately valuable in PPR
# fantasy and in analytics-forward offenses. Target share is the key
# opportunity metric. A RB with 60+ targets per season is a receiving back.

cat("\n--- Example 4: Dual-threat RB analysis (2024) ---\n")

MIN_RUSH_ATTEMPTS <- 50L
MIN_TARGETS       <- 30L

dual_threat_rbs <- panel_2024 %>%
  filter(
    position_group == "RB",
    !low_volume,
    rush_attempts >= MIN_RUSH_ATTEMPTS,
    targets >= MIN_TARGETS
  ) %>%
  mutate(
    total_touches      = rush_attempts + receptions,
    scrimmage_yards    = rushing_yards + receiving_yards,
    scrimmage_tds      = coalesce(rush_tds, 0L) + coalesce(rec_tds, 0L),
    rush_epa_display   = round(rush_epa_per_attempt, 3),
    rec_epa_display    = round(rec_epa_per_target, 3),
    catch_rate_pct     = round(catch_rate * 100, 1)
  ) %>%
  select(
    player_name, team, games_played,
    rush_attempts, rushing_yards, rush_epa_display,
    targets, receptions, catch_rate_pct, rec_epa_display,
    total_touches, scrimmage_yards, scrimmage_tds
  ) %>%
  arrange(desc(scrimmage_yards))

cat(glue(
  "Dual-threat RBs (>= {MIN_RUSH_ATTEMPTS} rush attempts AND >= {MIN_TARGETS} targets): ",
  "{nrow(dual_threat_rbs)}\n\n"
))
print(dual_threat_rbs)

cat("\nKey insight: The panel structure makes this trivial.\n")
cat("A traditional stat table would require merging rushing and receiving sources.\n")
cat("Here, both are on the same row, anchored to one player_id.\n")


# =============================================================================
# Example 5: Position Group Distribution Across 16 Seasons
# =============================================================================
# Confirms the panel covers all 16 seasons and that position classification
# is stable. Also surfaces how many players per position group appear in
# the data each year -- useful for understanding sample sizes before modeling.

cat("\n--- Example 5: Position group distribution across seasons ---\n")

# Build a 3-season subset for this example (faster than 16-season)
panel_recent <- build_player_season_panel(
  seasons   = 2022:2024,
  min_plays = 10,
  verbose   = FALSE
)

position_by_season <- panel_recent %>%
  filter(!is.na(position_group), !low_volume) %>%
  group_by(season, position_group) %>%
  summarise(
    n_players    = n(),
    median_games = median(games_played, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  arrange(season, position_group)

cat("Player-seasons above min_plays threshold, by position group:\n\n")
print(position_by_season)

# Cross-check: confirm WR and TE both appear in WR_TE group
wr_te_breakdown <- panel_recent %>%
  filter(position_group == "WR_TE", !low_volume) %>%
  count(position) %>%
  arrange(desc(n))

cat("\nWR_TE group breakdown by raw position:\n")
print(wr_te_breakdown)
cat("Both WR and TE route to the same WR_TE model routing group.\n")

# Spot-check: no skill-position players with NA position_group
na_group_skill <- panel_recent %>%
  filter(position %in% c("QB", "RB", "WR", "TE", "FB"),
         is.na(position_group))

cat(glue(
  "\nSkill-position players with NA position_group: {nrow(na_group_skill)}\n"
))
stopifnot(
  "All skill-position players must have a position_group" =
    nrow(na_group_skill) == 0L
)


# =============================================================================
# Example 6: Validate Panel Integrity
# =============================================================================
# Always run validate_panel_integrity() after build_player_season_panel().
# It catches duplicate rows, negative yardage, and games-cap violations
# before any downstream model consumes the panel.

cat("\n--- Example 6: Panel integrity validation ---\n")

integrity <- validate_panel_integrity(panel_2024, verbose = TRUE)

cat(glue("\nOverall validity: {if (integrity$valid) 'VALID' else 'INVALID'}\n"))

# Inspect the summary table
cat("\nFull check summary:\n")
print(integrity$summary)

# Show any position changes (informational -- not a failure)
if (nrow(integrity$position_changes) > 0) {
  cat("\nPlayers who changed positions (informational):\n")
  print(head(integrity$position_changes, 10))
} else {
  cat("\nNo position changes detected in this season.\n")
  cat("Position changes appear across seasons, not within a single season.\n")
}

# Show unmatched players (no nflreadr roster match)
if (nrow(integrity$unmatched_players) > 0) {
  cat(glue("\nPlayers with no nflreadr roster match: {nrow(integrity$unmatched_players)}\n"))
  cat("These are typically specialists on trick plays or historical ID gaps.\n")
  print(head(integrity$unmatched_players, 5))
}


# =============================================================================
# Example 7: Cross-Season Efficiency Trend for One Player
# =============================================================================
# With a multi-season panel, tracking year-over-year efficiency is a single
# filter + arrange. This is the foundation for the aging curves in Week 9.
# No re-joining, no re-aggregating -- it's all in one place.
#
# NFL context: EPA per dropback year-over-year is one of the more stable
# QB metrics. It has a year-over-year correlation of ~0.60, compared to
# yards per attempt at ~0.50 and wins at ~0.30. Stability matters for
# aging curve modeling.

cat("\n--- Example 7: Cross-season efficiency trend ---\n")

# Build a longer panel for a multi-year story
panel_multi <- build_player_season_panel(
  seasons   = 2018:2024,
  min_plays = 100,
  verbose   = FALSE
)

# Track Patrick Mahomes across his career
mahomes_trend <- panel_multi %>%
  filter(player_name == "Patrick Mahomes") %>%
  select(
    season, team, games_played,
    qb_dropbacks, pass_epa_per_dropback,
    pass_success_rate, mean_cpoe,
    rushing_yards, scrambles
  ) %>%
  arrange(season) %>%
  mutate(
    epa_per_db    = round(pass_epa_per_dropback, 3),
    success_pct   = round(pass_success_rate * 100, 1),
    cpoe_display  = round(mean_cpoe, 1)
  ) %>%
  select(season, games_played, qb_dropbacks,
         epa_per_db, success_pct, cpoe_display,
         rushing_yards, scrambles)

cat("Patrick Mahomes -- year-over-year EPA per dropback:\n\n")
print(mahomes_trend)

# Spot-check a second player: Lamar Jackson dual-threat tracking
lamar_trend <- panel_multi %>%
  filter(player_name == "Lamar Jackson") %>%
  select(
    season, games_played,
    pass_epa_per_dropback,
    rush_attempts, scrambles, rushing_yards,
    total_epa
  ) %>%
  arrange(season) %>%
  mutate(
    pass_epa_per_db = round(pass_epa_per_dropback, 3),
    rush_per_game   = round(rush_attempts / games_played, 1)
  ) %>%
  select(season, games_played, pass_epa_per_db, rush_per_game,
         scrambles, rushing_yards, total_epa)

cat("\nLamar Jackson -- rushing and passing efficiency by season:\n")
cat("(Both rush and pass stats on the same row -- this is the panel's core value)\n\n")
print(lamar_trend)

cat("\nNFL context: Year-over-year EPA/dropback correlation ~ 0.60.\n")
cat("More stable than wins (~0.30) or yards per attempt (~0.50).\n")
cat("This stability is why EPA/dropback is the anchor for aging curve modeling in Week 9.\n")


# =============================================================================
# Best Practices Summary
# =============================================================================

cat("\n=============================================================================\n")
cat("BEST PRACTICES SUMMARY -- R/16_player_season_panel.R\n")
cat("=============================================================================\n\n")

cat("1. ALWAYS build a single-season panel first before running all 16 seasons.\n")
cat("   build_player_season_panel(seasons = 2024) takes <60 seconds on cache hit.\n\n")

cat("2. ALWAYS validate integrity before downstream use:\n")
cat("   integrity <- validate_panel_integrity(panel)\n")
cat("   stopifnot(integrity$valid)\n\n")

cat("3. Position classification comes from nflreadr, not play-level role.\n")
cat("   Lamar Jackson = QB. McCaffrey = RB. Never override these without justification.\n\n")

cat("4. low_volume = TRUE means < 10 total plays. These rows exist in the panel\n")
cat("   but should be excluded from efficiency rate calculations:\n")
cat("   filter(!low_volume)\n\n")

cat("5. mean_cpoe is NA for 2010-2015 (tracking data not available).\n")
cat("   Always check for NAs before computing cross-season CPOE summaries.\n\n")

cat("6. total_tds propagates NA if any component (pass_tds/rush_tds/rec_tds)\n")
cat("   was absent in the source data. Use total_yards or total_epa for safe\n")
cat("   cross-season comparisons -- both use coalesce(x, 0) internally.\n\n")

cat("7. Scrambles count toward rush_epa and rushing_yards, NOT pass_epa.\n")
cat("   For total QB EPA, use total_epa (sums all three components).\n\n")

cat("Done. All 7 examples complete.\n")
