# ==============================================================================
# WEEK 7 EXAMPLES: Game Script & Leverage Features
# example_week7.R
# ==============================================================================
#
# 6 self-contained examples demonstrating the Week 7 functions.
# Each example is independent -- no shared state between sections.
#
# NFL Context throughout:
#   - Game script removes the single biggest source of play-calling bias
#   - Neutral-script EPA is the most talent-predictive efficiency metric
#   - WPA captures game-state importance; EPA alone does not
#   - Script-adjusted EPA is experimental -- always compare to raw
#
# Usage:
#   source(here::here("examples", "example_week7.R"))
# ==============================================================================

library(dplyr)
library(glue)
library(here)

source(here::here("R", "09_gamescript_features.R"))

# Shared synthetic data (representative of a full season)
# Season: 2025 (current year -- never hardcode prior years)
set.seed(2025)
n <- 2000L

make_example_pbp <- function(n) {
  teams <- c("KC","BUF","PHI","SF","BAL","CIN","DAL","MIA")
  qb_ids   <- paste0("QB", seq_along(teams))
  qb_names <- c("P.Mahomes","J.Allen","J.Hurts","B.Purdy",
                 "L.Jackson","J.Burrow","D.Prescott","T.Tagovailoa")
  rb_ids   <- paste0("RB", seq_along(teams))
  rb_names <- c("I.Pacheco","J.Cook","D.Swift","C.McCaffrey",
                 "J.Dobbins","J.Mixon","T.Pollard","R.Mostert")
  wr_ids   <- paste0("WR", seq_along(teams))
  wr_names <- c("T.Kelce","S.Diggs","A.Brown","D.Samuel",
                 "Z.Flowers","J.Chase","C.Lamb","T.Hill")

  # WP tendencies per team: stronger teams lead more often
  team_mean_wp <- c(KC=0.63,BUF=0.60,PHI=0.59,SF=0.62,BAL=0.55,CIN=0.50,DAL=0.52,MIA=0.48)

  pbp <- tibble(
    season    = 2025L,
    week      = sample(1:17, n, replace = TRUE),
    play_type = sample(c("pass","run"), n, replace = TRUE, prob = c(0.60, 0.40)),
    posteam   = sample(teams, n, replace = TRUE)
  ) %>%
    mutate(
      defteam  = "OPP",
      game_id  = paste0("2025_", sprintf("%02d", week), "_", posteam, "_OPP"),
      qtr      = sample(1:4, n, replace = TRUE),
      base_wp  = team_mean_wp[posteam],
      wp       = pmin(pmax(rnorm(n, base_wp, 0.14), 0.05), 0.95),
      epa      = rnorm(n, 0.04, 0.42),
      wpa      = rnorm(n, 0.0, 0.028),
      # score_differential is generated independently from wp using a realistic
      # NFL distribution: scores are multiples of 3 and 7, range -35 to +35 is
      # common. Deriving it from wp alone collapses all values near zero because
      # team mean WP is 0.48-0.63 -- that produces differentials of -1 to +4,
      # which means almost no plays qualify as comeback situations (deficit >= 8).
      # Instead, sample from a discrete set of realistic NFL scorelines and
      # correlate loosely with wp via team tendency.
      score_differential = as.integer(
        round(rnorm(n, mean = (base_wp - 0.5) * 20, sd = 12))
      ),
      score_differential = pmax(pmin(score_differential, 35L), -35L),
      team_idx = match(posteam, teams),
      passer_player_id   = ifelse(play_type == "pass", qb_ids[team_idx],   NA_character_),
      passer_player_name = ifelse(play_type == "pass", qb_names[team_idx], NA_character_),
      rusher_player_id   = ifelse(play_type == "run",  rb_ids[team_idx],   NA_character_),
      rusher_player_name = ifelse(play_type == "run",  rb_names[team_idx], NA_character_),
      receiver_player_id   = ifelse(play_type == "pass", wr_ids[team_idx],   NA_character_),
      receiver_player_name = ifelse(play_type == "pass", wr_names[team_idx], NA_character_),
      qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L
    ) %>%
    select(-base_wp, -team_idx)
}

pbp <- make_example_pbp(n)


# ==============================================================================
# EXAMPLE 1: Game Script Splits -- Basic Usage
# ==============================================================================
# NFL Context: A player's raw EPA is biased by when they play. Use
# neutral_epa_per_play for the cleanest talent signal.

cat("======================================================\n")
cat("EXAMPLE 1: Game Script Splits -- Basic Usage\n")
cat("======================================================\n\n")

gs <- get_game_script_splits(pbp, season = 2025L, min_plays = 30)

cat("Available columns:\n")
cat(paste(names(gs), collapse = "\n"), "\n\n")

# Top rushers by neutral-script EPA (least biased by game state)
cat("Top RBs by Neutral-Script EPA per Play (min 30 plays):\n")
gs %>%
  filter(position_group == "rusher", total_plays >= 30) %>%
  arrange(desc(neutral_epa_per_play)) %>%
  select(player_name, team, total_plays, neutral_plays,
         neutral_epa_per_play, leading_share, trailing_share) %>%
  head(8) %>%
  print()

cat("\nKey insight: neutral_epa_per_play removes clock-killing run bias.\n")
cat("RBs with high leading_share inflate raw EPA through favorable situations.\n\n")


# ==============================================================================
# EXAMPLE 2: Identifying Script-Dependent Players
# ==============================================================================
# NFL Context: script_epa_delta = neutral_epa minus trailing_epa.
# Large positive delta for WRs signals garbage-time target inflation.
# Large negative delta for RBs signals clock-killing run dependency.

cat("======================================================\n")
cat("EXAMPLE 2: Identifying Script-Dependent Players\n")
cat("======================================================\n\n")

# WRs with highest script delta (neutral >> trailing = not garbage-time dependent)
cat("WRs with highest script_epa_delta (production holds in neutral vs trailing):\n")
gs %>%
  filter(position_group == "receiver", total_plays >= 25,
         !is.na(script_epa_delta), !is.na(neutral_epa_per_play),
         !is.na(trailing_epa_per_play)) %>%
  arrange(desc(script_epa_delta)) %>%
  select(player_name, team, neutral_epa_per_play, trailing_epa_per_play,
         script_epa_delta, trailing_share) %>%
  head(8) %>%
  print()

cat("\nRBs most dependent on leading script (leading_share > 40%):\n")
gs %>%
  filter(position_group == "rusher", leading_share >= 0.40) %>%
  arrange(desc(leading_share)) %>%
  select(player_name, team, total_plays, leading_share,
         neutral_epa_per_play, leading_epa_per_play) %>%
  print()

cat("\nKey insight: RBs with high leading_share are clock-killing backs.\n")
cat("Their raw EPA looks good but neutral-script EPA reveals true efficiency.\n\n")


# ==============================================================================
# EXAMPLE 3: Leverage Features -- WPA Leaders
# ==============================================================================
# NFL Context: total_wpa captures value in high-stakes moments, not just
# overall volume. A QB can have high EPA but low WPA if their big plays
# happen in garbage time.

cat("======================================================\n")
cat("EXAMPLE 3: Leverage Features -- WPA Leaders\n")
cat("======================================================\n\n")

lev <- calculate_leverage_features(pbp, min_plays = 30)

cat("Available columns:\n")
cat(paste(names(lev), collapse = "\n"), "\n\n")

cat("Top QBs by Total WPA:\n")
lev %>%
  filter(position_group == "passer", total_plays >= 100) %>%
  arrange(desc(total_wpa)) %>%
  select(player_name, team, total_wpa, mean_wpa_per_play,
         high_leverage_plays, clutch_rate, clutch_note) %>%
  head(8) %>%
  print()

cat("\nIMPORTANT: Always check clutch_note before interpreting clutch_rate.\n")
cat("Players with < 10 high-leverage plays have unreliable clutch_rate values.\n\n")

# Proportion of plays that are high-leverage by position
cat("High-leverage play rate by position group:\n")
lev %>%
  group_by(position_group) %>%
  summarise(
    mean_hl_rate       = mean(high_leverage_rate, na.rm = TRUE),
    median_hl_plays    = median(high_leverage_plays, na.rm = TRUE),
    pct_sufficient_sample = mean(clutch_note == "sufficient_sample", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()


# ==============================================================================
# EXAMPLE 4: Comeback Profile
# ==============================================================================
# NFL Context: Trailing-game performance separates scheme-driven production
# from true talent. QBs who maintain EPA while down 8+ are more valuable
# than raw efficiency suggests.

cat("======================================================\n")
cat("EXAMPLE 4: Comeback Profile\n")
cat("======================================================\n\n")

cmb <- get_comeback_profile(pbp, deficit_threshold = 8, min_plays = 5)

cat("Available columns:\n")
cat(paste(names(cmb), collapse = "\n"), "\n\n")

cat("QBs who outperform their season average when trailing by 8+ pts:\n")
cmb %>%
  filter(position_group == "passer", comeback_plays >= 10) %>%
  arrange(desc(comeback_vs_overall_epa)) %>%
  select(player_name, team, comeback_plays, comeback_epa_per_play,
         overall_epa_per_play, comeback_vs_overall_epa, sample_note) %>%
  head(8) %>%
  print()

cat("\nRBs with most comeback opportunities (high-volume trailing teams):\n")
cmb %>%
  filter(position_group == "rusher") %>%
  arrange(desc(comeback_plays)) %>%
  select(player_name, team, comeback_plays, comeback_epa_per_play,
         overall_epa_per_play, sample_note) %>%
  head(5) %>%
  print()

cat("\nKey insight: RBs rarely get touches when trailing by 8+.\n")
cat("Few comeback_plays for RBs is normal -- not a data issue.\n\n")


# ==============================================================================
# EXAMPLE 5: Script-Adjusted EPA
# ==============================================================================
# NFL Context: Script-adjusted EPA levels the playing field for players
# on teams with extreme game scripts. Additive correction; always compare
# to raw_epa_per_play to see adjustment magnitude.

cat("======================================================\n")
cat("EXAMPLE 5: Script-Adjusted EPA\n")
cat("======================================================\n\n")

adj <- calculate_script_adjusted_epa(pbp, min_plays = 30)

cat("Available columns:\n")
cat(paste(names(adj), collapse = "\n"), "\n\n")

cat("Players most penalized by script adjustment (played in favorable scripts):\n")
adj %>%
  filter(total_plays >= 40) %>%
  arrange(desc(adjustment_applied)) %>%
  select(player_name, position_group, team, raw_epa_per_play,
         script_adjusted_epa, adjustment_applied, leading_share) %>%
  head(10) %>%
  print()

cat("\nPlayers most helped by script adjustment (played in unfavorable scripts):\n")
adj %>%
  filter(total_plays >= 40) %>%
  arrange(adjustment_applied) %>%
  select(player_name, position_group, team, raw_epa_per_play,
         script_adjusted_epa, adjustment_applied, trailing_share) %>%
  head(10) %>%
  print()

cat("\nMean adjustment by position group:\n")
adj %>%
  group_by(position_group) %>%
  summarise(
    mean_adjustment = mean(adjustment_applied, na.rm = TRUE),
    max_adjustment  = max(adjustment_applied, na.rm = TRUE),
    min_adjustment  = min(adjustment_applied, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

cat("\nEXPERIMENTAL: Script-adjusted EPA uses a league-average additive correction.\n")
cat("Large adjustments (>0.10 EPA/play) should be treated with caution.\n\n")


# ==============================================================================
# EXAMPLE 6: Combined Analysis Pipeline
# ==============================================================================
# NFL Context: Combining script splits, leverage, and adjusted EPA tells
# the full story: Who is efficient? In what situations? And does that
# efficiency hold up when it matters?

cat("======================================================\n")
cat("EXAMPLE 6: Combined Analysis Pipeline\n")
cat("======================================================\n\n")

# Join script splits with adjusted EPA for QBs
qb_full <- gs %>%
  filter(position_group == "passer", total_plays >= 80) %>%
  left_join(
    adj %>% filter(position_group == "passer") %>%
      select(season, player_id, raw_epa_per_play, script_adjusted_epa, adjustment_applied),
    by = c("season", "player_id")
  ) %>%
  left_join(
    lev %>% filter(position_group == "passer") %>%
      select(season, player_id, total_wpa, clutch_rate, clutch_note),
    by = c("season", "player_id")
  ) %>%
  select(player_name, team, total_plays, neutral_epa_per_play,
         script_adjusted_epa, adjustment_applied, total_wpa, clutch_rate, clutch_note)

cat("Full QB profile: neutral EPA + script adjustment + WPA leverage:\n")
qb_full %>%
  arrange(desc(script_adjusted_epa)) %>%
  print()

cat("\nBest practices summary:\n")
cat("1. Use neutral_epa_per_play as primary efficiency metric\n")
cat("2. Check script_epa_delta to flag garbage-time production\n")
cat("3. Use total_wpa alongside EPA (big plays in meaningless games = low WPA)\n")
cat("4. Always check clutch_note before interpreting clutch_rate\n")
cat("5. Treat script_adjusted_epa as experimental -- compare to raw always\n")
cat("6. Garbage time is excluded throughout -- this is intentional\n")
