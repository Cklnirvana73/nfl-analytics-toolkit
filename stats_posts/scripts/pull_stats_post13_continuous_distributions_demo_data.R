# ==============================================================================
# pull_stats_post13_continuous_distributions_demo_data.R
#
# Post 13: Continuous Distributions (uniform and exponential)
# Standalone data-pull script. Run this on its own. It sources no toolkit files.
#
# Produces two demos from verified 2025 nflfastR regular-season data:
#   DEMO 1 (EXPONENTIAL): waiting time (game-clock seconds) between scoring
#           events. Scores in game time behave like a Poisson process, so the
#           gaps between them should be roughly exponential.
#   DEMO 2 (UNIFORM):     drive-starting field position (own yard line). The
#           uniform distribution is the "equally likely anywhere" model; this
#           tests whether real drive starts match it.
#
# All figures printed to console are what the post will use. Copy the console
# block back into the session.
# ==============================================================================

# ------------------------------- CONFIGURATION --------------------------------
SEASON        <- 2025L      # single season, regular season only
MIN_YL        <- 1L         # valid own-yard-line lower bound for drive starts
MAX_YL        <- 99L        # valid own-yard-line upper bound for drive starts
BAND_WIDTH    <- 10L        # width of field-position bands for the uniform test
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(nflreadr)
  library(dplyr)
})

cat("================================================================\n")
cat("  POST 13 DEMO DATA PULL: CONTINUOUS DISTRIBUTIONS\n")
cat("  Season:", SEASON, "(regular season only)\n")
cat("================================================================\n\n")

# ------------------------------------------------------------------------------
# Load play-by-play
# ------------------------------------------------------------------------------
cat("Loading", SEASON, "play-by-play from nflreadr ...\n")
pbp <- nflreadr::load_pbp(SEASON) %>%
  filter(season_type == "REG")

cat("  Loaded", format(nrow(pbp), big.mark = ","), "regular-season plays.\n\n")

# ==============================================================================
# DEMO 1: EXPONENTIAL - waiting time between scoring events
# ==============================================================================
# A "scoring event" is a distinct score: a touchdown, a made field goal, or a
# safety. Extra points and two-point tries are excluded so a TD and its PAT do
# not count as two scores a few seconds apart. Regulation only (qtr <= 4) so the
# game clock is clean. elapsed = 3600 - game_seconds_remaining. Within each game
# we prepend kickoff (elapsed 0) and take consecutive differences, giving the
# waiting time from kickoff to the first score and between each pair of scores.
# ------------------------------------------------------------------------------
cat("----------------------------------------------------------------\n")
cat("DEMO 1 (EXPONENTIAL): time between scoring events\n")
cat("----------------------------------------------------------------\n")

scores <- pbp %>%
  filter(
    qtr <= 4,
    !is.na(game_seconds_remaining),
    coalesce(touchdown == 1, FALSE) |
      coalesce(field_goal_result == "made", FALSE) |
      coalesce(safety == 1, FALSE)
  ) %>%
  mutate(elapsed = 3600 - game_seconds_remaining) %>%
  distinct(game_id, elapsed, .keep_all = TRUE) %>%   # guard against duplicate clock stamps
  arrange(game_id, elapsed)

gaps <- scores %>%
  group_by(game_id) %>%
  arrange(elapsed, .by_group = TRUE) %>%
  mutate(prev_elapsed = lag(elapsed, default = 0)) %>%   # kickoff = 0
  ungroup() %>%
  mutate(gap = elapsed - prev_elapsed) %>%
  filter(gap > 0)                                        # drop any zero-length artifacts

n_gaps      <- nrow(gaps)
n_games     <- n_distinct(gaps$game_id)
mean_gap    <- mean(gaps$gap)
median_gap  <- median(gaps$gap)
sd_gap      <- sd(gaps$gap)
lambda_rate <- 1 / mean_gap                              # exponential rate estimate

# Exponential goodness check: for Exp, P(X <= mean) = 1 - e^-1 = 0.632.
obs_below_mean <- mean(gaps$gap <= mean_gap)
exp_below_mean <- 1 - exp(-1)

# CDF comparison at a few marks (observed proportion vs exponential prediction)
marks <- c(120, 300, 600, 900)   # 2, 5, 10, 15 minutes of game clock
cdf_tbl <- data.frame(
  seconds  = marks,
  observed = sapply(marks, function(s) mean(gaps$gap <= s)),
  exp_pred = sapply(marks, function(s) 1 - exp(-lambda_rate * s))
)

cat("  Scoring events (regulation):", format(nrow(scores), big.mark = ","), "\n")
cat("  Games with >=1 gap:         ", n_games, "\n")
cat("  Number of gaps:             ", format(n_gaps, big.mark = ","), "\n\n")
cat("  Mean gap:   ", round(mean_gap, 1),   "sec  (", round(mean_gap/60, 2), "min )\n")
cat("  Median gap: ", round(median_gap, 1), "sec  (", round(median_gap/60, 2), "min )\n")
cat("  SD of gap:  ", round(sd_gap, 1),     "sec\n")
cat("  SD / mean ratio (Exp predicts ~1.0):", round(sd_gap / mean_gap, 3), "\n")
cat("  Rate lambda = 1/mean:", round(lambda_rate, 5), "per sec\n\n")
cat("  P(gap <= mean): observed", round(obs_below_mean, 3),
    "vs exponential", round(exp_below_mean, 3), "\n\n")
cat("  CDF check (proportion of gaps at or under each mark):\n")
cat("    seconds   observed   exp_predicted\n")
for (i in seq_len(nrow(cdf_tbl))) {
  cat(sprintf("    %6d     %6.3f        %6.3f\n",
              cdf_tbl$seconds[i], cdf_tbl$observed[i], cdf_tbl$exp_pred[i]))
}
cat("\n")

# ==============================================================================
# DEMO 2: UNIFORM - drive-starting field position
# ==============================================================================
# First scrimmage snap of each drive. yardline_100 is distance to the opponent
# end zone, so own yard line = 100 - yardline_100 (0 = own goal line, 100 =
# opponent goal line). We bucket into 10-yard bands and compare the observed
# share per band against the uniform prediction of an equal share per band.
# ------------------------------------------------------------------------------
cat("----------------------------------------------------------------\n")
cat("DEMO 2 (UNIFORM): drive-starting field position (own yard line)\n")
cat("----------------------------------------------------------------\n")

drive_starts <- pbp %>%
  filter(
    !is.na(fixed_drive),
    !is.na(yardline_100),
    !is.na(down),                     # real scrimmage play
    yardline_100 >= MIN_YL,
    yardline_100 <= (100 - 1)         # keep on the field
  ) %>%
  group_by(game_id, fixed_drive) %>%
  arrange(play_id, .by_group = TRUE) %>%
  slice(1) %>%                        # first snap of each drive (play_id is monotonic within a game)
  ungroup() %>%
  mutate(own_yl = 100 - yardline_100) %>%
  filter(own_yl >= MIN_YL, own_yl <= MAX_YL)

n_drives   <- nrow(drive_starts)
mean_own   <- mean(drive_starts$own_yl)
median_own <- median(drive_starts$own_yl)
sd_own     <- sd(drive_starts$own_yl)

# Band the field into 10-yard bins: [1,10],[11,20],...,[91,99]
band_labels <- c("1-10","11-20","21-30","31-40","41-50",
                 "51-60","61-70","71-80","81-90","91-99")
drive_starts <- drive_starts %>%
  mutate(band = cut(own_yl,
                    breaks = c(0,10,20,30,40,50,60,70,80,90,99),
                    labels = band_labels,
                    include.lowest = TRUE))

band_tbl <- drive_starts %>%
  count(band, .drop = FALSE) %>%
  mutate(observed_pct = 100 * n / sum(n),
         uniform_pct  = 100 / length(band_labels))

# Modal band and the single most common exact yard line (the touchback spike)
modal_band <- band_tbl %>% slice_max(n, n = 1)
top_lines  <- drive_starts %>%
  count(own_yl, sort = TRUE) %>%
  slice_head(n = 5) %>%
  mutate(pct = 100 * n / n_drives)

cat("  Drives analyzed:", format(n_drives, big.mark = ","), "\n\n")
cat("  Mean own yard line:   ", round(mean_own, 2),   " (uniform predicts 50)\n")
cat("  Median own yard line: ", round(median_own, 2), "\n")
cat("  SD of own yard line:  ", round(sd_own, 2),
    " (uniform over 1-99 predicts ~28.3)\n\n")

cat("  Field-position bands (observed share vs uniform 10% each):\n")
cat("    band       n       observed%   uniform%\n")
for (i in seq_len(nrow(band_tbl))) {
  cat(sprintf("    %-7s  %6d      %6.2f      %6.2f\n",
              as.character(band_tbl$band[i]), band_tbl$n[i],
              band_tbl$observed_pct[i], band_tbl$uniform_pct[i]))
}
cat("\n")
cat("  Modal band:", as.character(modal_band$band[1]),
    "at", round(modal_band$observed_pct[1], 2), "% of drives",
    "(uniform predicts", round(100/length(band_labels), 2), "%)\n\n")

cat("  Five most common exact drive-start own yard lines:\n")
cat("    own_yl     n       pct%\n")
for (i in seq_len(nrow(top_lines))) {
  cat(sprintf("    %4d     %6d     %6.2f\n",
              top_lines$own_yl[i], top_lines$n[i], top_lines$pct[i]))
}
cat("\n")

# ==============================================================================
# CONSOLE SUMMARY (copy this block back into the session)
# ==============================================================================
cat("================================================================\n")
cat("  SUMMARY FOR POST 13\n")
cat("================================================================\n")
cat("EXPONENTIAL (time between scoring events, ", SEASON, " regulation):\n", sep = "")
cat("  gaps n =", n_gaps, "| games =", n_games, "\n")
cat("  mean =", round(mean_gap, 1), "sec | median =", round(median_gap, 1),
    "sec | SD =", round(sd_gap, 1), "sec\n")
cat("  lambda =", round(lambda_rate, 5), "per sec | SD/mean =",
    round(sd_gap / mean_gap, 3), "\n")
cat("  P(X<=mean): obs", round(obs_below_mean, 3),
    "vs Exp", round(exp_below_mean, 3), "\n\n")
cat("UNIFORM (drive-start own yard line, ", SEASON, "):\n", sep = "")
cat("  drives n =", n_drives, "\n")
cat("  mean =", round(mean_own, 2), "(uniform predicts 50) | SD =",
    round(sd_own, 2), "(uniform predicts ~28.3)\n")
cat("  modal band", as.character(modal_band$band[1]), "=",
    round(modal_band$observed_pct[1], 2), "% vs uniform",
    round(100/length(band_labels), 2), "%\n")
cat("  top exact line:", top_lines$own_yl[1], "at",
    round(top_lines$pct[1], 2), "% of drives\n")
cat("================================================================\n")
