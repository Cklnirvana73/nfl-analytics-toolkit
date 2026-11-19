# ============================================================
# Stats Post 12: Discrete Distributions (Binomial + Poisson)
# Data pull and verification script
# ============================================================
# Run this in your own R environment. Paste the full console
# output back so the post's numbers can be grounded in verified
# real figures rather than assumed values.
#
# Two demos:
#   A) Binomial  -- extra point (PAT) makes per team, fixed n trials
#   B) Poisson   -- interceptions thrown per team per game, count process
# ============================================================

library(nflreadr)
library(dplyr)
library(tidyr)

# ---- CONFIGURATION ----
SEASON <- 2025L

# ------------------------------------------------------------
# Load play-by-play, regular season only
# ------------------------------------------------------------
pbp <- nflreadr::load_pbp(SEASON) %>%
  filter(season_type == "REG")

cat("=====================================================\n")
cat("SEASON:", SEASON, "| Regular season plays loaded:", nrow(pbp), "\n")
cat("=====================================================\n\n")

# ------------------------------------------------------------
# DEMO A: Binomial -- extra point attempts per team
# ------------------------------------------------------------
pat <- pbp %>%
  filter(extra_point_attempt == 1) %>%
  filter(!is.na(posteam), !is.na(extra_point_result))

pat_team <- pat %>%
  group_by(posteam) %>%
  summarise(
    n_attempts = n(),
    n_made = sum(extra_point_result == "good", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_attempts))

overall_p <- sum(pat_team$n_made) / sum(pat_team$n_attempts)

cat("---- DEMO A: EXTRA POINT ATTEMPTS (BINOMIAL) ----\n")
cat("Teams with PAT attempts:", nrow(pat_team), "\n")
cat("Total PAT attempts (league):", sum(pat_team$n_attempts), "\n")
cat("Total PAT makes (league):", sum(pat_team$n_made), "\n")
cat("Overall make rate (p):", round(overall_p, 4), "\n\n")

# Modal n (most common attempt count across teams) -- this is the
# fixed number of trials for the binomial comparison
n_table <- table(pat_team$n_attempts)
modal_n <- as.integer(names(n_table)[which.max(n_table)])

cat("Distribution of attempt counts (n) across teams:\n")
print(n_table)
cat("\nModal n (fixed trials for binomial demo):", modal_n, "\n")

modal_n_teams <- pat_team %>% filter(n_attempts == modal_n)
cat("Teams with n =", modal_n, "attempts:", nrow(modal_n_teams), "\n\n")

cat("Observed makes among teams with n =", modal_n, ":\n")
makes_table <- table(factor(modal_n_teams$n_made, levels = 0:modal_n))
print(makes_table)

cat("\nFitted Binomial(n =", modal_n, ", p =", round(overall_p, 4), ") PMF:\n")
binom_pmf <- dbinom(0:modal_n, size = modal_n, prob = overall_p)
names(binom_pmf) <- 0:modal_n
print(round(binom_pmf, 4))

cat("\nExpected count of teams at each make-total (PMF * n_teams):\n")
expected_counts <- binom_pmf * nrow(modal_n_teams)
names(expected_counts) <- 0:modal_n
print(round(expected_counts, 2))

cat("\nFull team-level PAT table:\n")
print(as.data.frame(pat_team), row.names = FALSE)

cat("\n\n")

# ------------------------------------------------------------
# DEMO B: Poisson -- interceptions thrown per team per game
# ------------------------------------------------------------
# Universe of team-game offensive appearances (so games with
# zero interceptions are correctly counted as zero, not omitted)
team_games <- pbp %>%
  filter(!is.na(posteam), pass_attempt == 1 | rush_attempt == 1) %>%
  distinct(game_id, posteam)

int_counts <- pbp %>%
  filter(interception == 1, !is.na(posteam)) %>%
  group_by(game_id, posteam) %>%
  summarise(n_int = n(), .groups = "drop")

team_game_int <- team_games %>%
  left_join(int_counts, by = c("game_id", "posteam")) %>%
  mutate(n_int = ifelse(is.na(n_int), 0L, n_int))

lambda_hat <- mean(team_game_int$n_int)

cat("---- DEMO B: INTERCEPTIONS PER TEAM-GAME (POISSON) ----\n")
cat("Team-game observations:", nrow(team_game_int), "\n")
cat("Total interceptions (league):", sum(team_game_int$n_int), "\n")
cat("Mean interceptions per team-game (lambda):", round(lambda_hat, 4), "\n\n")

cat("Observed frequency distribution of interceptions per team-game:\n")
int_dist <- team_game_int %>%
  mutate(int_bucket = ifelse(n_int >= 4, "4+", as.character(n_int))) %>%
  count(int_bucket, name = "observed_n") %>%
  mutate(observed_pct = round(100 * observed_n / sum(observed_n), 2))
print(as.data.frame(int_dist), row.names = FALSE)

cat("\nFitted Poisson(lambda =", round(lambda_hat, 4), ") PMF, buckets 0-3 and 4+:\n")
pois_pmf_0_3 <- dpois(0:3, lambda = lambda_hat)
pois_pmf_4plus <- 1 - ppois(3, lambda = lambda_hat)
pois_pmf <- c(pois_pmf_0_3, pois_pmf_4plus)
names(pois_pmf) <- c("0", "1", "2", "3", "4+")
print(round(pois_pmf, 4))

cat("\nExpected count of team-games at each bucket (PMF * total team-games):\n")
expected_int_counts <- pois_pmf * nrow(team_game_int)
names(expected_int_counts) <- c("0", "1", "2", "3", "4+")
print(round(expected_int_counts, 2))

cat("\n=====================================================\n")
cat("END OF OUTPUT -- paste everything above back for post drafting\n")
cat("=====================================================\n")
