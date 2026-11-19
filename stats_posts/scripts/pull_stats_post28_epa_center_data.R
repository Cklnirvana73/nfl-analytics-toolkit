# ==============================================================================
# pull_stats_post28_epa_center_data.R
#
# Statistics Series, Post 28: one-sample t-test.
# Question: mean EPA per play is validated as "near zero" by eye in the
# multi-season play-by-play work. What does the formal test say?
#
# Reference value mu = 0 is fixed by construction: EPA is a change in expected
# points, so zero is the break-even point the expected-points model defines.
# It is not a benchmark chosen by anyone.
#
# Run this and return the ENTIRE console output.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(nflreadr)
})

MAIN_SEASON     <- 2025
CONTEXT_SEASONS <- 2021:2025   # 17-game era, week <= 18
ALPHA           <- 0.05

cat("\n")
cat(strrep("=", 78), "\n")
cat("POST 28 PULL :: one-sample t-test, mean EPA per play vs zero\n")
cat("run at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n")
cat(strrep("=", 78), "\n\n")

# ---------------------------------------------------------------- 1. load ----

pbp_raw <- nflreadr::load_pbp(CONTEXT_SEASONS)

cat("Raw rows loaded:", format(nrow(pbp_raw), big.mark = ","), "\n")
cat("Seasons present:", paste(sort(unique(pbp_raw$season)), collapse = ", "), "\n\n")

required_cols <- c("season", "week", "season_type", "game_id", "posteam",
                   "play_type", "epa", "qb_kneel", "qb_spike")
missing_cols <- setdiff(required_cols, names(pbp_raw))
stopifnot("Required columns missing from pbp" = length(missing_cols) == 0)
cat("Column check: all", length(required_cols), "required columns present.\n\n")

# --------------------------------------------------------------- 2. clean ----
# Standard efficiency filters: regular season only, scrimmage plays only,
# kneels and spikes removed, EPA and posteam non-missing.

clean_pbp <- function(df) {
  df %>%
    filter(
      season_type == "REG",
      week <= 18,
      play_type %in% c("pass", "run"),
      qb_kneel == 0,
      qb_spike == 0,
      !is.na(epa),
      !is.na(posteam),
      !is.na(game_id)
    )
}

pbp <- clean_pbp(pbp_raw)

cat("--- FILTER LADDER (all context seasons) ---\n")
step <- function(label, n) cat(sprintf("  %-46s %10s\n", label, format(n, big.mark = ",")))
step("raw rows", nrow(pbp_raw))
step("regular season, week <= 18",
     nrow(filter(pbp_raw, season_type == "REG", week <= 18)))
step("+ play_type in pass/run",
     nrow(filter(pbp_raw, season_type == "REG", week <= 18,
                 play_type %in% c("pass", "run"))))
step("+ kneels and spikes removed",
     nrow(filter(pbp_raw, season_type == "REG", week <= 18,
                 play_type %in% c("pass", "run"), qb_kneel == 0, qb_spike == 0)))
step("+ epa / posteam / game_id non-missing", nrow(pbp))
cat("\n")

main <- pbp %>% filter(season == MAIN_SEASON)
stopifnot("No rows for MAIN_SEASON after filtering" = nrow(main) > 0)

cat("2025 analysis set:", format(nrow(main), big.mark = ","), "plays,",
    n_distinct(main$game_id), "games,",
    n_distinct(main$posteam), "teams.\n\n")

# ------------------------------------------------- 3. play-level t-test ------
# The naive version: every play treated as an independent observation.

x <- main$epa
n <- length(x)
m <- mean(x)
s <- sd(x)
se <- s / sqrt(n)

tt <- t.test(x, mu = 0)

cat("--- PRIMARY: PLAY-LEVEL ONE-SAMPLE t-TEST, 2025, mu = 0 ---\n")
cat(sprintf("  n plays              : %s\n", format(n, big.mark = ",")))
cat(sprintf("  mean EPA per play    : %.6f\n", m))
cat(sprintf("  sd                   : %.6f\n", s))
cat(sprintf("  standard error       : %.6f\n", se))
cat(sprintf("  t statistic          : %.4f\n", unname(tt$statistic)))
cat(sprintf("  df                   : %.0f\n", unname(tt$parameter)))
cat(sprintf("  p-value              : %.6g\n", tt$p.value))
cat(sprintf("  95%% CI               : [%.6f, %.6f]\n",
            tt$conf.int[1], tt$conf.int[2]))
cat(sprintf("  reject at alpha=%.2f  : %s\n", ALPHA, tt$p.value < ALPHA))
cat("\n")

# Minimum detectable deviation at this n: the half-width of the CI.
# This is the teaching payload: how small a deviation this n would still flag.
mdd <- qt(1 - ALPHA / 2, df = n - 1) * se
cat("--- WHAT 'SIGNIFICANT' COSTS AT THIS SAMPLE SIZE ---\n")
cat(sprintf("  half-width of the 95%% CI (smallest deviation from 0\n"))
cat(sprintf("  that this n would still call significant): %.6f EPA per play\n", mdd))

plays_per_team_game <- n / (n_distinct(main$game_id) * 2)
cat(sprintf("  plays per team-game (2025)             : %.2f\n", plays_per_team_game))
cat(sprintf("  observed mean scaled to one team-game  : %.4f points\n",
            m * plays_per_team_game))
cat(sprintf("  CI half-width scaled to one team-game  : %.4f points\n",
            mdd * plays_per_team_game))
cat(sprintf("  observed mean scaled to a 17-game season: %.4f points\n",
            m * plays_per_team_game * 17))
cat("\n")

# --------------------------------------- 4. distribution shape reference -----

qs <- quantile(x, c(0.01, 0.25, 0.50, 0.75, 0.99))
cat("--- SHAPE OF THE PLAY-LEVEL EPA DISTRIBUTION, 2025 ---\n")
cat(sprintf("  min / max            : %.4f / %.4f\n", min(x), max(x)))
cat(sprintf("  1%% / 25%% / 50%% / 75%% / 99%% : %.4f / %.4f / %.4f / %.4f / %.4f\n",
            qs[1], qs[2], qs[3], qs[4], qs[5]))
cat(sprintf("  median               : %.6f\n", median(x)))
cat(sprintf("  share of plays with EPA > 0 : %.4f\n", mean(x > 0)))
cat("\n")

# ------------------------------------- 5. clustered version, team-game -------
# Plays are clustered inside games. Collapsing to one observation per team per
# game removes the within-game dependence and is the honest comparison.

tg <- main %>%
  group_by(game_id, posteam) %>%
  summarise(team_game_epa = mean(epa), n_plays = n(), .groups = "drop")

tt_tg <- t.test(tg$team_game_epa, mu = 0)

cat("--- SECONDARY: TEAM-GAME MEANS, 2025, mu = 0 ---\n")
cat(sprintf("  n team-games         : %s\n", format(nrow(tg), big.mark = ",")))
cat(sprintf("  mean of team-game means : %.6f\n", mean(tg$team_game_epa)))
cat(sprintf("  sd                   : %.6f\n", sd(tg$team_game_epa)))
cat(sprintf("  standard error       : %.6f\n",
            sd(tg$team_game_epa) / sqrt(nrow(tg))))
cat(sprintf("  t statistic          : %.4f\n", unname(tt_tg$statistic)))
cat(sprintf("  df                   : %.0f\n", unname(tt_tg$parameter)))
cat(sprintf("  p-value              : %.6g\n", tt_tg$p.value))
cat(sprintf("  95%% CI               : [%.6f, %.6f]\n",
            tt_tg$conf.int[1], tt_tg$conf.int[2]))
cat(sprintf("  reject at alpha=%.2f  : %s\n", ALPHA, tt_tg$p.value < ALPHA))
cat(sprintf("  SE inflation vs play-level: %.2fx\n",
            (sd(tg$team_game_epa) / sqrt(nrow(tg))) / se))
cat("\n")

# ------------------------------------------- 6. season-by-season context -----

season_tbl <- pbp %>%
  group_by(season) %>%
  summarise(
    n_plays = n(),
    mean_epa = mean(epa),
    sd_epa = sd(epa),
    se_epa = sd(epa) / sqrt(n()),
    t_stat = mean(epa) / (sd(epa) / sqrt(n())),
    p_value = 2 * pt(-abs(mean(epa) / (sd(epa) / sqrt(n()))), df = n() - 1),
    ci_lo = mean(epa) - qt(1 - ALPHA / 2, n() - 1) * sd(epa) / sqrt(n()),
    ci_hi = mean(epa) + qt(1 - ALPHA / 2, n() - 1) * sd(epa) / sqrt(n()),
    .groups = "drop"
  )

cat("--- CONTEXT: SAME TEST, EACH SEASON, PLAY LEVEL ---\n")
print(as.data.frame(season_tbl), digits = 6, row.names = FALSE)
cat("\n")

# ------------------------------------------------------------ 7. asserts -----

stopifnot(
  "2025 play count outside plausible range" =
    nrow(main) > 25000 && nrow(main) < 60000,
  "2025 team count is not 32" = n_distinct(main$posteam) == 32,
  "mean EPA outside plausible range" = abs(m) < 0.25,
  "team-game rows do not equal 2 per game" =
    nrow(tg) == n_distinct(main$game_id) * 2
)
cat("All assertions passed.\n")

cat("\n")
cat(strrep("=", 78), "\n")
cat("END OF PULL\n")
cat(strrep("=", 78), "\n")
