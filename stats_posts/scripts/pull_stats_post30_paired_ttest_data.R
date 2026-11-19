# pull_stats_post30_paired_ttest_data.R
# Statistics Series Post 30: paired t-test (Concept 30, [GAP], hands-on demo)
#
# Source: rebuilt A/B predictive validity panel (post name-split and position
# fix, 2026-09-17). Each row is one player-week carrying BOTH predictions of
# that week's PPR points plus the actual score, so the two predictors' errors
# form a true pair.
#
# NOTE: the A/B study measured correlation. This script computes a NEW
# quantity on its panel: per-row absolute error of each predictor, then tests
# the paired difference. Nothing here was run in the original study.
#
# Every number printed below is candidate material for the post. Nothing is
# written to disk.

# ---- CONFIGURATION ----------------------------------------------------------
library(dplyr)
library(here)

PANEL_PATH <- here("data", "season2_cache", "s2_week4_ab_panel.rds")
EXPECTED_SEASONS <- 2010:2025
DIGITS <- 6

fmt <- function(x) formatC(x, digits = DIGITS, format = "f")
p_fmt <- function(x) formatC(x, digits = 4, format = "e")

# ---- LOAD AND VERIFY --------------------------------------------------------
stopifnot(file.exists(PANEL_PATH))
cat("Panel file mtime:", format(file.mtime(PANEL_PATH)), "\n")
panel <- readRDS(PANEL_PATH)

required_cols <- c("season", "week", "player_id", "position",
                   "fantasy_pts_actual", "fantasy_pts_roll3", "fantasy_pts_std")
missing_cols <- setdiff(required_cols, names(panel))
if (length(missing_cols) > 0) stop("Missing columns: ", paste(missing_cols, collapse = ", "))

stopifnot(
  is.numeric(panel$fantasy_pts_actual),
  is.numeric(panel$fantasy_pts_roll3),
  is.numeric(panel$fantasy_pts_std),
  !anyNA(panel[, c("fantasy_pts_actual", "fantasy_pts_roll3", "fantasy_pts_std")])
)

n_dup <- panel %>% count(player_id, season, week) %>% filter(n > 1) %>% nrow()
n_imp <- sum(panel$games_played_prior > panel$week - 1)
cat("Duplicate player-weeks:", n_dup, " impossible prior counts:", n_imp, "\n")
if (n_dup > 0 || n_imp > 0) stop("Panel is not the rebuilt version. Rerun R/18 first.")
stopifnot(setequal(unique(panel$season), EXPECTED_SEASONS))

cat("Rows:", nrow(panel), " Players:", n_distinct(panel$player_id), "\n")
print(count(panel, position))

# ---- PER-ROW ERRORS ----------------------------------------------------------
# d = err_roll3 - err_std. Positive d means the rolling 3-game average missed
# by more than the season-to-date average on that player-week.
panel <- panel %>%
  mutate(
    err_std   = abs(fantasy_pts_actual - fantasy_pts_std),
    err_roll3 = abs(fantasy_pts_actual - fantasy_pts_roll3),
    d         = err_roll3 - err_std
  )

# ---- CORE COMPARISON: paired vs independent on the same columns --------------
compare_block <- function(df, label) {
  n <- nrow(df)
  if (n < 3) { cat(label, ": n < 3, skipped\n"); return(invisible(NULL)) }

  m_std <- mean(df$err_std); m_r3 <- mean(df$err_roll3)
  v_std <- var(df$err_std);  v_r3 <- var(df$err_roll3)
  cov_e <- cov(df$err_std, df$err_roll3)
  r_e   <- cor(df$err_std, df$err_roll3)
  m_d   <- mean(df$d);       v_d  <- var(df$d); sd_d <- sqrt(v_d)

  # Paired: SE from the spread of the differences
  se_pair <- sd_d / sqrt(n)
  t_pair  <- m_d / se_pair
  tt_pair <- t.test(df$err_roll3, df$err_std, paired = TRUE)

  # Independent (Welch) on the same two columns, ignoring the pairing
  se_ind  <- sqrt(v_r3 / n + v_std / n)
  tt_ind  <- t.test(df$err_roll3, df$err_std, paired = FALSE, var.equal = FALSE)

  cat("\n==================", label, "==================\n")
  cat("n pairs:", n, "\n")
  cat("mean abs error  std:", fmt(m_std), "  roll3:", fmt(m_r3), "\n")
  cat("mean d (roll3 - std):", fmt(m_d), "\n")
  cat("var err_std:", fmt(v_std), "  var err_roll3:", fmt(v_r3), "\n")
  cat("cov(errors):", fmt(cov_e), "  cor(errors):", fmt(r_e), "\n")
  cat("var(d):", fmt(v_d), "  check var1+var2-2cov:", fmt(v_std + v_r3 - 2 * cov_e), "\n")
  cat("sd(d):", fmt(sd_d), "\n")
  cat("PAIRED    SE:", fmt(se_pair), " t (hand):", fmt(t_pair),
      " t (t.test):", fmt(unname(tt_pair$statistic)),
      " df:", unname(tt_pair$parameter), " p:", p_fmt(tt_pair$p.value), "\n")
  cat("          95% CI:", fmt(tt_pair$conf.int[1]), fmt(tt_pair$conf.int[2]), "\n")
  cat("WELCH     SE:", fmt(se_ind), " t:", fmt(unname(tt_ind$statistic)),
      " df:", fmt(unname(tt_ind$parameter)), " p:", p_fmt(tt_ind$p.value), "\n")
  cat("          95% CI:", fmt(tt_ind$conf.int[1]), fmt(tt_ind$conf.int[2]), "\n")
  cat("SE ratio independent / paired:", fmt(se_ind / se_pair), "\n")
  cat("share d > 0:", fmt(mean(df$d > 0)), "  d < 0:", fmt(mean(df$d < 0)),
      "  d == 0:", fmt(mean(df$d == 0)), "\n")
  invisible(NULL)
}

compare_block(panel, "POOLED, player-week pairs")
for (pos in sort(unique(panel$position))) {
  compare_block(filter(panel, position == pos), paste("POSITION", pos, "player-week pairs"))
}

# ---- CLUSTER CHECK: one pair per player -------------------------------------
# Player-weeks repeat the same player. Averaging each player's errors first
# gives one pair per player, which removes that repetition from n.
rows_per_player <- count(panel, player_id)
cat("\nRows per player: ")
print(summary(rows_per_player$n))

player_level <- panel %>%
  group_by(player_id, position) %>%
  summarise(err_std = mean(err_std), err_roll3 = mean(err_roll3),
            n_weeks = n(), .groups = "drop") %>%
  mutate(d = err_roll3 - err_std)

# A player whose panel rows span positions would appear twice here
n_multi_pos <- player_level %>% count(player_id) %>% filter(n > 1) %>% nrow()
cat("Players appearing under more than one position:", n_multi_pos, "\n")

compare_block(player_level, "POOLED, player-level pairs")
for (pos in sort(unique(player_level$position))) {
  compare_block(filter(player_level, position == pos), paste("POSITION", pos, "player-level pairs"))
}

# ---- DIRECTION CHECK ON SQUARED ERROR ---------------------------------------
sq <- panel %>%
  summarise(mse_std = mean((fantasy_pts_actual - fantasy_pts_std)^2),
            mse_roll3 = mean((fantasy_pts_actual - fantasy_pts_roll3)^2))
cat("\nMSE std:", fmt(sq$mse_std), "  MSE roll3:", fmt(sq$mse_roll3), "\n")

cat("\nDone. Paste the full console output back into the Statistics Series chat.\n")
