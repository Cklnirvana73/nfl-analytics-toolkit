# ==============================================================================
# stats_post44_pull_v3.R
# Statistics Series Post 44: Simple Linear Regression (Concept 43), Option 1
#
# READ-ONLY. x = a wide receiver's 2025 regular-season targets,
# y = his 2025 regular-season total fantasy points (flat full PPR, all sources).
# Same-season description, not a forecast.
#
# Toolkit pieces, unchanged:
#   R/02 get_player_receiving_stats()   targets (games from the fantasy table)
#   R/05 calculate_fantasy_points()     flat full PPR (the default)
#   R/01 get_roster_data()              positions (needs internet for nflreadr)
#   R/06 run_regression_analysis()      gated against lm() with one predictor
#
# Rules fixed before seeing results:
#   - Regular season only. WRs by roster position. Minimum 20 targets
#     (the toolkit's own week 4 study minimum, analysis/week4_predictive_study.R).
#     Sensitivity at 10 and 40 targets.
#   - Assumption checks before any fit is trusted (detailed-patterns.md,
#     Linear Regression ORDER checklist): linearity, independence (by design),
#     constant spread, residual normality, influential points.
#   - Worked examples: the WR with the most targets, and the WR nearest the
#     median target count.
#   - Per-game version (targets per game -> points per game) fit alongside,
#     because season totals also carry games played.
#
# Writes only to stats_posts/output/data/stats_post44v3_*.csv.
# Source: GitHub Cklnirvana73/nfl-analytics-toolkit, commit d7be3bf
# ==============================================================================

# ---- 0. SETUP ----------------------------------------------------------------

TOOLKIT_ROOT <- here::here()
stopifnot(dir.exists(TOOLKIT_ROOT))

SEASON      <- 2025L
MIN_TARGETS <- 20L
SENS        <- c(10L, 40L)
TOL         <- 1e-9

library(dplyr)
library(glue)
library(purrr)

source(file.path(TOOLKIT_ROOT, "R", "01_data_loading.R"))
source(file.path(TOOLKIT_ROOT, "R", "02_player_stats.R"))
source(file.path(TOOLKIT_ROOT, "R", "05_consistency_metrics.R"))
source(file.path(TOOLKIT_ROOT, "R", "06_predictive_validation.R"))

cache_file <- file.path(TOOLKIT_ROOT, "data", "season2_cache",
                        glue("pbp_normalized_{SEASON}.rds"))
stopifnot(file.exists(cache_file))
pbp <- readRDS(cache_file) %>% filter(season == SEASON, season_type == "REG")
cat("Regular-season plays:", format(nrow(pbp), big.mark = ","),
    "| weeks", min(pbp$week), "to", max(pbp$week), "\n")
stopifnot(max(pbp$week) == 18)

roster <- get_roster_data(SEASON)
pos_lookup <- roster %>%
  filter(!is.na(gsis_id), !is.na(position)) %>%
  distinct(player_id = gsis_id, position) %>%
  group_by(player_id) %>% filter(n() == 1) %>% ungroup()

# ---- 1. BUILD THE TABLE ------------------------------------------------------

cat("\n================ SECTION 1: TABLE ================\n")
rec <- get_player_receiving_stats(pbp, season = SEASON)
cat("Receiving columns:\n"); print(names(rec))
stopifnot(all(c("player_id", "player_name", "targets") %in% names(rec)))

# get_player_receiving_stats() groups by receiver id AND name, so a receiver
# whose name is spelled two ways gets two rows. Targets are play counts, so
# summing across a player's rows is exact. Games come from the fantasy table.
dups <- rec %>% count(player_id) %>% filter(n > 1)
cat("Receiver ids with more than one row (name spellings):", nrow(dups), "\n")
if (nrow(dups) > 0) print(rec %>% filter(player_id %in% dups$player_id) %>%
                            select(player_id, player_name, targets) %>% arrange(player_id))
rec1 <- rec %>%
  group_by(player_id) %>%
  summarise(player_name = first(player_name), targets = sum(targets), .groups = "drop")
stopifnot(sum(rec1$targets) == sum(rec$targets), anyDuplicated(rec1$player_id) == 0)

fp <- calculate_fantasy_points(pbp, roster_data = roster, season = SEASON)
stopifnot(all(c("player_id", "game_id", "total_fantasy_points") %in% names(fp)))
fp_season <- fp %>%
  group_by(player_id) %>%
  summarise(total_points = sum(total_fantasy_points),
            fp_games = n_distinct(game_id), .groups = "drop")

wr_all <- rec1 %>%
  inner_join(pos_lookup, by = "player_id", relationship = "one-to-one") %>%
  filter(position == "WR") %>%
  left_join(fp_season, by = "player_id", relationship = "one-to-one") %>%
  mutate(total_points = coalesce(total_points, 0))
stopifnot(!anyNA(wr_all$fp_games))
cat("WRs with at least one target:", nrow(wr_all), "\n")

wr <- wr_all %>% filter(targets >= MIN_TARGETS) %>%
  mutate(tpg = ifelse(fp_games > 0, targets / fp_games, NA_real_),
         ppg = ifelse(fp_games > 0, total_points / fp_games, NA_real_))
cat("WRs with", MIN_TARGETS, "+ targets:", nrow(wr), "\n")
stopifnot(nrow(wr) > 30, !anyNA(wr$total_points))

# ---- 2. THE FIT --------------------------------------------------------------

cat("\n================ SECTION 2: FIT ================\n")
m  <- lm(total_points ~ targets, data = wr)
sm <- summary(m)
fit <- tibble(
  n = nrow(wr), slope = unname(coef(m)[2]), intercept = unname(coef(m)[1]),
  r = cor(wr$targets, wr$total_points), r_squared = sm$r.squared,
  resid_sd = sm$sigma,
  x_min = min(wr$targets), x_median = median(wr$targets), x_max = max(wr$targets),
  y_min = min(wr$total_points), y_median = median(wr$total_points),
  y_max = max(wr$total_points)
)
print(fit, width = Inf)

tk <- run_regression_analysis(
  train_stats = wr %>% select(player_id, targets),
  test_stats  = wr %>% select(player_id, total_points),
  predictors  = "targets", outcome = "total_points")
stopifnot(abs(tk$coefficients$estimate[tk$coefficients$term == "targets"] - fit$slope) < TOL,
          abs(tk$coefficients$estimate[tk$coefficients$term == "(Intercept)"] - fit$intercept) < TOL)
cat("GATE PASSED: run_regression_analysis() with one predictor = lm()\n")

# ---- 3. ASSUMPTION CHECKS ----------------------------------------------------

cat("\n================ SECTION 3: ASSUMPTIONS ================\n")
wr <- wr %>% mutate(fitted = fitted(m), resid = resid(m))

# 3a. Linearity: five target groups, group mean vs the line
bins <- wr %>%
  mutate(bin = ntile(targets, 5)) %>%
  group_by(bin) %>%
  summarise(n = n(), x_lo = min(targets), x_hi = max(targets),
            x_mean = mean(targets), y_mean = mean(total_points),
            mean_resid = mean(resid), .groups = "drop") %>%
  mutate(line_at_x_mean = fit$intercept + fit$slope * x_mean)
cat("\n3a. Linearity (group mean minus line = mean_resid):\n"); print(bins, width = Inf)
quad <- lm(total_points ~ targets + I(targets^2), data = wr)
cat("Quadratic term p-value:",
    signif(summary(quad)$coefficients[3, 4], 3), "\n")

# 3b. Independence: one row per player, one season (cross-section). Stated, not tested.
cat("\n3b. Independence: one row per player; Durbin-Watson not applicable to unordered rows.\n")

# 3c. Constant spread: residual size by target group, plus Breusch-Pagan
spread <- wr %>% mutate(bin = ntile(targets, 5)) %>%
  group_by(bin) %>% summarise(sd_resid = sd(resid), .groups = "drop")
cat("\n3c. Residual SD by target group:\n"); print(spread)
bp_aux <- lm(I(resid^2) ~ targets, data = wr)
bp_stat <- nrow(wr) * summary(bp_aux)$r.squared
cat("Breusch-Pagan (studentized): stat", round(bp_stat, 2), "| p",
    signif(pchisq(bp_stat, df = 1, lower.tail = FALSE), 3), "\n")

# 3d. Residual normality
sw <- shapiro.test(wr$resid)
cat("\n3d. Shapiro-Wilk on residuals: W", round(sw$statistic, 3), "| p",
    signif(sw$p.value, 3), "\n")

# 3e. Influential points
cooks <- cooks.distance(m)
wr$cooks <- cooks
cat("\n3e. Top 5 Cook's distance:\n")
print(wr %>% arrange(desc(cooks)) %>%
        select(player_name, targets, total_points, fitted, resid, cooks) %>% head(5))
drop1 <- lm(total_points ~ targets, data = wr[-which.max(cooks), ])
cat("Slope without the most influential WR:", round(coef(drop1)[2], 4), "\n")

# ---- 4. WORKED EXAMPLES AND READINGS -----------------------------------------

cat("\n================ SECTION 4: EXAMPLES ================\n")
line_at <- function(x) fit$intercept + fit$slope * x
ex <- bind_rows(
  wr %>% arrange(desc(targets)) %>% slice(1) %>% mutate(role = "most targets"),
  wr %>% mutate(d = abs(targets - median(targets))) %>% arrange(d, desc(targets)) %>%
    slice(1) %>% select(-d) %>% mutate(role = "nearest median")
) %>% select(role, player_name, targets, fp_games, total_points, fitted, resid)
print(ex, width = Inf)

grid <- tibble(targets = c(25, 50, 100, 150)) %>%
  mutate(in_range = targets >= fit$x_min & targets <= fit$x_max,
         line_points = line_at(targets))
cat("\nLine read at fixed target counts:\n"); print(grid)

# ---- 5. PER-GAME VERSION AND GAMES PLAYED ------------------------------------

cat("\n================ SECTION 5: PER GAME ================\n")
pg <- lm(ppg ~ tpg, data = wr)
pg_fit <- tibble(n = nrow(wr), slope = unname(coef(pg)[2]),
                 intercept = unname(coef(pg)[1]),
                 r = cor(wr$tpg, wr$ppg), r_squared = summary(pg)$r.squared,
                 x_min = min(wr$tpg), x_max = max(wr$tpg))
print(pg_fit, width = Inf)
cat("Correlation of games played with targets:",
    round(cor(wr$fp_games, wr$targets), 3), "\n")

# ---- 6. SENSITIVITY TO THE TARGET MINIMUM -----------------------------------

cat("\n================ SECTION 6: SENSITIVITY ================\n")
sens <- map_dfr(c(SENS[1], MIN_TARGETS, SENS[2]), function(k) {
  d <- wr_all %>% filter(targets >= k)
  mm <- lm(total_points ~ targets, data = d)
  tibble(min_targets = k, n = nrow(d), slope = unname(coef(mm)[2]),
         intercept = unname(coef(mm)[1]), r = cor(d$targets, d$total_points))
})
print(sens, width = Inf)

# ---- 7. WRITE CSVs -----------------------------------------------------------

out_dir <- file.path(TOOLKIT_ROOT, "stats_posts", "output", "data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
write.csv(wr,     file.path(out_dir, "stats_post44v3_wr.csv"), row.names = FALSE)
write.csv(fit,    file.path(out_dir, "stats_post44v3_fit.csv"), row.names = FALSE)
write.csv(bins,   file.path(out_dir, "stats_post44v3_bins.csv"), row.names = FALSE)
write.csv(ex,     file.path(out_dir, "stats_post44v3_examples.csv"), row.names = FALSE)
write.csv(pg_fit, file.path(out_dir, "stats_post44v3_per_game.csv"), row.names = FALSE)
write.csv(sens,   file.path(out_dir, "stats_post44v3_sensitivity.csv"), row.names = FALSE)
cat("\nWrote 6 CSVs to", out_dir, "\nALL GATES PASSED\n")
