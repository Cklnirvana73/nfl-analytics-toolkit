# ==============================================================================
# stats_post44_pull_v2.R
# Statistics Series Post 44: Simple Linear Regression (Concept 43), rebuild
#
# READ-ONLY. Replaces the week-number anchor. x = a player's fantasy points per
# game in weeks 1 to 8, y = his points per game in weeks 9 to 18, 2025 regular
# season. Uses the toolkit's own pieces unchanged:
#   R/06 split_season_by_week()   the toolkit's predictive-study split (week 8)
#   R/05 calculate_fantasy_points() flat full PPR (the default)
#   R/06 run_regression_analysis() gated against lm() with one predictor
#   R/01 get_roster_data()         positions (needs internet for nflreadr)
#
# Rules fixed before seeing results:
#   - Regular season only. Minimum 4 games in EACH half (sensitivity at 3 and 5).
#   - Positions QB, RB, WR, TE fit separately; WR is the worked example
#     (largest group). Pooled fit printed for reference only.
#   - Worked example player: the WR with the highest weeks 1-8 points per game.
#
# Writes only to stats_posts/output/data/stats_post44v2_*.csv.
# Source: GitHub Cklnirvana73/nfl-analytics-toolkit, commit d7be3bf
# ==============================================================================

# ---- 0. SETUP ----------------------------------------------------------------

TOOLKIT_ROOT <- here::here()
stopifnot(dir.exists(TOOLKIT_ROOT))

SEASON    <- 2025L
SPLIT_WK  <- 8L
MIN_GAMES <- 4L
SENS      <- c(3L, 5L)
POSITIONS <- c("QB", "RB", "WR", "TE")
ANCHOR    <- "WR"
TOL       <- 1e-9

library(dplyr)
library(glue)
library(purrr)

source(file.path(TOOLKIT_ROOT, "R", "01_data_loading.R"))
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
cat("Roster rows:", nrow(roster), "\n")

# ---- 1. TOOLKIT SPLIT AND SCORING -------------------------------------------

cat("\n================ SECTION 1: SPLIT AND SCORING ================\n")
splits <- split_season_by_week(pbp, split_week = SPLIT_WK)
stopifnot(max(splits$train$week) == SPLIT_WK, min(splits$test$week) == SPLIT_WK + 1)

score_half <- function(pbp_half) {
  calculate_fantasy_points(pbp_half, roster_data = roster, season = SEASON)
}
fp_train <- score_half(splits$train)
fp_test  <- score_half(splits$test)

cat("\nColumns returned:\n"); print(names(fp_train))
stopifnot(all(c("player_id", "player_name", "position", "week", "game_id",
                "total_fantasy_points") %in% names(fp_train)))

per_half <- function(fp, suffix) {
  fp %>%
    group_by(player_id) %>%
    summarise(
      player_name = first(player_name),
      position    = names(sort(table(position), decreasing = TRUE))[1],
      games       = n_distinct(game_id),
      points      = sum(total_fantasy_points),
      .groups     = "drop"
    ) %>%
    mutate(ppg = ifelse(games > 0, points / games, NA_real_)) %>%
    rename_with(~ paste0(.x, suffix), c(position, games, points, ppg))
}

h1 <- per_half(fp_train, "_h1")
h2 <- per_half(fp_test, "_h2") %>% select(-player_name)

paired_all <- h1 %>%
  inner_join(h2, by = "player_id", relationship = "one-to-one") %>%
  filter(position_h1 %in% POSITIONS)
cat("Players appearing in both halves (QB/RB/WR/TE):", nrow(paired_all), "\n")
cat("Position changed between halves:",
    sum(paired_all$position_h1 != paired_all$position_h2), "(h1 position used)\n")

paired <- paired_all %>%
  filter(games_h1 >= MIN_GAMES, games_h2 >= MIN_GAMES) %>%
  rename(position = position_h1)
stopifnot(!anyNA(paired$ppg_h1), !anyNA(paired$ppg_h2))

# ---- 2. ONE LINE PER POSITION ------------------------------------------------

cat("\n================ SECTION 2: FIT BY POSITION ================\n")

fit_one <- function(df, label) {
  m <- lm(ppg_h2 ~ ppg_h1, data = df)
  s <- summary(m)
  tibble(
    group      = label,
    n          = nrow(df),
    slope      = unname(coef(m)[2]),
    intercept  = unname(coef(m)[1]),
    r          = cor(df$ppg_h1, df$ppg_h2),
    r_squared  = s$r.squared,
    resid_sd   = s$sigma,
    x_min      = min(df$ppg_h1),
    x_median   = median(df$ppg_h1),
    x_max      = max(df$ppg_h1),
    y_mean     = mean(df$ppg_h2),
    x_mean     = mean(df$ppg_h1)
  )
}

fits <- bind_rows(
  map_dfr(POSITIONS, ~ fit_one(filter(paired, position == .x), .x)),
  fit_one(paired, "ALL (reference only)")
)
print(fits, width = Inf)

# ---- 3. GATE: the toolkit's regression function does the same fit -----------

cat("\n================ SECTION 3: TOOLKIT FUNCTION GATE ================\n")
wr <- filter(paired, position == ANCHOR)
tk <- run_regression_analysis(
  train_stats = wr %>% select(player_id, ppg_h1),
  test_stats  = wr %>% select(player_id, ppg_h2),
  predictors  = "ppg_h1",
  outcome     = "ppg_h2"
)
tk_coef <- tk$coefficients
wr_fit  <- filter(fits, group == ANCHOR)
stopifnot(abs(tk_coef$estimate[tk_coef$term == "ppg_h1"] - wr_fit$slope) < TOL,
          abs(tk_coef$estimate[tk_coef$term == "(Intercept)"] - wr_fit$intercept) < TOL,
          tk$sample_size == wr_fit$n)
cat("GATE PASSED: run_regression_analysis() with one predictor = lm() for", ANCHOR, "\n")

# ---- 4. WORKED EXAMPLE (WR) --------------------------------------------------

cat("\n================ SECTION 4: WORKED EXAMPLE ================\n")
line_wr <- function(x) wr_fit$intercept + wr_fit$slope * x

top <- wr %>% arrange(desc(ppg_h1)) %>% slice(1)
cat(glue("Top weeks 1-8 WR: {top$player_name} | games {top$games_h1}/{top$games_h2} | ",
         "h1 {round(top$ppg_h1, 2)} | line {round(line_wr(top$ppg_h1), 2)} | ",
         "h2 actual {round(top$ppg_h2, 2)}"), "\n")

grid <- tibble(x = c(5, 10, 15, 20)) %>%
  mutate(in_range = x >= wr_fit$x_min & x <= wr_fit$x_max,
         line_y   = line_wr(x),
         gap_to_x = line_y - x)
cat("\nLine read at fixed first-half values (WR):\n"); print(grid)
cat(glue("Line meets y = x at {round(wr_fit$intercept / (1 - wr_fit$slope), 2)} PPG ",
         "(below: line above x; above: line below x)"), "\n")

# ---- 5. LINEARITY CHECK: binned means vs the line ---------------------------

cat("\n================ SECTION 5: LINEARITY CHECK (WR) ================\n")
bins <- wr %>%
  mutate(bin = ntile(ppg_h1, 5)) %>%
  group_by(bin) %>%
  summarise(n = n(), x_mean = mean(ppg_h1), y_mean = mean(ppg_h2),
            x_lo = min(ppg_h1), x_hi = max(ppg_h1), .groups = "drop") %>%
  mutate(line_at_x_mean = line_wr(x_mean), bin_minus_line = y_mean - line_at_x_mean)
print(bins, width = Inf)

# ---- 6. IN-SAMPLE COMPARISON WITH THE SLOPE-1 PREDICTION ---------------------

cat("\n================ SECTION 6: LINE vs 'SAME AS FIRST HALF' (in-sample) ================\n")
cmp <- paired %>%
  group_by(position) %>%
  group_modify(~ {
    m <- lm(ppg_h2 ~ ppg_h1, data = .x)
    tibble(n = nrow(.x),
           mae_line = mean(abs(.x$ppg_h2 - fitted(m))),
           mae_same = mean(abs(.x$ppg_h2 - .x$ppg_h1)),
           share_line_closer = mean(abs(.x$ppg_h2 - fitted(m)) < abs(.x$ppg_h2 - .x$ppg_h1)))
  }) %>% ungroup()
print(cmp, width = Inf)
cat("In-sample: the line was fit to these same players.\n")

# ---- 7. SENSITIVITY TO THE GAMES MINIMUM ------------------------------------

cat("\n================ SECTION 7: SENSITIVITY ================\n")
sens <- map_dfr(c(SENS[1], MIN_GAMES, SENS[2]), function(k) {
  d <- paired_all %>% filter(games_h1 >= k, games_h2 >= k, position_h1 == ANCHOR) %>%
    rename(position = position_h1)
  fit_one(d, ANCHOR) %>% mutate(min_games = k, .before = 1)
})
print(sens %>% select(min_games, n, slope, intercept, r, x_min, x_max), width = Inf)

# ---- 8. WRITE CSVs -----------------------------------------------------------

out_dir <- file.path(TOOLKIT_ROOT, "stats_posts", "output", "data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
write.csv(paired, file.path(out_dir, "stats_post44v2_pairs.csv"), row.names = FALSE)
write.csv(fits,   file.path(out_dir, "stats_post44v2_fits.csv"), row.names = FALSE)
write.csv(bins,   file.path(out_dir, "stats_post44v2_wr_bins.csv"), row.names = FALSE)
write.csv(grid,   file.path(out_dir, "stats_post44v2_wr_grid.csv"), row.names = FALSE)
write.csv(cmp,    file.path(out_dir, "stats_post44v2_line_vs_same.csv"), row.names = FALSE)
write.csv(sens,   file.path(out_dir, "stats_post44v2_sensitivity.csv"), row.names = FALSE)
cat("\nWrote 6 CSVs to", out_dir, "\n")
cat("ALL GATES PASSED\n")
