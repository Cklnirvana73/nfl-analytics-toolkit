# ==============================================================================
# stats_post44_pull.R
# Statistics Series Post 44: Simple Linear Regression (Concept 43)
#
# READ-ONLY. Reads the cached 2025 play-by-play, runs the toolkit's own
# calculate_epa_trends() (R/07) unchanged, and verifies what its rolling
# slope is: a simple linear regression of EPA per play on game week over a
# player's previous 4 games.
#
# Writes only to stats_posts/output/data/stats_post44_*.csv. Touches no toolkit file,
# no cache file.
#
# Sections
#   0. Setup and input checks
#   1. What R/07 produces (row counts, distribution of the 4-game slope)
#   2. One worked example, refit with lm() and gated against the toolkit
#   3. Every 4-game slope rebuilt by hand and gated against the toolkit
#   4. Does extending the line one week predict the next game?
#      (computed by this pull, not by the toolkit)
#   5. Write CSVs
#
# Source: GitHub Cklnirvana73/nfl-analytics-toolkit, commit d7be3bf
#   R/07_epa_features.R calculate_epa_trends()  lines 105-330
#   calc_slope() = cov(x, y) / var(x)           lines 247-254
# ==============================================================================

# ---- 0. SETUP ----------------------------------------------------------------

TOOLKIT_ROOT <- here::here()
stopifnot(dir.exists(TOOLKIT_ROOT))

SEASON  <- 2025L
WINDOW  <- 4L          # the 4-game window is the worked example
TOL     <- 1e-9        # gate tolerance for slope reproduction

library(dplyr)
library(glue)
library(zoo)

source(file.path(TOOLKIT_ROOT, "R", "07_epa_features.R"))

cache_file <- file.path(TOOLKIT_ROOT, "data", "season2_cache",
                        glue("pbp_normalized_{SEASON}.rds"))
stopifnot(file.exists(cache_file))
cat("Cache file:", cache_file, "\n")
cat("Cache modified:", format(file.mtime(cache_file)), "\n")

pbp_all <- readRDS(cache_file)
cat("Rows in cache:", format(nrow(pbp_all), big.mark = ","), "\n")

# Column check before any use
needed <- c("game_id", "season", "week", "play_type", "posteam", "epa",
            "success", "wp", "qtr", "passer_player_id", "rusher_player_id",
            "receiver_player_id", "passer_player_name", "rusher_player_name",
            "season_type")
missing <- setdiff(needed, names(pbp_all))
if (length(missing) > 0) stop("Missing columns: ", paste(missing, collapse = ", "))

cat("\nseason_type counts (plays):\n")
print(table(pbp_all$season_type, useNA = "ifany"))

# Regular season only. calculate_epa_trends() does not filter season_type
# itself, so postseason weeks would otherwise enter the rolling windows.
pbp <- pbp_all %>% filter(season == SEASON, season_type == "REG")
cat("Regular-season rows:", format(nrow(pbp), big.mark = ","),
    "| weeks", min(pbp$week), "to", max(pbp$week), "\n")
stopifnot(max(pbp$week) <= 18)
rm(pbp_all); invisible(gc())

# ---- 1. WHAT R/07 PRODUCES ---------------------------------------------------

cat("\n================ SECTION 1: R/07 OUTPUT ================\n")

trends <- calculate_epa_trends(pbp, season = SEASON)   # toolkit defaults

cat("\nColumns returned:\n"); print(names(trends))
trend_col <- paste0("epa_trend_", WINDOW)
level_col <- paste0("epa_level_", WINDOW)
stopifnot(all(c("player_id", "player_name", "season", "week", "posteam",
                "plays", "epa_per_play", "pass_plays", "run_plays",
                trend_col, level_col) %in% names(trends)))

cat("\nPlayer-weeks:", nrow(trends),
    "| players:", n_distinct(trends$player_id), "\n")
for (w in c(3, 4, 6)) {
  col <- paste0("epa_trend_", w)
  cat(glue("Valid {w}-game slopes: {sum(!is.na(trends[[col]]))}"), "\n")
}

s4 <- trends[[trend_col]][!is.na(trends[[trend_col]])]
dist_tbl <- tibble(
  stat  = c("n", "mean", "sd", "min", "p10", "p25", "median", "p75", "p90",
            "max", "share_positive"),
  value = c(length(s4), mean(s4), sd(s4), min(s4),
            quantile(s4, c(.10, .25, .50, .75, .90), names = FALSE),
            max(s4), mean(s4 > 0))
)
cat("\nDistribution of the 4-game slope (EPA per play, per game week):\n")
print(dist_tbl, n = Inf)

# ---- 2. ONE WORKED EXAMPLE ---------------------------------------------------

cat("\n================ SECTION 2: WORKED EXAMPLE ================\n")

# Fixed selection rule, decided before seeing results:
# the passer (pass_plays > run_plays across his season) with the most total
# plays, at his last week that has a valid 4-game slope.
passers <- trends %>%
  group_by(player_id, player_name) %>%
  summarise(total_plays = sum(plays),
            pass_share  = sum(pass_plays) / sum(plays),
            n_valid     = sum(!is.na(.data[[trend_col]])),
            .groups = "drop") %>%
  filter(pass_share > 0.5, n_valid > 0) %>%
  arrange(desc(total_plays))
stopifnot(nrow(passers) > 0)
ex_id <- passers$player_id[1]
cat("Example player:", passers$player_name[1], "| id", ex_id,
    "| season plays", passers$total_plays[1], "\n")

ex_rows <- trends %>%
  filter(player_id == ex_id) %>%
  arrange(season, posteam, week)
target <- ex_rows %>% filter(!is.na(.data[[trend_col]])) %>% slice_tail(n = 1)
stopifnot(nrow(target) == 1)

# The window: the 4 games before the target week, same player, season, team
window_df <- ex_rows %>%
  filter(posteam == target$posteam, week < target$week) %>%
  slice_tail(n = WINDOW) %>%
  select(week, plays, epa_per_play)
stopifnot(nrow(window_df) == WINDOW)

cat("\nTarget week:", target$week, "| team", target$posteam, "\n")
cat("The 4 points the toolkit fits (x = week, y = EPA per play):\n")
print(window_df)

fit <- lm(epa_per_play ~ week, data = window_df)
slope_lm     <- unname(coef(fit)["week"])
intercept_lm <- unname(coef(fit)["(Intercept)"])
slope_cov    <- cov(window_df$week, window_df$epa_per_play) / var(window_df$week)
slope_tk     <- target[[trend_col]]

cat("\nSlope, toolkit epa_trend_4:", format(slope_tk, digits = 10), "\n")
cat("Slope, cov(x, y) / var(x):  ", format(slope_cov, digits = 10), "\n")
cat("Slope, lm():                ", format(slope_lm, digits = 10), "\n")
cat("Intercept, lm() (toolkit never stores it):",
    format(intercept_lm, digits = 10), "\n")

# GATE 1: the toolkit's slope is the least-squares slope of this window
stopifnot(abs(slope_tk - slope_lm) < TOL, abs(slope_tk - slope_cov) < TOL)
cat("GATE 1 PASSED: toolkit slope = lm() slope = cov/var slope\n")

window_df <- window_df %>%
  mutate(fitted   = intercept_lm + slope_lm * week,
         residual = epa_per_play - fitted)
cat("\nFitted line at each point:\n"); print(window_df)

pred_next <- intercept_lm + slope_lm * target$week
cat("\nLine extended to the target week", target$week, ":",
    round(pred_next, 4), "| actual:", round(target$epa_per_play, 4),
    "| 4-game average:", round(target[[level_col]], 4), "\n")
cat("(Extension to the next week is computed by this pull, not the toolkit.)\n")

# ---- 3. EVERY 4-GAME SLOPE REBUILT ------------------------------------------

cat("\n================ SECTION 3: FULL REBUILD GATE ================\n")

rebuilt <- trends %>%
  arrange(player_id, season, posteam, week) %>%
  group_by(player_id, season, posteam) %>%
  mutate(
    x1 = lag(week, 1), x2 = lag(week, 2), x3 = lag(week, 3), x4 = lag(week, 4),
    y1 = lag(epa_per_play, 1), y2 = lag(epa_per_play, 2),
    y3 = lag(epa_per_play, 3), y4 = lag(epa_per_play, 4)
  ) %>%
  ungroup() %>%
  filter(!is.na(.data[[trend_col]]))

stopifnot(nrow(rebuilt) == length(s4))
stopifnot(!anyNA(rebuilt[, c("x1", "x2", "x3", "x4", "y1", "y2", "y3", "y4")]))

xm <- as.matrix(rebuilt[, c("x1", "x2", "x3", "x4")])
ym <- as.matrix(rebuilt[, c("y1", "y2", "y3", "y4")])
x_bar <- rowMeans(xm); y_bar <- rowMeans(ym)
sxy <- rowSums((xm - x_bar) * (ym - y_bar))
sxx <- rowSums((xm - x_bar)^2)
stopifnot(all(sxx > 0))

rebuilt <- rebuilt %>%
  mutate(slope_hand     = sxy / sxx,
         intercept_hand = y_bar - slope_hand * x_bar,
         line_next      = intercept_hand + slope_hand * week,
         avg_prev4      = y_bar)

max_diff <- max(abs(rebuilt$slope_hand - rebuilt[[trend_col]]))
cat("Rows rebuilt:", nrow(rebuilt),
    "| max |hand slope - toolkit slope|:", format(max_diff, digits = 3), "\n")
# GATE 2: every toolkit slope reproduces from its 4 prior games
stopifnot(max_diff < TOL)
cat("GATE 2 PASSED: all", nrow(rebuilt), "toolkit 4-game slopes reproduced\n")

max_lvl <- max(abs(rebuilt$avg_prev4 - rebuilt[[level_col]]))
# GATE 3: epa_level_4 is the mean of the same 4 games
stopifnot(max_lvl < TOL)
cat("GATE 3 PASSED: epa_level_4 = mean of the same 4 games\n")

# ---- 4. DOES THE LINE PREDICT THE NEXT GAME? (pull-computed) ----------------

cat("\n================ SECTION 4: NEXT-GAME CHECK ================\n")
cat("Computed by this pull. The toolkit does not extend its slopes.\n")

noise <- rebuilt %>%
  summarise(
    n                      = n(),
    cor_slope_vs_next      = cor(slope_hand, epa_per_play),
    mae_line_extended      = mean(abs(epa_per_play - line_next)),
    mae_prev4_average      = mean(abs(epa_per_play - avg_prev4)),
    rmse_line_extended     = sqrt(mean((epa_per_play - line_next)^2)),
    rmse_prev4_average     = sqrt(mean((epa_per_play - avg_prev4)^2)),
    share_line_closer      = mean(abs(epa_per_play - line_next) <
                                  abs(epa_per_play - avg_prev4)),
    sd_slope               = sd(slope_hand),
    sd_next_game_epa       = sd(epa_per_play)
  )
print(t(noise))

# Same check split by role, to see whether passers and rushers differ
by_role <- rebuilt %>%
  mutate(role = if_else(pass_plays > run_plays, "passer", "rusher")) %>%
  group_by(role) %>%
  summarise(n = n(),
            cor_slope_vs_next = cor(slope_hand, epa_per_play),
            mae_line_extended = mean(abs(epa_per_play - line_next)),
            mae_prev4_average = mean(abs(epa_per_play - avg_prev4)),
            .groups = "drop")
cat("\nBy role (passer = more pass plays than runs that week):\n")
print(by_role)

# ---- 5. WRITE CSVs -----------------------------------------------------------

out_dir <- file.path(TOOLKIT_ROOT, "stats_posts", "output", "data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write.csv(dist_tbl, file.path(out_dir, "stats_post44_slope_distribution.csv"),
          row.names = FALSE)
write.csv(window_df %>%
            mutate(player_name = passers$player_name[1],
                   target_week = target$week,
                   slope = slope_lm, intercept = intercept_lm,
                   line_next = pred_next,
                   actual_next = target$epa_per_play,
                   avg_prev4 = target[[level_col]]),
          file.path(out_dir, "stats_post44_example_window.csv"),
          row.names = FALSE)
write.csv(noise,   file.path(out_dir, "stats_post44_next_game.csv"),
          row.names = FALSE)
write.csv(by_role, file.path(out_dir, "stats_post44_next_game_by_role.csv"),
          row.names = FALSE)

cat("\nWrote 4 CSVs to", out_dir, "\n")
cat("dplyr", as.character(packageVersion("dplyr")),
    "| zoo", as.character(packageVersion("zoo")), "\n")
cat("ALL GATES PASSED\n")
