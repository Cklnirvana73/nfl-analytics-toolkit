# pull_stats_post32_one_way_anova_data.R
# Statistics Series Post 32: one-way ANOVA (Concept 32, [GAP], hands-on demo)
#
# Demo built on toolkit OUTPUT. The toolkit never runs an ANOVA.
# Source: data/season2_cache/s2_week4_ab_panel.rds (R/18 rebuild, 2026-09-17)
#
# Unit: one row per player, 2025 season. Each player's value is the mean
# absolute error of a lagged predictor across that player's eligible weeks.
# One row per player keeps the units independent.
#
# What this script prints:
#   1. Why no TE rows exist (prefilter counts + R/18 position filter lines).
#   2. Panel checks for 2025.
#   3. Per-player table and weeks-per-player by position.
#   4. For each outcome x floor: group n / mean / sd / var, the between and
#      within sums of squares by hand, F by hand vs aov(), p, critical F,
#      between share of total, largest/smallest variance ratio, Welch F,
#      Shapiro-Wilk per group.
#   5. Scale diagnostic: scoring level by position.
#
# Nothing is written to disk. Every number the post uses comes from this run.

library(here)
library(dplyr)

SEASON     <- 2025L
POSITIONS  <- c("QB", "RB", "WR")
FLOORS     <- c(4L, 8L, 12L)
MAIN_FLOOR <- 8L
ALPHA      <- 0.05
N_ROWS_EXPECTED <- 2922L   # from the 2026-09-24 source check

fmt   <- function(x, d = 6) formatC(x, format = "f", digits = d)
p_fmt <- function(p) formatC(p, format = "g", digits = 6)

path_panel <- here::here("data", "season2_cache", "s2_week4_ab_panel.rds")
if (!file.exists(path_panel)) stop("Missing: ", path_panel, call. = FALSE)
panel_all <- readRDS(path_panel)

need <- c("season", "week", "player_id", "player_name", "position", "team",
          "fantasy_pts_actual", "fantasy_pts_roll3", "fantasy_pts_std",
          "games_played_prior")
missing_cols <- setdiff(need, names(panel_all))
if (length(missing_cols) > 0) stop("Missing columns: ", paste(missing_cols, collapse = ", "))
stopifnot(is.numeric(panel_all$fantasy_pts_actual),
          is.numeric(panel_all$fantasy_pts_std),
          is.numeric(panel_all$fantasy_pts_roll3),
          is.character(panel_all$position))

# ---- 1. TE SCOPE ------------------------------------------------------------
cat("\n==================== 1. TE SCOPE ====================\n")
pre <- attr(panel_all, "position_counts_prefilter")
if (is.null(pre)) {
  cat("No position_counts_prefilter attribute.\n")
} else {
  print(filter(pre, season == SEASON), n = Inf)
}
r_files <- list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE)
r18     <- r_files[grepl("^18_", basename(r_files))]
cat("R/18 file(s):", if (length(r18)) basename(r18) else "NONE FOUND", "\n")
for (f in r18) {
  src  <- readLines(f, warn = FALSE)
  hits <- grep("position.*%in%|%in%.*position|\"TE\"|c\\(\"QB\"", src)
  cat("Lines matching position filters in", basename(f), ":\n")
  if (length(hits)) cat(sprintf("%5d: %s", hits, src[hits]), sep = "\n") else cat("  none\n")
}

# ---- 2. PANEL CHECKS, 2025 ---------------------------------------------------
cat("\n==================== 2. PANEL CHECKS ====================\n")
p25 <- panel_all %>% filter(season == SEASON)
stopifnot(nrow(p25) == N_ROWS_EXPECTED)
stopifnot(setequal(unique(p25$position), POSITIONS))
dup <- p25 %>% count(player_id, week) %>% filter(n > 1)
cat("Duplicate player-week rows:", nrow(dup), "\n")
stopifnot(nrow(dup) == 0)

p25 <- p25 %>%
  mutate(err_std   = abs(fantasy_pts_actual - fantasy_pts_std),
         err_roll3 = abs(fantasy_pts_actual - fantasy_pts_roll3))
stopifnot(!anyNA(p25$err_std), !anyNA(p25$err_roll3))
print(count(p25, position))

# ---- 3. PER-PLAYER TABLE -----------------------------------------------------
cat("\n==================== 3. PER-PLAYER TABLE ====================\n")
players <- p25 %>%
  arrange(player_id, week) %>%
  group_by(player_id) %>%
  summarise(player_name = last(player_name),
            n_pos       = n_distinct(position),
            position    = first(position),
            team        = last(team),
            n_teams     = n_distinct(team),
            n_weeks     = n(),
            mae_std     = mean(err_std),
            mae_roll3   = mean(err_roll3),
            mean_actual = mean(fantasy_pts_actual),
            .groups = "drop")
stopifnot(all(players$n_pos == 1))
cat("Players:", nrow(players), " Multi-team players:", sum(players$n_teams > 1), "\n")
cat("Weeks per player by position:\n")
players %>% group_by(position) %>%
  summarise(n_players = n(), min = min(n_weeks), median = median(n_weeks),
            max = max(n_weeks), .groups = "drop") %>% print()
for (fl in FLOORS) {
  cat("Floor", fl, "players:\n")
  print(count(filter(players, n_weeks >= fl), position))
}

# ---- 4. ONE-WAY ANOVA BLOCK --------------------------------------------------
anova_block <- function(df, outcome, label) {
  d <- data.frame(y = df[[outcome]],
                  g = droplevels(factor(df$position, levels = POSITIONS)))
  k <- nlevels(d$g); N <- nrow(d)
  cat("\n------------------", label, "------------------\n")
  grp <- d %>% group_by(g) %>%
    summarise(n = n(), mean = mean(y), median = median(y),
              sd = sd(y), var = var(y), .groups = "drop")
  if (k < 3 || any(grp$n < 3)) { cat("Fewer than 3 groups or a group under n = 3; skipped\n"); print(grp); return(invisible(NULL)) }
  print(as.data.frame(grp), digits = 7)

  grand <- mean(d$y)
  ssb <- sum(grp$n * (grp$mean - grand)^2)
  ssw <- sum((grp$n - 1) * grp$var)
  sst <- sum((d$y - grand)^2)
  df_b <- k - 1L; df_w <- N - k
  msb <- ssb / df_b; msw <- ssw / df_w
  f_hand <- msb / msw
  p_hand <- pf(f_hand, df_b, df_w, lower.tail = FALSE)
  f_crit <- qf(1 - ALPHA, df_b, df_w)

  f_aov   <- summary(aov(y ~ g, data = d))[[1]][["F value"]][1]
  f_eq    <- oneway.test(y ~ g, data = d, var.equal = TRUE)
  welch   <- oneway.test(y ~ g, data = d, var.equal = FALSE)
  v_ratio <- if (min(grp$var) > 0) max(grp$var) / min(grp$var) else NA_real_

  stopifnot(isTRUE(all.equal(sst, ssb + ssw)),
            isTRUE(all.equal(f_hand, f_aov)),
            isTRUE(all.equal(f_hand, unname(f_eq$statistic))))

  cat("N:", N, " k:", k, " grand mean:", fmt(grand), "\n")
  cat("SS between:", fmt(ssb), " SS within:", fmt(ssw), " SS total:", fmt(sst),
      " check SSB+SSW==SST: TRUE\n")
  cat("df between:", df_b, " df within:", df_w, "\n")
  cat("MS between:", fmt(msb), " MS within:", fmt(msw), "\n")
  cat("F (hand):", fmt(f_hand), " F (aov):", fmt(f_aov), " p:", p_fmt(p_hand), "\n")
  cat("Critical F at alpha", ALPHA, ":", fmt(f_crit), "\n")
  cat("Between share of total SS:", fmt(ssb / sst), "\n")
  cat("Largest / smallest group variance:", fmt(v_ratio, 4), "\n")
  cat("Welch F:", fmt(unname(welch$statistic)),
      " df:", fmt(welch$parameter[1], 4), ",", fmt(welch$parameter[2], 4),
      " p:", p_fmt(welch$p.value), "\n")
  for (lv in levels(d$g)) {
    x <- d$y[d$g == lv]
    if (length(x) >= 3 && length(x) <= 5000) {
      sw <- shapiro.test(x)
      cat("Shapiro-Wilk", lv, " W:", fmt(unname(sw$statistic), 4), " p:", p_fmt(sw$p.value), "\n")
    }
  }
  invisible(NULL)
}

cat("\n==================== 4. ONE-WAY ANOVA ====================\n")
for (outcome in c("mae_std", "mae_roll3")) {
  for (fl in FLOORS) {
    anova_block(filter(players, n_weeks >= fl), outcome,
                paste(outcome, "| floor", fl, if (fl == MAIN_FLOOR) "(MAIN)" else ""))
  }
}

# ---- 5. SCALE DIAGNOSTIC -----------------------------------------------------
cat("\n==================== 5. SCALE DIAGNOSTIC (floor", MAIN_FLOOR, ") ====================\n")
players %>%
  filter(n_weeks >= MAIN_FLOOR) %>%
  mutate(rel_std = if_else(mean_actual > 0, mae_std / mean_actual, NA_real_)) %>%
  group_by(position) %>%
  summarise(n = n(), mean_actual = mean(mean_actual),
            mean_mae_std = mean(mae_std),
            mean_rel_std = mean(rel_std, na.rm = TRUE),
            n_rel_na = sum(is.na(rel_std)), .groups = "drop") %>%
  as.data.frame() %>% print(digits = 7)

cat("\nDone. Copy the full console output back verbatim.\n")
