# pull_stats_post34_post_hoc_tests_data.R
# Statistics Series, Post 34 (Concept 34, post-hoc tests). Verification pull.
#
# Units   : 2025 players, one row each, from s2_week4_ab_panel.rds.
# Outcome : mae_std, the player's mean absolute miss of the season-to-date
#           forecast across his forecast games (same definition as Post 32).
# Groups  : position (QB, RB, WR).
# Question: one-way ANOVA (Post 32) said the three averages are not all equal.
#           Which pairs differ?
#
# Rules fixed BEFORE any numbers exist (2026-09-30 session):
#   - Headline method: Tukey-Kramer (unequal group sizes), all three pairs,
#     alpha 0.05, mae_std, floor 8. Not changed after seeing results.
#   - Cross-checks only (never the headline): unadjusted pooled t,
#     Bonferroni, Games-Howell (unequal spreads).
#   - Robustness: floors 4 and 12, the rolling 3-game forecast (mae_roll3),
#     and miss scaled by the player's own average points (rel_miss).
#   - Section 7 is a seeded label-shuffle on the real players. It is an
#     illustration of what the adjustment protects against, not a test result.
#
# Gate: section 2 reproduces Post 32's verified one-way numbers and stops if
# they do not match. The aggregation block is copied from the Post 33 pull
# script, which reproduced Post 32 exactly.
#
# Read-only. Writes nothing to disk. Run from the toolkit root and paste the
# full console output back verbatim.

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
})
options(digits = 7, width = 120)

# ---- CONFIGURATION -----------------------------------------------------------
SEASON          <- 2025L
PANEL_PATH      <- here::here("data", "season2_cache", "s2_week4_ab_panel.rds")
POSITIONS       <- c("QB", "RB", "WR")
N_ROWS_EXPECTED <- 2922L
MAIN_FLOOR      <- 8L
FLOORS          <- c(4L, 8L, 12L)
ALPHA           <- 0.05
N_SHUFFLE       <- 10000L
SEED            <- 20260930L

# Post 32 verified values (mae_std | floor 8), used as a reproduction gate
P32_N    <- c(QB = 26L, RB = 70L, WR = 106L)
P32_MEAN <- c(QB = 8.272975, RB = 5.645086, WR = 5.157343)
P32_SSB  <- 203.399412
P32_SSW  <- 944.218806
P32_F    <- 21.433847

f6 <- function(x) sprintf("%.6f", x)
banner <- function(s) cat("\n====================", s, "====================\n")

# ---- 1. LOAD PANEL -----------------------------------------------------------
banner("1. PANEL")
if (!file.exists(PANEL_PATH)) stop("Panel not found at: ", PANEL_PATH)
panel_all <- readRDS(PANEL_PATH)
need <- c("season", "week", "player_id", "player_name", "position", "team",
          "fantasy_pts_actual", "fantasy_pts_roll3", "fantasy_pts_std")
miss <- setdiff(need, names(panel_all))
if (length(miss)) stop("Panel missing columns: ", paste(miss, collapse = ", "))

p25 <- panel_all %>% filter(season == SEASON)
cat("Rows in", SEASON, ":", nrow(p25), "(expected", N_ROWS_EXPECTED, ")\n")
stopifnot(nrow(p25) == N_ROWS_EXPECTED)
stopifnot(setequal(unique(p25$position), POSITIONS))
dup <- p25 %>% count(player_id, week) %>% filter(n > 1)
cat("Duplicate player-week rows:", nrow(dup), "\n")
stopifnot(nrow(dup) == 0)

p25 <- p25 %>%
  mutate(err_std   = abs(fantasy_pts_actual - fantasy_pts_std),
         err_roll3 = abs(fantasy_pts_actual - fantasy_pts_roll3))
stopifnot(!anyNA(p25$err_std), !anyNA(p25$err_roll3))

# Per-player table: copied from the Post 33 pull script
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

# ---- 2. REPRODUCE POST 32 (GATE) ---------------------------------------------
banner("2. POST 32 REPRODUCTION GATE")
pl8   <- players %>% filter(n_weeks >= MAIN_FLOOR)
n_by  <- as.integer(table(factor(pl8$position, levels = POSITIONS)))
names(n_by) <- POSITIONS
m_by  <- tapply(pl8$mae_std, factor(pl8$position, levels = POSITIONS), mean)
grand <- mean(pl8$mae_std)
ssb   <- sum(n_by * (m_by - grand)^2)
ssw   <- sum((pl8$mae_std - ave(pl8$mae_std, pl8$position))^2)
f_one <- (ssb / (length(POSITIONS) - 1)) / (ssw / (nrow(pl8) - length(POSITIONS)))
cat("n:", paste(names(n_by), n_by, collapse = " / "), "\n")
cat("means:", paste(names(m_by), f6(m_by), collapse = " / "), "\n")
cat("SSB", f6(ssb), " SSW", f6(ssw), " F", f6(f_one), "\n")
gate_ok <- all(n_by == P32_N) &&
  max(abs(m_by - P32_MEAN)) < 1e-5 &&
  abs(ssb - P32_SSB) < 1e-4 &&
  abs(ssw - P32_SSW) < 1e-4 &&
  abs(f_one - P32_F) < 1e-4
cat("Post 32 reproduced:", gate_ok, "\n")
if (!gate_ok) stop("Post 32 numbers not reproduced. Stop and report; do not continue.")

# ---- 3. PAIRWISE ENGINE ------------------------------------------------------
# Pair orientation matches TukeyHSD(): for levels QB, RB, WR the rows are
# RB-QB, WR-QB, WR-RB, and diff = mean(later level) - mean(earlier level).
pair_table <- function(df, outcome) {
  d <- df %>% mutate(g = factor(position, levels = POSITIONS), y = .data[[outcome]])
  stopifnot(nrow(d) > 0, !anyNA(d$y))
  k  <- nlevels(d$g)
  n  <- as.numeric(table(d$g))
  if (any(n < 2)) stop("A group has fewer than 2 players; variance undefined.")
  m  <- tapply(d$y, d$g, mean)
  v  <- tapply(d$y, d$g, var)
  N  <- nrow(d)
  df_W <- N - k
  msw  <- sum((d$y - ave(d$y, d$g))^2) / df_W
  q_crit <- qtukey(1 - ALPHA, nmeans = k, df = df_W)
  t_crit <- qt(1 - ALPHA / 2, df = df_W)

  idx <- combn(k, 2)
  rows <- lapply(seq_len(ncol(idx)), function(c) {
    i <- idx[1, c]; j <- idx[2, c]
    diff  <- m[[j]] - m[[i]]
    se_u  <- sqrt(msw * (1 / n[i] + 1 / n[j]))           # unadjusted pooled SE
    t_u   <- diff / se_u
    p_u   <- 2 * pt(-abs(t_u), df_W)
    se_tk <- se_u / sqrt(2)                              # Tukey-Kramer scale
    q_tk  <- abs(diff) / se_tk
    p_tk  <- ptukey(q_tk, nmeans = k, df = df_W, lower.tail = FALSE)
    a_i   <- v[[i]] / n[i]; a_j <- v[[j]] / n[j]         # Games-Howell
    se_gh <- sqrt(a_i + a_j)
    df_gh <- (a_i + a_j)^2 / (a_i^2 / (n[i] - 1) + a_j^2 / (n[j] - 1))
    t_gh  <- diff / se_gh
    p_gh  <- ptukey(abs(t_gh) * sqrt(2), nmeans = k, df = df_gh, lower.tail = FALSE)
    data.frame(
      pair        = paste(levels(d$g)[j], levels(d$g)[i], sep = "-"),
      diff        = diff,
      se_unadj    = se_u,
      t_unadj     = t_u,
      p_unadj     = p_u,
      p_bonf      = min(1, p_u * ncol(idx)),
      min_diff_unadj = t_crit * se_u,                    # smallest gap that clears 5%, unadjusted
      q_tukey     = q_tk,
      p_tukey     = p_tk,
      min_diff_tukey = q_crit * se_tk,                   # smallest gap that clears 5%, Tukey
      tukey_lwr   = diff - q_crit * se_tk,
      tukey_upr   = diff + q_crit * se_tk,
      unadj_lwr   = diff - t_crit * se_u,
      unadj_upr   = diff + t_crit * se_u,
      se_gh       = se_gh,
      df_gh       = df_gh,
      p_gh        = p_gh,
      row.names   = NULL
    )
  })
  list(tab = bind_rows(rows), k = k, n = n, m = m, v = v, N = N, df_W = df_W,
       msw = msw, q_crit = q_crit, t_crit = t_crit, d = d)
}

# ---- 4. MAIN: DESCRIPTIVES AND PAIRS ----------------------------------------
banner(paste0("4. MAIN: mae_std | floor ", MAIN_FLOOR, " | Tukey-Kramer"))
main <- pair_table(pl8, "mae_std")
desc <- data.frame(position = POSITIONS, n = main$n,
                   mean = f6(main$m), sd = f6(sqrt(main$v)), var = f6(main$v),
                   mean_actual = f6(tapply(main$d$mean_actual, main$d$g, mean)))
print(desc)
cat("Largest-to-smallest variance ratio:", f6(max(main$v) / min(main$v)), "\n")
cat("N", main$N, " k", main$k, " df_W", main$df_W, " MSW", f6(main$msw), "\n")
cat("Critical values at alpha", ALPHA, ": Tukey q", f6(main$q_crit),
    " q/sqrt(2)", f6(main$q_crit / sqrt(2)), " unadjusted t", f6(main$t_crit),
    " ratio (q/sqrt2)/t", f6(main$q_crit / sqrt(2) / main$t_crit), "\n")
cat("Bonferroni t (alpha/3, two-sided):", f6(qt(1 - ALPHA / 6, main$df_W)), "\n")

tab <- main$tab
cat("\nPairwise table (diff = later level minus earlier level):\n")
print(tab %>% select(pair, diff, se_unadj, t_unadj, p_unadj, p_bonf, q_tukey, p_tukey, p_gh, df_gh),
      digits = 7)
cat("\nSmallest gap that clears alpha, per pair (unadjusted vs Tukey):\n")
print(tab %>% select(pair, diff, min_diff_unadj, min_diff_tukey) %>%
        mutate(width_ratio = min_diff_tukey / min_diff_unadj), digits = 7)
cat("\n95% intervals, unadjusted vs Tukey family-wise:\n")
print(tab %>% select(pair, diff, unadj_lwr, unadj_upr, tukey_lwr, tukey_upr), digits = 7)

cat("\nVerdict at alpha", ALPHA, "(TRUE = pair differs):\n")
print(data.frame(pair = tab$pair,
                 unadjusted = tab$p_unadj < ALPHA,
                 bonferroni = tab$p_bonf < ALPHA,
                 tukey      = tab$p_tukey < ALPHA,
                 games_howell = tab$p_gh < ALPHA))

# ---- 5. CROSS-CHECKS AGAINST BASE R -----------------------------------------
banner("5. CROSS-CHECKS")
fit <- aov(y ~ g, data = main$d)
thsd <- TukeyHSD(fit, conf.level = 1 - ALPHA)$g
print(thsd, digits = 7)
stopifnot(identical(rownames(thsd), tab$pair))
tk_ok <- max(abs(thsd[, "diff"] - tab$diff)) < 1e-8 &&
  max(abs(thsd[, "lwr"] - tab$tukey_lwr)) < 1e-8 &&
  max(abs(thsd[, "upr"] - tab$tukey_upr)) < 1e-8 &&
  max(abs(thsd[, "p adj"] - tab$p_tukey)) < 1e-6
cat("Hand Tukey-Kramer matches TukeyHSD():", tk_ok, "\n")
if (!tk_ok) stop("Tukey-Kramer by hand does not match TukeyHSD(). Stop and report.")

pw_none <- pairwise.t.test(main$d$y, main$d$g, pool.sd = TRUE, p.adjust.method = "none")$p.value
pw_bonf <- pairwise.t.test(main$d$y, main$d$g, pool.sd = TRUE, p.adjust.method = "bonferroni")$p.value
print(pw_none, digits = 7); print(pw_bonf, digits = 7)
# pairwise.t.test lower-triangle layout: rows RB, WR; cols QB, RB
pw_vec <- function(mm) c(mm["RB", "QB"], mm["WR", "QB"], mm["WR", "RB"])
pw_ok <- max(abs(pw_vec(pw_none) - tab$p_unadj)) < 1e-8 &&
  max(abs(pw_vec(pw_bonf) - tab$p_bonf)) < 1e-8
cat("Hand unadjusted and Bonferroni match pairwise.t.test():", pw_ok, "\n")
if (!pw_ok) stop("Pairwise t by hand does not match pairwise.t.test(). Stop and report.")

f_check <- summary(fit)[[1]][["F value"]][1]
cat("aov() F:", f6(f_check), " (Post 32 F", f6(P32_F), ")\n")

# ---- 6. ROBUSTNESS -----------------------------------------------------------
banner("6. ROBUSTNESS: Tukey and Games-Howell verdicts by pair")
rob_row <- function(label, df, outcome) {
  out <- tryCatch(pair_table(df, outcome),
                  error = function(e) { cat(label, "-> not computable:", conditionMessage(e), "\n"); NULL })
  if (is.null(out)) return(NULL)
  out$tab %>%
    mutate(check = label, N = out$N,
           n = paste(out$n, collapse = "/"),
           pair, diff = f6(diff),
           p_unadj = signif(p_unadj, 6), p_bonf = signif(p_bonf, 6),
           p_tukey = signif(p_tukey, 6), p_gh = signif(p_gh, 6),
           var_ratio = f6(max(out$v) / min(out$v)),
           .keep = "none")
}
rows <- list(rob_row(paste("MAIN floor", MAIN_FLOOR), pl8, "mae_std"))
for (fl in setdiff(FLOORS, MAIN_FLOOR)) {
  rows[[length(rows) + 1]] <- rob_row(paste("floor", fl),
                                      players %>% filter(n_weeks >= fl), "mae_std")
}
rows[[length(rows) + 1]] <- rob_row("rolling 3-game forecast (mae_roll3)", pl8, "mae_roll3")
rel <- pl8 %>% mutate(rel_miss = if_else(mean_actual > 0, mae_std / mean_actual, NA_real_))
cat("Players dropped from relative-miss check (average points <= 0):", sum(is.na(rel$rel_miss)), "\n")
rows[[length(rows) + 1]] <- rob_row("relative miss (mae_std / mean_actual)",
                                    rel %>% filter(!is.na(rel_miss)), "rel_miss")
print(bind_rows(rows), row.names = FALSE)

# ---- 7. ILLUSTRATION: LABEL SHUFFLE ON THE REAL PLAYERS ----------------------
# Position labels are shuffled across the same 202 players (group sizes held
# at 26/70/106), so every real difference between positions is erased. Any
# pair that clears the cutoff after a shuffle is a false alarm. Counts how
# often at least one of the three pairs clears, unadjusted vs Tukey.
banner(paste0("7. LABEL SHUFFLE (illustration, ", N_SHUFFLE, " shuffles, seed ", SEED, ")"))
set.seed(SEED)
y    <- main$d$y
lab0 <- as.integer(main$d$g)
k    <- main$k
n_i  <- main$n
pairs_ij <- combn(k, 2)
se_mult  <- sqrt(1 / n_i[pairs_ij[1, ]] + 1 / n_i[pairs_ij[2, ]])
any_unadj <- any_tukey <- widest_unadj <- logical(N_SHUFFLE)
for (s in seq_len(N_SHUFFLE)) {
  lab <- sample(lab0)
  mm  <- vapply(seq_len(k), function(g) mean(y[lab == g]), numeric(1))
  ss  <- sum((y - mm[lab])^2)
  msw_s <- ss / main$df_W
  dd  <- abs(mm[pairs_ij[2, ]] - mm[pairs_ij[1, ]])
  se_u <- sqrt(msw_s) * se_mult
  t_s <- dd / se_u
  any_unadj[s] <- any(t_s > main$t_crit)
  any_tukey[s] <- any(t_s * sqrt(2) > main$q_crit)
  widest_unadj[s] <- t_s[which.max(dd)] > main$t_crit
}
cat("Share of shuffles where at least one pair clears the UNADJUSTED 5% cutoff:",
    f6(mean(any_unadj)), "\n")
cat("Share where the WIDEST pair (picked after looking) clears the unadjusted cutoff:",
    f6(mean(widest_unadj)), "\n")
cat("Share where at least one pair clears the TUKEY cutoff:", f6(mean(any_tukey)), "\n")
cat("Monte Carlo SE of a share near 0.05 at this many shuffles:",
    f6(sqrt(0.05 * 0.95 / N_SHUFFLE)), "\n")

cat("\nDone. Copy the full console output back verbatim.\n")
