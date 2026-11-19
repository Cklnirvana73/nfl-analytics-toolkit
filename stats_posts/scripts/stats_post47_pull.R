################################################################################
# stats_post47_pull.R
# Statistics Series Post 47, Concept 46: Regression assumptions (LINE)
#
# Read-only. Reads stats_posts/output/data/stats_post44v3_wr.csv (written by
# stats_post44_pull_v3.R) and writes three stats_post47_*.csv files.
# Base R. Loads play-by-play through the toolkit's R/01 loader ONLY if the
# CSV has no team column (needed for the independence check).
#
# Anchor: 2025 regular season, wide receivers with 20+ targets (n = 133).
#   y = season total fantasy points (flat full PPR), x = targets.
# Unit treated as independent: one receiver (no receiver repeats). Teammates
# share a quarterback and an offense, so Section 2 tests independence by team
# instead of declaring it by design.
#
# Sections, in the statistics-expert C46 order of importance:
#   0 reproduce the fit   1 linearity   2 independence   3 equal variance
#   4 normality           5 influence   6 write CSVs
# Every gate prints its value first and compares at the precision the value
# was printed in earlier sessions. Exact identities are checked only between
# quantities computed in this run.
################################################################################

TOOLKIT_ROOT <- here::here()
stopifnot(dir.exists(TOOLKIT_ROOT))
setwd(TOOLKIT_ROOT)

SEASON   <- 2025L
DATA_DIR <- file.path("stats_posts", "output", "data")
F_WR     <- file.path(DATA_DIR, "stats_post44v3_wr.csv")
R01      <- file.path("R", "01_data_loading.R")
SEED     <- 47L
N_PERM   <- 5000L

# Reproduction targets, at the precision they were printed in Posts 44 to 46
N_WR      <- 133L
X_MIN     <- 20
X_MAX     <- 185
SLOPE_TXT <- "1.87"      # sprintf("%.2f")
INT_TXT   <- "-9.39"     # sprintf("%.2f")
SSE_TXT   <- "58,362"    # rounded, big.mark
QUAD_TXT  <- "0.063"     # sprintf("%.3f")
BP_P2     <- 2.4e-6      # signif(, 2); avoids platform exponent formatting
SW_TXT    <- "0.005"     # sprintf("%.3f")
SE_DEF_TXT <- "0.048"    # sprintf("%.3f")
SE_HC1_TXT <- "0.069"    # sprintf("%.3f")
COOK_TXT  <- "0.40"      # sprintf("%.2f")
TEAM_CANDIDATES <- c("team", "recent_team", "posteam")

gate <- function(ok, msg) {
  if (!isTRUE(ok)) stop("GATE FAILED: ", msg, call. = FALSE)
  cat("  [PASS] ", msg, "\n", sep = "")
}
show <- function(label, value, digits = 7) {
  cat(sprintf("  %-56s %s\n", label, format(signif(value, digits), scientific = FALSE)))
}
showe <- function(label, value) cat(sprintf("  %-56s %.4e\n", label, value))

# ---- 0. LOAD AND REPRODUCE ---------------------------------------------------

cat("\n================ SECTION 0: LOAD AND REPRODUCE ================\n")
gate(file.exists(F_WR), "stats_post44v3_wr.csv exists (run stats_post44_pull_v3.R first)")
wr <- utils::read.csv(F_WR, stringsAsFactors = FALSE)
cat("  Columns:", paste(names(wr), collapse = ", "), "\n")
gate(all(c("player_id", "player_name", "targets", "total_points") %in% names(wr)),
     "CSV has player_id, player_name, targets, total_points")
gate(nrow(wr) == N_WR, sprintf("%d receivers", N_WR))
gate(!anyDuplicated(wr$player_id), "one row per player_id")
gate(!anyNA(wr$targets) && !anyNA(wr$total_points), "no missing targets or points")
gate(min(wr$targets) == X_MIN && max(wr$targets) == X_MAX,
     sprintf("targets run %d to %d", X_MIN, X_MAX))

wr <- wr[order(wr$targets, wr$player_id), ]
x <- wr$targets
y <- wr$total_points
n <- length(y)
k <- 2L

m  <- stats::lm(y ~ x)
a  <- unname(coef(m)[1])
b  <- unname(coef(m)[2])
e  <- unname(residuals(m))
yh <- unname(fitted(m))
X  <- cbind(1, x)
XtX_inv <- solve(crossprod(X))
h  <- rowSums((X %*% XtX_inv) * X)          # leverage
s  <- sqrt(sum(e^2) / (n - k))
show("slope", b)
show("intercept", a)
show("residual SD s (sqrt(SSE / (n - 2)))", s)
show("SSE", sum(e^2))
gate(sprintf("%.2f", b) == SLOPE_TXT, "slope reads 1.87")
gate(sprintf("%.2f", a) == INT_TXT, "intercept reads -9.39")
gate(format(round(sum(e^2)), big.mark = ",") == SSE_TXT, "SSE reads 58,362")
gate(abs(sum(h) - k) < 1e-8, "leverages sum to 2 (same-run identity)")
gate(max(abs(h - stats::hatvalues(m))) < 1e-10, "leverage matches hatvalues() (same-run identity)")

# Fifths of the target range, by rank (ties at a boundary can split; ranges printed)
fifth <- ceiling(seq_len(n) * 5 / n)
wr$fifth <- fifth
fifth_rng <- tapply(x, fifth, function(v) sprintf("%d to %d", min(v), max(v)))
fifth_n   <- as.vector(table(fifth))
cat("  Fifths (by target rank):\n")
for (f in 1:5) cat(sprintf("    fifth %d: n = %d, targets %s\n", f, fifth_n[f], fifth_rng[f]))
gate(sum(fifth_n) == n, "fifths cover every receiver")

# ---- 1. LINEARITY ------------------------------------------------------------
# The mean of points at each target level lies on the line.

cat("\n================ SECTION 1: LINEARITY ================\n")
res_mean_f <- tapply(e, fifth, mean)
for (f in 1:5) show(sprintf("mean residual, fifth %d", f), res_mean_f[f])
show("largest |mean residual| across fifths", max(abs(res_mean_f)))

mq <- stats::lm(y ~ x + I(x^2))
quad_p <- summary(mq)$coefficients[3, 4]
show("quadratic term p", quad_p)
gate(sprintf("%.3f", quad_p) == QUAD_TXT, "quadratic p reads 0.063")
# Size of the curve the quadratic would add, in points, across the range
grid_x <- seq(X_MIN, X_MAX, by = 1)
gap <- (coef(mq)[1] + coef(mq)[2] * grid_x + coef(mq)[3] * grid_x^2) - (a + b * grid_x)
for (t in c(25, 50, 100, 150, 185)) {
  show(sprintf("quadratic minus line at %d targets (points)", t), gap[grid_x == t])
}
show("largest |quadratic minus line| over 20 to 185 targets", max(abs(gap)))

# ---- 2. INDEPENDENCE ---------------------------------------------------------
# Knowing one receiver's miss tells you nothing about another's. The rows are
# distinct receivers, but teammates share a quarterback and an offense.

cat("\n================ SECTION 2: INDEPENDENCE ================\n")
t_col <- TEAM_CANDIDATES[TEAM_CANDIDATES %in% names(wr)][1]
if (!is.na(t_col)) {
  cat("  Team column found in CSV:", t_col, "\n")
  team <- wr[[t_col]]
} else {
  cat("  No team column in CSV. Deriving each receiver's main team from play-by-play.\n")
  gate(file.exists(R01), "R/01_data_loading.R exists")
  source(R01)
  pbp <- load_and_validate_pbp(seasons = SEASON)
  cat("  pbp rows:", nrow(pbp), "\n")
  gate(all(c("season_type", "play_type", "receiver_player_id", "posteam") %in% names(pbp)),
       "pbp has season_type, play_type, receiver_player_id, posteam")
  tg <- pbp[pbp$season == SEASON & pbp$season_type == "REG" & pbp$play_type == "pass" &
              !is.na(pbp$receiver_player_id), c("receiver_player_id", "posteam")]
  cnt <- stats::aggregate(list(n = rep(1L, nrow(tg))),
                          by = list(player_id = tg$receiver_player_id, team = tg$posteam), FUN = sum)
  cnt <- cnt[order(cnt$player_id, -cnt$n), ]
  main <- cnt[!duplicated(cnt$player_id), ]          # team with the most targets
  n_multi <- sum(table(cnt$player_id[cnt$player_id %in% wr$player_id]) > 1)
  show("receivers in sample targeted for 2+ teams", n_multi)
  team <- main$team[match(wr$player_id, main$player_id)]
  rm(pbp, tg); invisible(gc())
}
gate(!anyNA(team), "every receiver has a team")
wr$team <- team
tab_team <- table(team)
show("teams represented", length(tab_team))
cat("  Receivers per team:", paste(names(table(tab_team)), "->", as.vector(table(tab_team)),
                                   "teams", collapse = "; "), "\n")

# Teammate residual correlation: every ordered pair of teammates, both directions
pair_cor <- function(res, grp) {
  i_all <- integer(0); j_all <- integer(0)
  for (g in unique(grp)) {
    idx <- which(grp == g)
    if (length(idx) < 2L) next
    pr <- expand.grid(i = idx, j = idx)
    pr <- pr[pr$i != pr$j, ]
    i_all <- c(i_all, pr$i); j_all <- c(j_all, pr$j)
  }
  list(r = stats::cor(res[i_all], res[j_all]), n_pairs = length(i_all) / 2)
}
pc <- pair_cor(e, team)
show("teammate pairs", pc$n_pairs)
show("teammate residual correlation", pc$r)

# Permutation null: shuffle team labels, keep team sizes
set.seed(SEED)
perm_r <- vapply(seq_len(N_PERM), function(i) pair_cor(e, sample(team))$r, numeric(1))
perm_p <- (1 + sum(abs(perm_r) >= abs(pc$r))) / (1 + N_PERM)
show("permutation p (two-sided, team labels shuffled)", perm_p)
show("permutation null: 2.5th percentile", stats::quantile(perm_r, 0.025))
show("permutation null: 97.5th percentile", stats::quantile(perm_r, 0.975))

# Same-team-QB link, plainly: mean residual by team, its spread vs shuffled
team_mean <- tapply(e, team, mean)
show("SD of team mean residuals", stats::sd(team_mean))

# ---- 3. EQUAL VARIANCE -------------------------------------------------------
# The spread of points about the line is the same at every target level.

cat("\n================ SECTION 3: EQUAL VARIANCE ================\n")
sd_f <- tapply(e, fifth, stats::sd)
for (f in 1:5) show(sprintf("residual SD, fifth %d (%s targets)", f, fifth_rng[f]), sd_f[f])
show("residual SD ratio, top fifth / bottom fifth", sd_f[5] / sd_f[1])

aux  <- stats::lm(I(e^2) ~ x)
bp   <- n * summary(aux)$r.squared            # Koenker studentized Breusch-Pagan
bp_p <- stats::pchisq(bp, df = 1, lower.tail = FALSE)
show("Breusch-Pagan statistic (Koenker)", bp)
showe("Breusch-Pagan p", bp_p)
gate(abs(signif(bp_p, 2) - BP_P2) < 1e-12, "Breusch-Pagan p reads 2.4e-6 (2 significant figures)")

# Slope standard errors under each assumption set
sandwich_se <- function(w) {        # w: per-row weight on e^2
  meat <- crossprod(X * (e * sqrt(w)))
  sqrt(diag(XtX_inv %*% meat %*% XtX_inv))[2]
}
cluster_se <- function(grp) {
  G <- length(unique(grp))
  sc <- rowsum(X * e, grp)          # per-cluster score sums
  V <- XtX_inv %*% crossprod(sc) %*% XtX_inv * (G / (G - 1)) * ((n - 1) / (n - k))
  sqrt(diag(V))[2]
}
se_def <- summary(m)$coefficients[2, 2]
se_hc0 <- sandwich_se(rep(1, n))
se_hc1 <- se_hc0 * sqrt(n / (n - k))
se_hc3 <- sandwich_se(1 / (1 - h)^2)
se_cl  <- cluster_se(team)
gate(abs(cluster_se(seq_len(n)) - se_hc1) < 1e-10,
     "cluster SE with one receiver per cluster equals HC1 (same-run identity)")
show("slope SE, classical (assumes equal spread)", se_def)
show("slope SE, HC0", se_hc0)
show("slope SE, HC1", se_hc1)
show("slope SE, HC3 (sandwich::vcovHC default)", se_hc3)
show("slope SE, clustered by team (CR1)", se_cl)
gate(sprintf("%.3f", se_def) == SE_DEF_TXT, "classical SE reads 0.048")
gate(sprintf("%.3f", se_hc1) == SE_HC1_TXT, "HC1 SE reads 0.069")
show("HC3 / classical", se_hc3 / se_def)
tq <- stats::qt(0.975, n - k)
ci <- function(se) sprintf("%.3f to %.3f", b - tq * se, b + tq * se)
cat("  95% slope interval, classical:", ci(se_def), "\n")
cat("  95% slope interval, HC3      :", ci(se_hc3), "\n")
cat("  95% slope interval, clustered:", ci(se_cl), "\n")

# Classical 95% prediction interval: same formula width everywhere except leverage.
# In-sample count of receivers outside it, by fifth (about 5% each if spread were equal).
pi_half <- tq * s * sqrt(1 + h)
wr$fitted <- yh; wr$resid <- e
wr$pi_lo <- yh - pi_half; wr$pi_hi <- yh + pi_half
wr$outside_pi <- abs(e) > pi_half
out_f <- tapply(wr$outside_pi, fifth, sum)
for (f in 1:5) {
  cat(sprintf("  outside classical 95%% PI, fifth %d: %d of %d (%.1f%%), PI half-width %.1f to %.1f\n",
              f, out_f[f], fifth_n[f], 100 * out_f[f] / fifth_n[f],
              min(pi_half[fifth == f]), max(pi_half[fifth == f])))
}
show("outside classical 95% PI, all receivers", sum(wr$outside_pi))
gate(sum(out_f) == sum(wr$outside_pi), "fifth counts sum to the total (same-run identity)")

# ---- 4. NORMALITY ------------------------------------------------------------
# Least consequential for the slope at n = 133; reported, not used as a gate.

cat("\n================ SECTION 4: NORMALITY ================\n")
r_std <- e / (s * sqrt(1 - h))
gate(max(abs(r_std - stats::rstandard(m))) < 1e-10, "standardized residuals match rstandard() (same-run identity)")
sw <- stats::shapiro.test(e)
show("Shapiro-Wilk p (raw residuals)", sw$p.value)
gate(sprintf("%.3f", sw$p.value) == SW_TXT, "Shapiro-Wilk p reads 0.005")
skew <- mean((e - mean(e))^3) / mean((e - mean(e))^2)^1.5
show("residual skewness", skew)
show("standardized residuals beyond +/-2", sum(abs(r_std) > 2))
show("standardized residuals beyond +/-3", sum(abs(r_std) > 3))
show("expected beyond +/-2 under normal errors", n * 2 * stats::pnorm(-2))
qq_r <- stats::cor(sort(r_std), stats::qnorm(stats::ppoints(n)))
show("Q-Q correlation (sorted std residuals vs normal quantiles)", qq_r)

# ---- 5. INFLUENCE (C47 territory, reference only) ----------------------------

cat("\n================ SECTION 5: INFLUENCE ================\n")
cook <- stats::cooks.distance(m)
top_i <- which.max(cook)
show("max Cook's distance", cook[top_i])
cat("  receiver:", wr$player_name[top_i], "| targets", x[top_i], "| points", round(y[top_i], 1), "\n")
gate(sprintf("%.2f", cook[top_i]) == COOK_TXT, "max Cook's D reads 0.40")
m_drop <- stats::lm(y[-top_i] ~ x[-top_i])
show("slope without that receiver", coef(m_drop)[2])

# ---- 6. WRITE ----------------------------------------------------------------

cat("\n================ SECTION 6: WRITE ================\n")
wr$r_std <- r_std; wr$leverage <- h
receivers <- wr[, c("player_id", "player_name", "team", "targets", "total_points", "fifth",
                    "fitted", "resid", "r_std", "leverage", "pi_lo", "pi_hi", "outside_pi")]
fifths <- data.frame(fifth = 1:5, n = fifth_n, targets = as.vector(fifth_rng),
                     mean_resid = as.vector(res_mean_f), sd_resid = as.vector(sd_f),
                     outside_pi = as.vector(out_f))
summary_tbl <- data.frame(
  n = n, slope = b, intercept = a, s = s, sse = sum(e^2),
  quad_p = quad_p, max_abs_quad_gap = max(abs(gap)),
  n_teams = length(tab_team), teammate_pairs = pc$n_pairs, teammate_resid_cor = pc$r,
  perm_p = perm_p, bp_stat = bp, bp_p = bp_p,
  se_classical = se_def, se_hc0 = se_hc0, se_hc1 = se_hc1, se_hc3 = se_hc3, se_cluster = se_cl,
  sw_p = sw$p.value, resid_skew = skew, outside_pi_total = sum(wr$outside_pi),
  max_cook = unname(cook[top_i]), seed = SEED, n_perm = N_PERM
)
utils::write.csv(receivers, file.path(DATA_DIR, "stats_post47_receivers.csv"), row.names = FALSE)
utils::write.csv(fifths, file.path(DATA_DIR, "stats_post47_fifths.csv"), row.names = FALSE)
utils::write.csv(summary_tbl, file.path(DATA_DIR, "stats_post47_summary.csv"), row.names = FALSE)
cat("  Wrote stats_post47_receivers.csv, stats_post47_fifths.csv, stats_post47_summary.csv to",
    DATA_DIR, "\n")
cat("\nALL GATES PASSED\n")
