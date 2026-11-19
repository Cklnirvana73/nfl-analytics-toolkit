################################################################################
# stats_post46_pull.R
# Statistics Series Post 46, Concept 45: Multiple linear regression
#
# Read-only. Reads stats_posts/output/data/stats_post44v3_wr.csv (written by
# stats_post44_pull_v3.R) and writes five stats_post46_*.csv files.
# Run from the nfl-analytics-toolkit root. Base R, plus a one-time check
# that the toolkit's run_regression_analysis() (R/06) returns the same fit.
#
# Anchor: 2025 regular season, wide receivers with 20+ targets (n = 133).
#   y  = season total fantasy points (flat full PPR)
#   x1 = targets
#   x2 = games played
# Unit treated as independent: one receiver. No receiver repeats.
################################################################################

DATA_DIR <- file.path("stats_posts", "output", "data")
F_WR     <- file.path(DATA_DIR, "stats_post44v3_wr.csv")
R06      <- file.path("R", "06_predictive_validation.R")

# Values carried from the Post 44 and Post 45 pulls (reproduction targets)
N_WR       <- 133L
X_MIN      <- 20
X_MAX      <- 185
SLOPE_44   <- 1.8667
INT_44     <- -9.39
R_44       <- 0.959
SSE_45     <- 58362
R_GT_44    <- 0.632      # games played vs targets
GAMES_CANDIDATES <- c("fp_games", "games_played", "games")

gate <- function(ok, msg) {
  if (!isTRUE(ok)) stop("GATE FAILED: ", msg, call. = FALSE)
  cat("  [PASS] ", msg, "\n", sep = "")
}
show <- function(label, value, digits = 7) {
  cat(sprintf("  %-52s %s\n", label,
              format(signif(value, digits), scientific = FALSE)))
}

# ---- 0. LOAD AND REPRODUCE POST 44 -------------------------------------------

cat("\n================ SECTION 0: LOAD AND REPRODUCE ================\n")
gate(file.exists(F_WR), "stats_post44v3_wr.csv exists (run stats_post44_pull_v3.R first)")
wr <- utils::read.csv(F_WR, stringsAsFactors = FALSE)
cat("  Columns:", paste(names(wr), collapse = ", "), "\n")
gate(all(c("player_name", "targets", "total_points") %in% names(wr)),
     "CSV has player_name, targets, total_points")

g_col <- GAMES_CANDIDATES[GAMES_CANDIDATES %in% names(wr)][1]
if (is.na(g_col)) {
  stop("No games column found. Tried: ", paste(GAMES_CANDIDATES, collapse = ", "),
       ". Columns present: ", paste(names(wr), collapse = ", "), call. = FALSE)
}
cat("  Games column used:", g_col, "\n")
wr$games <- wr[[g_col]]

id_col <- if ("player_id" %in% names(wr)) "player_id" else "player_name"
cat("  Receiver key used for the independence check:", id_col, "\n")

show("rows", nrow(wr))
gate(nrow(wr) == N_WR, sprintf("%d receivers", N_WR))
gate(!anyNA(wr$targets) && !anyNA(wr$total_points) && !anyNA(wr$games),
     "no missing targets, points, or games")
show("min targets", min(wr$targets)); show("max targets", max(wr$targets))
gate(min(wr$targets) == X_MIN && max(wr$targets) == X_MAX,
     sprintf("targets run %d to %d", X_MIN, X_MAX))

cat("\n  Games played distribution (read before trusting it as a predictor):\n")
print(table(wr$games))
show("min games", min(wr$games)); show("median games", stats::median(wr$games))
show("max games", max(wr$games))
# fp_games = distinct games with at least one fantasy row (a pass, carry, or
# target) in calculate_fantasy_points(). 18 is possible for a receiver traded
# between teams whose byes fell in different weeks.
cat("  Receivers above 17 games:\n")
print(wr[wr$games > 17, c("player_name", "targets", "games", "total_points")], row.names = FALSE)
gate(all(wr$games >= 1 & wr$games <= 18 & wr$games == round(wr$games)),
     "games are whole numbers from 1 to 18")
gate(sum(wr$games > 17) == 1L, "exactly one receiver above 17 games (traded midseason)")

m1 <- stats::lm(total_points ~ targets, data = wr)
b1 <- unname(coef(m1)["targets"]); a1 <- unname(coef(m1)["(Intercept)"])
r_ty <- stats::cor(wr$targets, wr$total_points)
sse1 <- sum(resid(m1)^2)
show("targets-only slope", b1); show("targets-only intercept", a1)
show("r(targets, points)", r_ty); show("targets-only squared total", sse1)
gate(abs(b1 - SLOPE_44) < 5e-4, "slope matches Post 44 (1.8667)")
gate(abs(a1 - INT_44) < 5e-3, "intercept matches Post 44 (-9.39)")
gate(abs(r_ty - R_44) < 5e-4, "r matches Post 44 (0.959)")
gate(abs(sse1 - SSE_45) < 0.5, "squared total matches Post 45 (58,362)")

r_tg <- stats::cor(wr$targets, wr$games)
r_gy <- stats::cor(wr$games, wr$total_points)
show("r(games, targets)", r_tg); show("r(games, points)", r_gy)
gate(abs(r_tg - R_GT_44) < 5e-4, "games vs targets r matches Post 44 (0.632)")

# ---- 1. THREE FITS -----------------------------------------------------------

cat("\n================ SECTION 1: THREE FITS ================\n")
m2 <- stats::lm(total_points ~ games, data = wr)
m3 <- stats::lm(total_points ~ targets + games, data = wr)
b2 <- unname(coef(m2)["games"]); a2 <- unname(coef(m2)["(Intercept)"])
c3 <- coef(m3)
show("games-only slope (points per game played)", b2)
show("games-only intercept", a2)
show("two-predictor intercept", c3["(Intercept)"])
show("two-predictor targets coefficient", c3["targets"])
show("two-predictor games coefficient", c3["games"])
show("targets coefficient change (two-pred minus alone)", c3["targets"] - b1)
show("games coefficient change (two-pred minus alone)", c3["games"] - b2)

# ---- 2. SAME FIT THREE WAYS --------------------------------------------------

cat("\n================ SECTION 2: SAME FIT THREE WAYS ================\n")
X <- cbind(1, wr$targets, wr$games)
y <- wr$total_points
beta_mat <- solve(crossprod(X), crossprod(X, y))
show("matrix (X'X)^-1 X'y intercept", beta_mat[1])
show("matrix targets", beta_mat[2]); show("matrix games", beta_mat[3])
gate(max(abs(beta_mat - unname(c3))) < 1e-8, "matrix solution equals lm()")

if (file.exists(R06) && requireNamespace("dplyr", quietly = TRUE) &&
    requireNamespace("glue", quietly = TRUE)) {
  suppressPackageStartupMessages({ library(dplyr); library(glue) })
  ok_src <- tryCatch({ source(R06); TRUE },
                     error = function(err) { cat("  [SKIPPED] source(R06) failed:",
                                                 conditionMessage(err), "\n"); FALSE })
  gate(ok_src && exists("run_regression_analysis"), "R/06 sourced, run_regression_analysis() found")
  key <- if ("player_id" %in% names(wr)) wr$player_id else wr$player_name
  tr  <- data.frame(player_id = key, targets = wr$targets, games = wr$games)
  te  <- data.frame(player_id = key, total_points = wr$total_points)
  tk  <- run_regression_analysis(train_stats = tr, test_stats = te,
                                 predictors = c("targets", "games"),
                                 outcome = "total_points")
  est <- tk$coefficients$estimate
  names(est) <- tk$coefficients$term
  show("run_regression_analysis() targets", est["targets"])
  show("run_regression_analysis() games", est["games"])
  gate(abs(est["targets"] - c3["targets"]) < 1e-8 &&
       abs(est["games"] - c3["games"]) < 1e-8 &&
       abs(est["(Intercept)"] - c3["(Intercept)"]) < 1e-8,
       "toolkit run_regression_analysis() equals lm()")
} else {
  cat("  [SKIPPED] R/06 or dplyr/glue not available; toolkit check not run\n")
}

# ---- 3. WHAT "HOLDING THE OTHER FIXED" MEANS ---------------------------------

cat("\n================ SECTION 3: HOLDING THE OTHER FIXED ================\n")
# 3a. Partial out games from targets, then regress points on what is left.
t_left <- resid(stats::lm(targets ~ games, data = wr))
g_left <- resid(stats::lm(games ~ targets, data = wr))
b_t_left <- unname(coef(stats::lm(wr$total_points ~ t_left))[2])
b_g_left <- unname(coef(stats::lm(wr$total_points ~ g_left))[2])
show("slope of points on targets-with-games-removed", b_t_left)
show("slope of points on games-with-targets-removed", b_g_left)
gate(abs(b_t_left - c3["targets"]) < 1e-8, "partialled targets slope equals two-predictor coefficient")
gate(abs(b_g_left - c3["games"]) < 1e-8, "partialled games slope equals two-predictor coefficient")
show("SD of targets", stats::sd(wr$targets))
show("SD of targets left after removing games", stats::sd(t_left))
show("SD of games", stats::sd(wr$games))
show("SD of games left after removing targets", stats::sd(g_left))

# 3b. Model's prediction differences (one step on each predictor)
pred <- function(t, g) unname(c3["(Intercept)"] + c3["targets"] * t + c3["games"] * g)
show("prediction at 100 targets, 16 games", pred(100, 16))
show("prediction at 101 targets, 16 games", pred(101, 16))
show("prediction at 100 targets, 17 games", pred(100, 17))
gate(abs((pred(101, 16) - pred(100, 16)) - c3["targets"]) < 1e-8,
     "one more target, same games, moves prediction by the targets coefficient")
gate(abs((pred(100, 17) - pred(100, 16)) - c3["games"]) < 1e-8,
     "one more game, same targets, moves prediction by the games coefficient")

# 3c. Targets slope inside groups that played the same number of games
cat("\n  Targets slope within each games-played count (n >= 10 only):\n")
within <- do.call(rbind, lapply(sort(unique(wr$games)), function(g) {
  d <- wr[wr$games == g, ]
  if (nrow(d) < 10) return(NULL)
  data.frame(games = g, n = nrow(d),
             targets_min = min(d$targets), targets_max = max(d$targets),
             slope = unname(coef(stats::lm(total_points ~ targets, data = d))[2]))
}))
if (is.null(within)) {
  cat("  No games count has 10+ receivers\n")
  within <- data.frame(games = integer(), n = integer(), targets_min = numeric(),
                       targets_max = numeric(), slope = numeric())
} else {
  print(within, row.names = FALSE)
  show("receivers covered by these groups", sum(within$n))
}

# 3d. Real pairs: close targets, games differ by 4+
cat("\n  Receiver pairs within 5 targets of each other, games differing by 4+:\n")
pairs <- list()
for (i in seq_len(nrow(wr) - 1)) for (j in (i + 1):nrow(wr)) {
  if (abs(wr$targets[i] - wr$targets[j]) <= 5 && abs(wr$games[i] - wr$games[j]) >= 4) {
    pairs[[length(pairs) + 1]] <- data.frame(
      a = wr$player_name[i], a_tgt = wr$targets[i], a_g = wr$games[i],
      a_pts = round(wr$total_points[i], 1),
      b = wr$player_name[j], b_tgt = wr$targets[j], b_g = wr$games[j],
      b_pts = round(wr$total_points[j], 1))
  }
}
pairs_df <- if (length(pairs)) do.call(rbind, pairs) else data.frame()
show("pairs found", nrow(pairs_df))
if (nrow(pairs_df)) print(utils::head(pairs_df[order(-pmax(pairs_df$a_tgt, pairs_df$b_tgt)), ], 15),
                          row.names = FALSE)

# ---- 4. SQUARED TOTALS -------------------------------------------------------

cat("\n================ SECTION 4: SQUARED TOTALS ================\n")
sse2 <- sum(resid(m2)^2); sse3 <- sum(resid(m3)^2)
show("squared total, targets only", sse1)
show("squared total, games only", sse2)
show("squared total, targets + games", sse3)
show("drop from adding games to targets", sse1 - sse3)
show("drop as share of targets-only total", (sse1 - sse3) / sse1)
gate(sse3 <= sse1 + 1e-8 && sse3 <= sse2 + 1e-8,
     "two-predictor squared total is no larger than either one-predictor total")
show("residuals sum (two-predictor)", sum(resid(m3)))
show("residuals x targets sum", sum(resid(m3) * wr$targets))
show("residuals x games sum", sum(resid(m3) * wr$games))
cat("  RESERVED (Concept 48/49, do not spend): R-squared alone / two-pred:",
    round(summary(m1)$r.squared, 4), "/", round(summary(m3)$r.squared, 4), "\n")

# ---- 5. STANDARD ERRORS ------------------------------------------------------

cat("\n================ SECTION 5: STANDARD ERRORS ================\n")
n <- nrow(X); k <- ncol(X)
e <- resid(m3)
XtXi <- solve(crossprod(X))
V_def <- XtXi * sum(e^2) / (n - k)
meat  <- crossprod(X * e)
V_hc1 <- XtXi %*% meat %*% XtXi * n / (n - k)
se_def <- sqrt(diag(V_def)); se_hc1 <- sqrt(diag(V_hc1))
gate(max(abs(se_def - summary(m3)$coefficients[, "Std. Error"])) < 1e-8,
     "manual default SE equals lm()")
se_tab <- data.frame(term = c("(Intercept)", "targets", "games"),
                     estimate = unname(c3), se_default = se_def, se_hc1 = se_hc1,
                     t_hc1 = unname(c3) / se_hc1,
                     p_hc1 = 2 * stats::pt(-abs(unname(c3) / se_hc1), df = n - k))
print(se_tab, row.names = FALSE, digits = 6)

# ---- 6. ASSUMPTION CHECKLIST (detailed-patterns.md) --------------------------

cat("\n================ SECTION 6: ASSUMPTIONS ================\n")
wr$fitted <- fitted(m3); wr$resid <- e

# 6a. Linearity
grp <- cut(wr$fitted, breaks = stats::quantile(wr$fitted, 0:5 / 5), include.lowest = TRUE)
lin <- data.frame(n = as.vector(table(grp)),
                  mean_resid = as.vector(tapply(wr$resid, grp, mean)),
                  sd_resid = as.vector(tapply(wr$resid, grp, stats::sd)))
cat("  Residuals by fitted-value fifth:\n"); print(lin, row.names = FALSE, digits = 4)
q_t <- stats::lm(total_points ~ targets + games + I(targets^2), data = wr)
q_g <- stats::lm(total_points ~ targets + games + I(games^2), data = wr)
p_qt <- summary(q_t)$coefficients["I(targets^2)", "Pr(>|t|)"]
p_qg <- summary(q_g)$coefficients["I(games^2)", "Pr(>|t|)"]
show("squared-targets term p", p_qt); show("squared-games term p", p_qg)

# 6b. Independence
dup <- anyDuplicated(wr[[id_col]])
show("duplicated receivers", dup)
gate(dup == 0, "one row per receiver")

# 6c. Constant spread (Breusch-Pagan, Koenker form)
aux <- stats::lm(I(e^2) ~ targets + games, data = wr)
bp  <- n * summary(aux)$r.squared
p_bp <- stats::pchisq(bp, df = 2, lower.tail = FALSE)
show("Breusch-Pagan statistic", bp); show("Breusch-Pagan p", p_bp)

# 6d. Normality
sw <- stats::shapiro.test(e)
show("Shapiro-Wilk p", sw$p.value)

# 6e. Overlap between predictors
vif <- 1 / (1 - r_tg^2)
show("VIF (both predictors)", vif)

# 6f. Influence
cd <- stats::cooks.distance(m3)
imax <- which.max(cd)
show("max Cook's D", cd[imax]); cat("  max Cook's D receiver:", wr$player_name[imax], "\n")
cat("  Five largest Cook's D:\n")
top_cd <- order(-cd)[1:5]
print(data.frame(player = wr$player_name[top_cd], targets = wr$targets[top_cd],
                 games = wr$games[top_cd], points = round(wr$total_points[top_cd], 1),
                 cooks_d = round(cd[top_cd], 4)), row.names = FALSE)
m3_drop <- stats::lm(total_points ~ targets + games, data = wr[-imax, ])
show("targets coefficient without that receiver", coef(m3_drop)["targets"])
show("games coefficient without that receiver", coef(m3_drop)["games"])

# ---- 7. WRITE CSVs -----------------------------------------------------------

cat("\n================ SECTION 7: WRITE ================\n")
fits <- data.frame(
  model = c("targets only", "games only", "targets + games"),
  intercept = c(a1, a2, unname(c3["(Intercept)"])),
  targets_coef = c(b1, NA, unname(c3["targets"])),
  games_coef = c(NA, b2, unname(c3["games"])),
  squared_total = c(sse1, sse2, sse3))
assump <- data.frame(
  check = c("r_games_targets", "vif", "quad_targets_p", "quad_games_p",
            "bp_stat", "bp_p", "shapiro_p", "max_cooks_d"),
  value = c(r_tg, vif, p_qt, p_qg, bp, p_bp, sw$p.value, unname(cd[imax])))
players <- wr[, c("player_name", "targets", "games", "total_points", "fitted", "resid")]
players$t_left <- t_left

out <- list(stats_post46_fits.csv = fits, stats_post46_coefficients.csv = se_tab,
            stats_post46_assumptions.csv = assump, stats_post46_players.csv = players,
            stats_post46_within_games.csv = within)
for (f in names(out)) {
  utils::write.csv(out[[f]], file.path(DATA_DIR, f), row.names = FALSE)
  gate(file.exists(file.path(DATA_DIR, f)), paste("wrote", f))
}
cat("\nDone. Paste the full console output.\n")
