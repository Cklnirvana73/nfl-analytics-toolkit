# ==============================================================================
# stats_post45_pull.R
# Statistics Series Post 45 (Concept 44: Ordinary least squares)
#
# Verification pull. Read-only: reads one CSV, writes three new CSVs.
# Input : stats_posts/output/data/stats_post44v3_wr.csv (written by stats_post44_pull_v3.R)
#         2025 regular season, wide receivers with 20+ targets, n = 133,
#         targets vs season total fantasy points (flat full PPR).
# Output: stats_posts/output/data/stats_post45_summary.csv
#         stats_posts/output/data/stats_post45_sse_grid.csv
#         stats_posts/output/data/stats_post45_top_contributors.csv
#
# Base R only (stats, utils). No toolkit functions are sourced.
#
# Reserved for later posts, printed here only where a gate needs them and
# never for prose: R-squared (Concept 48), residual SD (Concepts 46-47),
# Cook's distance and the no-Nacua refit (Concept 47).
# ==============================================================================

TOOLKIT_ROOT <- here::here()
stopifnot(dir.exists(TOOLKIT_ROOT))
DATA_DIR <- file.path(TOOLKIT_ROOT, "stats_posts", "output", "data")
F_WR     <- file.path(DATA_DIR, "stats_post44v3_wr.csv")

# Values locked in Post 44 (Outline_Amendment_Post44.md)
N_WR      <- 133L
X_MIN     <- 20
X_MAX     <- 185
SLOPE_44  <- 1.87
INT_44    <- -9.39
BP_P_44   <- 2.4e-6   # Breusch-Pagan p from the v3 pull
QUAD_P_44 <- 0.063    # quadratic-term p from the v3 pull

gate <- function(ok, msg) {
  if (!isTRUE(ok)) stop("GATE FAILED: ", msg, call. = FALSE)
  cat("  [PASS] ", msg, "\n", sep = "")
}
check <- function(ok, msg) {   # soft check: reports, never stops
  cat(if (isTRUE(ok)) "  [MATCH] " else "  [DIFFERS, investigate] ", msg, "\n", sep = "")
}
show <- function(label, value, digits = 7) {
  cat(sprintf("  %-48s %s\n", label, format(signif(value, digits), scientific = FALSE)))
}

# ---- 0. LOAD AND REPRODUCE POST 44 -------------------------------------------

cat("\n================ SECTION 0: REPRODUCE POST 44 ================\n")
gate(file.exists(F_WR), "stats_post44v3_wr.csv exists (run stats_post44_pull_v3.R first)")
wr <- utils::read.csv(F_WR, stringsAsFactors = FALSE)
cat("  Columns:", paste(names(wr), collapse = ", "), "\n")
gate(all(c("player_name", "targets", "total_points") %in% names(wr)),
     "CSV has player_name, targets, total_points")
gate(nrow(wr) == N_WR, sprintf("%d receivers", N_WR))
gate(!anyNA(wr$targets) && !anyNA(wr$total_points), "no missing targets or points")
gate(min(wr$targets) == X_MIN && max(wr$targets) == X_MAX,
     sprintf("targets run %d to %d", X_MIN, X_MAX))

x <- wr$targets
y <- wr$total_points
n <- length(y)

m <- stats::lm(y ~ x)
a <- unname(coef(m)[1])
b <- unname(coef(m)[2])
show("lm() slope", b)
show("lm() intercept", a)
gate(abs(b - SLOPE_44) < 0.005, "slope rounds to 1.87")
gate(abs(a - INT_44) < 0.005, "intercept rounds to -9.39")

# ---- 1. CLOSED-FORM OLS -------------------------------------------------------

cat("\n================ SECTION 1: CLOSED FORM ================\n")
x_bar <- mean(x)
y_bar <- mean(y)
Sxx   <- sum((x - x_bar)^2)
Sxy   <- sum((x - x_bar) * (y - y_bar))
b_cf  <- Sxy / Sxx
a_cf  <- y_bar - b_cf * x_bar
show("mean targets", x_bar)
show("mean points", y_bar)
show("closed-form slope  Sxy / Sxx", b_cf)
show("closed-form intercept  y_bar - slope * x_bar", a_cf)
show("cov(x, y) / var(x)", stats::cov(x, y) / stats::var(x))
gate(abs(b_cf - b) < 1e-8, "closed-form slope equals lm() slope")
gate(abs(a_cf - a) < 1e-8, "closed-form intercept equals lm() intercept")
show("line at mean targets", a + b * x_bar)
gate(abs((a + b * x_bar) - y_bar) < 1e-8, "line passes through (mean targets, mean points)")

# ---- 2. RESIDUAL PROPERTIES ---------------------------------------------------

cat("\n================ SECTION 2: RESIDUALS ================\n")
e <- y - (a + b * x)
scale_e <- sum(abs(e))
show("sum of residuals", sum(e))
show("sum of residuals x targets", sum(e * x))
show("correlation(residuals, targets)", stats::cor(e, x))
show("players above the line", sum(e > 0))
show("players below the line", sum(e < 0))
gate(abs(sum(e)) < 1e-8 * scale_e, "residuals sum to zero")
gate(abs(sum(e * x)) < 1e-8 * scale_e * max(x), "residuals uncorrelated with targets")

# ---- 3. THE QUANTITY OLS MINIMIZES -------------------------------------------

cat("\n================ SECTION 3: SUM OF SQUARED MISSES ================\n")
sse  <- function(a_, b_) sum((y - a_ - b_ * x)^2)
SSE0 <- sse(a, b)
show("SSE at the OLS line", SSE0)

# 3a. Tilt the slope while keeping the line through the mean point
tilt <- do.call(rbind, lapply(c(-0.25, -0.10, -0.05, 0.05, 0.10, 0.25), function(d) {
  b2 <- b + d
  a2 <- y_bar - b2 * x_bar
  data.frame(change = "tilt slope", amount = d, slope = b2, intercept = a2,
             sse = sse(a2, b2))
}))
# 3b. Shift the intercept with the slope fixed
shift <- do.call(rbind, lapply(c(-10, -5, -1, 1, 5, 10), function(d) {
  data.frame(change = "shift intercept", amount = d, slope = b, intercept = a + d,
             sse = sse(a + d, b))
}))
# 3c. Best line forced through zero targets, zero points
b0 <- sum(x * y) / sum(x^2)
origin <- data.frame(change = "through origin", amount = NA_real_, slope = b0,
                     intercept = 0, sse = sse(0, b0))

grid <- rbind(
  data.frame(change = "OLS", amount = 0, slope = b, intercept = a, sse = SSE0),
  tilt, shift, origin
)
grid$sse_increase     <- grid$sse - SSE0
grid$sse_increase_pct <- 100 * grid$sse_increase / SSE0
print(grid, digits = 6, row.names = FALSE)

gate(all(grid$sse[-1] > SSE0), "every alternative line has a larger SSE")
gate(all(abs(shift$sse - (SSE0 + n * shift$amount^2)) < 1e-6 * SSE0),
     "intercept shift d adds exactly n * d^2 to SSE")
gate(all(abs(tilt$sse - (SSE0 + Sxx * tilt$amount^2)) < 1e-6 * SSE0),
     "slope tilt d about the mean point adds exactly Sxx * d^2 to SSE")

# 3d. Brute-force search over a grid of lines
slopes     <- seq(1.50, 2.30, by = 0.01)
intercepts <- seq(-40, 25, by = 0.5)
best <- c(Inf, NA, NA)
for (bb in slopes) for (aa in intercepts) {
  s <- sse(aa, bb)
  if (s < best[1]) best <- c(s, aa, bb)
}
show("grid search best slope", best[3])
show("grid search best intercept", best[2])
show("grid search best SSE", best[1])
gate(best[1] >= SSE0, "no grid line beats the OLS line")
gate(abs(best[3] - b) <= 0.01 && abs(best[2] - a) <= 0.5,
     "grid search lands next to the OLS line")

# ---- 4. SQUARED VS ABSOLUTE MISSES -------------------------------------------

cat("\n================ SECTION 4: SQUARED VS ABSOLUTE ================\n")
# Least absolute deviations line by iteratively reweighted least squares.
a_l <- a
b_l <- b
for (it in 1:500) {
  r  <- y - a_l - b_l * x
  w  <- 1 / pmax(abs(r), 1e-6)
  fw <- stats::lm.wfit(cbind(1, x), y, w)$coefficients
  done <- max(abs(fw - c(a_l, b_l))) < 1e-10
  a_l <- unname(fw[1])
  b_l <- unname(fw[2])
  if (done) break
}
sae <- function(a_, b_) sum(abs(y - a_ - b_ * x))
show("LAD iterations", it)
show("LAD slope", b_l)
show("LAD intercept", a_l)
show("sum of absolute misses, OLS line", sae(a, b))
show("sum of absolute misses, LAD line", sae(a_l, b_l))
show("sum of squared misses, OLS line", SSE0)
show("sum of squared misses, LAD line", sse(a_l, b_l))
gate(sae(a_l, b_l) <= sae(a, b) + 1e-8, "LAD line has the smaller absolute sum")
gate(SSE0 <= sse(a_l, b_l) + 1e-8, "OLS line has the smaller squared sum")

# ---- 5. WHICH PLAYERS DRIVE THE SQUARED SUM ----------------------------------

cat("\n================ SECTION 5: CONTRIBUTIONS ================\n")
wr$fitted_ols <- a + b * x
wr$resid_ols  <- e
wr$sq_share   <- 100 * e^2 / SSE0
wr$abs_share  <- 100 * abs(e) / scale_e
top <- wr[order(-wr$sq_share), c("player_name", "targets", "total_points",
                                 "fitted_ols", "resid_ols", "sq_share", "abs_share")]
print(head(top, 10), digits = 5, row.names = FALSE)
k10 <- ceiling(0.10 * n)
show("share of SSE from the largest miss (pct)", top$sq_share[1])
show("share of SSE from the 5 largest (pct)", sum(top$sq_share[1:5]))
show(sprintf("share of SSE from the top %d (10 pct of players)", k10), sum(top$sq_share[1:k10]))
show("share of absolute sum from the largest miss (pct)", top$abs_share[1])

# ---- 6. METHOD-FIT CHECKLIST (detailed-patterns.md, Linear Regression) -------

cat("\n================ SECTION 6: ASSUMPTIONS ================\n")
# Linearity: quadratic term
mq <- stats::lm(y ~ x + I(x^2))
quad_p <- summary(mq)$coefficients[3, 4]
show("linearity: quadratic term p", quad_p)
check(abs(quad_p - QUAD_P_44) < 0.0015, "quadratic p matches Post 44 pull (0.063)")

# Independence: one row per player by construction
gate(!anyDuplicated(wr$player_id), "independence: one row per player_id")

# Constant spread: Breusch-Pagan (Koenker studentized form, n * R^2 of e^2 on x)
aux   <- stats::lm(I(e^2) ~ x)
bp    <- n * summary(aux)$r.squared
bp_p  <- stats::pchisq(bp, df = 1, lower.tail = FALSE)
show("constant spread: Breusch-Pagan statistic", bp)
show("constant spread: Breusch-Pagan p", bp_p)
check(abs(log10(bp_p) - log10(BP_P_44)) < 0.1, "BP p matches Post 44 pull (2.4e-6)")

# Normality of residuals (reported for completeness; not needed for the slope)
sw <- stats::shapiro.test(e)
show("normality: Shapiro-Wilk p", sw$p.value)

# What uneven spread does to OLS: slope unchanged, default SE unreliable
X     <- cbind(1, x)
bread <- solve(crossprod(X))
meat  <- crossprod(X * e)
V_hc1 <- bread %*% meat %*% bread * n / (n - 2)
se_default <- summary(m)$coefficients[2, 2]
se_hc1     <- sqrt(V_hc1[2, 2])
show("slope SE, default (assumes constant spread)", se_default)
show("slope SE, HC1 robust (allows uneven spread)", se_hc1)
show("robust / default ratio", se_hc1 / se_default)

# ---- 7. WRITE CSVs -----------------------------------------------------------

cat("\n================ SECTION 7: WRITE ================\n")
summary_tbl <- data.frame(
  n = n, slope = b, intercept = a, mean_targets = x_bar, mean_points = y_bar,
  sse = SSE0, through_origin_slope = b0, through_origin_sse = sse(0, b0),
  lad_slope = b_l, lad_intercept = a_l,
  top1_sse_share_pct = top$sq_share[1], top5_sse_share_pct = sum(top$sq_share[1:5]),
  quad_p = quad_p, bp_p = bp_p, shapiro_p = sw$p.value,
  se_default = se_default, se_hc1 = se_hc1
)
utils::write.csv(summary_tbl, file.path(DATA_DIR, "stats_post45_summary.csv"), row.names = FALSE)
utils::write.csv(grid, file.path(DATA_DIR, "stats_post45_sse_grid.csv"), row.names = FALSE)
utils::write.csv(top, file.path(DATA_DIR, "stats_post45_top_contributors.csv"), row.names = FALSE)
cat("  Wrote 3 CSVs to", DATA_DIR, "\n")
cat("\nALL GATES PASSED\n")
