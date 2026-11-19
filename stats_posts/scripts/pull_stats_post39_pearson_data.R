# ==============================================================================
# Statistics Series: Post 39 data pull
# Pearson correlation (Concept 39, tag [NFL], anchor R/18)
#
# READ ONLY with respect to R/18. Nothing is sourced, retrained, or rerun.
# Reads the saved R/18 outputs, reproduces the stored Pearson r values, then
# computes the teaching numbers for the post from the same saved panel.
#
# Sources verified this session (R/18_predictive_validity_ab_testing.R):
#   lines 666-669   r_roll3 / r_std via cor(x, y, use = "complete.obs"),
#                   no method argument, so Pearson (the default)
#   lines 662-664   y = fantasy_pts_actual, x = fantasy_pts_roll3 / fantasy_pts_std
#   lines 648-650   one r per position, pooled across 16 seasons
#   lines 776-777   stored r rounded to 4 decimals
#   lines 1250-1261 saved files in data/season2_cache/
#
# Gates (script stops before writing anything if any fails):
#   G1 structure: columns, row counts, position counts match the stored results
#   G2 reproduction: recomputed r, rounded to 4 decimals, equals stored r exactly
#   G3 identity: r = covariance / (sd_x * sd_y) = sum of z-score products / (n - 1)
#   G4 units: rescaling changes covariance by the scale factor, leaves r unchanged
#
# Writes (only after all gates pass), to stats_posts/output/data/:
#   stats_post39_position_summary.csv
#   stats_post39_season_r.csv
#   stats_post39_deciles.csv
#
# HOW TO RUN: source("stats_posts/scripts/pull_stats_post39_pearson_data.R") from the project root.
# Paste the full console output back into the chat.
# ==============================================================================

# ---- CONFIGURATION -----------------------------------------------------------
TOOLKIT_ROOT <- here::here()
if (!dir.exists(TOOLKIT_ROOT)) stop("TOOLKIT_ROOT not found: ", TOOLKIT_ROOT)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

CACHE_DIR    <- file.path(TOOLKIT_ROOT, "data", "season2_cache")
PANEL_PATH   <- file.path(CACHE_DIR, "s2_week4_ab_panel.rds")
RESULTS_PATH <- file.path(CACHE_DIR, "s2_week4_ab_results.rds")
OUT_DIR      <- file.path(TOOLKIT_ROOT, "stats_posts", "output", "data")

EXPECTED_ROWS <- 44786L
POSITIONS     <- c("QB", "RB", "WR")
TOL_IDENT     <- 1e-12   # algebraic identities
TOL_COV       <- 1e-9    # covariance rescaling
SCALE_DIV     <- 6       # rescale points into six-point units (one touchdown's worth)
N_BINS        <- 10L     # decile bins for the straight-line check

for (p in c(PANEL_PATH, RESULTS_PATH)) {
  if (!file.exists(p)) stop("Missing file: ", p)
}

cat("R:", R.version.string, "| dplyr", as.character(packageVersion("dplyr")),
    "| readr", as.character(packageVersion("readr")), "\n")
cat("Panel   :", PANEL_PATH, "| modified", format(file.mtime(PANEL_PATH)), "\n")
cat("Results :", RESULTS_PATH, "| modified", format(file.mtime(RESULTS_PATH)), "\n\n")

# ---- HELPER ------------------------------------------------------------------
# Every statistic for one predictor/outcome pair, computed by hand and by R.
# Means here are the position-wide means of each column, which is what
# Pearson r uses (not each player's own average).
pair_stats <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  x  <- x[ok]
  y  <- y[ok]
  n  <- length(x)
  if (n < 3L) stop("Fewer than 3 complete pairs")
  mx <- mean(x)
  my <- mean(y)
  sx <- sd(x)
  sy <- sd(y)
  if (sx == 0 || sy == 0) stop("Zero spread in x or y; r undefined")
  dx <- x - mx
  dy <- y - my
  prod_xy  <- dx * dy
  cov_hand <- sum(prod_xy) / (n - 1)
  data.frame(
    n          = n,
    mean_x     = mx,
    sd_x       = sx,
    mean_y     = my,
    sd_y       = sy,
    cov_hand   = cov_hand,
    cov_r      = cov(x, y),
    r_cor      = cor(x, y),
    r_def      = cov_hand / (sx * sy),
    r_z        = sum((dx / sx) * (dy / sy)) / (n - 1),
    same_side  = sum(prod_xy > 0),
    opp_side   = sum(prod_xy < 0),
    on_line    = sum(prod_xy == 0),
    pos_sum    = sum(prod_xy[prod_xy > 0]),
    neg_sum    = sum(prod_xy[prod_xy < 0]),
    cov_six    = cov(x / SCALE_DIV, y / SCALE_DIV),
    r_six      = cor(x / SCALE_DIV, y / SCALE_DIV),
    cov_x_six  = cov(x / SCALE_DIV, y),
    r_x_six    = cor(x / SCALE_DIV, y)
  )
}

# ---- SECTION 1: STRUCTURE (G1) -----------------------------------------------
panel   <- readRDS(PANEL_PATH)
results <- readRDS(RESULTS_PATH)

need_panel <- c("season", "week", "player_id", "player_name", "position", "team",
                "fantasy_pts_actual", "fantasy_pts_roll3", "fantasy_pts_std",
                "games_played_prior")
need_res   <- c("position", "n_player_weeks", "n_seasons", "r_roll3", "r_std")
miss_p <- setdiff(need_panel, names(panel))
miss_r <- setdiff(need_res, names(results))
if (length(miss_p) > 0) stop("Panel missing: ", paste(miss_p, collapse = ", "))
if (length(miss_r) > 0) stop("Results missing: ", paste(miss_r, collapse = ", "))

stopifnot(
  is.numeric(panel$fantasy_pts_actual),
  is.numeric(panel$fantasy_pts_roll3),
  is.numeric(panel$fantasy_pts_std),
  is.character(panel$position)
)

cat("==== SECTION 1: STRUCTURE ====\n")
cat("Panel rows:", nrow(panel), "(expected", EXPECTED_ROWS, ")\n")
if (nrow(panel) != EXPECTED_ROWS) stop("G1 FAIL: panel row count")
if (!setequal(unique(panel$position), POSITIONS)) stop("G1 FAIL: panel positions")
if (!setequal(results$position, POSITIONS)) stop("G1 FAIL: results positions")

pos_counts <- panel %>% count(position, name = "n_panel")
g1 <- results %>%
  select(position, n_player_weeks, n_seasons) %>%
  inner_join(pos_counts, by = "position", relationship = "one-to-one")
print(as.data.frame(g1))
if (any(g1$n_panel != g1$n_player_weeks)) stop("G1 FAIL: position counts differ from stored n")

cat("\nSeasons in panel:", paste(range(panel$season), collapse = " to "),
    "| distinct:", n_distinct(panel$season), "\n")
cat("Min games_played_prior:", min(panel$games_played_prior), "\n")
if (min(panel$games_played_prior) < 3L) stop("G1 FAIL: rows below the 3-prior-game gate")

dup <- panel %>% count(player_id, season, team, week) %>% filter(n > 1)
cat("Duplicate player-season-team-week keys:", nrow(dup), "\n")

cat("\nNA counts:\n")
print(colSums(is.na(panel[, c("fantasy_pts_actual", "fantasy_pts_roll3", "fantasy_pts_std")])))

cat("\nDistributions by position (fantasy points):\n")
dist_tbl <- panel %>%
  group_by(position) %>%
  summarise(
    across(
      c(fantasy_pts_actual, fantasy_pts_roll3, fantasy_pts_std),
      list(min = \(v) min(v, na.rm = TRUE),
           p50 = \(v) median(v, na.rm = TRUE),
           max = \(v) max(v, na.rm = TRUE)),
      .names = "{.col}__{.fn}"
    ),
    .groups = "drop"
  )
print(as.data.frame(dist_tbl), digits = 4)

# ---- SECTION 2: REPRODUCTION GATE (G2) ---------------------------------------
cat("\n==== SECTION 2: REPRODUCTION GATE ====\n")
recalc <- panel %>%
  group_by(position) %>%
  summarise(
    r_std_calc   = cor(fantasy_pts_std,   fantasy_pts_actual, use = "complete.obs"),
    r_roll3_calc = cor(fantasy_pts_roll3, fantasy_pts_actual, use = "complete.obs"),
    .groups = "drop"
  ) %>%
  inner_join(results %>% select(position, r_std, r_roll3),
             by = "position", relationship = "one-to-one") %>%
  mutate(
    std_match   = abs(round(r_std_calc, 4)   - r_std)   < 1e-9,
    roll3_match = abs(round(r_roll3_calc, 4) - r_roll3) < 1e-9
  )
print(as.data.frame(recalc), digits = 6)
if (!all(recalc$std_match) || !all(recalc$roll3_match)) {
  stop("G2 FAIL: recomputed r does not reproduce stored r. Nothing written.")
}
cat("G2 PASS: all 6 stored r values reproduced to 4 decimals.\n")

# ---- SECTION 3: IDENTITY GATE (G3) AND TEACHING TABLE ------------------------
cat("\n==== SECTION 3: IDENTITY GATE AND TEACHING TABLE ====\n")
summ_list <- list()
for (pos in POSITIONS) {
  d <- panel[panel$position == pos, ]
  for (pred in c("std", "roll3")) {
    xcol <- if (pred == "std") "fantasy_pts_std" else "fantasy_pts_roll3"
    s <- pair_stats(d[[xcol]], d$fantasy_pts_actual)
    s$position  <- pos
    s$predictor <- pred
    summ_list[[length(summ_list) + 1L]] <- s
  }
}
summ <- bind_rows(summ_list) %>% relocate(position, predictor)

ident_ok <- with(summ,
  all(abs(r_cor - r_def) < TOL_IDENT) &&
  all(abs(r_cor - r_z) < TOL_IDENT) &&
  all(abs(cov_hand - cov_r) < TOL_COV)
)
cat("Max |r - cov/(sd_x*sd_y)|:", format(max(abs(summ$r_cor - summ$r_def)), digits = 3), "\n")
cat("Max |r - sum(z_x*z_y)/(n-1)|:", format(max(abs(summ$r_cor - summ$r_z)), digits = 3), "\n")
cat("Max |hand covariance - cov()|:", format(max(abs(summ$cov_hand - summ$cov_r)), digits = 3), "\n")
if (!ident_ok) stop("G3 FAIL: identity check. Nothing written.")
cat("G3 PASS\n\n")

print(as.data.frame(summ %>% select(position, predictor, n, mean_x, sd_x,
                                    mean_y, sd_y, cov_hand, r_cor)), digits = 6)

cat("\nSame-side vs opposite-side player-weeks (relative to position-wide means):\n")
print(as.data.frame(summ %>%
  transmute(position, predictor, n, same_side, opp_side, on_line,
            same_share = round(same_side / n, 4),
            pos_sum = round(pos_sum, 2), neg_sum = round(neg_sum, 2))))

# ---- SECTION 4: UNITS GATE (G4) ----------------------------------------------
cat("\n==== SECTION 4: UNITS (points vs six-point units) ====\n")
units_tbl <- summ %>%
  transmute(position, predictor,
            cov_points = cov_hand, cov_both_six = cov_six, cov_x_only_six = cov_x_six,
            r_points = r_cor, r_both_six = r_six, r_x_only_six = r_x_six)
print(as.data.frame(units_tbl), digits = 6)
units_ok <- with(summ,
  all(abs(cov_six - cov_hand / SCALE_DIV^2) < TOL_COV) &&
  all(abs(cov_x_six - cov_hand / SCALE_DIV) < TOL_COV) &&
  all(abs(r_six - r_cor) < TOL_IDENT) &&
  all(abs(r_x_six - r_cor) < TOL_IDENT)
)
if (!units_ok) stop("G4 FAIL: rescaling check. Nothing written.")
cat("G4 PASS: covariance shrinks by", SCALE_DIV^2, "(both) and", SCALE_DIV,
    "(one side); r unchanged.\n")

# ---- SECTION 5: RANKING BY COVARIANCE VS BY r --------------------------------
cat("\n==== SECTION 5: POSITION ORDER, COVARIANCE VS r ====\n")
for (pred in c("std", "roll3")) {
  s <- summ[summ$predictor == pred, ]
  ord_cov <- s$position[order(-s$cov_hand)]
  ord_r   <- s$position[order(-s$r_cor)]
  same    <- identical(ord_cov, ord_r)
  cat(sprintf("%-5s | by covariance: %s | by r: %s | orders %s\n",
              pred, paste(ord_cov, collapse = " > "), paste(ord_r, collapse = " > "),
              if (same) "MATCH" else "DIFFER"))
}

# ---- SECTION 6: SEASON-BY-SEASON r (CONTEXT) ---------------------------------
cat("\n==== SECTION 6: r BY SEASON (context, not a test) ====\n")
season_r <- panel %>%
  group_by(position, season) %>%
  summarise(
    n       = n(),
    r_std   = cor(fantasy_pts_std,   fantasy_pts_actual, use = "complete.obs"),
    r_roll3 = cor(fantasy_pts_roll3, fantasy_pts_actual, use = "complete.obs"),
    .groups = "drop"
  )
season_span <- season_r %>%
  group_by(position) %>%
  summarise(
    seasons       = n(),
    min_n         = min(n),
    r_std_min     = min(r_std),
    r_std_median  = median(r_std),
    r_std_max     = max(r_std),
    r_roll3_min   = min(r_roll3),
    r_roll3_max   = max(r_roll3),
    std_wins      = sum(r_std > r_roll3),
    .groups = "drop"
  )
print(as.data.frame(season_span), digits = 4)

# ---- SECTION 7: STRAIGHT-LINE CHECK BY DECILE (CONTEXT) ----------------------
cat("\n==== SECTION 7: DECILES OF SEASON-TO-DATE AVERAGE (context) ====\n")
deciles <- panel %>%
  filter(!is.na(fantasy_pts_std), !is.na(fantasy_pts_actual)) %>%
  group_by(position) %>%
  mutate(bin = ntile(fantasy_pts_std, N_BINS)) %>%
  group_by(position, bin) %>%
  summarise(
    n           = n(),
    mean_std    = mean(fantasy_pts_std),
    mean_actual = mean(fantasy_pts_actual),
    sd_actual   = sd(fantasy_pts_actual),
    .groups = "drop"
  )
print(as.data.frame(deciles), digits = 4)
mono <- deciles %>%
  group_by(position) %>%
  summarise(rises_every_bin = all(diff(mean_actual) > 0), .groups = "drop")
print(as.data.frame(mono))

# ---- SECTION 8: WRITE (only reached if G1 to G4 passed) ----------------------
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
summary_out <- summ %>%
  select(position, predictor, n, mean_x, sd_x, mean_y, sd_y,
         covariance = cov_hand, r = r_cor, same_side, opp_side, on_line,
         pos_sum, neg_sum, cov_both_six = cov_six)
p1 <- file.path(OUT_DIR, "stats_post39_position_summary.csv")
p2 <- file.path(OUT_DIR, "stats_post39_season_r.csv")
p3 <- file.path(OUT_DIR, "stats_post39_deciles.csv")
stopifnot(nrow(summary_out) == 6L, nrow(deciles) == length(POSITIONS) * N_BINS)
write_csv(summary_out, p1)
write_csv(season_r, p2)
write_csv(deciles, p3)
cat("\nWrote:\n ", p1, "(", nrow(summary_out), "rows)\n ", p2, "(", nrow(season_r),
    "rows)\n ", p3, "(", nrow(deciles), "rows)\n")

# ---- CONSOLE SUMMARY ---------------------------------------------------------
cat("\n================ Post 39 pull summary ================\n")
for (pos in POSITIONS) {
  s <- summ[summ$position == pos & summ$predictor == "std", ]
  cat(sprintf(paste0("%s | n %d | season-to-date avg sd %.4f, actual sd %.4f | ",
                     "covariance %.4f (points x points) | r %.4f | same-side %.1f%%\n"),
              pos, s$n, s$sd_x, s$sd_y, s$cov_hand, s$r_cor, 100 * s$same_side / s$n))
}
cat("All gates passed. Paste this full console output back into the chat.\n")
