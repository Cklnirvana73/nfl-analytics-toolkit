# ==============================================================================
# Statistics Series: Post 38 corner-data pull (for the four-corners visual)
# Covariance (Concept 38, [GAP] demo built this session)
#
# Rebuilds the 31 QB1 + WR1 teammate pairs exactly as
# pull_stats_post38_covariance_data.R did, then for every one of the 419
# pair-weeks computes each player's distance from his own average over that
# pair's shared weeks. It prints the four corner counts and how much of the
# covariance each corner contributes, and saves the 419 rows to one CSV that
# the visual script reads.
#
# REPRODUCTION GATE: the script stops before writing anything unless it
# reproduces the verified Post 38 values (31 pairs, 419 pair-weeks, combined
# covariance 21.6992, variances 62.3258 / 63.8538).
#
# Writes exactly one file:
#   stats_posts/output/data/stats_post38_pairweek_deviations.csv
# ==============================================================================

# ---- 0. CONFIGURATION --------------------------------------------------------
TOOLKIT_ROOT <- here::here()
if (!dir.exists(TOOLKIT_ROOT)) stop("TOOLKIT_ROOT not found: ", TOOLKIT_ROOT)
cat("TOOLKIT_ROOT:", TOOLKIT_ROOT, "\n")

library(dplyr)
library(nflreadr)

SEASON     <- 2025L
MAIN_FLOOR <- 8L
OUT_DIR    <- file.path(TOOLKIT_ROOT, "stats_posts", "output", "data")
OUT_CSV    <- file.path(OUT_DIR, "stats_post38_pairweek_deviations.csv")

# Verified Post 38 values (pull script console, section 5)
GATE_K      <- 31L
GATE_N      <- 419L
GATE_COV    <- 21.6992
GATE_VAR_QB <- 62.3258
GATE_VAR_WR <- 63.8538

f4 <- function(x) formatC(x, digits = 4, format = "f")
banner <- function(x) cat("\n========== ", x, " ==========\n", sep = "")

# ---- 1. LOAD (same loader and filters as the verified pull) ------------------
banner("1. LOAD")
weekly_raw <- tryCatch(
  nflreadr::load_player_stats(seasons = SEASON, summary_level = "week"),
  error = function(e) {
    message("summary_level signature failed, falling back: ", conditionMessage(e))
    nflreadr::load_player_stats(seasons = SEASON)
  }
)
team_col <- intersect(c("team", "recent_team"), names(weekly_raw))[1]
name_col <- intersect(c("player_display_name", "player_name"), names(weekly_raw))[1]
if (is.na(team_col)) stop("No team column (team / recent_team) found.")
if (is.na(name_col)) stop("No player name column found.")
need <- c("player_id", "position", "week", "attempts", "targets", "fantasy_points_ppr")
miss <- setdiff(need, names(weekly_raw))
if (length(miss) > 0) stop("Missing columns: ", paste(miss, collapse = ", "))

weekly <- weekly_raw %>%
  rename(team = all_of(team_col), pname = all_of(name_col))
if ("season_type" %in% names(weekly)) {
  weekly <- filter(weekly, season_type == "REG")
} else {
  weekly <- filter(weekly, week <= 18L)
}
weekly <- weekly %>%
  filter(position %in% c("QB", "WR"), !is.na(fantasy_points_ppr))
dup <- weekly %>% count(player_id, week) %>% filter(n > 1)
stopifnot(nrow(weekly) > 0, nrow(dup) == 0)
cat("QB/WR regular-season rows:", nrow(weekly), "\n")

# ---- 2. PAIRS AND SHARED WEEKS (same definitions as the verified pull) -------
banner("2. PAIRS")
qb1 <- weekly %>%
  filter(position == "QB") %>%
  group_by(team, player_id) %>%
  summarise(att = sum(attempts, na.rm = TRUE), .groups = "drop") %>%
  group_by(team) %>%
  slice_max(att, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(qb_id = player_id)
wr1 <- weekly %>%
  filter(position == "WR") %>%
  group_by(team, player_id) %>%
  summarise(tgt = sum(targets, na.rm = TRUE), .groups = "drop") %>%
  group_by(team) %>%
  slice_max(tgt, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(wr_id = player_id)

qb_weeks <- weekly %>%
  inner_join(select(qb1, team, qb_id), by = c("team", "player_id" = "qb_id"),
             relationship = "many-to-one") %>%
  select(team, week, qb_pts = fantasy_points_ppr)
wr_weeks <- weekly %>%
  inner_join(select(wr1, team, wr_id), by = c("team", "player_id" = "wr_id"),
             relationship = "many-to-one") %>%
  select(team, week, wr_pts = fantasy_points_ppr)
pairs_long <- inner_join(qb_weeks, wr_weeks, by = c("team", "week"),
                         relationship = "one-to-one")

main_teams <- pairs_long %>% count(team) %>% filter(n >= MAIN_FLOOR) %>% pull(team)
dev <- pairs_long %>%
  filter(team %in% main_teams) %>%
  group_by(team) %>%
  mutate(qb_dev = qb_pts - mean(qb_pts), wr_dev = wr_pts - mean(wr_pts)) %>%
  ungroup() %>%
  mutate(product = qb_dev * wr_dev)

# ---- 3. REPRODUCTION GATE ----------------------------------------------------
banner("3. REPRODUCTION GATE")
k <- n_distinct(dev$team)
N <- nrow(dev)
df_pooled <- N - k
cov_chk    <- sum(dev$product) / df_pooled
var_qb_chk <- sum(dev$qb_dev^2) / df_pooled
var_wr_chk <- sum(dev$wr_dev^2) / df_pooled
cat("Pairs:", k, " Pair-weeks:", N, "\n")
cat("Combined covariance:", f4(cov_chk), "  variances QB / WR:",
    f4(var_qb_chk), "/", f4(var_wr_chk), "\n")
gate_ok <- k == GATE_K && N == GATE_N &&
  abs(cov_chk - GATE_COV) < 1e-4 &&
  abs(var_qb_chk - GATE_VAR_QB) < 1e-4 &&
  abs(var_wr_chk - GATE_VAR_WR) < 1e-4
cat("Post 38 reproduced:", gate_ok, "\n")
if (!gate_ok) stop("Post 38 numbers not reproduced. Nothing written. Stop and report.")

# ---- 4. CORNER COUNTS AND CONTRIBUTIONS --------------------------------------
banner("4. CORNERS")
dev <- dev %>%
  mutate(corner = case_when(
    qb_dev > 0 & wr_dev > 0 ~ "both_above",
    qb_dev < 0 & wr_dev < 0 ~ "both_below",
    qb_dev > 0 & wr_dev < 0 ~ "qb_above_wr_below",
    qb_dev < 0 & wr_dev > 0 ~ "qb_below_wr_above",
    .default = "on_an_axis"
  ))
corners <- dev %>%
  group_by(corner) %>%
  summarise(weeks = n(), product_sum = sum(product), .groups = "drop") %>%
  arrange(corner)
print(corners)
same_side <- sum(dev$corner %in% c("both_above", "both_below"))
opposite  <- sum(dev$corner %in% c("qb_above_wr_below", "qb_below_wr_above"))
cat("Same-side weeks:", same_side, "  Opposite-side weeks:", opposite,
    "  On an axis:", sum(dev$corner == "on_an_axis"), "\n")
pos_sum <- sum(dev$product[dev$product > 0])
neg_sum <- sum(dev$product[dev$product < 0])
cat("Sum of positive products:", f4(pos_sum), "  sum of negative products:", f4(neg_sum), "\n")
cat("Net sum / (pair-weeks - pairs) =", f4((pos_sum + neg_sum) / df_pooled),
    "(must equal the combined covariance)\n")
stopifnot(abs((pos_sum + neg_sum) / df_pooled - GATE_COV) < 1e-4)

cat("QB distance range:", f4(min(dev$qb_dev)), "to", f4(max(dev$qb_dev)), "\n")
cat("WR distance range:", f4(min(dev$wr_dev)), "to", f4(max(dev$wr_dev)), "\n")

# ---- 5. WRITE THE ONE CSV ----------------------------------------------------
banner("5. WRITE")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
out <- dev %>% select(team, week, qb_dev, wr_dev, product, corner)
write.csv(out, OUT_CSV, row.names = FALSE)
back <- read.csv(OUT_CSV, stringsAsFactors = FALSE)
stopifnot(nrow(back) == GATE_N,
          abs(sum(back$product) / (nrow(back) - n_distinct(back$team)) - GATE_COV) < 1e-4)
cat("Wrote", nrow(back), "rows to", OUT_CSV, "\n")
cat("Read-back check passed: CSV reproduces the combined covariance.\n")
