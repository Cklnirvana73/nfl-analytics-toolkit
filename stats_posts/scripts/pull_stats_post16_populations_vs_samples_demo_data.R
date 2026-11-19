# =============================================================================
# pull_stats_post16_populations_vs_samples_demo_data.R
#
# Statistics Series, Post 16: Populations vs. Samples (Concept 16, [GAP])
#
# PURPOSE
#   Demonstrate that a minimum-appearance filter is a population definition,
#   not a data-cleaning step. The same statistic is computed across four
#   nested player pools built from the same 2025 season, and the script
#   reports how much of the scoring each pool retains relative to how many
#   players it drops.
#
#   This script only reads and summarizes. It writes nothing and fits nothing.
#
# HOW TO RUN
#   Rscript pull_stats_post16_populations_vs_samples_demo_data.R
#   (or source() the whole file in an interactive session)
# =============================================================================

# ---------------------------------------------------------------------------
# CONFIGURATION
# ---------------------------------------------------------------------------
SEASON          <- 2025L
POSITIONS       <- c("QB", "RB", "WR", "TE")
GAME_THRESHOLDS <- c(1L, 4L, 8L, 12L)   # minimum recorded games per pool
HEADLINE_MIN    <- 8L                    # the project's standard skill-position filter

suppressPackageStartupMessages({
  library(nflreadr)
  library(dplyr)
  library(tidyr)
})

cat("\n=========================================================\n")
cat("Post 16 demo pull: populations vs. samples\n")
cat("Season:", SEASON, "| Positions:", paste(POSITIONS, collapse = ", "), "\n")
cat("=========================================================\n\n")

# ---------------------------------------------------------------------------
# 1. LOAD WEEKLY PLAYER STATS
#    nflreadr has changed the load_player_stats() signature across versions,
#    so try the current form first and fall back to the older one.
# ---------------------------------------------------------------------------
weekly_raw <- tryCatch(
  nflreadr::load_player_stats(seasons = SEASON, summary_level = "week"),
  error = function(e) {
    message("Modern signature failed, falling back. Message: ", conditionMessage(e))
    nflreadr::load_player_stats(seasons = SEASON)
  }
)

cat("Rows loaded (all season types):", format(nrow(weekly_raw), big.mark = ","), "\n")

# Column-name defense: confirm every column this script depends on exists.
needed_cols <- c("player_id", "position", "week", "fantasy_points_ppr")
missing_cols <- setdiff(needed_cols, names(weekly_raw))
if (length(missing_cols) > 0) {
  stop("Missing expected columns: ", paste(missing_cols, collapse = ", "),
       "\nAvailable columns: ", paste(names(weekly_raw), collapse = ", "))
}

# Regular season only. Some nflreadr versions carry season_type, some do not.
weekly <- weekly_raw
if ("season_type" %in% names(weekly)) {
  weekly <- dplyr::filter(weekly, season_type == "REG")
} else {
  weekly <- dplyr::filter(weekly, week <= 18L)
}

weekly <- weekly %>%
  dplyr::filter(position %in% POSITIONS) %>%
  dplyr::filter(!is.na(fantasy_points_ppr))

cat("Rows after regular season + position filter:",
    format(nrow(weekly), big.mark = ","), "\n\n")

if (nrow(weekly) == 0) stop("No rows survived filtering. Check the season and column names.")

# ---------------------------------------------------------------------------
# 2. COLLAPSE TO PLAYER-SEASON
#    A "recorded game" is one weekly row with a non-missing PPR line.
#    Traded players: group by player_id only, never player_id + team.
# ---------------------------------------------------------------------------
player_season <- weekly %>%
  dplyr::group_by(player_id) %>%
  dplyr::summarise(
    position    = dplyr::last(position),
    games       = dplyr::n(),
    total_ppr   = sum(fantasy_points_ppr, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  dplyr::filter(games > 0) %>%                      # zero-denominator guard
  dplyr::mutate(ppr_per_game = total_ppr / games)

league_players <- nrow(player_season)
league_ppr     <- sum(player_season$total_ppr)

cat("Full pool (every player with at least one recorded game)\n")
cat("  Players:", format(league_players, big.mark = ","), "\n")
cat("  Total PPR:", format(round(league_ppr, 1), big.mark = ","), "\n\n")

# ---------------------------------------------------------------------------
# 3. THE SAME STATISTIC ACROSS FOUR NESTED POOLS
# ---------------------------------------------------------------------------
pool_summary <- lapply(GAME_THRESHOLDS, function(g) {
  pool <- dplyr::filter(player_season, games >= g)
  if (nrow(pool) == 0) return(NULL)
  data.frame(
    min_games      = g,
    players        = nrow(pool),
    pct_players    = 100 * nrow(pool) / league_players,
    pct_ppr_kept   = 100 * sum(pool$total_ppr) / league_ppr,
    mean_ppr_pg    = mean(pool$ppr_per_game),
    median_ppr_pg  = median(pool$ppr_per_game),
    sd_ppr_pg      = sd(pool$ppr_per_game)
  )
})
pool_summary <- do.call(rbind, pool_summary)

cat("---------------------------------------------------------\n")
cat("SAME STATISTIC, FOUR POPULATIONS\n")
cat("---------------------------------------------------------\n")
print(
  pool_summary %>%
    dplyr::mutate(dplyr::across(c(pct_players, pct_ppr_kept, mean_ppr_pg,
                                  median_ppr_pg, sd_ppr_pg),
                                ~ round(.x, 2))),
  row.names = FALSE
)
cat("\n")

# ---------------------------------------------------------------------------
# 4. THE HEADLINE CONTRAST: EVERYONE vs. THE STANDARD FILTER
# ---------------------------------------------------------------------------
all_pool  <- player_season
filt_pool <- dplyr::filter(player_season, games >= HEADLINE_MIN)

if (nrow(filt_pool) == 0) stop("Headline filter removed every player. Check HEADLINE_MIN.")

dropped_players <- nrow(all_pool) - nrow(filt_pool)
dropped_ppr     <- sum(all_pool$total_ppr) - sum(filt_pool$total_ppr)

cat("---------------------------------------------------------\n")
cat("HEADLINE CONTRAST: all players vs.", HEADLINE_MIN, "+ games\n")
cat("---------------------------------------------------------\n")
cat("Players dropped:", format(dropped_players, big.mark = ","),
    sprintf("(%.1f%% of the pool)\n", 100 * dropped_players / nrow(all_pool)))
cat("Scoring dropped:", format(round(dropped_ppr, 1), big.mark = ","),
    sprintf("(%.1f%% of all PPR)\n", 100 * dropped_ppr / sum(all_pool$total_ppr)))
cat("Mean PPR/game, all players:   ", sprintf("%.2f", mean(all_pool$ppr_per_game)), "\n")
cat("Mean PPR/game, filtered pool: ", sprintf("%.2f", mean(filt_pool$ppr_per_game)), "\n")
cat("Shift:                        ",
    sprintf("%+.2f", mean(filt_pool$ppr_per_game) - mean(all_pool$ppr_per_game)), "\n\n")

# ---------------------------------------------------------------------------
# 5. WHO THE FILTER REMOVES (position composition of each pool)
# ---------------------------------------------------------------------------
composition <- dplyr::bind_rows(
  all_pool  %>% dplyr::count(position) %>% dplyr::mutate(pool = "all players"),
  filt_pool %>% dplyr::count(position) %>% dplyr::mutate(pool = paste0(HEADLINE_MIN, "+ games"))
) %>%
  dplyr::group_by(pool) %>%
  dplyr::mutate(pct_of_pool = round(100 * n / sum(n), 1)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(position, pool)

cat("---------------------------------------------------------\n")
cat("POSITION COMPOSITION OF EACH POOL\n")
cat("---------------------------------------------------------\n")
print(as.data.frame(composition), row.names = FALSE)
cat("\n")

# ---------------------------------------------------------------------------
# 6. PER-POSITION EFFECT OF THE HEADLINE FILTER
# ---------------------------------------------------------------------------
by_pos <- dplyr::bind_rows(
  all_pool  %>% dplyr::group_by(position) %>%
    dplyr::summarise(n = dplyr::n(), mean_ppr_pg = mean(ppr_per_game), .groups = "drop") %>%
    dplyr::mutate(pool = "all"),
  filt_pool %>% dplyr::group_by(position) %>%
    dplyr::summarise(n = dplyr::n(), mean_ppr_pg = mean(ppr_per_game), .groups = "drop") %>%
    dplyr::mutate(pool = "filtered")
) %>%
  tidyr::pivot_wider(names_from = pool,
                     values_from = c(n, mean_ppr_pg)) %>%
  dplyr::mutate(
    mean_shift  = mean_ppr_pg_filtered - mean_ppr_pg_all,
    pct_dropped = round(100 * (n_all - n_filtered) / n_all, 1),
    dplyr::across(dplyr::starts_with("mean_"), ~ round(.x, 2))
  )

cat("---------------------------------------------------------\n")
cat("PER-POSITION EFFECT OF THE", HEADLINE_MIN, "+ GAME FILTER\n")
cat("---------------------------------------------------------\n")
print(as.data.frame(by_pos), row.names = FALSE)
cat("\n")

# ---------------------------------------------------------------------------
# CONSOLE SUMMARY
# ---------------------------------------------------------------------------
cat("=========================================================\n")
cat("SUMMARY\n")
cat("=========================================================\n")
cat("Season:", SEASON, "| player-weeks:", format(nrow(weekly), big.mark = ","), "\n")
cat("Full pool:", league_players, "players |",
    HEADLINE_MIN, "+ game pool:", nrow(filt_pool), "players\n")
cat(sprintf("Filter drops %.1f%% of players and %.1f%% of the scoring.\n",
            100 * dropped_players / nrow(all_pool),
            100 * dropped_ppr / sum(all_pool$total_ppr)))
cat(sprintf("Mean PPR/game moves %.2f -> %.2f (%+.2f).\n",
            mean(all_pool$ppr_per_game),
            mean(filt_pool$ppr_per_game),
            mean(filt_pool$ppr_per_game) - mean(all_pool$ppr_per_game)))
cat("=========================================================\n\n")
