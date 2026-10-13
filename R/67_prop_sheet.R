# ==============================================================================
# NFL Analytics Toolkit -- Season 3
# Prop Sheet: expected stats and X+ threshold probabilities by matchup
# File: R/67_prop_sheet.R
#
# PRIVATE TOOL. Output is for Christian's own betting use and is NEVER
# published: not in the column, not in Notes, not on LinkedIn.
#
# PURPOSE
# -------
# For every playable QB/RB/WR/TE in the coming week:
#   - expected volume (targets, carries, pass attempts)
#   - expected receptions, receiving yards, rushing yards, passing yards,
#     each WITH and WITHOUT the opponent adjustment, so the size of the
#     matchup effect is visible directly
#   - probability of clearing every threshold on a fixed grid (X+ markets)
#   - a Lean / Mixed / Avoid call per team position, built only from matchup
#     categories that pass a persistence gate
#
# ONE CODE PATH FOR LIVE AND HOLDOUT
# ----------------------------------
# Every input is rebuilt from nflreadr play-by-play inside this file, so the
# 2025 holdout tests exactly the code that produces the live sheet. Inputs:
# nflreadr pbp, snap counts, rosters, players, schedules. The R/59 bands file
# is read for live availability only.
#
# MODEL (constants fitted on training seasons, never on the evaluated season)
# ---------------------------------------------------------------------------
# 1. Share. Season-to-date share in games played, blended with last season's
#    share on the SAME team by a games weight n/(n+k). k is fitted per
#    position and share type by grid on training seasons. There is no recent
#    window term: R/18 settled season-to-date over last-3 (Oct 13).
#    Share types without a market (e.g. WR carry share) use
#    R67_K_SHARE_FALLBACK; they only feed the vacated-share sums.
# 2. Vacated share. When a teammate is unavailable his share is either
#    redistributed proportionally to available teammates or left unallocated.
#    Which one is chosen on training-season volume error, and printed.
#    QB attempts go entirely to the available QB with the most season attempts.
# 3. Team volume. lm of game targets, pass attempts and carries on the
#    season-to-date per-game value, the team's pregame margin and the game
#    total (nflverse spread_line, total_line). A model without the line terms
#    is the fallback when a line is missing.
# 4. Efficiency. Catch rate, yards per reception, yards per carry, completion
#    rate and yards per completion, each shrunk toward the season-to-date
#    position mean with k fitted by grid.
# 5. Matchup. The opponent's season-to-date allowed rate for the same quantity
#    and position group, as a ratio to the league mean, shrunk toward 1 with
#    k_def fitted by grid. Inf is in the grid: if a matchup term adds nothing
#    out of sample it is switched off, and the console says so.
# 6. Distribution. Volume ~ negative binomial, dispersion fitted by moments per
#    position and volume type on training residuals (Poisson if
#    underdispersed). Receptions and completions ~ binomial. Yards = sum of
#    draws from the position's empirical per-touch yards distribution
#    (training seasons), scaled to the player's expected yards per touch.
#
# GATES (fixed before any result was seen)
# ----------------------------------------
# Calibration gate. Params fitted on the two seasons before the holdout,
#   scored on the holdout season weeks 3-18 for players who played that week,
#   had >= 2 prior games, and clear the volume floor. Per market, predicted
#   probabilities across all thresholds are binned in tenths; every bin with
#   n >= R67_CAL_MIN_N must satisfy |observed - predicted| <= R67_CAL_TOL.
#   A market that fails has its probabilities WITHHELD in the live sheet (NA
#   in the CSV, "withheld" in the markdown). Expected values still print.
#   The holdout conditions on the player playing. The live sheet conditions on
#   R/59 playable. A player who left a game early is in the holdout as it
#   happened, so in-game injury risk is inside the fitted distribution.
# Persistence gate. A matchup category drives Lean/Avoid only if, on the three
#   seasons before the live season (team-weeks, week >= 4), the correlation
#   between a team's season-to-date value and its next-week value has a
#   cluster-bootstrap (team-season, B = R67_PERSIST_B) 10th percentile above
#   zero, for BOTH the offense side and the defense side.
#
# CALL RULE (unchanged from the weekly column)
# --------------------------------------------
# Defense ranked 1-8 most generous to offenses counts for the offense, 25-32
# against. Offense ranked 1-16 is top half. For + top half = Lean; against +
# bottom half = Avoid; any other defense signal = Mixed. Per position, a Lean
# and an Avoid combine to Mixed; a Mixed with a Lean or an Avoid takes the
# Lean or the Avoid.
#
# NOT MODELED (stated, not hidden)
# --------------------------------
# Defense effect on target VOLUME by position; touchdowns; weather; QB
# rushing matchup (ratio fixed at 1); players with zero games this season.
#
# OUTPUTS (data/season3_cache)
# ----------------------------
#   s3_r67_prop_sheet_wNN.csv        one row per player
#   s3_r67_prop_probs_wNN.csv        player x market x threshold probability
#   s3_r67_calls_wNN.csv             offense team x position x category detail
#   s3_r67_prop_sheet_wNN.md         PRIVATE readable sheet
#   s3_r67_holdout_gate_<season>.rds cached calibration gate (force to rebuild)
#   s3_r67_persistence_<season>.rds  cached persistence gate
#   prop_ledger.csv                  bet log for R/68, created once if absent
#
# RUN
# ---
#   source(here::here("R", "67_prop_sheet.R"))
#   ps <- run_prop_sheet(week = 6)
#
# Author: Christian K. LeBlanc
# Version: 1.3
#
# CHANGELOG
# ---------
# 1.3  (Oct 13) Team volume anchored to the current season. v1.2 holdout:
#      team targets ran 31.9 model vs 30.4 actual for every position, flat at
#      31.5-32.1 all season while actuals ranged 28-33; shares were close
#      (WR 0.189 vs 0.183). The level-form lm shrank each team toward the
#      2023-2024 mean. Now each model is fitted on deviations from this
#      season's league mean per team-game through w-1 (y - L ~ (std - L) +
#      margin + total), and predictions add that mean back. Leak-free: L uses
#      only games before the target week. Scale factors are re-fitted on top;
#      any that land near 1.0 will be neutralized in place after this run.
# 1.2  (Oct 13) Fixes for the two markets that failed the v1.1 gate, both
#      fitted on training seasons only and re-scored on the holdout.
#      (a) QB attempts as a two-part mixture. On the 2025 holdout, full games
#      (QB threw >= 90% of team attempts) were on target (32.6 model vs 32.1
#      actual attempts, error SD 74 pass yards) while the 14% partial games
#      averaged 99 yards short; one unimodal draw was too wide on top and too
#      thin at the bottom. Now: with probability q (training share of partial
#      games) attempts = E_att x a draw from the training partial-game fraction
#      distribution; otherwise NB around E_att x r_full with dispersion fitted
#      on full games only.
#      (b) Volume scale factors per position x volume type = actual / predicted
#      on training rows, applied to expected targets, carries and attempts.
#      Holdout WR targets ran 6.04 model vs 5.54 actual with catch rate on
#      target; if that bias is 2025-specific the factor lands near 1.0 and the
#      gate says so.
#      (c) Diagnostics now split WR/TE/RB targets into share and team total.
# 1.1  (Oct 13) Live availability: a player missing from the R/59 pool is
#      counted available if he played in his team's most recent game. R/59's
#      pool comes from the R/29 projection set, which does not carry backup QBs,
#      so in v1.0 Huntley (BAL), Bagent (CHI) and Jalon Daniels (TB) were
#      treated as unavailable and their teams' attempts went unassigned. This
#      mirrors the holdout, where available means played that week.
#      Holdout diagnostics added (component bias by position, simulated vs
#      realized spread per market, QB attempts split by full game) and written
#      to s3_r67_holdout_diag_<season>.csv / _rows_<season>.csv, to locate the
#      receptions and pass_yards calibration failures. No model constant was
#      changed; any fix is fitted on training seasons and re-scored on the
#      holdout. Basis lines no longer print for a position with no player row.
# 1.0  (Oct 13) Initial build.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(glue)
  library(here)
})

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------

R67_SCHEMA        <- "s3_r67_props_v1_3"   # bumping this invalidates the holdout cache
R67_QB_FULL_SHARE <- 0.9   # QB threw >= this share of team attempts = full game
R67_SEASON        <- 2026L
R67_CACHE_DIR     <- here::here("data", "season3_cache")
R67_POS           <- c("QB", "RB", "WR", "TE")

R67_MIN_PRIOR_G        <- 2L   # games played before the target week
R67_PRIOR_SEASON_MIN_G <- 4L   # last-season games needed for its share to count
R67_FLOOR <- list(QB_att = 15, RB_car = 5, RB_tgt = 3, WR_tgt = 3, TE_tgt = 2)

R67_K_SHARE_GRID     <- c(0, 0.5, 1, 2, 3, 4, 6, 8, 12)
R67_K_RATE_GRID      <- c(0, 5, 10, 20, 40, 80, 160, 320, 640)
R67_K_DEF_GRID       <- c(5, 10, 20, 40, 80, 160, 320, 640, Inf)
R67_K_SHARE_FALLBACK <- 2
R67_HIST_WEEKS       <- 3:18

R67_NSIM_LIVE    <- 10000L
R67_NSIM_HOLDOUT <- 2000L
R67_CAL_TOL      <- 0.05
R67_CAL_MIN_N    <- 200L
R67_PERSIST_B    <- 500L
R67_PERSIST_Q    <- 0.10
R67_SEED         <- 20261013L

R67_THRESHOLDS <- list(
  receptions = 1:12,
  rec_yards  = seq(10, 150, 5),
  rush_yards = seq(10, 150, 5),
  pass_yards = seq(150, 375, 25)
)
R67_MARKETS_BY_POS <- list(
  QB = c("pass_yards", "rush_yards"),
  RB = c("rush_yards", "receptions", "rec_yards"),
  WR = c("receptions", "rec_yards"),
  TE = c("receptions", "rec_yards")
)

# Matchup categories for Lean/Avoid. hg = higher value is better for offense.
R67_CATS <- tibble::tribble(
  ~cat,           ~label,                         ~hg,   ~positions,
  "qb_epa_db",    "EPA per dropback",             TRUE,  "QB",
  "qb_sack_rate", "sack rate",                    FALSE, "QB",
  "deep_epa",     "EPA per deep target (20+)",    TRUE,  "QB|WR",
  "rush_epa",     "EPA per designed rush",        TRUE,  "RB",
  "stuff_rate",   "stuff rate (runs for <= 0)",   FALSE, "RB",
  "rb_tgt_epa",   "EPA per RB target",            TRUE,  "RB",
  "wr_tgt_epa",   "EPA per WR target",            TRUE,  "WR",
  "te_tgt_epa",   "EPA per TE target",            TRUE,  "TE",
  "middle_epa",   "EPA per middle-of-field target", TRUE, "TE"
)

R67_PBP_COLS <- c(
  "season", "week", "season_type", "game_id", "posteam", "defteam",
  "home_team", "away_team", "spread_line", "total_line", "play_type",
  "pass_attempt", "sack", "complete_pass", "receiver_player_id",
  "receiving_yards", "rush_attempt", "qb_scramble", "rusher_player_id",
  "rushing_yards", "passer_player_id", "passing_yards", "qb_kneel",
  "qb_spike", "two_point_attempt", "qb_dropback", "epa", "air_yards",
  "pass_location", "yards_gained"
)

R67_TEAM_MAP <- c(
  LAR = "LA", STL = "LA", SL = "LA", JAC = "JAX", WSH = "WAS", OAK = "LV",
  LVR = "LV", SD = "LAC", ARZ = "ARI", BLT = "BAL", CLV = "CLE", HST = "HOU",
  GNB = "GB", KAN = "KC", NOR = "NO", NWE = "NE", SFO = "SF", TAM = "TB"
)

`%||%` <- function(a, b) if (is.null(a)) b else a

.r67_team <- function(x) {
  x <- as.character(x)
  hit <- !is.na(x) & x %in% names(R67_TEAM_MAP)
  x[hit] <- unname(R67_TEAM_MAP[x[hit]])
  x
}

.r67_div <- function(num, den) ifelse(!is.na(den) & den > 0, num / den, NA_real_)

# ==============================================================================
# DATA LOADING
# ==============================================================================

.r67_load_pbp <- function(seasons) {
  out <- lapply(seasons, function(s) {
    message(glue("    pbp {s}"))
    d <- as.data.frame(nflreadr::load_pbp(s))
    miss <- setdiff(R67_PBP_COLS, names(d))
    if (length(miss) > 0L) {
      stop(glue("R/67: pbp {s} is missing columns: {paste(miss, collapse = ', ')}"),
           call. = FALSE)
    }
    d <- d[, R67_PBP_COLS]
    d[d$season_type == "REG" & !is.na(d$posteam) & !is.na(d$defteam), ]
  })
  dplyr::bind_rows(out) %>%
    dplyr::mutate(
      season    = as.integer(season),
      week      = as.integer(week),
      posteam   = .r67_team(posteam),
      defteam   = .r67_team(defteam),
      home_team = .r67_team(home_team),
      away_team = .r67_team(away_team)
    )
}

# Season-specific positions plus a pfr -> gsis crosswalk for snap counts.
.r67_load_ids <- function(seasons) {
  ro <- as.data.frame(nflreadr::load_rosters(seasons))
  need <- c("season", "gsis_id", "position", "full_name")
  miss <- setdiff(need, names(ro))
  if (length(miss) > 0L) {
    stop(glue("R/67: rosters missing columns: {paste(miss, collapse = ', ')}"),
         call. = FALSE)
  }
  if (!"pfr_id" %in% names(ro)) ro$pfr_id <- NA_character_

  ro <- ro %>%
    dplyr::transmute(season  = as.integer(season),
                     gsis_id = as.character(gsis_id),
                     pos     = as.character(position),
                     name    = as.character(full_name),
                     pfr_id  = as.character(pfr_id)) %>%
    dplyr::filter(!is.na(gsis_id), gsis_id != "") %>%
    dplyr::mutate(pos = dplyr::if_else(pos %in% c("HB", "FB"), "RB", pos))

  pl <- tryCatch(as.data.frame(nflreadr::load_players()), error = function(e) NULL)
  xw_pl <- NULL
  if (!is.null(pl) && all(c("gsis_id", "pfr_id") %in% names(pl))) {
    xw_pl <- pl %>%
      dplyr::transmute(gsis_id = as.character(gsis_id),
                       pfr_id  = as.character(pfr_id)) %>%
      dplyr::filter(!is.na(gsis_id), !is.na(pfr_id), pfr_id != "")
  }

  pos_tbl <- ro %>%
    dplyr::distinct(season, gsis_id, .keep_all = TRUE) %>%
    dplyr::select(season, gsis_id, pos, name)

  xw <- dplyr::bind_rows(
    ro %>% dplyr::filter(!is.na(pfr_id), pfr_id != "") %>%
      dplyr::select(gsis_id, pfr_id),
    xw_pl
  ) %>%
    dplyr::distinct(pfr_id, .keep_all = TRUE)

  list(pos = pos_tbl, xw = xw)
}

.r67_load_snaps <- function(seasons, xw) {
  sn <- as.data.frame(nflreadr::load_snap_counts(seasons))
  need <- c("game_id", "season", "game_type", "week", "pfr_player_id", "team",
            "offense_snaps")
  miss <- setdiff(need, names(sn))
  if (length(miss) > 0L) {
    stop(glue("R/67: snap counts missing columns: {paste(miss, collapse = ', ')}"),
         call. = FALSE)
  }
  sn <- sn %>%
    dplyr::filter(game_type == "REG") %>%
    dplyr::transmute(season = as.integer(season), week = as.integer(week),
                     game_id = as.character(game_id), team = .r67_team(team),
                     pfr_id = as.character(pfr_player_id),
                     offense_snaps = as.numeric(offense_snaps))
  n_all <- sum(sn$offense_snaps > 0, na.rm = TRUE)
  out <- sn %>%
    dplyr::inner_join(xw, by = "pfr_id") %>%
    dplyr::group_by(season, week, game_id, gsis_id) %>%
    dplyr::summarise(team = dplyr::first(team),
                     offense_snaps = sum(offense_snaps, na.rm = TRUE),
                     .groups = "drop")
  n_hit <- sum(out$offense_snaps > 0, na.rm = TRUE)
  message(glue("    snap crosswalk: {n_hit} of {n_all} offensive snap rows ",
               "matched to a gsis id ({round(100 * n_hit / max(n_all, 1), 1)}%)"))
  out
}

# ==============================================================================
# TABLES FROM PBP
# ==============================================================================

.r67_build_tables <- function(pbp, snaps, pos_tbl) {

  p <- pbp %>%
    dplyr::mutate(
      two  = dplyr::coalesce(two_point_attempt, 0),
      sk   = dplyr::coalesce(sack, 0),
      scr  = dplyr::coalesce(qb_scramble, 0),
      kn   = dplyr::coalesce(qb_kneel, 0),
      sp   = dplyr::coalesce(qb_spike, 0),
      comp = dplyr::coalesce(complete_pass, 0),
      ra   = dplyr::coalesce(rush_attempt, 0),
      pa   = dplyr::coalesce(pass_attempt, 0),
      db   = dplyr::coalesce(qb_dropback, 0)
    )

  rp <- pos_tbl %>% dplyr::select(season, gsis_id, pos)

  # Play-level tables. A target is a receiver-charged pass attempt; a carry is
  # a designed run or a scramble (DK rushing yards include scrambles); a pass
  # attempt excludes sacks, scrambles, spikes and two-point tries.
  tgt <- p %>%
    dplyr::filter(pa == 1, sk == 0, two == 0, scr == 0, !is.na(receiver_player_id)) %>%
    dplyr::mutate(ry = dplyr::if_else(comp == 1,
                                      dplyr::coalesce(receiving_yards, yards_gained, 0), 0)) %>%
    dplyr::left_join(rp, by = c("season", "receiver_player_id" = "gsis_id"))

  car <- p %>%
    dplyr::filter(ra == 1 | scr == 1, kn == 0, two == 0, !is.na(rusher_player_id)) %>%
    dplyr::mutate(ruy = dplyr::coalesce(rushing_yards, yards_gained, 0)) %>%
    dplyr::left_join(rp, by = c("season", "rusher_player_id" = "gsis_id"))

  att <- p %>%
    dplyr::filter(pa == 1, sk == 0, two == 0, scr == 0, sp == 0,
                  !is.na(passer_player_id)) %>%
    dplyr::mutate(py = dplyr::if_else(comp == 1,
                                      dplyr::coalesce(passing_yards, yards_gained, 0), 0))

  keys <- c("season", "week", "game_id", "team", "gsis_id")
  pt <- tgt %>%
    dplyr::group_by(season, week, game_id, team = posteam, gsis_id = receiver_player_id) %>%
    dplyr::summarise(targets = dplyr::n(), receptions = sum(comp),
                     rec_yards = sum(ry), .groups = "drop")
  pc <- car %>%
    dplyr::group_by(season, week, game_id, team = posteam, gsis_id = rusher_player_id) %>%
    dplyr::summarise(carries = dplyr::n(), rush_yards = sum(ruy), .groups = "drop")
  pp <- att %>%
    dplyr::group_by(season, week, game_id, team = posteam, gsis_id = passer_player_id) %>%
    dplyr::summarise(pass_att = dplyr::n(), completions = sum(comp),
                     pass_yards = sum(py), .groups = "drop")

  pg <- dplyr::full_join(pt, pc, by = keys) %>% dplyr::full_join(pp, by = keys)

  # Game info and team totals
  gi <- p %>%
    dplyr::distinct(season, week, game_id, home_team, away_team, spread_line, total_line) %>%
    dplyr::group_by(game_id) %>% dplyr::slice(1) %>% dplyr::ungroup()
  tgi <- dplyr::bind_rows(
    gi %>% dplyr::transmute(season, week, game_id, team = home_team, opp = away_team,
                            margin = spread_line, total = total_line, home = TRUE),
    gi %>% dplyr::transmute(season, week, game_id, team = away_team, opp = home_team,
                            margin = -spread_line, total = total_line, home = FALSE)
  )

  tg <- tgi %>%
    dplyr::left_join(tgt %>% dplyr::count(game_id, team = posteam, name = "team_targets"),
                     by = c("game_id", "team")) %>%
    dplyr::left_join(car %>% dplyr::count(game_id, team = posteam, name = "team_carries"),
                     by = c("game_id", "team")) %>%
    dplyr::left_join(att %>% dplyr::count(game_id, team = posteam, name = "team_att"),
                     by = c("game_id", "team")) %>%
    dplyr::mutate(dplyr::across(c(team_targets, team_carries, team_att),
                                ~ dplyr::coalesce(as.numeric(.x), 0)))

  # Snap-only rows (on the field, no opportunity). Team must be one of the two
  # teams in that game or the row is dropped from team totals.
  sn_only <- snaps %>%
    dplyr::filter(offense_snaps > 0) %>%
    dplyr::anti_join(pg %>% dplyr::distinct(season, week, game_id, gsis_id),
                     by = c("season", "week", "game_id", "gsis_id")) %>%
    dplyr::semi_join(tgi, by = c("game_id", "team")) %>%
    dplyr::select(season, week, game_id, team, gsis_id)

  pg <- dplyr::bind_rows(pg, sn_only) %>%
    dplyr::left_join(snaps %>% dplyr::select(season, week, game_id, gsis_id, offense_snaps),
                     by = c("season", "week", "game_id", "gsis_id")) %>%
    dplyr::mutate(dplyr::across(c(targets, receptions, rec_yards, carries, rush_yards,
                                  pass_att, completions, pass_yards),
                                ~ dplyr::coalesce(as.numeric(.x), 0)),
                  played = dplyr::coalesce(offense_snaps, 0) > 0 |
                    (targets + carries + pass_att) > 0) %>%
    dplyr::left_join(rp, by = c("season", "gsis_id")) %>%
    dplyr::filter(pos %in% R67_POS) %>%
    dplyr::left_join(tg %>% dplyr::select(game_id, team, team_targets, team_carries, team_att),
                     by = c("game_id", "team")) %>%
    dplyr::mutate(dplyr::across(c(team_targets, team_carries, team_att),
                                ~ dplyr::coalesce(.x, 0)))

  # Defense allowed, by game and position group
  dtg <- tgt %>%
    dplyr::filter(pos %in% c("RB", "WR", "TE")) %>%
    dplyr::group_by(season, week, game_id, def = defteam, grp = pos) %>%
    dplyr::summarise(d_tgt = dplyr::n(), d_rec = sum(comp), d_recyds = sum(ry),
                     .groups = "drop")
  dca <- car %>%
    dplyr::filter(!is.na(pos), pos != "QB") %>%
    dplyr::group_by(season, week, game_id, def = defteam) %>%
    dplyr::summarise(d_car = dplyr::n(), d_ruyds = sum(ruy), .groups = "drop")
  dpa <- att %>%
    dplyr::group_by(season, week, game_id, def = defteam) %>%
    dplyr::summarise(d_att = dplyr::n(), d_comp = sum(comp), d_pyds = sum(py),
                     .groups = "drop")

  # Category numerators/denominators per team-game and side
  dbk <- p %>% dplyr::filter(db == 1, two == 0, !is.na(epa))
  drs <- p %>% dplyr::filter(ra == 1, scr == 0, kn == 0, two == 0, !is.na(epa))
  tge <- tgt %>% dplyr::filter(!is.na(epa))
  cat_src <- list(
    qb_epa_db    = dbk %>% dplyr::mutate(num = epa),
    qb_sack_rate = dbk %>% dplyr::mutate(num = sk),
    deep_epa     = tge %>% dplyr::filter(!is.na(air_yards), air_yards >= 20) %>%
                     dplyr::mutate(num = epa),
    rush_epa     = drs %>% dplyr::mutate(num = epa),
    stuff_rate   = drs %>% dplyr::mutate(num = as.numeric(dplyr::coalesce(yards_gained, 0) <= 0)),
    rb_tgt_epa   = tge %>% dplyr::filter(pos == "RB") %>% dplyr::mutate(num = epa),
    wr_tgt_epa   = tge %>% dplyr::filter(pos == "WR") %>% dplyr::mutate(num = epa),
    te_tgt_epa   = tge %>% dplyr::filter(pos == "TE") %>% dplyr::mutate(num = epa),
    middle_epa   = tge %>% dplyr::filter(pass_location == "middle") %>%
                     dplyr::mutate(num = epa)
  )
  cg <- dplyr::bind_rows(lapply(names(cat_src), function(cc) {
    d <- cat_src[[cc]]
    dplyr::bind_rows(
      d %>% dplyr::group_by(season, week, game_id, team = posteam) %>%
        dplyr::summarise(num = sum(num), den = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(side = "off"),
      d %>% dplyr::group_by(season, week, game_id, team = defteam) %>%
        dplyr::summarise(num = sum(num), den = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(side = "def")
    ) %>% dplyr::mutate(cat = cc)
  }))

  # Team EPA per play for the game note
  ep <- p %>% dplyr::filter(play_type %in% c("pass", "run"), !is.na(epa))
  epa_g <- dplyr::bind_rows(
    ep %>% dplyr::group_by(season, week, team = posteam) %>%
      dplyr::summarise(num = sum(epa), den = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(side = "off"),
    ep %>% dplyr::group_by(season, week, team = defteam) %>%
      dplyr::summarise(num = sum(epa), den = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(side = "def")
  )

  list(pg = pg, tg = tg, dtg = dtg, dca = dca, dpa = dpa, cg = cg, epa_g = epa_g,
       tgt = tgt %>% dplyr::select(season, week, comp, ry, pos),
       car = car %>% dplyr::select(season, week, ruy, pos),
       att = att %>% dplyr::select(season, week, comp, py))
}

# ==============================================================================
# FEATURES FOR ONE (SEASON, WEEK), USING ONLY GAMES BEFORE THAT WEEK
# ==============================================================================

# Team season-to-date volume per game, plus this season's league mean per
# team-game through week w-1 (v1.3 anchor). Both use only games before w.
.r67_team_std <- function(T, s, w) {
  tg_s <- T$tg %>% dplyr::filter(season == s, week < w)
  tg_s %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(t_tgt_pg = mean(team_targets), t_att_pg = mean(team_att),
                     t_car_pg = mean(team_carries), .groups = "drop") %>%
    dplyr::mutate(L_tgt = mean(tg_s$team_targets),
                  L_att = mean(tg_s$team_att),
                  L_car = mean(tg_s$team_carries))
}

# roster_now: gsis_id, team_now, available
# gi:         team, opp, margin, total (the target week's game per team)
.r67_features <- function(T, s, w, roster_now, gi) {

  pgS <- T$pg %>% dplyr::filter(season == s, week < w, played)
  if (nrow(pgS) == 0L) return(NULL)

  std <- pgS %>%
    dplyr::arrange(week) %>%
    dplyr::group_by(gsis_id) %>%
    dplyr::summarise(pos = dplyr::last(pos), team_last = dplyr::last(team), n_g = dplyr::n(),
                     targets = sum(targets), receptions = sum(receptions),
                     rec_yards = sum(rec_yards), carries = sum(carries),
                     rush_yards = sum(rush_yards), pass_att = sum(pass_att),
                     completions = sum(completions), pass_yards = sum(pass_yards),
                     tt = sum(team_targets), tc = sum(team_carries), ta = sum(team_att),
                     .groups = "drop")

  pri <- T$pg %>%
    dplyr::filter(season == s - 1L, played) %>%
    dplyr::arrange(week) %>%
    dplyr::group_by(gsis_id) %>%
    dplyr::summarise(p_team = dplyr::last(team), p_g = dplyr::n(),
                     p_tsh = .r67_div(sum(targets), sum(team_targets)),
                     p_csh = .r67_div(sum(carries), sum(team_carries)),
                     .groups = "drop") %>%
    dplyr::filter(p_g >= R67_PRIOR_SEASON_MIN_G)

  lg <- pgS %>%
    dplyr::group_by(pos) %>%
    dplyr::summarise(L_catch  = .r67_div(sum(receptions), sum(targets)),
                     L_ypr    = .r67_div(sum(rec_yards), sum(receptions)),
                     L_ypc    = .r67_div(sum(rush_yards), sum(carries)),
                     L_comp   = .r67_div(sum(completions), sum(pass_att)),
                     L_ypcomp = .r67_div(sum(pass_yards), sum(completions)),
                     .groups = "drop")

  tstd <- .r67_team_std(T, s, w)

  dr <- T$dtg %>% dplyr::filter(season == s, week < w) %>%
    dplyr::group_by(def, grp) %>%
    dplyr::summarise(d_tgt = sum(d_tgt), d_rec = sum(d_rec), d_recyds = sum(d_recyds),
                     .groups = "drop")
  Lr <- dr %>% dplyr::group_by(grp) %>%
    dplyr::summarise(Ld_catch = .r67_div(sum(d_rec), sum(d_tgt)),
                     Ld_ypr = .r67_div(sum(d_recyds), sum(d_rec)), .groups = "drop")
  dc <- T$dca %>% dplyr::filter(season == s, week < w) %>%
    dplyr::group_by(def) %>%
    dplyr::summarise(d_car = sum(d_car), d_ruyds = sum(d_ruyds), .groups = "drop")
  Ld_ypc <- .r67_div(sum(dc$d_ruyds), sum(dc$d_car))
  dp <- T$dpa %>% dplyr::filter(season == s, week < w) %>%
    dplyr::group_by(def) %>%
    dplyr::summarise(d_att = sum(d_att), d_comp = sum(d_comp), d_pyds = sum(d_pyds),
                     .groups = "drop")
  Ld_comp   <- .r67_div(sum(dp$d_comp), sum(dp$d_att))
  Ld_ypcomp <- .r67_div(sum(dp$d_pyds), sum(dp$d_comp))

  roster_now %>%
    dplyr::inner_join(std, by = "gsis_id") %>%
    dplyr::left_join(pri, by = "gsis_id") %>%
    dplyr::left_join(lg, by = "pos") %>%
    dplyr::left_join(gi %>% dplyr::select(team, opp, margin, total),
                     by = c("team_now" = "team")) %>%
    dplyr::left_join(tstd, by = c("team_now" = "team")) %>%
    dplyr::left_join(dr, by = c("opp" = "def", "pos" = "grp")) %>%
    dplyr::left_join(Lr, by = c("pos" = "grp")) %>%
    dplyr::left_join(dc, by = c("opp" = "def")) %>%
    dplyr::left_join(dp, by = c("opp" = "def")) %>%
    dplyr::mutate(season = as.integer(s), week = as.integer(w),
                  Ld_ypc = Ld_ypc, Ld_comp = Ld_comp, Ld_ypcomp = Ld_ypcomp)
}

.r67_eligible <- function(f) {
  f %>%
    dplyr::mutate(
      tpg = targets / n_g, cpg = carries / n_g, apg = pass_att / n_g,
      elig = available & n_g >= R67_MIN_PRIOR_G & !is.na(opp) &
        dplyr::case_when(
          pos == "QB" ~ apg >= R67_FLOOR$QB_att,
          pos == "RB" ~ cpg >= R67_FLOOR$RB_car | tpg >= R67_FLOOR$RB_tgt,
          pos == "WR" ~ tpg >= R67_FLOOR$WR_tgt,
          pos == "TE" ~ tpg >= R67_FLOOR$TE_tgt,
          TRUE ~ FALSE)
    )
}

# Historical roster for a played week: who played (available) and who had
# season-to-date games but did not play (unavailable, share vacated).
.r67_roster_hist <- function(T, s, w) {
  playing <- T$pg %>%
    dplyr::filter(season == s, week == w, played) %>%
    dplyr::distinct(gsis_id, .keep_all = TRUE) %>%
    dplyr::transmute(gsis_id, team_now = team, available = TRUE)
  absent <- T$pg %>%
    dplyr::filter(season == s, week < w, played) %>%
    dplyr::arrange(week) %>%
    dplyr::group_by(gsis_id) %>%
    dplyr::summarise(team_now = dplyr::last(team), .groups = "drop") %>%
    dplyr::anti_join(playing, by = "gsis_id") %>%
    dplyr::mutate(available = FALSE)
  dplyr::bind_rows(playing, absent)
}

.r67_hist_set <- function(T, seasons, weeks = R67_HIST_WEEKS) {
  rows <- list(); teams <- list()
  for (s in seasons) for (w in weeks) {
    gw <- T$tg %>% dplyr::filter(season == s, week == w)
    if (nrow(gw) == 0L) next
    f <- .r67_features(T, s, w, .r67_roster_hist(T, s, w), gw)
    if (is.null(f)) next
    act <- T$pg %>%
      dplyr::filter(season == s, week == w, played) %>%
      dplyr::group_by(gsis_id) %>%
      dplyr::summarise(targets_w = sum(targets), rec_w = sum(receptions),
                       recyds_w = sum(rec_yards), carries_w = sum(carries),
                       rushyds_w = sum(rush_yards), att_w = sum(pass_att),
                       comp_w = sum(completions), passyds_w = sum(pass_yards),
                       .groups = "drop")
    f <- f %>%
      dplyr::left_join(act, by = "gsis_id") %>%
      dplyr::left_join(gw %>% dplyr::select(team, tt_w = team_targets,
                                            tc_w = team_carries, ta_w = team_att),
                       by = c("team_now" = "team"))
    rows[[length(rows) + 1L]] <- f
    tstd <- .r67_team_std(T, s, w)
    teams[[length(teams) + 1L]] <- gw %>%
      dplyr::inner_join(tstd, by = "team") %>%
      dplyr::mutate(season = as.integer(s), week = as.integer(w))
  }
  list(rows = .r67_eligible(dplyr::bind_rows(rows)), teams = dplyr::bind_rows(teams))
}

# ==============================================================================
# APPLY PARAMETERS
# ==============================================================================

.r67_key <- function(lst, keys, fallback) {
  vapply(keys, function(k) {
    v <- lst[[k]]
    if (is.null(v)) fallback else v
  }, numeric(1), USE.NAMES = FALSE)
}

.r67_shrink <- function(num, den, L, k) {
  out <- ifelse(is.infinite(k), L, (num + k * L) / (den + k))
  out[!is.finite(out)] <- L[!is.finite(out)]
  out
}

.r67_dratio <- function(dnum, dden, Ld, kd) {
  r <- ((dnum + kd * Ld) / (dden + kd)) / Ld
  r[is.infinite(kd) | is.na(dnum) | is.na(dden) | is.na(Ld) | Ld <= 0 | !is.finite(r)] <- 1
  r
}

.r67_shares <- function(f, P) {
  k_t <- .r67_key(P$k_share, paste0(f$pos, "_tgt"), R67_K_SHARE_FALLBACK)
  k_c <- .r67_key(P$k_share, paste0(f$pos, "_car"), R67_K_SHARE_FALLBACK)
  f %>%
    dplyr::mutate(
      tsh_std = dplyr::coalesce(.r67_div(targets, tt), 0),
      csh_std = dplyr::coalesce(.r67_div(carries, tc), 0),
      same    = !is.na(p_team) & p_team == team_now,
      tsh = dplyr::if_else(same & !is.na(p_tsh), (n_g * tsh_std + k_t * p_tsh) / (n_g + k_t), tsh_std),
      csh = dplyr::if_else(same & !is.na(p_csh), (n_g * csh_std + k_c * p_csh) / (n_g + k_c), csh_std)
    )
}

.r67_redistribute <- function(f, redistribute) {
  f %>%
    dplyr::group_by(season, week, team_now) %>%
    dplyr::mutate(
      tsh_av = dplyr::if_else(available, tsh, 0),
      csh_av = dplyr::if_else(available, csh, 0),
      t_all = sum(tsh), t_av = sum(tsh_av),
      c_all = sum(csh), c_av = sum(csh_av),
      tsh_f = if (isTRUE(redistribute)) dplyr::if_else(t_av > 0, tsh_av * t_all / t_av, 0) else tsh_av,
      csh_f = if (isTRUE(redistribute)) dplyr::if_else(c_av > 0, csh_av * c_all / c_av, 0) else csh_av,
      qa = dplyr::if_else(pos == "QB" & available, pass_att, -Inf),
      att_sh = as.numeric(pos == "QB" & available & dplyr::row_number(dplyr::desc(qa)) == 1L)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-t_all, -t_av, -c_all, -c_av, -qa)
}

# v1.3: models are fitted on deviations from this season's league mean, so a
# prediction is that mean plus the fitted deviation. Shrinkage now pulls a
# team toward the current season's level, not the training-era level.
.r67_team_volume <- function(f, P) {
  nd <- data.frame(dev_tgt = f$t_tgt_pg - f$L_tgt, dev_att = f$t_att_pg - f$L_att,
                   dev_car = f$t_car_pg - f$L_car,
                   margin = f$margin, total = f$total)
  has_line <- !is.na(nd$margin) & !is.na(nd$total)
  pr <- function(m_line, m_ns, L) {
    out <- rep(NA_real_, nrow(nd))
    ok <- !is.na(nd$dev_tgt) & !is.na(L)
    if (any(ok)) out[ok] <- stats::predict(m_ns, newdata = nd[ok, , drop = FALSE])
    hl <- ok & has_line
    if (any(hl)) out[hl] <- stats::predict(m_line, newdata = nd[hl, , drop = FALSE])
    pmax(out + L, 0)
  }
  f$team_tgt_hat <- pr(P$lm$tgt, P$lm$tgt_ns, f$L_tgt)
  f$team_att_hat <- pr(P$lm$att, P$lm$att_ns, f$L_att)
  f$team_car_hat <- pr(P$lm$car, P$lm$car_ns, f$L_car)
  f$line_used    <- has_line
  f
}

.r67_rates <- function(f, P, use_def = TRUE) {
  kr <- function(prefix) .r67_key(P$k_rate, paste0(prefix, "_", f$pos), 0)
  kd <- function(prefix) {
    if (!use_def) return(rep(Inf, nrow(f)))
    .r67_key(P$k_def, paste0(prefix, "_", f$pos), Inf)
  }
  ypc_kd <- if (use_def) ifelse(f$pos == "QB", Inf, P$k_def[["ypc_RB"]] %||% Inf) else rep(Inf, nrow(f))

  f %>%
    dplyr::mutate(
      catch_hat  = .r67_shrink(receptions, targets, L_catch, kr("catch")),
      ypr_hat    = .r67_shrink(rec_yards, receptions, L_ypr, kr("ypr")),
      ypc_hat    = .r67_shrink(rush_yards, carries, L_ypc, kr("ypc")),
      comp_hat   = .r67_shrink(completions, pass_att, L_comp, kr("comp")),
      ypcomp_hat = .r67_shrink(pass_yards, completions, L_ypcomp, kr("ypcomp")),
      dr_catch   = .r67_dratio(d_rec, d_tgt, Ld_catch, kd("catch")),
      dr_ypr     = .r67_dratio(d_recyds, d_rec, Ld_ypr, kd("ypr")),
      dr_ypc     = .r67_dratio(d_ruyds, d_car, Ld_ypc, ypc_kd),
      dr_comp    = .r67_dratio(d_comp, d_att, Ld_comp, kd("comp")),
      dr_ypcomp  = .r67_dratio(d_pyds, d_comp, Ld_ypcomp, kd("ypcomp"))
    )
}

.r67_expect <- function(f, P) {
  vs <- P$vol_scale %||% list()
  s_t <- .r67_key(vs, paste0(f$pos, "_tgt"), 1)
  s_c <- .r67_key(vs, paste0(f$pos, "_car"), 1)
  s_a <- .r67_key(vs, paste0(f$pos, "_att"), 1)
  f %>%
    dplyr::mutate(
      E_tgt = pmax(dplyr::coalesce(tsh_f * team_tgt_hat, 0), 0) * s_t,
      E_car = pmax(dplyr::coalesce(csh_f * team_car_hat, 0), 0) * s_c,
      E_att = pmax(dplyr::coalesce(att_sh * team_att_hat, 0), 0) * s_a,
      catch_eff  = pmin(dplyr::coalesce(catch_hat * dr_catch, 0), 0.99),
      ypr_eff    = pmax(dplyr::coalesce(ypr_hat * dr_ypr, 0), 0),
      ypc_eff    = dplyr::coalesce(ypc_hat * dr_ypc, 0),
      comp_eff   = pmin(dplyr::coalesce(comp_hat * dr_comp, 0), 0.99),
      ypcomp_eff = pmax(dplyr::coalesce(ypcomp_hat * dr_ypcomp, 0), 0),
      exp_rec          = E_tgt * catch_eff,
      exp_rec_yards    = exp_rec * ypr_eff,
      exp_rush_yards   = E_car * ypc_eff,
      exp_pass_yards   = E_att * comp_eff * ypcomp_eff,
      exp_rec_nm       = E_tgt * pmin(dplyr::coalesce(catch_hat, 0), 0.99),
      exp_rec_yards_nm = exp_rec_nm * pmax(dplyr::coalesce(ypr_hat, 0), 0),
      exp_rush_yards_nm = E_car * dplyr::coalesce(ypc_hat, 0),
      exp_pass_yards_nm = E_att * pmin(dplyr::coalesce(comp_hat, 0), 0.99) *
        pmax(dplyr::coalesce(ypcomp_hat, 0), 0)
    )
}

.r67_predict <- function(f, P) {
  f %>%
    .r67_shares(P) %>%
    .r67_redistribute(P$redistribute) %>%
    .r67_team_volume(P) %>%
    .r67_rates(P, use_def = TRUE) %>%
    .r67_expect(P)
}

# ==============================================================================
# FITTING (training seasons only)
# ==============================================================================

.r67_grid <- function(grid, loss) {
  L <- vapply(grid, loss, numeric(1))
  list(k = grid[which.min(L)], loss = L)
}

.r67_fit <- function(H, T, train_seasons) {

  R  <- H$rows %>% dplyr::filter(elig)
  P  <- list(k_share = list(), k_rate = list(), k_def = list(), theta = list(),
             emp = list(), lm = list(), train_seasons = train_seasons, fit_log = list())

  # ---- 1. share k ----
  share_keys <- list(
    RB_tgt = list(pos = "RB", vol = "t"), WR_tgt = list(pos = "WR", vol = "t"),
    TE_tgt = list(pos = "TE", vol = "t"), RB_car = list(pos = "RB", vol = "c"),
    QB_car = list(pos = "QB", vol = "c")
  )
  for (key in names(share_keys)) {
    sk <- share_keys[[key]]
    d <- R %>% dplyr::filter(pos == sk$pos)
    if (sk$vol == "t") {
      d <- d %>% dplyr::filter(tt_w > 0) %>%
        dplyr::mutate(y = dplyr::coalesce(targets_w, 0) / tt_w, wt = tt_w,
                      sh = dplyr::coalesce(.r67_div(targets, tt), 0), pr = p_tsh)
    } else {
      d <- d %>% dplyr::filter(tc_w > 0) %>%
        dplyr::mutate(y = dplyr::coalesce(carries_w, 0) / tc_w, wt = tc_w,
                      sh = dplyr::coalesce(.r67_div(carries, tc), 0), pr = p_csh)
    }
    d <- d %>% dplyr::mutate(same = !is.na(p_team) & p_team == team_now & !is.na(pr))
    g <- .r67_grid(R67_K_SHARE_GRID, function(k) {
      est <- ifelse(d$same, (d$n_g * d$sh + k * d$pr) / (d$n_g + k), d$sh)
      sum(d$wt * (d$y - est)^2)
    })
    P$k_share[[key]] <- g$k
    P$fit_log[[paste0("k_share_", key)]] <- g$k
  }

  # ---- 2. team volume models ----
  tm <- H$teams
  # v1.3: deviation form, anchored to this season's league mean through w-1.
  tm <- tm %>%
    dplyr::mutate(y_tgt = team_targets - L_tgt, y_att = team_att - L_att,
                  y_car = team_carries - L_car,
                  dev_tgt = t_tgt_pg - L_tgt, dev_att = t_att_pg - L_att,
                  dev_car = t_car_pg - L_car)
  P$lm$tgt    <- stats::lm(y_tgt ~ dev_tgt + margin + total, data = tm)
  P$lm$att    <- stats::lm(y_att ~ dev_att + margin + total, data = tm)
  P$lm$car    <- stats::lm(y_car ~ dev_car + margin + total, data = tm)
  P$lm$tgt_ns <- stats::lm(y_tgt ~ dev_tgt, data = tm)
  P$lm$att_ns <- stats::lm(y_att ~ dev_att, data = tm)
  P$lm$car_ns <- stats::lm(y_car ~ dev_car, data = tm)

  # ---- 3. vacated-share rule, chosen on volume error ----
  vol_err <- function(redis) {
    PP <- P; PP$redistribute <- redis
    x <- H$rows %>% .r67_shares(PP) %>% .r67_redistribute(redis) %>%
      .r67_team_volume(PP) %>%
      dplyr::filter(elig) %>%
      dplyr::mutate(Et = tsh_f * team_tgt_hat, Ec = csh_f * team_car_hat)
    sum((dplyr::coalesce(x$targets_w, 0) - x$Et)^2, na.rm = TRUE) +
      sum((dplyr::coalesce(x$carries_w, 0) - x$Ec)^2, na.rm = TRUE)
  }
  e_on  <- vol_err(TRUE)
  e_off <- vol_err(FALSE)
  P$redistribute <- e_on < e_off
  P$fit_log$vacated_sse_redistribute <- e_on
  P$fit_log$vacated_sse_leave        <- e_off

  # ---- 4. player rate k ----
  rate_specs <- list(
    catch  = list(num = "receptions", den = "targets",  L = "L_catch",  y_num = "rec_w",     y_den = "targets_w",
                  dnum = "d_rec", dden = "d_tgt", Ld = "Ld_catch", pos = c("RB", "WR", "TE")),
    ypr    = list(num = "rec_yards",  den = "receptions", L = "L_ypr",  y_num = "recyds_w",  y_den = "rec_w",
                  dnum = "d_recyds", dden = "d_rec", Ld = "Ld_ypr", pos = c("RB", "WR", "TE")),
    ypc    = list(num = "rush_yards", den = "carries",  L = "L_ypc",    y_num = "rushyds_w", y_den = "carries_w",
                  dnum = "d_ruyds", dden = "d_car", Ld = "Ld_ypc", pos = c("RB", "QB")),
    comp   = list(num = "completions", den = "pass_att", L = "L_comp",  y_num = "comp_w",    y_den = "att_w",
                  dnum = "d_comp", dden = "d_att", Ld = "Ld_comp", pos = "QB"),
    ypcomp = list(num = "pass_yards", den = "completions", L = "L_ypcomp", y_num = "passyds_w", y_den = "comp_w",
                  dnum = "d_pyds", dden = "d_comp", Ld = "Ld_ypcomp", pos = "QB")
  )
  for (rn in names(rate_specs)) {
    rs <- rate_specs[[rn]]
    for (ps in rs$pos) {
      d <- R %>% dplyr::filter(pos == ps, dplyr::coalesce(.data[[rs$y_den]], 0) > 0)
      if (nrow(d) < 50L) next
      y  <- d[[rs$y_num]] / d[[rs$y_den]]
      wt <- d[[rs$y_den]]
      g <- .r67_grid(R67_K_RATE_GRID, function(k) {
        est <- .r67_shrink(d[[rs$num]], d[[rs$den]], d[[rs$L]], rep(k, nrow(d)))
        sum(wt * (y - est)^2, na.rm = TRUE)
      })
      key <- paste0(rn, "_", ps)
      P$k_rate[[key]] <- g$k
      P$fit_log[[paste0("k_rate_", key)]] <- g$k

      # ---- 5. matchup k, given the player k ----
      if (rn == "ypc" && ps == "QB") next   # QB rushing matchup not modeled
      base <- .r67_shrink(d[[rs$num]], d[[rs$den]], d[[rs$L]], rep(g$k, nrow(d)))
      gd <- .r67_grid(R67_K_DEF_GRID, function(kd) {
        est <- base * .r67_dratio(d[[rs$dnum]], d[[rs$dden]], d[[rs$Ld]], rep(kd, nrow(d)))
        sum(wt * (y - est)^2, na.rm = TRUE)
      })
      P$k_def[[key]] <- gd$k
      P$fit_log[[paste0("k_def_", key)]] <- gd$k
    }
  }

  # ---- 6a. volume scale factors (v1.2): actual / predicted on training ----
  P$vol_scale <- list()
  X0 <- .r67_predict(H$rows, P) %>% dplyr::filter(elig)
  ratio <- function(y, mu) {
    ok <- is.finite(mu) & mu > 0
    s <- sum(dplyr::coalesce(y[ok], 0)) / sum(mu[ok])
    if (!is.finite(s) || s <= 0) 1 else s
  }
  for (ps in c("RB", "WR", "TE")) {
    d <- X0 %>% dplyr::filter(pos == ps)
    P$vol_scale[[paste0(ps, "_tgt")]] <- ratio(d$targets_w, d$E_tgt)
  }
  for (ps in c("RB", "QB")) {
    d <- X0 %>% dplyr::filter(pos == ps)
    P$vol_scale[[paste0(ps, "_car")]] <- ratio(d$carries_w, d$E_car)
  }
  dq0 <- X0 %>% dplyr::filter(pos == "QB")
  P$vol_scale$QB_att <- ratio(dq0$att_w, dq0$E_att)
  for (nm in names(P$vol_scale)) P$fit_log[[paste0("vol_scale_", nm)]] <- P$vol_scale[[nm]]

  # ---- 6b. dispersion of volume (moments on training residuals) ----
  X <- .r67_predict(H$rows, P) %>% dplyr::filter(elig)
  mom <- function(y, mu) {
    ok <- is.finite(y) & is.finite(mu) & mu > 0
    y <- y[ok]; mu <- mu[ok]
    ex <- sum((y - mu)^2 - mu)
    if (length(y) < 50L || ex <= 0) return(Inf)
    th <- sum(mu^2) / ex
    if (!is.finite(th) || th > 1000) Inf else th
  }
  for (ps in c("RB", "WR", "TE")) {
    d <- X %>% dplyr::filter(pos == ps)
    P$theta[[paste0(ps, "_tgt")]] <- mom(dplyr::coalesce(d$targets_w, 0), d$E_tgt)
  }
  for (ps in c("RB", "QB")) {
    d <- X %>% dplyr::filter(pos == ps)
    P$theta[[paste0(ps, "_car")]] <- mom(dplyr::coalesce(d$carries_w, 0), d$E_car)
  }
  dq <- X %>% dplyr::filter(pos == "QB")
  P$theta$QB_att <- mom(dplyr::coalesce(dq$att_w, 0), dq$E_att)

  # ---- 6c. QB attempts mixture (v1.2) ----
  dm <- dq %>%
    dplyr::filter(dplyr::coalesce(ta_w, 0) > 0, E_att > 0) %>%
    dplyr::mutate(a = dplyr::coalesce(att_w, 0), full = a / ta_w >= R67_QB_FULL_SHARE)
  if (sum(!dm$full) >= 20L && sum(dm$full) >= 50L) {
    r_full <- sum(dm$a[dm$full]) / sum(dm$E_att[dm$full])
    P$qb_mix <- list(
      q          = mean(!dm$full),
      r_full     = r_full,
      frac       = dm$a[!dm$full] / dm$E_att[!dm$full],
      theta_full = mom(dm$a[dm$full], dm$E_att[dm$full] * r_full)
    )
    P$fit_log$qb_mix_q          <- P$qb_mix$q
    P$fit_log$qb_mix_r_full     <- P$qb_mix$r_full
    P$fit_log$qb_mix_frac_mean  <- mean(P$qb_mix$frac)
    P$fit_log$qb_mix_theta_full <- P$qb_mix$theta_full
  }

  # ---- 7. empirical per-touch yards (training seasons) ----
  tr <- function(d) d %>% dplyr::filter(season %in% train_seasons)
  for (ps in c("RB", "WR", "TE")) {
    P$emp[[paste0("rec_", ps)]] <- tr(T$tgt) %>%
      dplyr::filter(comp == 1, pos == ps) %>% dplyr::pull(ry)
  }
  P$emp$car_QB    <- tr(T$car) %>% dplyr::filter(pos == "QB") %>% dplyr::pull(ruy)
  P$emp$car_NONQB <- tr(T$car) %>% dplyr::filter(!is.na(pos), pos != "QB") %>% dplyr::pull(ruy)
  P$emp$comp_QB   <- tr(T$att) %>% dplyr::filter(comp == 1) %>% dplyr::pull(py)

  P
}

# ==============================================================================
# SIMULATION
# ==============================================================================

.r67_rvol <- function(n, mu, theta) {
  if (!is.finite(mu) || mu <= 0) return(integer(n))
  if (!is.finite(theta)) stats::rpois(n, mu) else stats::rnbinom(n, size = theta, mu = mu)
}

.r67_sum_draws <- function(cnt, emp, target_mean) {
  n <- length(cnt)
  tot <- sum(cnt)
  if (tot == 0L || length(emp) == 0L) return(numeric(n))
  sc <- max(target_mean, 0) / mean(emp)
  d  <- sample(emp, tot, replace = TRUE) * sc
  id <- rep.int(seq_len(n), cnt)
  s  <- rowsum(d, id)
  out <- numeric(n)
  out[as.integer(rownames(s))] <- s[, 1]
  out
}

.r67_sim_market <- function(mkt, r, P, nsim) {
  th <- function(key) P$theta[[key]] %||% Inf
  if (mkt %in% c("receptions", "rec_yards")) {
    V  <- .r67_rvol(nsim, r$E_tgt, th(paste0(r$pos, "_tgt")))
    Rc <- stats::rbinom(nsim, V, r$catch_eff)
    if (mkt == "receptions") return(Rc)
    return(.r67_sum_draws(Rc, P$emp[[paste0("rec_", r$pos)]], r$ypr_eff))
  }
  if (mkt == "rush_yards") {
    V   <- .r67_rvol(nsim, r$E_car, th(paste0(r$pos, "_car")))
    emp <- if (r$pos == "QB") P$emp$car_QB else P$emp$car_NONQB
    # Per-carry yards can be negative; scale the shape to the expected mean
    # only when that mean is positive, else keep the empirical shape.
    if (is.finite(r$ypc_eff) && r$ypc_eff > 0) {
      return(.r67_sum_draws(V, emp, r$ypc_eff))
    }
    return(.r67_sum_draws(V, emp, mean(emp)))
  }
  if (mkt == "pass_yards") {
    mx <- P$qb_mix
    if (!is.null(mx) && is.finite(r$E_att) && r$E_att > 0) {
      part <- stats::runif(nsim) < mx$q
      V <- integer(nsim)
      if (any(!part)) V[!part] <- .r67_rvol(sum(!part), r$E_att * mx$r_full, mx$theta_full)
      if (any(part)) {
        V[part] <- stats::rpois(sum(part),
                                r$E_att * sample(mx$frac, sum(part), replace = TRUE))
      }
    } else {
      V <- .r67_rvol(nsim, r$E_att, th("QB_att"))
    }
    C <- stats::rbinom(nsim, V, r$comp_eff)
    return(.r67_sum_draws(C, P$emp$comp_QB, r$ypcomp_eff))
  }
  stop(glue("R/67: unknown market {mkt}"), call. = FALSE)
}

.r67_sim_probs <- function(X, P, nsim) {
  out <- vector("list", nrow(X))
  for (i in seq_len(nrow(X))) {
    r <- X[i, , drop = FALSE]
    mk <- R67_MARKETS_BY_POS[[r$pos]]
    out[[i]] <- dplyr::bind_rows(lapply(mk, function(m) {
      x <- .r67_sim_market(m, r, P, nsim)
      thr <- R67_THRESHOLDS[[m]]
      tibble::tibble(gsis_id = r$gsis_id, season = r$season, week = r$week,
                     market = m, threshold = thr,
                     p = vapply(thr, function(t) mean(x >= t), numeric(1)),
                     sim_median = stats::median(x), sim_mean = mean(x),
                     sim_sd = stats::sd(x))
    }))
  }
  dplyr::bind_rows(out)
}

# ==============================================================================
# GATES
# ==============================================================================

.r67_calibration <- function(T, P_train, eval_season, nsim) {
  H <- .r67_hist_set(T, eval_season)
  X <- .r67_predict(H$rows, P_train) %>% dplyr::filter(elig)
  message(glue("    holdout {eval_season}: {nrow(X)} player-weeks, simulating {nsim} draws each"))
  pr <- .r67_sim_probs(X, P_train, nsim)
  act <- X %>%
    dplyr::transmute(gsis_id, week,
                     receptions = dplyr::coalesce(rec_w, 0),
                     rec_yards  = dplyr::coalesce(recyds_w, 0),
                     rush_yards = dplyr::coalesce(rushyds_w, 0),
                     pass_yards = dplyr::coalesce(passyds_w, 0)) %>%
    tidyr::pivot_longer(-c(gsis_id, week), names_to = "market", values_to = "actual")
  pr <- pr %>%
    dplyr::left_join(act, by = c("gsis_id", "week", "market")) %>%
    dplyr::mutate(hit = as.numeric(actual >= threshold),
                  bin = cut(p, breaks = seq(0, 1, 0.1), include.lowest = TRUE))
  bins <- pr %>%
    dplyr::group_by(market, bin) %>%
    dplyr::summarise(n = dplyr::n(), predicted = mean(p), observed = mean(hit),
                     .groups = "drop") %>%
    dplyr::mutate(gap = abs(observed - predicted), counted = n >= R67_CAL_MIN_N)
  verdict <- bins %>%
    dplyr::group_by(market) %>%
    dplyr::summarise(bins_counted = sum(counted),
                     max_gap = max(gap[counted], -Inf),
                     brier = NA_real_, .groups = "drop") %>%
    dplyr::mutate(pass = bins_counted > 0 & max_gap <= R67_CAL_TOL)
  brier <- pr %>% dplyr::group_by(market) %>%
    dplyr::summarise(brier = mean((p - hit)^2), .groups = "drop")
  verdict <- verdict %>% dplyr::select(-brier) %>% dplyr::left_join(brier, by = "market")

  # Matchup value on the holdout: expected-stat error with vs without matchup
  mv <- X %>%
    dplyr::summarise(
      rec_mse_match     = mean((dplyr::coalesce(rec_w, 0) - exp_rec)^2),
      rec_mse_nomatch   = mean((dplyr::coalesce(rec_w, 0) - exp_rec_nm)^2),
      recyd_mse_match   = mean((dplyr::coalesce(recyds_w, 0) - exp_rec_yards)^2),
      recyd_mse_nomatch = mean((dplyr::coalesce(recyds_w, 0) - exp_rec_yards_nm)^2),
      rushyd_mse_match  = mean((dplyr::coalesce(rushyds_w, 0) - exp_rush_yards)^2),
      rushyd_mse_nomatch = mean((dplyr::coalesce(rushyds_w, 0) - exp_rush_yards_nm)^2),
      passyd_mse_match  = mean((dplyr::coalesce(passyds_w, 0)[pos == "QB"] -
                                  exp_pass_yards[pos == "QB"])^2),
      passyd_mse_nomatch = mean((dplyr::coalesce(passyds_w, 0)[pos == "QB"] -
                                   exp_pass_yards_nm[pos == "QB"])^2)
    )
  # ---- v1.1 diagnostics: where does a miscalibrated market go wrong? ----
  # Component bias: model mean vs actual mean for each input, by position.
  # Volumes are compared player-week means; rates are compared pooled
  # (sum of model expectation over sum of actual denominator), which is the
  # quantity the simulation actually uses.
  comp_bias <- X %>%
    dplyr::group_by(pos) %>%
    dplyr::summarise(
      n = dplyr::n(),
      targets_model   = mean(E_tgt),  targets_actual   = mean(dplyr::coalesce(targets_w, 0)),
      targets_prescale = mean(dplyr::coalesce(tsh_f * team_tgt_hat, 0)),
      share_model     = mean(tsh_f, na.rm = TRUE),
      share_actual    = mean(dplyr::coalesce(targets_w, 0)[dplyr::coalesce(tt_w, 0) > 0] /
                               tt_w[dplyr::coalesce(tt_w, 0) > 0]),
      team_tgt_model  = mean(team_tgt_hat, na.rm = TRUE),
      team_tgt_actual = mean(tt_w, na.rm = TRUE),
      catch_model     = .r67_div(sum(catch_eff * dplyr::coalesce(targets_w, 0)),
                                 sum(dplyr::coalesce(targets_w, 0))),
      catch_actual    = .r67_div(sum(dplyr::coalesce(rec_w, 0)), sum(dplyr::coalesce(targets_w, 0))),
      rec_model       = mean(exp_rec), rec_actual = mean(dplyr::coalesce(rec_w, 0)),
      carries_model   = mean(E_car), carries_actual = mean(dplyr::coalesce(carries_w, 0)),
      att_model       = mean(E_att), att_actual = mean(dplyr::coalesce(att_w, 0)),
      comp_model      = .r67_div(sum(comp_eff * dplyr::coalesce(att_w, 0)),
                                 sum(dplyr::coalesce(att_w, 0))),
      comp_actual     = .r67_div(sum(dplyr::coalesce(comp_w, 0)), sum(dplyr::coalesce(att_w, 0))),
      ypcomp_model    = .r67_div(sum(ypcomp_eff * dplyr::coalesce(comp_w, 0)),
                                 sum(dplyr::coalesce(comp_w, 0))),
      ypcomp_actual   = .r67_div(sum(dplyr::coalesce(passyds_w, 0)), sum(dplyr::coalesce(comp_w, 0))),
      .groups = "drop")

  # Spread check per market: spread_ratio > 1 means the simulated
  # distribution is WIDER than the realized errors (tails too fat);
  # < 1 means too narrow. bias = actual mean minus simulated mean.
  spread <- pr %>%
    dplyr::distinct(gsis_id, week, market, sim_mean, sim_sd, actual) %>%
    dplyr::group_by(market) %>%
    dplyr::summarise(n = dplyr::n(),
                     sim_mean = mean(sim_mean), actual_mean = mean(actual),
                     bias = mean(actual) - mean(sim_mean),
                     rmse_realized = sqrt(mean((actual - sim_mean)^2)),
                     sd_simulated  = sqrt(mean(sim_sd^2)),
                     spread_ratio  = sd_simulated / rmse_realized,
                     .groups = "drop")

  # QB attempts split by whether the QB finished the game as the passer:
  # a QB who leaves early drags actual attempts down and fattens the error.
  qb_att <- X %>%
    dplyr::filter(pos == "QB") %>%
    dplyr::mutate(att_share_w = .r67_div(dplyr::coalesce(att_w, 0), ta_w),
                  full_game = !is.na(att_share_w) & att_share_w >= 0.9) %>%
    dplyr::group_by(full_game) %>%
    dplyr::summarise(n = dplyr::n(), att_model = mean(E_att),
                     att_actual = mean(dplyr::coalesce(att_w, 0)),
                     passyds_model = mean(exp_pass_yards),
                     passyds_actual = mean(dplyr::coalesce(passyds_w, 0)),
                     .groups = "drop")

  rows <- X %>%
    dplyr::select(season, week, gsis_id, pos, team_now, opp, n_g,
                  tsh_f, team_tgt_hat, tt_w,
                  E_tgt, targets_w, catch_eff, exp_rec, rec_w, ypr_eff, exp_rec_yards, recyds_w,
                  E_car, carries_w, ypc_eff, exp_rush_yards, rushyds_w,
                  E_att, att_w, ta_w, comp_eff, comp_w, ypcomp_eff, exp_pass_yards, passyds_w)

  list(bins = bins, verdict = verdict, matchup_value = mv,
       n_player_weeks = nrow(X), eval_season = eval_season,
       comp_bias = comp_bias, spread = spread, qb_att = qb_att, rows = rows)
}

.r67_persistence <- function(T, seasons) {
  cg <- T$cg %>% dplyr::filter(season %in% seasons)
  res <- list()
  set.seed(R67_SEED)
  for (cc in unique(cg$cat)) for (sd in c("off", "def")) {
    d <- cg %>%
      dplyr::filter(cat == cc, side == sd) %>%
      dplyr::group_by(season, week, team) %>%
      dplyr::summarise(num = sum(num), den = sum(den), .groups = "drop") %>%
      dplyr::arrange(season, team, week) %>%
      dplyr::group_by(season, team) %>%
      dplyr::mutate(cnum = dplyr::lag(cumsum(num)), cden = dplyr::lag(cumsum(den)),
                    gp = dplyr::row_number() - 1L) %>%
      dplyr::ungroup() %>%
      dplyr::filter(week >= 4, gp >= 3, cden > 0, den > 0) %>%
      dplyr::mutate(x = cnum / cden, y = num / den, cl = paste(season, team))
    if (nrow(d) < 100L) next
    r0 <- stats::cor(d$x, d$y)
    cls <- unique(d$cl)
    idx <- split(seq_len(nrow(d)), d$cl)
    bs <- vapply(seq_len(R67_PERSIST_B), function(b) {
      pick <- unlist(idx[sample(cls, length(cls), replace = TRUE)], use.names = FALSE)
      stats::cor(d$x[pick], d$y[pick])
    }, numeric(1))
    res[[length(res) + 1L]] <- tibble::tibble(
      cat = cc, side = sd, n = nrow(d), r = r0,
      lower = stats::quantile(bs, R67_PERSIST_Q, na.rm = TRUE, names = FALSE))
  }
  out <- dplyr::bind_rows(res) %>% dplyr::mutate(pass_side = lower > 0)
  gate <- out %>% dplyr::group_by(cat) %>%
    dplyr::summarise(pass = all(pass_side) & dplyr::n() == 2L, .groups = "drop")
  list(detail = out, gate = gate, seasons = seasons)
}

# ==============================================================================
# CALLS
# ==============================================================================

.r67_rank_good <- function(v, hg) {
  if (hg) rank(-v, ties.method = "min", na.last = "keep")
  else rank(v, ties.method = "min", na.last = "keep")
}

.r67_calls <- function(T, s, w, games, persist) {
  cur <- T$cg %>%
    dplyr::filter(season == s, week < w) %>%
    dplyr::group_by(cat, side, team) %>%
    dplyr::summarise(num = sum(num), den = sum(den), .groups = "drop") %>%
    dplyr::mutate(val = .r67_div(num, den)) %>%
    dplyr::left_join(R67_CATS %>% dplyr::select(cat, hg), by = "cat") %>%
    dplyr::group_by(cat, side) %>%
    dplyr::mutate(rk = .r67_rank_good(val, hg[1])) %>%
    dplyr::ungroup()
  offv <- cur %>% dplyr::filter(side == "off") %>%
    dplyr::select(cat, team, off_val = val, off_n = den, off_rank = rk)
  defv <- cur %>% dplyr::filter(side == "def") %>%
    dplyr::select(cat, opp = team, def_val = val, def_n = den, def_rank = rk)
  pos_cat <- R67_CATS %>%
    tidyr::separate_rows(positions, sep = "\\|") %>%
    dplyr::rename(pos = positions)

  games %>%
    dplyr::select(team, opp) %>%
    tidyr::crossing(pos_cat) %>%
    dplyr::left_join(offv, by = c("cat", "team")) %>%
    dplyr::left_join(defv, by = c("cat", "opp")) %>%
    dplyr::left_join(persist$gate, by = "cat") %>%
    dplyr::mutate(
      pass = dplyr::coalesce(pass, FALSE),
      dfav = def_rank <= 8, dagainst = def_rank >= 25, otop = off_rank <= 16,
      label = dplyr::case_when(
        !pass ~ "parked",
        is.na(def_rank) | is.na(off_rank) ~ "-",
        dfav & otop ~ "Lean",
        dagainst & !otop ~ "Avoid",
        dfav | dagainst ~ "Mixed",
        TRUE ~ "-")
    )
}

.r67_combine <- function(labels) {
  l <- labels[labels %in% c("Lean", "Avoid", "Mixed")]
  if (length(l) == 0L) return("No call")
  if ("Lean" %in% l && "Avoid" %in% l) return("Mixed")
  if ("Lean" %in% l) return("Lean")
  if ("Avoid" %in% l) return("Avoid")
  "Mixed"
}

# ==============================================================================
# MARKDOWN (PRIVATE)
# ==============================================================================

.r67_fmt <- function(x, d = 1) ifelse(is.na(x), "NA", formatC(x, format = "f", digits = d))

.r67_pick_thresholds <- function(pp) {
  q <- pp %>% dplyr::filter(!is.na(p), p >= 0.20, p <= 0.85) %>% dplyr::arrange(threshold)
  if (nrow(q) == 0L) return("none between 20% and 85%")
  sel <- unique(round(seq(1, nrow(q), length.out = min(4L, nrow(q)))))
  q <- q[sel, ]
  paste0(q$threshold, "+ ", round(100 * q$p), "%", collapse = ", ")
}

.r67_markdown <- function(sheet, probs, calls, games, epa_rank, verdict, s, w, path) {
  mk_label <- c(receptions = "rec", rec_yards = "rec yds", rush_yards = "rush yds",
                pass_yards = "pass yds")
  L <- c(
    glue("# Week {w} prop sheet, {s}"),
    "",
    "**PRIVATE. NOT FOR PUBLICATION.**",
    "",
    "Expected values show no-matchup -> with-matchup. X+ probabilities are the share of 10,000",
    "simulated games clearing each number; the full grid is in the probs CSV.",
    "Calibration gate (holdout): ",
    paste0("- ", verdict$market, ": ", ifelse(verdict$pass, "PASS", "FAIL, probabilities withheld"),
           " (max bin gap ", .r67_fmt(verdict$max_gap, 3), ")"),
    ""
  )
  for (g in seq_len(nrow(games))) {
    gm <- games[g, ]
    er <- function(tm, sd) epa_rank$rk[epa_rank$team == tm & epa_rank$side == sd][1]
    line_txt <- if (is.na(gm$spread_line) || is.na(gm$total_line)) {
      "No line posted yet."
    } else {
      fav <- if (gm$spread_line >= 0) gm$home else gm$away
      imp_h <- gm$total_line / 2 + gm$spread_line / 2
      imp_a <- gm$total_line / 2 - gm$spread_line / 2
      glue("{fav} favored by {abs(gm$spread_line)}, total {gm$total_line} ",
           "(implied {gm$away} {.r67_fmt(imp_a)}, {gm$home} {.r67_fmt(imp_h)}).")
    }
    L <- c(L, glue("## {gm$away} at {gm$home}"), "",
           paste0(line_txt, " Season to date, EPA per play: ",
                  gm$away, " offense ", er(gm$away, "off"), ", defense ", er(gm$away, "def"),
                  "; ", gm$home, " offense ", er(gm$home, "off"), ", defense ", er(gm$home, "def"),
                  " (1 = best)."), "")
    for (tm in c(gm$away, gm$home)) {
      opp <- if (tm == gm$away) gm$home else gm$away
      L <- c(L, glue("*{tm} offense vs {opp} defense*"), "",
             "| Pos | Player | Call | Volume | Expected (no matchup -> matchup) | X+ |",
             "|---|---|---|---|---|---|")
      for (ps in R67_POS) {
        cl <- calls %>% dplyr::filter(team == tm, pos == ps)
        call_txt <- .r67_combine(cl$label)
        basis <- cl %>% dplyr::filter(label %in% c("Lean", "Avoid", "Mixed"))
        pl <- sheet %>% dplyr::filter(team == tm, pos == ps)
        if (ps == "QB") pl <- pl %>% dplyr::arrange(dplyr::desc(E_att)) %>% utils::head(1)
        if (ps == "RB") pl <- pl %>% dplyr::arrange(dplyr::desc(E_car + E_tgt)) %>% utils::head(2)
        if (ps == "WR") pl <- pl %>% dplyr::arrange(dplyr::desc(E_tgt)) %>% utils::head(3)
        if (ps == "TE") pl <- pl %>% dplyr::arrange(dplyr::desc(E_tgt)) %>% utils::head(1)
        for (i in seq_len(nrow(pl))) {
          r <- pl[i, ]
          vol <- switch(ps,
                        QB = glue("{.r67_fmt(r$E_att)} att"),
                        RB = glue("{.r67_fmt(r$E_car)} car, {.r67_fmt(r$E_tgt)} tgt"),
                        glue("{.r67_fmt(r$E_tgt)} tgt"))
          ex <- switch(ps,
            QB = glue("pass yds {.r67_fmt(r$exp_pass_yards_nm, 0)} -> {.r67_fmt(r$exp_pass_yards, 0)}; ",
                      "rush yds {.r67_fmt(r$exp_rush_yards_nm, 0)} -> {.r67_fmt(r$exp_rush_yards, 0)}"),
            RB = glue("rush yds {.r67_fmt(r$exp_rush_yards_nm, 0)} -> {.r67_fmt(r$exp_rush_yards, 0)}; ",
                      "rec {.r67_fmt(r$exp_rec_nm)} -> {.r67_fmt(r$exp_rec)}; ",
                      "rec yds {.r67_fmt(r$exp_rec_yards_nm, 0)} -> {.r67_fmt(r$exp_rec_yards, 0)}"),
            glue("rec {.r67_fmt(r$exp_rec_nm)} -> {.r67_fmt(r$exp_rec)}; ",
                 "rec yds {.r67_fmt(r$exp_rec_yards_nm, 0)} -> {.r67_fmt(r$exp_rec_yards, 0)}"))
          pp <- probs %>% dplyr::filter(gsis_id == r$gsis_id)
          xp <- paste(vapply(R67_MARKETS_BY_POS[[ps]], function(m) {
            q <- pp %>% dplyr::filter(market == m)
            if (all(is.na(q$p))) return(glue("{mk_label[[m]]}: withheld"))
            glue("{mk_label[[m]]}: {.r67_pick_thresholds(q)}")
          }, character(1)), collapse = "<br>")
          flag <- if (!is.na(r$injury_note) && nzchar(r$injury_note)) glue(" ({r$injury_note})") else ""
          L <- c(L, glue("| {ps} | {r$player_name}{flag} | {if (i == 1) call_txt else ''} | {vol} | {ex} | {xp} |"))
        }
        if (nrow(basis) > 0L && nrow(pl) > 0L) {
          L <- c(L, paste0("|  | ", ps, " basis |  |  | ",
                           paste(glue("{basis$label}: {R67_CATS$label[match(basis$cat, R67_CATS$cat)]} ",
                                      "(off {basis$off_rank}, def {basis$def_rank}, n {basis$def_n})"),
                                 collapse = "; "), " |  |"))
        }
      }
      L <- c(L, "")
    }
  }
  writeLines(L, path, useBytes = TRUE)
  invisible(path)
}

# ==============================================================================
# LEDGER TEMPLATE
# ==============================================================================

.r67_ledger_init <- function(path) {
  if (file.exists(path)) return(invisible(FALSE))
  readr::write_csv(tibble::tibble(
    date_placed = character(), season = integer(), week = integer(),
    player_name = character(), nfl_gsis_id = character(), market = character(),
    threshold = numeric(), odds_american = integer(), stake_units = numeric(),
    sheet_prob = numeric(), book = character(), result = character(),
    notes = character()), path)
  message(glue("  Ledger template created: {path}"))
  invisible(TRUE)
}

# ==============================================================================
# PUBLIC ENTRY POINT
# ==============================================================================

#' Build the private prop sheet for a coming week
#'
#' @param week Integer. The week being bet (games not yet played).
#' @param season Integer. Live season.
#' @param force_holdout Logical. Rebuild the cached calibration gate.
#' @param force_persistence Logical. Rebuild the cached persistence gate.
#' @param nsim Integer. Draws per player per market for the live sheet.
#' @return List: sheet, probs, calls, params, calibration, persistence.
#' @export
run_prop_sheet <- function(week, season = R67_SEASON, force_holdout = FALSE,
                           force_persistence = FALSE, nsim = R67_NSIM_LIVE) {

  w <- as.integer(week); s <- as.integer(season)
  set.seed(R67_SEED)
  dir.create(R67_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

  message(glue("\n{strrep('=', 70)}\nR/67 prop sheet: season {s}, week {w} (PRIVATE)\n{strrep('=', 70)}"))

  # STEP 1: data
  seasons <- (s - 4L):s
  message("\nSTEP 1/7: Loading pbp, rosters, snaps")
  pbp <- .r67_load_pbp(seasons)
  ids <- .r67_load_ids(seasons)
  sn  <- .r67_load_snaps(seasons, ids$xw)
  T   <- .r67_build_tables(pbp, sn, ids$pos)
  rm(pbp); invisible(gc(verbose = FALSE))
  max_wk <- suppressWarnings(max(T$tg$week[T$tg$season == s]))
  message(glue("    {s} pbp coverage: through week {max_wk}"))
  if (!is.finite(max_wk) || max_wk < w - 1L) {
    stop(glue("R/67: {s} pbp only reaches week {max_wk}; week {w - 1L} is needed. ",
              "Rerun after the data updates."), call. = FALSE)
  }

  # STEP 2: calibration gate on the holdout season (cached)
  ho_season <- s - 1L
  ho_train  <- (s - 3L):(s - 2L)
  ho_path <- file.path(R67_CACHE_DIR, glue("s3_r67_holdout_gate_{ho_season}.rds"))
  ho_key  <- paste(R67_SCHEMA, paste(ho_train, collapse = "-"), ho_season,
                   R67_NSIM_HOLDOUT, R67_CAL_TOL, R67_CAL_MIN_N, sep = "|")
  cal <- if (!force_holdout && file.exists(ho_path)) readRDS(ho_path) else NULL
  if (is.null(cal) || !identical(cal$key, ho_key)) {
    message(glue("\nSTEP 2/7: Calibration gate: fit {paste(ho_train, collapse = '-')}, ",
                 "score {ho_season} (slow, cached after the first run)"))
    H_ho <- .r67_hist_set(T, ho_train)
    P_ho <- .r67_fit(H_ho, T, ho_train)
    cal  <- .r67_calibration(T, P_ho, ho_season, R67_NSIM_HOLDOUT)
    cal$key <- ho_key
    cal$params_fit_log <- P_ho$fit_log
    saveRDS(cal, ho_path)
  } else {
    message(glue("\nSTEP 2/7: Calibration gate: using cache {basename(ho_path)}"))
  }

  # STEP 3: live parameters on the three seasons before the live season
  live_train <- (s - 3L):(s - 1L)
  message(glue("\nSTEP 3/7: Fitting live parameters on {paste(live_train, collapse = '-')}"))
  H  <- .r67_hist_set(T, live_train)
  P  <- .r67_fit(H, T, live_train)

  # STEP 4: persistence gate (cached)
  ps_path <- file.path(R67_CACHE_DIR, glue("s3_r67_persistence_{s}.rds"))
  persist <- if (!force_persistence && file.exists(ps_path)) readRDS(ps_path) else NULL
  if (is.null(persist) || !identical(persist$seasons, live_train)) {
    message(glue("\nSTEP 4/7: Persistence gate on {paste(live_train, collapse = '-')}"))
    persist <- .r67_persistence(T, live_train)
    saveRDS(persist, ps_path)
  } else {
    message(glue("\nSTEP 4/7: Persistence gate: using cache {basename(ps_path)}"))
  }

  # STEP 5: live roster, schedule, features
  message("\nSTEP 5/7: Live roster, schedule and features")
  bands_path <- file.path(R67_CACHE_DIR, sprintf("s3_r59_weekly_bands_w%02d.csv", w))
  if (!file.exists(bands_path)) {
    stop(glue("R/67: {basename(bands_path)} not found. Run R/59 for week {w} first."),
         call. = FALSE)
  }
  bands <- readr::read_csv(bands_path, show_col_types = FALSE)
  need_b <- c("nfl_gsis_id", "player_name", "position", "team", "on_bye", "playable")
  miss_b <- setdiff(need_b, names(bands))
  if (length(miss_b) > 0L) {
    stop(glue("R/67: bands file missing columns: {paste(miss_b, collapse = ', ')}"),
         call. = FALSE)
  }
  for (cc in c("sleeper_injury", "report_status")) if (!cc %in% names(bands)) bands[[cc]] <- NA_character_
  bands <- bands %>%
    dplyr::filter(position %in% R67_POS, !is.na(nfl_gsis_id)) %>%
    dplyr::distinct(nfl_gsis_id, .keep_all = TRUE) %>%
    dplyr::mutate(team = .r67_team(team),
                  injury_note = dplyr::coalesce(as.character(report_status),
                                                as.character(sleeper_injury), ""))

  sch <- as.data.frame(nflreadr::load_schedules(s)) %>%
    dplyr::filter(game_type == "REG", week == w) %>%
    dplyr::transmute(game_id, home = .r67_team(home_team), away = .r67_team(away_team),
                     spread_line = as.numeric(spread_line), total_line = as.numeric(total_line))
  if (nrow(sch) == 0L) stop(glue("R/67: no {s} week {w} games in the schedule."), call. = FALSE)
  gi <- dplyr::bind_rows(
    sch %>% dplyr::transmute(team = home, opp = away, margin = spread_line, total = total_line),
    sch %>% dplyr::transmute(team = away, opp = home, margin = -spread_line, total = total_line)
  )
  n_line <- sum(!is.na(sch$spread_line) & !is.na(sch$total_line))
  message(glue("    {nrow(sch)} games, {n_line} with a spread and total posted"))

  roster_live <- bands %>%
    dplyr::transmute(gsis_id = nfl_gsis_id, team_now = team,
                     available = playable & !dplyr::coalesce(on_bye, FALSE))
  # Players not in the R/59 pool (v1.1). Available if they played in their
  # team's most recent game; otherwise unavailable (share vacated).
  team_last_wk <- T$tg %>%
    dplyr::filter(season == s, week < w) %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(team_last_wk = max(week), .groups = "drop")
  absent <- T$pg %>%
    dplyr::filter(season == s, week < w, played) %>%
    dplyr::arrange(week) %>%
    dplyr::group_by(gsis_id) %>%
    dplyr::summarise(team_now = dplyr::last(team), last_wk = max(week), .groups = "drop") %>%
    dplyr::anti_join(roster_live, by = "gsis_id") %>%
    dplyr::left_join(team_last_wk, by = c("team_now" = "team")) %>%
    dplyr::mutate(available = !is.na(team_last_wk) & last_wk == team_last_wk &
                    team_now %in% c(sch$home, sch$away))
  n_rescued <- sum(absent$available)
  message(glue("    {n_rescued} player(s) outside the R/59 pool counted available ",
               "(played in their team's most recent game)"))
  roster_live <- dplyr::bind_rows(roster_live,
                                  absent %>% dplyr::select(gsis_id, team_now, available))

  f <- .r67_features(T, s, w, roster_live, gi)
  X <- .r67_predict(f, P) %>%
    .r67_eligible() %>%
    dplyr::left_join(bands %>% dplyr::select(gsis_id = nfl_gsis_id, player_name, injury_note),
                     by = "gsis_id") %>%
    dplyr::left_join(ids$pos %>% dplyr::filter(season == s) %>%
                       dplyr::select(gsis_id, name_fb = name),
                     by = "gsis_id") %>%
    dplyr::mutate(player_name = dplyr::coalesce(player_name, name_fb),
                  injury_note = dplyr::if_else(is.na(injury_note) & available,
                                               "not in R/59 pool", injury_note)) %>%
    dplyr::select(-name_fb)

  sheet <- X %>%
    dplyr::filter(elig) %>%
    dplyr::rename(team = team_now) %>%
    dplyr::mutate(schema_tag = R67_SCHEMA, built_at = format(Sys.time(), "%Y-%m-%d %H:%M"))

  # STEP 6: simulation
  message(glue("\nSTEP 6/7: Simulating {nrow(sheet)} players x {nsim} draws"))
  probs <- .r67_sim_probs(sheet, P, nsim)
  withheld <- cal$verdict$market[!cal$verdict$pass]
  probs <- probs %>%
    dplyr::mutate(withheld = market %in% withheld,
                  p = dplyr::if_else(withheld, NA_real_, p)) %>%
    dplyr::left_join(sheet %>% dplyr::select(gsis_id, player_name, team, pos), by = "gsis_id")

  # STEP 7: calls, outputs
  message("\nSTEP 7/7: Calls and outputs")
  games_long <- dplyr::bind_rows(
    sch %>% dplyr::transmute(team = home, opp = away),
    sch %>% dplyr::transmute(team = away, opp = home))
  calls <- .r67_calls(T, s, w, games_long, persist)
  epa_rank <- T$epa_g %>%
    dplyr::filter(season == s, week < w) %>%
    dplyr::group_by(team, side) %>%
    dplyr::summarise(v = sum(num) / sum(den), .groups = "drop") %>%
    dplyr::group_by(side) %>%
    dplyr::mutate(rk = if (side[1] == "off") rank(-v, ties.method = "min")
                  else rank(v, ties.method = "min")) %>%
    dplyr::ungroup()

  tag <- sprintf("w%02d", w)
  p_sheet <- file.path(R67_CACHE_DIR, glue("s3_r67_prop_sheet_{tag}.csv"))
  p_probs <- file.path(R67_CACHE_DIR, glue("s3_r67_prop_probs_{tag}.csv"))
  p_calls <- file.path(R67_CACHE_DIR, glue("s3_r67_calls_{tag}.csv"))
  p_md    <- file.path(R67_CACHE_DIR, glue("s3_r67_prop_sheet_{tag}.md"))
  readr::write_csv(sheet %>% dplyr::select(
    season, week, gsis_id, player_name, team, opp, pos, injury_note, n_g,
    margin, total, line_used, tsh_f, csh_f, att_sh, team_tgt_hat, team_car_hat, team_att_hat,
    E_tgt, E_car, E_att, catch_hat, ypr_hat, ypc_hat, comp_hat, ypcomp_hat,
    dr_catch, dr_ypr, dr_ypc, dr_comp, dr_ypcomp,
    exp_rec_nm, exp_rec, exp_rec_yards_nm, exp_rec_yards, exp_rush_yards_nm, exp_rush_yards,
    exp_pass_yards_nm, exp_pass_yards, schema_tag, built_at), p_sheet)
  readr::write_csv(probs, p_probs)
  readr::write_csv(calls, p_calls)
  .r67_markdown(sheet, probs, calls, sch, epa_rank, cal$verdict, s, w, p_md)
  .r67_ledger_init(file.path(R67_CACHE_DIR, "prop_ledger.csv"))

  # Console summary
  message(glue("\n{strrep('=', 70)}\nSUMMARY\n{strrep('=', 70)}"))
  message("Fitted constants (live, training ", paste(live_train, collapse = "-"), "):")
  fl <- unlist(P$fit_log)
  message(paste0("  ", names(fl), " = ", signif(fl, 4), collapse = "\n"))
  message(glue("  vacated share rule: {if (P$redistribute) 'redistribute' else 'leave unallocated'}"))
  message("  dispersion theta (Inf = Poisson): ",
          paste0(names(P$theta), "=", signif(unlist(P$theta), 3), collapse = ", "))
  message("\nTeam volume models (with line):")
  for (nm in c("tgt", "att", "car")) {
    cf <- stats::coef(P$lm[[nm]])
    message(glue("  {nm}: ", paste0(names(cf), "=", signif(cf, 3), collapse = ", ")))
  }
  hl <- unlist(cal$params_fit_log)
  hl <- hl[grepl("^(vol_scale|qb_mix)", names(hl))]
  if (length(hl)) {
    message("\nHoldout-fit (training only) scale factors and QB mixture:")
    message(paste0("  ", names(hl), " = ", signif(hl, 4), collapse = "\n"))
  }
  message(glue("\nCalibration gate, holdout {cal$eval_season} ({cal$n_player_weeks} player-weeks):"))
  print(as.data.frame(cal$verdict))
  message("Holdout matchup value (MSE with vs without matchup):")
  print(as.data.frame(cal$matchup_value))
  if (!is.null(cal$comp_bias)) {
    message("\nHoldout diagnostics: component bias by position (model vs actual):")
    print(as.data.frame(cal$comp_bias %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))))
    message("Holdout diagnostics: spread per market (spread_ratio > 1 = simulated tails too wide):")
    print(as.data.frame(cal$spread %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))))
    message("Holdout diagnostics: QB attempts, full game (>= 90% of team attempts) vs not:")
    print(as.data.frame(cal$qb_att %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))))
    d_path <- file.path(R67_CACHE_DIR, glue("s3_r67_holdout_diag_{cal$eval_season}.csv"))
    r_path <- file.path(R67_CACHE_DIR, glue("s3_r67_holdout_rows_{cal$eval_season}.csv"))
    readr::write_csv(dplyr::bind_rows(
      cal$bins %>% dplyr::mutate(table = "bins", bin = as.character(bin)),
      cal$spread %>% dplyr::mutate(table = "spread"),
      cal$comp_bias %>% dplyr::mutate(table = "comp_bias"),
      cal$qb_att %>% dplyr::mutate(table = "qb_att")), d_path)
    readr::write_csv(cal$rows, r_path)
    message(glue("  diagnostics written: {basename(d_path)}, {basename(r_path)}"))
  }
  message(glue("\nPersistence gate ({paste(live_train, collapse = '-')}):"))
  print(as.data.frame(persist$detail %>% dplyr::mutate(r = round(r, 3), lower = round(lower, 3))))
  message(glue("\nPlayers on sheet: {nrow(sheet)} | lines used: {sum(sheet$line_used)} | ",
               "withheld markets: {if (length(withheld)) paste(withheld, collapse = ', ') else 'none'}"))
  message(glue("Wrote:\n  {p_sheet}\n  {p_probs}\n  {p_calls}\n  {p_md}"))

  invisible(list(sheet = sheet, probs = probs, calls = calls, params = P,
                 calibration = cal, persistence = persist))
}
