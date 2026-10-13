# ==============================================================================
# R/46  VALUE BOARD -- VORP vs market ADP (FantasyPros redraft + DK best ball)
# ==============================================================================
#
# PURPOSE
# -------
# Presentation layer on top of the R/33 VORP board. For every player it lines up
# where WE rank them (VORP) against where the MARKET drafts them, from two ADP
# sources:
#   fp_adp  -- FantasyPros redraft PPR consensus, already pulled by R/29
#   dk_adp  -- DraftKings best-ball ADP, from the DK pre-draft rankings CSV export
# and surfaces the value gap (ADP minus our rank; positive = the market lets him
# fall past where we'd take him = a target) plus the FantasyPros consensus delta
# (our projected PPG minus their consensus projection).
#
# This is a VIEW. It changes no projection and writes no cache. It reads the
# fresh R/33 rankings, joins the FantasyPros fields straight from the R/29 CSV
# (authoritative, in case R/32/R/33 dropped them), and crosswalks the DK CSV to
# the board by normalized name + position.
#
# CAVEAT ON QB: vorp overall_rank and ADP are both overall draft position, but
# in a 1-QB league VORP pushes QBs far down while ADP drafts elite QBs early, so
# QB value gaps read very negative by construction. Read RB/WR/TE for clean value
# signal, or filter with position.
# ==============================================================================

library(dplyr)
library(readr)
library(glue)
library(here)

# Mirror of R/31 .normalize_name_for_match_r31 so the DK crosswalk matches the
# board exactly the way the rest of the pipeline matches names.
.vb_normalize_name <- function(name_vec) {
  out <- tolower(as.character(name_vec))
  out <- gsub("[.']", "", out)
  out <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b", "", out, ignore.case = TRUE)
  out <- gsub("[^a-z ]", "", out)
  out <- gsub("\\s+", " ", out)
  trimws(out)
}


# ------------------------------------------------------------------------------
# .vb_latest_dk_adp
# ------------------------------------------------------------------------------

# ADP VINTAGE FIX.
#
# build_value_board() and run_value_boards() both defaulted dk_csv_path to a
# hardcoded "DkPreDraftRankings20260713.csv". Every rerun after July 13 silently
# reused July 13 ADP unless the path was passed by hand, and nothing in the
# console said so, so a stale board was indistinguishable from a fresh one.
#
# The default now resolves the newest dated export on disk and ALWAYS prints the
# file it picked and that file's date. The vintage is a stated fact in console
# output rather than an assumption. An explicit dk_csv_path still wins.
#
# Filenames must carry an 8-digit YYYYMMDD stamp. Sorting is on the parsed date,
# not on the string, so a malformed stamp is dropped rather than mis-ordered.
.VB_DK_ADP_PATTERN <- "^DkPreDraftRankings([0-9]{8})\\.csv$"

.vb_latest_dk_adp <- function(dir = here::here("data", "season3_cache")) {

  if (!dir.exists(dir)) {
    stop(glue(".vb_latest_dk_adp(): directory not found: {dir}"), call. = FALSE)
  }

  files <- list.files(dir, pattern = .VB_DK_ADP_PATTERN)

  if (length(files) == 0L) {
    stop(glue(
      ".vb_latest_dk_adp(): no DK ADP export matching ",
      "DkPreDraftRankings<YYYYMMDD>.csv in {dir}. ",
      "Drop a fresh export there, or pass dk_csv_path explicitly."
    ), call. = FALSE)
  }

  stamps <- sub(.VB_DK_ADP_PATTERN, "\\1", files)
  dates  <- suppressWarnings(as.Date(stamps, format = "%Y%m%d"))

  bad <- files[is.na(dates)]
  if (length(bad) > 0L) {
    message(glue("  .vb_latest_dk_adp(): ignoring {length(bad)} file(s) with an ",
                 "unparseable date stamp: {paste(bad, collapse = ', ')}"))
    files <- files[!is.na(dates)]
    dates <- dates[!is.na(dates)]
  }

  if (length(files) == 0L) {
    stop(glue(".vb_latest_dk_adp(): every candidate in {dir} had an unparseable ",
              "date stamp. Pass dk_csv_path explicitly."), call. = FALSE)
  }

  ord  <- order(dates, decreasing = TRUE)
  pick <- files[ord][1L]
  age  <- as.integer(Sys.Date() - dates[ord][1L])

  message(glue(
    "  DK ADP resolved: {pick}  (dated {format(dates[ord][1L], '%Y-%m-%d')}, ",
    "{age} day(s) old, {length(files)} candidate(s) on disk)"
  ))
  if (age > 14L) {
    message(glue("  NOTE: newest DK ADP on disk is {age} days old. ",
                 "Export a fresh one before trusting value gaps."))
  }

  file.path(dir, pick)
}


# ------------------------------------------------------------------------------
# load_dk_adp
# ------------------------------------------------------------------------------

#' Load the DraftKings best-ball ADP export.
#'
#' The DK pre-draft rankings CSV has columns ID, Name, Position, ADP, Team (plus
#' an instructions column that is ignored). ADP is a fractional overall pick.
#'
#' @param dk_csv_path Path to the DK export.
#' @param verbose Logical.
#' @return tibble(name_norm, position, team_dk, dk_adp).
load_dk_adp <- function(dk_csv_path, verbose = TRUE) {
  raw <- readr::read_csv(dk_csv_path, show_col_types = FALSE,
                         name_repair = "unique")
  need <- c("Name", "Position", "ADP")
  miss <- setdiff(need, names(raw))
  if (length(miss) > 0L) {
    stop(glue("load_dk_adp(): missing columns {paste(miss, collapse = ', ')}. ",
              "Present: {paste(head(names(raw), 12), collapse = ', ')}"))
  }

  dk <- raw %>%
    dplyr::transmute(
      name_norm = .vb_normalize_name(.data$Name),
      position  = toupper(trimws(as.character(.data$Position))),
      team_dk   = if ("Team" %in% names(raw)) toupper(trimws(as.character(.data$Team)))
                  else NA_character_,
      dk_adp    = suppressWarnings(as.numeric(.data$ADP))
    ) %>%
    dplyr::filter(!is.na(.data$dk_adp), nchar(.data$name_norm) > 0,
                  .data$position %in% c("QB", "RB", "WR", "TE")) %>%
    dplyr::arrange(.data$dk_adp) %>%
    # one row per name+position (keep the earliest ADP if duplicated)
    dplyr::distinct(.data$name_norm, .data$position, .keep_all = TRUE)

  if (verbose) message(glue("  load_dk_adp(): {nrow(dk)} DK best-ball ADP entries"))
  dk
}


# ------------------------------------------------------------------------------
# build_value_board
# ------------------------------------------------------------------------------

#' Join the VORP board to FantasyPros and DK ADP and compute value gaps.
#'
#' @param rankings Output of compute_vorp_rankings() (R/33). Needs player_uid,
#'   nfl_gsis_id, player_name, position, team, r32_posterior_mu, adjusted_vorp,
#'   overall_rank. player_uid is the identity key; nfl_gsis_id is NA for the
#'   draftable rookie cohort.
#' @param r29_csv_path R/29 projections CSV (source of the FantasyPros fields).
#' @param dk_csv_path DK best-ball ADP export. Defaults to the newest dated
#'   export in data/season3_cache, resolved and reported at call time.
#' @param verbose Logical.
#'
#' @return tibble sorted by our VORP rank with: player_uid, player_name,
#'   position, team, r32_posterior_mu, vorp_base, adjusted_vorp, vorp_rank,
#'   fp_adp, dk_adp, value_vs_fp, value_vs_dk, consensus_delta.
#'   Value = ADP - our rank (positive = value).
build_value_board <- function(rankings,
                              r29_csv_path = here::here(
                                "data", "season2_cache",
                                "s2_week15_player_projections.csv"),
                              dk_csv_path  = .vb_latest_dk_adp(),
                              season = 2026L,
                              verbose = TRUE) {

  # WAVE A2 IDENTITY FIX.
  #
  # consensus_delta comes from the R/29 CSV. This previously keyed on
  # nfl_gsis_id, which is NA for the draftable rookie cohort. distinct() then
  # collapsed all id-less players to one row and the left_join matched NA to
  # NA, handing every id-less rookie the same consensus_delta. Keyed on
  # player_uid the cohort is addressable and each row gets its own value.
  r29_raw <- readr::read_csv(r29_csv_path, show_col_types = FALSE)
  if (!"player_uid" %in% names(r29_raw)) {
    stop(glue(
      "build_value_board(): R/29 CSV at {r29_csv_path} carries no player_uid. ",
      "It predates the Wave A2 identity fix. Regenerate R/29 with ",
      "run_projection_engine() rather than building a board on this file."
    ), call. = FALSE)
  }
  fp_consensus <- r29_raw %>%
    dplyr::select(dplyr::any_of(c("player_uid", "consensus_delta"))) %>%
    dplyr::distinct(.data$player_uid, .keep_all = TRUE)

  # ---------------------------------------------------------------------------
  # no_history flag  [2026-09-02]
  # ---------------------------------------------------------------------------
  # WHAT IT MEANS. TRUE = R/29 built this player's prior WITHOUT any NFL
  # history. It is a statement about the model's inputs, not about the player.
  #
  # WHY IT EXISTS. On dk6_week15, 44 players carried a fallback prior_source
  # with n_nfl_seasons = 0 and hist_mean_used = NA. For all of them
  # prior_mu_pre_age == prior_mu == r29_posterior_mu exactly: no aging
  # adjustment, no posterior update, the fallback number passes through
  # untouched. prior_sigma is a position constant (WR 3.904, RB 3.999,
  # TE 2.618), so their intervals are not player-specific either.
  #
  # The resulting projections are not small. Laquon Treadwell came out at
  # 10.946 R/29 PPG against an R/31 volume line of 1.030, landing 78th among
  # skill players. Phillip Dorsett 8.062 against 0.887, landing 150th. The
  # rest of the cohort sits past rank 200.
  #
  # WHY ALL THREE CONDITIONS. prior_source alone is a label, and a label is a
  # proxy for the property rather than the property. n_nfl_seasons == 0 and
  # is.na(hist_mean_used) are direct measurements of "no history was used".
  # Conjoining them makes the column mean what its name says. Verified on
  # 2026-09-02: 0 of the flagged rows carry a non-NA hist_mean_used.
  #
  # KNOWN LIMIT. This does NOT say the player is retired or off a roster. A
  # roster-based flag was designed and REJECTED on 2026-09-02: Treadwell is
  # listed ACT on IND in load_rosters(2026), so the roster source would have
  # marked the single worst case on the board as clean, while flagging 75
  # legitimate 2026 rookies who are absent from the roster file under a
  # synthetic id. The measurement did not match the claim, so it was not
  # shipped. Revive only if a source is found that separates retired players
  # from not-yet-listed rookies.
  #
  # Rookies do NOT trip this flag: they carry translation_capped_rookie or
  # translation_active, not the two fallback sources.
  .NH_SRC <- c("phaseout_no_history_fallback", "calibrated_fallback")
  .nh_need <- c("prior_source", "n_nfl_seasons", "hist_mean_used")
  .nh_missing <- setdiff(.nh_need, names(r29_raw))
  if (length(.nh_missing) > 0L) {
    stop(glue(
      "build_value_board(): R/29 CSV at {r29_csv_path} carries no ",
      "{paste(.nh_missing, collapse = ', ')}. The no_history flag cannot be ",
      "derived. Regenerate R/29 with run_projection_engine()."
    ), call. = FALSE)
  }
  no_hist_tbl <- r29_raw %>%
    dplyr::transmute(
      player_uid = .data$player_uid,
      # isTRUE-equivalent: any NA in the inputs resolves to FALSE rather than
      # leaving three-valued logic in a column that is rendered as a boolean.
      no_history = dplyr::coalesce(
        .data$prior_source %in% .NH_SRC &
          .data$n_nfl_seasons == 0 &
          is.na(.data$hist_mean_used),
        FALSE
      )
    ) %>%
    dplyr::distinct(.data$player_uid, .keep_all = TRUE)

  if (verbose) {
    message(glue("  no_history: {sum(no_hist_tbl$no_history)} of ",
                 "{nrow(no_hist_tbl)} player(s) have no NFL history behind ",
                 "their R/29 prior"))
  }

  # FP ADP by NAME, not gsis: R/29's ADP join keys on gsis, so rookies with a
  # synthetic gsis come back NA. load_adp_fantasypros() is name-keyed and cached,
  # so crosswalking it by name+position (same as DK) recovers the rookies.
  fp_adp_tbl <- tryCatch(
    load_adp_fantasypros(season = season, scoring = "PPR"),
    error = function(e) { message(glue("  FP ADP load failed: {e$message}")); NULL })
  if (!is.null(fp_adp_tbl)) {
    fp_adp_tbl <- fp_adp_tbl %>%
      dplyr::transmute(name_norm = .vb_normalize_name(.data$player_name),
                       position  = toupper(.data$pos),
                       fp_adp    = .data$adp_rank) %>%
      dplyr::filter(!is.na(.data$fp_adp), nchar(.data$name_norm) > 0) %>%
      dplyr::arrange(.data$fp_adp) %>%
      dplyr::distinct(.data$name_norm, .data$position, .keep_all = TRUE)
  } else {
    fp_adp_tbl <- tibble::tibble(name_norm = character(), position = character(),
                                 fp_adp = numeric())
  }

  dk <- load_dk_adp(dk_csv_path, verbose = verbose)

  if (!"vorp_base" %in% names(rankings)) {
    stop("build_value_board(): rankings carry no vorp_base. Regenerate the ",
         "R/33 VORP rankings; adjusted_vorp alone cannot be decomposed.",
         call. = FALSE)
  }

  if (!"player_uid" %in% names(rankings)) {
    stop(
      "build_value_board(): rankings carry no player_uid. Regenerate the R/33 ",
      "VORP rankings -- nfl_gsis_id is NA for draftable rookies and joining ",
      "on it matches NA to NA.",
      call. = FALSE
    )
  }

  # [2026-08-13] Intervals are now carried onto the board for assign_tiers().
  # Checked explicitly: a transmute of a missing column errors with a bare
  # "object not found", and the actionable cause (rankings predate the R/32
  # interval rebuild) would not be in the message.
  .iv <- c("r32_projection_lower_80", "r32_projection_upper_80")
  .iv_missing <- setdiff(.iv, names(rankings))
  if (length(.iv_missing) > 0L) {
    stop(glue("build_value_board(): rankings carry no ",
              "{paste(.iv_missing, collapse = ', ')}. Regenerate the R/33 VORP ",
              "rankings from a current R/32 run; tiering needs the 80% ",
              "posterior bounds."), call. = FALSE)
  }

  board <- rankings %>%
    dplyr::mutate(name_norm = .vb_normalize_name(.data$player_name)) %>%
    dplyr::left_join(fp_consensus, by = "player_uid") %>%
    dplyr::left_join(no_hist_tbl, by = "player_uid") %>%
    dplyr::left_join(fp_adp_tbl, by = c("name_norm", "position")) %>%
    dplyr::left_join(dplyr::select(dk, name_norm, position, dk_adp),
                     by = c("name_norm", "position"))

  if (verbose) {
    fp_hit <- mean(!is.na(board$fp_adp))
    dk_hit <- mean(!is.na(board$dk_adp))
    message(glue("  value board: {nrow(board)} players | ",
                 "FP ADP matched {round(100*fp_hit)}% | ",
                 "DK ADP matched {round(100*dk_hit)}%"))
  }

  board %>%
    dplyr::transmute(
      player_uid = .data$player_uid,
      player_name, position, team,
      r32_posterior_mu = round(.data$r32_posterior_mu, 1),
      # vorp_base is VORP BEFORE the boom/bust/ceiling adjustments
      # (adjusted_vorp = vorp_base + boom_modifier + bust_modifier). Carrying
      # both lets any downstream analysis test whether a result is driven by
      # replacement-level scarcity or by the five ungated weights R/33 applies.
      # Without this column that test is impossible from a value board alone.
      vorp_base        = round(.data$vorp_base, 2),
      adjusted_vorp    = round(.data$adjusted_vorp, 2),
      # [2026-08-13] The 80% posterior bounds are carried so tiering and any
      # risk view can run off a board CSV alone. R/32 rebuilds these from
      # r32_posterior_mu and r32_posterior_sigma under BLEND_SIGMA_VOL_MODE, so
      # they are consistent with the mu ranked on above -- do NOT substitute
      # R/29's r29_projection_* bounds here, which describe a different mu.
      # NOT rounded: assign_tiers() recovers each player's posterior sigma from
      # the bound spacing, and rounding to 1dp would inject ~4% sigma error.
      # NOTE the lower bound is floored at 0 by R/32 (pmax at ~line 1631), which
      # TRUNCATES the recovered sigma for deep players. assign_tiers() recovers
      # sigma from the upper side alone whenever the lower bound sits at 0.
      proj_lower_80    = .data$r32_projection_lower_80,
      proj_upper_80    = .data$r32_projection_upper_80,
      vorp_rank        = .data$overall_rank,
      fp_adp           = .data$fp_adp,
      dk_adp           = round(.data$dk_adp, 1),
      value_vs_fp      = .data$fp_adp - .data$overall_rank,
      value_vs_dk      = round(.data$dk_adp - .data$overall_rank, 1),
      consensus_delta  = round(.data$consensus_delta, 1),
      # A player on the board but absent from the R/29 CSV joins to NA here.
      # That is not the same as "has history", so it coalesces to FALSE and
      # the row is left unmarked rather than being asserted either way.
      no_history       = dplyr::coalesce(.data$no_history, FALSE)
    ) %>%
    dplyr::arrange(.data$vorp_rank)
}


# ------------------------------------------------------------------------------
# assign_tiers
# ------------------------------------------------------------------------------

# TIERS -- 80% test on the DIFFERENCE between two players, leader-anchored.
# [2026-08-13]
#
# THE CLAIM. Two players share a tier when the model cannot tell them apart at
# the 80% level. That is a statement about the distribution of mu_A - mu_B, not
# about whether their two marginal intervals happen to touch.
#
# WHAT WAS BUILT FIRST, AND WHY IT WAS WRONG. The first version merged players
# whose 80% intervals OVERLAPPED. Overlap fails only once the gap exceeds
# z*(sigma_A + sigma_B); the difference is significant once the gap exceeds
# z*sqrt(sigma_A^2 + sigma_B^2). For equal sigmas that is 2.00 sigma against
# 1.41 sigma, so an "80%" overlap rule behaves like a ~97% test and merges
# players it has no business merging. On the 2026-08-13 Gridiron Guzzlers board
# it returned three tiers at every position, with 120 of 248 WRs in one tier.
# The estimand was wrong, not the level.
#
# READ THIS BEFORE CHANGING ANYTHING HERE. The correction was made AFTER seeing
# a result that was unusable, and it produces more tiers. That is the exact
# shape of motivated reasoning, and it was accepted only because the argument
# is about which quantity answers the question, with the confidence level held
# fixed at 80% throughout. If a future change to this function is justified by
# the tiers looking better rather than by the estimand being wrong, it is the
# thing this comment exists to stop.
#
# NO FREE PARAMETER. z = 1.2816 is the 80% two-sided constant, the same one
# R/32 used to build the bounds. It is not tunable. Lowering the level to make
# tiers narrower reintroduces a fitted constant through the back door.
#
# LINKAGE. Significance is not transitive. Chaining single-linkage on a dense
# board collapses a position into one tier, so this anchors on the TIER LEADER:
# a new tier opens when a player is distinguishable FROM THE LEADER, and that
# player becomes the new leader. Membership means "not distinguishable from the
# best player in this tier," which is the claim a drafter acts on.
#
# INDEPENDENCE CAVEAT, UNMEASURED. sqrt(sigma_A^2 + sigma_B^2) assumes the two
# players' errors are independent. Teammates and shared-environment players
# have positively correlated errors, which SHRINKS the true difference variance
# and would yield more tiers still. This rule is therefore conservative in a
# known direction. Quantifying it needs the R/50 copula and is not done here.
#
# SIGMA RECOVERY AND THE FLOOR. sigma is recovered from the bound spacing:
# full width / 2.5631. R/32 clamps the lower bound at 0 (pmax, ~line 1631), so
# for any player at the floor the width is TRUNCATED and that recovery would
# understate sigma, manufacturing significance and inflating tier counts at the
# bottom of the board. For those players sigma comes from the upper side alone,
# (upper - mu) / 1.2816, which is not truncated. The floored population is
# counted and reported.
#
# WHY THIS ONLY BECAME HONEST AFTER 2026-07-30. Before the R2 fix, veterans'
# posterior_sigma was overwritten and ran roughly 5x too wide. Every test would
# have come back indistinguishable. This function depends on that fix.
#
# WITHIN-POSITION EQUIVALENCE. vorp_base is r32_posterior_mu minus a constant
# replacement_ppg per position, so the mu gap and the vorp_base gap are the
# same number within a position. vorp_base is used for the gap because the
# board rounds mu to 1dp and vorp_base carries 2dp.

.TIER_Z80 <- stats::qnorm(0.90)   # 1.2816; half-width of an 80% interval

#' Assign tiers by an 80% significance test on the difference, within position.
#'
#' @param board Output of build_value_board(), carrying proj_lower_80,
#'   proj_upper_80, position, vorp_base, and r32_posterior_mu.
#' @param within c("position","overall"). Tiers are computed within position by
#'   default. "overall" tiers the whole board on the vorp scale, which is only
#'   meaningful because vorp_base is already replacement-adjusted.
#' @param verbose Print the tier-size distribution and the floored-sigma count.
#' @return board with tier, tier_size, and sigma_recovered columns added,
#'   in the order supplied.
assign_tiers <- function(board, within = c("position", "overall"),
                         verbose = TRUE) {

  within <- match.arg(within)

  need <- c("proj_lower_80", "proj_upper_80", "position", "vorp_base",
            "r32_posterior_mu")
  miss <- setdiff(need, names(board))
  if (length(miss) > 0L) {
    stop(glue("assign_tiers(): board is missing {paste(miss, collapse = ', ')}. ",
              "Rebuild it with a current build_value_board(); boards written ",
              "before 2026-08-13 do not carry the interval bounds."),
         call. = FALSE)
  }

  z <- .TIER_Z80

  # Recover sigma. Full width when the lower bound is off the floor; upper side
  # alone when it is on the floor, because the floor truncates the width.
  floored <- !is.na(board$proj_lower_80) & board$proj_lower_80 <= 0
  sigma <- dplyr::if_else(
    floored,
    (board$proj_upper_80 - board$r32_posterior_mu) / z,
    (board$proj_upper_80 - board$proj_lower_80) / (2 * z)
  )
  # A non-positive or NA sigma cannot support a test. Those players are tiered
  # as NA rather than being silently declared significant against everything.
  sigma[!is.finite(sigma) | sigma <= 0] <- NA_real_

  n_na <- sum(is.na(sigma))
  if (n_na > 0L && verbose) {
    message(glue("  assign_tiers: {n_na} of {nrow(board)} players have no ",
                 "usable sigma and get tier = NA."))
  }

  # Leader-anchored sweep over one already-sorted group.
  .tier_one_group <- function(v, s) {
    n <- length(v)
    out <- rep(NA_integer_, n)
    if (n == 0L) return(out)
    cur <- 1L
    lead_v <- NA_real_
    lead_s <- NA_real_
    for (i in seq_len(n)) {
      if (is.na(v[i]) || is.na(s[i])) next
      if (is.na(lead_v)) {
        lead_v <- v[i]; lead_s <- s[i]; out[i] <- cur
        next
      }
      # Distinguishable from the leader at 80% on the difference?
      crit <- z * sqrt(lead_s^2 + s[i]^2)
      if ((lead_v - v[i]) > crit) {
        cur <- cur + 1L
        lead_v <- v[i]; lead_s <- s[i]
      }
      out[i] <- cur
    }
    out
  }

  grp <- if (within == "position") board$position else rep("ALL", nrow(board))

  tiered <- board %>%
    dplyr::mutate(sigma_recovered = round(sigma, 3),
                  .sig = sigma, .grp = grp, .row = dplyr::row_number()) %>%
    dplyr::arrange(.data$.grp, dplyr::desc(.data$vorp_base)) %>%
    dplyr::group_by(.data$.grp) %>%
    dplyr::mutate(tier = .tier_one_group(.data$vorp_base, .data$.sig)) %>%
    dplyr::group_by(.data$.grp, .data$tier) %>%
    dplyr::mutate(tier_size = dplyr::if_else(is.na(.data$tier),
                                             NA_integer_, dplyr::n())) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$.row) %>%
    dplyr::select(-.sig, -.grp, -.row)

  if (verbose) {
    message(glue("  assign_tiers: within = {within}, 80% test on the ",
                 "difference (z = {round(z, 4)})"))
    summ <- tiered %>%
      dplyr::filter(!is.na(.data$tier)) %>%
      dplyr::group_by(.data$position) %>%
      dplyr::summarise(n_players = dplyr::n(),
                       n_tiers   = dplyr::n_distinct(.data$tier),
                       largest   = max(.data$tier_size),
                       median_sz = stats::median(.data$tier_size),
                       .groups   = "drop")
    print(as.data.frame(summ))
    message(glue("  sigma from upper side only (lower bound on the 0 floor): ",
                 "{sum(floored, na.rm = TRUE)} player(s)"))
    if (nrow(summ) > 0L && max(summ$largest) > 20L) {
      message("  NOTE a tier above 20 players is a statement about sigma, not ",
              "a tuning target. Report it; do not lower the level.")
    }
  }

  tiered
}


# ------------------------------------------------------------------------------
# value_targets / value_reaches  (convenience views)
# ------------------------------------------------------------------------------

#' Biggest values (market drafts them well after our rank), RB/WR/TE by default.
#' @param board Output of build_value_board().
#' @param source c("dk","fp"). @param n rows. @param positions to include.
value_targets <- function(board, source = c("dk", "fp"), n = 25L,
                          positions = c("RB", "WR", "TE")) {
  source <- match.arg(source)
  col <- if (source == "dk") "value_vs_dk" else "value_vs_fp"
  board %>%
    dplyr::filter(.data$position %in% positions, !is.na(.data[[col]])) %>%
    dplyr::arrange(dplyr::desc(.data[[col]])) %>%
    utils::head(n)
}

#' Biggest reaches (market drafts them well ahead of our rank).
value_reaches <- function(board, source = c("dk", "fp"), n = 25L,
                          positions = c("RB", "WR", "TE")) {
  source <- match.arg(source)
  col <- if (source == "dk") "value_vs_dk" else "value_vs_fp"
  board %>%
    dplyr::filter(.data$position %in% positions, !is.na(.data[[col]])) %>%
    dplyr::arrange(.data[[col]]) %>%
    utils::head(n)
}


# ------------------------------------------------------------------------------
# dk_best_ball_config  (verified DK Best Ball preset for R/33)
# ------------------------------------------------------------------------------

# DK Best Ball scoring (verified 2026-07-13 vs DK DFS rules): full PPR, 0.04/pass
# yd, 4/pass TD, -1 INT, -1 fumble, 0.1 rush+rec yd, 6 rush+rec TD, +3 at 100
# rush yds, +3 at 100 rec yds, +3 at 300 pass yds. R/17 schema.
# DK_BEST_BALL_SCORING is defined ONCE, in R/17. [2026-07-16]
# Was dual-defined here and at R/45:61; source order silently picked a winner.
# Do not redefine. Derive variants with modifyList(DK_BEST_BALL_SCORING, ...).
if (!exists("DK_BEST_BALL_SCORING")) source(here::here("R", "17_extended_scoring.R"))

#' DraftKings Best Ball league config, verified 2026-07-13 against DK rules.
#' Weekly lineup: 1 QB, 2 RB, 3 WR, 1 TE, 1 FLEX (RB/WR/TE); 20-man roster (the
#' bench depth does not change starter replacement levels). Scoring is full PPR
#' with the 100/300-yard bonuses; those bonuses shape the board through the
#' best_ball CEILING weighting in R/33 (boom_weight 4.0, ceiling_factor 0.3),
#' which is best ball's proxy for upside, not literal per-projection bonus points
#' (R/29 cannot price play-level bonuses at the aggregate prior level).
#'
#' num_teams defaults to 12 (DK Millionaire pod size). Requires R/33 sourced.
#'
#' @return a league_config object for compute_vorp_rankings().
dk_best_ball_config <- function(num_teams = 12L, league_name = "DK Best Ball") {
  if (!exists("build_league_config")) {
    stop("dk_best_ball_config(): source R/33 first (build_league_config not found).")
  }
  build_league_config(
    league_name = league_name,
    num_teams   = as.integer(num_teams),
    starters    = list(QB = 1L, RB = 2L, WR = 3L, TE = 1L),
    flex        = 1L,
    superflex   = 0L,
    format      = "best_ball"
  )
}


# ------------------------------------------------------------------------------
# run_value_boards  (guided, TRUE per-league scoring)
# ------------------------------------------------------------------------------

# Stable key for a scoring config so leagues with identical scoring share one
# engine run.
# Serialize one scoring value deterministically.
#
# [2026-07-16] Rewritten for nested params. The old version was
# paste(x, collapse = ","), which flattens a named list to its values and drops
# the names. long_td_tiers = list(rec = c("40"=2,"50"=4)) and
# list(rush = c("40"=2,"50"=4)) would both serialize to "2,4" and collide, so
# R/47's dedupe would run ONE engine for two different rulesets and write the
# same board to both. tiered_rec_tiers survived that only because it is a bare
# unnamed vector.
.scoring_val <- function(x) {
  if (is.null(x) || length(x) == 0L) return("")
  if (is.list(x)) {
    x <- x[order(names(x))]
    return(paste0("{", paste(names(x), vapply(x, .scoring_val, character(1)),
                             sep = ":", collapse = "|"), "}"))
  }
  if (!is.null(names(x)) && any(nzchar(names(x)))) {
    o <- order(names(x))
    return(paste(names(x)[o], x[o], sep = "+", collapse = ","))
  }
  paste(x, collapse = ",")
}

.scoring_key <- function(sc) {
  flat <- sc[order(names(sc))]
  paste(names(flat), vapply(flat, .scoring_val, character(1)),
        sep = "=", collapse = ";")
}

# R/42 resolves only the scoring fields Sleeper provides; R/29 references its full
# R/17-schema set, and a missing field reads as NULL and collapses a scoring term
# to length 0. Merge league scoring onto this complete R/17 template (neutral
# gap-fills), so league values override and every other field has a real default.
# NOTE: this is R/17's schema (per-reception key is `ppr`). It deliberately does
# NOT read a global DEFAULT_SCORING_SETTINGS, because R/29 and R/32 each define
# one with different keys (R/32 uses `rec`), and whichever was sourced last would
# otherwise poison the scoring passed to calculate_fantasy_points_ext.
# NOTE [2026-07-16] on pick6_penalty = 0.
# This template's job is to fill keys the league did NOT specify, so every
# default here must be NEUTRAL, not R/17's opinionated default. te_premium
# (R/17: TRUE), rush_att_bonus (R/17: 0.25) and use_tiered_ppr (R/17: TRUE)
# were all neutralized. pick6_penalty was not: it sat at -4, mirroring R/17.
#
# Until pass_int_td was wired into R/19 (STEP 6c) no league ever emitted
# pick6_penalty, so all 7 boards silently carried a -4 pick-six penalty that
# only 1 league (Super flex, pass_int_td = -4) actually scores. That league
# was correct by accident; the other six were not. Surfaced by the
# 10 Guys / 4th and Drunk scoring_key divergence: 10 Guys sends
# pass_int_td = 0 explicitly, 4th and Drunk omits the field, so identical
# leagues stopped hashing identically.
#
# Absence of a Sleeper field means the league does not score it. Default 0.
.SCORING_TEMPLATE <- list(
  pass_yd = 0.04, pass_td = 4, pass_int = -2, pick6_penalty = 0,
  rush_yd = 0.1, rush_td = 6, rec_yd = 0.1, rec_td = 6, ppr = 1, fumbles = -2,
  use_tiered_ppr = FALSE, te_premium = FALSE, rush_att_bonus = 0,
  first_down_points = 0, long_td_tiers = NULL,
  hundred_yard_bonus = 0, superflex_pass_td = 0, two_point_conversion = 2,
  sack_penalty = 0, tiered_rec_tiers = NULL,
  bonus_pass_yd_300 = 0, bonus_pass_yd_400 = 0,
  bonus_rec_yd_200 = 0, bonus_rush_yd_200 = 0, pass_2pt = 2
)

.complete_scoring <- function(sc) {
  merged <- utils::modifyList(.SCORING_TEMPLATE, sc)
  merged[["rec"]] <- NULL   # R/17 uses `ppr`; drop any stray Sleeper-style `rec`
  merged
}

#' Guided value-board runner with TRUE per-league scoring.
#'
#' Prompts for a Sleeper username, lets you pick leagues, resolves each league's
#' real scoring via R/42, and, deduped by scoring, RE-RUNS R/29 and R/32 under
#' that scoring (to temp files, production caches untouched) so each league's
#' board is projected on its own rules, not a shared PPR baseline. Then VORP's
#' each league on its scored board and writes one value board CSV per league.
#'
#' Requires the fresh 2026 R/30 and R/31 caches on disk (volume is scoring-
#' independent and reused across scorings). Run interactively.
#'
#' @param dk_csv_path DK best-ball ADP export for the dk_adp column. Defaults
#'   to the newest dated export in data/season3_cache; the resolved filename and
#'   its date are printed on every run.
#' @param out_dir Output directory for per-league CSVs.
#' @param season Season to project. Default 2026.
#' @return (invisibly) a named list of value boards, one per league.
run_value_boards <- function(
    dk_csv_path = .vb_latest_dk_adp(),
    out_dir     = here::here("data", "season3_cache"),
    season      = 2026L) {

  need <- c("resolve_league_scoring", "get_user_leagues", "build_config_from_sleeper",
            "compute_vorp_rankings", "run_projection_engine", "reconcile_projections")
  miss <- need[!vapply(need, exists, logical(1))]
  if (length(miss) > 0L) {
    stop(glue("run_value_boards(): missing {paste(miss, collapse = ', ')}. ",
              "Source R/29, R/32, R/33, R/42 first."))
  }
  if (!interactive()) message("run_value_boards(): prompts you; run interactively.")

  # ---- gather leagues ----
  leagues <- list()   # each: list(name, config, scoring)

  username <- trimws(readline("Sleeper username (blank to skip Sleeper): "))
  if (nzchar(username)) {
    lg <- tryCatch(get_user_leagues(username, season = season),
                   error = function(e) { message(glue("  Sleeper error: {e$message}")); NULL })
    if (!is.null(lg) && nrow(lg) > 0L) {
      picks <- utils::select.list(lg$name, multiple = TRUE,
                                  title = "Select league(s) to build")
      for (nm in picks) {
        lid <- lg$league_id[match(nm, lg$name)]
        cfg <- tryCatch(build_config_from_sleeper(lid),
                        error = function(e) { message(glue("  Skip {nm}: {e$message}")); NULL })
        sc  <- tryCatch(resolve_league_scoring(lid, verbose = FALSE)$offense,
                        error = function(e) { message(glue("  Scoring skip {nm}: {e$message}")); NULL })
        if (!is.null(cfg) && !is.null(sc)) {
          leagues[[cfg$league_name]] <- list(name = cfg$league_name, config = cfg,
                                             scoring = .complete_scoring(sc))
        }
      }
    } else message("  No Sleeper leagues found.")
  }

  if (tolower(trimws(readline("Include DK Best Ball board? [Y/n]: "))) %in% c("", "y", "yes")) {
    leagues[["DK Best Ball"]] <- list(name = "DK Best Ball",
                                      config = dk_best_ball_config(),
                                      scoring = .complete_scoring(DK_BEST_BALL_SCORING))
  }

  if (length(leagues) == 0L) { message("Nothing selected."); return(invisible(list())) }

  # ---- confirm ----
  message("\nLeagues to build (each projected on its own scoring):")
  for (L in leagues) {
    st <- L$config$starters
    message(glue("  - {L$name}: format={L$config$format}, teams={L$config$num_teams}, ",
                 "lineup QB{st$QB}/RB{st$RB}/WR{st$WR}/TE{st$TE}+{L$config$flex}FLEX | ",
                 "ppr={L$scoring$ppr %||% NA}, pass_td={L$scoring$pass_td %||% NA}, ",
                 "te_prem={L$scoring$te_premium %||% NA}"))
  }
  if (!(tolower(trimws(readline("Proceed? [Y/n]: "))) %in% c("", "y", "yes"))) {
    message("Aborted."); return(invisible(list()))
  }

  .build_boards(leagues = leagues, dk_csv_path = dk_csv_path,
                out_dir = out_dir, season = season)
}


# ------------------------------------------------------------------------------
# .build_boards
# ------------------------------------------------------------------------------

#' Build one value board per league, one engine run per distinct scoring.
#'
#' This is the single definition of the build loop. run_value_boards() gathers
#' its league list by prompting; batch_value_boards() (R/47) gathers the same
#' list from arguments. Both hand it here. The loop is deliberately NOT
#' duplicated in R/47 -- duplicated definitions are how DK_BEST_BALL_SCORING
#' and DEFAULT_SCORING_SETTINGS each ended up with two sources of truth.
#'
#' @param leagues Named list. Each element: list(name, config, scoring), where
#'   scoring has already been through .complete_scoring().
#' @param dk_csv_path DK best-ball ADP export.
#' @param out_dir Directory for the per-league CSVs and the combined file.
#' @param season Integer season.
#' @param write_combined Logical. Also write one long file across all leagues
#'   with a league_name column.
#' @param out_prefix Filename stem for the written CSVs. Change this for any
#'   run whose boards must NOT overwrite the production boards -- for example a
#'   held-config reference run. A prior baseline was lost to a same-filename
#'   overwrite; this argument exists so that cannot happen silently.
#' @return Invisible named list of board tibbles.
#' @keywords internal
.build_boards <- function(leagues,
                          dk_csv_path,
                          out_dir,
                          season,
                          write_combined = TRUE,
                          out_prefix = "value_board") {

  if (length(leagues) == 0L) {
    message("Nothing to build.")
    return(invisible(list()))
  }

  # ---- dedupe by scoring, re-run engine per distinct scoring ----
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  keys <- vapply(leagues, function(L) .scoring_key(L$scoring), character(1))
  boards <- list()

  old_out <- if (exists("OUTPUT_PROJECTIONS_PATH", envir = .GlobalEnv)) get("OUTPUT_PROJECTIONS_PATH", envir = .GlobalEnv) else NULL
  old_def <- if (exists("OUTPUT_DEF_ST_PATH", envir = .GlobalEnv)) get("OUTPUT_DEF_ST_PATH", envir = .GlobalEnv) else NULL
  on.exit({
    if (!is.null(old_out)) assign("OUTPUT_PROJECTIONS_PATH", old_out, envir = .GlobalEnv)
    if (!is.null(old_def)) assign("OUTPUT_DEF_ST_PATH", old_def, envir = .GlobalEnv)
  }, add = TRUE)

  for (k in unique(keys)) {
    group <- leagues[keys == k]
    sc    <- group[[1]]$scoring
    grp_names <- paste(vapply(group, function(g) g$name, character(1)), collapse = ", ")
    message(glue("\n=== scoring group: {grp_names} ==="))

    tmp_csv <- tempfile(fileext = ".csv")
    assign("OUTPUT_PROJECTIONS_PATH", tmp_csv, envir = .GlobalEnv)
    if (!is.null(old_def)) assign("OUTPUT_DEF_ST_PATH", tempfile(fileext = ".csv"), envir = .GlobalEnv)

    message("  running R/29 under this scoring...")
    run_projection_engine(season = season, week = 1L, scoring_settings = sc)
    message("  reconciling (R/32) under this scoring...")
    # R/29 uses `ppr`; R/32's volume-implied conversion uses `rec`. Give R/32 a
    # `rec` mirror of `ppr` so it scores receptions (without it, vi receiving = 0
    # and every pass-catcher's blended projection collapses).
    sc_r32 <- utils::modifyList(sc, list(rec = sc$ppr))
    board_sc <- reconcile_projections(r29_path = tmp_csv, scoring_settings = sc_r32,
                                      save_output = FALSE)

    for (L in group) {
      rankings <- compute_vorp_rankings(board_sc, L$config)
      vb <- build_value_board(rankings, r29_csv_path = tmp_csv,
                              dk_csv_path = dk_csv_path, season = season,
                              verbose = TRUE)
      safe  <- gsub("[^A-Za-z0-9]+", "_", L$name)
      fpath <- file.path(out_dir, glue("{out_prefix}_{safe}.csv"))
      readr::write_csv(vb, fpath)
      message(glue("  wrote {basename(fpath)}  ({nrow(vb)} players)"))
      boards[[L$name]] <- vb
    }
    unlink(tmp_csv)
  }

  # ---- combined long file ----
  if (isTRUE(write_combined) && length(boards) > 0L) {
    combined <- dplyr::bind_rows(boards, .id = "league_name") %>%
      dplyr::arrange(.data$league_name, .data$vorp_rank)
    combined_path <- file.path(out_dir,
                               glue("{out_prefix}s_all_{format(Sys.Date(), '%Y%m%d')}.csv"))
    readr::write_csv(combined, combined_path)
    message(glue("  wrote {basename(combined_path)}  ({nrow(combined)} rows, ",
                 "{dplyr::n_distinct(combined$league_name)} leagues)"))
  }

  message(glue("\nDone. {length(boards)} league board(s), ",
               "{length(unique(keys))} engine run(s), in {out_dir}."))
  invisible(boards)
}
