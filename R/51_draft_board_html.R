# ==============================================================================
# R/51  DRAFT BOARD HTML GENERATOR  (tier 1: filter, counter, pick log)
# ==============================================================================
#
# PURPOSE
# -------
# Turns one R/46 value board CSV into a single self-contained HTML file that
# opens on a phone with no server, no R session and no network. One file per
# league. The board is frozen at build time, which matches how it is used: the
# board does not change during a draft.
#
# WHAT IT IS
# ----------
# A filter, a counter and a log. It does NOT recompute VORP, and it does not
# compute marginal value. Removing a player from the pool does not move
# replacement level, because replacement level is rank-based and already assumes
# every starter-rank player gets drafted. Anything that claimed to "update VORP"
# on a checkbox would be theatre.
#
# What it does do:
#   - search and position filter over the board
#   - mark a player TAKEN or MINE, with per-player undo and a full reset
#   - live roster counter against the league's starting requirements
#   - LINEUP FIT: what a player adds to your best starting lineup GIVEN what you
#     already hold, recomputed on every pick
#   - LIVE DRAFT SYNC: polls the Sleeper draft and marks picks automatically
#   - a PICK LOG recording pick order, player and whether the pick was yours
#
# THE PICK LOG IS THE POINT. It cannot be reconstructed after a draft. It is
# what turns "where did the room deviate from the board" into an observed
# question instead of a simulated one.
#
# LINEUP FIT (roster-aware marginal value)
# ----------------------------------------
# VORP is a property of the player, not of the board: it prices him against a
# rank-based replacement that already assumes every starter-rank player gets
# drafted. Taking Bijan Robinson off the pool does not change what RB25
# projects. So VORP correctly does NOT move as picks come off.
#
# What DOES change is what a player is worth TO YOU. Fit is:
#
#   fit(p) = best starting lineup(my roster + p) - best starting lineup(my roster)
#
# with unfilled slots valued at replacement. On an empty roster fit is close to
# VORP. Once you hold two RBs the third competes only for a flex seat and his
# fit collapses toward the flex margin. A second QB in a 1-QB league scores 0.
#
# This is slot arithmetic on projected PPG, not a distributional quantity, so it
# is valid for REDRAFT. It is NOT valid for best ball, where value depends on
# how often a player cracks a weekly max-scoring lineup. Do not read the FIT
# sort as a best-ball recommendation.
#
# Replacement level is derived from the board itself: base demand is
# num_teams * starting slots per position, then flex seats are allocated to
# whichever RB/WR/TE are best among those beyond base demand. Replacement at a
# position is the projected PPG of the next player past that total demand.
#
# LIVE DRAFT SYNC
# ---------------
# Supply league_id and sleeper_user_id and the page polls Sleeper directly:
#
#   GET /v1/league/<league_id>/drafts        -> draft_id (once, on load)
#   GET /v1/draft/<draft_id>/picks           -> every pick, on an interval
#
# Picks carry Sleeper's own player_id, so a sleeper_player_id -> player_uid
# crosswalk is baked in at build time from R/19's cached player database. No
# name matching happens during a draft.
#
# picked_by on each pick is the user_id the pick goes to, so your own picks are
# identified without you touching the screen. Sleeper's guidance is to stay
# under 1000 calls per minute; a 5 second interval is 12.
#
# VERIFIED 2026-08-06: these calls succeed from a file:// origin, so the page
# stays a single local file with no server. If that ever changes the page
# reports the failure and manual marking continues to work.
#
# PRECEDENCE: manual marks WIN and are never overwritten by a poll. A live pick
# for a player you already marked is ignored. This exists so a stalled or
# rate-limited poll can never strand you: the fallback is always available.
#
# ADP SOURCE
# ----------
# Three columns ship and the toggle is live. Sleeper ADP is the market your
# opponents are actually looking at in the draft room, so it is the default when
# supplied. fp_adp is FantasyPros redraft consensus. dk_adp is DraftKings BEST
# BALL ADP, a different format with different roster construction.
#
# Sleeper publishes no public ADP endpoint, so sleeper_adp_path takes a plain
# text file of "<rank> <name>" lines exported from the app. DEF and K lines are
# skipped but their ranks are NOT renumbered, because those picks really do
# consume draft slots.
#
# VALUE FLAG (Fork A, locked)
# ---------------------------
# adp_gap is shown and colored. The VALUE flag is suppressed when the gap
# exceeds flag_cap picks in either direction, and those rows are labeled
# "market disagrees, check news". A gap that large is usually news the model has
# not seen, not an edge.
#
# USAGE
# -----
#   source(here::here("R", "51_draft_board_html.R"))
#   build_draft_board_html(
#     csv_path    = here::here("data", "season3_cache",
#                              "value_board_Gridiron_Guzzlers.csv"),
#     league_name = "Gridiron Guzzlers",
#     num_teams   = 12L,
#     starters    = list(QB = 1L, RB = 2L, WR = 2L, TE = 1L),
#     flex        = 2L,
#     adp_default = "fp"
#   )
# ==============================================================================

library(dplyr)
library(readr)
library(glue)
library(here)

DRAFT_BOARD_OUTPUT_DIR <- here::here("output", "draft_boards")


# Sleeper renders some players under a nickname while the roster feed uses the
# formal name. Normalizing punctuation and suffixes cannot bridge that, so a
# lookup is the honest fix. Keys and values are both normalized form.
# LEFT = as Sleeper writes it. RIGHT = as the board writes it.
SLEEPER_NAME_ALIASES <- c(
  "kenny gainwell" = "kenneth gainwell"
)


# The live-sync crosswalk runs the OTHER direction: board name -> Sleeper name.
# It is a separate table on purpose. Inverting the one above would be wrong the
# moment a mapping is not one to one.
# LEFT = as the board writes it. RIGHT = as Sleeper writes it.
# Each verified against get_all_sleeper_players() before being added.
BOARD_NAME_ALIASES <- c(
  "kenneth gainwell" = "kenny gainwell",     # RB TB,  sleeper_player_id 7567
  "josh palmer"      = "joshua palmer",      # WR BUF, 7670
  "bam knight"       = "zonovan knight",     # RB ARI, 8122
  "mitchell tinsley" = "mitch tinsley"       # WR CIN, 11068
)


# Mirror of R/46 .vb_normalize_name so this file stands alone. Sleeper's export
# is a bare name list, so name matching is the only key available.
.vb_normalize_name_local <- function(name_vec) {
  out <- tolower(as.character(name_vec))
  out <- gsub("[.']", "", out)
  out <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b", "", out, ignore.case = TRUE)
  out <- gsub("[^a-z ]", "", out)
  out <- gsub("\\s+", " ", out)
  trimws(out)
}


# ------------------------------------------------------------------------------
# .db_json_escape
# ------------------------------------------------------------------------------

# Minimal JSON string escaper. Player names carry apostrophes (Ja'Marr Chase,
# De'Zhaun Stribling) and the board is injected into a <script> block, so an
# unescaped quote or a literal "</script>" would break the page silently.
.db_json_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\"", "\\\\\"", x)
  x <- gsub("\r", "", x, fixed = TRUE)
  x <- gsub("\n", " ", x, fixed = TRUE)
  x <- gsub("\t", " ", x, fixed = TRUE)
  x <- gsub("<", "\\\\u003c", x, fixed = TRUE)
  x <- gsub(">", "\\\\u003e", x, fixed = TRUE)
  x <- gsub("&", "\\\\u0026", x, fixed = TRUE)
  x
}

.db_num <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- ifelse(is.na(x), "null", format(round(x, 2), trim = TRUE,
                                         scientific = FALSE))
  out
}


# ------------------------------------------------------------------------------
# build_draft_board_html
# ------------------------------------------------------------------------------

#' Build a self-contained phone draft board from an R/46 value board CSV.
#'
#' @param csv_path Path to a value_board_<League>.csv written by R/46.
#' @param league_name Display name. Also seeds the storage key, so two leagues
#'   opened on the same phone keep separate draft state.
#' @param num_teams Integer team count.
#' @param starters Named list of starting slots, e.g. list(QB=1,RB=2,WR=2,TE=1).
#' @param flex Integer flex slots (RB/WR/TE eligible).
#' @param rec_flex Integer REC_FLEX slots (WR/TE eligible only). [2026-08-11]
#'   build_league_config() (R/33:326) has carried this since it was written
#'   and this function ignored it, so a league using a receiver-only flex had
#'   that seat silently dropped from the roster-aware FIT column. No error, no
#'   warning: the value board was right and the draft board was quietly wrong
#'   about what the roster still needed.
#' @param wrrb_flex Integer WR/RB flex slots (WR/RB eligible only, no TE).
#'   Same history as rec_flex.
#' @param superflex Integer SUPER_FLEX slots (QB/RB/WR/TE eligible). Get this
#'   from build_config_from_sleeper()$superflex, NOT from the league's
#'   printed lineup string, which historically omitted it. In a superflex
#'   league a second QB is a starter, so leaving this at 0 wrongly scores him
#'   at zero.
#' @param sleeper_adp_path Optional path to a plain text Sleeper ADP export,
#'   one "<rank> <name>" per line. DEF/K lines are skipped, ranks are kept as
#'   given. When supplied, Sleeper becomes the default ADP source.
#' @param adp_default "sleeper", "fp" or "dk". Which ADP the gap is computed
#'   against on load. All supplied columns are available in the file.
#' @param league_id Optional Sleeper league id. When supplied the page resolves
#'   the draft and polls it. Omit for a purely manual board.
#' @param sleeper_user_id Optional Sleeper user id, used to identify which live
#'   picks are yours via each pick's picked_by field. Required for live sync.
#' @param preseed Optional data frame with columns player_uid and mine
#'   (logical). Rows are marked taken when the page first loads, with mine =
#'   TRUE routed to your own roster so the FIT column scores against the slots
#'   you have actually filled. Intended for a dynasty or keeper draft, where
#'   most of the board is already rostered before the draft opens.
#'
#'   MERGE RULE: preseed is applied only to players not already in the pick
#'   log. A manual mark or a live pick always wins, so re-opening the page
#'   never resurrects a preseeded player over something you did by hand, and a
#'   preseeded player who is somehow drafted live keeps the live record.
#'
#'   Preseeded entries carry src = "preseed" and no pick_no, so they sort ahead
#'   of real picks in the rail and are distinguishable in the CSV export.
#' @param poll_seconds Poll interval. Default 5.
#' @param flag_cap Gap magnitude above which the VALUE flag is suppressed and
#'   the row is labeled "market disagrees". Fork A locked this at 40.
#' @param max_players Rows to include, taken from the top of the VORP board.
#'   594 rows is more than any draft consumes; trimming keeps phone search fast.
#' @param out_dir Output directory.
#' @return (invisibly) the path written.
build_draft_board_html <- function(csv_path,
                                   league_name,
                                   num_teams   = 12L,
                                   starters    = list(QB = 1L, RB = 2L,
                                                      WR = 2L, TE = 1L),
                                   flex        = 2L,
                                   superflex   = 0L,
                                   rec_flex    = 0L,
                                   wrrb_flex   = 0L,
                                   sleeper_adp_path = NULL,
                                   league_id = NULL,
                                   sleeper_user_id = NULL,
                                   preseed = NULL,
                                   poll_seconds = 5L,
                                   adp_default = c("sleeper", "fp", "dk"),
                                   flag_cap    = 40,
                                   max_players = 350L,
                                   out_dir     = DRAFT_BOARD_OUTPUT_DIR) {

  adp_default <- match.arg(adp_default)
  if (adp_default == "sleeper" && is.null(sleeper_adp_path)) adp_default <- "fp"

  if (!file.exists(csv_path)) {
    stop(glue("build_draft_board_html(): no file at {csv_path}"), call. = FALSE)
  }

  board <- readr::read_csv(csv_path, show_col_types = FALSE)

  need <- c("player_uid", "player_name", "position", "team",
            "r32_posterior_mu", "vorp_rank", "fp_adp", "dk_adp")
  miss <- setdiff(need, names(board))
  if (length(miss) > 0L) {
    stop(glue("build_draft_board_html(): {csv_path} is missing ",
              "{paste(miss, collapse = ', ')}. Present: ",
              "{paste(names(board), collapse = ', ')}. A board without ",
              "player_uid predates the Wave A2 identity fix; rebuild it with ",
              "run_value_boards()."), call. = FALSE)
  }

  # ---- no_history flag (OPTIONAL) [2026-09-02] ----------------------------
  # TRUE = R/29 built this player's prior with no NFL history at all. See the
  # long note in R/46 build_value_board() for the evidence and for why a
  # roster-based flag was rejected.
  #
  # Deliberately NOT added to `need` above. A board CSV written before
  # 2026-09-02 has no such column, and hard-requiring it would break every
  # archived board for a purely additive marker. Absent column = all FALSE,
  # which renders exactly as the board rendered before the flag existed.
  if (!"no_history" %in% names(board)) {
    board$no_history <- FALSE
    message("  no_history: column absent from this board CSV, treating as ",
            "all FALSE. Rebuild with run_value_boards() to populate it.")
  } else {
    board$no_history <- !is.na(board$no_history) & as.logical(board$no_history)
  }

  board <- board %>%
    dplyr::filter(.data$position %in% c("QB", "RB", "WR", "TE"),
                  !is.na(.data$vorp_rank)) %>%
    dplyr::arrange(.data$vorp_rank) %>%
    utils::head(as.integer(max_players))

  if (nrow(board) == 0L) {
    stop("build_draft_board_html(): no rows survived filtering.", call. = FALSE)
  }

  # ---- Sleeper ADP join (name-keyed; Sleeper's export carries no position) ----
  board$sleeper_adp <- NA_real_
  if (!is.null(sleeper_adp_path)) {
    if (!file.exists(sleeper_adp_path)) {
      stop(glue("build_draft_board_html(): no Sleeper ADP file at ",
                "{sleeper_adp_path}"), call. = FALSE)
    }
    raw <- readLines(sleeper_adp_path, warn = FALSE)
    raw <- trimws(raw[nzchar(trimws(raw))])
    m   <- regmatches(raw, regexec("^([0-9]+)[[:space:]]+(.*)$", raw))
    ok  <- vapply(m, length, integer(1)) == 3L
    rk  <- as.numeric(vapply(m[ok], `[`, character(1), 2L))
    nm  <- vapply(m[ok], `[`, character(1), 3L)

    keep <- !grepl("\\((DEF|DST|K)\\)", nm)   # ranks are NOT renumbered
    n_def <- sum(!keep)
    rk <- rk[keep]; nm <- nm[keep]

    key <- .vb_normalize_name_local(nm)
    ali <- match(key, names(SLEEPER_NAME_ALIASES))
    n_ali <- sum(!is.na(ali))
    key[!is.na(ali)] <- unname(SLEEPER_NAME_ALIASES[ali[!is.na(ali)]])
    dup <- duplicated(key)
    key <- key[!dup]; rk <- rk[!dup]

    bkey <- .vb_normalize_name_local(board$player_name)
    idx  <- match(bkey, key)
    board$sleeper_adp <- rk[idx]

    matched <- sum(!is.na(board$sleeper_adp))
    unmatched <- setdiff(key, bkey)
    message(glue("  Sleeper ADP: {length(key)} ranked player(s) ",
                 "({n_def} DEF/K line(s) skipped, ranks kept) | ",
                 "{matched} joined to the board | {n_ali} alias(es) applied"))
    if (length(unmatched) > 0L) {
      message(glue("    not on the board ({length(unmatched)}): ",
                   "{paste(utils::head(unmatched, 15), collapse = ', ')}"))
      message("    a ranked player absent from the board was dropped upstream ",
              "by R/29 as retired, unsigned or unverifiable. Check status.")
    }
  }

  # ---- serialize board to JSON ----
  rows <- glue::glue(
    '{{"u":"{u}","n":"{n}","p":"{p}","t":"{t}","m":{m},',
    '"r":{r},"fp":{fp},"dk":{dk},"sl":{sl},"nh":{nh}}}',
    u  = .db_json_escape(board$player_uid),
    n  = .db_json_escape(board$player_name),
    p  = .db_json_escape(board$position),
    t  = .db_json_escape(board$team),
    m  = .db_num(board$r32_posterior_mu),
    r  = .db_num(board$vorp_rank),
    fp = .db_num(board$fp_adp),
    dk = .db_num(board$dk_adp),
    sl = .db_num(board$sleeper_adp),
    nh = ifelse(board$no_history, "true", "false")
  )
  board_json <- paste0("[", paste(rows, collapse = ","), "]")

  # ---- Sleeper player_id crosswalk for live sync ----
  sid_json <- "{}"
  live_on  <- FALSE
  if (!is.null(league_id)) {
    if (is.null(sleeper_user_id)) {
      stop("build_draft_board_html(): league_id supplied without ",
           "sleeper_user_id. Without it, live picks cannot be attributed and ",
           "your own roster would never fill.", call. = FALSE)
    }
    if (!exists("get_all_sleeper_players")) {
      stop("build_draft_board_html(): live sync needs R/19 sourced ",
           "(get_all_sleeper_players not found).", call. = FALSE)
    }
    sp <- get_all_sleeper_players()

    # The id column is sleeper_player_id (see R/19 .get_sleeper_players). A
    # wrong name here yields an EMPTY crosswalk and a page that silently never
    # auto-marks anything, so verify rather than warn.
    need_sp <- c("sleeper_player_id", "nfl_gsis_id", "player_name", "position")
    miss_sp <- setdiff(need_sp, names(sp))
    if (length(miss_sp) > 0L) {
      stop(glue("build_draft_board_html(): Sleeper player table is missing ",
                "{paste(miss_sp, collapse = ', ')}. Present: ",
                "{paste(names(sp), collapse = ', ')}"), call. = FALSE)
    }

    # Primary key is gsis, which is what player_uid carries for everyone with a
    # real id. The id-less rookie cohort falls back to name + position.
    uid_by_gsis <- stats::setNames(board$player_uid, board$player_uid)
    gsis_hit <- sp$sleeper_player_id[!is.na(sp$nfl_gsis_id) &
                             sp$nfl_gsis_id %in% board$player_uid]
    gsis_uid <- sp$nfl_gsis_id[!is.na(sp$nfl_gsis_id) &
                               sp$nfl_gsis_id %in% board$player_uid]

    # NAME FALLBACK.
    # Sleeper's cached gsis_id is sparsely populated, so in practice most of the
    # crosswalk resolves here rather than by gsis. name + position is NOT unique
    # across 12,210 Sleeper players, and match() silently takes the first hit,
    # so a collision would map a pick to the WRONG player with no error. Match
    # on name + position + team first, fall back to name + position only where
    # that key is unambiguous, and refuse an ambiguous key outright.
    idless <- board[!board$player_uid %in% gsis_uid, , drop = FALSE]
    nm_hit <- character(0); nm_uid <- character(0)
    n_amb <- 0L; n_team <- 0L; n_balias <- 0L
    if (nrow(idless) > 0L) {
      spn <- .vb_normalize_name_local(sp$player_name)
      spp <- toupper(sp$position)
      spt <- toupper(ifelse(is.na(sp$team), "", sp$team))

      bn <- .vb_normalize_name_local(idless$player_name)
      ba <- match(bn, names(BOARD_NAME_ALIASES))
      n_balias <- sum(!is.na(ba))
      bn[!is.na(ba)] <- unname(BOARD_NAME_ALIASES[ba[!is.na(ba)]])
      bp <- toupper(idless$position)
      bt <- toupper(ifelse(is.na(idless$team), "", idless$team))

      m1 <- match(paste0(bn, "|", bp, "|", bt),
                  paste0(spn, "|", spp, "|", spt))

      k2      <- paste0(spn, "|", spp)
      tab     <- table(k2)
      uniq_k2 <- names(tab)[tab == 1L]
      bk2     <- paste0(bn, "|", bp)
      m2      <- match(bk2, k2)
      m2[!(bk2 %in% uniq_k2)] <- NA_integer_

      n_amb  <- sum(is.na(m1) & !is.na(match(bk2, k2)) & !(bk2 %in% uniq_k2))
      m      <- ifelse(!is.na(m1), m1, m2)
      n_team <- sum(!is.na(m1))

      ok <- !is.na(m)
      nm_hit <- sp$sleeper_player_id[m[ok]]
      nm_uid <- idless$player_uid[ok]
    }

    sid <- c(gsis_hit, nm_hit)
    uid <- c(gsis_uid, nm_uid)
    keep <- !is.na(sid) & !is.na(uid) & !duplicated(sid)
    sid <- sid[keep]; uid <- uid[keep]

    sid_json <- paste0("{", paste(sprintf('"%s":"%s"',
                                          .db_json_escape(sid),
                                          .db_json_escape(uid)),
                                  collapse = ","), "}")
    if (length(sid) == 0L) {
      stop("build_draft_board_html(): the Sleeper crosswalk resolved 0 ",
           "players. Live sync would connect and never mark anything. ",
           "Check that the board's player_uid values are gsis ids.",
           call. = FALSE)
    }
    if (length(sid) < 0.8 * nrow(board)) {
      warning(glue("build_draft_board_html(): crosswalk covers only ",
                   "{length(sid)} of {nrow(board)} board players. Picks for ",
                   "the rest will not auto-mark."), call. = FALSE)
    }
    live_on <- TRUE
    message(glue("  live sync: crosswalk {length(sid)} of {nrow(board)} board ",
                 "player(s) to Sleeper ids ",
                 "({length(gsis_hit)} by gsis, {n_team} by name+pos+team, ",
                 "{length(nm_hit) - n_team} by unique name+pos, ",
                 "{n_balias} alias(es))"))
    if (n_amb > 0L) {
      message(glue("    {n_amb} player(s) REFUSED: name+position is ambiguous ",
                   "in Sleeper's database and team did not disambiguate. ",
                   "Their picks will not auto-mark; mark them by hand."))
    }
    unmapped <- setdiff(board$player_uid, uid)
    if (length(unmapped) > 0L) {
      nms <- board$player_name[match(unmapped, board$player_uid)]
      message(glue("    {length(unmapped)} board player(s) not crosswalked: ",
                   "{paste(utils::head(nms, 10), collapse = ', ')}"))
    }
    message(glue("    league {league_id} | user {sleeper_user_id} | ",
                 "poll {poll_seconds}s"))
  }

  # ---- preseed: mark already-rostered players taken on first load ----
  # Validated against the board here rather than in JS, so a uid that does not
  # exist on the board is reported at build time instead of silently doing
  # nothing on a phone at 6pm.
  preseed_json <- "[]"
  if (!is.null(preseed)) {
    if (!is.data.frame(preseed)) {
      stop("build_draft_board_html(): preseed must be a data frame.",
           call. = FALSE)
    }
    need_ps <- c("player_uid", "mine")
    miss_ps <- setdiff(need_ps, names(preseed))
    if (length(miss_ps) > 0L) {
      stop(glue("build_draft_board_html(): preseed is missing ",
                "{paste(miss_ps, collapse = ', ')}."), call. = FALSE)
    }

    ps <- preseed[!is.na(preseed$player_uid), , drop = FALSE]
    ps <- ps[!duplicated(ps$player_uid), , drop = FALSE]

    on_board <- ps$player_uid %in% board$player_uid
    n_off    <- sum(!on_board)
    ps       <- ps[on_board, , drop = FALSE]

    if (nrow(ps) > 0L) {
      preseed_json <- paste0(
        "[",
        paste(sprintf('{"u":"%s","mine":%s}',
                      .db_json_escape(ps$player_uid),
                      ifelse(isTRUE(as.logical(ps$mine)) |
                               as.logical(ps$mine) %in% TRUE,
                             "true", "false")),
              collapse = ","),
        "]"
      )
    }

    n_mine <- sum(as.logical(ps$mine) %in% TRUE)
    message(glue("  preseed: {nrow(ps)} player(s) marked taken ",
                 "({n_mine} yours), {n_off} uid(s) not on the board"))
  }

  starters <- starters[c("QB", "RB", "WR", "TE")]
  starters[vapply(starters, is.null, logical(1))] <- 0L
  # REC and WRRB carry their own eligibility and cannot be folded into FLEX:
  # a receiver-only seat must not be filled by a spare RB. [2026-08-11]
  slots_json <- glue::glue(
    '{{"QB":{qb},"RB":{rb},"WR":{wr},"TE":{te},"FLEX":{fx},',
    '"REC":{rc},"WRRB":{wb},"SF":{sf}}}',
    qb = as.integer(starters$QB), rb = as.integer(starters$RB),
    wr = as.integer(starters$WR), te = as.integer(starters$TE),
    fx = as.integer(flex), rc = as.integer(rec_flex),
    wb = as.integer(wrrb_flex), sf = as.integer(superflex)
  )

  storage_key <- paste0("draft_",
                        gsub("[^A-Za-z0-9]+", "_", tolower(league_name)))
  built_stamp <- format(Sys.time(), "%Y-%m-%d %H:%M")
  adp_col <- if (adp_default == "sleeper") "sleeper_adp" else paste0(adp_default, "_adp")
  adp_missing <- sum(is.na(board[[adp_col]]))

  html <- .db_template(
    board_json  = board_json,
    slots_json  = slots_json,
    league_name = .db_json_escape(league_name),
    num_teams   = as.integer(num_teams),
    # R's TRUE/FALSE render as "TRUE"/"FALSE" through paste0, which are not
    # JavaScript identifiers. Emitting them raw threw a ReferenceError on the
    # first line of the script block and killed the entire page. Convert here.
    has_sleeper = if (is.null(sleeper_adp_path)) "false" else "true",
    sid_json     = sid_json,
    preseed_json = preseed_json,
    live_league = if (live_on) as.character(league_id) else "",
    live_user   = if (live_on) as.character(sleeper_user_id) else "",
    poll_ms     = as.integer(poll_seconds) * 1000L,
    adp_default = adp_default,
    flag_cap    = flag_cap,
    storage_key = storage_key,
    built_stamp = built_stamp,
    n_players   = nrow(board)
  )

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  safe  <- gsub("[^A-Za-z0-9]+", "_", league_name)
  fpath <- file.path(out_dir, glue("draft_board_{safe}.html"))
  writeLines(html, fpath, useBytes = TRUE)

  message(glue("  wrote {basename(fpath)}"))
  message(glue("    league   : {league_name} | {num_teams} teams | ",
               "QB{starters$QB}/RB{starters$RB}/WR{starters$WR}/",
               "TE{starters$TE}+{flex}FLEX",
               "{if (as.integer(rec_flex)  > 0L) paste0('+', rec_flex,  'REC')  else ''}",
               "{if (as.integer(wrrb_flex) > 0L) paste0('+', wrrb_flex, 'WRRB') else ''}",
               "{if (as.integer(superflex) > 0L) paste0('+', superflex, 'SF')   else ''}"))
  message(glue("    players  : {nrow(board)} | ADP source: {adp_default} ",
               "({adp_missing} without an ADP)"))
  message(glue("    source   : {basename(csv_path)}"))
  message(glue("    built    : {built_stamp}"))

  invisible(fpath)
}


# ------------------------------------------------------------------------------
# .db_template
# ------------------------------------------------------------------------------

# The page. Kept as one function returning one string so the generator stays a
# single file with no template assets to lose.
.db_template <- function(board_json, slots_json, league_name, num_teams,
                         adp_default, flag_cap, storage_key, built_stamp,
                         n_players, has_sleeper, sid_json, live_league,
                         live_user, poll_ms, preseed_json = "[]") {

  paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover">
<meta name="theme-color" content="#13311d">
<title>', league_name, ' draft board</title>
<style>
:root{
  --ink:#13311d; --field:#1f6f3f; --cream:#f4ead0; --gold:#e6b53c;
  --paper:#0d2415; --line:rgba(244,234,208,.14); --dim:rgba(244,234,208,.55);
  --good:#7fc99a; --bad:#e08b7a;
}
*{box-sizing:border-box;-webkit-tap-highlight-color:transparent}
html,body{margin:0;padding:0}
body{
  background:var(--paper); color:var(--cream);
  font:15px/1.35 ui-sans-serif,-apple-system,"Segoe UI",Roboto,sans-serif;
  padding-bottom:env(safe-area-inset-bottom);
  -webkit-text-size-adjust:100%;
}
.wrap{max-width:640px;margin:0 auto}

/* ---- sticky head ---- */
header{
  position:sticky;top:0;z-index:20;background:var(--ink);
  border-bottom:1px solid var(--line);
  padding:calc(8px + env(safe-area-inset-top)) 10px 8px;
}
.title{display:flex;align-items:baseline;gap:8px;margin-bottom:7px}
.title h1{
  font-size:14px;font-weight:700;letter-spacing:.14em;text-transform:uppercase;
  margin:0;color:var(--cream)
}
.title .meta{font-size:11px;color:var(--dim);margin-left:auto;font-variant-numeric:tabular-nums}

/* signature: the roster rail. slot pips, always visible, fill as you draft. */
.rail{display:flex;gap:9px;align-items:center;margin-bottom:8px;flex-wrap:wrap}
.grp{display:flex;align-items:center;gap:4px}
.grp b{font-size:10px;letter-spacing:.1em;color:var(--dim);font-weight:700;width:26px}
.pip{width:13px;height:13px;border-radius:3px;border:1.5px solid var(--field);background:transparent}
.pip.on{background:var(--gold);border-color:var(--gold)}
.pip.over{background:var(--field);border-color:var(--field)}
.count{margin-left:auto;font-size:11px;color:var(--dim);font-variant-numeric:tabular-nums}

.row2{display:flex;gap:6px;align-items:center}
#q{
  flex:1;min-width:0;background:var(--paper);border:1px solid var(--line);
  color:var(--cream);border-radius:9px;padding:11px 12px;font-size:16px;
  font-family:inherit
}
#q::placeholder{color:var(--dim)}
#q:focus{outline:2px solid var(--gold);outline-offset:-1px}
.mini{
  background:transparent;border:1px solid var(--line);color:var(--dim);
  border-radius:9px;padding:11px 10px;font-size:12px;font-family:inherit;
  font-weight:600;white-space:nowrap
}
.mini:active{background:var(--field);color:var(--cream)}

.chips{display:flex;gap:5px;margin-top:8px;overflow-x:auto;-webkit-overflow-scrolling:touch}
.chip{
  flex:0 0 auto;background:transparent;border:1px solid var(--line);
  color:var(--dim);border-radius:999px;padding:6px 13px;font-size:12px;
  font-weight:600;font-family:inherit
}
.chip[aria-pressed="true"]{background:var(--field);border-color:var(--field);color:var(--cream)}

/* ---- list ---- */
ul{list-style:none;margin:0;padding:0}
li{
  display:flex;align-items:center;gap:9px;padding:10px;
  border-bottom:1px solid var(--line)
}
li.gone{opacity:.32}
/* A pick of yours is the opposite of gone. .mine must win on every property
   .gone sets, or your own roster reads as removed from the board. */
li.mine{
  opacity:1;background:rgba(230,181,60,.16);
  border-left:4px solid var(--gold);padding-left:6px
}
li.mine .nm{text-decoration:none;color:var(--gold)}
li.mine .rk{color:var(--gold)}
.tag{
  flex:0 0 auto;font-size:9px;font-weight:800;letter-spacing:.1em;
  color:var(--ink);background:var(--gold);border-radius:4px;padding:3px 5px
}
.rk{
  width:30px;flex:0 0 auto;text-align:right;font-size:12px;color:var(--dim);
  font-variant-numeric:tabular-nums
}
.who{flex:1;min-width:0}
.nm{font-size:15px;font-weight:600;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
li.gone .nm{text-decoration:line-through}
.sub{font-size:11px;color:var(--dim);margin-top:2px;font-variant-numeric:tabular-nums}
.gap{font-weight:700}
.fit{
  flex:0 0 auto;width:44px;text-align:right;font-variant-numeric:tabular-nums;
  font-size:15px;font-weight:700;color:var(--cream);line-height:1.1
}
.fit small{display:block;font-size:8px;letter-spacing:.1em;color:var(--dim);font-weight:700}
.fit.low{color:var(--dim)}
.live{
  display:flex;align-items:center;gap:7px;margin-top:8px;font-size:11px;
  color:var(--dim);font-variant-numeric:tabular-nums
}
.dot{width:8px;height:8px;border-radius:50%;background:var(--dim);flex:0 0 auto}
.dot.on{background:var(--good)} .dot.err{background:var(--bad)} .dot.off{background:var(--gold)}
.live .who2{flex:1;min-width:0;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}
.src{
  flex:0 0 auto;font-size:8px;font-weight:800;letter-spacing:.08em;
  color:var(--dim);border:1px solid var(--line);border-radius:3px;padding:2px 4px
}
.nohist{
  flex:0 0 auto;font-size:8px;font-weight:800;letter-spacing:.08em;
  color:var(--gold);border:1px solid var(--gold);border-radius:3px;
  padding:2px 4px;cursor:help
}
.sortrow{display:flex;gap:5px;margin-top:8px;align-items:center}
.sortrow span{font-size:10px;letter-spacing:.1em;color:var(--dim);font-weight:700}
.gap.pos{color:var(--good)} .gap.neg{color:var(--bad)}
.warn{color:var(--gold);font-weight:600}
.btn{
  flex:0 0 auto;border:1px solid var(--line);background:transparent;
  color:var(--dim);border-radius:8px;padding:9px 10px;font-size:11px;
  font-weight:700;font-family:inherit;letter-spacing:.04em
}
.btn.take:active{background:var(--field);color:var(--cream)}
.btn.mine:active{background:var(--gold);color:var(--ink)}
li.gone .btn{opacity:.7}

.empty{padding:36px 16px;text-align:center;color:var(--dim);font-size:14px}
footer{padding:18px 12px 30px;color:var(--dim);font-size:11px;line-height:1.6}
dialog{
  border:1px solid var(--line);background:var(--ink);color:var(--cream);
  border-radius:12px;padding:18px;max-width:300px;font-family:inherit
}
dialog::backdrop{background:rgba(0,0,0,.6)}
dialog p{margin:0 0 14px;font-size:14px}
dialog .act{display:flex;gap:8px;justify-content:flex-end}
@media (prefers-reduced-motion:no-preference){
  li{transition:opacity .12s ease,background .12s ease}
}
</style>
</head>
<body>
<div class="wrap">
<header>
  <div class="title">
    <h1>', league_name, '</h1>
    <span class="meta">', num_teams, ' teams</span>
  </div>
  <div class="rail" id="rail"></div>
  <div class="row2">
    <input id="q" type="search" placeholder="Search player" autocomplete="off"
           autocapitalize="off" spellcheck="false" enterkeyhint="search">
    <button class="mini" id="adpBtn" type="button"></button>
    <button class="mini" id="undoBtn" type="button">Undo</button>
  </div>
  <div class="chips" id="chips"></div>
  <div class="sortrow"><span>SORT</span><div class="chips" id="sorts"></div></div>
  <div class="live" id="live" hidden>
    <span class="dot" id="dot"></span>
    <span class="who2" id="liveMsg">connecting to Sleeper...</span>
    <button class="mini" id="pauseBtn" type="button">Pause</button>
  </div>
</header>

<ul id="list"></ul>
<div class="empty" id="empty" hidden>No player matches that search.</div>

<footer>
  Tap <b>TAKE</b> when anyone drafts a player. Tap <b>MINE</b> when you do.
  MINE also works on a row you already took. <b>UNDO</b> puts a player back.<br>
  <b>FIT</b> is what a player adds to your best starting lineup given what you
  already hold. It falls as you fill a position. Redraft only.<br>
  Rows tagged <b>SLP</b> came from the live draft feed. Anything you mark by
  hand wins and is never overwritten by a poll.<br>
  <b>NO HIST</b> means the projection was built with no NFL history behind it
  and carries a position-default spread. Treat the number as unfounded and
  check the player is active before spending a pick.<br>
  Board built ', built_stamp, ' &middot; ', n_players, ' players &middot;
  gaps above ', flag_cap, ' picks are flagged, not treated as value.<br>
  <button class="mini" id="exportBtn" type="button">Copy pick log</button>
  <button class="mini" id="resetBtn" type="button">Reset draft</button>
</footer>
</div>

<dialog id="dlg">
  <p id="dlgMsg"></p>
  <div class="act">
    <button class="mini" id="dlgNo" type="button">Cancel</button>
    <button class="mini" id="dlgYes" type="button">Confirm</button>
  </div>
</dialog>

<script>
(function(){
"use strict";

var BOARD = ', board_json, ';
var SLOTS = ', slots_json, ';
var KEY   = "', storage_key, '";
var T     = ', num_teams, ';
var HAS_SLEEPER = ', has_sleeper, ';
var SIDMAP  = ', sid_json, ';
var PRESEED = ', preseed_json, ';
var LEAGUE  = "', live_league, '";
var MYUSER  = "', live_user, '";
var POLLMS  = ', poll_ms, ';
var CAP   = ', flag_cap, ';
var adp   = "', adp_default, '";

/* ---- state ----
   picks: ordered log. {u, seq, mine, ts}. Order is the record; it cannot be
   rebuilt afterwards, which is why it is stored rather than a taken flag. */
var picks = [];
var memOnly = false;

function load(){
  try{
    var raw = window.localStorage.getItem(KEY);
    if(raw){ var p = JSON.parse(raw); if(Array.isArray(p)) picks = p; }
  }catch(e){ memOnly = true; }
  applyPreseed();
}

/* Preseed runs on every load, not just the first, so a player rostered
   between sessions still gets marked. It never touches a uid already in the
   log, so manual marks and live picks always win. */
function applyPreseed(){
  if(!PRESEED || !PRESEED.length) return;
  var added = 0;
  PRESEED.forEach(function(ps){
    if(taken(ps.u)) return;
    picks.push({u:ps.u, seq:picks.length+1, mine:!!ps.mine,
                ts:Date.now(), src:"preseed",
                pick_no:null, round:null, slot:null, roster:null});
    added++;
  });
  if(added) save();
}
function save(){
  if(memOnly) return;
  try{ window.localStorage.setItem(KEY, JSON.stringify(picks)); }
  catch(e){ memOnly = true; }
}

var byUid = {};
BOARD.forEach(function(p){ byUid[p.u] = p; });
function taken(u){ for(var i=0;i<picks.length;i++) if(picks[i].u===u) return picks[i]; return null; }

/* ---- search index ---- */
BOARD.forEach(function(p){
  p.key = (p.n + " " + p.t + " " + p.p).toLowerCase()
          .replace(/[^a-z0-9 ]/g,"");
});

var pos = "ALL";
var q = "";
var sortBy = "rank";

/* ---- roster rail ---- */
function renderRail(){
  var mine = picks.filter(function(k){ return k.mine; })
                  .map(function(k){ return byUid[k.u]; })
                  .filter(Boolean);
  var have = {QB:0,RB:0,WR:0,TE:0};
  mine.forEach(function(p){ if(have[p.p]!==undefined) have[p.p]++; });

  var html = "";
  ["QB","RB","WR","TE"].forEach(function(k){
    var need = SLOTS[k]||0, got = have[k]||0;
    if(!need && !got) return;
    var pips = "";
    for(var i=0;i<need;i++) pips += \'<span class="pip\'+(i<got?" on":"")+\'"></span>\';
    var extra = Math.max(0, got-need);
    for(var j=0;j<extra;j++) pips += \'<span class="pip over"></span>\';
    html += \'<span class="grp"><b>\'+k+\'</b>\'+pips+\'</span>\';
  });

  var flexOverflow = 0;
  FLEX_POS.forEach(function(k){
    flexOverflow += Math.max(0,(have[k]||0)-(SLOTS[k]||0)); });
  var qbOverflow = Math.max(0,(have.QB||0)-(SLOTS.QB||0));

  var flexNeed = SLOTS.FLEX||0;
  var flexGot = Math.min(flexNeed, flexOverflow);
  if(flexNeed){
    var fp = "";
    for(var f=0;f<flexNeed;f++) fp += \'<span class="pip\'+(f<flexGot?" on":"")+\'"></span>\';
    html += \'<span class="grp"><b>FLX</b>\'+fp+\'</span>\';
  }
  /* Restricted flex pips. Overflow is counted per eligibility group and each
     group consumes what it fills, so the SF count below sees only what is
     genuinely left over. [2026-08-11] */
  var recOverflow = 0;
  REC_POS.forEach(function(k){
    recOverflow += Math.max(0,(have[k]||0)-(SLOTS[k]||0)); });
  var wrrbOverflow = 0;
  WRRB_POS.forEach(function(k){
    wrrbOverflow += Math.max(0,(have[k]||0)-(SLOTS[k]||0)); });

  var spare = flexOverflow - flexGot;

  var recNeed = SLOTS.REC||0;
  var recGot  = Math.min(recNeed, Math.min(recOverflow, spare));
  if(recNeed){
    var rp2 = "";
    for(var r2=0;r2<recNeed;r2++) rp2 += \'<span class="pip\'+(r2<recGot?" on":"")+\'"></span>\';
    html += \'<span class="grp"><b>REC</b>\'+rp2+\'</span>\';
    spare -= recGot;
  }

  var wbNeed = SLOTS.WRRB||0;
  var wbGot  = Math.min(wbNeed, Math.min(wrrbOverflow, spare));
  if(wbNeed){
    var wp = "";
    for(var w2=0;w2<wbNeed;w2++) wp += \'<span class="pip\'+(w2<wbGot?" on":"")+\'"></span>\';
    html += \'<span class="grp"><b>W/R</b>\'+wp+\'</span>\';
    spare -= wbGot;
  }

  var sfNeed = SLOTS.SF||0;
  if(sfNeed){
    var sfGot = Math.min(sfNeed, spare + qbOverflow);
    var sp = "";
    for(var g=0;g<sfNeed;g++) sp += \'<span class="pip\'+(g<sfGot?" on":"")+\'"></span>\';
    html += \'<span class="grp"><b>SF</b>\'+sp+\'</span>\';
  }
  html += \'<span class="count">\'+mine.length+\' mine &middot; \'+picks.length+\' off board</span>\';
  document.getElementById("rail").innerHTML = html;
}

/* ---- replacement levels, derived from the board ----
   Base demand is teams * starting slots. Flex seats then go to whichever
   RB/WR/TE are best among those beyond base demand, so flex demand is measured
   off this board rather than assumed. Replacement at a position is the next
   player past that total demand. */
var REP = {}, REPFLEX = 0, REPSF = 0;
var FLEX_POS = ["RB","WR","TE"];
var REC_POS  = ["WR","TE"];        /* receiver-only flex */
var WRRB_POS = ["WR","RB"];        /* WR/RB flex, no TE  */
var SF_POS   = ["QB","RB","WR","TE"];
function buildReplacement(){
  var byPos = {QB:[],RB:[],WR:[],TE:[]};
  BOARD.forEach(function(p){ if(byPos[p.p] && p.m!==null) byPos[p.p].push(p); });
  Object.keys(byPos).forEach(function(k){
    byPos[k].sort(function(a,b){ return b.m - a.m; }); });

  var demand = {QB:(SLOTS.QB||0)*T, RB:(SLOTS.RB||0)*T,
                WR:(SLOTS.WR||0)*T, TE:(SLOTS.TE||0)*T};

  var pool = [];
  FLEX_POS.forEach(function(k){
    pool = pool.concat(byPos[k].slice(demand[k])); });
  pool.sort(function(a,b){ return b.m - a.m; });
  pool.slice(0, (SLOTS.FLEX||0)*T).forEach(function(p){ demand[p.p]++; });

  /* Restricted flex seats, allocated AFTER generic FLEX and BEFORE SUPER_FLEX,
     each drawing only from its own eligible positions. Folding these into
     FLEX would let a spare RB fill a receiver-only seat. [2026-08-11] */
  function allocRestricted(nSlots, elig){
    if(!nSlots) return;
    var rp = [];
    elig.forEach(function(k){ rp = rp.concat(byPos[k].slice(demand[k])); });
    rp.sort(function(a,b){ return b.m - a.m; });
    rp.slice(0, nSlots*T).forEach(function(p){ demand[p.p]++; });
  }
  allocRestricted(SLOTS.REC||0,  REC_POS);
  allocRestricted(SLOTS.WRRB||0, WRRB_POS);

  /* SUPER_FLEX is QB/RB/WR/TE eligible and is allocated AFTER flex, mirroring
     R/33\'s allocator. This is what makes QB scarce in a superflex league: QB
     demand roughly doubles, so QB replacement level drops hard. */
  if (SLOTS.SF){
    var spool = [];
    SF_POS.forEach(function(k){
      spool = spool.concat(byPos[k].slice(demand[k])); });
    spool.sort(function(a,b){ return b.m - a.m; });
    spool.slice(0, SLOTS.SF*T).forEach(function(p){ demand[p.p]++; });
  }

  Object.keys(byPos).forEach(function(k){
    var lst = byPos[k], i = demand[k];
    REP[k] = lst.length ? (lst[i] ? lst[i].m : lst[lst.length-1].m) : 0;
  });
  REPFLEX = Math.max(REP.RB||0, REP.WR||0, REP.TE||0);
  REPSF   = Math.max(REP.QB||0, REPFLEX);
}

/* Best starting lineup for a set of players, unfilled slots at replacement. */
function lineup(ps){
  var by = {QB:[],RB:[],WR:[],TE:[]};
  ps.forEach(function(p){ if(by[p.p]) by[p.p].push(p.m===null?0:p.m); });
  Object.keys(by).forEach(function(k){
    by[k].sort(function(a,b){ return b-a; }); });

  /* FLEX is RB/WR/TE only. Surplus QBs are NOT flex-eligible in a 1-QB league,
     so a second QB must contribute nothing. Collecting spare from all four
     positions let a backup QB fill a flex seat and score like a starter. */
  var tot = 0, flexSpare = [], sfOnly = [];
  ["QB","RB","WR","TE"].forEach(function(k){
    var need = SLOTS[k]||0;
    for(var i=0;i<need;i++) tot += (by[k][i]!==undefined ? by[k][i] : REP[k]);
    if(by[k].length > need){
      var extra = by[k].slice(need);
      /* FLEX is RB/WR/TE only. Surplus QBs are not flex-eligible, so they wait
         for a SUPER_FLEX seat. With no SF slot they contribute nothing, which
         is why a second QB scores 0 in a 1-QB league. */
      if(FLEX_POS.indexOf(k) !== -1) flexSpare = flexSpare.concat(extra);
      else sfOnly = sfOnly.concat(extra);
    }
  });
  flexSpare.sort(function(a,b){ return b-a; });
  var fn = SLOTS.FLEX||0;
  for(var j=0;j<fn;j++) tot += (flexSpare[j]!==undefined ? flexSpare[j] : REPFLEX);

  var sn = SLOTS.SF||0;
  if(sn){
    var sfPool = flexSpare.slice(fn).concat(sfOnly);
    sfPool.sort(function(a,b){ return b-a; });
    for(var q=0;q<sn;q++) tot += (sfPool[q]!==undefined ? sfPool[q] : REPSF);
  }
  return tot;
}

/* fit(p) = what p adds to your best starting lineup, given what you hold. */
var myRoster = [], myBase = 0;
function refreshRoster(){
  myRoster = picks.filter(function(k){ return k.mine; })
                  .map(function(k){ return byUid[k.u]; })
                  .filter(Boolean);
  myBase = lineup(myRoster);
}
function fitOf(p){
  return Math.round((lineup(myRoster.concat([p])) - myBase)*10)/10;
}

/* ---- list ---- */
function adpOf(p){
  if(adp==="sleeper") return p.sl;
  if(adp==="fp") return p.fp;
  return p.dk;
}
function gapOf(p){
  var a = adpOf(p);
  if(a===null||a===undefined||p.r===null) return null;
  return Math.round((a - p.r)*10)/10;
}

function renderList(){
  refreshRoster();

  var view = BOARD.filter(function(p){
    if(pos!=="ALL" && p.p!==pos) return false;
    if(q && p.key.indexOf(q)===-1) return false;
    return true;
  });
  view.forEach(function(p){ p._f = fitOf(p); });
  if(sortBy==="fit"){
    view.sort(function(a,b){
      var ta = taken(a.u)?1:0, tb = taken(b.u)?1:0;   /* gone sinks */
      if(ta!==tb) return ta-tb;
      return b._f - a._f;
    });
  }

  var out = [], shown = 0;
  for(var i=0;i<view.length;i++){
    var p = view[i];
    shown++;
    if(shown>220) break;

    var t = taken(p.u);
    var g = gapOf(p);
    var a = adpOf(p);
    var f = p._f;

    var cls = "";
    if(t) cls += " gone";
    if(t && t.mine) cls += " mine";

    var gapHtml;
    if(g===null){ gapHtml = \'<span class="gap">no ADP</span>\'; }
    else if(Math.abs(g) > CAP){
      gapHtml = \'<span class="warn">market disagrees, check news</span>\';
    } else {
      gapHtml = \'<span class="gap \'+(g>0?"pos":"neg")+\'">\'+(g>0?"+":"")+g.toFixed(1)+\'</span>\';
    }

    var sub = p.p+" "+p.t+" &middot; "+(p.m===null?"--":p.m.toFixed(1))+" ppg"
            + " &middot; adp "+(a===null?"--":a.toFixed(1))+" &middot; "+gapHtml;

    out.push(
      \'<li class="\'+cls+\'" data-u="\'+p.u+\'">\'
      + \'<span class="rk">\'+(p.r===null?"":p.r)+\'</span>\'
      + \'<span class="fit\'+(f<1?" low":"")+\'">\'+f.toFixed(1)+\'<small>FIT</small></span>\'
      + \'<span class="who"><div class="nm">\'+p.n+\'</div><div class="sub">\'+sub+\'</div></span>\'
      + (p.nh ? \'<span class="nohist" title="R/29 built this projection with no NFL history. Position-default sigma. Verify the player is active before drafting.">NO HIST</span>\' : "")
      + ((t && t.mine) ? \'<span class="tag">MINE</span>\' : "")
      + ((t && t.src==="live") ? \'<span class="src">SLP</span>\' : "")
      + \'<button class="btn take" data-act="take" data-u="\'+p.u+\'">\'+(t?"UNDO":"TAKE")+\'</button>\'
      + ((t && t.mine) ? "" : \'<button class="btn mine" data-act="mine" data-u="\'+p.u+\'">MINE</button>\')
      + \'</li>\'
    );
  }
  document.getElementById("list").innerHTML = out.join("");
  document.getElementById("empty").hidden = out.length>0;
}

function render(){ renderRail(); renderList(); }

/* ---- actions ---- */
function mark(u, mine){
  var t = taken(u);
  if(t && mine && !t.mine){
    /* Mis-tapped TAKE on your own pick. Convert in place rather than forcing
       an undo, which would lose the pick\'s position in the log. */
    t.mine = true;
  } else if(t){
    picks = picks.filter(function(k){ return k.u!==u; });
    picks.forEach(function(k,i){ k.seq = i+1; });
  } else {
    picks.push({u:u, seq:picks.length+1, mine:!!mine, ts:Date.now(),
                src:"manual"});
  }
  save(); render();
}

document.getElementById("list").addEventListener("click", function(e){
  var b = e.target.closest("button[data-act]");
  if(!b) return;
  mark(b.getAttribute("data-u"), b.getAttribute("data-act")==="mine");
});

document.getElementById("q").addEventListener("input", function(e){
  q = e.target.value.toLowerCase().replace(/[^a-z0-9 ]/g,"").trim();
  renderList();
});

var chips = document.getElementById("chips");
["ALL","QB","RB","WR","TE"].forEach(function(k){
  var b = document.createElement("button");
  b.className = "chip"; b.type = "button"; b.textContent = k;
  b.setAttribute("aria-pressed", k===pos ? "true" : "false");
  b.addEventListener("click", function(){
    pos = k;
    Array.prototype.forEach.call(chips.children, function(c){
      c.setAttribute("aria-pressed", c.textContent===k ? "true":"false"); });
    renderList();
  });
  chips.appendChild(b);
});

var sorts = document.getElementById("sorts");
[["rank","BOARD"],["fit","FIT"]].forEach(function(pair){
  var b = document.createElement("button");
  b.className = "chip"; b.type = "button"; b.textContent = pair[1];
  b.setAttribute("aria-pressed", pair[0]===sortBy ? "true":"false");
  b.addEventListener("click", function(){
    sortBy = pair[0];
    Array.prototype.forEach.call(sorts.children, function(c){
      c.setAttribute("aria-pressed", c.textContent===pair[1] ? "true":"false"); });
    renderList();
  });
  sorts.appendChild(b);
});

var ADPS = HAS_SLEEPER ? ["sleeper","fp","dk"] : ["fp","dk"];
var ADPLBL = {sleeper:"ADP: SLP", fp:"ADP: FP", dk:"ADP: DK"};
var adpBtn = document.getElementById("adpBtn");
function adpLabel(){ adpBtn.textContent = ADPLBL[adp]; }
adpBtn.addEventListener("click", function(){
  adp = ADPS[(ADPS.indexOf(adp)+1) % ADPS.length]; adpLabel(); renderList();
});

document.getElementById("undoBtn").addEventListener("click", function(){
  if(!picks.length) return;
  picks.pop(); save(); render();
});

/* ---- confirm dialog ---- */
var dlg = document.getElementById("dlg"), pending = null;
function ask(msg, fn){
  document.getElementById("dlgMsg").textContent = msg;
  pending = fn;
  if(dlg.showModal) dlg.showModal(); else if(confirm(msg)){ fn(); pending=null; }
}
document.getElementById("dlgNo").addEventListener("click", function(){
  pending=null; dlg.close(); });
document.getElementById("dlgYes").addEventListener("click", function(){
  if(pending) pending(); pending=null; dlg.close(); });

document.getElementById("resetBtn").addEventListener("click", function(){
  ask("Clear all "+picks.length+" picks? This cannot be undone.", function(){
    picks = []; save(); render();
  });
});

/* ---- pick log export ----
   Ships as CSV text so it drops straight into R. This is the article data. */
document.getElementById("exportBtn").addEventListener("click", function(){
  var lines = ["seq,pick_no,round,draft_slot,roster_id,source,player_uid," +
               "player_name,position,team,vorp_rank,sleeper_adp,fp_adp,dk_adp,mine"];
  picks.forEach(function(k){
    var p = byUid[k.u] || {n:"",p:"",t:"",r:"",sl:"",fp:"",dk:""};
    lines.push([k.seq, (k.pick_no==null?"":k.pick_no),
                (k.round==null?"":k.round), (k.slot==null?"":k.slot),
                (k.roster==null?"":k.roster), (k.src||"manual"),
                k.u, \'"\'+p.n+\'"\', p.p, p.t,
                (p.r===null?"":p.r), (p.sl==null?"":p.sl),
                (p.fp===null?"":p.fp),
                (p.dk===null?"":p.dk), k.mine?1:0].join(","));
  });
  var txt = lines.join("\\n");
  var done = function(){ var b=document.getElementById("exportBtn");
    b.textContent="Copied "+picks.length+" picks"; setTimeout(function(){
      b.textContent="Copy pick log"; },1800); };
  if(navigator.clipboard && navigator.clipboard.writeText){
    navigator.clipboard.writeText(txt).then(done, function(){ window.prompt("Pick log", txt); });
  } else { window.prompt("Pick log", txt); }
});

/* ---- live draft sync ----
   Manual marks win. A live pick for a player already in the log is skipped, so
   a poll can never overwrite something you entered by hand. */
var draftId = null, paused = false, timer = null, lastCount = -1;

function liveStatus(cls, msg){
  var el = document.getElementById("live");
  if(el.hidden) return;
  document.getElementById("dot").className = "dot " + cls;
  document.getElementById("liveMsg").textContent = msg;
}

function applyPicks(rows){
  var added = 0, unknown = 0;
  rows.forEach(function(pk){
    var uid = SIDMAP[pk.player_id];
    if(!uid){ unknown++; return; }
    if(taken(uid)) return;                       /* manual mark wins */
    picks.push({u:uid, seq:(pk.pick_no||picks.length+1),
                mine:(pk.picked_by === MYUSER), ts:Date.now(), src:"live",
                pick_no:pk.pick_no||null, round:pk.round||null,
                slot:pk.draft_slot||null, roster:pk.roster_id||null});
    added++;
  });
  if(added){
    picks.sort(function(a,b){
      var pa = a.pick_no||9999, pb = b.pick_no||9999;
      if(pa!==pb) return pa-pb;
      return (a.ts||0)-(b.ts||0);
    });
    save(); render();
  }
  return {added:added, unknown:unknown};
}

function poll(){
  if(paused || !draftId) return;
  fetch("https://api.sleeper.app/v1/draft/" + draftId + "/picks")
    .then(function(r){ if(!r.ok) throw new Error("HTTP " + r.status); return r.json(); })
    .then(function(rows){
      var res = applyPicks(rows || []);
      lastCount = (rows||[]).length;
      var mine = picks.filter(function(k){ return k.mine; }).length;
      liveStatus("on", "live: " + lastCount + " pick(s), " + mine + " yours"
        + (res.unknown ? " (" + res.unknown + " off board)" : "")
        + " - " + new Date().toLocaleTimeString());
    })
    .catch(function(e){
      liveStatus("err", "poll failed: " + e.message + ". Mark by hand and it will resync.");
    });
}

function startLive(){
  if(!LEAGUE || !MYUSER) return;
  document.getElementById("live").hidden = false;
  liveStatus("off", "resolving draft...");
  fetch("https://api.sleeper.app/v1/league/" + LEAGUE + "/drafts")
    .then(function(r){ if(!r.ok) throw new Error("HTTP " + r.status); return r.json(); })
    .then(function(ds){
      if(!ds || !ds.length) throw new Error("no draft on this league yet");
      draftId = ds[0].draft_id;
      liveStatus("off", "draft " + ds[0].status + ", polling every "
                 + Math.round(POLLMS/1000) + "s");
      poll();
      timer = setInterval(poll, POLLMS);
    })
    .catch(function(e){
      liveStatus("err", "live sync unavailable: " + e.message
                 + ". Manual marking still works.");
    });
}

document.getElementById("pauseBtn").addEventListener("click", function(){
  paused = !paused;
  this.textContent = paused ? "Resume" : "Pause";
  if(paused) liveStatus("off", "paused. Manual marking only.");
  else { liveStatus("off", "resuming..."); poll(); }
});

buildReplacement(); load(); adpLabel(); render(); startLive();
if(memOnly){
  document.querySelector("footer").insertAdjacentHTML("afterbegin",
    "<div style=\\"color:#e6b53c;font-weight:600;margin-bottom:8px\\">"
    + "Storage unavailable here. Picks are held in memory only and a refresh "
    + "clears them. Open the file directly in your phone browser to keep state."
    + "</div>");
}
})();
</script>
</body>
</html>
')
}
