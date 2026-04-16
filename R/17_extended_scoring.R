# ==============================================================================
# SEASON 2 WEEK 3: EXTENDED FANTASY SCORING FUNCTION
# File: R/17_extended_scoring.R
# ==============================================================================
#
# PURPOSE
# -------
# Extends the Season 1 calculate_fantasy_points() (05_consistency_metrics.R)
# to cover the full Sleeper scoring option set. New parameters all default to
# 0 or their Season 1 equivalent, preserving 100% backward compatibility.
# Calling calculate_fantasy_points_ext() with no new arguments produces output
# identical to Season 1 calculate_fantasy_points() output.
#
# NAVIGATION
# ----------
#   Lines   55-120  : Constants and configuration
#   Lines  122-260  : calculate_fantasy_points_ext()    (main function)
#   Lines  262-430  : .build_ext_passing_fantasy()      (internal helper)
#   Lines  432-590  : .build_ext_rushing_fantasy()      (internal helper)
#   Lines  592-750  : .build_ext_receiving_fantasy()    (internal helper)
#   Lines  752-870  : .build_two_point_fantasy()        (internal helper)
#   Lines  872-960  : validate_ext_scoring_params()     (parameter validator)
#   Lines  962-1060 : compare_scoring_systems()         (system comparator)
#   Lines 1062-1150 : get_ext_scoring_defaults()        (defaults inspector)
#
# NEW PARAMETERS vs SEASON 1
# ---------------------------
#   first_down_points    : Points per first down earned (rush or receiving)
#   long_td_bonus        : Bonus points for TDs over long_td_threshold yards
#   long_td_threshold    : Yardage threshold for long TD bonus (default 40)
#   hundred_yard_bonus   : Bonus for 100+ rushing or receiving yards in a game
#   superflex_pass_td    : Passing TD value for superflex leagues (4 or 6)
#   two_point_conversion : Points for a successful 2-point conversion
#   sack_penalty         : Negative points per sack taken by QB
#
# NFL CONTEXT
# -----------
#   EPA definition: EP_after - EP_before (measures play value in expected points)
#   Two-point conversions use a SEPARATE nflfastR tracking mechanism from
#   standard scoring plays. They appear as play_type = "no_play" with
#   two_point_attempt = 1. They are NOT included in standard EPA calculations.
#   Sack penalty applies to the QB (passer_player_id), not the o-line.
#   First down bonus applies to rushing first downs (first_down_rush == 1)
#   and receiving first downs (first_down_pass == 1) only -- not passing TDs
#   that also happen to be first downs (those are already rewarded via TD points).
#
# BACKWARD COMPATIBILITY
# ----------------------
#   All Season 1 parameters are preserved with identical names and defaults:
#     pass_yd = 0.04, pass_td = 6, pass_int = -2, pick6_penalty = -4
#     rush_yd = 0.1, rush_td = 6
#     rec_yd = 0.1, rec_td = 6, ppr = 1
#     fumbles = -2, use_tiered_ppr = TRUE, te_premium = TRUE
#     rush_att_bonus = 0.25
#   The output schema is identical to Season 1 with additional columns appended.
#   Season 1 code that reads total_fantasy_points still works unchanged.
#
# DEPENDENCIES
# ------------
#   nflfastR  : Play-by-play data source
#   nflreadr  : Roster data for position anchoring
#   dplyr     : Data manipulation
#   tidyr     : Pivoting and reshaping
#   glue      : String formatting in messages
#   here      : Path resolution
#
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(glue)
library(here)

# ==============================================================================
# SECTION 1: CONSTANTS
# ==============================================================================

# Tiered PPR reception point values (Season 1 -- preserved exactly)
# Rewards explosive receptions over check-downs
TIERED_PPR_BREAKS <- c(
  yards_0_4   = 0.0,   # <5 yards: no bonus
  yards_5_9   = 0.5,   # 5-9 yards
  yards_10_19 = 1.0,   # 10-19 yards
  yards_20_29 = 1.5,   # 20-29 yards
  yards_30_39 = 2.0,   # 30-39 yards
  yards_40_up = 2.0    # 40+ yards
)

# TE premium (Season 1 -- preserved exactly)
TE_PREMIUM_PTS <- 0.5

# Two-point conversion result value in nflfastR
# two_point_conv_result == "success" means the play scored
TWO_PT_SUCCESS_VALUE <- "success"

# Minimum plays to consider a player active in a game (sanity check)
MIN_PLAYS_FOR_APPEARANCE <- 1L

# ==============================================================================
# SECTION 2: MAIN FUNCTION
# ==============================================================================

#' Calculate Fantasy Points with Extended Sleeper Scoring Options
#'
#' @description
#' Extends the Season 1 calculate_fantasy_points() to support the full range
#' of Sleeper league scoring options. All new parameters default to 0 (no
#' effect), ensuring 100% backward compatibility with Season 1 code.
#'
#' This function handles the six scoring categories independently:
#' passing, rushing, receiving, two-point conversions, first downs,
#' and long TD bonuses. Results are joined by player-game key.
#'
#' @param pbp_data Tibble. Play-by-play data from load_multi_season_pbp() or
#'   load_and_validate_pbp(). Must include CORE_COLUMNS from R/15_multi_season_pbp.R.
#' @param roster_data Optional tibble. Roster data from nflreadr::load_rosters().
#'   Required for TE premium (te_premium = TRUE). If NULL, TEs are not identified
#'   and te_premium has no effect.
#' @param season Optional integer vector. Season(s) to include. NULL = all seasons.
#' @param week_min Optional integer. Minimum week (inclusive). NULL = no lower bound.
#' @param week_max Optional integer. Maximum week (inclusive). NULL = no upper bound.
#'
#' # --- Season 1 parameters (preserved with identical defaults) ---
#' @param pass_yd Numeric. Points per passing yard. Default 0.04.
#' @param pass_td Numeric. Points per passing TD. Default 6.
#' @param pass_int Numeric. Points per interception thrown. Default -2.
#' @param pick6_penalty Numeric. Additional penalty when INT returned for TD.
#'   Added to pass_int. Default -4 (total pick-6 cost = -6).
#' @param rush_yd Numeric. Points per rushing yard. Default 0.1.
#' @param rush_td Numeric. Points per rushing TD. Default 6.
#' @param rec_yd Numeric. Points per receiving yard. Default 0.1.
#' @param rec_td Numeric. Points per receiving TD. Default 6.
#' @param ppr Numeric. Points per reception (flat rate). Ignored when
#'   use_tiered_ppr = TRUE. Default 1.
#' @param fumbles Numeric. Points lost per fumble lost. Default -2.
#' @param use_tiered_ppr Logical. Use tiered reception points based on
#'   yards gained (see TIERED_PPR_BREAKS). Default TRUE.
#' @param te_premium Logical. Add TE_PREMIUM_PTS (0.5) per TE reception.
#'   Requires roster_data. Default TRUE.
#' @param rush_att_bonus Numeric. Points per rushing attempt regardless of
#'   outcome. Default 0.25.
#'
#' # --- New Season 2 parameters (all default to 0 = no effect) ---
#' @param first_down_points Numeric. Bonus points for each first down earned
#'   via rush or reception (NOT pass completions that happen to gain a first
#'   down -- only the rusher/receiver earns this). Common Sleeper value: 0.5.
#'   Default 0.
#' @param long_td_bonus Numeric. Bonus points for a touchdown play that exceeds
#'   long_td_threshold yards. Applied on top of standard TD points.
#'   Common Sleeper value: 2.0. Default 0.
#' @param long_td_threshold Integer. Yardage threshold for long_td_bonus.
#'   Play yardage must be STRICTLY GREATER THAN this value.
#'   Common Sleeper values: 25, 40, 50. Default 40.
#' @param hundred_yard_bonus Numeric. Bonus for reaching 100 rushing or
#'   100 receiving yards in a single game. Applied once per player per game
#'   per category. Common Sleeper value: 3.0. Default 0.
#' @param superflex_pass_td Numeric. Passing TD value override for superflex
#'   leagues. When non-zero, this replaces pass_td. Allows quick toggling
#'   between 4-point and 6-point passing TD leagues. Default 0 (pass_td used).
#' @param two_point_conversion Numeric. Points for a successful 2-point
#'   conversion. Applies to both the passer (if passing 2PC) and the rusher/
#'   receiver who scores. In Sleeper this is typically awarded to the scorer.
#'   Common Sleeper value: 2.0. Default 0.
#' @param sack_penalty Numeric. Negative points per sack taken. Applied to
#'   the QB (passer_player_id). Common Sleeper value: -1.0. Default 0.
#'
#' # --- Sleeper parity parameters (all default to 0 / NULL = no effect) ---
#' @param tiered_rec_tiers Numeric vector of length 6, or NULL. Per-reception
#'   points by yardage bucket, in order: 0-4 yards, 5-9 yards, 10-19 yards,
#'   20-29 yards, 30-39 yards, 40+ yards. When NULL (default), uses
#'   TIERED_PPR_BREAKS (0, 0.5, 1.0, 1.5, 2.0, 2.0). Only applied when
#'   use_tiered_ppr = TRUE. Maps from Sleeper fields:
#'   rec_0_4, rec_5_9, rec_10_19, rec_20_29, rec_30_39, rec_40p.
#' @param bonus_pass_yd_300 Numeric. Bonus for 300+ passing yards in a game.
#'   Cumulative with bonus_pass_yd_400 (reaching 400 yards earns both).
#'   Common Sleeper value: 2.0. Default 0.
#' @param bonus_pass_yd_400 Numeric. Bonus for 400+ passing yards in a game.
#'   Common Sleeper value: 4.0. Default 0.
#' @param bonus_rec_yd_200 Numeric. Bonus for 200+ receiving yards in a game.
#'   Cumulative with hundred_yard_bonus (reaching 200 yards earns both).
#'   Common Sleeper value: 4.0. Default 0.
#' @param bonus_rush_yd_200 Numeric. Bonus for 200+ rushing yards in a game.
#'   Cumulative with hundred_yard_bonus. Common Sleeper value: 4.0. Default 0.
#' @param pass_2pt Numeric. Points awarded to the PASSER for a successful
#'   passing 2-point conversion. The scorer (receiver) still earns
#'   two_point_conversion points separately. Common Sleeper value: 2.0.
#'   Default 0.
#'
#' @return Tibble with one row per player per game (season + week + game_id +
#'   player_id). Columns:
#'   - season (int)
#'   - week (int)
#'   - game_id (chr)
#'   - player_id (chr)
#'   - player_name (chr)
#'   - position (chr): anchored to roster_data if provided, else inferred
#'   - team (chr)
#'   - pass_fantasy_points (dbl): Season 1 equivalent
#'   - rush_fantasy_points (dbl): Season 1 equivalent
#'   - rec_fantasy_points (dbl): Season 1 equivalent
#'   - two_pt_fantasy_points (dbl): NEW -- 2PC scoring only
#'   - first_down_fantasy_points (dbl): NEW -- first down bonus only
#'   - long_td_fantasy_points (dbl): NEW -- long TD bonus only
#'   - hundred_yard_fantasy_points (dbl): NEW -- yardage milestone bonus
#'     (100-yard + 200-yard bonuses combined, rush and receiving)
#'   - pass_milestone_fantasy_points (dbl): NEW -- QB passing yardage
#'     milestone bonuses (300-yard and 400-yard game bonuses combined)
#'   - total_fantasy_points (dbl): Sum of all components
#'   - pass_yards (dbl), pass_tds (int), pass_ints (int): raw stats
#'   - rush_yards (dbl), rush_tds (int), rush_attempts (int): raw stats
#'   - rec_yards (dbl), rec_tds (int), receptions (int), targets (int): raw stats
#'   - rush_first_downs (int), rec_first_downs (int): NEW raw counts
#'   - sacks_taken (int): NEW raw count
#'   - two_point_successes (int): NEW raw count
#'
#' @details
#' **Backward Compatibility:**
#' Calling with no new arguments is identical to Season 1 calculate_fantasy_points()
#' because all new parameters default to 0. The output has additional columns
#' (two_pt_fantasy_points, first_down_fantasy_points, etc.) but total_fantasy_points
#' is identical to Season 1 output when new parameters are at their defaults.
#'
#' **Two-Point Conversion Handling:**
#' Two-point attempts appear in nflfastR as two_point_attempt = 1 with
#' two_point_conv_result = "success" or "failure". These plays are NOT in
#' the standard EPA framework and use yardline_100 = 98 (2-yard line snap).
#' Points are awarded to the player who scores (rusher or receiver) and
#' optionally to the passer on a passing 2PC. This function awards points to
#' the scorer only (Sleeper default behavior). If you need passer credit on
#' passing 2PCs, set two_point_conversion on both the scoring role and adjust
#' pass_td accordingly -- this is outside standard Sleeper settings.
#'
#' **Sack Penalty Position Anchor:**
#' Sacks are coded in nflfastR as rush plays (play_type == "run" with sack == 1)
#' but attributed to passer_player_id. The sack penalty is applied to the QB
#' via passer_player_id, not rusher_player_id, to correctly attribute the penalty
#' to the quarterback who was sacked.
#'
#' **Long TD Bonus Measurement:**
#' For passing plays: yards_gained (the full play yards, i.e., receiving_yards).
#' For rushing plays: rushing_yards.
#' The RECEIVER earns the long TD bonus on passing plays (they gained the yards).
#' The PASSER does NOT earn a long TD bonus -- they earn standard pass_td points.
#' This matches Sleeper behavior.
#'
#' **First Down Attribution:**
#' first_down_rush earns the bonus for the rusher.
#' first_down_pass earns the bonus for the receiver.
#' The passer does NOT earn a first down bonus.
#'
#' @examples
#' \dontrun{
#' library(here)
#' source(here::here("R", "15_multi_season_pbp.R"))
#'
#' # Load a season
#' pbp <- load_normalized_season(2024)
#' roster <- nflreadr::load_rosters(seasons = 2024)
#'
#' # --- EXAMPLE 1: Default settings (identical to Season 1 output) ---
#' fp_default <- calculate_fantasy_points_ext(pbp, roster, season = 2024L)
#'
#' # --- EXAMPLE 2: Sleeper-style scoring ---
#' fp_sleeper <- calculate_fantasy_points_ext(
#'   pbp_data            = pbp,
#'   roster_data         = roster,
#'   season              = 2024L,
#'   first_down_points   = 0.5,
#'   long_td_bonus       = 2.0,
#'   long_td_threshold   = 40L,
#'   hundred_yard_bonus  = 3.0,
#'   two_point_conversion = 2.0,
#'   sack_penalty        = -1.0
#' )
#'
#' # --- EXAMPLE 3: Superflex league (6-point pass TDs) ---
#' fp_superflex <- calculate_fantasy_points_ext(
#'   pbp_data          = pbp,
#'   roster_data       = roster,
#'   season            = 2024L,
#'   superflex_pass_td = 6.0,
#'   first_down_points = 0.5
#' )
#'
#' # --- EXAMPLE 4: Standard scoring (no PPR, no bonuses, 4pt pass TDs) ---
#' fp_standard <- calculate_fantasy_points_ext(
#'   pbp_data       = pbp,
#'   roster_data    = roster,
#'   season         = 2024L,
#'   use_tiered_ppr = FALSE,
#'   te_premium     = FALSE,
#'   rush_att_bonus = 0,
#'   ppr            = 0,
#'   pass_td        = 4
#' )
#'
#' # --- EXAMPLE 5: Compare default vs Sleeper scoring ---
#' comparison <- compare_scoring_systems(
#'   pbp_data    = pbp,
#'   roster_data = roster,
#'   season      = 2024L,
#'   systems     = list(
#'     Default = list(),
#'     Sleeper = list(first_down_points = 0.5, hundred_yard_bonus = 3.0,
#'                    long_td_bonus = 2.0, two_point_conversion = 2.0)
#'   )
#' )
#' }
#'
#' @seealso
#' \code{\link{validate_ext_scoring_params}} to validate parameters before passing
#' \code{\link{compare_scoring_systems}} to compare multiple scoring systems
#' \code{\link{get_ext_scoring_defaults}} to inspect all default values
#'
#' @export
calculate_fantasy_points_ext <- function(
  pbp_data,
  roster_data          = NULL,
  season               = NULL,
  week_min             = NULL,
  week_max             = NULL,
  # --- Season 1 parameters (preserved exactly) ---
  pass_yd              = 0.04,
  pass_td              = 6,
  pass_int             = -2,
  pick6_penalty        = -4,
  rush_yd              = 0.1,
  rush_td              = 6,
  rec_yd               = 0.1,
  rec_td               = 6,
  ppr                  = 1,
  fumbles              = -2,
  use_tiered_ppr       = TRUE,
  te_premium           = TRUE,
  rush_att_bonus       = 0.25,
  # --- New Season 2 parameters (default to 0 = no effect) ---
  first_down_points    = 0,
  long_td_bonus        = 0,
  long_td_threshold    = 40L,
  hundred_yard_bonus   = 0,
  superflex_pass_td    = 0,
  two_point_conversion = 0,
  sack_penalty         = 0,
  # --- New Sleeper parity parameters (all default to 0 / NULL = no effect) ---
  tiered_rec_tiers  = NULL,
  bonus_pass_yd_300 = 0,
  bonus_pass_yd_400 = 0,
  bonus_rec_yd_200  = 0,
  bonus_rush_yd_200 = 0,
  pass_2pt          = 0
) {

  # ============================================================================
  # INPUT VALIDATION
  # ============================================================================

  if (!is.data.frame(pbp_data)) {
    stop("pbp_data must be a data frame")
  }

  required_cols <- c(
    "season", "week", "game_id", "play_type",
    "passer_player_id", "passer_player_name",
    "rusher_player_id", "rusher_player_name",
    "receiver_player_id", "receiver_player_name",
    "passing_yards", "rushing_yards", "receiving_yards",
    "pass_touchdown", "rush_touchdown",
    "interception", "fumble_lost",
    "posteam"
  )
  missing_cols <- setdiff(required_cols, names(pbp_data))
  if (length(missing_cols) > 0) {
    stop(glue(
      "pbp_data is missing required columns: {paste(missing_cols, collapse = ', ')}\n",
      "Load data via load_multi_season_pbp() or load_normalized_season()."
    ))
  }

  # Validate all numeric scoring parameters
  params_check <- validate_ext_scoring_params(
    pass_yd = pass_yd, pass_td = pass_td, pass_int = pass_int,
    pick6_penalty = pick6_penalty, rush_yd = rush_yd, rush_td = rush_td,
    rec_yd = rec_yd, rec_td = rec_td, ppr = ppr, fumbles = fumbles,
    rush_att_bonus = rush_att_bonus, first_down_points = first_down_points,
    long_td_bonus = long_td_bonus, long_td_threshold = long_td_threshold,
    hundred_yard_bonus = hundred_yard_bonus, superflex_pass_td = superflex_pass_td,
    two_point_conversion = two_point_conversion, sack_penalty = sack_penalty,
    tiered_rec_tiers = tiered_rec_tiers,
    bonus_pass_yd_300 = bonus_pass_yd_300, bonus_pass_yd_400 = bonus_pass_yd_400,
    bonus_rec_yd_200 = bonus_rec_yd_200, bonus_rush_yd_200 = bonus_rush_yd_200,
    pass_2pt = pass_2pt
  )
  if (!params_check$valid) {
    stop(paste(params_check$errors, collapse = "\n"))
  }

  # Resolve superflex pass TD: if non-zero, it overrides pass_td
  effective_pass_td <- if (superflex_pass_td != 0) superflex_pass_td else pass_td

  message("=== calculate_fantasy_points_ext() ===")
  message(glue("  Rows in pbp_data: {format(nrow(pbp_data), big.mark=',')}"))

  # ============================================================================
  # FILTER: SEASON, WEEK, REGULAR SEASON
  # ============================================================================

  pbp_filtered <- pbp_data

  # Regular season only (same as Season 1)
  if ("season_type" %in% names(pbp_filtered)) {
    pbp_filtered <- pbp_filtered %>%
      dplyr::filter(season_type == "REG")
  }

  if (!is.null(season)) {
    if (!is.numeric(season) || any(season < 1999) || any(season > 2030)) {
      stop("season must be a numeric vector of valid NFL seasons (1999-2030)")
    }
    pbp_filtered <- pbp_filtered %>% dplyr::filter(season %in% !!season)
  }

  if (!is.null(week_min)) {
    if (!is.numeric(week_min) || length(week_min) != 1 || week_min < 1) {
      stop("week_min must be a single positive integer")
    }
    pbp_filtered <- pbp_filtered %>% dplyr::filter(week >= !!week_min)
  }

  if (!is.null(week_max)) {
    if (!is.numeric(week_max) || length(week_max) != 1 || week_max < 1) {
      stop("week_max must be a single positive integer")
    }
    pbp_filtered <- pbp_filtered %>% dplyr::filter(week <= !!week_max)
  }

  message(glue("  Rows after filters: {format(nrow(pbp_filtered), big.mark=',')}"))

  if (nrow(pbp_filtered) == 0) {
    warning("No rows remain after filtering. Check season/week parameters.")
    return(.empty_ext_result())
  }

  # ============================================================================
  # BUILD POSITION LOOKUP FROM ROSTER DATA
  # ============================================================================

  position_lookup <- NULL
  if (!is.null(roster_data)) {
    # nflreadr::load_rosters() uses gsis_id, full_name, position, team, season
    # Guard against unexpected column names
    id_col   <- intersect(c("gsis_id", "player_id"), names(roster_data))[1]
    name_col <- intersect(c("full_name", "player_name"), names(roster_data))[1]
    pos_col  <- intersect(c("position"), names(roster_data))[1]

    if (!is.na(id_col) && !is.na(pos_col)) {
      position_lookup <- roster_data %>%
        dplyr::select(
          player_id = dplyr::all_of(id_col),
          position  = dplyr::all_of(pos_col)
        ) %>%
        dplyr::filter(!is.na(player_id), !is.na(position)) %>%
        dplyr::distinct(player_id, .keep_all = TRUE)

      message(glue("  Position lookup built: {nrow(position_lookup)} players from roster"))
    } else {
      warning(
        "roster_data does not have expected columns (gsis_id/full_name/position). ",
        "te_premium will not be applied."
      )
    }
  }

  # ============================================================================
  # BUILD COMPONENT SCORING TABLES
  # ============================================================================

  message("  Building scoring components...")

  # -- Passing --
  passing_pts <- .build_ext_passing_fantasy(
    pbp               = pbp_filtered,
    pass_yd           = pass_yd,
    pass_td           = effective_pass_td,
    pass_int          = pass_int,
    pick6_penalty     = pick6_penalty,
    sack_penalty      = sack_penalty,
    fumbles           = fumbles,
    bonus_pass_yd_300 = bonus_pass_yd_300,
    bonus_pass_yd_400 = bonus_pass_yd_400
  )
  message(glue("    Passing: {nrow(passing_pts)} player-games"))

  # -- Rushing --
  rushing_pts <- .build_ext_rushing_fantasy(
    pbp               = pbp_filtered,
    rush_yd           = rush_yd,
    rush_td           = rush_td,
    rush_att_bonus    = rush_att_bonus,
    fumbles           = fumbles,
    first_down_points = first_down_points,
    long_td_bonus     = long_td_bonus,
    long_td_threshold = long_td_threshold,
    hundred_yard_bonus = hundred_yard_bonus,
    bonus_rush_yd_200  = bonus_rush_yd_200
  )
  message(glue("    Rushing: {nrow(rushing_pts)} player-games"))

  # -- Receiving --
  receiving_pts <- .build_ext_receiving_fantasy(
    pbp               = pbp_filtered,
    rec_yd            = rec_yd,
    rec_td            = rec_td,
    ppr               = ppr,
    fumbles           = fumbles,
    use_tiered_ppr    = use_tiered_ppr,
    te_premium        = te_premium,
    position_lookup   = position_lookup,
    first_down_points = first_down_points,
    long_td_bonus     = long_td_bonus,
    long_td_threshold  = long_td_threshold,
    hundred_yard_bonus = hundred_yard_bonus,
    tiered_rec_tiers   = tiered_rec_tiers,
    bonus_rec_yd_200   = bonus_rec_yd_200
  )
  message(glue("    Receiving: {nrow(receiving_pts)} player-games"))

  # -- Two-point conversions --
  two_pt_pts <- .build_two_point_fantasy(
    pbp                  = pbp_filtered,
    two_point_conversion = two_point_conversion,
    pass_2pt             = pass_2pt
  )
  message(glue("    Two-point: {nrow(two_pt_pts)} player-games"))

  # ============================================================================
  # COMBINE ALL COMPONENTS
  # ============================================================================

  # Build a unified player-game spine from all component tables
  # Use full_join so players appear even if they only contributed in one category
  join_key <- c("season", "week", "game_id", "player_id", "player_name", "team")

  all_components <- list(passing_pts, rushing_pts, receiving_pts, two_pt_pts)

  # Each component carries a `position` column (the inferred role: QB/RB/WR).
  # `position` is NOT part of the player-game join key, so full_join auto-suffixes
  # it to position.x / position.y across components. That breaks the downstream
  # coalesce at the position_lookup step and the final select().
  # Strip position before joining; it is resolved after from the roster lookup
  # or inferred from scoring role when no roster_data is provided.
  combined <- purrr::reduce(
    purrr::map(all_components, ~ dplyr::select(.x, -dplyr::any_of("position"))),
    function(x, y) {
      dplyr::full_join(x, y, by = intersect(intersect(names(x), names(y)), join_key))
    }
  )

  # Replace NAs introduced by full_join with 0 for all point/stat columns.
  # These are the actual column names returned by each helper -- note that the
  # first_down, long_td, and hundred_yard components come back as split rush/rec
  # columns and are combined into the final output column names below.
  pt_cols <- c(
    # Season 1 base components
    "pass_fantasy_points", "rush_fantasy_points", "rec_fantasy_points",
    # Two-point component
    "two_pt_fantasy_points",
    # New Season 2 components from rushing helper
    "rush_first_down_pts", "long_td_rush_pts", "hundred_yard_rush_pts",
    # New Season 2 components from receiving helper
    "rec_first_down_pts", "long_td_rec_pts", "hundred_yard_rec_pts",
    # Raw stat columns
    "pass_yards", "pass_tds", "pass_ints", "sacks_taken",
    "rush_yards", "rush_tds", "rush_attempts", "rush_first_downs",
    "rec_yards", "rec_tds", "receptions", "targets", "rec_first_downs",
    "two_point_successes",
    # New passing milestone column
    "pass_milestone_pts"
  )

  combined <- combined %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(pt_cols),
        ~ dplyr::coalesce(.x, 0)
      )
    )

  # Combine rush + receiving components into the final named output columns.
  # This must happen after the coalesce so NAs are already zeroed out.
  combined <- combined %>%
    dplyr::mutate(
      first_down_fantasy_points   = dplyr::coalesce(rush_first_down_pts, 0) +
                                     dplyr::coalesce(rec_first_down_pts, 0),
      long_td_fantasy_points      = dplyr::coalesce(long_td_rush_pts, 0) +
                                     dplyr::coalesce(long_td_rec_pts, 0),
      hundred_yard_fantasy_points = dplyr::coalesce(hundred_yard_rush_pts, 0) +
                                     dplyr::coalesce(hundred_yard_rec_pts, 0),
      pass_milestone_fantasy_points = dplyr::coalesce(pass_milestone_pts, 0)
    )

  # Resolve position. position was stripped before the full_join to prevent
  # column-suffix collisions (position.x / position.y). Reattach it here.
  # When roster_data is provided, anchor to the roster (accurate for dual-role
  # players like QBs who rush). When not, infer from scoring role: QB has
  # passing points, RB has rushing but no passing, everything else is WR.
  if (!is.null(position_lookup)) {
    combined <- combined %>%
      dplyr::left_join(
        position_lookup %>% dplyr::rename(position_roster = position),
        by = "player_id"
      ) %>%
      dplyr::mutate(
        position = dplyr::coalesce(position_roster, NA_character_)
      ) %>%
      dplyr::select(-position_roster)
  } else {
    combined <- combined %>%
      dplyr::mutate(
        position = dplyr::case_when(
          dplyr::coalesce(pass_fantasy_points, 0) != 0                        ~ "QB",
          dplyr::coalesce(rush_fantasy_points, 0) != 0 &
            dplyr::coalesce(pass_fantasy_points, 0) == 0                      ~ "RB",
          TRUE                                                                 ~ "WR"
        )
      )
  }

  # ============================================================================
  # COMPUTE TOTAL FANTASY POINTS
  # ============================================================================

  combined <- combined %>%
    dplyr::mutate(
      total_fantasy_points =
        dplyr::coalesce(pass_fantasy_points,         0) +
        dplyr::coalesce(rush_fantasy_points,         0) +
        dplyr::coalesce(rec_fantasy_points,          0) +
        dplyr::coalesce(two_pt_fantasy_points,       0) +
        dplyr::coalesce(first_down_fantasy_points,   0) +
        dplyr::coalesce(long_td_fantasy_points,        0) +
        dplyr::coalesce(hundred_yard_fantasy_points,   0) +
        dplyr::coalesce(pass_milestone_fantasy_points, 0)
    )

  # ============================================================================
  # FINAL COLUMN ORDER
  # ============================================================================

  combined <- combined %>%
    dplyr::select(
      # Keys
      season, week, game_id, player_id, player_name, position, team,
      # Point components (Season 1 cols first for backward compat)
      pass_fantasy_points, rush_fantasy_points, rec_fantasy_points,
      # New Season 2 components
      two_pt_fantasy_points, first_down_fantasy_points,
      long_td_fantasy_points, hundred_yard_fantasy_points,
      # Passing milestone bonuses (new Sleeper parity)
      pass_milestone_fantasy_points,
      # Total (Season 1 col name preserved)
      total_fantasy_points,
      # Raw stats (Season 1 set)
      dplyr::any_of(c(
        "pass_yards", "pass_tds", "pass_ints",
        "rush_yards", "rush_tds", "rush_attempts",
        "rec_yards", "rec_tds", "receptions", "targets"
      )),
      # New raw stats
      dplyr::any_of(c(
        "rush_first_downs", "rec_first_downs",
        "sacks_taken", "two_point_successes"
      ))
    ) %>%
    dplyr::arrange(season, week, desc(total_fantasy_points))

  message(glue(
    "  Complete. {nrow(combined)} player-games | ",
    "{n_distinct(combined$player_id)} unique players"
  ))

  return(combined)
}

# ==============================================================================
# SECTION 3: INTERNAL HELPER -- PASSING
# ==============================================================================

#' Build Passing Fantasy Points Component
#'
#' @description Internal. Aggregates passing fantasy points by player-game.
#' Excludes two-point attempts, QB kneels, and QB spikes.
#' Sack penalty is applied here because sacks are attributed to passer_player_id.
#'
#' @param pbp Filtered play-by-play tibble
#' @param pass_yd Numeric. Points per passing yard.
#' @param pass_td Numeric. Points per passing TD (already resolved for superflex).
#' @param pass_int Numeric. Points per interception.
#' @param pick6_penalty Numeric. Additional penalty for pick-6.
#' @param sack_penalty Numeric. Points per sack taken (negative).
#' @param fumbles Numeric. Points per fumble lost.
#'
#' @return Tibble with columns: season, week, game_id, player_id, player_name,
#'   team, position, pass_yards, pass_tds, pass_ints, sacks_taken,
#'   pass_fantasy_points.
#'
#' @keywords internal
.build_ext_passing_fantasy <- function(
  pbp, pass_yd, pass_td, pass_int, pick6_penalty, sack_penalty, fumbles,
  bonus_pass_yd_300 = 0, bonus_pass_yd_400 = 0
) {

  # Guard: coalesce optional columns to 0 before use
  qb_spike_vec <- if ("qb_spike" %in% names(pbp))
    dplyr::coalesce(pbp$qb_spike, 0L) else rep(0L, nrow(pbp))

  two_pt_vec <- if ("two_point_attempt" %in% names(pbp))
    dplyr::coalesce(pbp$two_point_attempt, 0L) else rep(0L, nrow(pbp))

  # interception_player_id identifies who returned the INT;
  # return_touchdown identifies if any play was returned for TD.
  # In nflfastR, return_touchdown == 1 on the same play as interception == 1
  # means it was a pick-6. We guard for column absence.
  has_return_td <- "return_touchdown" %in% names(pbp)

  # ---- Pass plays: pass_attempt OR sack, excluding spikes/two-points ----
  pass_plays <- pbp %>%
    dplyr::filter(
      !is.na(passer_player_id),
      !is.na(passer_player_name),
      (
        dplyr::coalesce(pass_attempt, 0L) == 1L |
        dplyr::coalesce(sack, 0L) == 1L
      ),
      qb_spike_vec == 0L,
      two_pt_vec == 0L
    )

  if (nrow(pass_plays) == 0) {
    return(.empty_passing_component())
  }

  pass_agg <- pass_plays %>%
    dplyr::group_by(
      season, week, game_id,
      player_id   = passer_player_id,
      player_name = passer_player_name
    ) %>%
    dplyr::summarise(
      team        = dplyr::last(posteam),
      position    = "QB",

      # Raw stat columns
      pass_yards  = sum(passing_yards, na.rm = TRUE),
      pass_tds    = as.integer(sum(
        dplyr::coalesce(pass_touchdown, 0L) == 1L, na.rm = TRUE
      )),
      pass_ints   = as.integer(sum(
        dplyr::coalesce(interception, 0L) == 1L, na.rm = TRUE
      )),
      pick6_count = as.integer(if (has_return_td) {
        sum(
          dplyr::coalesce(interception, 0L) == 1L &
          dplyr::coalesce(return_touchdown, 0L) == 1L,
          na.rm = TRUE
        )
      } else 0L),
      sacks_taken = as.integer(sum(
        dplyr::coalesce(sack, 0L) == 1L, na.rm = TRUE
      )),
      pass_fumbles_lost = as.integer(sum(
        dplyr::coalesce(fumble_lost, 0L) == 1L &
        passer_player_id == fumbled_1_player_id,
        na.rm = TRUE
      )),

      .groups = "drop"
    ) %>%
    dplyr::mutate(
      pass_fantasy_points =
        (pass_yards      * pass_yd) +
        (pass_tds        * pass_td) +
        (pass_ints       * pass_int) +
        (pick6_count     * pick6_penalty) +
        (sacks_taken     * sack_penalty) +
        (pass_fumbles_lost * fumbles),
      # Passing yardage milestone bonuses (cumulative: 400+ also earns 300+ bonus)
      pass_milestone_pts =
        dplyr::if_else(pass_yards >= 300, bonus_pass_yd_300, 0) +
        dplyr::if_else(pass_yards >= 400, bonus_pass_yd_400, 0)
    ) %>%
    dplyr::select(
      season, week, game_id, player_id, player_name, team, position,
      pass_yards, pass_tds, pass_ints, sacks_taken,
      pass_fantasy_points, pass_milestone_pts
    )

  return(pass_agg)
}

# helper to return typed empty tibble for passing component
.empty_passing_component <- function() {
  tibble::tibble(
    season = integer(), week = integer(), game_id = character(),
    player_id = character(), player_name = character(),
    team = character(), position = character(),
    pass_yards = numeric(), pass_tds = integer(), pass_ints = integer(),
    sacks_taken = integer(), pass_fantasy_points = numeric(),
    pass_milestone_pts = numeric()
  )
}

# ==============================================================================
# SECTION 4: INTERNAL HELPER -- RUSHING
# ==============================================================================

#' Build Rushing Fantasy Points Component
#'
#' @description Internal. Aggregates rushing fantasy points by player-game.
#' Excludes QB kneels, QB spikes, and two-point attempts.
#' Sacks are NOT included here -- they appear in passing component.
#' Long TD bonus for rushing TDs is computed here.
#'
#' @param pbp Filtered play-by-play tibble
#' @param rush_yd,rush_td,rush_att_bonus,fumbles Numeric. Scoring parameters.
#' @param first_down_points Numeric. Bonus per first down.
#' @param long_td_bonus Numeric. Bonus for long TDs.
#' @param long_td_threshold Integer. Yardage threshold for long TD bonus.
#' @param hundred_yard_bonus Numeric. Bonus for 100+ rushing yards in a game.
#'
#' @return Tibble with columns: season, week, game_id, player_id, player_name,
#'   team, position, rush_yards, rush_tds, rush_attempts, rush_first_downs,
#'   rush_fantasy_points, long_td_fantasy_points, hundred_yard_fantasy_points.
#'   Note: first_down_fantasy_points and long/hundred bonuses are stored in
#'   their own columns so totals are transparent. first_down is separate
#'   because it also has a receiving component -- we combine them in the main
#'   function.
#'
#' @keywords internal
.build_ext_rushing_fantasy <- function(
  pbp, rush_yd, rush_td, rush_att_bonus, fumbles,
  first_down_points, long_td_bonus, long_td_threshold, hundred_yard_bonus,
  bonus_rush_yd_200 = 0
) {

  # Guard optional columns
  qb_kneel_vec <- if ("qb_kneel" %in% names(pbp))
    dplyr::coalesce(pbp$qb_kneel, 0L) else rep(0L, nrow(pbp))

  qb_spike_vec <- if ("qb_spike" %in% names(pbp))
    dplyr::coalesce(pbp$qb_spike, 0L) else rep(0L, nrow(pbp))

  two_pt_vec <- if ("two_point_attempt" %in% names(pbp))
    dplyr::coalesce(pbp$two_point_attempt, 0L) else rep(0L, nrow(pbp))

  sack_vec <- if ("sack" %in% names(pbp))
    dplyr::coalesce(pbp$sack, 0L) else rep(0L, nrow(pbp))

  has_first_down_rush <- "first_down_rush" %in% names(pbp)

  rush_plays <- pbp %>%
    dplyr::filter(
      !is.na(rusher_player_id),
      !is.na(rusher_player_name),
      play_type %in% c("run"),
      qb_kneel_vec == 0L,
      qb_spike_vec == 0L,
      two_pt_vec   == 0L,
      sack_vec     == 0L   # Sacks are rush-coded but go to passer component
    )

  if (nrow(rush_plays) == 0) {
    return(.empty_rushing_component())
  }

  # Determine if rush was a long TD (rushing_yards > threshold AND rush_touchdown)
  rush_plays <- rush_plays %>%
    dplyr::mutate(
      is_long_rush_td = (
        dplyr::coalesce(rush_touchdown, 0L) == 1L &
        dplyr::coalesce(rushing_yards, 0)  > long_td_threshold
      )
    )

  rush_agg <- rush_plays %>%
    dplyr::group_by(
      season, week, game_id,
      player_id   = rusher_player_id,
      player_name = rusher_player_name
    ) %>%
    dplyr::summarise(
      team          = dplyr::last(posteam),
      position      = "RB",   # will be overridden by roster lookup in caller

      # Raw stats
      rush_yards    = sum(rushing_yards, na.rm = TRUE),
      rush_tds      = as.integer(sum(
        dplyr::coalesce(rush_touchdown, 0L) == 1L, na.rm = TRUE
      )),
      rush_attempts = as.integer(dplyr::n()),
      rush_first_downs = as.integer(if (has_first_down_rush) {
        sum(dplyr::coalesce(first_down_rush, 0L) == 1L, na.rm = TRUE)
      } else 0L),
      rush_fumbles_lost = as.integer(sum(
        dplyr::coalesce(fumble_lost, 0L) == 1L &
        rusher_player_id == fumbled_1_player_id,
        na.rm = TRUE
      )),

      # Long TD count (for bonus)
      long_rush_tds = as.integer(sum(is_long_rush_td, na.rm = TRUE)),

      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # Standard rush points (Season 1 equivalent)
      rush_fantasy_points =
        (rush_yards      * rush_yd) +
        (rush_tds        * rush_td) +
        (rush_attempts   * rush_att_bonus) +
        (rush_fumbles_lost * fumbles),

      # First down bonus (rushing portion -- combined with receiving in caller)
      rush_first_down_pts = rush_first_downs * first_down_points,

      # Long TD bonus (rushing)
      long_td_rush_pts = long_rush_tds * long_td_bonus,

      # Yardage milestone bonus (cumulative: 200+ yards also earns 100-yard bonus)
      hundred_yard_rush_pts =
        dplyr::if_else(rush_yards >= 100, hundred_yard_bonus, 0) +
        dplyr::if_else(rush_yards >= 200, bonus_rush_yd_200, 0)
    ) %>%
    dplyr::select(
      season, week, game_id, player_id, player_name, team, position,
      rush_yards, rush_tds, rush_attempts, rush_first_downs,
      rush_fantasy_points,
      rush_first_down_pts,
      long_td_rush_pts,
      hundred_yard_rush_pts
    )

  return(rush_agg)
}

.empty_rushing_component <- function() {
  tibble::tibble(
    season = integer(), week = integer(), game_id = character(),
    player_id = character(), player_name = character(),
    team = character(), position = character(),
    rush_yards = numeric(), rush_tds = integer(),
    rush_attempts = integer(), rush_first_downs = integer(),
    rush_fantasy_points = numeric(),
    rush_first_down_pts = numeric(),
    long_td_rush_pts = numeric(),
    hundred_yard_rush_pts = numeric()
  )
}

# ==============================================================================
# SECTION 5: INTERNAL HELPER -- RECEIVING
# ==============================================================================

#' Build Receiving Fantasy Points Component
#'
#' @description Internal. Aggregates receiving fantasy points by player-game.
#' Handles tiered PPR, TE premium, first down bonus, long TD bonus,
#' and 100-yard receiving bonus. Excludes two-point attempts.
#'
#' @param pbp Filtered play-by-play tibble
#' @param rec_yd,rec_td,ppr,fumbles Numeric. Scoring parameters.
#' @param use_tiered_ppr Logical. Use tiered reception scoring.
#' @param te_premium Logical. Add TE premium per reception.
#' @param position_lookup Optional tibble with player_id, position columns.
#' @param first_down_points,long_td_bonus,long_td_threshold,hundred_yard_bonus
#'   Numeric. New Season 2 parameters.
#'
#' @return Tibble with columns for receiving fantasy points and raw stats.
#'
#' @keywords internal
.build_ext_receiving_fantasy <- function(
  pbp, rec_yd, rec_td, ppr, fumbles,
  use_tiered_ppr, te_premium, position_lookup,
  first_down_points, long_td_bonus, long_td_threshold, hundred_yard_bonus,
  tiered_rec_tiers = NULL, bonus_rec_yd_200 = 0
) {

  two_pt_vec <- if ("two_point_attempt" %in% names(pbp))
    dplyr::coalesce(pbp$two_point_attempt, 0L) else rep(0L, nrow(pbp))

  has_first_down_pass <- "first_down_pass" %in% names(pbp)

  # Filter to completed receptions and targets (complete_pass for receptions,
  # pass_attempt for targets). Exclude two-point attempts and incomplete passes
  # for yardage but keep targets for counting.
  target_plays <- pbp %>%
    dplyr::filter(
      !is.na(receiver_player_id),
      !is.na(receiver_player_name),
      dplyr::coalesce(pass_attempt, 0L) == 1L,
      two_pt_vec == 0L
    )

  if (nrow(target_plays) == 0) {
    return(.empty_receiving_component())
  }

  # Resolve active tier values once (before play-level mutation for performance)
  # Order: 0-4 yds, 5-9 yds, 10-19 yds, 20-29 yds, 30-39 yds, 40+ yds
  active_tiers <- as.numeric(
    if (!is.null(tiered_rec_tiers)) tiered_rec_tiers else TIERED_PPR_BREAKS
  )
  if (length(active_tiers) != 6L) {
    stop("tiered_rec_tiers must have exactly 6 values.", call. = FALSE)
  }

  # Flag long TDs at play level
  target_plays <- target_plays %>%
    dplyr::mutate(
      is_reception  = dplyr::coalesce(complete_pass, 0L) == 1L,
      is_long_rec_td = (
        dplyr::coalesce(pass_touchdown, 0L) == 1L &
        dplyr::coalesce(receiving_yards, 0) > long_td_threshold
      ),
      # Tiered PPR: per-play reception value using active_tiers
      # (either from tiered_rec_tiers param or TIERED_PPR_BREAKS default)
      tiered_ppr_pts = dplyr::case_when(
        !is_reception                              ~ 0,
        dplyr::coalesce(receiving_yards, 0) < 5   ~ active_tiers[1],
        dplyr::coalesce(receiving_yards, 0) < 10  ~ active_tiers[2],
        dplyr::coalesce(receiving_yards, 0) < 20  ~ active_tiers[3],
        dplyr::coalesce(receiving_yards, 0) < 30  ~ active_tiers[4],
        dplyr::coalesce(receiving_yards, 0) < 40  ~ active_tiers[5],
        TRUE                                       ~ active_tiers[6]
      )
    )

  rec_agg <- target_plays %>%
    dplyr::group_by(
      season, week, game_id,
      player_id   = receiver_player_id,
      player_name = receiver_player_name
    ) %>%
    dplyr::summarise(
      team             = dplyr::last(posteam),
      position         = "WR",   # overridden by roster lookup

      targets          = as.integer(dplyr::n()),
      receptions       = as.integer(sum(is_reception, na.rm = TRUE)),
      rec_yards        = sum(
        dplyr::if_else(is_reception, dplyr::coalesce(receiving_yards, 0), 0),
        na.rm = TRUE
      ),
      rec_tds          = as.integer(sum(
        dplyr::coalesce(pass_touchdown, 0L) == 1L, na.rm = TRUE
      )),
      rec_first_downs  = as.integer(if (has_first_down_pass) {
        sum(dplyr::coalesce(first_down_pass, 0L) == 1L, na.rm = TRUE)
      } else 0L),
      rec_fumbles_lost = as.integer(sum(
        dplyr::coalesce(fumble_lost, 0L) == 1L &
        receiver_player_id == fumbled_1_player_id,
        na.rm = TRUE
      )),
      long_rec_tds     = as.integer(sum(is_long_rec_td, na.rm = TRUE)),
      tiered_ppr_total = sum(tiered_ppr_pts, na.rm = TRUE),

      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # Determine reception point value.
      # use_tiered_ppr is a scalar logical -- use base if/else, not dplyr::if_else()
      # (dplyr::if_else requires condition to be the same length as true/false vectors)
      rec_pts_per_play = if (use_tiered_ppr) tiered_ppr_total else receptions * ppr,

      # Standard receiving points (Season 1 equivalent)
      rec_fantasy_points =
        (rec_yards       * rec_yd) +
        (rec_tds         * rec_td) +
        rec_pts_per_play +
        (rec_fumbles_lost * fumbles),

      # First down bonus (receiving portion)
      rec_first_down_pts = rec_first_downs * first_down_points,

      # Long TD bonus (receiving)
      long_td_rec_pts = long_rec_tds * long_td_bonus,

      # Yardage milestone bonus (cumulative: 200+ yards also earns 100-yard bonus)
      hundred_yard_rec_pts =
        dplyr::if_else(rec_yards >= 100, hundred_yard_bonus, 0) +
        dplyr::if_else(rec_yards >= 200, bonus_rec_yd_200, 0)
    )

  # Apply TE premium if requested and roster lookup is available
  if (te_premium && !is.null(position_lookup)) {
    rec_agg <- rec_agg %>%
      dplyr::left_join(
        position_lookup %>% dplyr::rename(pos_from_roster = position),
        by = "player_id"
      ) %>%
      dplyr::mutate(
        te_premium_pts = dplyr::if_else(
          dplyr::coalesce(pos_from_roster, "") == "TE",
          receptions * TE_PREMIUM_PTS,
          0
        ),
        rec_fantasy_points = rec_fantasy_points + te_premium_pts
      ) %>%
      dplyr::select(-pos_from_roster, -te_premium_pts)
  }

  rec_agg <- rec_agg %>%
    dplyr::select(
      season, week, game_id, player_id, player_name, team, position,
      targets, receptions, rec_yards, rec_tds, rec_first_downs,
      rec_fantasy_points,
      rec_first_down_pts,
      long_td_rec_pts,
      hundred_yard_rec_pts
    )

  return(rec_agg)
}

.empty_receiving_component <- function() {
  tibble::tibble(
    season = integer(), week = integer(), game_id = character(),
    player_id = character(), player_name = character(),
    team = character(), position = character(),
    targets = integer(), receptions = integer(),
    rec_yards = numeric(), rec_tds = integer(), rec_first_downs = integer(),
    rec_fantasy_points = numeric(),
    rec_first_down_pts = numeric(),
    long_td_rec_pts = numeric(),
    hundred_yard_rec_pts = numeric()
  )
}

# ==============================================================================
# SECTION 6: INTERNAL HELPER -- TWO-POINT CONVERSIONS
# ==============================================================================

#' Build Two-Point Conversion Fantasy Points Component
#'
#' @description Internal. Awards two_point_conversion points to the player who
#' scored the 2-point conversion (rusher or receiver). In nflfastR, 2-point
#' attempts appear as two_point_attempt = 1 with two_point_conv_result =
#' "success" or "failure". This function awards points to the scorer only.
#'
#' NFL context: Two-point conversions snap from the 2-yard line. They exist
#' outside the standard EPA framework. Only successful conversions score.
#'
#' @param pbp Filtered play-by-play tibble
#' @param two_point_conversion Numeric. Points per successful 2-point conversion.
#'
#' @return Tibble with player-game level two-point conversion data.
#'
#' @keywords internal
.build_two_point_fantasy <- function(pbp, two_point_conversion, pass_2pt = 0) {

  # Short-circuit: if parameter is 0, no need to process
  if (two_point_conversion == 0 && pass_2pt == 0) {
    return(.empty_two_point_component())
  }

  # Guard: two_point_attempt may not exist in all pbp versions
  if (!("two_point_attempt" %in% names(pbp))) {
    warning(
      "two_point_attempt column not found in pbp_data. ",
      "two_point_conversion scoring cannot be calculated."
    )
    return(.empty_two_point_component())
  }

  two_pt_plays <- pbp %>%
    dplyr::filter(
      dplyr::coalesce(two_point_attempt, 0L) == 1L,
      dplyr::coalesce(two_point_conv_result, "") == TWO_PT_SUCCESS_VALUE
    )

  if (nrow(two_pt_plays) == 0) {
    return(.empty_two_point_component())
  }

  # Identify scorer: rusher on rush 2PC, receiver on pass 2PC
  # Two-point rush: rusher_player_id is not NA
  # Two-point pass: receiver_player_id is not NA (receiver scores, not passer)
  two_pt_rush <- two_pt_plays %>%
    dplyr::filter(!is.na(rusher_player_id)) %>%
    dplyr::group_by(
      season, week, game_id,
      player_id   = rusher_player_id,
      player_name = rusher_player_name
    ) %>%
    dplyr::summarise(
      team                 = dplyr::last(posteam),
      two_point_successes  = as.integer(dplyr::n()),
      .groups              = "drop"
    ) %>%
    dplyr::mutate(position = "RB")

  two_pt_rec <- two_pt_plays %>%
    dplyr::filter(!is.na(receiver_player_id)) %>%
    dplyr::group_by(
      season, week, game_id,
      player_id   = receiver_player_id,
      player_name = receiver_player_name
    ) %>%
    dplyr::summarise(
      team                 = dplyr::last(posteam),
      two_point_successes  = as.integer(dplyr::n()),
      .groups              = "drop"
    ) %>%
    dplyr::mutate(position = "WR")

  two_pt_combined <- dplyr::bind_rows(two_pt_rush, two_pt_rec) %>%
    dplyr::mutate(
      two_pt_fantasy_points = two_point_successes * two_point_conversion
    ) %>%
    dplyr::select(
      season, week, game_id, player_id, player_name, team, position,
      two_point_successes, two_pt_fantasy_points
    )

  # --- Passer credit on passing 2-point conversions ---
  # Awarded to the QB separately from scorer credit. A receiving 2PC
  # is identified by receiver_player_id being non-NA on the 2PC play.
  if (pass_2pt != 0) {
    two_pt_pass_credit <- two_pt_plays %>%
      dplyr::filter(
        !is.na(passer_player_id),
        !is.na(passer_player_name),
        !is.na(receiver_player_id)  # confirms this was a passing 2PC
      ) %>%
      dplyr::group_by(
        season, week, game_id,
        player_id   = passer_player_id,
        player_name = passer_player_name
      ) %>%
      dplyr::summarise(
        team                  = dplyr::last(posteam),
        two_point_successes   = as.integer(dplyr::n()),
        .groups               = "drop"
      ) %>%
      dplyr::mutate(
        position              = "QB",
        two_pt_fantasy_points = two_point_successes * pass_2pt
      ) %>%
      dplyr::select(
        season, week, game_id, player_id, player_name, team, position,
        two_point_successes, two_pt_fantasy_points
      )

    two_pt_combined <- dplyr::bind_rows(two_pt_combined, two_pt_pass_credit)
  }

  # Collapse duplicate player-game rows (e.g., QB rushes for a 2PC AND
  # throws a passing 2PC in the same game -- appears in both tables)
  two_pt_combined <- two_pt_combined %>%
    dplyr::group_by(
      season, week, game_id, player_id, player_name, team, position
    ) %>%
    dplyr::summarise(
      two_point_successes   = sum(two_point_successes, na.rm = TRUE),
      two_pt_fantasy_points = sum(two_pt_fantasy_points, na.rm = TRUE),
      .groups               = "drop"
    )

  return(two_pt_combined)
}

.empty_two_point_component <- function() {
  tibble::tibble(
    season = integer(), week = integer(), game_id = character(),
    player_id = character(), player_name = character(),
    team = character(), position = character(),
    two_point_successes = integer(),
    two_pt_fantasy_points = numeric()
  )
}

# Empty result for full function
.empty_ext_result <- function() {
  tibble::tibble(
    season = integer(), week = integer(), game_id = character(),
    player_id = character(), player_name = character(),
    position = character(), team = character(),
    pass_fantasy_points = numeric(), rush_fantasy_points = numeric(),
    rec_fantasy_points = numeric(), two_pt_fantasy_points = numeric(),
    first_down_fantasy_points = numeric(), long_td_fantasy_points = numeric(),
    hundred_yard_fantasy_points = numeric(), total_fantasy_points = numeric()
  )
}

# ==============================================================================
# SECTION 7: PARAMETER VALIDATOR
# ==============================================================================

#' Validate Extended Scoring Parameters
#'
#' @description
#' Validates all scoring parameters before passing to calculate_fantasy_points_ext().
#' Returns a list with $valid (logical) and $errors (character vector).
#'
#' @param ... Named scoring parameters matching calculate_fantasy_points_ext() args.
#'
#' @return List with:
#'   - valid (logical): TRUE if all parameters pass
#'   - errors (character): Vector of error messages (empty if valid)
#'   - warnings (character): Vector of advisory messages
#'
#' @examples
#' \dontrun{
#' result <- validate_ext_scoring_params(
#'   pass_yd = 0.04, pass_td = 6, pass_int = -2,
#'   long_td_threshold = 40L
#' )
#' if (!result$valid) stop(paste(result$errors, collapse = "\n"))
#' }
#'
#' @export
validate_ext_scoring_params <- function(
  pass_yd = 0.04, pass_td = 6, pass_int = -2, pick6_penalty = -4,
  rush_yd = 0.1, rush_td = 6,
  rec_yd = 0.1, rec_td = 6, ppr = 1, fumbles = -2,
  rush_att_bonus = 0.25,
  first_down_points = 0, long_td_bonus = 0, long_td_threshold = 40L,
  hundred_yard_bonus = 0, superflex_pass_td = 0,
  two_point_conversion = 0, sack_penalty = 0,
  tiered_rec_tiers  = NULL,
  bonus_pass_yd_300 = 0, bonus_pass_yd_400 = 0,
  bonus_rec_yd_200  = 0, bonus_rush_yd_200  = 0,
  pass_2pt          = 0
) {
  errors   <- character(0)
  warnings <- character(0)

  # Helper: check numeric scalar
  assert_numeric_scalar <- function(x, name) {
    if (!is.numeric(x) || length(x) != 1 || is.na(x)) {
      errors <<- c(errors, glue("{name} must be a single non-NA numeric value"))
    }
  }

  # Season 1 parameters
  assert_numeric_scalar(pass_yd,       "pass_yd")
  assert_numeric_scalar(pass_td,       "pass_td")
  assert_numeric_scalar(pass_int,      "pass_int")
  assert_numeric_scalar(pick6_penalty, "pick6_penalty")
  assert_numeric_scalar(rush_yd,       "rush_yd")
  assert_numeric_scalar(rush_td,       "rush_td")
  assert_numeric_scalar(rec_yd,        "rec_yd")
  assert_numeric_scalar(rec_td,        "rec_td")
  assert_numeric_scalar(ppr,           "ppr")
  assert_numeric_scalar(fumbles,       "fumbles")
  assert_numeric_scalar(rush_att_bonus, "rush_att_bonus")

  # Season 2 parameters
  assert_numeric_scalar(first_down_points,    "first_down_points")
  assert_numeric_scalar(long_td_bonus,        "long_td_bonus")
  assert_numeric_scalar(long_td_threshold,    "long_td_threshold")
  assert_numeric_scalar(hundred_yard_bonus,   "hundred_yard_bonus")
  assert_numeric_scalar(superflex_pass_td,    "superflex_pass_td")
  assert_numeric_scalar(two_point_conversion, "two_point_conversion")
  assert_numeric_scalar(sack_penalty,         "sack_penalty")

  # Sleeper parity parameters
  if (!is.null(tiered_rec_tiers)) {
    if (!is.numeric(tiered_rec_tiers) || length(tiered_rec_tiers) != 6L) {
      errors <- c(errors,
        "tiered_rec_tiers must be NULL or a numeric vector of exactly 6 values")
    }
  }
  assert_numeric_scalar(bonus_pass_yd_300, "bonus_pass_yd_300")
  assert_numeric_scalar(bonus_pass_yd_400, "bonus_pass_yd_400")
  assert_numeric_scalar(bonus_rec_yd_200,  "bonus_rec_yd_200")
  assert_numeric_scalar(bonus_rush_yd_200, "bonus_rush_yd_200")
  assert_numeric_scalar(pass_2pt,          "pass_2pt")

  # Domain checks
  if (is.numeric(pass_int) && pass_int > 0) {
    warnings <- c(warnings,
      "pass_int is positive -- interceptions will award points. Verify intent.")
  }
  if (is.numeric(fumbles) && fumbles > 0) {
    warnings <- c(warnings,
      "fumbles is positive -- fumbles lost will award points. Verify intent.")
  }
  if (is.numeric(sack_penalty) && sack_penalty > 0) {
    warnings <- c(warnings,
      "sack_penalty is positive -- sacks will award points to QB. Verify intent.")
  }
  if (is.numeric(long_td_threshold) && long_td_threshold < 10) {
    warnings <- c(warnings, glue(
      "long_td_threshold = {long_td_threshold} is very low. ",
      "Most short passes will trigger the bonus."
    ))
  }
  if (is.numeric(superflex_pass_td) && superflex_pass_td != 0 &&
      is.numeric(pass_td) && superflex_pass_td == pass_td) {
    warnings <- c(warnings,
      "superflex_pass_td equals pass_td -- superflex_pass_td has no effect.")
  }

  list(
    valid    = length(errors) == 0,
    errors   = errors,
    warnings = warnings
  )
}

# ==============================================================================
# SECTION 8: SCORING SYSTEM COMPARATOR
# ==============================================================================

#' Compare Fantasy Scoring Systems
#'
#' @description
#' Runs calculate_fantasy_points_ext() under multiple named scoring systems and
#' returns a wide tibble showing each player's total points per system.
#' Used for LinkedIn visualization and league comparison analysis.
#'
#' @param pbp_data Tibble. Play-by-play data.
#' @param roster_data Optional tibble. Roster data.
#' @param season Optional integer vector. Seasons to include.
#' @param week_min,week_max Optional integers. Week range.
#' @param systems Named list of lists. Each element is a named list of
#'   scoring parameter overrides. The default element (empty list) uses
#'   Season 1 defaults. Example:
#'   \code{list(Default = list(), Sleeper = list(first_down_points = 0.5))}
#' @param min_games Integer. Minimum games played to include a player.
#'   Default 4 (prevents noise from small samples).
#'
#' @return Tibble with columns: player_id, player_name, position, team,
#'   games_played, one column per system (named by systems list names)
#'   containing total_fantasy_points, and one column per system for ppg
#'   (points per game).
#'
#' @examples
#' \dontrun{
#' pbp <- load_normalized_season(2024)
#' roster <- nflreadr::load_rosters(seasons = 2024)
#'
#' comparison <- compare_scoring_systems(
#'   pbp_data    = pbp,
#'   roster_data = roster,
#'   season      = 2024L,
#'   systems     = list(
#'     Default  = list(),
#'     Sleeper  = list(first_down_points = 0.5, hundred_yard_bonus = 3.0,
#'                     long_td_bonus = 2.0, two_point_conversion = 2.0),
#'     Superflex = list(superflex_pass_td = 6.0, first_down_points = 0.5)
#'   )
#' )
#' }
#'
#' @seealso \code{\link{calculate_fantasy_points_ext}}
#'
#' @export
compare_scoring_systems <- function(
  pbp_data,
  roster_data = NULL,
  season      = NULL,
  week_min    = NULL,
  week_max    = NULL,
  systems     = list(Default = list()),
  min_games   = 4L
) {

  if (!is.list(systems) || length(systems) == 0) {
    stop("systems must be a non-empty named list of parameter override lists")
  }
  if (is.null(names(systems)) || any(names(systems) == "")) {
    stop("All elements of systems must have names")
  }

  results <- purrr::imap(systems, function(params, system_name) {
    message(glue("\nRunning system: {system_name}"))

    args <- c(
      list(
        pbp_data    = pbp_data,
        roster_data = roster_data,
        season      = season,
        week_min    = week_min,
        week_max    = week_max
      ),
      params
    )

    fp <- do.call(calculate_fantasy_points_ext, args)

    fp %>%
      dplyr::group_by(player_id, player_name, position, team) %>%
      dplyr::summarise(
        games_played = dplyr::n_distinct(game_id),
        total_pts    = sum(total_fantasy_points, na.rm = TRUE),
        .groups      = "drop"
      ) %>%
      dplyr::filter(games_played >= min_games) %>%
      dplyr::rename(
        !!glue("{system_name}_total") := total_pts,
        !!glue("{system_name}_games") := games_played
      )
  })

  # Join all systems on player identity
  combined <- purrr::reduce(results, function(x, y) {
    dplyr::full_join(x, y,
      by = c("player_id", "player_name", "position", "team")
    )
  })

  # Add ppg columns for each system
  system_names <- names(systems)
  for (sn in system_names) {
    total_col <- glue("{sn}_total")
    games_col <- glue("{sn}_games")
    ppg_col   <- glue("{sn}_ppg")
    if (total_col %in% names(combined) && games_col %in% names(combined)) {
      combined <- combined %>%
        dplyr::mutate(
          !!ppg_col := dplyr::if_else(
            dplyr::coalesce(.data[[games_col]], 0L) > 0,
            .data[[total_col]] / .data[[games_col]],
            NA_real_
          )
        )
    }
  }

  combined %>%
    dplyr::arrange(desc(.data[[glue("{system_names[1]}_total")]]))
}

# ==============================================================================
# SECTION 9: DEFAULTS INSPECTOR
# ==============================================================================

#' Get Extended Scoring Defaults
#'
#' @description
#' Returns a named list of all default parameter values for
#' calculate_fantasy_points_ext(). Useful for documentation, testing,
#' and verifying backward compatibility.
#'
#' @param show_new_only Logical. If TRUE, returns only Season 2 new parameters.
#'   Default FALSE (returns all parameters).
#'
#' @return Named list of all parameter defaults.
#'
#' @examples
#' \dontrun{
#' # View all defaults
#' get_ext_scoring_defaults()
#'
#' # View only new Season 2 parameters
#' get_ext_scoring_defaults(show_new_only = TRUE)
#' }
#'
#' @export
get_ext_scoring_defaults <- function(show_new_only = FALSE) {

  season1_defaults <- list(
    pass_yd        = 0.04,
    pass_td        = 6,
    pass_int       = -2,
    pick6_penalty  = -4,
    rush_yd        = 0.1,
    rush_td        = 6,
    rec_yd         = 0.1,
    rec_td         = 6,
    ppr            = 1,
    fumbles        = -2,
    use_tiered_ppr = TRUE,
    te_premium     = TRUE,
    rush_att_bonus = 0.25
  )

  season2_defaults <- list(
    first_down_points    = 0,
    long_td_bonus        = 0,
    long_td_threshold    = 40L,
    hundred_yard_bonus   = 0,
    superflex_pass_td    = 0,
    two_point_conversion = 0,
    sack_penalty         = 0,
    # Sleeper parity parameters
    tiered_rec_tiers  = NULL,
    bonus_pass_yd_300 = 0,
    bonus_pass_yd_400 = 0,
    bonus_rec_yd_200  = 0,
    bonus_rush_yd_200 = 0,
    pass_2pt          = 0
  )

  if (show_new_only) {
    return(season2_defaults)
  }

  c(season1_defaults, season2_defaults)
}
