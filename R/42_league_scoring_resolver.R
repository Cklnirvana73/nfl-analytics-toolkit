# ==============================================================================
# 42_league_scoring_resolver.R
# ==============================================================================
#
# PURPOSE
# -------
# One clean entry point from a Sleeper league to its FULL, VERIFIED scoring:
# offense params for calculate_fantasy_points_ext() (R/17, mapped by R/19) plus
# D/ST tiers and event values (R/34), read from the league's real settings, not
# Sleeper's coarse "ppr"/"standard" label. Bundles both halves into a single
# object the projection/VORP board consumes directly.
#
# Any live (non-zero) scoring field the pipeline does not actually consume --
# kicker fields, IDP (idp_*), return yardage, and anything else unmapped -- is
# surfaced in `out_of_scope` (with a warning) rather than dropped silently.
# The mapped-field list is built explicitly from what R/19's scoring parser
# and R/34's DEF parser read.
#
# DEPENDENCIES (guarded-sourced below)
#   R/19_sleeper_api.R  : get_user_leagues(), map_sleeper_scoring(),
#                         connect_sleeper_league()
#   R/34_def_st_projection.R : .load_league_def_scoring()
#
# VERSION
#   1.0  Initial build. Resolver only: reads and bundles verified scoring.
#        No projection or VORP here.
# ==============================================================================

library(here)
library(glue)

source(here::here("R", "19_sleeper_api.R"))
source(here::here("R", "34_def_st_projection.R"))

for (fn in c("get_user_leagues", "map_sleeper_scoring", "connect_sleeper_league",
             ".load_league_def_scoring")) {
  if (!exists(fn)) {
    stop(glue(
      "42_league_scoring_resolver.R requires {fn}(), not found after sourcing ",
      "R/19 and R/34. Check source paths."
    ))
  }
}


# ------------------------------------------------------------------------------
# resolve_league_scoring
# ------------------------------------------------------------------------------

#' Resolve one Sleeper league to its full verified scoring.
#'
#' @param league_id Character. Sleeper league id.
#' @param verbose Logical. Print a scoring summary. Default TRUE.
#'
#' @return A list:
#'   league_id, league_name, season, n_teams, is_superflex
#'   offense       named list, ready for do.call(calculate_fantasy_points_ext).
#'   def_st        list(pts_allow, yds_allow, events), ready for R/34.
#'   def_st_source character, provenance tag from R/34.
#'   out_of_scope  named numeric of ALL live (non-zero) Sleeper scoring fields
#'                 the pipeline does not consume -- kicker, IDP (idp_*), and
#'                 anything else unmapped (empty if none). A warning is raised
#'                 when non-empty.
resolve_league_scoring <- function(league_id, verbose = TRUE) {

  if (length(league_id) != 1L || is.na(league_id) || !nzchar(league_id)) {
    stop("league_id must be a single non-empty string.")
  }

  # --- league metadata + raw settings (one connect) ---
  lg <- connect_sleeper_league(league_id)
  raw <- lg$scoring_settings %||% list()

  # --- offense: R/19 maps raw settings to R/17 ext params ---
  off <- map_sleeper_scoring(league_id)
  offense <- off$params
  if (is.null(offense) || length(offense) == 0L) {
    stop(glue("map_sleeper_scoring() returned no offense params for {league_id}."))
  }

  # --- D/ST: R/34 reads the league's real tiers + event values ---
  def <- .load_league_def_scoring(league_id = league_id)

  # --- out_of_scope: every live (non-zero) field the pipeline does not consume
  # Inverted from the old kicker-only grep, which silently dropped IDP
  # (idp_*), yds_allow_*, and any other unmapped non-zero field. The
  # mapped-field set is built explicitly from what R/19's
  # .parse_scoring_settings() and R/34's .parse_def_scoring() actually read,
  # so any new field Sleeper sends surfaces here instead of vanishing.

  # Offense fields consumed by R/19: the direct map plus the derived-handler
  # fields (STEPs 2-8b of .parse_scoring_settings).
  offense_mapped <- c(
    names(SLEEPER_DIRECT_MAP),
    "pass_sack", "bonus_rec_te",
    "bonus_rec_yd_100", "bonus_rush_yd_100",
    "bonus_fd_rb", "bonus_fd_wr", "bonus_fd_te", "bonus_fd_qb",
    "pass_td_40p", "pass_td_50p", "rush_td_40p", "rush_td_50p",
    "rec_td_40p", "rec_td_50p",
    "rec_2pt", "rush_2pt", "pass_int_td",
    "rec_0_4", "rec_5_9", "rec_10_19", "rec_20_29", "rec_30_39", "rec_40p"
  )

  # DEF fields consumed by R/34: points-allowed tiers, yards-allowed tiers,
  # and every accepted event-key spelling.
  def_mapped <- c(
    names(DEF_SCORING_DEFAULT$pts_allow),
    names(DEF_SCORING_DEFAULT$yds_allow),
    unname(unlist(SLEEPER_DEF_EVENT_KEYS))
  )

  mapped_fields <- unique(c(offense_mapped, def_mapped))

  raw_vals <- vapply(raw, function(v) suppressWarnings(as.numeric(v)[1]),
                     numeric(1))
  live_fields  <- names(raw_vals)[!is.na(raw_vals) & raw_vals != 0]
  out_of_scope <- raw_vals[setdiff(live_fields, mapped_fields)]

  if (length(out_of_scope) > 0L) {
    warning(glue(
      "League {league_id}: {length(out_of_scope)} live scoring field(s) have ",
      "no mapping in the pipeline and are NOT applied: ",
      "{paste(names(out_of_scope), collapse = ', ')}."
    ), call. = FALSE)
  }

  result <- list(
    league_id     = league_id,
    league_name   = lg$name %||% NA_character_,
    season        = lg$season %||% NA_integer_,
    n_teams       = lg$total_rosters %||% NA_integer_,
    is_superflex  = lg$is_superflex %||% NA,
    offense       = offense,
    def_st        = def$scoring,
    def_st_source = def$source %||% NA_character_,
    out_of_scope  = out_of_scope
  )

  if (verbose) {
    message(glue("\n=== {result$league_name} ({league_id}) ==="))
    message(glue("  teams {result$n_teams} | superflex {result$is_superflex}"))
    message("  OFFENSE (calculate_fantasy_points_ext params):")
    for (nm in names(offense)) message(glue("    {nm}: {offense[[nm]]}"))
    message("  D/ST pts_allow tiers:")
    message(glue("    {paste(names(def$scoring$pts_allow), def$scoring$pts_allow, sep='=', collapse=' | ')}"))
    message("  D/ST events:")
    message(glue("    {paste(names(def$scoring$events), def$scoring$events, sep='=', collapse=' | ')}"))
    if (length(out_of_scope) > 0L) {
      message("  OUT OF SCOPE (live fields with no scoring home):")
      message(glue("    {paste(names(out_of_scope), out_of_scope, sep='=', collapse=' | ')}"))
    } else {
      message("  OUT OF SCOPE: none")
    }
  }

  result
}
