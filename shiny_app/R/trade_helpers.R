# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Phase 5
# Trade Evaluation Helper
# File: shiny_app/R/trade_helpers.R
#
# PURPOSE
# -------
# Contains evaluate_trade(), extracted from R/37_trade_value_model.R for use
# in the Shiny app. Sourced by shiny_app/app.R at startup.
#
# CANONICAL SOURCE
# ----------------
# This function is identical to the version in R/37_trade_value_model.R.
# If R/37 is updated, mirror any changes here. The copy exists because R/37's
# top-level source() calls pull in R/19 and R/23, adding unnecessary weight
# to app startup. evaluate_trade() itself has no dependencies beyond dplyr
# and glue, which app.R already loads.
#
# DEPENDENCIES (loaded by app.R before this file is sourced)
# ----------------------------------------------------------
#   dplyr, glue
# ==============================================================================

# ------------------------------------------------------------------------------
# CONFIGURATION (mirrors R/37)
# ------------------------------------------------------------------------------

# Net trade-value delta inside which a trade is "ROUGHLY EVEN".
# Matches TRADE_VERDICT_BAND in R/37_trade_value_model.R.
TRADE_VERDICT_BAND <- 0.5

# ------------------------------------------------------------------------------
# evaluate_trade
# ------------------------------------------------------------------------------

#' Evaluate a proposed trade for one league
#'
#' Sums trade_value on each side and reports the net delta and a verdict. Pure:
#' no I/O, no side effects. Unknown player IDs are warned about and excluded
#' from the sum rather than erroring, so the Shiny caller never crashes on a
#' stale ID. Same-game correlation between players is not modeled (v2 item).
#'
#' @param give_ids Character vector. nfl_gsis_id of players traded away.
#' @param receive_ids Character vector. nfl_gsis_id of players received.
#' @param trade_values Tibble. Output of build_trade_value_table() from R/37,
#'   loaded at app startup from data/season2_cache/s2_week17_trade_values.rds.
#' @param league_name Character or NULL. Target league. If NULL, uses the first
#'   league present in trade_values.
#' @return Named list: give_value, receive_value, net_delta, give_detail,
#'   receive_detail, verdict ("WIN" / "LOSS" / "ROUGHLY EVEN").
#' @seealso R/37_trade_value_model.R (canonical source)
evaluate_trade <- function(give_ids, receive_ids, trade_values,
                           league_name = NULL) {

  target_league <- if (is.null(league_name)) {
    trade_values$league_name[1]
  } else {
    league_name
  }

  tv <- dplyr::filter(trade_values, .data$league_name == target_league)
  if (nrow(tv) == 0L) {
    stop(
      glue::glue("No trade values found for league '{target_league}'."),
      call. = FALSE
    )
  }

  side_detail <- function(ids) {
    found   <- tv %>% dplyr::filter(.data$nfl_gsis_id %in% ids)
    missing <- setdiff(ids, found$nfl_gsis_id)
    if (length(missing) > 0L) {
      warning(
        glue::glue(
          "Player IDs not found in '{target_league}' (excluded from sum): ",
          "{paste(missing, collapse = ', ')}"
        ),
        call. = FALSE
      )
    }
    found %>% dplyr::select(player_name, position, trade_value, tv_tier)
  }

  give_detail    <- side_detail(give_ids)
  receive_detail <- side_detail(receive_ids)

  give_value    <- sum(give_detail$trade_value,    na.rm = TRUE)
  receive_value <- sum(receive_detail$trade_value, na.rm = TRUE)
  net_delta     <- receive_value - give_value

  verdict <- if (net_delta > TRADE_VERDICT_BAND) {
    "WIN"
  } else if (net_delta < -TRADE_VERDICT_BAND) {
    "LOSS"
  } else {
    "ROUGHLY EVEN"
  }

  list(
    give_value     = give_value,
    receive_value  = receive_value,
    net_delta      = net_delta,
    give_detail    = give_detail,
    receive_detail = receive_detail,
    verdict        = verdict
  )
}
