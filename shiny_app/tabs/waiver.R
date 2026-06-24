# ==============================================================================
# NFL Analytics Toolkit | Season 2, Phase 5
# Shiny Tab 2: Waiver / Start-Sit
# File: shiny_app/tabs/waiver.R
#
# PURPOSE
# -------
# Tab 2 of the Phase 5 Shiny app. Renders the matchup-adjusted optimal lineup
# from R/35 enriched with the R/36 start/sit uncertainty metrics (probability a
# start is correct, expected regret if it is wrong, and a stakes label), plus a
# DEF streaming recommendation when the optimal lineup has an open defense slot.
# Read-only from cache. No live Sleeper API call and no projection
# re-derivation; the table is the single source of truth produced by R/36.
#
# SOURCED BY
# ----------
# shiny_app/app.R. This file contains PURE DEFINITIONS ONLY. Sourcing it just
# defines waiverUI() and waiverServer(). The libraries (shiny, bslib, DT, dplyr)
# are attached by app.R; calls here are namespaced so the file is also
# sourceable in isolation for inspection.
#
# REACTIVE ACCESSOR DISCIPLINE (non-negotiable)
# ---------------------------------------------
# waiverServer() reads data ONLY through the reactive accessors passed in
# (current_start_sit, current_def_streaming), never from app_data directly. For
# the current single-user build these accessors return the cached objects. The
# post-launch multi-user upgrade changes only the accessor definitions in
# app.R's server(); nothing in this file changes. current_start_sit is the same
# accessor wired into Tab 1's seam; this tab is its home.
#
# DATA CONTRACT
# -------------
# current_start_sit() (app_data$start_sit_uq, R/36 schema s2_w16_uq_v1):
#   slot, nfl_gsis_id, player_name, team, position, opponent, base_proj,
#   matchup_factor, adj_proj, adjusted_vorp, confidence_flag, schema_tag,
#   alt_player, alt_position, alt_source, alt_adj_proj, p_start_correct,
#   avg_miss, expected_regret, stakes, uq_schema_tag
# current_def_streaming() (app_data$def_streaming, R/36 schema s2_w16_uq_v1):
#   rank, def_team, opponent, adj_proj, p_vs_next, pick_clarity, uq_schema_tag
#   This slot is legitimately NULL when the cached optimal lineup already starts
#   a defense (no open slot to stream into); R/36 only writes the file when the
#   DEF slot is open. The tab shows an informative note in that case.
#
# SCHEMA TAG: s2_w20_startsittab_v1
# ==============================================================================

# ------------------------------------------------------------------------------
# waiverUI
# ------------------------------------------------------------------------------

#' Tab 2 UI: waiver / start-sit dashboard
#'
#' Returns the tab body: a short intro, a calibration note, a week-level risk
#' summary, the optimal lineup table enriched with start/sit confidence, and a
#' DEF streaming card. All inputs and outputs are namespaced with the module id
#' so multiple instances never collide.
#'
#' @param id Character. Module namespace id (matched in waiverServer()).
#' @return A Shiny UI tag list for use inside a bslib::nav_panel().
waiverUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::h3("Start/Sit Confidence and DEF Streaming"),
    shiny::p(
      class = "lead text-muted",
      paste(
        "Your matchup-adjusted optimal lineup with the probability each start",
        "is correct, the expected points given up if it is wrong, and DEF",
        "streaming options when the defense slot is open.",
        "Powered by R/35 and R/36."
      )
    ),
    shiny::hr(),

    # Calibration note. Lighter than Tab 1's banner, but the same caveat
    # applies: the start/sit confidence numbers inherit the preseason
    # projection priors that are still being tuned.
    shiny::div(
      class = "alert alert-warning",
      role  = "alert",
      shiny::tags$strong("Confidence values are being calibrated. "),
      paste(
        "Start/sit probabilities derive from the same preseason projection",
        "priors as the Projections tab, which are still being tuned.",
        "Known and tracked."
      )
    ),

    # Week-level risk summary. Rendered server-side from the cached lineup so
    # it always matches the table below. Display-only: a sum of a column that
    # is already in the cache, not a re-derivation of any ranking.
    shiny::uiOutput(ns("risk_summary")),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Optimal Lineup with Start/Sit Confidence"),
      DT::DTOutput(ns("lineup_table"))
    ),

    bslib::card(
      bslib::card_header("DEF Streaming"),
      DT::DTOutput(ns("def_table"))
    )
  )
}

# ------------------------------------------------------------------------------
# waiverServer
# ------------------------------------------------------------------------------

#' Tab 2 server: render the start/sit lineup and DEF streaming tables
#'
#' Reads the enriched lineup and the DEF streaming recommendation through the
#' reactive accessors passed in and renders them as DT tables, with an
#' informative message in place of each table when its cache slot is missing.
#' There are no filters: the cached lineup is a single optimized lineup, so the
#' rows are shown in their natural slot order.
#'
#' @param id Character. Module namespace id (matched in waiverUI()).
#' @param current_start_sit Reactive returning the start/sit UQ tibble (R/36
#'   output), or NULL if the cache is missing. The single source for the lineup
#'   and start/sit columns in this tab.
#' @param current_def_streaming Reactive returning the DEF streaming tibble
#'   (R/36 output), or NULL when the cached lineup has no open DEF slot.
#' @return Invisibly NULL. Called for its side effect of wiring the module.
waiverServer <- function(id, current_start_sit, current_def_streaming) {
  shiny::moduleServer(id, function(input, output, session) {

    # ---- Week-level risk summary ---------------------------------------------
    output$risk_summary <- shiny::renderUI({
      df <- current_start_sit()
      if (is.null(df) || nrow(df) == 0L) {
        return(NULL)
      }
      risk <- sum(df$expected_regret, na.rm = TRUE)
      shiny::div(
        class = "alert alert-secondary",
        shiny::tags$strong("Week risk score: "),
        sprintf("%.1f expected points at risk across the lineup. ", risk),
        shiny::tags$span(
          class = "text-muted",
          "Higher means more of the start/sit decisions are close calls."
        )
      )
    })

    # ---- Optimal lineup with start/sit confidence ----------------------------
    output$lineup_table <- DT::renderDT({

      df <- current_start_sit()
      shiny::validate(
        shiny::need(
          !is.null(df),
          paste(
            "Start/sit data is not loaded. Run R/36 with save_output = TRUE",
            "to build the cache, then relaunch."
          )
        )
      )
      shiny::validate(
        shiny::need(nrow(df) > 0L, "The cached lineup is empty.")
      )

      # No arrange(): preserve the slot order R/35 wrote into the cache.
      display <- df |>
        dplyr::transmute(
          Slot             = .data$slot,
          Player           = .data$player_name,
          Team             = .data$team,
          Pos              = .data$position,
          Opp              = .data$opponent,
          Proj             = .data$adj_proj,
          Confidence       = .data$confidence_flag,
          `P(Start Right)` = .data$p_start_correct,
          `Next Best`      = .data$alt_player,
          From             = .data$alt_source,
          `Exp. Regret`    = .data$expected_regret,
          Stakes           = .data$stakes
        )

      # Center every column except Player (1) and Next Best (8), 0-indexed.
      center_targets <- c(0, 2, 3, 4, 5, 6, 7, 9, 10, 11)

      DT::datatable(
        display,
        rownames = FALSE,
        class    = "stripe hover compact",
        options  = list(
          paging     = FALSE,
          searching  = FALSE,
          info       = FALSE,
          ordering   = FALSE,        # keep the cache's natural slot order
          scrollX    = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = center_targets)
          )
        )
      ) |>
        DT::formatRound(c("Proj", "Exp. Regret"), digits = 1) |>
        DT::formatPercentage("P(Start Right)", digits = 0)
    })

    # ---- DEF streaming recommendation ----------------------------------------
    output$def_table <- DT::renderDT({

      df <- current_def_streaming()
      shiny::validate(
        shiny::need(
          !is.null(df) && nrow(df) > 0L,
          paste(
            "No DEF streaming recommendation is cached. R/36 produces one only",
            "when the optimal lineup has no started defense (an open DEF slot)."
          )
        )
      )

      display <- df |>
        dplyr::arrange(.data$rank) |>
        dplyr::transmute(
          Rank         = .data$rank,
          Defense      = .data$def_team,
          Opp          = .data$opponent,
          Proj         = .data$adj_proj,
          `Beats Next` = .data$p_vs_next,
          Clarity      = .data$pick_clarity
        )

      # Center every column except Defense (1), 0-indexed.
      center_targets <- c(0, 2, 3, 4, 5)

      DT::datatable(
        display,
        rownames = FALSE,
        class    = "stripe hover compact",
        options  = list(
          paging     = FALSE,
          searching  = FALSE,
          info       = FALSE,
          ordering   = FALSE,
          scrollX    = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = center_targets)
          )
        )
      ) |>
        DT::formatRound("Proj", digits = 1) |>
        DT::formatPercentage("Beats Next", digits = 0)
    })

    invisible(NULL)
  })
}
