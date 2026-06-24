# ==============================================================================
# NFL Analytics Toolkit | Season 2, Phase 5
# Shiny Tab 4: Rookie Tracker and Player League Lookup
# File: shiny_app/tabs/rookie.R
#
# PURPOSE
# -------
# Tab 4 of the Phase 5 Shiny app, two sections.
#
# Rookie Tracker. Lists the current incoming rookie class (R/28 prediction
# cohort) with a position filter and a sortable table. Selecting a rookie shows
# the historical players who scored most like him (nearest by score_final at the
# same position) with their real outcomes (hit or not, and production), plus a
# distribution plot placing his score in the historical spread. This turns the
# tracker into a "what did players who scored like this become" view, grounded
# in actual results rather than projections.
#
# Player League Lookup. Enter a player name and a Sleeper username to see which
# of that user's leagues the player is rostered in, and the slot.
#
# SOURCED BY
# ----------
# shiny_app/app.R. PURE DEFINITIONS ONLY. Sourcing defines rookieUI() and
# rookieServer(). Libraries (shiny, bslib, DT, dplyr, ggplot2) and the Sleeper
# API functions (get_user_leagues(), get_sleeper_rosters(),
# get_all_sleeper_players(), the `%||%` helper) are attached/sourced by app.R.
# Calls here are namespaced.
#
# DATA + LIVE-CALL CONTRACT
# -------------------------
# current_prospects() (app_data$prospects, R/28 s2_week14_final_prospect_scores
#   .csv) columns used: cfb_player_name, position, draft_year, draft_round,
#   draft_pick, draft_class_type ("prediction" = current class, "training" =
#   historical), score_final, is_hit, ppr_per_game_y13. The slot is NULL when
#   the cache is missing; the tab shows an informative note.
#
# The Rookie Tracker is fully cache-read. The Player League Lookup makes live
# Sleeper calls (one get_user_leagues, then get_sleeper_rosters and
# get_league_users per league), gated behind the Search button with a progress
# indicator. It lists every one of the user's leagues and shows the rostering
# team name, or FA when the player is not on any roster in that league.
# get_all_sleeper_players() maps the typed name to a sleeper_player_id via
# full_name.
#
# SCHEMA TAG: s2_w21_rookietab_v2
# ==============================================================================

# ------------------------------------------------------------------------------
# rookieUI
# ------------------------------------------------------------------------------

#' Tab 4 UI: rookie tracker and player league lookup
#'
#' @param id Character. Module namespace id (matched in rookieServer()).
#' @return A Shiny UI tag list for use inside a bslib::nav_panel().
rookieUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::h3("Rookie Tracker and Player League Lookup"),
    shiny::p(
      class = "lead text-muted",
      paste(
        "Track the current rookie class, see which past players scored like a",
        "given rookie and what they became, and look up which of your Sleeper",
        "leagues a player is rostered in. Powered by R/28 and R/19."
      )
    ),
    shiny::hr(),

    # Calibration note, consistent with the other tabs.
    shiny::div(
      class = "alert alert-warning",
      role  = "alert",
      shiny::tags$strong("Rookie scores are being calibrated. "),
      paste(
        "Scores come from the R/28 prospect model, which feeds the same",
        "preseason priors still being tuned. Use comparables and outcomes as",
        "context, not a guarantee. Known and tracked."
      )
    ),

    # ---- Section 1: Rookie Tracker -------------------------------------------
    shiny::h4("Rookie Tracker"),

    bslib::layout_columns(
      col_widths = c(12),
      shiny::selectInput(
        ns("pos_filter"),
        label    = "Filter by position",
        choices  = c("QB", "RB", "WR", "TE"),
        selected = NULL,
        multiple = TRUE
      )
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Current rookie class"),
      bslib::card_body(
        shiny::p(
          class = "text-muted",
          "Select a rookie to see comparable historical players and where his score falls."
        ),
        DT::DTOutput(ns("rookie_tbl"))
      )
    ),

    bslib::layout_columns(
      col_widths = c(6, 6),

      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Comparable historical players (nearest 10 by score)"),
        DT::DTOutput(ns("comps_tbl"))
      ),

      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Score in historical context"),
        shiny::plotOutput(ns("comps_plot"), height = "320px")
      )
    ),

    shiny::hr(),

    # ---- Section 2: Player League Lookup -------------------------------------
    shiny::h4("Player League Lookup"),

    bslib::card(
      bslib::card_header("Find a player across your leagues"),
      bslib::card_body(
        bslib::layout_columns(
          col_widths = c(5, 5, 2),
          shiny::textInput(
            ns("lookup_name"),
            label       = NULL,
            placeholder = "Player name"
          ),
          shiny::textInput(
            ns("lookup_user"),
            label       = NULL,
            placeholder = "Sleeper username"
          ),
          shiny::actionButton(
            ns("lookup_search"),
            label = "Search",
            class = "btn-secondary"
          )
        ),
        shiny::uiOutput(ns("lookup_status"))
      )
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header("This player across your leagues"),
      DT::DTOutput(ns("lookup_tbl"))
    )
  )
}

# ------------------------------------------------------------------------------
# rookieServer
# ------------------------------------------------------------------------------

#' Tab 4 server: rookie tracker, comparables, and player league lookup
#'
#' @param id Character. Module namespace id (matched in rookieUI()).
#' @param current_prospects Reactive returning the R/28 prospect tibble, or NULL
#'   if the cache is missing. Source for the rookie table, the comparables, and
#'   the distribution plot.
#' @param app_season Integer. The NFL season (app.R's SEASON constant), passed
#'   to get_user_leagues() so the lookup queries the right season.
#' @return Invisibly NULL. Called for its side effect of wiring the module.
rookieServer <- function(id, current_prospects, app_season) {
  shiny::moduleServer(id, function(input, output, session) {

    # ---- Cohorts -------------------------------------------------------------
    rookies_all <- shiny::reactive({
      p <- current_prospects()
      shiny::validate(
        shiny::need(
          !is.null(p),
          "Prospect scores are not loaded. Run R/28 to build the cache, then relaunch."
        )
      )
      dplyr::filter(p, .data$draft_class_type == "prediction")
    })

    training_all <- shiny::reactive({
      p <- current_prospects()
      shiny::req(p)
      dplyr::filter(p, .data$draft_class_type == "training")
    })

    # Position-filtered rookie table data, ranked within position. Guards the
    # optional draft_round / draft_pick columns so a missing one cannot error.
    rookie_table_data <- shiny::reactive({
      df <- rookies_all()
      for (cc in c("draft_round", "draft_pick")) {
        if (!cc %in% names(df)) df[[cc]] <- NA_integer_
      }
      pos <- input$pos_filter
      if (length(pos) > 0L) {
        df <- dplyr::filter(df, .data$position %in% pos)
      }
      df |>
        dplyr::group_by(.data$position) |>
        dplyr::mutate(pos_rank = dplyr::row_number(dplyr::desc(.data$score_final))) |>
        dplyr::ungroup() |>
        dplyr::arrange(dplyr::desc(.data$score_final))
    })

    # ---- Rookie table --------------------------------------------------------
    output$rookie_tbl <- DT::renderDT({
      df <- rookie_table_data()
      shiny::validate(
        shiny::need(nrow(df) > 0L, "No rookies match the selected position(s).")
      )
      display <- df |>
        dplyr::transmute(
          Player     = .data$cfb_player_name,
          Pos        = .data$position,
          Year       = .data$draft_year,
          Round      = .data$draft_round,
          Pick       = .data$draft_pick,
          Score      = .data$score_final,
          `Pos Rank` = .data$pos_rank
        )
      DT::datatable(
        display,
        rownames  = FALSE,
        selection = "single",
        class     = "stripe hover compact",
        options   = list(
          pageLength = 10,
          searching  = TRUE,
          info       = FALSE,
          scrollX    = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = c(1, 2, 3, 4, 5, 6))
          )
        )
      ) |>
        DT::formatRound("Score", digits = 2)
    })

    # The selected rookie row, mapped from the DT selection index back into the
    # exact data frame the table was built from.
    selected_rookie <- shiny::reactive({
      sel <- input$rookie_tbl_rows_selected
      df  <- rookie_table_data()
      if (is.null(sel) || length(sel) == 0L || nrow(df) == 0L) return(NULL)
      df[sel, , drop = FALSE]
    })

    # Nearest historical players at the same position, by absolute score gap.
    # Only players with a known outcome are used so every comp shows a result.
    comps <- shiny::reactive({
      r <- selected_rookie()
      if (is.null(r)) return(NULL)
      tr  <- training_all()
      pos <- r$position[1]
      sc  <- r$score_final[1]
      tr |>
        dplyr::filter(
          .data$position == pos,
          !is.na(.data$score_final),
          !is.na(.data$is_hit)
        ) |>
        dplyr::mutate(score_gap = abs(.data$score_final - sc)) |>
        dplyr::arrange(.data$score_gap) |>
        dplyr::slice_head(n = 10)
    })

    # ---- Comparables table ---------------------------------------------------
    output$comps_tbl <- DT::renderDT({
      r <- selected_rookie()
      shiny::validate(
        shiny::need(!is.null(r), "Select a rookie above to see comparable historical players.")
      )
      cp <- comps()
      shiny::validate(
        shiny::need(!is.null(cp) && nrow(cp) > 0L,
                    "No historical players with known outcomes at this position to compare.")
      )
      display <- cp |>
        dplyr::transmute(
          Player          = .data$cfb_player_name,
          Year            = .data$draft_year,
          Score           = .data$score_final,
          Hit             = dplyr::case_when(
            is.na(.data$is_hit) ~ "Unknown",
            .data$is_hit > 0    ~ "Yes",
            TRUE                ~ "No"
          ),
          `PPG (yrs 1-3)` = .data$ppr_per_game_y13
        )
      DT::datatable(
        display,
        rownames  = FALSE,
        selection = "none",
        class     = "stripe hover compact",
        options   = list(
          paging     = FALSE,
          searching  = FALSE,
          info       = FALSE,
          ordering   = FALSE,
          scrollX    = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = c(1, 2, 3, 4))
          )
        )
      ) |>
        DT::formatRound(c("Score", "PPG (yrs 1-3)"), digits = 2)
    })

    # ---- Distribution plot ---------------------------------------------------
    output$comps_plot <- shiny::renderPlot({
      r <- selected_rookie()
      shiny::validate(
        shiny::need(!is.null(r), "Select a rookie above to see his score in historical context.")
      )
      tr     <- training_all()
      pos    <- r$position[1]
      sc     <- r$score_final[1]
      tr_pos <- dplyr::filter(tr, .data$position == pos, !is.na(.data$score_final))
      shiny::validate(
        shiny::need(nrow(tr_pos) > 0L, "No historical players at this position.")
      )

      ggplot2::ggplot(tr_pos, ggplot2::aes(x = .data$score_final)) +
        ggplot2::geom_histogram(bins = 30, fill = "grey75", color = "white") +
        ggplot2::geom_vline(xintercept = sc, color = "#0B5394", linewidth = 1.2) +
        ggplot2::annotate(
          "text", x = sc, y = Inf, label = r$cfb_player_name[1],
          vjust = 1.5, hjust = -0.05, color = "#0B5394", fontface = "bold"
        ) +
        ggplot2::labs(
          x     = paste0(pos, " final score (historical class players)"),
          y     = "Number of players",
          title = paste0("Where ", r$cfb_player_name[1], " falls versus history")
        ) +
        ggplot2::theme_minimal(base_size = 13)
    })

    # ---- Player League Lookup ------------------------------------------------
    lk <- shiny::reactiveValues(result = NULL, searched = FALSE,
                                msg = NULL, msg_class = NULL)

    shiny::observeEvent(input$lookup_search, {
      name <- trimws(input$lookup_name %||% "")
      user <- trimws(input$lookup_user %||% "")
      lk$searched <- TRUE

      if (nchar(name) == 0L || nchar(user) == 0L) {
        lk$result    <- NULL
        lk$msg       <- "Enter both a player name and your Sleeper username."
        lk$msg_class <- "alert alert-warning"
        return(invisible(NULL))
      }

      res <- shiny::withProgress(
        message = "Searching your leagues...",
        value   = 0.1,
        {
          players <- get_all_sleeper_players()
          norm    <- function(x) tolower(trimws(x))
          hits <- players[
            !is.na(players$full_name) &
              grepl(norm(name), norm(players$full_name), fixed = TRUE), ,
            drop = FALSE
          ]
          if (nrow(hits) == 0L) {
            list(error = "no_player")
          } else {
            target_ids   <- hits$sleeper_player_id
            display_name <- hits$full_name[1]
            leagues <- get_user_leagues(user, season = app_season)
            if (is.null(leagues) || nrow(leagues) == 0L) {
              list(error = "no_user")
            } else {
              rows <- list()
              n <- nrow(leagues)
              for (i in seq_len(n)) {
                shiny::incProgress(0.9 / n, detail = leagues$name[i])
                lid  <- leagues$league_id[i]
                rost <- tryCatch(get_sleeper_rosters(lid),
                                 error = function(e) NULL)
                found <- if (!is.null(rost) && nrow(rost) > 0L) {
                  rost[rost$player_id %in% target_ids, , drop = FALSE]
                } else {
                  NULL
                }

                if (is.null(found) || nrow(found) == 0L) {
                  # Player is a free agent in this league.
                  rows[[length(rows) + 1L]] <- data.frame(
                    League = leagues$name[i],
                    Player = display_name,
                    Team   = "FA",
                    stringsAsFactors = FALSE
                  )
                } else {
                  # Rostered: resolve owner_id to a team name.
                  users <- tryCatch(get_league_users(lid),
                                    error = function(e) NULL)
                  for (j in seq_len(nrow(found))) {
                    pid  <- found$player_id[j]
                    oid  <- found$owner_id[j]
                    team <- if (!is.null(users) && nrow(users) > 0L) {
                      tm <- users$team_name[match(oid, users$owner_id)]
                      if (length(tm) == 0L || is.na(tm)) "Unknown team" else tm
                    } else {
                      "Unknown team"
                    }
                    rows[[length(rows) + 1L]] <- data.frame(
                      League = leagues$name[i],
                      Player = hits$full_name[match(pid, hits$sleeper_player_id)],
                      Team   = team,
                      stringsAsFactors = FALSE
                    )
                  }
                }
              }
              list(table = do.call(rbind, rows), n_leagues = n)
            }
          }
        }
      )

      if (!is.null(res$error)) {
        lk$result    <- NULL
        lk$msg_class <- "alert alert-warning"
        lk$msg <- switch(
          res$error,
          no_player = "No Sleeper player matched that name. Try a fuller spelling.",
          no_user   = paste(
            "Could not find that user, or no leagues for the season.",
            "Use your Sleeper username, not your display name."
          ),
          "Search failed."
        )
      } else {
        lk$result    <- res$table
        lk$msg_class <- "alert alert-success"
        rostered <- unique(res$table$League[res$table$Team != "FA"])
        n_rost   <- length(rostered)
        lk$msg <- sprintf(
          "Searched %d league(s). Rostered in %d, free agent in %d.",
          res$n_leagues, n_rost, res$n_leagues - n_rost
        )
      }
    }, ignoreInit = TRUE)

    output$lookup_status <- shiny::renderUI({
      if (!lk$searched || is.null(lk$msg)) return(NULL)
      shiny::div(class = lk$msg_class, lk$msg)
    })

    output$lookup_tbl <- DT::renderDT({
      shiny::validate(
        shiny::need(lk$searched, "Enter a player and your username, then Search.")
      )
      df <- lk$result
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0L, "No results to show.")
      )
      DT::datatable(
        df,
        rownames  = FALSE,
        selection = "none",
        class     = "stripe hover compact",
        options   = list(
          paging     = FALSE,
          searching  = FALSE,
          info       = FALSE,
          ordering   = FALSE,
          scrollX    = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = c(2))
          )
        )
      )
    })

    invisible(NULL)
  })
}
