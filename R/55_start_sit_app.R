# ==============================================================================
# NFL Analytics Toolkit -- Season 3
# R/55: Start/Sit App (local)
# File: R/55_start_sit_app.R
#
# PURPOSE
# -------
# A local Shiny app with a dropdown of every Sleeper league the user is in.
# Picking a league runs the live start/sit chain for that league:
#
#   build_league_source()  ->  optimize_lineup()  ->  analyze_start_sit()
#
# and renders the starters, the bench, the DEF streaming table, and a
# diagnostics strip. A save button writes the league- and week-keyed snapshot
# that the calibration backtest depends on.
#
# WHY THIS IS NOT A TAB IN shiny_app/
# -----------------------------------
# shiny_app/ is the shinyapps.io deploy bundle. Two reasons this app is
# separate:
#   1. `here` anchors to shiny_app/.here inside that bundle, while this chain
#      needs data/season2_cache/ and data/season3_cache/ under the PROJECT
#      root. Same here() call, different answer.
#   2. This app reads private league rosters over the Sleeper API. That does
#      not belong on a public host.
# This file is therefore run from the project root and is not deployable.
#
# RUN (fresh R session, working directory = project root)
# -------------------------------------------------------
#   source(here::here("R", "55_start_sit_app.R"))
#   run_start_sit_app()
#
# The first launch of a session is slow: it loads the reconciled projections,
# the Sleeper player database, and computes the DvP and DEF matchup factors
# from pbp. Those are computed ONCE at startup and passed into every league
# run, because compute_dvp_factors() and compute_def_matchup_factors() have no
# disk cache and would otherwise re-read pbp on every dropdown change.
#
# WHAT THIS APP DOES NOT DO
# -------------------------
# - It does not re-project. The projections come from R/32's reconciled board,
#   which is a preseason artifact. Weekly matchup adjustment is applied on top
#   of it by R/35. For offense that adjustment is currently inert: R/35's
#   DVP_CAP_OFFENSE is 0.00 on gate evidence, so every offensive dvp_factor is
#   exactly 1.0. DEF matchup adjustment is live at DVP_CAP_DEF 0.25 and is
#   documented in R/35 as untested.
# - It does not set lineups on Sleeper. Read-only.
# - It has no dashboard tabs yet. Those are a separate delivery.
#
# SIGMA CONTRACT (do not change without reading R/36)
# ---------------------------------------------------
# The uncertainty shown here is r32_sigma_week, the ONE-WEEK predictive SD.
# Never r32_posterior_sigma. R/36 owns that choice; this app only displays it.
# The diagnostics strip reports sigma_source counts so a silent fallback to a
# position median is visible rather than hidden inside an average.
#
# THRESHOLDS
# ----------
# STAKES_HIGH_MIN and CALL_START_MIN live in R/36 and were set before sigma was
# known to run near 10. They are deliberately NOT re-tuned here. Retune them in
# R/36 off real lineups, not off this app's display.
#
# SCHEMA TAG: s3_r55_app_v5
# ==============================================================================

library(shiny)
library(dplyr)
library(glue)

if (!exists("analyze_start_sit")) {
  source(here::here("R", "36_start_sit_uq.R"))
}
if (!exists("optimize_lineup")) {
  source(here::here("R", "35_lineup_optimizer.R"))
}
if (!exists("get_nfl_state")) {
  source(here::here("R", "19_sleeper_api.R"))
}

SCHEMA_TAG_APP <- "s3_r55_app_v5"

# The Sleeper account whose leagues populate the dropdown.
APP_SLEEPER_USERNAME <- Sys.getenv("SLEEPER_USERNAME")
if (!nzchar(APP_SLEEPER_USERNAME)) {
  stop("Set SLEEPER_USERNAME in the project .Renviron, then restart R.", call. = FALSE)
}

# Season the app operates on.
APP_SEASON <- 2026L

# History window for the DvP and DEF matchup factors. Prior completed season.
APP_MATCHUP_SEASONS <- APP_SEASON - 1L

# Canonical input artifacts.
APP_RECONCILED_PATH <- here::here("data", "season2_cache",
                                  "s2_week15_reconciled_projections.rds")
APP_DEF_PROJ_PATH   <- here::here("data", "season2_cache",
                                  "s2_week16_def_st_projections.rds")

# Columns shown in each table, in order. Any column missing from the upstream
# output is dropped at render time rather than erroring, so a schema change
# degrades the display instead of taking the app down.
#
# v1 guessed these off the R/35 and R/36 source and got three things wrong.
# Corrected against a real result object from the King of Kings week 1 run:
#   - the sigma column is `sigma_s`, not `r32_sigma_week`. R/36 renames it
#     when attaching, because the value can come from a source other than the
#     reconciled board (see sigma_source).
#   - the alternative columns are alt_player / alt_adj_proj / alt_source, not
#     best_alternative.
#   - p_vs_next and pick_clarity are not emitted by R/36 at all.
STARTER_COLS <- c("slot", "player_name", "position", "team", "opponent",
                  "on_bye", "base_proj", "matchup_factor", "adj_proj",
                  "sigma_s", "sigma_source", "p_start_correct", "call",
                  "stakes", "avg_miss", "expected_regret", "confidence_flag",
                  "alt_player", "alt_position", "alt_adj_proj", "alt_source",
                  "alt_player_uc", "alt_adj_proj_uc", "p_start_correct_uc",
                  "call_uc", "alt_player_re", "alt_adj_proj_re",
                  "p_start_correct_re", "call_re", "re_path")

# The bench frame comes from R/35 and carries the valuation columns rather than
# the UQ ones. is_available matters here: an unavailable bench player is not an
# eligible alternative, which is why a slot can score NA.
BENCH_COLS <- c("player_name", "position", "team", "opponent", "on_bye",
                "base_proj", "adj_proj", "lower_80", "upper_80",
                "boom_probability", "bust_probability", "adjusted_vorp",
                "is_available", "availability_status")

DEF_COLS <- c("player_name", "team", "opponent", "adj_proj", "sigma_s",
              "sigma_source", "is_available", "p_start_correct", "call",
              "stakes")


# ------------------------------------------------------------------------------
# STARTUP LOAD
# ------------------------------------------------------------------------------

#' Load everything that is shared across leagues
#'
#' Runs once per app launch. Everything here is league-independent. Anything
#' that depends on league scoring or roster settings (VORP, the source object,
#' the lineup) is computed per league instead.
#'
#' @return A list with reconciled, def_proj, sleeper_players, leagues, dvp,
#'   def_factors, and state.
app_startup <- function(username = APP_SLEEPER_USERNAME,
                        season = APP_SEASON,
                        matchup_seasons = APP_MATCHUP_SEASONS) {

  message("R/55 startup: loading shared inputs...")

  if (!file.exists(APP_RECONCILED_PATH)) {
    stop(glue("Reconciled projections not found:\n  {APP_RECONCILED_PATH}\n",
              "Regenerate R/32 before launching."), call. = FALSE)
  }
  if (!file.exists(APP_DEF_PROJ_PATH)) {
    stop(glue("DEF projections not found:\n  {APP_DEF_PROJ_PATH}\n",
              "Regenerate R/34 before launching."), call. = FALSE)
  }

  reconciled <- readRDS(APP_RECONCILED_PATH)
  def_proj   <- readRDS(APP_DEF_PROJ_PATH)
  message(glue("  reconciled: {nrow(reconciled)} rows"))
  message(glue("  def_proj:   {nrow(def_proj)} rows"))

  # The sigma column R/36 requires. Checked here so the failure surfaces at
  # startup rather than on the first league pick.
  if (!"r32_sigma_week" %in% names(reconciled)) {
    stop("reconciled has no r32_sigma_week column. R/36 cannot score ",
         "start/sit without it. Regenerate R/32.", call. = FALSE)
  }

  state <- get_nfl_state()

  leagues <- get_user_leagues(username, season = season)
  if (is.null(leagues) || nrow(leagues) == 0L) {
    stop(glue("No leagues returned for '{username}' in {season}."),
         call. = FALSE)
  }
  user_id <- attr(leagues, "sleeper_user_id")
  leagues <- attach_user_roster_ids(leagues, user_id = user_id)

  # One shared pull of the ~5MB Sleeper player database, reused by every
  # build_league_source() call instead of once per league.
  sleeper_players <- get_all_sleeper_players()

  # Computed once. These read pbp and have no disk cache.
  message(glue("  computing DvP factors ({matchup_seasons})..."))
  dvp <- compute_dvp_factors(seasons = matchup_seasons)
  message(glue("  computing DEF matchup factors ({matchup_seasons})..."))
  def_factors <- compute_def_matchup_factors(seasons = matchup_seasons)

  message("R/55 startup complete.")

  list(
    reconciled      = reconciled,
    def_proj        = def_proj,
    sleeper_players = sleeper_players,
    leagues         = leagues,
    user_id         = user_id,
    dvp             = dvp,
    def_factors     = def_factors,
    state           = state
  )
}


# ------------------------------------------------------------------------------
# PER-LEAGUE RUN
# ------------------------------------------------------------------------------

#' Run the full start/sit chain for one league
#'
#' @param shared The list returned by app_startup().
#' @param league_id Character Sleeper league id.
#' @param user_roster_id Integer roster id inside that league.
#' @param week Integer week to solve for.
#' @param save_output Logical. TRUE writes the league- and week-keyed snapshot.
#'
#' @return A list with source, vorp, lineup, uq, and timing.
run_league_start_sit <- function(shared, league_id, user_roster_id, week,
                                 save_output = FALSE) {

  t0 <- Sys.time()

  src <- build_league_source(
    league_id       = league_id,
    user_roster_id  = user_roster_id,
    season          = APP_SEASON,
    sleeper_players = shared$sleeper_players
  )

  # VORP is league-specific: replacement level depends on team count, starter
  # slots and flex count, all of which live in src$config.
  vorp <- compute_vorp_rankings(shared$reconciled, src$config)

  lineup <- optimize_lineup(
    src, shared$reconciled, vorp, shared$def_proj,
    week        = week,
    dvp         = shared$dvp,
    def_factors = shared$def_factors,
    season      = APP_SEASON,
    save_output = FALSE
  )

  uq <- analyze_start_sit(
    lineup, shared$reconciled,
    source      = src,
    vorp        = vorp,
    def_proj    = shared$def_proj,
    week        = week,
    league_id   = league_id,
    dvp         = shared$dvp,
    def_factors = shared$def_factors,
    season      = APP_SEASON,
    save_output = save_output
  )

  list(
    source   = src,
    vorp     = vorp,
    lineup   = lineup,
    uq       = uq,
    elapsed  = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}


# Keep only the columns that exist, in the requested order, and round the
# numeric ones for display. Display rounding only; nothing written to disk
# passes through here.
.display_table <- function(df, cols, digits = 2) {
  if (is.null(df) || nrow(df) == 0L) return(NULL)
  keep <- intersect(cols, names(df))
  out  <- df[, keep, drop = FALSE]
  num  <- vapply(out, is.numeric, logical(1))
  out[num] <- lapply(out[num], function(x) round(x, digits))
  out
}


# The dropdown label. Scoring, size and superflex are shown because they change
# what a correct lineup even is, and because two of these leagues are
# superflex while one of those is standard scoring.
.league_label <- function(leagues) {
  vapply(seq_len(nrow(leagues)), function(i) {
    r <- leagues[i, ]
    sf <- if (isTRUE(r$is_superflex)) " | SF" else ""
    trimws(glue("{r$name} ({r$total_rosters}-team {r$scoring_type}{sf})"))
  }, character(1))
}


# ------------------------------------------------------------------------------
# APP
# ------------------------------------------------------------------------------

#' Launch the start/sit app
#'
#' @param shared Optional. A prebuilt app_startup() list, so a relaunch inside
#'   the same session skips the slow startup.
#' @export
run_start_sit_app <- function(shared = NULL) {

  shared <- shared %||% app_startup()

  league_choices <- stats::setNames(shared$leagues$league_id,
                                    .league_label(shared$leagues))

  # display_week is what you set a lineup for: it advances past `week` once the
  # current week's games have finished. The override exists because that
  # rollover is Sleeper's, not yours.
  default_week <- shared$state$display_week %||% shared$state$week %||% 1L

  ui <- fluidPage(
    titlePanel("Start / Sit"),

    fluidRow(
      column(6, selectInput("league", "League", choices = league_choices,
                            width = "100%")),
      column(3, numericInput("week", "Week", value = default_week,
                             min = 1, max = 18, step = 1)),
      column(3,
             br(),
             actionButton("run", "Run", class = "btn-primary"),
             actionButton("save", "Save snapshot"))
    ),

    verbatimTextOutput("status"),
    div(style = "color:#a33; font-weight:bold;", textOutput("coverage")),

    tabsetPanel(
      tabPanel("Starters",    br(), tableOutput("starters")),
      tabPanel("Bench",       br(), tableOutput("bench")),
      tabPanel("DEF",         br(), br(), textOutput("def_note"),
                              br(), tableOutput("def")),
      tabPanel("Diagnostics", br(), verbatimTextOutput("diag"))
    )
  )

  server <- function(input, output, session) {

    result <- reactiveVal(NULL)
    status <- reactiveVal("Pick a league and press Run.")

    # Running is explicit rather than firing on dropdown change. Each run makes
    # several Sleeper API calls and solves an IP; a stray click should not
    # start one.
    do_run <- function(save_output) {
      lg <- shared$leagues[shared$leagues$league_id == input$league, ]
      if (nrow(lg) != 1L) {
        status("Could not resolve the selected league.")
        return(invisible(NULL))
      }
      if (is.na(lg$user_roster_id)) {
        status(glue("No user roster resolved for {lg$name}."))
        return(invisible(NULL))
      }

      status(glue("Running {lg$name}, week {input$week}..."))

      res <- tryCatch(
        run_league_start_sit(shared, lg$league_id, lg$user_roster_id,
                             week = as.integer(input$week),
                             save_output = save_output),
        error = function(e) e
      )

      if (inherits(res, "error")) {
        result(NULL)
        status(glue("FAILED: {conditionMessage(res)}"))
        return(invisible(NULL))
      }

      result(res)
      saved <- if (save_output) " | snapshot saved" else ""
      status(glue("{lg$name} | week {input$week} | ",
                  "{round(res$elapsed, 1)}s{saved}"))
    }

    observeEvent(input$run,  do_run(save_output = FALSE))
    observeEvent(input$save, do_run(save_output = TRUE))

    output$status <- renderText(status())

    # Scope banner. The toolkit projects offense and DEF only; it has no kicker
    # projections. A league with a K slot therefore starts more players than
    # this app can score, and both the projected total and the week risk score
    # cover the filled slots only. That limitation is stated on screen rather
    # than inferred from a short table, and it is repeated in the saved
    # snapshot's diagnostics so a future calibration run knows what it is
    # measuring.
    output$coverage <- renderText({
      res <- result(); req(res)
      st  <- res$uq$starters
      caps <- tryCatch(.build_slot_caps(res$source), error = function(e) NULL)
      if (is.null(caps)) return("")
      want  <- sum(unlist(caps), na.rm = TRUE)
      short <- want - nrow(st)
      if (short <= 0L) return("")
      glue("SCOPE: {nrow(st)} of {want} league slots projected. ",
           "{short} slot(s) have no projections in the toolkit. ",
           "Projected total and week risk score cover the projected slots only.")
    })

    output$starters <- renderTable({
      res <- result(); req(res)
      .display_table(res$uq$starters, STARTER_COLS)
    })

    output$bench <- renderTable({
      res <- result(); req(res)
      .display_table(res$lineup$bench, BENCH_COLS)
    })

    # R/36 only returns a def_streaming element when the DEF slot is OPEN. With
    # a DEF already rostered the element is absent entirely, which v1 rendered
    # as a silently empty tab. The note says which case you are looking at.
    output$def_note <- renderText({
      res <- result(); req(res)
      ds <- res$uq$def_streaming
      if (is.null(ds)) {
        "DEF slot is filled, so no streaming table was produced. The rostered DEF is scored on the Starters tab."
      } else if (nrow(ds) == 0L) {
        "DEF slot is open but no streaming candidates were returned."
      } else {
        glue("{nrow(ds)} streaming candidate(s).")
      }
    })

    output$def <- renderTable({
      res <- result(); req(res)
      .display_table(res$uq$def_streaming, DEF_COLS)
    })

    # The diagnostics strip exists because the failure modes of this chain are
    # silent ones: a sigma that fell back to a position median, a bye row that
    # got scored, a schema tag that does not match what the code expects. All
    # three are counts, not averages, so a single bad row is visible.
    output$diag <- renderPrint({
      res <- result(); req(res)
      st <- res$uq$starters

      # The schema tags are COLUMNS on the starters frame, not elements on the
      # result list. v1 read them off the list and printed "(absent)" for both.
      .tag <- function(df, col) {
        if (!col %in% names(df)) return("(absent)")
        v <- unique(df[[col]])
        paste(v[!is.na(v)], collapse = ", ")
      }

      cat("app schema tag   :", SCHEMA_TAG_APP, "\n")
      cat("uq schema tag    :", .tag(st, "uq_schema_tag"), "\n")
      cat("lineup schema tag:", .tag(st, "schema_tag"), "\n")
      cat("league_id        :", res$source$league_id %||% "(absent)", "\n")
      cat("format           :", res$source$format %||% "(absent)", "\n\n")

      cat("sigma_source counts\n")
      if ("sigma_source" %in% names(st)) {
        print(table(st$sigma_source, useNA = "ifany"))
      } else {
        cat("  (no sigma_source column)\n")
      }

      cat("\nsigma_s summary (starters)\n")
      if ("sigma_s" %in% names(st)) {
        print(summary(st$sigma_s))
      } else {
        cat("  (no sigma_s column)\n")
      }

      # A slot scores NA when it has no ELIGIBLE alternative: the pairwise
      # probability is start-vs-best-alternative, so with nothing to compare
      # against there is no number to report. Unavailable bench players do not
      # count as alternatives. This is expected behavior at thin positions, but
      # it silently removes a slot from the risk score, so it is counted here.
      cat("\nunscored slots\n")
      if ("p_start_correct" %in% names(st)) {
        uns <- st[is.na(st$p_start_correct), , drop = FALSE]
        cat("  scored  :", sum(!is.na(st$p_start_correct)), "of", nrow(st), "\n")
        if (nrow(uns) > 0L) {
          for (i in seq_len(nrow(uns))) {
            cat("  no alternative:", uns$slot[i], "-", uns$player_name[i], "\n")
          }
        }
      }

      # R/35 warns about an unfillable required slot and then returns a short
      # lineup. The projected total is then NOT the league's real projected
      # total. The toolkit has no kicker projections, so a league with a K slot
      # will always land here.
      cat("\nslot coverage\n")
      req_slots <- tryCatch(.build_slot_caps(res$source), error = function(e) NULL)
      if (!is.null(req_slots)) {
        want <- sum(unlist(req_slots), na.rm = TRUE)
        cat("  required slots:", want, "| filled:", nrow(st), "\n")
        if (nrow(st) < want) {
          cat("  SHORT by", want - nrow(st),
              "slot(s). Projected total is understated.\n")
        }
      } else {
        cat("  (could not read slot caps)\n")
      }

      cat("\nbye rows\n")
      cat("  starters on bye:",
          if ("on_bye" %in% names(st)) sum(st$on_bye, na.rm = TRUE) else NA,
          "\n")
      cat("  bench on bye   :",
          if ("on_bye" %in% names(res$lineup$bench))
            sum(res$lineup$bench$on_bye, na.rm = TRUE) else NA, "\n")

      cat("\nrow counts\n")
      cat("  starters:", nrow(st), "\n")
      cat("  bench   :", nrow(res$lineup$bench), "\n")
      cat("  DEF rows:",
          if (is.null(res$uq$def_streaming)) 0L else nrow(res$uq$def_streaming),
          "\n")

      cat("\nweek risk score:", res$uq$week_risk_score %||% NA, "\n")
      cat("  (summed from the EXCLUSIVE expected_regret column only;",
          "the _uc columns double count by construction and must not be",
          "summed)\n")
    })
  }

  shiny::runApp(shinyApp(ui, server), launch.browser = TRUE)
}
