# ==============================================================================
# shiny_dfs/app.R
#
# Daily (DFS) lineup app, v3: FanDuel classic.  [2026-11-05]
# (v1 2026-10-09, v2 2026-10-22; v3 shows the R/65 v3 FanDuel blend: proj is
#  the projection the solver uses, ours = our projection, fd = FanDuel's.)
#
# A local Shiny surface over R/65. It recomputes nothing upstream: it reads the
# salary CSV and the chain output that run_dfs_chain() already wrote, and calls
# R/65's own parser, matcher, eligibility rule, lock check, rules and solver.
# The console optimizer (run_dfs_optimizer) and this app always agree because
# they run the same functions.
#
# DESIGN (forks locked 2026-09-30)
#   5 C  Locks and excludes are clicked in the app (select rows, press Lock or
#        Exclude). The lock set is checked live for legality before Solve.
#   9 B  Questionable players are excluded by default; one checkbox includes
#        them. A locked questionable player is always allowed.
#  12 B  Standalone folder. On start it loads the newest CSV in
#        data/dfs_salaries/ (name_overrides.csv excluded); an upload button
#        uses a different file for this session only. Nothing is written to
#        your folders by the app; lineups download through the browser.
#
# V2 (forks locked 2026-10-22; all model-side logic lives in R/65 v2)
#   Rules panel: max players per team, max share of projected points from one
#     team, QB stack (0/1/2 WR/TE), bring-back, no players facing your DEF.
#     Every control maps to one dfs_rules() field.
#   Lineup tab: swap cost and replacement per player; team share line.
#   Lineups tab: top N lineups with a minimum player difference; download.
#   Pool and Excluded tabs: prior_mu, ytd_mean, ytd_n, ytd_gap from the site's
#     R/29 file (display only; a stale prior shows as a large negative gap).
#   Simulation tab: R/69 simulate-and-rank over the Lineups tab's candidate
#     set (same-team correlation from R/50, common random numbers). Needs
#     build_sim_cache() run once; the tab says so if it has not been.
#
# THE WEEK
#   Taken from the salary file: the app finds the one 2026 regular-season week
#   that contains every game on the slate and stops if there is not exactly
#   one. The chain output for that week must already exist
#   (run_dfs_chain(week = N)); if it does not, the app says so.
#
# NEEDS
#   shiny, DT (plus everything R/65 needs: dplyr, tidyr, purrr, tibble, readr,
#   glue, here, nflreadr, Rglpk).
#
# RUN
#   rm(list = ls())
#   shiny::runApp(here::here("shiny_dfs"))
# ==============================================================================

library(shiny)
library(DT)
library(dplyr)
library(glue)
library(here)
library(readr)

source(here::here("R", "65_daily_optimizer.R"))
if (!exists("dfs_rules") || !exists("solve_dfs_lineups") || !exists("swap_costs") ||
    !exists("optimality_problems") || !exists(".r65_apply_blend")) {
  stop("shiny_dfs v3 needs R/65 v3 (dfs_rules, solve_dfs_lineups, swap_costs, .r65_apply_blend). ",
       "Save the 2026-11-05 R/65 replacement first.", call. = FALSE)
}

source(here::here("R", "69_dfs_simulation.R"))
APP_SIM <- load_sim_inputs()          # NULL until build_sim_cache() has been run

APP_SITE   <- "fd"
APP_SEASON <- 2026L
APP_CFG    <- .r65_site(APP_SITE)
APP_POSITIONS <- c("QB", "RB", "WR", "TE", "DEF")

# ------------------------------------------------------------------------------
# Helpers (app-only; everything model-side comes from R/65)
# ------------------------------------------------------------------------------

# The one REG week containing every slate game; stops otherwise.
.app_infer_week <- function(sal, season) {
  sch <- .r65_load_schedules(season) %>%
    dplyr::filter(.data$game_type == "REG") %>%
    dplyr::mutate(game = paste(.r65_clean_abbr(away_team), "@", .r65_clean_abbr(home_team)))
  games <- unique(sal$game)
  wk <- sch %>%
    dplyr::filter(game %in% games) %>%
    dplyr::count(week) %>%
    dplyr::filter(n == length(games)) %>%
    dplyr::pull(week)
  if (length(wk) != 1L) {
    stop(glue("The salary file's {length(games)} games do not all fall in one ",
              "{season} regular-season week. Check that this is a classic main-slate file."),
         call. = FALSE)
  }
  as.integer(wk)
}

.app_age <- function(t) {
  h <- as.numeric(difftime(Sys.time(), t, units = "hours"))
  if (!is.finite(h)) return("unknown")
  if (h < 1) return(glue("{round(h * 60)} min ago"))
  glue("{round(h, 1)} h ago")
}

.app_share_text <- function(team_share) {
  paste0(names(team_share), " ", round(100 * team_share), "%", collapse = ", ")
}

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel(glue("Daily optimizer: {APP_CFG$name} classic")),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Slate"),
      uiOutput("slate_info"),
      fileInput("upload", "Use a different salary CSV (this session only)",
                accept = ".csv"),
      hr(),
      h4("Pool"),
      checkboxInput("include_q", "Include questionable players", value = FALSE),
      checkboxGroupInput("pos", "Positions", choices = APP_POSITIONS,
                         selected = APP_POSITIONS, inline = TRUE),
      selectInput("team", "Team", choices = "All", selected = "All"),
      radioButtons("show", "Show",
                   choices = c("Eligible" = "eligible",
                               "All matched" = "matched",
                               "Everything" = "all"),
                   selected = "eligible", inline = TRUE),
      hr(),
      h4("Rules"),
      numericInput("max_per_team", glue("Max players per team (site limit {APP_CFG$max_per_team})"),
                   value = APP_CFG$max_per_team, min = 1, max = APP_CFG$max_per_team, step = 1),
      sliderInput("max_share", "Max share of projected points from one team (100 = off)",
                  min = 20, max = 100, value = 100, step = 5, post = "%"),
      selectInput("qb_stack", "QB stack (WR/TE from his team)",
                  choices = c("Off" = "0", "1" = "1", "2" = "2"), selected = "0"),
      helpText("QB stack has evidence in our data (R/50: QB and his pass catchers move together).",
               "The two rules below are contest choices, not model findings."),
      checkboxInput("bring_back", "Bring-back: 1+ opponent player with the QB", value = FALSE),
      checkboxInput("no_def_conflict", "No players facing your DEF", value = FALSE),
      uiOutput("blocked_qbs"),
      hr(),
      h4("Locks and excludes"),
      helpText("Select rows in the Pool tab, then:"),
      actionButton("lock", "Lock"),
      actionButton("exclude", "Exclude"),
      actionButton("clear_sel", "Clear selected"),
      br(), br(),
      actionButton("clear_all", "Clear all locks and excludes"),
      br(), br(),
      uiOutput("lock_summary"),
      uiOutput("feasibility"),
      hr(),
      actionButton("solve", "Solve lineup", class = "btn-primary", width = "100%")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        tabPanel("Pool", br(),
                 helpText("ytd_gap = 2026 per-game mean minus the prior (display only). ",
                          "A large negative gap with several games played is a stale prior."),
                 DTOutput("pool_tbl")),
        tabPanel("Lineup", br(), uiOutput("lineup_msg"), DTOutput("lineup_tbl"),
                 br(), uiOutput("lineup_totals"),
                 helpText("swap_cost = projected points lost by the best lineup without that player; ",
                          "replacement = who comes in (a swap can reshuffle more than one slot)."),
                 downloadButton("dl_lineup", "Download lineup CSV")),
        tabPanel("Lineups", br(),
                 fluidRow(
                   column(3, numericInput("n_lineups", "Number of lineups", value = 10,
                                          min = 2, max = 50, step = 1)),
                   column(3, numericInput("min_diff", "Min players different", value = 2,
                                          min = 1, max = sum(APP_CFG$slots), step = 1)),
                   column(3, br(), actionButton("build_lineups", "Build lineups",
                                                class = "btn-primary"))
                 ),
                 helpText("Uses the same rules, locks and excludes as Solve. ",
                          "Select a row to see that lineup."),
                 uiOutput("lineups_msg"),
                 DTOutput("lineups_tbl"), br(),
                 DTOutput("lineups_detail"), br(),
                 downloadButton("dl_lineups", "Download all lineups CSV")),
        tabPanel("Simulation", br(),
                 fluidRow(
                   column(3, selectInput("sim_obj", "Rank by",
                                         choices = c("Mean" = "mean", "Median (p50)" = "p50",
                                                     "p80" = "p80", "p90 (tournament)" = "p90",
                                                     "p95" = "p95", "P(total >= threshold)" = "p_ge"),
                                         selected = "p90")),
                   column(3, numericInput("sim_threshold", "Threshold (points)", value = 150,
                                          min = 50, max = 300, step = 5)),
                   column(3, numericInput("sim_draws", "Simulated weeks", value = 10000,
                                          min = 1000, max = 50000, step = 1000)),
                   column(3, br(), actionButton("simulate", "Simulate lineups", class = "btn-primary"))
                 ),
                 helpText("Simulates the lineups from the Lineups tab. Build them there first ",
                          "(20 lineups, min difference 1 is a good candidate set)."),
                 helpText(R69_LABEL),
                 uiOutput("sim_msg"),
                 DTOutput("sim_tbl")),
        tabPanel("Unmatched", br(),
                 helpText(glue("Priced players with no projection. Add any that ",
                               "should be in play to data/dfs_salaries/{basename(R65_OVERRIDE_PATH)} ",
                               "(columns site, site_name, team, nfl_gsis_id) and restart the app.")),
                 DTOutput("unmatched_tbl")),
        tabPanel("Excluded", br(),
                 helpText("Matched players the solver will not use, and why."),
                 DTOutput("excluded_tbl"))
      )
    )
  )
)

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(locks = character(0), excl = character(0),
                       result = NULL, swap = NULL, problems = character(0), stale = TRUE,
                       lineups = NULL, lineups_msg = NULL, lineups_stale = TRUE,
                       sim = NULL, sim_msg = NULL, sim_threshold = NA_real_)

  salary_path <- reactive({
    if (!is.null(input$upload)) input$upload$datapath else newest_salary_file()
  })
  salary_label <- reactive({
    if (!is.null(input$upload)) paste0(input$upload$name, " (uploaded)")
    else basename(salary_path())
  })

  # Salary file + chain output + matching. Independent of the Q toggle.
  base <- reactive({
    path <- salary_path()
    sal  <- read_fd_salaries(path)
    wk   <- .app_infer_week(sal, APP_SEASON)
    bands_rds <- file.path(R65_OUTPUT_DIR,
                           sprintf("%sr59_weekly_bands_w%02d.rds", APP_CFG$prefix, wk))
    def_rds   <- .r65_path(APP_CFG, "def_st", wk, "rds")
    for (f in c(bands_rds, def_rds)) {
      if (!file.exists(f)) {
        stop(glue("Chain output for week {wk} not found ({basename(f)}). ",
                  "Run run_dfs_chain(week = {wk}) first."), call. = FALSE)
      }
    }
    bands <- readRDS(bands_rds)
    def   <- readRDS(def_rds)
    ov    <- .r65_read_overrides(APP_SITE)
    ctx   <- .r65_read_r29_context(APP_CFG)
    list(week = wk,
         pool = .r65_match(sal, bands, def, ov, ctx),
         n_games = dplyr::n_distinct(sal$game),
         bands_built = max(bands$built_at),
         lines_pulled = max(def$lines_pulled_at, na.rm = TRUE),
         n_overrides = nrow(ov))
  })

  # Eligibility depends on the Q toggle.
  pool <- reactive({
    .r65_eligibility(base()$pool, include_q = isTRUE(input$include_q))
  })

  # Rules from the sidebar. Out-of-range numeric input falls back to the site limit.
  rules <- reactive({
    mpt <- suppressWarnings(as.integer(input$max_per_team))
    if (length(mpt) != 1L || is.na(mpt) || mpt < 1L || mpt > APP_CFG$max_per_team) {
      mpt <- APP_CFG$max_per_team
    }
    share <- if (is.null(input$max_share) || input$max_share >= 100) NULL else input$max_share / 100
    # A QB stack of k needs k + 1 players from one team; clamp rather than error.
    qs <- as.integer(input$qb_stack)
    if (qs > 0L && qs + 1L > mpt) {
      qs <- max(0L, mpt - 1L)
      showNotification(glue("QB stack lowered to {qs}: max players per team is {mpt}."), type = "warning")
    }
    dfs_rules(APP_CFG, max_per_team = mpt, max_team_share = share,
              qb_stack = qs,
              bring_back = isTRUE(input$bring_back),
              no_def_conflict = isTRUE(input$no_def_conflict))
  })

  # New slate: reset everything.
  observeEvent(base(), {
    rv$locks <- character(0); rv$excl <- character(0)
    rv$result <- NULL; rv$swap <- NULL; rv$problems <- character(0); rv$stale <- TRUE
    rv$lineups <- NULL; rv$lineups_msg <- NULL; rv$lineups_stale <- TRUE
    teams <- sort(unique(base()$pool$team))
    updateSelectInput(session, "team", choices = c("All", teams), selected = "All")
  })

  # Anything that changes the problem makes solved lineups stale.
  observeEvent(list(pool(), rv$locks, rv$excl, rules()), {
    rv$stale <- TRUE; rv$lineups_stale <- TRUE
  }, ignoreInit = TRUE)

  # Rows shown in the Pool tab (selection indices refer to these rows).
  shown <- reactive({
    P <- pool() %>%
      dplyr::mutate(status = dplyr::case_when(
        !is.na(player_uid) & player_uid %in% rv$locks ~ "LOCK",
        !is.na(player_uid) & player_uid %in% rv$excl  ~ "EXCLUDE",
        TRUE ~ ""))
    P <- switch(input$show,
                eligible = P %>% dplyr::filter(eligible | status != ""),
                matched  = P %>% dplyr::filter(match_status %in% c("matched", "override", "dst")),
                all      = P)
    if (length(input$pos) > 0) {
      P <- P %>% dplyr::filter(is.na(position) | position %in% input$pos)
    }
    if (!is.null(input$team) && input$team != "All") {
      P <- P %>% dplyr::filter(team == input$team)
    }
    P %>% dplyr::arrange(dplyr::desc(status != ""), dplyr::desc(proj))
  })

  # ---- sidebar text ----
  output$slate_info <- renderUI({
    b <- base()
    tagList(
      tags$div(tags$b("File: "), salary_label()),
      tags$div(tags$b("Week: "), b$week, glue(" ({b$n_games} games)")),
      tags$div(tags$b("Projections built: "), format(b$bands_built, "%a %m-%d %H:%M"),
               glue(" ({.app_age(b$bands_built)})")),
      tags$div(tags$b("Vegas lines pulled: "), format(b$lines_pulled, "%a %m-%d %H:%M")),
      tags$div(tags$b("Projection: "), {
        bw <- unique(stats::na.omit(b$pool$blend_w))
        if (length(bw) == 1L) glue("blend, {bw} ours + {1 - bw} FanDuel") else "ours only (blend off; see console)"
      }),
      tags$div(tags$b("Name overrides: "), b$n_overrides),
      tags$div(tags$b("Cap: "), paste0("$", format(APP_CFG$cap, big.mark = ",")))
    )
  })

  output$lock_summary <- renderUI({
    P <- pool()
    nm <- function(ids) {
      if (length(ids) == 0) return("none")
      paste(P$site_name[match(ids, P$player_uid)], collapse = ", ")
    }
    tagList(tags$div(tags$b("Locked: "), nm(rv$locks)),
            tags$div(tags$b("Excluded: "), nm(rv$excl)))
  })

  lock_idx <- reactive({ i <- match(rv$locks, pool()$player_uid); i[!is.na(i)] })
  excl_idx <- reactive({ i <- match(rv$excl,  pool()$player_uid); i[!is.na(i)] })

  output$feasibility <- renderUI({
    probs <- .r65_lock_problems(pool(), lock_idx(), APP_CFG, rules())
    if (length(probs) == 0) {
      if (length(rv$locks) == 0) return(NULL)
      return(tags$div(style = "color:#2e7d32;", "Locks are legal."))
    }
    tags$div(style = "color:#c62828;",
             tags$b("Locks cannot form a legal lineup:"),
             tags$ul(lapply(probs, tags$li)))
  })

  output$blocked_qbs <- renderUI({
    blk <- rule_blocked_qbs(pool(), rules(), lock_idx(), excl_idx())
    if (nrow(blk) == 0) return(NULL)
    blk <- blk %>% dplyr::arrange(dplyr::desc(proj))
    tags$div(style = "color:#ef6c00;",
             tags$b(glue("{nrow(blk)} QB(s) unusable under these rules:")),
             tags$ul(lapply(utils::head(seq_len(nrow(blk)), 8), function(i)
               tags$li(glue("{blk$site_name[i]} ({blk$team[i]}): {blk$reason[i]}")))))
  })

  # ---- lock / exclude buttons ----
  selected_ids <- reactive({
    s <- input$pool_tbl_rows_selected
    if (length(s) == 0) return(character(0))
    ids <- shown()$player_uid[s]
    if (any(is.na(ids))) {
      showNotification("Rows with no projection cannot be locked or excluded; they were skipped.",
                       type = "warning")
    }
    ids[!is.na(ids)]
  })
  observeEvent(input$lock, {
    ids <- selected_ids()
    rv$locks <- union(rv$locks, ids); rv$excl <- setdiff(rv$excl, ids)
  })
  observeEvent(input$exclude, {
    ids <- selected_ids()
    rv$excl <- union(rv$excl, ids); rv$locks <- setdiff(rv$locks, ids)
  })
  observeEvent(input$clear_sel, {
    ids <- selected_ids()
    rv$locks <- setdiff(rv$locks, ids); rv$excl <- setdiff(rv$excl, ids)
  })
  observeEvent(input$clear_all, { rv$locks <- character(0); rv$excl <- character(0) })

  infeasible_text <- function() {
    paste0("No legal lineup exists. Active rules: ", .r65_rules_text(rules(), APP_CFG),
           ". Loosen a rule or a lock, remove an exclude, or include questionable players.")
  }

  # ---- solve ----
  observeEvent(input$solve, {
    P <- pool()
    li <- lock_idx(); ei <- excl_idx(); R <- rules()
    probs <- .r65_lock_problems(P, li, APP_CFG, R)
    if (length(probs) > 0) {
      rv$problems <- probs; rv$result <- NULL; rv$swap <- NULL; rv$stale <- FALSE
    } else {
      res <- withProgress(message = "Solving", value = 0.3, {
        solve_dfs_lineup(P, APP_CFG, li, ei, R)
      })
      if (res$status != 0) {
        rv$problems <- infeasible_text()
        rv$result <- NULL; rv$swap <- NULL
      } else {
        sw <- withProgress(message = "Swap costs (one re-solve per player)", value = 0.6, {
          swap_costs(P, APP_CFG, li, ei, R, res)
        })
        opt <- optimality_problems(sw, res)
        if (length(opt) > 0) {
          rv$problems <- c(opt, "Do not use this lineup.")
          rv$result <- NULL; rv$swap <- NULL
        } else {
          rv$problems <- character(0); rv$result <- res; rv$swap <- sw
        }
      }
      rv$stale <- FALSE
    }
    updateTabsetPanel(session, "tabs", selected = "Lineup")
  })

  # ---- multiple lineups ----
  observeEvent(input$build_lineups, {
    P <- pool()
    li <- lock_idx(); ei <- excl_idx(); R <- rules()
    n  <- suppressWarnings(as.integer(input$n_lineups))
    md <- suppressWarnings(as.integer(input$min_diff))
    if (is.na(n) || n < 2L || n > 50L || is.na(md) || md < 1L || md > sum(APP_CFG$slots)) {
      rv$lineups <- NULL
      rv$lineups_msg <- glue("Number of lineups must be 2 to 50 and min difference 1 to {sum(APP_CFG$slots)}.")
      return()
    }
    probs <- .r65_lock_problems(P, li, APP_CFG, R)
    if (length(probs) > 0) {
      rv$lineups <- NULL
      rv$lineups_msg <- paste("Locks cannot form a legal lineup:", paste(probs, collapse = "; "))
      return()
    }
    lus <- withProgress(message = glue("Solving up to {n} lineups"), value = 0.3, {
      solve_dfs_lineups(P, APP_CFG, li, ei, R, n = n, min_diff = md)
    })
    rv$lineups <- lus
    rv$lineups_stale <- FALSE
    if (nrow(lus) == 0) {
      rv$lineups_msg <- infeasible_text()
    } else if (nrow(lus) < n) {
      rv$lineups_msg <- glue("Only {nrow(lus)} of {n} lineups exist with these rules and min difference {md}.")
    } else {
      rv$lineups_msg <- NULL
    }
  })

  # ---- simulation (R/69) ----
  observeEvent(rv$lineups, { rv$sim <- NULL; rv$sim_msg <- NULL }, ignoreNULL = FALSE)
  observeEvent(input$simulate, {
    if (is.null(APP_SIM)) {
      rv$sim <- NULL
      rv$sim_msg <- "No simulation cache. In the console run: source(here::here('R', '69_dfs_simulation.R')); build_sim_cache()  then restart the app."
      return()
    }
    if (is.null(rv$lineups) || nrow(rv$lineups) == 0) {
      rv$sim <- NULL; rv$sim_msg <- "Build lineups on the Lineups tab first."
      return()
    }
    nd <- suppressWarnings(as.integer(input$sim_draws))
    if (is.na(nd) || nd < 1000L || nd > 50000L) nd <- 10000L
    thr <- suppressWarnings(as.numeric(input$sim_threshold))
    sim <- tryCatch(withProgress(message = glue("Simulating {nrow(rv$lineups)} lineups x {nd} weeks"),
                                 value = 0.4, {
      simulate_lineups(rv$lineups, APP_SIM, n_draws = nd,
                       thresholds = if (is.finite(thr)) thr else numeric(0))
    }), error = function(e) { rv$sim_msg <- conditionMessage(e); NULL })
    rv$sim <- sim
    rv$sim_threshold <- if (is.finite(thr)) thr else NA_real_
    if (!is.null(sim)) rv$sim_msg <- NULL
  })

  output$sim_msg <- renderUI({
    msgs <- list()
    if (!is.null(rv$sim_msg)) msgs <- c(msgs, list(tags$div(style = "color:#c62828;", rv$sim_msg)))
    if (!is.null(rv$sim) && isTRUE(rv$lineups_stale)) {
      msgs <- c(msgs, list(tags$div(style = "color:#ef6c00;",
        "Rules, locks or the pool changed since these lineups were built. Rebuild lineups, then simulate again.")))
    }
    if (!is.null(rv$sim) && identical(input$sim_obj, "p_ge") &&
        !isTRUE(all.equal(rv$sim_threshold, suppressWarnings(as.numeric(input$sim_threshold))))) {
      msgs <- c(msgs, list(tags$div(style = "color:#ef6c00;",
        glue("The simulation was run at threshold {rv$sim_threshold}. Press Simulate lineups again for the new threshold."))))
    }
    if (length(msgs) == 0) return(NULL)
    tagList(msgs)
  })

  output$sim_tbl <- renderDT({
    req(rv$sim)
    obj <- input$sim_obj
    thr <- rv$sim_threshold          # the threshold the sim was actually run at
    ranked <- tryCatch(rank_lineups(rv$sim, obj, threshold = thr),
                       error = function(e) rank_lineups(rv$sim, "p90"))
    lu_meta <- rv$lineups %>% dplyr::select(lineup_id, top_team, n_teams)
    d <- ranked %>% dplyr::left_join(lu_meta, by = "lineup_id") %>%
      dplyr::select(-objective) %>%
      dplyr::rename(rank = sim_rank, lineup = lineup_id)
    datatable(d, rownames = FALSE, selection = "none",
              caption = glue("Ranked by {ranked$objective[1]}. To see a lineup's players, select the same lineup id on the Lineups tab."),
              options = list(pageLength = 25, ordering = FALSE, scrollX = TRUE))
  })

  # ---- tables ----
  output$pool_tbl <- renderDT({
    d <- shown() %>%
      dplyr::transmute(status, name = site_name, pos = position, team, opp = opponent,
                       salary, proj = round(proj, 2), ours = round(proj_ours, 2),
                       value = round(value, 2),
                       ytd_n, ytd_gap,
                       availability, excluded = excluded_reason, match = match_status,
                       site_proj = file_proj)
    datatable(d, rownames = FALSE, selection = "multiple",
              options = list(pageLength = 25, scrollX = TRUE, order = list())) %>%
      formatCurrency("salary", digits = 0) %>%
      formatStyle("status", target = "row",
                  backgroundColor = styleEqual(c("LOCK", "EXCLUDE"), c("#e3f2fd", "#fdecea")))
  })

  output$lineup_msg <- renderUI({
    if (length(rv$problems) > 0) {
      return(tags$div(style = "color:#c62828;", tags$b("Not solved: "),
                      tags$ul(lapply(rv$problems, tags$li))))
    }
    if (is.null(rv$result)) return(helpText("Press Solve lineup."))
    if (rv$stale) {
      return(tags$div(style = "color:#ef6c00;",
                      "Rules, locks, excludes or the pool changed since this lineup was solved. Press Solve lineup again."))
    }
    NULL
  })

  lineup_df <- reactive({
    req(rv$result)
    lu <- rv$result$lineup
    if (!is.null(rv$swap)) lu <- lu %>% dplyr::left_join(rv$swap, by = "player_uid")
    else lu <- lu %>% dplyr::mutate(swap_cost = NA_real_, replacement = NA_character_)
    lu %>%
      dplyr::transmute(slot, name = site_name, pos = position, team, opp = opponent,
                       salary, proj = round(proj, 2), ours = round(proj_ours, 2),
                       fd = file_proj, value = round(value, 2),
                       swap_cost, replacement, locked, availability)
  })

  output$lineup_tbl <- renderDT({
    datatable(lineup_df(), rownames = FALSE, selection = "none",
              options = list(dom = "t", pageLength = 9, ordering = FALSE)) %>%
      formatCurrency("salary", digits = 0)
  })

  output$lineup_totals <- renderUI({
    req(rv$result)
    r <- rv$result
    tagList(
      tags$p(tags$b(glue("Projected {round(r$total_proj, 2)}")),
             glue(" | salary ${format(r$total_salary, big.mark = ',')} of ",
                  "${format(APP_CFG$cap, big.mark = ',')} ",
                  "(${format(APP_CFG$cap - r$total_salary, big.mark = ',')} left) | ",
                  "teams {dplyr::n_distinct(r$lineup$team)}")),
      tags$p(tags$b("Share of projected points by team: "), .app_share_text(r$team_share)),
      tags$p(tags$b("Rules: "), .r65_rules_text(r$rules, APP_CFG))
    )
  })

  output$dl_lineup <- downloadHandler(
    filename = function() sprintf("%s_lineup_w%02d.csv", APP_SITE, base()$week),
    content  = function(file) readr::write_csv(lineup_df(), file)
  )

  output$lineups_msg <- renderUI({
    msgs <- list()
    if (!is.null(rv$lineups_msg)) msgs <- c(msgs, list(tags$div(style = "color:#c62828;", rv$lineups_msg)))
    if (!is.null(rv$lineups) && rv$lineups_stale) {
      msgs <- c(msgs, list(tags$div(style = "color:#ef6c00;",
                                    "Rules, locks, excludes or the pool changed. Press Build lineups again.")))
    }
    if (length(msgs) == 0) return(NULL)
    tagList(msgs)
  })

  output$lineups_tbl <- renderDT({
    req(rv$lineups, nrow(rv$lineups) > 0)
    d <- rv$lineups %>%
      dplyr::transmute(lineup = lineup_id, total_proj, delta_vs_best, total_salary,
                       top_team, top_team_share = paste0(round(100 * top_team_share), "%"), n_teams)
    datatable(d, rownames = FALSE, selection = "single",
              options = list(pageLength = 25, ordering = FALSE)) %>%
      formatCurrency("total_salary", digits = 0)
  })

  output$lineups_detail <- renderDT({
    req(rv$lineups, nrow(rv$lineups) > 0)
    s <- input$lineups_tbl_rows_selected
    i <- if (length(s) == 0 || s[1] > nrow(rv$lineups)) 1L else s[1]
    d <- rv$lineups$lineup[[i]] %>%
      dplyr::transmute(slot, name = site_name, pos = position, team, opp = opponent,
                       salary, proj = round(proj, 2), value = round(value, 2))
    datatable(d, rownames = FALSE, selection = "none",
              caption = glue("Lineup {rv$lineups$lineup_id[i]}"),
              options = list(dom = "t", pageLength = 9, ordering = FALSE)) %>%
      formatCurrency("salary", digits = 0)
  })

  output$dl_lineups <- downloadHandler(
    filename = function() sprintf("%s_lineups_w%02d.csv", APP_SITE, base()$week),
    content  = function(file) {
      if (is.null(rv$lineups) || nrow(rv$lineups) == 0) {
        readr::write_csv(tibble::tibble(note = "No lineups built yet. Press Build lineups first."), file)
        return(invisible())
      }
      long <- purrr::map2_dfr(rv$lineups$lineup_id, rv$lineups$lineup, function(id, lu) {
        lu %>% dplyr::transmute(lineup = id, slot, name = site_name, pos = position, team,
                                opp = opponent, salary, proj = round(proj, 2))
      })
      readr::write_csv(long, file)
    }
  )

  output$unmatched_tbl <- renderDT({
    d <- pool() %>%
      dplyr::filter(match_status %in% c("no_projection", "ambiguous",
                                        "override_not_found", "dst_no_projection")) %>%
      dplyr::arrange(dplyr::desc(salary)) %>%
      dplyr::transmute(name = site_name, team, opp = opponent, salary,
                       match = match_status, site_proj = file_proj)
    datatable(d, rownames = FALSE, selection = "none",
              options = list(pageLength = 25)) %>%
      formatCurrency("salary", digits = 0)
  })

  output$excluded_tbl <- renderDT({
    d <- pool() %>%
      dplyr::filter(match_status %in% c("matched", "override", "dst"), !eligible) %>%
      dplyr::arrange(excluded_reason, dplyr::desc(proj)) %>%
      dplyr::transmute(name = site_name, pos = position, team, salary,
                       proj = round(proj, 2), reason = excluded_reason, availability,
                       prior_mu = round(prior_mu, 2), ytd_mean = round(ytd_mean, 2),
                       ytd_n, ytd_gap)
    datatable(d, rownames = FALSE, selection = "none",
              options = list(pageLength = 25, scrollX = TRUE)) %>%
      formatCurrency("salary", digits = 0)
  })
}

shinyApp(ui, server)
