# ==============================================================================
# R/47 -- BATCH VALUE BOARDS
# ==============================================================================
#
# Non-interactive all-leagues board runner. Christian's spec, verbatim:
#   "i want it to do all the leagues and the dk automaticly not me putting in
#    my stuff each time"
#
# That is the whole point of this file. run_value_boards() in R/46 prompts four
# times -- username, a select.list of leagues, DK yes/no, and proceed yes/no.
# batch_value_boards() takes all four as arguments and asks nothing.
#
# IT DOES NOT DUPLICATE THE BUILD LOOP. The loop lives once, in R/46, as
# .build_boards(). run_value_boards() gathers its league list by prompting;
# this file gathers the same list from arguments; both hand it to the same
# builder. Duplicated definitions are how DK_BEST_BALL_SCORING and
# DEFAULT_SCORING_SETTINGS each ended up with two sources of truth and a
# source-order coin flip deciding which one won.
#
# COST MODEL, read before running:
#   One R/29 + R/32 chain per DISTINCT resolved scoring key, not per league.
#   Leagues sharing a scoring signature share a run. Aging curves are
#   scoring-specific, so a scoring key with no cached curve pays an R/23 fit,
#   and a new panel key pays a 16-season game panel build on top of that.
#   Use dry_run = TRUE first. It prints the league list, the resolved scoring
#   per league, and the engine-run count, then exits without building.
#
# REQUIRES (source first): R/17, R/19, R/29, R/32, R/33, R/42, R/45, R/46.
# ==============================================================================


# ------------------------------------------------------------------------------
# .verify_team_volume_cache
# ------------------------------------------------------------------------------

#' Hard-stop if the R/30 team volume cache is not for the target season.
#'
#' VERIFIES ONLY. It does not regenerate. Regenerating means calling R/30 from
#' inside a board runner, which is how a prior season's volumes got clobbered
#' in the first place. If the cache is wrong, the correct response is to stop
#' and run R/30 deliberately.
#'
#' @param season Integer season the boards are being built for.
#' @param path Path to the R/30 team volumes RDS.
#' @return Invisible TRUE, or stops.
#' @keywords internal
.verify_team_volume_cache <- function(season, path) {

  if (!file.exists(path)) {
    stop(glue("batch_value_boards(): R/30 team volume cache not found at ",
              "{path}. Run R/30 before building boards."), call. = FALSE)
  }

  tv <- readRDS(path)

  if (!"season" %in% names(tv)) {
    stop(glue("batch_value_boards(): R/30 team volume cache at {path} has no ",
              "`season` column, so the season it holds cannot be verified. ",
              "Regenerate R/30 rather than building boards on an unverifiable ",
              "cache."), call. = FALSE)
  }

  found <- sort(unique(tv$season))
  if (!season %in% found) {
    stop(glue("batch_value_boards(): R/30 team volume cache holds season(s) ",
              "{paste(found, collapse = ', ')}, not {season}. This cache is ",
              "stale. Run R/30 for {season}, then re-run. Not regenerating it ",
              "from inside a board runner."), call. = FALSE)
  }

  message(glue("  STEP 0 ok: R/30 team volumes hold season {season} ",
               "({nrow(tv)} rows)."))
  invisible(TRUE)
}


# ------------------------------------------------------------------------------
# batch_value_boards
# ------------------------------------------------------------------------------

#' Build value boards for every Sleeper league plus DK best ball, no prompts.
#'
#' @param username Sleeper username. Not the display name.
#' @param include_dk Logical. Add a DK best-ball board.
#' @param leagues NULL for every league the username returns. Otherwise a
#'   character vector of league names or league_ids to restrict to.
#' @param season Integer season.
#' @param dk_csv_path DK best-ball ADP export. Defaults to R/46's
#'   .vb_latest_dk_adp(), which picks the newest parseable export in
#'   season3_cache and prints its age. [2026-08-13] This used to be a
#'   hardcoded DkPreDraftRankings20260713.csv, which silently reverted R/46's
#'   stale-ADP resolver in the one runner actually used on draft day. Do NOT
#'   "fix" a stale board by bumping a date string here; that recreates the
#'   identical bug with a later expiry. Pass an explicit path only to pin a
#'   specific export on purpose.
#' @param out_dir Directory for the per-league CSVs and the combined file.
#' @param verify_cache Logical. Run the STEP 0 team-volume season check.
#' @param r30_path Path to the R/30 team volumes RDS, for STEP 0.
#' @param config_override NULL, or a single league_config. When supplied, EVERY
#'   league's own roster config is replaced by this one, so scoring is the only
#'   thing that varies across the resulting boards. This is what makes rank
#'   travel attributable to scoring rather than to team count and lineup slots.
#'   It deliberately overrides superflex and best-ball shapes too: a held-config
#'   board does NOT describe how that league actually drafts, and must not be
#'   presented as if it does.
#' @param out_prefix Filename stem. MUST be changed from the default whenever
#'   config_override is used, otherwise the held-config boards overwrite the
#'   real ones. The function enforces this.
#' @param write_combined Logical. Also write one long file across all leagues
#'   with a league_name column.
#' @param dry_run Logical. Print the plan and the engine-run count, build
#'   nothing, return the plan invisibly.
#' @return Invisible named list of board tibbles, or the plan when dry_run.
#' @export
batch_value_boards <- function(
    username       = Sys.getenv("SLEEPER_USERNAME"),
    include_dk     = TRUE,
    leagues        = NULL,
    season         = 2026L,
    dk_csv_path    = .vb_latest_dk_adp(),
    out_dir        = here::here("data", "season3_cache"),
    verify_cache   = TRUE,
    r30_path       = here::here("data", "season2_cache",
                                "s2_week15_team_volumes.rds"),
    config_override = NULL,
    out_prefix      = "value_board",
    write_combined  = TRUE,
    dry_run         = FALSE) {

  need <- c("resolve_league_scoring", "get_user_leagues",
            "build_config_from_sleeper", "compute_vorp_rankings",
            "run_projection_engine", "reconcile_projections",
            "build_value_board", ".build_boards", ".complete_scoring",
            ".scoring_key", "dk_best_ball_config", "DK_BEST_BALL_SCORING",
            ".vb_latest_dk_adp")
  miss <- need[!vapply(need, exists, logical(1))]
  if (length(miss) > 0L) {
    stop(glue("batch_value_boards(): missing {paste(miss, collapse = ', ')}. ",
              "Source R/17, R/19, R/29, R/32, R/33, R/42, R/45, R/46 first."),
         call. = FALSE)
  }

  season <- as.integer(season)

  # dk_csv_path defaults to a FUNCTION CALL, so R's lazy evaluation would not
  # force it until .build_boards() at the very end -- after every R/29 + R/32
  # engine run. Force it here so a missing or unparseable ADP export fails
  # before any compute, and so dry_run reports the file it would have used.
  # [2026-08-13]
  if (isTRUE(include_dk)) {
    force(dk_csv_path)
    if (!file.exists(dk_csv_path)) {
      stop(glue("batch_value_boards(): dk_csv_path does not exist: ",
                "{dk_csv_path}"), call. = FALSE)
    }
    message(glue("  DK ADP export: {basename(dk_csv_path)}"))
  }

  if (!is.null(config_override)) {
    if (!inherits(config_override, "league_config")) {
      stop("batch_value_boards(): config_override must be a league_config ",
           "object from build_league_config().", call. = FALSE)
    }
    if (identical(out_prefix, "value_board")) {
      stop("batch_value_boards(): config_override is set but out_prefix is ",
           "still the default. A held-config run would overwrite the ",
           "production boards. Pass out_prefix (e.g. 'refboard').",
           call. = FALSE)
    }
  }

  # ---- STEP 0: verify, do not regenerate ----
  if (isTRUE(verify_cache)) {
    .verify_team_volume_cache(season = season, path = r30_path)
  }

  # ---- gather leagues from arguments, no prompts ----
  gathered <- list()

  username <- trimws(as.character(username))
  if (nzchar(username)) {
    lg <- tryCatch(get_user_leagues(username, season = season),
                   error = function(e) {
                     message(glue("  Sleeper error: {e$message}")); NULL
                   })

    if (is.null(lg) || nrow(lg) == 0L) {
      message(glue("  No Sleeper leagues returned for '{username}' in {season}."))
    } else {
      if (!is.null(leagues)) {
        keep <- lg$name %in% leagues | lg$league_id %in% leagues
        unmatched <- setdiff(as.character(leagues),
                             c(lg$name[keep], lg$league_id[keep]))
        if (length(unmatched) > 0L) {
          stop(glue("batch_value_boards(): requested league(s) not found for ",
                    "this user: {paste(unmatched, collapse = ', ')}. ",
                    "Available: {paste(lg$name, collapse = ', ')}."),
               call. = FALSE)
        }
        lg <- lg[keep, , drop = FALSE]
      }

      message(glue("  {nrow(lg)} league(s) to resolve."))

      for (i in seq_len(nrow(lg))) {
        nm  <- lg$name[i]
        lid <- lg$league_id[i]
        cfg <- tryCatch(build_config_from_sleeper(lid),
                        error = function(e) {
                          message(glue("  Skip {nm}: {e$message}")); NULL
                        })
        sc  <- tryCatch(resolve_league_scoring(lid, verbose = FALSE)$offense,
                        error = function(e) {
                          message(glue("  Scoring skip {nm}: {e$message}")); NULL
                        })
        if (!is.null(cfg) && !is.null(sc)) {
          gathered[[cfg$league_name]] <- list(
            name    = cfg$league_name,
            config  = cfg,
            scoring = .complete_scoring(sc)
          )
        }
      }
    }
  }

  # Held-config: replace every league's roster settings with the reference.
  # Scoring is untouched, so the boards differ ONLY by scoring signature.
  if (!is.null(config_override) && length(gathered) > 0L) {
    for (nm in names(gathered)) {
      cfg <- config_override
      cfg$league_name <- nm
      gathered[[nm]]$config <- cfg
    }
    message(glue("  config_override applied to {length(gathered)} league(s): ",
                 "{config_override$num_teams} teams, ",
                 "QB{config_override$starters$QB}/RB{config_override$starters$RB}/",
                 "WR{config_override$starters$WR}/TE{config_override$starters$TE}",
                 "+{config_override$flex}FLEX, superflex={config_override$superflex}"))
  }

  if (isTRUE(include_dk)) {
    gathered[["DK Best Ball"]] <- list(
      name    = "DK Best Ball",
      config  = if (is.null(config_override)) dk_best_ball_config() else {
        cfg <- config_override; cfg$league_name <- "DK Best Ball"; cfg
      },
      scoring = .complete_scoring(DK_BEST_BALL_SCORING)
    )
  }

  if (length(gathered) == 0L) {
    message("batch_value_boards(): nothing to build.")
    return(invisible(list()))
  }

  # ---- the plan ----
  keys <- vapply(gathered, function(L) .scoring_key(L$scoring), character(1))

  # Scoring keys are long serialized strings that all begin with the same field
  # name, so printing a truncation of them distinguishes nothing. Label each
  # distinct key S1..Sn in first-seen order instead. Leagues sharing a label
  # share an engine run.
  key_levels <- unique(keys)
  key_label  <- setNames(paste0("S", seq_along(key_levels)), key_levels)

  message(glue("\n{strrep('=', 70)}"))
  message(glue("BATCH PLAN -- {length(gathered)} board(s), ",
               "{length(unique(keys))} engine run(s)"))
  message(glue("{strrep('=', 70)}"))
  for (i in seq_along(gathered)) {
    L  <- gathered[[i]]
    st <- L$config$starters
    message(glue(
      "  - {L$name}: format={L$config$format}, teams={L$config$num_teams}, ",
      "lineup QB{st$QB}/RB{st$RB}/WR{st$WR}/TE{st$TE}+{L$config$flex}FLEX | ",
      "ppr={L$scoring$ppr %||% NA}, pass_td={L$scoring$pass_td %||% NA}, ",
      "te_prem={L$scoring$te_premium %||% NA} | scoring={key_label[[keys[i]]]}"
    ))
  }
  shared <- table(keys)
  shared <- shared[shared > 1L]
  if (length(shared) > 0L) {
    for (k in names(shared)) {
      message(glue("  {key_label[[k]]} shared by: ",
                   "{paste(names(keys)[keys == k], collapse = ', ')} ",
                   "-> one engine run"))
    }
  }
  message(glue("{strrep('=', 70)}"))

  if (isTRUE(dry_run)) {
    message("  dry_run = TRUE. Nothing built.")
    return(invisible(list(leagues = gathered, keys = keys,
                          key_label = key_label,
                          n_engine_runs = length(key_levels))))
  }

  # ---- build, via R/46's single loop definition ----
  .build_boards(leagues        = gathered,
                dk_csv_path    = dk_csv_path,
                out_dir        = out_dir,
                season         = season,
                write_combined = write_combined,
                out_prefix     = out_prefix)
}
