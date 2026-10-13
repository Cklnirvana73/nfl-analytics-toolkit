# ==============================================================================
# 59_weekly_projection_bands.R
#
# Weekly projection bands: per-player mean, low and high.  [2026-09-18]
#
# WHAT THIS IS
#   An export, not a model. Every number comes from two places:
#     mean    r32_posterior_mu   (R/32 reconciled projections)
#     spread  r32_sigma_week     (R/32: sqrt(tau^2 + omega^2), one-week SD)
#     shape   R/50 shape table   (per-position empirical z quantiles, built on
#                                 within-player standardized residuals)
#   low/high = predictive_quantile(position, p, mu, sigma_week, shape_table)
#            = mu + sigma_week * z_position(p)
#
#   A Normal band (mu +/- z * sigma_week) is emitted beside it as a DIAGNOSTIC.
#   R/36 still scores start/sit under a symmetric Normal; the delta columns
#   show how far that assumption is from the measured shape (RB/WR skew ~1.39,
#   TE ~1.35, QB ~0.45). Wave A2 item R5 is the fix on the R/36 side.
#
# WHAT IT IS NOT
#   Week-specific. r32_posterior_mu is a per-game season mean, updated by the
#   R/29 YTD rule (see ytd_weight_mode). No opponent, no Vegas. R/35's offense
#   matchup factor is neutralized (DVP_CAP_OFFENSE = 0.00) on gate evidence,
#   so nothing shifts the mean for the opponent. Bye teams are flagged from
#   the schedule and their band is zeroed, the same rule R/35 v1.1 applies.
#
# AVAILABILITY  [2026-09-18, v2]
#   The chain knows nothing about who will play. A commissioner-list player and
#   an injured player both surfaced in the top 12 of the first week 2 run with
#   full bands. Two feeds are joined, both flag-only (the band is left intact so
#   the number still reads as "what he is worth when active"; playable = FALSE
#   is the gate downstream consumers apply):
#     Sleeper player DB  status / injury_status  (same vocabulary as R/35:
#                        OUT_FOR_SEASON_STATUSES, OUT_THIS_WEEK_INJURY)
#     nflreadr injuries  report_status for the week (Out / Doubtful /
#                        Questionable)
#   Any Sleeper status other than "Active" that is not in the out-for-season
#   list is carried verbatim as "status:<value>" rather than mapped, so a
#   designation this file has never seen (exempt list, suspension, NFI) is
#   visible instead of silently "active".
#
#   [v3, 2026-09-18] Two feed gaps from the first v2 run, both fixed here:
#   - Sleeper rows with no gsis_id (Bowers) missed the join. A name+position
#     fallback now covers rows the gsis join left NA, unique matches only.
#   - An exempt-list player (Jacobs) is "Active" in Sleeper and absent from
#     the injury report. Nothing automated carries that. A manual override CSV
#     gets the final word, using EXACTLY R/35's load_availability() contract
#     (columns gsis_id, status in {active, out_week, out_for_season},
#     optional return_week) at R59_OVERRIDE_PATH, so one file serves both
#     tools; pass the same path as availability_path to R/35.
#   - Sleeper's IR players carry status "Inactive" + injury_status "IR", not
#     status "Injured Reserve". Mapped to out_for_season here; R/35's
#     OUT_FOR_SEASON_STATUSES never matches that pair (logged, not fixed here).
#
# QUANTILES
#   low = p10, q1 = p25, q3 = p75, high = p90, p95 = p95. Under the measured
#   right skew (RB/WR ~1.39) the p90 sits close to the Normal p90; the tail
#   divergence opens at p95, which is why p95 is carried as the ceiling column.
#
# USAGE
#   source(here::here("R", "59_weekly_projection_bands.R"))
#   bands <- run_weekly_bands(week = 2)
#   # per-site chain (R/65): bands <- run_weekly_bands(5, recon_path = <fd>, out_prefix = "s3_fd_")
# ==============================================================================

library(dplyr)
library(glue)
library(purrr)
library(readr)
source(here::here("R", "50_weekly_predictive_shape.R"))

R59_RECON_PATH  <- here::here("data", "season2_cache", "s2_week15_reconciled_projections.rds")
R59_SHAPE_PATH  <- SHAPE_CACHE_PATH            # from R/50: data/season2_cache/s3_weekly_shape_table.rds
R59_OUTPUT_DIR  <- here::here("data", "season3_cache")
R59_SCHEMA_TAG  <- "s3_r59_bands_v3"
# Shared manual override, R/35 load_availability() contract. Final word.
R59_OVERRIDE_PATH <- here::here("data", "season3_cache", "availability_overrides.csv")
R59_OVERRIDE_STATUSES <- c("active", "out_week", "out_for_season")
R59_SLEEPER_PATH <- here::here("data", "season2_cache", "sleeper_players_nfl.rds")
# Mirrors R/35 (not sourced: R/35 pulls in the whole optimizer chain).
R59_OUT_FOR_SEASON_STATUSES <- c("Injured Reserve", "IR",
                                 "Physically Unable to Perform", "PUP",
                                 "Non Football Injury", "NFI")
R59_OUT_THIS_WEEK_INJURY    <- c("Out", "IR", "Doubtful")
# Sleeper injury_status values that mean the season, when paired with a
# non-Active status. Observed 2026-09-18: status "Inactive" + injury_status "IR".
R59_SEASON_INJURY_STATUSES  <- c("IR", "PUP", "NFI", "Sus")

# Name normalization for the Sleeper fallback join: lowercase, strip suffixes
# and punctuation. Local so R/59 does not depend on R/19's helpers.
.r59_norm_name <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("\\.", "", x)
  x <- gsub("'", "", x)
  x <- gsub("\\s+(jr|sr|ii|iii|iv|v)$", "", x)
  x <- gsub("[^a-z ]", "", x)
  trimws(gsub("\\s+", " ", x))
}
R59_P_95        <- 0.95
R59_POSITIONS   <- c("QB", "RB", "WR", "TE")
R59_P_LOW       <- 0.10
R59_P_HIGH      <- 0.90
R59_P_Q1        <- 0.25
R59_P_Q3        <- 0.75

# [2026-10-01] Output prefix. The default "s3_" reproduces the original file
# names exactly (s3_r59_weekly_bands_wNN). A per-site chain passes its own
# prefix (R/65: "s3_fd_" -> s3_fd_r59_weekly_bands_wNN), so a FanDuel run never
# overwrites the flat-PPR bands. R/60's file glob is anchored on
# ^s3_r59_weekly_bands_w, so prefixed files are not loaded by R/60.
R59_OUT_PREFIX_DEFAULT <- "s3_"

.r59_out_path <- function(week, ext, out_prefix = R59_OUT_PREFIX_DEFAULT) {
  file.path(R59_OUTPUT_DIR,
            sprintf("%sr59_weekly_bands_w%02d.%s", out_prefix, as.integer(week), ext))
}

# ------------------------------------------------------------------------------
# .r59_week_teams: who plays this week, and whom
# ------------------------------------------------------------------------------
.r59_week_teams <- function(season, week) {
  sched <- tryCatch(nflreadr::load_schedules(seasons = season),
                    error = function(e) NULL)
  if (is.null(sched)) {
    message("  load_schedules() unavailable; bye/opponent columns left NA")
    return(NULL)
  }
  wk <- sched %>%
    dplyr::filter(.data$game_type == "REG", .data$week == !!week)
  dplyr::bind_rows(
    wk %>% dplyr::transmute(team = home_team, opponent = away_team, home = TRUE),
    wk %>% dplyr::transmute(team = away_team, opponent = home_team, home = FALSE)
  )
}

# ------------------------------------------------------------------------------
# .r59_availability: Sleeper status + weekly injury report, keyed on gsis_id
# ------------------------------------------------------------------------------
.r59_availability <- function(season, week, sleeper_path = R59_SLEEPER_PATH) {

  # Sleeper player DB (cached by R/19; R/29 refreshes it on a 24h TTL)
  sl <- NULL
  if (file.exists(sleeper_path)) {
    sl_raw <- tryCatch(readRDS(sleeper_path), error = function(e) NULL)
    if (!is.null(sl_raw) && all(c("gsis_id", "status", "injury_status") %in% names(sl_raw))) {
      age_h <- round(as.numeric(difftime(Sys.time(), file.mtime(sleeper_path), units = "hours")), 1)
      message(glue("  Sleeper player DB: {nrow(sl_raw)} rows, cache age {age_h}h"))
      sl_all <- sl_raw %>%
        dplyr::mutate(gsis_id = trimws(.data$gsis_id),
                      gsis_id = dplyr::if_else(!is.na(gsis_id) & nchar(gsis_id) > 0,
                                               gsis_id, NA_character_),
                      nm_key = paste0(.r59_norm_name(.data$full_name), "|",
                                      toupper(as.character(.data$position)))) %>%
        dplyr::filter(!is.na(.data$full_name), !is.na(.data$position)) %>%
        # one row per key: prefer a row with a team, then the first
        dplyr::arrange(is.na(.data$team))
      sl <- sl_all %>%
        dplyr::filter(!is.na(gsis_id)) %>%
        dplyr::distinct(gsis_id, .keep_all = TRUE) %>%
        dplyr::select(nfl_gsis_id = gsis_id,
                      sleeper_status = status,
                      sleeper_injury = injury_status)
      # Name+position fallback: only keys that resolve to exactly one Sleeper row
      sl_by_name <- sl_all %>%
        dplyr::group_by(nm_key) %>%
        dplyr::filter(dplyr::n() == 1L) %>%
        dplyr::ungroup() %>%
        dplyr::select(nm_key,
                      sleeper_status_nm = status,
                      sleeper_injury_nm = injury_status)
      message(glue("    gsis-keyed rows {nrow(sl)}; name-keyed fallback rows {nrow(sl_by_name)}"))
    }
  }
  if (is.null(sl)) {
    message("  Sleeper player DB unavailable; sleeper_status/sleeper_injury left NA")
    sl_by_name <- NULL
  }

  # nflreadr weekly injury report
  inj <- tryCatch(nflreadr::load_injuries(seasons = season), error = function(e) NULL)
  if (!is.null(inj) && all(c("gsis_id", "week", "report_status") %in% names(inj))) {
    inj <- inj %>%
      dplyr::filter(.data$week == !!week) %>%
      dplyr::mutate(gsis_id = trimws(.data$gsis_id)) %>%
      dplyr::filter(!is.na(.data$gsis_id)) %>%
      dplyr::distinct(gsis_id, .keep_all = TRUE) %>%
      dplyr::select(nfl_gsis_id = gsis_id, report_status,
                    dplyr::any_of("practice_status"))
    message(glue("  Injury report week {week}: {nrow(inj)} players listed"))
  } else {
    message(glue("  Injury report for week {week} unavailable; report_status left NA"))
    inj <- NULL
  }

  # Manual override (R/35 contract). Final word.
  ov <- NULL
  if (file.exists(R59_OVERRIDE_PATH)) {
    ov_raw <- tryCatch(readr::read_csv(R59_OVERRIDE_PATH, show_col_types = FALSE),
                       error = function(e) NULL)
    if (is.null(ov_raw) || !all(c("gsis_id", "status") %in% names(ov_raw))) {
      warning("R/59: override CSV must have columns gsis_id and status; ignoring.",
              call. = FALSE)
    } else {
      bad <- setdiff(unique(ov_raw$status), R59_OVERRIDE_STATUSES)
      if (length(bad) > 0) {
        warning(glue("R/59: override status value(s) not in {paste(R59_OVERRIDE_STATUSES, collapse = '/')}: ",
                     "{paste(bad, collapse = ', ')}; those rows ignored."), call. = FALSE)
      }
      ov <- ov_raw %>%
        dplyr::filter(.data$status %in% R59_OVERRIDE_STATUSES) %>%
        dplyr::transmute(nfl_gsis_id = as.character(gsis_id),
                         override_status = as.character(status),
                         override_return_week = if ("return_week" %in% names(ov_raw))
                           suppressWarnings(as.integer(return_week)) else NA_integer_) %>%
        dplyr::distinct(nfl_gsis_id, .keep_all = TRUE)
      message(glue("  Manual overrides: {nrow(ov)} row(s) from {basename(R59_OVERRIDE_PATH)}"))
    }
  } else {
    message(glue("  No manual override file at {basename(R59_OVERRIDE_PATH)} (optional)"))
  }

  list(sleeper = sl, sleeper_by_name = sl_by_name, injuries = inj, overrides = ov)
}

# ------------------------------------------------------------------------------
# run_weekly_bands
# ------------------------------------------------------------------------------
run_weekly_bands <- function(week,
                             season = 2026L,
                             recon_path = R59_RECON_PATH,
                             shape_path = R59_SHAPE_PATH,
                             p_low = R59_P_LOW,
                             p_high = R59_P_HIGH,
                             write_outputs = TRUE,
                             print_top = 12L,
                             out_prefix = R59_OUT_PREFIX_DEFAULT) {

  week   <- as.integer(week)
  season <- as.integer(season)

  message(glue("\n{strrep('=', 70)}"))
  message(glue("R/59: Weekly projection bands -- season {season} week {week}"))
  message(glue("{strrep('=', 70)}\n"))

  # ---- inputs ----
  if (!file.exists(recon_path)) stop("R/59: reconciled file not found: ", recon_path, call. = FALSE)
  if (!file.exists(shape_path)) stop("R/59: shape table not found: ", shape_path, call. = FALSE)
  rec <- readRDS(recon_path)
  st  <- readRDS(shape_path)

  need <- c("player_uid", "nfl_gsis_id", "player_name", "position", "team",
            "prior_source", "r32_posterior_mu", "r32_sigma_week", "schema_tag")
  miss <- setdiff(need, names(rec))
  if (length(miss) > 0) stop("R/59: reconciled file missing: ", paste(miss, collapse = ", "), call. = FALSE)

  recon_tag <- unique(rec$schema_tag)
  message(glue("  reconciled: {nrow(rec)} rows, schema {paste(recon_tag, collapse = ',')}, ",
               "mtime {format(file.mtime(recon_path), '%Y-%m-%d %H:%M')}"))
  message(glue("  shape table: built {format(st$meta$built_at, '%Y-%m-%d')} on ",
               "{paste(st$meta$target_seasons, collapse = ',')}, ",
               "{st$meta$n_weeks_total} pooled weeks"))
  for (p in R59_POSITIONS) {
    message(glue("    {p}: method={st[[p]]$method} n={st[[p]]$n_weeks} skew_z={round(st[[p]]$skew_z, 3)}"))
  }

  # YTD provenance (present once R/29 carries the 2026-09-18 columns)
  has_ytd_cols <- all(c("ytd_n", "ytd_weight_mode") %in% names(rec))
  if (has_ytd_cols) {
    message(glue("  YTD: {sum(rec$ytd_n > 0, na.rm = TRUE)} of {nrow(rec)} players with observed games; ",
                 "weight mode: {paste(names(table(rec$ytd_weight_mode)), table(rec$ytd_weight_mode), sep = '=', collapse = ', ')}"))
  } else if ("ytd_n" %in% names(rec)) {
    message(glue("  YTD: {sum(rec$ytd_n > 0, na.rm = TRUE)} of {nrow(rec)} players with observed games; ",
                 "weight mode column absent (pre-2026-09-18 R/29)"))
  } else {
    message("  YTD columns absent from reconciled file")
  }

  # ---- schedule + availability ----
  wt <- .r59_week_teams(season, week)
  av <- .r59_availability(season, week)

  # ---- bands ----
  df <- rec %>%
    dplyr::filter(.data$position %in% R59_POSITIONS,
                  !is.na(.data$r32_posterior_mu),
                  !is.na(.data$r32_sigma_week), .data$r32_sigma_week > 0)
  n_dropped <- nrow(rec %>% dplyr::filter(.data$position %in% R59_POSITIONS)) - nrow(df)
  if (n_dropped > 0) message(glue("  {n_dropped} row(s) dropped for NA/zero mu or sigma (no band fabricated)"))

  mu  <- df$r32_posterior_mu
  sig <- df$r32_sigma_week
  pos <- df$position

  df <- df %>%
    dplyr::mutate(
      season   = season,
      week     = week,
      mu       = mu,
      sigma_wk = sig,
      low      = predictive_quantile(pos, p_low,  mu, sig, st),
      q1       = predictive_quantile(pos, R59_P_Q1, mu, sig, st),
      q3       = predictive_quantile(pos, R59_P_Q3, mu, sig, st),
      high     = predictive_quantile(pos, p_high, mu, sig, st),
      p95      = predictive_quantile(pos, R59_P_95, mu, sig, st),
      # Normal diagnostic at the same probabilities
      low_norm  = mu + sig * stats::qnorm(p_low),
      high_norm = mu + sig * stats::qnorm(p_high),
      delta_low  = low  - low_norm,    # positive: shape floor sits ABOVE the Normal floor
      delta_high = high - high_norm,   # positive: shape ceiling sits ABOVE the Normal ceiling
      band_width      = high - low,
      band_width_norm = high_norm - low_norm,
      p_low = p_low, p_high = p_high
    )

  if (!is.null(wt)) {
    df <- df %>%
      dplyr::left_join(wt, by = "team") %>%
      dplyr::mutate(
        on_bye = is.na(.data$opponent),
        # Bye: no game, no points. Same rule as R/35 v1.1 (.apply_matchup).
        dplyr::across(c(mu, low, q1, q3, high, p95, low_norm, high_norm),
                      ~ dplyr::if_else(on_bye, 0, .x))
      )
    message(glue("  schedule: {sum(!df$on_bye)} players on teams playing, ",
                 "{sum(df$on_bye)} on bye ({paste(sort(unique(df$team[df$on_bye])), collapse = ', ')})"))
  } else {
    df$opponent <- NA_character_; df$home <- NA; df$on_bye <- NA
  }

  # ---- availability join (flag only; band left intact) ----
  if (!is.null(av$sleeper)) {
    df <- df %>% dplyr::left_join(av$sleeper, by = "nfl_gsis_id", na_matches = "never")
  } else {
    df$sleeper_status <- NA_character_; df$sleeper_injury <- NA_character_
  }
  # Name+position fallback for rows the gsis join left NA
  df$sleeper_match <- dplyr::if_else(!is.na(df$sleeper_status), "gsis", NA_character_)
  if (!is.null(av$sleeper_by_name)) {
    df <- df %>%
      dplyr::mutate(nm_key = paste0(.r59_norm_name(.data$player_name), "|",
                                    toupper(as.character(.data$position)))) %>%
      dplyr::left_join(av$sleeper_by_name, by = "nm_key") %>%
      dplyr::mutate(
        use_nm = is.na(sleeper_status) & !is.na(sleeper_status_nm),
        sleeper_status = dplyr::if_else(use_nm, sleeper_status_nm, sleeper_status),
        sleeper_injury = dplyr::if_else(use_nm, sleeper_injury_nm, sleeper_injury),
        sleeper_match  = dplyr::if_else(use_nm, "name_position", sleeper_match)
      ) %>%
      dplyr::select(-nm_key, -sleeper_status_nm, -sleeper_injury_nm, -use_nm)
    message(glue("  Sleeper match: gsis {sum(df$sleeper_match %in% 'gsis')}, ",
                 "name+position {sum(df$sleeper_match %in% 'name_position')}, ",
                 "unmatched {sum(is.na(df$sleeper_match))}"))
  }
  if (!is.null(av$injuries)) {
    df <- df %>% dplyr::left_join(av$injuries, by = "nfl_gsis_id", na_matches = "never")
  } else {
    df$report_status <- NA_character_
  }
  if (!"practice_status" %in% names(df)) df$practice_status <- NA_character_
  if (!is.null(av$overrides)) {
    df <- df %>% dplyr::left_join(av$overrides, by = "nfl_gsis_id", na_matches = "never")
  } else {
    df$override_status <- NA_character_; df$override_return_week <- NA_integer_
  }

  df <- df %>%
    dplyr::mutate(
      availability_inferred = dplyr::case_when(
        on_bye %in% TRUE ~ "bye",
        sleeper_status %in% R59_OUT_FOR_SEASON_STATUSES ~ "out_for_season",
        !is.na(sleeper_status) & sleeper_status != "Active" &
          sleeper_injury %in% R59_SEASON_INJURY_STATUSES ~ "out_for_season",
        !is.na(sleeper_status) & sleeper_status != "Active" ~ paste0("status:", sleeper_status),
        sleeper_injury %in% R59_OUT_THIS_WEEK_INJURY ~ "out_week",
        report_status %in% c("Out", "Doubtful") ~ "out_week",
        sleeper_injury %in% "Questionable" | report_status %in% "Questionable" ~ "questionable",
        TRUE ~ "active"
      ),
      # Manual override is the final word, except a bye is a bye.
      availability = dplyr::case_when(
        on_bye %in% TRUE ~ "bye",
        !is.na(override_status) ~ override_status,
        TRUE ~ availability_inferred
      ),
      avail_source = dplyr::case_when(
        on_bye %in% TRUE ~ "schedule",
        !is.na(override_status) ~ "manual_override",
        !is.na(sleeper_match) | !is.na(report_status) ~ "inferred",
        TRUE ~ "no_feed"
      ),
      playable = availability %in% c("active", "questionable")
    )
  n_ov <- sum(df$avail_source == "manual_override")
  if (n_ov > 0) message(glue("  {n_ov} row(s) set by manual override"))

  out <- df %>%
    dplyr::transmute(
      season, week, player_uid, nfl_gsis_id, player_name, position, team,
      opponent, home, on_bye,
      availability, playable, avail_source, availability_inferred,
      sleeper_status, sleeper_injury, sleeper_match, report_status, practice_status,
      override_return_week,
      prior_source,
      ytd_n           = if ("ytd_n" %in% names(df)) ytd_n else NA_integer_,
      ytd_weight_mode = if ("ytd_weight_mode" %in% names(df)) ytd_weight_mode else NA_character_,
      mu       = round(mu, 2),
      sigma_wk = round(sigma_wk, 2),
      low      = round(low, 2),
      q1       = round(q1, 2),
      q3       = round(q3, 2),
      high     = round(high, 2),
      p95      = round(p95, 2),
      low_norm  = round(low_norm, 2),
      high_norm = round(high_norm, 2),
      delta_low  = round(delta_low, 2),
      delta_high = round(delta_high, 2),
      band_width      = round(band_width, 2),
      band_width_norm = round(band_width_norm, 2),
      p_low, p_high,
      recon_schema = paste(recon_tag, collapse = ","),
      shape_built  = as.character(as.Date(st$meta$built_at)),
      schema_tag   = R59_SCHEMA_TAG,
      built_at     = Sys.time()
    ) %>%
    dplyr::arrange(dplyr::desc(.data$mu))

  # ---- availability diagnostics ----
  message("\n  Availability (all positions):")
  print(out %>% dplyr::count(availability, sort = TRUE))
  flagged_top <- out %>%
    dplyr::filter(!playable) %>%
    dplyr::group_by(position) %>%
    dplyr::mutate(pos_rank = dplyr::row_number()) %>%
    dplyr::ungroup()
  if (nrow(flagged_top) > 0) {
    message("\n  Not playable, by position (showing first 15 by mean):")
    print(flagged_top %>%
            dplyr::arrange(dplyr::desc(mu)) %>%
            dplyr::select(player_name, position, team, mu, availability, avail_source,
                          sleeper_status, sleeper_injury, report_status) %>%
            dplyr::slice_head(n = 15), n = 15)
  }

  # ---- diagnostics ----
  message("\n  Normal-vs-shape delta by position (points), players not on bye:")
  print(out %>%
          dplyr::filter(!on_bye %in% TRUE) %>%
          dplyr::group_by(position) %>%
          dplyr::summarise(n = dplyr::n(),
                           mean_delta_low  = round(mean(delta_low), 2),
                           mean_delta_high = round(mean(delta_high), 2),
                           med_width       = round(stats::median(band_width), 2),
                           med_width_norm  = round(stats::median(band_width_norm), 2),
                           .groups = "drop"))

  for (p in R59_POSITIONS) {
    message(glue("\n  Top {print_top} {p} by mean, playable only:"))
    print(out %>%
            dplyr::filter(position == p, playable) %>%
            dplyr::slice_head(n = print_top) %>%
            dplyr::select(player_name, team, opponent, mu, low, high, p95, sigma_wk,
                          ytd_n, availability),
          n = print_top)
  }

  # ---- write ----
  if (write_outputs) {
    dir.create(R59_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
    saveRDS(out, .r59_out_path(week, "rds", out_prefix))
    readr::write_csv(out, .r59_out_path(week, "csv", out_prefix))
    message(glue("\n  Wrote {.r59_out_path(week, 'rds', out_prefix)}"))
    message(glue("  Wrote {.r59_out_path(week, 'csv', out_prefix)}"))
  }

  invisible(out)
}
