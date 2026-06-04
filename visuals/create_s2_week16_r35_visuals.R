# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 16
# Visuals: R/35 Lineup + Waiver Optimizer
# File: examples/create_s2_week16_r35_visuals.R
#
# CHARTS
# ------
#   1. s2_week16_r35_dvp_heatmap.png / .html
#        Defense-vs-position matchup matrix: which defenses are easiest or
#        toughest for each offensive position. 32 defenses x 4 positions =
#        128 labeled cells, so an interactive HTML is saved alongside the PNG
#        per the density-threshold rule (50+ labeled points).
#
#   2. s2_week16_r35_matchup_shift.png
#        Matchup-adjustment dumbbell: how much does the DvP layer move each
#        player's projection? Gray dot = R/32 base; colored dot = DvP-adjusted.
#        Uses 2025 week 6 as a stand-in live week.
#
#   3. s2_week16_r35_dynasty_wire.png
#        Dynasty wire landscape: density distribution of adjusted VORP for
#        rostered vs available players, faceted by position. Answers the
#        question the stash waiver operates on -- what does the wire actually
#        look like?
#
# DATA REQUIREMENTS
# -----------------
#   Normalized 2025 pbp in data/season2_cache/ (Charts 1 and 2)
#   R/32 reconciled projections: s2_week15_reconciled_projections.rds
#   R/34 DEF projections: s2_week16_def_st_projections.rds
#   nflreadr::load_schedules() -- network, 2025 season (Chart 2)
#
# NOTES
# -----
#   Chart 2 uses 2025 week 6 as a stand-in live season (same strategy as
#   R/34 visuals). The DvP factors are from 2025, so chart 2 is internally
#   consistent: 2025 defenses vs 2026 player projections, matchup applied
#   using 2025 opponent data.
#
#   KEY INSIGHTS at the bottom are computed dynamically from the data.
#   Never hardcode them.
# ==============================================================================

# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------

STAND_IN_WEEK     <- 6L      # 2025 week used as stand-in live week in Chart 2
DVP_PRIOR_SEASONS <- 2025L   # season(s) for DvP computation

# Output
OUTPUT_DIR <- here::here("output", "plots")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

make_path <- function(name) file.path(OUTPUT_DIR, name)

PLOT1_PNG  <- make_path("s2_week16_r35_dvp_heatmap.png")
PLOT1_HTML <- make_path("s2_week16_r35_dvp_heatmap.html")
PLOT2_PNG  <- make_path("s2_week16_r35_matchup_shift.png")
PLOT3_PNG  <- make_path("s2_week16_r35_dynasty_wire.png")

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(htmlwidgets)
  library(here)
  library(glue)
  library(nflreadr)
})

suppressPackageStartupMessages({
  source(here::here("R", "35_lineup_optimizer.R"))
})

reconciled <- readRDS(here::here("data", "season2_cache",
                                 "s2_week15_reconciled_projections.rds"))
def_proj   <- readRDS(here::here("data", "season2_cache",
                                 "s2_week16_def_st_projections.rds"))

message(glue("Loaded {nrow(reconciled)} projections (R/32) | ",
             "{nrow(def_proj)} DEF projections (R/34)"))

# ------------------------------------------------------------------------------
# SHARED THEME AND CAPTION
# ------------------------------------------------------------------------------

CAPTION_BASE <- glue(
  "NFL Analytics Toolkit | Season {SEASON} Week 16 | R/35 Lineup + Waiver Optimizer\n",
  "Data: nflfastR 2010-2025"
)

theme_r35 <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title         = element_text(face = "bold", size = 13,
                                        margin = margin(b = 4)),
      plot.subtitle      = element_text(size = 9, color = "gray45",
                                        lineheight = 1.35,
                                        margin = margin(b = 8)),
      plot.caption       = element_text(size = 7.5, color = "gray55",
                                        hjust = 1, face = "italic",
                                        lineheight = 1.25),
      panel.grid.minor   = element_blank(),
      legend.title       = element_text(size = 9),
      axis.title         = element_text(size = 9),
      strip.text         = element_text(face = "bold", size = 10),
      plot.margin        = margin(10, 14, 8, 8)
    )
}

# Position color palette (consistent across all charts)
POS_COLORS <- c(QB = "#E15759", RB = "#4E79A7", WR = "#59A14F", TE = "#F28E2B")


# ==============================================================================
# CHART 1: Defense-vs-position matchup heatmap
# ==============================================================================
# PURPOSE: at-a-glance reference for which defenses to exploit or avoid at each
# position. Factor > 1 = easy matchup (defense gives up more than average);
# factor < 1 = tough matchup. 128 cells in the matrix.

message("\nChart 1: computing DvP factors from ", DVP_PRIOR_SEASONS, " cache...")
dvp <- compute_dvp_factors(seasons = DVP_PRIOR_SEASONS)

stopifnot(
  "Chart 1: DvP returned no data -- check pbp cache"  = nrow(dvp) > 0,
  "Chart 1: fewer than 4 positions in DvP output"     = dplyr::n_distinct(dvp$position) >= 4
)

n_def <- dplyr::n_distinct(dvp$def_team)

# Order defenses by average factor across positions (easiest at top).
def_order <- dvp %>%
  dplyr::group_by(def_team) %>%
  dplyr::summarise(avg_factor = mean(dvp_factor, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(avg_factor)) %>%
  dplyr::pull(def_team)

easiest_wr_def  <- dvp %>% dplyr::filter(position == "WR") %>%
  dplyr::arrange(dplyr::desc(dvp_factor)) %>% dplyr::slice(1)
toughest_rb_def <- dvp %>% dplyr::filter(position == "RB") %>%
  dplyr::arrange(dvp_factor) %>% dplyr::slice(1)

heatmap_data <- dvp %>%
  dplyr::mutate(
    def_team = factor(def_team, levels = rev(def_order)),
    position = factor(position, levels = c("QB", "RB", "WR", "TE"))
  )

p1 <- ggplot(heatmap_data,
             aes(x = position, y = def_team, fill = dvp_factor)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", dvp_factor)),
            size = 2.3, color = "gray15") +
  scale_fill_gradient2(
    low = "#1a6faf", mid = "white", high = "#b2182b",
    midpoint = 1.0,
    limits   = c(1 - DVP_CAP, 1 + DVP_CAP),
    name     = "DvP factor\n(>1 = easy matchup)"
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Matchup Intelligence: Fantasy Points Allowed by Defense and Position",
    subtitle = glue(
      "{DVP_PRIOR_SEASONS} season | {n_def} defenses | ",
      "Factor = pts allowed / league avg, capped at [{1 - DVP_CAP}, {1 + DVP_CAP}]\n",
      "Easiest WR matchup: {easiest_wr_def$def_team} ({round(easiest_wr_def$dvp_factor, 2)}) | ",
      "Toughest RB matchup: {toughest_rb_def$def_team} ({round(toughest_rb_def$dvp_factor, 2)})"
    ),
    x       = NULL,
    y       = NULL,
    caption = CAPTION_BASE
  ) +
  theme_r35() +
  theme(
    panel.grid  = element_blank(),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 9.5, face = "bold")
  )

ggsave(PLOT1_PNG, plot = p1, width = 6.5, height = 9.5, dpi = 300)
message("  Saved: ", PLOT1_PNG)

# Interactive HTML (128 labeled cells > 50 density threshold -- required).
p1_plotly <- plotly::ggplotly(p1, tooltip = c("x", "y", "fill"))
htmlwidgets::saveWidget(p1_plotly, PLOT1_HTML, selfcontained = TRUE)
message("  Saved: ", PLOT1_HTML, " (interactive, 128 cells)")


# ==============================================================================
# CHART 2: Matchup-adjustment dumbbell
# ==============================================================================
# PURPOSE: show how much the DvP layer moves individual player projections
# relative to the R/32 base. Gray dot = base; colored dot = adjusted.
# 2025 week STAND_IN_WEEK used as a stand-in live week for illustration.

message(glue("\nChart 2: building matchup shift for 2025 week {STAND_IN_WEEK}..."))

sched_raw <- tryCatch(
  nflreadr::load_schedules(seasons = 2025L),
  error = function(e) NULL
)
if (is.null(sched_raw) || nrow(sched_raw) == 0L) {
  message("  nflreadr::load_schedules unavailable -- skipping Chart 2.")
} else {
  sched_wk <- sched_raw %>%
    dplyr::filter(week == STAND_IN_WEEK) %>%
    dplyr::filter(!is.na(home_team), !is.na(away_team))

  wk_matchups <- dplyr::bind_rows(
    dplyr::transmute(sched_wk, team = home_team, opponent = away_team),
    dplyr::transmute(sched_wk, team = away_team, opponent = home_team)
  )

  shift_data <- reconciled %>%
    dplyr::filter(.data$position %in% LINEUP_OFFENSE_POSITIONS,
                  !is.na(.data$r32_posterior_mu)) %>%
    dplyr::left_join(wk_matchups, by = "team") %>%
    dplyr::filter(!is.na(opponent)) %>%           # exclude bye-week players
    dplyr::left_join(dvp, by = c("opponent" = "def_team", "position")) %>%
    dplyr::mutate(
      dvp_factor  = dplyr::coalesce(.data$dvp_factor, 1),
      adj_proj    = .data$r32_posterior_mu * .data$dvp_factor,
      delta       = .data$adj_proj - .data$r32_posterior_mu,
      match_dir   = dplyr::case_when(
        .data$delta >  0.25 ~ "Boosted",
        .data$delta < -0.25 ~ "Faded",
        TRUE                ~ "Neutral"
      ),
      player_label = glue("{player_name} ({team} vs {opponent})")
    ) %>%
    dplyr::arrange(dplyr::desc(.data$adj_proj)) %>%
    dplyr::slice_head(n = 25)

  stopifnot(
    "Chart 2: no players after matchup join" = nrow(shift_data) >= 2
  )

  n_boost  <- sum(shift_data$match_dir == "Boosted")
  n_fade   <- sum(shift_data$match_dir == "Faded")
  max_gain <- shift_data %>% dplyr::arrange(dplyr::desc(delta)) %>%
    dplyr::slice(1)
  max_fade <- shift_data %>% dplyr::arrange(delta) %>% dplyr::slice(1)

  p2 <- ggplot(shift_data,
               aes(y = stats::reorder(player_label, adj_proj))) +
    geom_segment(
      aes(x = r32_posterior_mu, xend = adj_proj,
          yend = stats::reorder(player_label, adj_proj)),
      color = "gray78", linewidth = 0.9
    ) +
    geom_point(aes(x = r32_posterior_mu), color = "gray72", size = 2.5) +
    geom_point(aes(x = adj_proj, color = position), size = 3) +
    geom_text(
      aes(x = adj_proj,
          label  = sprintf("%+.1f", delta),
          hjust  = ifelse(.data$delta >= 0, -0.4, 1.4)),
      size = 2.5, color = "gray35"
    ) +
    scale_color_manual(values = POS_COLORS, name = "Position") +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.12))) +
    labs(
      title    = "How Much Does the Matchup Move Each Projection?",
      subtitle = glue(
        "2025 week {STAND_IN_WEEK} as stand-in | Top 25 by DvP-adjusted projection\n",
        "Gray dot = R/32 base | Colored dot = matchup-adjusted | Label = delta | ",
        "{n_boost} boosted, {n_fade} faded among top 25"
      ),
      x       = "PPR pts/game",
      y       = NULL,
      caption = CAPTION_BASE
    ) +
    theme_r35() +
    theme(axis.text.y = element_text(size = 7.5))

  ggsave(PLOT2_PNG, plot = p2, width = 9.5, height = 7.5, dpi = 300)
  message("  Saved: ", PLOT2_PNG)
}


# ==============================================================================
# CHART 3: Dynasty wire landscape
# ==============================================================================
# PURPOSE: density distribution of adjusted VORP by roster status (rostered vs
# available), faceted by position. Answers the strategic question that drives
# the stash waiver: what does the dynasty wire actually look like, and where is
# the stash-target zone relative to replacement level?
#
# Rostered = top 15 per team (12-team, 180 total) ranked by adjusted VORP.
# Available = everyone else. This is a simulation; real leagues vary.

message("\nChart 3: computing dynasty wire landscape...")

ref_config <- build_league_config(
  "Reference 12-team PPR", num_teams = 12L,
  starters   = list(QB = 1L, RB = 2L, WR = 2L, TE = 1L),
  flex       = 1L, superflex = 0L, format = "ppr"
)

vorp_full <- compute_vorp_rankings(reconciled, ref_config)

n_rostered <- 12L * 15L   # 180 players rostered across a 12-team league

wire_data <- vorp_full %>%
  dplyr::filter(.data$position %in% LINEUP_OFFENSE_POSITIONS) %>%
  dplyr::arrange(dplyr::desc(.data$adjusted_vorp)) %>%
  dplyr::mutate(
    roster_rank    = dplyr::row_number(),
    roster_status  = ifelse(.data$roster_rank <= n_rostered,
                            "Rostered", "Available (wire)"),
    position       = factor(.data$position,
                            levels = c("QB", "RB", "WR", "TE"))
  )

stopifnot(
  "Chart 3: VORP data has no non-NA values" =
    sum(!is.na(wire_data$adjusted_vorp)) > 0,
  "Chart 3: fewer than 2 positions in VORP output" =
    dplyr::n_distinct(wire_data$position) >= 2
)

# Per-position replacement PPG for annotation labels
repl_ppg <- vorp_full %>%
  dplyr::group_by(.data$position) %>%
  dplyr::summarise(repl = mean(.data$replacement_ppg, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::filter(.data$position %in% LINEUP_OFFENSE_POSITIONS) %>%
  dplyr::mutate(position = factor(.data$position,
                                  levels = c("QB","RB","WR","TE")))

wire_n <- sum(wire_data$roster_status == "Available (wire)")

p3 <- ggplot(wire_data,
             aes(x = adjusted_vorp, fill = roster_status,
                 color = roster_status)) +
  geom_density(alpha = 0.30, linewidth = 0.8, trim = TRUE) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray45", linewidth = 0.6) +
  annotate("text", x = 0.2, y = Inf, label = "Replacement",
           hjust = 0, vjust = 1.5, size = 2.8, color = "gray45",
           fontface = "italic") +
  facet_wrap(~position, scales = "free_y", ncol = 2) +
  scale_fill_manual(
    values = c("Rostered" = "#4E79A7", "Available (wire)" = "#F28E2B"),
    name   = NULL
  ) +
  scale_color_manual(
    values = c("Rostered" = "#4E79A7", "Available (wire)" = "#F28E2B"),
    name   = NULL
  ) +
  labs(
    title    = "The Dynasty Wire: What Does the Waiver Pool Look Like?",
    subtitle = glue(
      "12-team simulation | Rostered = top {n_rostered} by VORP | ",
      "Wire = {wire_n} players below that threshold\n",
      "Players clustered near VORP = 0 are the stash-target zone: ",
      "close to starting-quality, most likely to crack a lineup"
    ),
    x       = "Adjusted VORP (PPR pts/game above replacement)",
    y       = "Density",
    caption = CAPTION_BASE
  ) +
  theme_r35() +
  theme(legend.position = "bottom")

ggsave(PLOT3_PNG, plot = p3, width = 9, height = 6, dpi = 300)
message("  Saved: ", PLOT3_PNG)


# ==============================================================================
# KEY INSIGHTS (computed from data -- never hardcoded)
# ==============================================================================

top_wr_matchup  <- dvp %>% dplyr::filter(position == "WR") %>%
  dplyr::arrange(dplyr::desc(dvp_factor)) %>% dplyr::slice(1)
top_rb_matchup  <- dvp %>% dplyr::filter(position == "RB") %>%
  dplyr::arrange(dplyr::desc(dvp_factor)) %>% dplyr::slice(1)
worst_te_matchup <- dvp %>% dplyr::filter(position == "TE") %>%
  dplyr::arrange(dvp_factor) %>% dplyr::slice(1)

dvp_spread <- dvp %>%
  dplyr::group_by(position) %>%
  dplyr::summarise(spread = max(dvp_factor) - min(dvp_factor), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(spread))

message(glue("\n{strrep('=', 70)}"))
message(glue("R/35 visuals | Season {SEASON} Week 16"))
message(strrep("=", 70))
message(glue("  Charts saved to: {OUTPUT_DIR}"))
message(glue("  Easiest WR matchup ({DVP_PRIOR_SEASONS}): {top_wr_matchup$def_team}",
             " (factor {round(top_wr_matchup$dvp_factor, 2)},",
             " {round(top_wr_matchup$dvp_allowed_ppg, 1)} PPG allowed)"))
message(glue("  Easiest RB matchup ({DVP_PRIOR_SEASONS}): {top_rb_matchup$def_team}",
             " (factor {round(top_rb_matchup$dvp_factor, 2)},",
             " {round(top_rb_matchup$dvp_allowed_ppg, 1)} PPG allowed)"))
message(glue("  Toughest TE matchup ({DVP_PRIOR_SEASONS}): {worst_te_matchup$def_team}",
             " (factor {round(worst_te_matchup$dvp_factor, 2)})"))
message(glue("  Position with widest matchup spread: {dvp_spread$position[1]}",
             " (range {round(dvp_spread$spread[1], 3)} from toughest to easiest)"))
message(glue("  Dynasty wire: {wire_n} available players in reference 12-team sim"))
message(strrep("=", 70))
message("Done.")
