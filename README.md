# NFL Analytics Toolkit

An R toolkit for NFL play-by-play analysis and fantasy football decision-making,
built in public over three seasons. It covers data loading and validation,
feature engineering, predictive models, a season-long and weekly projection
engine, college-to-NFL prospect translation, and lineup and DFS optimization.
Every model change that reaches production passes an out-of-sample gate first.

Author: Christian K. LeBlanc

## Status

Season 3: the toolkit is in production. Each week is written up as a long-form
Substack post covering what the models say and any new builds or edits to the
toolkit. Weekly outputs include projection bands, usage and matchup reports,
start/sit and DFS lineups, prop sheets, and model-vs-market comparisons against
ADP and betting lines.

## What's in it

69 numbered modules in R/, sourced in order. Each builds on the ones before it.

Season 1, foundations (R/01 to R/14)
- Data loading and validation, player, team and game statistics
- Consistency metrics and predictive validation
- Feature layers: EPA trends, usage and opportunity, game script and leverage,
  opponent adjustment
- XGBoost fantasy points model, boom/bust classifier, ensemble pipeline,
  season-long projection system

Season 2, the projection engine (R/15 to R/38)
- Multi-season play-by-play and player-season panels, extended scoring
- Sleeper API integration, strength-of-schedule reconciliation, aging curves
- College-to-NFL translation model (CFB play-by-play and player panels through
  final prospect scores and signal tiers)
- Projection engine: team volume, player volume allocation, reconciliation,
  VORP rankings, DEF/ST projection
- Lineup optimizer, trade value model, Shiny decision app

Season 3, production and gates (R/36, R/39 to R/71)
- Out-of-sample gates on every layer: efficiency, volume and blend weights,
  uncertainty calibration, in-season update rule, features, RB blend, DEF/ST,
  Vegas offense shift, DFS projection blend
- Weekly projection bands, usage and matchup reports, prop sheet
- Value boards (VORP vs market ADP) and HTML draft boards
- Schema contract checks and player identity resolution
- DFS: FanDuel optimizer, lineup simulation and ranking, forward scoreboard

## Design principles

- No lookahead: features use only information available before the game
  being predicted.
- Gate before ship: a model change is adopted only if it beats the current
  version out of sample. Failed gates are kept and documented, not deleted.
- Paths through here::here(), run from the project root.
- Schema tags on cached artifacts so downstream modules fail loudly on a
  mismatch instead of reading stale data.

## Getting started

    install.packages(c("nflreadr", "nflfastR", "cfbfastR", "tidyverse",
                       "tidymodels", "xgboost", "glue", "here", "shiny"))

Open nfl-analytics-toolkit.Rproj and run scripts from the project root. The
weekly scripts in examples/ show how the modules chain together, for example:

    source("examples/example_season2_week17.R")

Data is pulled from nflverse and cached locally under data/ (not tracked).

## Project structure

    nfl-analytics-toolkit/
    ├── R/              Pipeline modules (numbered, sourced in order)
    ├── tests/          testthat suites; tests/legacy_runners/ holds early weekly runners
    ├── examples/       Weekly example and verification scripts
    ├── visuals/        NFL figure scripts (write to output/plots/, not tracked)
    ├── analysis/       One-off studies
    ├── diagnostics/    Diagnostic scripts
    ├── shiny_app/      Season 2 fantasy decision app
    ├── shiny_dfs/      DFS lineup app
    ├── scripts/        deploy/, season3/, dev/
    ├── stats_posts/    Statistics Series: scripts, figures, per-post data
    ├── reference/      Column and schema references
    ├── archive/        Superseded reference copies (never sourced)
    ├── figures/        Curated figures
    ├── output/         Pipeline artifacts (mostly not tracked)
    └── data/           Cached data (not tracked)

## Testing

64 test files under tests/, using testthat.

## Statistics Series

stats_posts/ holds the code, data and figures for a LinkedIn series that
teaches statistics with NFL data (posts 3 through 47 so far). Those scripts
read the toolkit's caches but never write outside stats_posts/.

## Contact

Christian K. LeBlanc
LinkedIn: https://www.linkedin.com/in/christian-k-leblanc/

## License

MIT
