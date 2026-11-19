# NFL Analytics Toolkit

🚧 **Under Development** - Building a production-ready NFL analytics toolkit over 12 weeks

## Project Overview

A modular, well-tested R package for NFL play-by-play analysis, fantasy football predictions, and player analytics. Built with software engineering best practices and statistical rigor.

## Current Progress

- [x] Week 1: Data loading & validation infrastructure (In Progress)
- [ ] Week 2: Fantasy scoring engine
- [ ] Week 3: Player consistency metrics
- [ ] Week 4: Comprehensive testing framework
- [ ] Week 5-8: Feature engineering
- [ ] Week 9-12: Predictive modeling

## Installation
```r
# Install dependencies
install.packages(c("nflfastR", "tidyverse", "nflplotR"))

# Clone and run examples
# Coming soon...
```

## Features (In Development)

- **Data Infrastructure**: Validated data loading with caching and error handling
- **Fantasy Scoring**: Modular engine supporting multiple scoring formats
- **Player Analytics**: Consistency metrics, feature engineering, similarity analysis
- **ML Models**: XGBoost predictor with interpretability
- **Testing**: 85%+ code coverage with unit and integration tests

## Project Structure
```
nfl-analytics-toolkit/
├── R/                  # Pipeline modules (numbered, sourced in order)
├── tests/              # testthat suites; tests/legacy_runners/ holds the early weekly runners
├── examples/           # Weekly example and verification scripts
├── visuals/            # NFL figure scripts (write to output/plots/, not tracked)
├── analysis/           # One-off studies
├── diagnostics/        # Diagnostic scripts
├── shiny_app/          # Season 2 fantasy decision app
├── shiny_dfs/          # DFS lineup app
├── scripts/            # deploy/ (shinyapps.io), season3/, dev/
├── stats_posts/        # LinkedIn Statistics Series: scripts, figures, per-post data
├── reference/          # Column and schema references
├── archive/            # Superseded reference copies (never sourced)
├── figures/            # Curated figures
├── output/             # Pipeline artifacts (mostly not tracked)
└── data/               # Cached data (not tracked)
```

## Skills Demonstrated

- Software engineering (modular design, testing, documentation)
- Statistical analysis (CV validation, effect sizes, proper hypothesis testing)
- Machine learning (feature engineering, XGBoost, model interpretation)
- Domain expertise (NFL-specific edge cases and data quirks)

## Development Timeline

Building over 12 weeks (Feb-Apr 2026) with weekly LinkedIn posts documenting progress.

Follow along: https://www.linkedin.com/in/christian-k-leblanc/

## Contact

Christian K. LeBlanc
- LinkedIn: https://www.linkedin.com/in/christian-k-leblanc/
- Email: [email removed]

## License

MIT