library(dplyr)
library(purrr)
library(tidymodels)
library(xgboost)
library(glue)
library(here)

source(here("R", "01_data_loading.R"))
source(here("R", "05_consistency_metrics.R"))
source(here("R", "07_epa_features.R"))
source(here("R", "08_usage_features.R"))
source(here("R", "09_gamescript_features.R"))
source(here("R", "10_opponent_features.R"))
source(here("R", "11_xgboost_fantasy.R"))

# All seasons we might need across all test windows
all_seasons <- 2015:2025

# Build multi-season feature matrix (one season at a time, then bind)
message("Building multi-season feature matrix...")
features_all <- map_dfr(all_seasons, function(s) {
  message(glue("  Season {s}..."))
  pbp_s     <- load_and_validate_pbp(seasons = s)
  opp_adj_s <- calculate_opponent_adjustments(pbp_s, season = s)
  def_sty_s <- classify_defensive_style(pbp_s, season = s)
  compile_feature_matrix(pbp_s, opp_adj_s, def_sty_s, season = s)
})

message(glue("Feature matrix complete: {nrow(features_all)} rows, ",
             "{n_distinct(features_all$season)} seasons"))

# Load 2025 pbp for prepare_model_features (needs pbp for absence reconstruction)
pbp_2025 <- load_and_validate_pbp(seasons = 2025)

# For multi-season, we need pbp for all seasons too
pbp_all <- load_and_validate_pbp(seasons = all_seasons)

# Season window comparison
seasons_to_test <- list(
  "1 season"   = c(2024),
  "3 seasons"  = c(2022, 2023, 2024),
  "5 seasons"  = c(2020, 2021, 2022, 2023, 2024),
  "7 seasons"  = c(2018, 2019, 2020, 2021, 2022, 2023, 2024),
  "10 seasons" = c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)
)

results <- map_dfr(names(seasons_to_test), function(label) {
  message(glue("Training: {label}..."))
  seasons <- seasons_to_test[[label]]
  
  # Include test season (2025) so train_fantasy_model has something to evaluate on
  seasons_with_test <- c(seasons, 2025)
  
  ml  <- prepare_model_features(features_all, pbp_data = pbp_all,
                                seasons = seasons_with_test)
  mdl <- train_fantasy_model(ml, test_season = 2025)
  
  map_dfr(c("passer", "rusher", "receiver"), function(pos) {
    if (is.null(mdl[[pos]])) return(tibble())
    eval <- evaluate_model(mdl[[pos]], ml, pos)
    eval$comparison_table %>%
      filter(model == "XGBoost") %>%
      mutate(training_window = label, n_seasons = length(seasons))
  })
})

# Results table
results %>%
  select(training_window, n_seasons, position, rmse, mae, rsq) %>%
  arrange(position, rmse)