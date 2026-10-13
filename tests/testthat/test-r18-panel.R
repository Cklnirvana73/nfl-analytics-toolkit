# Acceptance checks on a rebuilt R/18 panel. Skipped unless the panel exists.
panel_path <- file.path(normalizePath(file.path(testthat::test_path(), "..", "..")),
                        "data", "season2_cache", "s2_week4_ab_panel.rds")

test_that("R/18 panel acceptance checks", {
  skip_if_not(file.exists(panel_path), "panel not built")
  panel <- readRDS(panel_path)
  expect_equal(nrow(panel), nrow(distinct(panel, player_id, season, week)))
  k_name <- panel %>% group_by(season, player_id) %>% summarise(k = n_distinct(player_name), .groups = "drop")
  expect_true(all(k_name$k == 1))
  expect_equal(sum(panel$games_played_prior > panel$week - 1), 0L)
  k_pos <- panel %>% group_by(season, player_id) %>% summarise(k = n_distinct(position), .groups = "drop")
  expect_true(all(k_pos$k == 1))
  expect_true(all(panel$position %in% c("QB", "RB", "WR")))
  expect_true(all(panel$position[panel$player_id == "00-0034857" & panel$season == 2024] == "QB"))
})
