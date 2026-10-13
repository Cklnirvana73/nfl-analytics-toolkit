test_that("R/17: one row per season, week, game_id, player_id", {
  fp <- quiet(calculate_fantasy_points_ext(fixture_pbp()))
  expect_equal(nrow(fp), nrow(distinct(fp, across(all_of(KEY)))))
  fr <- quiet(calculate_fantasy_points_ext(fixture_pbp(), roster_data = fixture_roster()))
  expect_equal(nrow(fr), nrow(distinct(fr, across(all_of(KEY)))))
})

test_that("R/17: one player_name per player_id per season", {
  fp <- quiet(calculate_fantasy_points_ext(perturb_names(fixture_pbp())))
  k <- fp %>% group_by(season, player_id) %>% summarise(k = n_distinct(player_name), .groups = "drop")
  expect_true(all(k$k == 1))
})

test_that("R/17 conservation: name spelling variants change no counting stat", {
  args <- list(two_point_conversion = 2, pass_2pt = 1)  # exercise the 2PC path
  base <- quiet(do.call(calculate_fantasy_points_ext, c(list(fixture_pbp()), args))) %>% arrange(across(all_of(KEY)))
  pert <- quiet(do.call(calculate_fantasy_points_ext, c(list(perturb_names(fixture_pbp())), args))) %>% arrange(across(all_of(KEY)))
  expect_equal(nrow(pert), nrow(base))
  expect_identical(pert[, KEY], base[, KEY])
  for (col in intersect(COUNT_COLS_17, names(base))) expect_identical(pert[[col]], base[[col]], info = col)
  expect_equal(pert$total_fantasy_points, base$total_fantasy_points)
})

test_that("R/17: De.Williams 2013 weeks 5, 7, 12 are one row each", {
  fp <- quiet(calculate_fantasy_points_ext(fixture_pbp()))
  dw <- fp %>% filter(player_id == "00-0024242")
  expect_equal(sort(dw$week), c(5L, 7L, 12L))
})
