test_that("R/05: one row per season, week, game_id, player_id (no roster)", {
  fp <- quiet(calculate_fantasy_points(fixture_pbp()))
  expect_equal(nrow(fp), nrow(distinct(fp, across(all_of(KEY)))))
})

test_that("R/05: one row per player-game with roster", {
  fp <- quiet(calculate_fantasy_points(fixture_pbp(), roster_data = fixture_roster()))
  expect_equal(nrow(fp), nrow(distinct(fp, across(all_of(KEY)))))
})

test_that("R/05: one player_name per player_id per season", {
  fp <- quiet(calculate_fantasy_points(perturb_names(fixture_pbp())))
  n_names <- fp %>% group_by(season, player_id) %>% summarise(k = n_distinct(player_name), .groups = "drop")
  expect_true(all(n_names$k == 1))
})

test_that("R/05 conservation: name spelling variants change no counting stat or points", {
  base <- quiet(calculate_fantasy_points(fixture_pbp(), roster_data = fixture_roster())) %>% arrange(across(all_of(KEY)))
  pert <- quiet(calculate_fantasy_points(perturb_names(fixture_pbp()), roster_data = fixture_roster())) %>% arrange(across(all_of(KEY)))
  expect_equal(nrow(pert), nrow(base))
  expect_identical(pert[, KEY], base[, KEY])
  for (col in COUNT_COLS_05) expect_identical(pert[[col]], base[[col]], info = col)
  expect_equal(pert$total_fantasy_points, base$total_fantasy_points)
  expect_identical(pert$position, base$position)
})

test_that("R/05: De.Williams 2013 weeks 5, 7, 12 are one row each", {
  fp <- quiet(calculate_fantasy_points(fixture_pbp(), roster_data = fixture_roster()))
  dw <- fp %>% filter(player_id == "00-0024242")
  expect_equal(sort(dw$week), c(5L, 7L, 12L))
  expect_equal(unique(dw$player_name), "De.Williams")
})

test_that("R/05: roster positions (Allen QB, B.Robinson single, Hooper/Bowers TE)", {
  fp <- quiet(calculate_fantasy_points(fixture_pbp(), roster_data = fixture_roster()))
  expect_equal(unique(fp$position[fp$player_id == "00-0034857"]), "QB")
  expect_length(unique(fp$position[fp$player_id == "00-0037746" & fp$season == 2024]), 1)
  expect_true(all(fp$position[fp$player_id %in% c("00-0032392", "00-0039338")] == "TE"))
  per_season <- fp %>% group_by(season, player_id) %>% summarise(k = n_distinct(position), .groups = "drop")
  expect_true(all(per_season$k == 1))
})

test_that("R/05: players missing from roster are UNK, never an inferred role", {
  ros <- fixture_roster() %>% filter(gsis_id != "00-0034857")
  fp  <- quiet(calculate_fantasy_points(fixture_pbp(), roster_data = ros))
  expect_equal(unique(fp$position[fp$player_id == "00-0034857"]), "UNK")
  expect_false(any(is.na(fp$position)))
})

test_that("R/05: roster lookup is season-keyed", {
  ros <- bind_rows(
    fixture_roster(),
    tibble::tibble(season = 2023L, gsis_id = "00-0039338", position = "WR", full_name = "x")
  ) %>% arrange(season)   # 2023 row first: a season-blind distinct() would keep WR
  fp <- quiet(calculate_fantasy_points(fixture_pbp(), roster_data = ros))
  expect_equal(unique(fp$position[fp$player_id == "00-0039338"]), "TE")
})

test_that("R/05: display name is most frequent spelling, ties alphabetical", {
  pbp <- fixture_pbp()
  rows <- which(pbp$rusher_player_id == "00-0024242")
  # make exactly half the De.Williams rush credits "Aa.Williams": counts become
  # a close contest across all roles; recompute expected from the rule itself
  pbp$rusher_player_name[rows[seq(1, length(rows), 2)]] <- "Aa.Williams"
  expected <- bind_rows(
    tibble::tibble(nm = pbp$passer_player_name[pbp$passer_player_id %in% "00-0024242"]),
    tibble::tibble(nm = pbp$rusher_player_name[pbp$rusher_player_id %in% "00-0024242"]),
    tibble::tibble(nm = pbp$receiver_player_name[pbp$receiver_player_id %in% "00-0024242"])
  )
  # restrict to scored plays the way the function does
  filt <- pbp %>% filter(play_type %in% c("pass", "run"),
                         two_point_attempt == 0 | is.na(two_point_attempt))
  tab <- c(filt$rusher_player_name[filt$rusher_player_id %in% "00-0024242"],
           filt$receiver_player_name[filt$receiver_player_id %in% "00-0024242"],
           filt$passer_player_name[filt$passer_player_id %in% "00-0024242"])
  cnt <- sort(table(tab), decreasing = TRUE)
  top <- sort(names(cnt)[cnt == max(cnt)])[1]
  fp <- quiet(calculate_fantasy_points(pbp))
  expect_equal(unique(fp$player_name[fp$player_id == "00-0024242"]), top)

  # Pure tie check on the helper
  tie <- tibble::tibble(season = 1L,
    passer_player_id = NA_character_, passer_player_name = NA_character_,
    rusher_player_id = c("p1", "p1"), rusher_player_name = c("Zed", "Abe"),
    receiver_player_id = NA_character_, receiver_player_name = NA_character_)
  expect_equal(.resolve_display_names(tie)$player_name, "Abe")
})
