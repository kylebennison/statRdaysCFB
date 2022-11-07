test_that("get_plays() works", {
  expect_s3_class(get_plays(), "data.frame", exact = FALSE)
  expect_gt(nrow(get_plays()), 0L)
})

test_that("get_colors() function works", {
  expect_s3_class(get_colors(), "data.frame", exact = FALSE)
})

test_that("get_games() works", {
  expect_s3_class(get_games(), "data.frame", exact = FALSE)
  expect_gt(nrow(get_games()), 0L)
})

test_that("get_anything() works", {
  expect_s3_class(
    get_anything(url = "https://api.collegefootballdata.com/venues"),
    "data.frame",
    exact = FALSE
  )
  expect_gt(nrow(
    get_anything(url = "https://api.collegefootballdata.com/venues")
  ), 0L)
})

test_that("get_betting() works", {
  expect_s3_class(get_betting(2021, 2021, 1, 1), "data.frame", exact = FALSE)
  expect_gt(nrow(get_betting(2021, 2021, 1, 1)), 0L)
  expect_gt(nrow(get_betting(2021, 2021, 1, 1, season_type = "postseason")), 0L)
  expect_gt(nrow(get_betting(2021, 2021, 1, 1, season_type = "regular")), 0L)
  expect_vector(get_betting(2021,2021, 1, 1)$homeScore)
  expect_vector(get_betting(2021,2021, 1, 1)$awayScore)
})

test_that("get_drives() works", {
  expect_s3_class(get_drives(), "data.frame", exact = FALSE)
  expect_gt(nrow(get_drives()), 0L)
})

test_that("get_elo() works", {
  expect_s3_class(get_elo(), "data.frame", exact = FALSE)
  expect_gt(nrow(get_elo()), 0L)
})
