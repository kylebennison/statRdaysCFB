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
  df <- get_anything(url = "https://api.collegefootballdata.com/venues")
  expect_s3_class(
    df,
    "data.frame",
    exact = FALSE
  )
  expect_gt(nrow(
    df
  ), 0L)
  expect_true("year" %in% colnames(df))
})

test_that("get_betting() works", {
  bet_df <- get_betting(2021, 2021, 1, 1)
  expect_s3_class(bet_df, "data.frame", exact = FALSE)
  expect_gt(nrow(bet_df), 0L)
  expect_gt(nrow(get_betting(2021, 2021, 1, 1, season_type = "postseason")), 0L)
  expect_gt(nrow(get_betting(2021, 2021, 1, 1, season_type = "regular")), 0L)
  expect_vector(bet_df$homeScore)
  expect_vector(bet_df$awayScore)
})

test_that("get_drives() works", {
  expect_s3_class(get_drives(), "data.frame", exact = FALSE)
  expect_gt(nrow(get_drives()), 0L)
})

test_that("get_elo() works", {
  expect_s3_class(get_elo(), "data.frame", exact = FALSE)
  expect_gt(nrow(get_elo()), 0L)
})
