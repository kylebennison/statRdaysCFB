test_that("Test that get_plays() returns a dataframe object with data", {
  expect_s3_class(get_plays(), "data.frame", exact = FALSE)
  expect_gt(nrow(get_plays()), 0L)
})

test_that("Test that colors function works", {
  expect_s3_class(get_colors(), "data.frame", exact = FALSE)
})

test_that("Test that get_games() returns a dataframe object with data", {
  expect_s3_class(get_games(), "data.frame", exact = FALSE)
  expect_gt(nrow(get_games()), 0L)
})

test_that("Test that get_anything() returns a dataframe object with data", {
  expect_s3_class(
    get_anything(url = "https://api.collegefootballdata.com/venues"),
    "data.frame",
    exact = FALSE
  )
  expect_gt(nrow(
    get_anything(url = "https://api.collegefootballdata.com/venues")
  ), 0L)
})

test_that("Test that get_betting() returns a dataframe object with data", {
  expect_s3_class(get_betting(), "data.frame", exact = FALSE)
  expect_gt(nrow(get_betting()), 0L)
})

test_that("Test that get_drives() returns a dataframe object with data", {
  expect_s3_class(get_drives(), "data.frame", exact = FALSE)
  expect_gt(nrow(get_drives()), 0L)
})

test_that("Test that get_elo() returns a dataframe object with data", {
  expect_s3_class(get_elo(), "data.frame", exact = FALSE)
  expect_gt(nrow(get_elo()), 0L)
})
