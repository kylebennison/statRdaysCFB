test_that("Test that get returns a dataframe object with data", {
  expect_type(get_plays(), "data.frame")
  expect_gt(nrow(get_plays()), 0L)
})
