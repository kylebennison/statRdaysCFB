test_that("Test that get returns a dataframe object with data", {
  expect_s3_class(get_plays(), "data.frame", exact = FALSE)
  expect_gt(nrow(get_plays()), 0L)
})
