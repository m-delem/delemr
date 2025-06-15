test_that("sum_consecutive works properly", {
  expect_equal(
    sum_consecutive(c(1, 2, 0, 0, 3, 4, 0, 0, 0, 4, 3)),
    c(3, 7, 7)
  )
  expect_equal(
    sum_consecutive(c(1, 2, 0, 12, 1, 3, 0, 0, 8, 0, 3)),
    c(3, 16, 8, 3)
  )
  # Test with all zeros
  expect_equal(sum_consecutive(c(0, 0, 0)), numeric(0))
  # Test with no zeros
  expect_equal(sum_consecutive(c(1, 2, 3)), c(1 + 2 + 3))
})
