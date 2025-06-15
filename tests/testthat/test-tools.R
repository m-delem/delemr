test_that("stop_quietly stops quietly", {
  expect_error(stop_quietly())
})

test_that("glue_col_lit works as expected", {
  expect_equal(glue_col_lit("Hello'World"), "Hello'World")
  expect_equal(glue_col_lit("Value's: {1 + 1}"), "Value's: 2")
})

test_that("quote_vector quotes elements correctly", {
  expect_equal(quote_vector(c(a, b, c)),       c("a", "b", "c"))
  expect_equal(quote_vector(c("a", b, "c")),   c("a", "b", "c"))
  expect_equal(quote_vector(c(a, "b", c)),     c("a", "b", "c"))
  expect_equal(quote_vector(c("a", "b", "c")), c("a", "b", "c"))
})

test_that("ignore_unused_imports works as expected", {
  expect_null(ignore_unused_imports())
})
