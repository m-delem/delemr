test_that("contrasts are defined correctly", {
  df1 <- data.frame(x = factor(rep(c("a", "b", "c"), 3)))

  df2 <- df1 |> define_contrasts(col = x, contr.sum(3))

  contrasts(df1$x) <- contr.sum(3)

  expect_equal(contrasts(df1$x), contrasts(df2$x))
  expect_setequal(contrasts(df2$x),contr.sum(3))

  # alias works properly
  expect_equal(
    df1 |> define_contrasts(col = x, contr.sum(3)),
    df1 |> def_contr(col = x, contr.sum(3))
  )
})

test_that("relevant errors are thrown when expected", {
  df1 <- data.frame(x = factor(rep(c("a", "b", "c"), 3)))

  expect_error(define_contrasts(df = c(1)), "First argument")
  expect_error(define_contrasts(df1, col = y), "Second argument")
  expect_error(define_contrasts(df1, col = x, c(-1, 1)), "contrast vector")
  expect_error(define_contrasts(df1, col = x, contr.sum(2)), "contrast matrix")
})

