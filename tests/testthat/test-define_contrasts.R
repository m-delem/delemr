test_that("contrasts are defined correctly", {
  df <-
    data.frame(
      group = factor(rep(c("A", "B"), each = 3)),
      condition = factor(rep(c("X", "Y", "Z"), each = 2))
    )

  df2 <-
    df |>
    define_contrasts(
      cols = c(group, condition),
      contrasts = list(contr.sum(2), contr.sum(3))
    )

  contrasts(df$group) <- contr.sum(2)
  contrasts(df$condition) <- contr.sum(3)

  expect_equal(contrasts(df$group), contrasts(df2$group))
  expect_equal(contrasts(df$condition), contrasts(df2$condition))
})

test_that("relevant errors are thrown when expected", {
  df <-
    data.frame(
      group = factor(rep(c("A", "B"), each = 3)),
      condition = factor(rep(c("X", "Y", "Z"), each = 2))
      )

  expect_error(define_contrasts(df = c(1)), "must be a dataframe")
  expect_error(define_contrasts(df, cols = y), "must be a vector of column names")
  expect_error(
    define_contrasts(df, col = c(group, condition), contrasts = c(-1, 1)),
    "must be a list"
  )
  expect_error(
    define_contrasts(df, col = c(group, condition), contrasts = list("a", c(1))),
    "must contain only numerical vectors or matrices"
  )
  expect_error(
    define_contrasts(
      df,
      col = c(group, condition),
      contrasts = list(c(-1), contr.sum(3))
    ),
    "must match the number of factor levels"
  )
  expect_error(
    define_contrasts(
      df,
      col = c(group, condition),
      contrasts = list(c(-1, 1), contr.sum(2))
    ),
    "must match the number of factor levels"
  )
})
