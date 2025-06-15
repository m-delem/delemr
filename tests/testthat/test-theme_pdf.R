test_that("theme_pdf works properly", {
  p <-
    iris |>
    dplyr::mutate(Species = stringr::str_to_title(Species)) |>
    ggplot2::ggplot(ggplot2::aes(
      x = Sepal.Length,
      y = Sepal.Width,
      color = Species,
      fill  = Species,
      size = Petal.Width
    )) +
    ggplot2::geom_jitter(alpha = 0.5) +
    ggplot2::labs(
      title    = "Iris Dataset",
      subtitle = "A classic dataset for testing",
      caption  = "Source: R's built-in iris dataset",
      x = "Sepal Length (cm)",
      y = "Sepal Width (cm)"
    ) +
    ggplot2::scale_color_manual(
      values = palette.colors(palette = "Okabe-Ito")[c(1, 2, 3)]
    ) +
    ggplot2::scale_size_continuous(range = c(1, 2.5))

  p1 <- p + theme_pdf()

  p2 <- p + theme_pdf(ggplot2::theme_classic, family = "Roboto Slab")

  expect_equal(class(p), class(p1))
  expect_equal(class(p), class(p2))
})
