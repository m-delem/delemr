#' Create a hex sticker for a package.
#'
#' @description
#' This is the code I used to create the sticker for the `delemr` package.
#'
#' I created this function to remember some cool tricks I found on the internet
#' to create the sticker of this package from an image while bypassing the
#' limitations of the `hexSticker` package. This is mostly the use of the
#' `cropcircles` package to crop the image, the `showtext` package to use a
#' custom font, and the creation of a precise ggplot object to have way more
#' control over the final sticker.
#'
#' The function is not flexible enough to be exported, although I allowed to
#' specify a package name. In that setting, putting the raw image in the
#' `man/figures` folder and name it `pkg_name_raw_img.png` should suffice to run
#' the function. However, the ggplot call will a "delemr" text on the sticker,
#' so this code still has to be tweaked to avoid that and become truly flexible.
#'
#' @param pkg_name A string with the package name.
#'
#' @return Nothing, but creates a sticker in the `man/figures` folder and opens
#' it in the viewer.
create_sticker <- function(pkg_name = "delemr"){
  path <- paste0("man/figures/", pkg_name)
  raw_image <- paste0(path, "_raw_img.png")
  stick_raw <- paste0(path, "_sticker_raw.png")
  stick_final <- paste0(path, "_sticker.png")
  logo_path <- "man/figures/logo.png"
  font <- "Poiret One"

  raw <- system.file(raw_image, package = pkg_name)

  raw_cropped <- cropcircles::hex_crop(
    images = raw,
    border_colour = "#394049",
    border_size = 7
  )

  sysfonts::font_add_google(font)
  showtext::showtext_auto()

  sticker_plot <-
    ggplot2::ggplot() +
    ggpath::geom_from_path(ggplot2::aes(.5, .5, path = raw_cropped)) +
    ggplot2::annotate(
      "text",
      x = .27,
      y = .22,
      label = pkg_name,
      color = "white",
      family = font,
      size = 54,
      angle = -30
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::theme_void()

  ggplot2::ggsave(
    filename = stick_raw,
    plot = sticker_plot,
    width = 6,
    height = 6,
    dpi = 300
    )

  sticker_raw <- system.file(stick_raw, package = pkg_name)

  hexSticker::sticker(
    sticker_raw,
    package = pkg_name,
    s_x = 1,
    s_y = 1,
    s_width = 1,
    s_height = 1,
    h_size = 0,
    p_size = 0,
    filename = stick_final
  )

  use_logo(stick_final)

  file.show(logo_path)
}
