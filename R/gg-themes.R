#' Theme modifiers for ggplot2
#'
#' These functions modify specific theme elements of a plot. Be wary that,
#' contrary the usual "theme" functions (e.g., [theme_bw()]), these do not
#' "reset" all current elements, they only modify some of them.
#'
#' @details
#' \describe{
#'
#' \item{`theme_transparent()`}{
#' Sets the background of the plot, panel, and legend to transparent.}
#'
#' \item{`theme_element_colour()`}{
#' Sets the colour of all small line and text elements to a specified colour.}
#' }
#'
#' @seealso [theme()], [theme_bw()]
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#'
#' p + theme_transparent()
#' p + theme_element_colour("red")
#'
#' @export
#' @rdname ggthemes
theme_transparent <- function() {
  theme(
    panel.background =      element_rect(fill = "transparent"),
    plot.background =       element_rect(fill = "transparent", color = NA),
    legend.background =     element_rect(fill = 'transparent'),
    legend.box.background = element_rect(fill = 'transparent', color = NA)
  )
}

#' @param colour The colour to set all small line and text elements to.
#' @export
#' @rdname ggthemes
theme_element_colour <- function(colour = "black") {
  theme(
    axis.text          = element_text(color = colour),
    axis.title         = element_text(color = colour),
    axis.ticks         = element_line(color = colour),
    axis.line          = element_line(color = colour),
    legend.text        = element_text(color = colour),
    legend.title       = element_text(color = colour),
    panel.grid.major.x = element_line(color = colour),
    panel.grid.major.y = element_line(color = colour),
    panel.grid.minor.x = element_line(color = colour),
    panel.grid.minor.y = element_line(color = colour),
    panel.background   = element_rect(color = colour)
  )
}
