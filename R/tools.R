#' Stop in the middle of a function without returning an error
#'
#' @export
stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

#' Use glue syntax with literal interpretation
#' @param ... Character vectors to be concatenated
#' @export
glue_col_lit <- function(...) {
  glue::glue_col(..., .literal = TRUE)
}

#' Add quotes to a vector with quoted or unquoted elements
#'
#' @param vect A vector of elements that can be quoted or unquoted, e.g.,
#' `c(a, b, c)`, `c("a", b, "c")`, `c(a, "b", c)`, etc.
#'
#' @returns A vector with all the elements quoted, i.e., turned into strings.
#' @export
quote_vector <- function(vect) {
  vect <- rlang::enexpr(vect)
  quo_vect <- purrr::map(as.list(vect), rlang::quo_name)[-1] |> unlist()
  return(quo_vect)
}
