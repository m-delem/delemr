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
  glue_col(..., .literal = TRUE)
}
