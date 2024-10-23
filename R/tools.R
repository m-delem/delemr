stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

glue_col_lit <- function(...) {
  glue_col(..., .literal = TRUE)
}
