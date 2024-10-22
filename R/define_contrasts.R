define_contrasts <- function(df, col, contrast) {
  contrasts(df[[col]]) <- contrast
  return(df)
}
