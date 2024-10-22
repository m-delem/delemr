#' Define contrasts inside a pipeline
#' @description
#' Short wrapper around the `contrasts` function to define contrasts inside a pipeline easily. The function takes a dataframe as input and returns a dataframe with updated contrasts, allowing to pipe it, which differs from the usual use of the `contrasts` function.
#'
#'
#' @param df A dataframe.
#' @param col A column name.
#' @param contrast A matrix of contrasts.
#'
#' @return A dataframe with the contrasts defined.
#' @export
#'
#' @examples
#' df <- data.frame(group = factor(rep(c("A", "B", "C"), 3)))
#'
#' # The usual way
#' contrasts(df$group) <- contr.sum(3)
#'
#' # the pipeline way
#' df <- df |> define_contrasts(group, contr.sum(3))
#'
#' contrasts(df$group)
define_contrasts <- function(df, col, contrast) {
  contrasts(df[[ensym(col)]]) <- contrast
  return(df)
}
