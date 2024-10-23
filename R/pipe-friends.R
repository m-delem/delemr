#' Define contrasts inside a pipeline
#'
#' Short wrapper around the [stats::contrasts<-] function to define
#' contrasts inside a pipeline easily. The function takes a dataframe as input
#' and returns a dataframe with updated contrasts, allowing to pipe it, which
#' differs from the usual use of the `contrasts<-` function.
#'
#' @param df A dataframe.
#' @param cols A vector of column names, e.g., `c(group, condition)`.
#' @param contrasts A list of contrasts, each specified either:
#'   *  as a vector, e.g., `c(-1, 1)` for a two-level factor
#'   *  as a matrix, e.g., `contr.sum(3)` for a three-level factor
#' @returns A dataframe with the contrasts defined for the supplied columns.
#' @seealso [stats::contrasts<-] which this function wraps.
#' @examples
#' df <- data.frame(
#'   group = factor(rep(c("A", "B"), 3)),
#'   condition = factor(rep(c("X", "Y", "Z"), each = 2))
#'   )
#'
#' # The usual way
#' contrasts(df$group) <- contr.sum(2)
#' contrasts(df$condition) <- contr.sum(3)
#'
#' contrasts(df$group)
#' contrasts(df$condition)
#'
#' # the pipeline way
#' df <- df |>
#'   define_contrasts(
#'     c(group, condition),
#'     c(contr.sum(2), contr.sum(3)
#'     )
#'
#' contrasts(df$group)
#' contrasts(df$condition)
#'
#' @aliases def_contr
#' @export
define_contrasts <- function(df, cols, contrasts) {
  for (col in cols) {

  }
  cols <- enquo(cols)
  print(cols)
  # for (i in cols) print(quo_name(i))
  stop()

  # ---- Checks
  if (!is.data.frame(df)) {
    stop(glue_col("{red First argument 'df' must be a dataframe.}", .literal = TRUE))
  }

  for (col in cols) {
    if (!(as_name(expr(col)) %in% names(df))) {
      stop(glue_col("{red Second argument 'cols' must contain column names from the dataframe.}", .literal = TRUE))
    }
  }

  for (i in seq_along(contrasts)) {
    if (!is.vector(contrasts[[i]]) && !is.matrix(contrasts[[i]])) {
      stop(glue_col("{red Third argument 'contrasts' must be a list of vectors or matrices.}", .literal = TRUE))
    }

    if (is.vector(contrasts[[i]]) && length(contrasts[[i]]) != nlevels(df[[ensym(cols[i])]])) {
      stop(glue_col("{red The length of the contrast vector in position {i} must match the number of factor levels in the column '{ensym(cols[i])}'.}", .literal = TRUE))
    }

    if (is.matrix(contrasts[[i]]) && nrow(contrasts[[i]]) != nlevels(df[[ensym(cols[i])]])) {
      stop(glue_col("{red The number of rows in the contrast matrix in position {i} must match the number of factor levels in the column '{ensym(cols[i])}'.}", .literal = TRUE))
    }
  }
  # ---- End checks

  for (i in seq_along(cols)) contrasts(df[[ensym(cols[i])]]) <- contrasts[[i]]

  return(df)
}

#' @export
def_contr <- define_contrasts


