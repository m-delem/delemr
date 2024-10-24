#' Define contrasts inside a pipeline
#'
#' Short wrapper around the [contrasts<-] function to define
#' contrasts inside a pipeline easily. The function takes a dataframe as input
#' and returns a dataframe with updated contrasts, allowing to pipe it, which
#' differs from the usual use of the `contrasts<-` function.
#'
#' @param df A dataframe.
#' @param cols A vector of column names (quoted or not), e.g., `c(group, condition)`.
#' @param contrasts A list of contrasts, each specified either:
#'   *  as a vector, e.g., `c(-1, 1)` for a two-level factor
#'   *  as a matrix, e.g., `contr.sum(3)` for a three-level factor
#' @returns A dataframe with the contrasts defined for the supplied columns.
#' @seealso [contrasts<-] which this function wraps.
#' @examples
#' df <- data.frame(
#'   group = factor(rep(c("A", "B"), 3)),
#'   condition = factor(rep(c("X", "Y", "Z"), each = 2))
#' )
#'
#' # The usual way
#' contrasts(df$group) <- contr.sum(2)
#' contrasts(df$condition) <- contr.sum(3)
#'
#' # Result
#' contrasts(df$group)
#' contrasts(df$condition)
#'
#' # the pipeline way
#' df <- df |>
#'   define_contrasts(
#'     cols = c(group, condition),
#'     contrasts = list(contr.sum(2), contr.sum(3))
#'   )
#'
#' # Same result
#' contrasts(df$group)
#' contrasts(df$condition)
#'
#' @aliases def_contr
#' @export
define_contrasts <- function(df, cols, contrasts) {
  # ---- Checks
  if (!is.data.frame(df)) {
    stop(glue_col("{red 'df' must be a dataframe.}"))
  }

  cols <- enexpr(cols)
  cols <- map(cols, quo_name)
  if (length(cols) > 1) cols <- cols[-1]

  for (col in cols) {
    if (!(col %in% names(df))) {
      stop(glue_col("{red 'cols' must be a vector of column names from the dataframe.}"))
    }
  }

  if (!is.list(contrasts)) {
    stop(glue_col("{red 'contrasts' must be a list of vectors or matrices.}"))
  }

  if (length(cols) != length(contrasts)) {
    stop(glue_col("{red 'cols' and 'contrasts' must be the same length.}"))
  }

  for (i in seq_along(contrasts)) {
    if (!is.numeric(contrasts[[i]])) {
      stop(glue_col("{red 'contrasts' must contain only numerical vectors or matrices.}"))
    }

    if (!is.matrix(contrasts[[i]]) && length(contrasts[[i]]) != nlevels(df[[cols[[i]]]])) {
      stop(glue_col("{red The length of the contrast vector in position {i} must match the number of factor levels in the '{cols[[i]]}' column.}"))
    }

    if (is.matrix(contrasts[[i]]) && nrow(contrasts[[i]]) != nlevels(df[[cols[[i]]]])) {
      stop(glue_col("{red The number of rows in the contrast matrix in position {i} must match the number of factor levels in the '{cols[[i]]}' column.}"))
    }
  }

  # ---- End checks

  for (i in seq_along(cols)) contrasts(df[[cols[[i]]]]) <- contrasts[[i]]

  return(df)
}

#' @export
def_contr <- define_contrasts
