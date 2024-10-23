#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @import rlang
#' @importFrom dplyr mutate if_else group_by pull filter select distinct
#' @importFrom glue glue
#' @importFrom glue glue glue_col
#' @importFrom lifecycle deprecated
#' @importFrom stats contrasts<-
## usethis namespace: end
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(glue_col("{blue Welcome to '{cyan {underline delemr}}'. The functions here only have in common that they have been useful to me at some point, but they should be well documented if you're curious about them.}", .literal = TRUE))
}

# To avoid check NOTE on package sub-dependencies not called
ignore_unused_imports <- function() {
  crayon::underline # for glue_col
}
