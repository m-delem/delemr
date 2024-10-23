#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @import rlang
#' @importFrom dplyr mutate if_else group_by pull filter select distinct
#' @importFrom crayon blue cyan underline red silver italic %+%
#' @importFrom glue glue
#' @importFrom lifecycle deprecated
#' @importFrom stats contrasts<-
## usethis namespace: end
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(blue("Welcome to " %+% cyan$underline("delemr") %+% ". The functions here only have in common that they have been useful to me at some point, but they should be well documented if you're curious about them."))
}
