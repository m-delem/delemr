#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom rlang .data
## usethis namespace: end
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(glue::glue_col(
    "{blue
    Welcome to {cyan delemr}.
    }
    ",
    .literal = TRUE))
}
# The functions here only have in common that they have been useful to me
# at some point, but they are documented, if you're curious about them.

# To avoid check NOTE on package sub-dependencies not called
ignore_unused_imports <- function() {
  crayon::underline # for glue_col
  return(NULL)
}
