#' Sum non-null consecutive values in a vector
#'
#' Function to sum  non-null consecutive numeric values in a vector containing
#' zeros. Multiple successive numbers are all replaced with their sum and all
#' zeros are removed.
#'
#' @param x A vector.
#' @returns A vector where consecutive non-null numeric values have been summed.
#' @examples
#' sum_consecutive(c(1, 2, 0, 0, 3, 4, 0, 0, 0, 4, 3))
#' sum_consecutive(c(1, 2, 0, 12, 1, 3, 0, 0, 8, 0, 3))
#' @export
sum_consecutive <- function(x) {
  summed_vector <-
    data.frame(
      x = x,
      group = cumsum(x == 0) # creates a group for each zero
    ) |>
    dplyr::mutate(
      group = as.factor(.data$group),
      group = ifelse(x == 0, "no", .data$group)
    ) |> # replace zeros to isolate them
    dplyr::group_by(.data$group) |>
    dplyr::mutate(summed = max(cumsum(.data$x))) |> # sums and keeps the biggest
    dplyr::select(!x) |>
    dplyr::distinct() |> # removes duplicates from the same group (!= all dupes)
    dplyr::filter(.data$group != "no") |> # removes zeros
    dplyr::pull(.data$summed) # extract the vector

  return(summed_vector)
}
