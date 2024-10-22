#' Sum non-null consecutive values in a vector
#' @description
#' Function to sum consecutive non-null numeric values in a vector containing zeros. Multiple successive numbers are all replaced with their sum (see examples). Duplicates can be removed by setting the `unique = TRUE`.
#'
#' @param x A vector.
#' @param unique Logical. If `TRUE`, the function will return only unique values.
#'
#' @return A vector where consecutive non-null numeric values have been summed.
#' @export
#'
#' @examples
#' sum_consecutive_values(c(1, 2, 0, 0, 3, 4, 0, 0, 0, 5, 6))
#' sum_consecutive_values(c(1, 2, 0, 0, 3, 4, 0, 0, 0, 5, 6), unique = TRUE)
sum_consecutive_values <- function(x, unique = FALSE){

  summed_vector <-
    data.frame(
      x = x,
      group = cumsum(x == 0) # creates a group for each zero
    ) |>
    mutate(
      group = as.factor(.data$group),
      group = if_else(x == 0, "no", .data$group)) |> # replace zeroes to isolate them
    group_by(.data$group) |>
    mutate(summed = max(cumsum(.data$x))) |> # sums and keeps the biggest value
    pull(.data$summed) # extract the vector

  if(unique){
    summed_vector <- unique(summed_vector)
  }

  return(summed_vector)
}
