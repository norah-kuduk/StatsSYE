 #' Add Two Number
 #'
 #' This is a function that adds two scalar numbers
 #' together
 #'
 #' @param x is a scalar to be added
 #' @param y is a scalar to be added
 #' @return a scalar for the two numbers added together
 #'
 #' @examples
 #' x_num <- 4
 #' y_num <- 9
 #' add_nums(x_num, y_num)
 #' @import dplyr
 #' @import tibble
 #' @export

add_nums <- function(x,y) {
  tibble(variables = c(x,y)) |>
    summarise(sum_xy = sum(x,y)) |>
    pull(sum_xy)

  return(sum_xy)
}
