#' process and bind multiple qualifying datasets
#'
#' @param ... A list of data frames, each representing a qualifying lap
#'   Each data frame should contain a `lap_number` column and its corresponding driver
#' @param drivers A numeric vector of driver corresponding to each dataset in `...`.
#' @return A single data frame with `lap_number` as a factor, a `driver` column, and all laps combined.
#' @import dplyr
#' @export
bind_data_driver <- function(..., drivers) {
  laps <- list(...)

  # Check if the number of laps matches the number of years
  if (length(laps) != length(drivers)) {
    stop("The number of datasets must match the number of years.")
  }

  # Process each dataset
  processed_laps <- lapply(seq_along(laps), function(i) {
    laps[[i]] |>
      dplyr::mutate(
        lap_number = as.factor(lap_number),
        driver = driver[i]
      )
  })

  # Bind the datasets into one data frame and ensure year is a factor
  bind_rows(processed_laps) |>
    dplyr::mutate(driver = as.factor(driver))
}
