#' process and bind multiple qualifying datasets
#'
#' @param ... A list of data frames, each representing a qualifying lap
#'   Each data frame should contain a `lap_number` column and its corresponding year.
#' @param years A numeric vector of years corresponding to each dataset in `...`.
#' @return A single data frame with `lap_number` as a factor, a `year` column, and all laps combined.
#' @import dplyr
#' @export
bind_data_year <- function(..., years) {
  laps <- list(...)

  print(length(laps))
  # Check if the number of laps matches the number of years
  if (length(laps) != length(years)) {
    stop("The number of datasets must match the number of years.")
  }

  # Process each dataset
  processed_laps <- lapply(seq_along(laps), function(i) {
    laps[[i]] |>
      dplyr::mutate(
        lap_number = as.factor(lap_number),
        year = years[i]
      )
  })

  # Bind the datasets into one data frame and ensure year is a factor
  bind_rows(processed_laps) |>
    dplyr::mutate(year = as.factor(year))
}
