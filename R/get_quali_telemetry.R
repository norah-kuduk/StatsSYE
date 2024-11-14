#' Get Qualifying Telemetry Data for Multiple Laps Safely
#'
#' @param lap A numeric vector specifying the lap numbers to load telemetry data for. Default is 1 to 50.
#' @param season The racing season (e.g., 2022).
#' @param round The round of the race (e.g., 10).
#' @param driver The driver's code (e.g., "VER").
#'
#' @return A data frame containing telemetry data for the specified laps with an added `lap_number` column.
#'         If a lap is not found, the mapping stops and returns the collected data up to that point.
#' @import f1dataR
#' @import purrr
#' @import dplyr
#' @export
#'
#' @examples
#' # Example usage
#' get_quali_telemetry(season = 2022, round = 10, driver = "VER")
get_quali_telemetry <- function(lap = 1:50, season, round, driver) {
  # safely wrap the telemetry loading function
  safe_load_telemetry <- safely(function(current_lap) {
    load_driver_telemetry(
      season = season,
      round = round,
      driver = driver,
      session = "Q",
      laps = current_lap
    ) |>
      mutate(lap_number = current_lap)  # Add the lap number as a new column
  })

  # iterate over laps and collect data
  result <- map(lap, function(current_lap) {
    # Try to get telemetry for the current lap
    res <- safe_load_telemetry(current_lap)

    # check if an error occurred
    if (!is.null(res$error)) {
      return(NULL)  # Stop processing if an error is encountered
    }

    # return the successful result
    return(res$result)
  })

  # combine all successful results into a single data frame
  telemetry_data <- bind_rows(result)

  return(telemetry_data)
}
