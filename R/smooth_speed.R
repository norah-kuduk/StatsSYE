#' Smooth Speed Data
#'
#' This function groups telemetry data into segments and calculates a smoothed speed value.
#'
#' @param data A data frame containing at least a `speed` column.
#' @param grouping A variable containing either `driver` or `year` to group speed by
#' @param frame_size The number of data points per smoothing group (default: 16).
#' @return A data frame with a new `speed_smooth` column.
#' @examples
#' # Assuming `plot_data` contains F1 telemetry data:
#' smoothed_data <- smooth_speed(plot_data, grouping = "driver")
#' @export
smooth_speed <- function(data, grouping, frame_size = 16) {
  plot_data |>
    group_by(driver) |>
    # create groups of 4
    mutate(group = ceiling(row_number() / 16)) |> # why cant i put frame size here
    # calculate the mean speed for each group and assign to speed_smooth
    group_by(group) |>
    mutate(speed_smooth = round(mean(speed, na.rm = TRUE))) |>
    ungroup() |>
    # remove the temporary grouping variable
    select(-group)

}
