#' Smooth Speed Data
#'
#' @param data A data frame containing at least `year` and `speed` columns.
#' @param group_size An integer specifying the size of groups for smoothing (default: 16).
#' @return A data frame with an added `speed_smooth` column containing smoothed speed values.
#'   The temporary grouping variable is removed.
#' @import dplyr
#' @export
smooth_speed <- function(data, group_size = 16) {
  data |>
    dplyr::group_by(year) |>
    # Create groups based on the specified group size
    dplyr::mutate(group = ceiling(dplyr::row_number() / group_size)) |>
    # Calculate the mean speed for each group and assign to `speed_smooth`
    dplyr::group_by(group, .add = TRUE) |>
    dplyr::mutate(speed_smooth = round(mean(speed, na.rm = TRUE))) |>
    dplyr::ungroup() |>
    # Remove the temporary grouping variable
    dplyr::select(-group)
}
