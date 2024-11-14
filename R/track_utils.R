#' Adjust track plot aspect ratio
#'
#' @param track_plot A ggplot2 object representing a track plot.
#' @param track_coords A data frame with columns x and y containing track coordinates.
#' @return A ggplot2 plot object with corrected aspect ratio.
#' @import ggplot2
#' @export
corrected_track <- function(track_plot, track_coords) {
  xrange <- range(track_coords$x, na.rm = TRUE) + c(-500, 500)
  yrange <- range(track_coords$y, na.rm = TRUE) + c(-500, 500)
  maxdiff <- max(abs(xrange[2] - xrange[1]), abs(yrange[2] - yrange[1]), na.rm = TRUE)

  xmid <- mean(xrange)
  ymid <- mean(yrange)

  newxlim <- c(xmid - 0.5 * maxdiff, xmid + 0.5 * maxdiff)
  newylim <- c(ymid - 0.5 * maxdiff, ymid + 0.5 * maxdiff)

  track_plot <- track_plot + ggplot2::coord_fixed(xlim = newxlim, ylim = newylim)

  return(track_plot)
}

#' Generate a smooth track from lap details
#'
#' @param lap_details A data frame containing x and y coordinates of lap details. It should not include a 'time' column.
#' @return A data frame with smoothed track coordinates.
#' @import dplyr
#' @export
smooth_track <- function(lap_details) {
  track <- lap_details |>
    dplyr::select(-time) |>
    dplyr::mutate(x2 = lap_details$x, y2 = lap_details$y) |>
    dplyr::select(-x, -y) |>
    dplyr::select(x2, y2)

  track <- dplyr::bind_rows(track, track |> dplyr::slice(1))

  return(track)
}
