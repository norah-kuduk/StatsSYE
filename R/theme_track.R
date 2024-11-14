#' Custom ggplot2 Theme for Track Visualizations
#'
#' @return A ggplot2 theme object customized for track visualizations.
#' @import ggplot2
#' @export
theme_track <- function() {
  theme_gray() +
    theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank()) +
    theme(
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(colour = "white"),
      plot.title = ggplot2::element_text(face = "bold", size = 18, color = "#aa1a0a"),
      plot.subtitle = ggplot2::element_text(face = "bold", size = 16),
      plot.background = ggplot2::element_rect(fill = "grey10"),
      panel.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(size = 16, color = "white"),
      text = ggplot2::element_text(color = "white")
    )
}
