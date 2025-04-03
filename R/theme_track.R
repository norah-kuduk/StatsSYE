#' Custom ggplot2 Theme for Track Visualizations
#'
#' @return A ggplot2 theme object customized for track visualizations.
#' @import ggplot2
#' @export
theme_track <- function() {

  theme_gray() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.title = element_text(face = "bold", size = 18, color = "#aa1a0a"),
      plot.subtitle = element_text(face = "bold", size = 16),
      plot.background = element_rect(fill = "#1c1c1c"),
      panel.background = element_blank(),
      legend.background = element_blank(),
      legend.key = element_blank(),
      plot.caption = element_text(size = 16, color = "white"),
      text = element_text(color = "white")
    )
}
