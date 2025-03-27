#' f1 inspired reactable theme
#'
#' Bootstrap-inspired darkly theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 14.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #ffffff.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #afbdcc.
#'
#' @param background_color Background color of the table.
#'      Default is #222222.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard darkly theme
#' reactable(data,
#'           theme = darkly())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = darkly(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export
#'
f1_reactable <- function(font_size = 14,
                   font_color = "#ffffff",
                   header_font_size = 15,
                   header_font_color = "#afbdcc",
                   background_color = "#222222",
                   cell_padding = 6,
                   centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else { centered_content = NULL }

  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = background_color,
    borderColor = "#222222",
    borderWidth = "1px",
    stripedColor = "#adb5bd",
    highlightColor = "#adb5bd",
    cellPadding = cell_padding,
    tableStyle = list(fontSize = font_size),
    headerStyle = list(
      borderWidth = "2px",
      backgroundColor = "#7a1307",
      color = header_font_color,
      transitionDuration = "0.5s",
      "&:hover[aria-sort]" = list(color = "#ffffff"),
      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(color = "#ffffff"),
      fontSize = header_font_size
    ),
    groupHeaderStyle = list(
      "&:not(:empty)" = list(color = font_color,
                             fontSize = header_font_size),
      "&:hover" = list(
        fontWeight = "bold",
        transitionDuration = "1s",
        transitionTimingFunction = "ease-out",
        color = "#afbdcc"
      )
    ),
    searchInputStyle = list(
      backgroundColor = "#ffffff",
      color = "#212529",
      borderColor = "#222222",
      "&:focus" = list(color = "#212529")
    ),
    inputStyle = list(backgroundColor = "#ffffff", color = "#212529"),
    rowSelectedStyle = list(backgroundColor = "#7a1307", color = "#ffffff"),
    selectStyle = list(
      backgroundColor = "#7a1307",
      color = "#ffffff",
      borderColor = "#ffffff",
      outlineColor = "#ffffff",
      "&:hover" = list(backgroundColor = "#aa1a0a")
    ),
    pageButtonStyle = list(
      backgroundColor = "#7a1307",
      color = "#ffffff",
      "&:hover" = list(backgroundColor = "#aa1a0a")
    ),
    pageButtonHoverStyle = list(backgroundColor = "#aa1a0a", color = "#ffffff"),
    pageButtonActiveStyle = list(backgroundColor = "#aa1a0a", color = "#ffffff"),
    pageButtonCurrentStyle = list(backgroundColor = "#aa1a0a", color = "#ffffff")
  )
}
