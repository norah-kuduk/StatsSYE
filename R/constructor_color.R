#' Get the color associated with a constructor
#'
#' @param constructor_input A character string representing either the full name of the constructor or a constructor abbreviation.
#' @return A character string representing the color associated with the constructor. Returns NA if the constructor is not found.
#' @import dplyr
#' @export
constructor_color <- function(constructor_input) {
  TEAM_COLORS <- c('alpine' = '#ff87bc', 'aston martin' = '#00665f', 'ferrari' = '#e8002d',
                   'haas' = '#b6babd', 'mclaren' = '#ff8000', 'mercedes' = '#27f4d2',
                   'rb' = '#364aa9', 'red bull' = '#0600ef', 'sauber' = '#00e700',
                   'williams' = '#00a0dd', 'renault' = '#ffd700', 'racing point' = '#f596c8',
                   'alpha tauri' = '#2b4562', 'torro rosso' = '#0032ff', 'force india' = '#ff80c7',
                   'alfa romeo' = '#900000')

  TEAM_ABBR <- c('AMR' = 'aston martin', 'APN' = 'alpine', 'FER' = 'ferrari',
                 'HAA' = 'haas', 'MCL' = 'mclaren', 'MER' = 'mercedes',
                 'RB' = 'rb', 'RBR' = 'red bull', 'SAU' = 'sauber',
                 'WIL' = 'williams', 'REN' = 'renault', 'RP' = 'racing point',
                 'AT' = 'alpha tauri', 'TR' = 'torro rosso', 'FI' = 'force india',
                 'ALF' = 'alfa romeo')

  team_colors_df <- data.frame(team_name = names(TEAM_COLORS), color = TEAM_COLORS, stringsAsFactors = FALSE)
  team_abbr_df <- data.frame(team_abbr = names(TEAM_ABBR), team_name = TEAM_ABBR, stringsAsFactors = FALSE)
  team_df <- team_abbr_df |> left_join(team_colors_df, by = "team_name")

  if (constructor_input %in% team_df$team_name) {
    return(team_df$color[team_df$team_name == constructor_input])
  }

  if (constructor_input %in% team_df$team_abbr) {
    return(team_df$color[team_df$team_abbr == constructor_input])
  }

  return(NA)
}
