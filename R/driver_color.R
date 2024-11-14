#' Get the color associated with a driver
#'
#' @param driver_input A character string representing either the full name of the driver or a driver abbreviation.
#' @return A character string representing the color associated with the driver. Returns NA if the driver is not found.
#' @import dplyr
#' @export
driver_color <- function(driver_input) {
  DRIVER_COLORS <- c('alexander albon' = '#005aff', 'ayumu iwasa' = '#1e6176', 'carlos sainz' = '#ff8181',
                     'charles leclerc' = '#dc0000', 'daniel ricciardo' = '#2b4562', 'esteban ocon' = '#ff117c',
                     'felipe drugovich' = '#2f9b90', 'fernando alonso' = '#006f62', 'franco colapinto' = '#639aff',
                     'frederik vesti' = '#00a6ff', 'george russell' = '#24ffff', 'isack hadjar' = '#1e6176',
                     'jack doohan' = '#894667', 'jake dennis' = '#907400', 'kevin magnussen' = '#ffffff',
                     'lance stroll' = '#00413b', 'lando norris' = '#eeb370', 'lewis hamilton' = '#00d2be',
                     'liam lawson' = '#2b4562', 'logan sargeant' = '#012564', 'max verstappen' = '#fcd700',
                     'nico hulkenberg' = '#cacaca', 'nyck de vries' = '#1e3d61', 'oliver bearman' = '#c40000',
                     'oscar piastri' = '#ff8700', 'pato oward' = '#ee6d3a', 'pierre gasly' = '#fe86bc',
                     'robert shwartzman' = '#9c0000', 'sergio perez' = '#ffec7b', 'theo pourchaire' = '#004601',
                     'valtteri bottas' = '#00e701', 'yuki tsunoda' = '#356cac', 'zak osullivan' = '#1b3d97',
                     'zhou guanyu' = '#008d01')

  DRIVER_ABBR <- c('ALB' = 'alexander albon', 'ALO' = 'fernando alonso', 'BEA' = 'oliver bearman',
                   'BOT' = 'valtteri bottas', 'COL' = 'franco colapinto', 'DEN' = 'jake dennis',
                   'DEV' = 'nyck de vries', 'DOO' = 'jack doohan', 'DRU' = 'felipe drugovich',
                   'GAS' = 'pierre gasly', 'HAD' = 'isack hadjar', 'HAM' = 'lewis hamilton',
                   'HUL' = 'nico hulkenberg', 'IWA' = 'ayumu iwasa', 'LAW' = 'liam lawson',
                   'LEC' = 'charles leclerc', 'MAG' = 'kevin magnussen', 'NOR' = 'lando norris',
                   'OCO' = 'esteban ocon', 'OSU' = 'zak osullivan', 'OWA' = 'pato oward',
                   'PER' = 'sergio perez', 'PIA' = 'oscar piastri', 'POU' = 'theo pourchaire',
                   'RIC' = 'daniel ricciardo', 'RUS' = 'george russell', 'SAI' = 'carlos sainz',
                   'SAR' = 'logan sargeant', 'SHW' = 'robert shwartzman', 'STR' = 'lance stroll',
                   'TSU' = 'yuki tsunoda', 'VER' = 'max verstappen', 'VES' = 'frederik vesti',
                   'ZHO' = 'zhou guanyu')

  driver_colors_df <- data.frame(driver_name = names(DRIVER_COLORS), color = DRIVER_COLORS, stringsAsFactors = FALSE)
  driver_abbr_df <- data.frame(driver_abbr = names(DRIVER_ABBR), driver_name = DRIVER_ABBR, stringsAsFactors = FALSE)
  driver_df <- driver_abbr_df |> left_join(driver_colors_df, by = "driver_name")

  if (driver_input %in% driver_df$driver_name) {
    return(driver_df$color[driver_df$driver_name == driver_input])
  }

  if (driver_input %in% driver_df$driver_abbr) {
    return(driver_df$color[driver_df$driver_abbr == driver_input])
  }

  return(NA)
}
