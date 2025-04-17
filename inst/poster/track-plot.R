library(tidyverse)
library(f1animateR)
library(f1dataR)

brazil24 <- get_quali_telemetry(laps = "fastest", season = 2024, round = 21, driver = "NOR", verbose = TRUE)
brazil23 <- get_quali_telemetry(laps = "fastest", season = 2023, round = 20, driver = "NOR", verbose = TRUE)

# smooth Abu Dhabi track data
brazil_track <- smooth_track(brazil24)
start_coord <- brazil_track |> slice(1)

# det team and driver colors
color_24 <- driver_color("NOR")
color_23 <- "darkorange4"

# Add driver labels and combine data
brazil24 <- brazil24 |> mutate(season = 2024)
brazil23 <- brazil23 |> mutate(season = 2023)

plot_data <- bind_rows(brazil24, brazil23) |> mutate(season = as.factor(season))

time_data <- plot_data |> group_by(season) |> filter(time > 64) |> filter(time < 64.2)

# Static plot
static_plot <- ggplot() +
  geom_path(data = brazil_track, aes(x = x2, y = y2, group = 1),
            linewidth = 4, color = "white") +
  geom_point(data = start_coord, aes(x = x2, y = y2),
             color = "black", shape = 18, size = 3) +
  geom_point(data = time_data, aes(x = x, y = y, group = season, color = season),
             size = 2) +
  scale_color_manual(values = c("2024" = color_24, "2023" = color_23)) +
  theme_track() +
  labs(title = "2024 vs. 2023 Norris Qualifying Lap",
       subtitle = "Sao Paulo Grand Prix 2024",
       x = NULL, y = NULL)

# Correct the track alignment
corrected_plot <- corrected_track(static_plot, brazil24)

corrected_plot
