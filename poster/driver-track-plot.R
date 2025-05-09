library(tidyverse)
library(f1animateR)
library(f1dataR)
library(knitr)

ver <- get_quali_telemetry(laps = "fastest", season = 2023, round = 20, driver = "VER", verbose = FALSE)
zho <- get_quali_telemetry(laps = "fastest", season = 2023, round = 20, driver = "ZHO", verbose = FALSE)

# smooth Abu Dhabi track data
brazil_track <- smooth_track(ver)
start_coord <- brazil_track |> slice(1)

# det team and driver colors
ver_color <- constructor_color("redbull")
zho_color <- constructor_color("sauber")

# Add driver labels and combine data
ver <- ver |> mutate(driver = "VER")
zho <- zho |> mutate(driver = "ZHO")

plot_data <- bind_rows(ver, zho) |> mutate(driver = as.factor(driver))

time_data <- plot_data |> group_by(driver) |> filter(time > 63.2) |> filter(time < 63.4)

# Pick two consecutive points near the start of the track to show direction
arrow_start <- brazil_track |> slice(2)
arrow_end <- brazil_track |> slice(10)

# Static plot
static_plot <- ggplot() +
  geom_path(data = brazil_track, aes(x = x2, y = y2, group = 1),
            linewidth = 4, color = "white") +
  geom_point(data = start_coord, aes(x = x2, y = y2),
             color = "black", shape = 18, size = 3) +
  geom_point(data = time_data, aes(x = x, y = y, group = driver, color = driver),
             size = 2) +
  scale_color_manual(values = c("VER" = ver_color, "ZHO" = zho_color)) +
  theme_track() +
  geom_segment(
    data = NULL,
    aes(x = arrow_start$x, y = arrow_start$y,
        xend = arrow_end$x, yend = arrow_end$y),
    arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
    color = "gray40",
    linewidth = 0.7
  ) +
  labs(title = "Verstappen vs. Zhou Qualifying Lap",
       subtitle = "São Paulo Grand Prix 2023",
       x = NULL, y = NULL)

# Correct the track alignment
corrected_plot <- corrected_track(static_plot, brazil24)

corrected_plot


data <- get_session_drivers_and_teams(2023, "São Paulo Grand Prix", "Q") |>
  mutate(driver_code = abbreviation) |>
  select(-abbreviation)

data <- data |> filter(driver_code == "VER" | driver_code == "ZHO")

quali_laps_b <- fetch_quali_data(2023, "São Paulo Grand Prix") |>
  filter(driver == "VER" | driver == "ZHO") |>
  filter(lap_time > 0) |>
  select(driver, lap_time, lap_number, compound) |>
  mutate(season = 2023) |>
  group_by(driver) |>
  filter(lap_time == min(lap_time)) |>
  select(driver, season, lap_time, compound)

kable(quali_laps_b)
