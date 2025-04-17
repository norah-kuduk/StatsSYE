fetch_quali_data_seasons <- function(season_a, season_b, round) {
  tryCatch(
    {
      results_a <- load_session_laps(season_a, round, "Q") |> mutate(season = season_a)
      results_b <- load_session_laps(season_b, round, "Q") |> mutate(season = season_b)
      if (is.null(results_a) || is.null(results_b) || nrow(results_a) == 0 || nrow(results_b) == 0) {
        return(NULL)
      } else {
        results <- bind_rows(results_a, results_b)
        return(results)
      }

    },
    error = function(e) NULL
  )
}

quali_laps <- fetch_quali_data_seasons(2024, 2023, "São Paulo Grand Prix") |>
  filter(driver == "NOR") |>
  filter(lap_time > 0) |>
  select(driver, season, lap_time, lap_number, compound) |>
  arrange(desc(lap_time)) |>
  unique() |>
  mutate(index = row_number()) |>
  mutate(season = as.factor(season))


ggplot(quali_laps, aes(x = index, y = lap_time)) +
  geom_segment(aes(x = index, xend = index, y = 0, yend = lap_time, color = compound, linetype = season)) +
  geom_point(size = 4, aes(color = compound), alpha = 0.7) +
  geom_label(aes(label = lap_number), nudge_x = 0.2) +
  scale_color_manual(values = c("SOFT" = "red", "MEDIUM" = "gold", "HARD" = "white", "WET" = "blue", "INTERMEDIATE" = "green",
                                "HYPERSOFT" = "lightpink", "ULTRASOFT" = "purple", "SUPERSOFT" = "tomato", "SUPERHARD" = "orange")) +
  coord_flip() +
  labs(
    title = "Tire Performance Degradation for Lando Norris",
    y = "Lap Time (seconds)",
    color = "Tire Compound",
    linetype = "Season"
  ) +
  theme_track() +
  theme(legend.position = "top", legend.text = element_text(color = "#1C1C1C"),
        legend.title = element_text(color = "#1C1C1C", face = "bold"),
        legend.box.background = element_rect(color="white"),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
