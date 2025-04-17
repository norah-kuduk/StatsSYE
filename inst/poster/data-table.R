fetch_quali_data <- function(season, round) {
  tryCatch(
    {
      results <- load_session_laps(season, round, "Q")
      if (is.null(results) || nrow(results) == 0) return(NULL)
      results
    },
    error = function(e) NULL
  )
}

year_1 <- get_session_drivers_and_teams(2024, "São Paulo Grand Prix", "Q") |>
  mutate(driver_code = abbreviation) |>
  select(-abbreviation)

year_2 <- get_session_drivers_and_teams(2023, "São Paulo Grand Prix", "Q") |>
  mutate(driver_code = abbreviation) |>
  select(-abbreviation)

data <- bind_rows(year_1, year_2) |> filter(driver_code == "NOR")

quali_laps_a <- fetch_quali_data(2024, "São Paulo Grand Prix") |>
  filter(driver == "NOR") |>
  filter(lap_time > 0) |>
  mutate(season = 2024)

quali_laps_b <- fetch_quali_data(2023, "São Paulo Grand Prix") |>
  filter(driver == "NOR") |>
  filter(lap_time > 0) |>
  select(driver, lap_time, lap_number, compound) |>
  mutate(season = 2023)

data <- bind_rows(quali_laps_a, quali_laps_b) |>
  unique() |>
  arrange(lap_time) |>
  group_by(season) |>
  filter(lap_time == min(lap_time)) |>
  select(driver, season, lap_time, compound, fresh_tyre, lap_number)

kable(data)
