library(tidyverse)
library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(f1dataR)
library(f1animateR)
library(knitr)
library(kableExtra)
library(gganimate)
library(reactable)
library(ggrepel)
library(tidyr)
library(bslib)
library(reactablefmtr)
library(shinycssloaders)
library(showtext)
library(gifski)

## setup_fastf1()
## devtools::install_github("https://github.com/norah-kuduk/StatsSYE”)
## use_virtualenv("f1dataR_env")

# Create and activate environment if it doesn't exist
if (!virtualenv_exists("f1env")) {
  virtualenv_create("f1env")
  virtualenv_install("f1env", packages = c("fastf1"))
}
use_virtualenv("f1env", required = TRUE)

# Set a cache directory that works in ShinyApps.io
cache_dir <- file.path(tempdir(), "fastf1_cache")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

# Enable FastF1 cache via reticulate
fastf1 <- import("fastf1", delay_load = TRUE)
fastf1$Cache$enable_cache(cache_dir)

cat("FastF1 cache directory set to:", cache_dir, "\n")

font <- font_link("Titillium Web",
                  href="https://fonts.googleapis.com/css2?family=Patrick+Hand&family=Titillium+Web:ital,wght@0,200;0,300;0,400;0,600;0,700;0,900;1,200;1,300;1,400;1,600;1,700")

# custom theme
f1_theme <- bs_theme(
  bootswatch = "darkly",  # dark theme base
  bg = "#1C1C1C",          # background color
  fg = "#ffffff",           # default text color
  primary = "#aa1a0a",    # red highlight color (matches plot title)
  secondary = "#aa1a0b",    # secondary text color
  base_font = font,
  code_font = font_google("Fira Code"),
  "nav-link-hover-color" = "#aa1a0a",  # hover color for tabs
  "nav-link-active-color" = "#aa1a0a",   # active tab color
  "link-color" = "#aa1a0a" # link color
)

options(spinner.image = "f1_loading.gif",
        spinner.image.width = "150px",
        spinner.image.height = "150px")

font_add_google("Titillium Web", "titillium")
showtext_auto()

ui <- fluidPage(
  theme = f1_theme,

  titlePanel(div(
    "F1 Qualifying Analysis",
    actionButton("restart_app", "Restart App", class = "btn-danger", style = "float: right; margin-right: 20px; margin-top: 10px;")
  )),

  tabsetPanel(
    #### Tab 1: Instructions and Important Infomation ####
    tabPanel("Instructions",
             fluidPage(
               titlePanel("How to Use This App"),

               h3("Overview"),
               p("This app allows users to analyze F1 qualifying performance by comparing telemetry data for drivers."),

               h3("Different Comparison Modes"),
               tags$ul(
                 tags$li(strong("Same Session:"), " Compare two drivers in the same qualifying session."),
                 tags$li(strong("Same Driver, Different Years:"), " Analyze how a single driver performed in different seasons at the same circuit.")
               ),

               h3("The Data Exploration Page"),
               tags$ul(
                 tags$li("Table of all the lap times across drivers/seasons."),
                 tags$li("Tire degradation analysis."),
                 tags$li("Final qualifying and race position tables."),
               ),

               h3("How to Use Animation Feature"),
               tags$ol(
                 tags$li("Choose whether or not to carry over data from exploration page."),
                 tags$li("Select the desired comparison mode."),
                 tags$li("Choose the season(s) and round you want to analyze."),
                 tags$li("Select the drivers (or driver, if comparing across seasons)."),
                 tags$li("Click 'Animate Qualifying' to visualize lap performance."),
                 tags$li("Use 'Clear Animation' to reset.")
               ),

               h3("Important Notes"),
               tags$ul(
                 tags$li("Please be patient, there is a lot of data that gets loaded when things on the app are changed, if it feels like it's glitching, wait a minute."),
                 tags$li("Animations loop through each driver or season's fastest lap from the qualifying sesson until stopped."),
                 tags$li("If you want to change any of the inputs for your animation, hit the 'Clear Animation' button first, this will prevent loading issues."),
               ),

               h3("References"),
               p("This app utilizes the `f1dataR` package for fetching Formula 1 data, including qualifying times, race results, and telemetry."),
               tags$a(href="https://cran.r-project.org/package=f1dataR", "f1dataR on CRAN", target="_blank")
             )

    ),


    #### Tab 2: Data Exploration ####
    tabPanel("Data Exploration",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("compare_mode", "Comparison Mode:",
                              choices = c("Same Session - Two Drivers" = "same_session",
                                          "Same Driver - Two Seasons" = "diff_seasons")),

                 conditionalPanel(
                   condition = "input.compare_mode == 'same_session'",
                   selectInput("season_1", "Select Season:", choices = 2018:2024, selected = 2024),
                   uiOutput("round_select"),
                   uiOutput("driver_select_1"),
                   uiOutput("driver_select_2"),
                 ),

                 conditionalPanel(
                   condition = "input.compare_mode == 'diff_seasons'",
                   uiOutput("driver_same"),
                   uiOutput("season_a"),
                   uiOutput("season_b"),
                   uiOutput("round_overlap")
                 )
               ),

               mainPanel(
                 withSpinner(
                   reactableOutput("quali_comparison")
                 ),
                 withSpinner(
                   tableOutput("quali_race_table")
                 ),
                 withSpinner(
                   plotOutput("tire_degradation_plot")
                 ),
               )
             )),

    #### Tab 3: Animation ####
    tabPanel("Qualifying Animation",
             sidebarLayout(
               sidebarPanel(
                 checkboxInput("carry_over", "Use Selections from Data Exploration", value = TRUE),

                 radioButtons("anim_compare_mode", "Comparison Mode:",
                              choices = c("Same Session - Two Drivers" = "same_session",
                                          "Same Driver - Two Seasons" = "diff_seasons")),

                 conditionalPanel(
                   condition = "input.anim_compare_mode == 'same_session'",
                   selectInput("anim_season_1", "Select Season:", choices = 2018:2024, selected = 2024),
                   uiOutput("anim_round_select"),
                   uiOutput("anim_driver_select_1"),
                   uiOutput("anim_driver_select_2"),
                   sliderInput("smoothing_factor", "Smoothing Factor:",
                               min = 0, max = 60,
                               value = 16),
                   actionButton("animate_btn", "Animate Qualifying"),
                   actionButton("stop_btn", "Clear Animation")

                 ),

                 conditionalPanel(
                   condition = "input.anim_compare_mode == 'diff_seasons'",
                   uiOutput("anim_driver_same"),
                   uiOutput("anim_season_a"),
                   uiOutput("anim_season_b"),
                   uiOutput("anim_round_overlap"),
                   sliderInput("smoothing_factor", "Smoothing Factor:",
                               min = 0, max = 60,
                               value = 16),
                   actionButton("animate_btn", "Animate Qualifying"),
                   actionButton("stop_btn", "Clear Animation")
                 )
               ),

               mainPanel(
                 withSpinner(
                   plotOutput("animation_output")
                 )
               )
             ))
  )
)

server <- function(input, output, session) {
  #### Functions ####

  # fetch qualifying data
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
  #### Reactive ####

  observeEvent(input$restart_app, {
    session$reload()
  })

  # fetch schedule data for a given season
  schedule_data <- reactive({
    req(input$season_1)
    load_schedule(input$season_1)
  })

  seasons_schedule_data <- reactive({
    req(input$season_a, input$season_b)
    schedule_a <- load_schedule(input$season_a) |> mutate(season = input$season_a)
    schedule_b <- load_schedule(input$season_b) |> mutate(season = input$season_b)
    bind_rows(schedule_a, schedule_b)
  })

  # fetch qualifying drivers/teams
  quali_data_1 <- reactive({
    # browser()
    req(input$season_1, input$round_1)
    get_session_drivers_and_teams(input$season_1, input$round_1, "Q") |>
      mutate(driver_code = abbreviation) |>
      select(-abbreviation)
  })

  quali_data_diff_seasons <- reactive({
    req(input$season_a, input$season_b, input$round_constant, input$driver_same)
    year_1 <- get_session_drivers_and_teams(input$season_a, input$round_constant, "Q") |>
      mutate(driver_code = abbreviation) |>
      select(-abbreviation)

    year_2 <- get_session_drivers_and_teams(input$season_b, input$round_constant, "Q") |>
      mutate(driver_code = abbreviation) |>
      select(-abbreviation)

    bind_rows(year_1, year_2)
  })

  # get quali and race results for comparison
  quali_race_results <- reactive({
    req(input$season_1, input$round_1, input$driver_1, input$driver_2)
    schedule <- schedule_data()
    round_num <- schedule |>
      filter(race_name == input$round_1) |>
      mutate(round = as.integer(round)) |>
      pull(round)

    full_results2 <- load_results(input$season_1, round_num)
    driver_data <- load_drivers(input$season_1)

    inner_join(full_results2, driver_data, by = "driver_id")
  })

  # get quali and race results for comparison (diff seasons)
  seasons_quali_race_results <- reactive({
    req(input$season_a, input$season_b, input$round_constant)

    schedule <- seasons_schedule_data()

    rounds <- schedule |>
      filter(race_name == input$round_constant) |>
      mutate(round = as.integer(round))

    round_num_a <- rounds |>
      filter(season == input$season_a) |>
      pull(round)

    round_num_b <- rounds |>
      filter(season == input$season_b) |>
      pull(round)

    full_results_a <- load_results(input$season_a, round_num_a) |> mutate(season = input$season_a)
    full_results_b <- load_results(input$season_b, round_num_b) |> mutate(season = input$season_b)

    full_results <- bind_rows(full_results_a, full_results_b)

    driver_data <- load_drivers(input$season_a)

    inner_join(full_results, driver_data, by = "driver_id")
  })


  # get all drivers from 2018 to 2024 that drove in more than 1 season
  all_drivers <- reactive({
    seasons <- 2018:2024

    # Fetch and combine drivers from all seasons
    driver_data <- bind_rows(lapply(seasons, function(season) {
      load_drivers(season = season) |>
        mutate(season = season)
    }))

    driver_data |>
      unite("Driver", c("given_name", "family_name"), sep = " ") |>
      group_by(code) |>
      mutate(n_seasons = n()) |>
      filter(n_seasons > 1)
  })

  # get all seasons that a driver participated in (2018 to 2024)
  valid_seasons <- reactive({
    req(input$driver_same)

    driver_data <- all_drivers()

    driver_data |> filter(code == input$driver_same) |> pull(season)
  })

  # find races that only existed in those two seasons (has to be by name)
  round_overlap <- reactive({
    req(input$season_a, input$season_b)

    season_a <- input$season_a
    season_b <- input$season_b

    seasons <- c(season_a, season_b)

    race_data <- bind_rows(lapply(seasons, function(season) {
      load_schedule(season = season)
    }))

    race_data |> group_by(race_name) |>
      mutate(num_races = n()) |>
      filter(num_races > 1) |>
      distinct(race_name)
  })

  #### Comparison Panel ####

  ###### SAME SESSION LOADING ######
  output$round_select <- renderUI({
    data <- schedule_data()
    data <- data |> mutate(round = as.integer(round))

    selectInput("round_1", "Select Round: ", choices = data$race_name)
  })

  # populate driver selection for same session comparison
  output$driver_select_1 <- renderUI({
    data <- quali_data_1()
    driver_choices <- setNames(data$driver_code, data$name)
    selectInput("driver_1", "Select Driver 1: ", choices = driver_choices)
  })

  output$driver_select_2 <- renderUI({
    data <- quali_data_1()
    driver_choices <- setNames(data$driver_code, data$name)
    selectInput("driver_2", "Select Driver 2: ", choices = driver_choices)
  })

  ###### SAME DRIVER LOADING ######

  output$driver_same <- renderUI({
    driver_data <- all_drivers()
    driver_choices <- setNames(driver_data$code, driver_data$Driver)
    selectInput("driver_same", "Select Driver: ", choices = driver_choices, selected = "Lewis Hamilton")
  })

  output$season_a <- renderUI({
    data <- valid_seasons()

    selectInput("season_a", "Select First Season: ", choices = data)
  })

  output$season_b <- renderUI({
    data <- valid_seasons()

    selectInput("season_b", "Select Second Season: ", choices = data)
  })

  output$round_overlap <- renderUI({
    data <- round_overlap()

    selectInput("round_constant", "Select Round: ", choices = data$race_name)
  })

  ##### Output Tables #####

  # display comparison table (two drivers)
  output$quali_comparison <- renderReactable({
    if (input$compare_mode == "same_session") {
      req(input$driver_1, input$driver_2)

      data <- quali_data_1()
      req(data)

      comparison <- data |>
        filter(driver_code == input$driver_1 | driver_code == input$driver_2)

      quali_laps <- fetch_quali_data(input$season_1, input$round_1) |>
        filter(driver == input$driver_1 | driver == input$driver_2) |>
        filter(lap_time > 0) |>
        select(driver, lap_time, lap_number, compound) |>
        arrange(lap_time)

      reactable(
        quali_laps,
        striped = TRUE,
        highlight = TRUE,
        bordered = TRUE,
        defaultPageSize = 15,
        theme = f1_reactable(),
        columns = list(
          driver = colDef(name = "Driver", align = "left"),
          lap_time = colDef(name = "Lap Time", align = "center"),
          lap_number = colDef(name = "Lap Number", align = "center"),
          compound = colDef(name = "Tire Compound", align = "center")
        )
      )
    } else {
      req(input$driver_same, input$season_a, input$season_b, input$round_constant)

      data <- quali_data_diff_seasons()

      comparison <- data |> filter(driver_code == input$driver_same)

      quali_laps_a <- fetch_quali_data(input$season_a, input$round_constant) |>
        filter(driver == input$driver_same) |>
        filter(lap_time > 0) |>
        select(driver, lap_time, lap_number, compound) |>
        mutate(season = input$season_a)

      quali_laps_b <- fetch_quali_data(input$season_b, input$round_constant) |>
        filter(driver == input$driver_same) |>
        filter(lap_time > 0) |>
        select(driver, lap_time, lap_number, compound) |>
        mutate(season = input$season_b)

      data <- bind_rows(quali_laps_a, quali_laps_b) |>
        unique() |>
        arrange(lap_time)

      reactable(
        data,
        striped = TRUE,
        highlight = TRUE,
        bordered = TRUE,
        defaultPageSize = 15,
        theme = f1_reactable(),
        columns = list(
          driver = colDef(name = "Driver", align = "left"),
          season = colDef(name = "Season", align = "center"),
          lap_time = colDef(name = "Lap Time", align = "center"),
          lap_number = colDef(name = "Lap Number", align = "center"),
          compound = colDef(name = "Tire Compound", align = "center")
        )
      )
    }
  })

  output$tire_degradation_plot <- renderPlot({
    if (input$compare_mode == "same_session") {

      req(input$driver_1, input$driver_2, input$season_1)

      quali_laps <- fetch_quali_data(input$season_1, input$round_1) |>
        filter(driver == input$driver_1 | driver == input$driver_2) |>
        filter(lap_time > 0) |>
        select(driver, lap_time, lap_number, compound) |>
        arrange(desc(lap_time)) |>
        mutate(index = row_number())


      ggplot(quali_laps, aes(x = index, y = lap_time)) +
        geom_segment(aes(x = index, xend = index, y = 0, yend = lap_time, color = compound, linetype = driver)) +
        geom_point(size = 4, aes(color = compound), alpha = 0.7) +
        geom_label(aes(label = lap_number), nudge_x = 0.2) +
        scale_color_manual(values = c("SOFT" = "red", "MEDIUM" = "gold", "HARD" = "white", "WET" = "blue", "INTERMEDIATE" = "green",
                                      "HYPERSOFT" = "lightpink", "ULTRASOFT" = "purple", "SUPERSOFT" = "tomato", "SUPERHARD" = "orange")) +
        coord_flip() +
        labs(
          title = "Tire Performance Degradation",
          y = "Lap Time (seconds)",
          color = "Tire Compound",
          linetype = "Driver"
        ) +
        theme_track() +
        theme(legend.position = "top", legend.text = element_text(color = "#1C1C1C", size = 16),
              legend.title = element_text(color = "#1C1C1C", face = "bold", size = 16),
              legend.box.background = element_rect(color="white"),
              axis.title.y = element_blank(), axis.text.y = element_blank(),
              axis.ticks.y = element_blank())

    } else {
      req(input$season_a, input$season_b, input$round_constant, input$driver_same)


      quali_laps <- fetch_quali_data_seasons(input$season_a, input$season_b, input$round_constant) |>
        filter(driver == input$driver_same) |>
        filter(lap_time > 0) |>
        select(driver, season, lap_time, lap_number, compound) |>
        arrange(desc(lap_time)) |>
        unique() |>
        mutate(index = row_number())


      ggplot(quali_laps, aes(x = index, y = lap_time)) +
        geom_segment(aes(x = index, xend = index, y = 0, yend = lap_time, color = compound, linetype = season)) +
        geom_point(size = 4, aes(color = compound), alpha = 0.7) +
        geom_label(aes(label = lap_number), nudge_x = 0.2) +
        scale_color_manual(values = c("SOFT" = "red", "MEDIUM" = "gold", "HARD" = "white", "WET" = "blue", "INTERMEDIATE" = "green",
                                      "HYPERSOFT" = "lightpink", "ULTRASOFT" = "purple", "SUPERSOFT" = "tomato", "SUPERHARD" = "orange")) +
        coord_flip() +
        labs(
          title = "Tire Performance Degradation",
          y = "Lap Time (seconds)",
          color = "Tire Compound",
          linetype = "Season"
        ) +
        theme_track() +
        theme(legend.position = "top", legend.text = element_text(color = "#1C1C1C", size = 16),
              legend.title = element_text(color = "#1C1C1C", face = "bold", size = 16),
              legend.box.background = element_rect(color="white"),
              axis.title.y = element_blank(), axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
    }
  }, height = 600, width = 600)

  output$quali_race_table <- renderTable({
    if (input$compare_mode == "same_session") {

      # browser()
      data <- quali_race_results()

      data <- data |> filter(code == input$driver_1 | code == input$driver_2) |>
        unite("driver", c(given_name, family_name), sep = " ") |>
        rename("Driver" = driver, "Team" = constructor_id, "Quali Position" = grid, "Race Position" = position) |>
        select(Driver, Team, `Quali Position`, `Race Position`)

      kable(data, format = "html") |> kable_styling("striped", full_width = FALSE)

    } else {

      data <- seasons_quali_race_results()


      data <- data |> filter(code == input$driver_same) |>
        unite("driver", c(given_name, family_name), sep = " ") |>
        rename("Season" = season, "Driver" = driver, "Team" = constructor_id, "Quali Position" = grid, "Race Position" = position) |>
        select(Season, Driver, Team, `Quali Position`, `Race Position`) |>
        unique()

      kable(data, format = "html") |> kable_styling("striped", full_width = FALSE)

    }
  }, sanitize.text.function = function(x) x)


  #### Animation Panel ####

  # Observe the 'carry_over' checkbox and update inputs when it's checked
  observe({
    req(input$carry_over)  # Only execute when carry_over is checked

    # Ensure inputs exist before updating
    req(input$compare_mode)

    # If comparison mode is "same_session", update corresponding inputs
    if (input$compare_mode == "same_session") {
      req(input$season_1, input$driver_1, input$driver_2, input$round_1)

      updateRadioButtons(session, "anim_compare_mode", selected = input$compare_mode)
      updateSelectInput(session, "anim_season_1", selected = input$season_1)
    } else {
      req(input$season_a, input$season_b, input$driver_same, input$round_constant)

      updateRadioButtons(session, "anim_compare_mode", selected = input$compare_mode)
    }
  })

  ##### SAME SESSION #####

  # Define reactive variables for the schedule and qualifying data
  anim_schedule_data <- reactive({
    req(input$anim_season_1)
    load_schedule(input$anim_season_1)
  })

  anim_quali_data_1 <- reactive({
    req(input$anim_season_1, input$round_1)
    get_session_drivers_and_teams(input$anim_season_1, input$round_1, "Q") |>
      mutate(driver_code = abbreviation) |>
      select(-abbreviation)
  })

  output$anim_round_select <- renderUI({
    data <- anim_schedule_data()

    if (input$carry_over) {
      # Use selection from Data Exploration panel
      selectInput("round_1", "Select Round:",
                  choices = schedule_data()$race_name,
                  selected = input$round_1)  # Set selected to match the carry_over data
    } else {
      # Default behavior: use current data
      selectInput("round_1", "Select Round:",
                  choices = data$race_name)
    }
  })

  output$anim_driver_select_1 <- renderUI({
    data <- anim_quali_data_1()

    if (input$carry_over) {
      selectInput("driver_1", "Select Driver 1:",
                  choices = setNames(data$driver_code, data$name),
                  selected = input$driver_1)  # Set selected to carry over value
    } else {
      selectInput("driver_1", "Select Driver 1:",
                  choices = setNames(data$driver_code, data$name))
    }
  })

  # Update the driver selection UI reactively for Driver 2
  output$anim_driver_select_2 <- renderUI({
    data <- anim_quali_data_1()

    if (input$carry_over) {
      selectInput("driver_2", "Select Driver 2:",
                  choices = setNames(data$driver_code, data$name),
                  selected = input$driver_2)  # Set selected to carry over value
    } else {
      selectInput("driver_2", "Select Driver 2:",
                  choices = setNames(data$driver_code, data$name))
    }
  })

  ##### SAME DRIVER #####

  output$anim_driver_same <- renderUI({
    driver_data <- all_drivers()
    driver_choices <- setNames(driver_data$code, driver_data$Driver)

    selected_driver <- isolate(input$driver_same)

    selectInput("driver_same", "Select Driver:",
                choices = driver_choices,
                selected = if (input$carry_over) selected_driver else NULL)
  })

  output$anim_season_a <- renderUI({
    data <- valid_seasons()

    selected_season_a <- isolate(input$season_a)

    selectInput("season_a", "Select First Season:",
                choices = data,
                selected = if (input$carry_over) selected_season_a else NULL)
  })

  output$anim_season_b <- renderUI({
    data <- valid_seasons()

    selected_season_b <- isolate(input$season_b)

    selectInput("season_b", "Select Second Season: ",
                choices = data,
                selected = if (input$carry_over) selected_season_b else NULL)
  })

  output$anim_round_overlap <- renderUI({
    req(input$season_a, input$season_b)

    data <- round_overlap()

    selected_round <- isolate(input$round_constant)

    selectInput("round_constant", "Select Round: ",
                choices = data$race_name,
                selected = if (input$carry_over) selected_round else NULL)
  })


  # reactive variable to store telemetry data
  plot_data <- reactiveVal(NULL)
  track_data <- reactiveVal(NULL)
  animation_running <- reactiveVal(FALSE)

  ##### Output #####

  # animate button clicked
  observeEvent(input$animate_btn, {
    showSpinner()

    if (input$anim_compare_mode == "same_session") {
      animation_running(TRUE)

      # fetch telemetry data
      req(input$driver_1, input$driver_2, input$season_1, input$round_1)

      driver1_data <- get_quali_telemetry(laps = "fastest", season = input$season_1, round = input$round_1, driver = input$driver_1, verbose = TRUE)
      driver2_data <- get_quali_telemetry(laps = "fastest", season = input$season_1, round = input$round_1, driver = input$driver_2, verbose = TRUE)

      # smooth the track data
      track_data(smooth_track(driver1_data))

      # assign driver labels
      driver1_data <- driver1_data |> mutate(driver = input$driver_1)
      driver2_data <- driver2_data |> mutate(driver = input$driver_2)

      # combine telemetry data
      combined_data <- bind_data_driver(driver1_data, driver2_data, drivers = c(input$driver_1, input$driver_2))

      # smooth speed values

      smooth_speed(plot_data = combined_data, grouping = "driver", input$smoothing_factor)

      # only proceed if animation is still running
      if (animation_running()) {
        plot_data(combined_data)
      }
    } else {
      animation_running(TRUE)

      # fetch telemetry data
      req(input$driver_same, input$season_a, input$season_b, input$round_constant)

      driver_seasona_data <- get_quali_telemetry(laps = "fastest", season = input$season_a, round = input$round_constant, driver = input$driver_1, verbose = TRUE)
      driver_seasonb_data <- get_quali_telemetry(laps = "fastest", season = input$season_b, round = input$round_constant, driver = input$driver_1, verbose = TRUE)

      # smooth the track data
      track_data(smooth_track(driver_seasona_data))

      # assign season labels
      driver_seasona_data <- driver_seasona_data |> mutate(season = input$season_a)
      driver_seasonb_data <- driver_seasonb_data |> mutate(season = input$season_b)

      # combine telemetry data
      combined_data <- bind_rows(driver_seasona_data, driver_seasonb_data) |> mutate(season = as.factor(season))

      smooth_speed(plot_data = combined_data, grouping = "season", input$smoothing_factor)

      # only proceed if animation is still running
      if (animation_running()) {
        plot_data(combined_data)
      }
    }
    hideSpinner()

  })

  driver_colors <- reactive({
    req(input$driver_1, input$driver_2, input$season_1, input$round_1)

    driver1_team <- tolower(get_team_by_driver(
      input$driver_1, season = input$season_1, round = input$round_1, short = TRUE
    ))
    driver2_team <- tolower(get_team_by_driver(
      input$driver_2, season = input$season_1, round = input$round_1, short = TRUE
    ))

    if (driver1_team == driver2_team) {
      driver1_color <- constructor_color(driver1_team)
      driver2_color <- "black"
    } else {
      driver1_color <- constructor_color(driver1_team)
      driver2_color <- constructor_color(driver2_team)
    }

    setNames(c(driver1_color, driver2_color), c(input$driver_1, input$driver_2))
  })

  observeEvent(input$stop_btn, {
    animation_running(FALSE)  # stop animation
    plot_data(NULL)  # clear data to stop rendering
  })

  season_colors <- reactive({
    req(input$season_a, input$season_b, input$driver_same, input$round_constant)

    seasona_team <- tolower(get_team_by_driver(
      input$driver_same, season = input$season_a, round = input$round_constant, short = TRUE
    ))
    seasonb_team <- tolower(get_team_by_driver(
      input$driver_same, season = input$season_b, round = input$round_constant, short = TRUE
    ))

    if (seasona_team == seasonb_team) {
      seasona_color <- constructor_color(seasona_team)
      seasonb_color <- "black"
    } else {
      seasona_color <- constructor_color(seasona_team)
      seasonb_color <- constructor_color(seasonb_team)
    }

    setNames(c(seasona_color, seasonb_color), c(input$season_a, input$season_b))
  })

  # generate animated plot
  output$animation_output <- renderImage({
    showSpinner()
    req(input$anim_compare_mode)


    if (input$anim_compare_mode == "same_session") {
      req(plot_data(), track_data(), animation_running())  # Ensure animation is running

      t_df <- track_data()
      plot_data <- plot_data()


      plot_data <- smooth_speed(plot_data, "driver", input$smoothing_factor)


      start_coord <- t_df |> slice(1)

      driver_colors <- driver_colors()


      static_plot <- ggplot() +
        geom_path(data = t_df, aes(x = x2, y = y2, group = 1),
                  linewidth = 8, color = "white") +
        geom_point(data = start_coord, aes(x = x2, y = y2),
                   color = "black", shape = 18, size = 4) +
        geom_point(data = plot_data(), aes(x = x, y = y, group = driver, color = driver),
                   size = 3) +
        scale_color_manual(values = driver_colors) +
        theme_track() +
        theme(plot.title = element_text(family = "titillium", face = "bold", size = 20, color = "#aa1a0a"),
              plot.subtitle = element_text(face = "bold", size = 16, color = "white"),
              plot.caption = element_text(face = "plain", size = 16, color = "white")) +
        labs(title = paste(input$driver_1, "vs.", input$driver_2, "Qualifying Lap"),
             subtitle = paste(input$season_1, input$round_1),
             x = NULL, y = NULL)

      # apply track correction
      corrected_plot <- corrected_track(static_plot, plot_data())

      animated_plot <- corrected_plot +
        transition_reveal(along = time) +
        ease_aes('linear') +
        labs(
          caption = paste(
            "Time: {sprintf('%.3f', frame_along)} s\n",
            input$driver_1, " Speed: {plot_data$speed_smooth[which.min(abs(plot_data$time[plot_data$driver == input$driver_1] - frame_along))]} kph\n",
            input$driver_2," Speed: {plot_data$speed_smooth[which.min(abs(plot_data$time[plot_data$driver == input$driver_2] - frame_along))]} kph"
          )
        )

      outfile <- tempfile(fileext = ".gif")
      animation <- animate(animated_plot, width = 700, height = 700, fps = 10, renderer = gifski_renderer())
      anim_save(outfile, animation = animation)
    }

    if (input$anim_compare_mode == "diff_seasons") {
      req(plot_data(), track_data(), animation_running())  # Ensure animation is running


      t_df <- track_data()
      plot_data <- plot_data()

      plot_data <- smooth_speed(plot_data, "season", input$smoothing_factor)

      start_coord <- t_df |> slice(1)

      season_colors <- season_colors()

      static_plot <- ggplot() +
        geom_path(data = t_df, aes(x = x2, y = y2, group = 1),
                  linewidth = 8, color = "white") +
        geom_point(data = start_coord, aes(x = x2, y = y2),
                   color = "black", shape = 18, size = 4) +
        geom_point(data = plot_data(), aes(x = x, y = y, group = season, color = season),
                   size = 3) +
        scale_color_manual(values = season_colors) +
        theme_track() +
        theme(plot.title = element_text(family = "titillium", face = "bold", size = 20, color = "#aa1a0a"),
              plot.subtitle = element_text(face = "bold", size = 16, color = "white"),
              plot.caption = element_text(face = "plain", size = 16, color = "white")) +
        labs(title = paste(input$season_a, "vs.", input$season_b, "Qualifying Lap"),
             subtitle = paste(input$driver_same, input$round_constant),
             x = NULL, y = NULL)

      # apply track correction
      corrected_plot <- corrected_track(static_plot, plot_data())


      animated_plot <- corrected_plot +
        transition_reveal(along = time) +
        ease_aes('linear') +
        labs(
          caption = paste(
            "Time: {sprintf('%.3f', frame_along)} s\n",
            input$season_a, " Speed: {plot_data$speed_smooth[which.min(abs(plot_data$time[plot_data$season == input$season_a] - frame_along))]} kph\n",
            input$season_b," Speed: {plot_data$speed_smooth[which.min(abs(plot_data$time[plot_data$season == input$season_b] - frame_along))]} kph"
          )
        )


      outfile <- tempfile(fileext = ".gif")
      animation <- animate(animated_plot, width = 700, height = 700, fps = 10, renderer = gifski_renderer())
      anim_save(outfile, animation = animation)
    }
    hideSpinner()

    # return a list containing the filename
    list(src = outfile, contentType = 'image/gif')

  }, deleteFile = TRUE)
}

shinyApp(ui, server)
