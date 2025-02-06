library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(f1dataR)
library(f1animateR)
library(knitr)
library(kableExtra)

## setup_fastf1()
use_virtualenv("f1dataR_env")

ui <- fluidPage(

  titlePanel("F1 Qualifying Analysis"),

  tabsetPanel(

    # Tab 1: Data Exploration
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
                   selectInput("driver_same", "Select Driver:", choices = NULL),
                   selectInput("season_a", "Select First Season:", choices = 2018:2024, selected = 2024),
                   selectInput("season_b", "Select Second Season:", choices = 2018:2024, selected = 2023),
                   uiOutput("round_ui")
                 )
               ),

               mainPanel(
                 tableOutput("quali_comparison")
               )
             )),

    # Tab 2: Animation (Placeholder)
    tabPanel("Qualifying Animation",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("compare_mode", "Comparison Mode:",
                              choices = c("Same Session - Two Drivers" = "same_session",
                                          "Same Driver - Two Seasons" = "diff_seasons")),

                 conditionalPanel(
                   condition = "input.compare_mode == 'same_session'",
                   selectInput("season_1", "Select Season:", choices = 2018:2024, selected = 2024),
                   uiOutput("anim_round_select"),
                   uiOutput("anim_driver_select_1"),
                   uiOutput("anim_driver_select_2"),
                   actionButton("animate_btn", "Animate Qualifying")
                 ),

                 conditionalPanel(
                   condition = "input.compare_mode == 'diff_seasons'",
                   selectInput("driver_same", "Select Driver:", choices = NULL),
                   selectInput("season_a", "Select First Season:", choices = 2018:2024, selected = 2024),
                   selectInput("season_b", "Select Second Season:", choices = 2018:2024, selected = 2023),
                   uiOutput("round_ui"),
                   actionButton("animate_btn", "Animate Qualifying")
                 )

               ),
               mainPanel(
                 plotOutput("animation_output")  # TODO make animation
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

  #### Reactive ####

  # fetch schedule data for a given season
  schedule_data <- reactive({
    load_schedule(input$season_1)
  })

  # fetch qualifying drivers/teams
  quali_data_1 <- reactive({
    get_session_drivers_and_teams(input$season_1, input$round_1, "Q") |>
      mutate(driver_code = abbreviation) |>
      select(-abbreviation)
  })

  #### Comparison Panel ####

  output$round_select <- renderUI({
    data <- schedule_data()
    selectInput("round_1", "Select Round: ", choices = data$race_name)
  })

  # populate driver selection for same session comparison
  output$driver_select_1 <- renderUI({
    data <- quali_data_1()
    driver_choices <- setNames(data$driver_code, data$name)
    selectInput("driver_1", "Select Driver 1:", choices = driver_choices)
  })

  output$driver_select_2 <- renderUI({
    data <- quali_data_1()
    driver_choices <- setNames(data$driver_code, data$name)
    selectInput("driver_2", "Select Driver 2:", choices = driver_choices)
  })

  # TODO do the different year inputs and code

  # display comparison table (two drivers)
  output$quali_comparison <- renderTable({
    if (input$compare_mode == "same_session") {
      req(input$driver_1, input$driver_2)
      data <- quali_data_1()
      req(data)

      comparison <- data |>
        filter(driver_code == input$driver_1 | driver_code == input$driver_2)

      quali_laps <- fetch_quali_data(input$season_1, input$round_1) |>
        filter(driver == input$driver_1 | driver == input$driver_2) |>
        select(driver, lap_time, lap_number, compound)
      # TODO filter out NaNs

    } else {
      # TODO when compare mode is diff_seasons
      data <- quali_data_1()
    }

    kable(quali_laps, format = "html") |>
      kable_styling("striped", full_width = FALSE)
  }, sanitize.text.function = function(x) x)

  #### Animation Panel ####

  # populate round selection for a season (animation)
  output$anim_round_select <- renderUI({
    data <- schedule_data()
    selectInput("round_1", "Select Round: ", choices = data$race_name)
  })

  # populate driver selection for same session comparison (animation)
  output$anim_driver_select_1 <- renderUI({
    data <- quali_data_1()
    driver_choices <- setNames(data$driver_code, data$name)
    selectInput("driver_1", "Select Driver 1:", choices = driver_choices)
  })

  output$anim_driver_select_2 <- renderUI({
    data <- quali_data_1()
    driver_choices <- setNames(data$driver_code, data$name)
    selectInput("driver_2", "Select Driver 2:", choices = driver_choices)
  })

  output$animate_ui <- renderUI({
    req(input$driver_1, input$driver_2)
    actionButton("animate_btn", "Animate Qualifying")
  })

  # reactive variable to store telemetry data
  plot_data <- reactiveVal(NULL)
  track_data <- reactiveVal(NULL)

  # animate button clicked
  observeEvent(input$animate_btn, {
    # fetch telemetry data
    req(input$driver_1, input$driver_2, input$season_1, input$round_1)

    driver1_data <- get_quali_telemetry(laps = "fastest", season = input$season_1, round = input$round_1, driver = input$driver_1, verbose = TRUE)
    driver2_data <- get_quali_telemetry(laps = "fastest", season = input$season_1, round = input$round_1, driver = input$driver_2, verbose = TRUE)

    # smooth the track data
    track_data(smooth_track(driver1_data))

    driver1_color <- driver_color(driver = input$driver_1)
    driver2_color <- driver_color(driver = input$driver_2)

    # assign driver labels
    driver1_data <- driver1_data |> mutate(driver = input$driver_1)
    driver2_data <- driver2_data |> mutate(driver = input$driver_2)

    # combine telemetry data
    combined_data <- bind_rows(driver1_data, driver2_data) |> mutate(driver = as.factor(driver))

    plot_data(combined_data)
  })

  # Generate animated plot
  output$animation_output <- renderPlot({
    req(plot_data(), track_data())
    track_data <- track_data()
    start_coord <- track_data |> slice(1)

    ggplot() +
      geom_path(data = track_data(), aes(x = x2, y = y2, group = 1),
                linewidth = 8, color = "white") +
      geom_point(data = start_coord, aes(x = x2, y = y2),
                 color = "black", shape = 18, size = 4) +
      geom_point(data = plot_data(), aes(x = x, y = y, group = driver, color = driver),
                 size = 3) +
      theme_track() +
      labs(x = NULL, y = NULL)
      # TODO add in reactive title/caption based on the inputs

    # TODO actual animation

  })

}

shinyApp(ui, server)
