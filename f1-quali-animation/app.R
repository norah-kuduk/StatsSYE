library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(f1dataR)
library(f1animateR)
library(knitr)
library(kableExtra)
library(gganimate)

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
                 checkboxInput("carry_over", "Use Selections from Data Exploration", value = FALSE),

                 radioButtons("anim_compare_mode", "Comparison Mode:",
                              choices = c("Same Session - Two Drivers" = "same_session",
                                          "Same Driver - Two Seasons" = "diff_seasons")),

                 conditionalPanel(
                   condition = "input.anim_compare_mode == 'same_session'",
                   selectInput("anim_season_1", "Select Season:", choices = 2018:2024, selected = 2024),
                   uiOutput("anim_round_select"),
                   uiOutput("anim_driver_select_1"),
                   uiOutput("anim_driver_select_2"),
                   actionButton("animate_btn", "Animate Qualifying"),
                   actionButton("stop_btn", "Clear Animation")

                 ),

                 conditionalPanel(
                   condition = "input.anim_compare_mode == 'diff_seasons'",
                   selectInput("driver_same", "Select Driver:", choices = NULL),
                   selectInput("season_a", "Select First Season:", choices = 2018:2024, selected = 2024),
                   selectInput("season_b", "Select Second Season:", choices = 2018:2024, selected = 2023),
                   uiOutput("round_ui"),
                   actionButton("animate_btn", "Animate Qualifying"),
                   actionButton("stop_btn", "Clear Animation")
                 )

               ),
               mainPanel(
                 plotOutput("animation_output")
               )
             ))
  )
)

server <- function(input, output, session) {
  # TODO possibly add a way to carry over inputs from data exploration (default?)

  observe({
    req(input$carry_over)  # Only execute when carry_over is checked

    # Ensure inputs exist before updating
    req(input$compare_mode, input$season_1, input$driver_1, input$driver_2, input$round_1)

    # If comparison mode is "same_session", update corresponding inputs
    if (input$compare_mode == "same_session") {
      updateRadioButtons(session, "anim_compare_mode", selected = input$compare_mode)
      updateSelectInput(session, "anim_season_1", selected = input$season_1)
      updateSelectInput(session, "anim_round_select", selected = input$round_1)
      updateSelectInput(session, "anim_driver_select_1", selected = input$driver_1)
      updateSelectInput(session, "anim_driver_select_2", selected = input$driver_2)
    }
  })

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
        filter(lap_time > 0) |>
        select(driver, lap_time, lap_number, compound, session_type)
      # TODO filter out NaNs


    } else {
      # TODO when compare mode is diff_seasons
      data <- quali_data_1()
    }

    kable(quali_laps, format = "html") |>
      kable_styling("striped", full_width = FALSE)
  }, sanitize.text.function = function(x) x)

  #### Animation Panel ####

  # TODO separate comparison modes

  # need to add separate season selection
  anim_schedule_data <- reactive({
    load_schedule(input$anim_season_1)
  })

  anim_quali_data_1 <- reactive({
    get_session_drivers_and_teams(input$anim_season_1, input$round_1, "Q") |>
      mutate(driver_code = abbreviation) |>
      select(-abbreviation)
  })

  # populate round selection for a season (animation)
  output$anim_round_select <- renderUI({
    data <- anim_schedule_data()
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


  # reactive variable to store telemetry data
  plot_data <- reactiveVal(NULL)
  track_data <- reactiveVal(NULL)
  animation_running <- reactiveVal(FALSE)

  # animate button clicked
  observeEvent(input$animate_btn, {
    animation_running(TRUE)

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

    # only proceed if animation is still running
    if (animation_running()) {
      plot_data(combined_data)
    }
  })

  observeEvent(input$stop_btn, {
    animation_running(FALSE)  # stop animation
    plot_data(NULL)  # clear data to stop rendering
  })

  # Generate animated plot
  output$animation_output <- renderImage({
    req(plot_data(), track_data(), animation_running())  # Ensure animation is running
    outfile <- tempfile(fileext='.gif')

    t_df <- track_data()

    start_coord <- t_df |> slice(1)

    static_plot <- ggplot() +
      geom_path(data = track_data(), aes(x = x2, y = y2, group = 1),
                linewidth = 8, color = "white") +
      geom_point(data = start_coord, aes(x = x2, y = y2),
                 color = "black", shape = 18, size = 4) +
      geom_point(data = plot_data(), aes(x = x, y = y, group = driver, color = driver),
                 size = 3) +
      theme_track() +
      labs(title = paste(input$driver_1, "vs.", input$driver_2, "Qualifying Lap"),
           subtitle = paste(input$season_1, input$round_1),
           x = NULL, y = NULL)

    # apply track correction
    corrected_plot <- corrected_track(static_plot, plot_data())

    animated_plot <- corrected_plot +
      transition_reveal(along = time) +
      ease_aes('linear')

    anim_save("outfile.gif", animate(animated_plot, width = 700, height = 700, fps = 10)) # New

    # Return a list containing the filename
    list(src = "outfile.gif", contentType = 'image/gif')
    }, deleteFile = TRUE)
  }

shinyApp(ui, server)
