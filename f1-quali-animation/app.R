library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(f1dataR)
library(f1animateR)
library(knitr)
library(kableExtra)

ui <- fluidPage(
  titlePanel("F1 Qualifying Animation"),

  sidebarLayout(
    sidebarPanel(
      radioButtons("compare_mode", "Comparison Mode:",
                   choices = c("Same Session - Two Drivers" = "same_session",
                               "Same Driver - Two Seasons" = "diff_seasons")),

      conditionalPanel(
        condition = "input.compare_mode == 'same_session'",
        selectInput("season_1", "Select Season:", choices = 2018:2024, selected = 2024),
        selectInput("round_1", "Select Round:", choices = 1:22, selected = 1),
        uiOutput("driver_select_1"),
        uiOutput("driver_select_2")
      ),

      conditionalPanel(
        condition = "input.compare_mode == 'diff_seasons'",

      )
    ),

    mainPanel(
      tableOutput("quali_comparison")
    )
  )
)

server <- function(input, output, session) {

  use_virtualenv("f1dataR_env")

  # fetch qualifying data for a given season and round
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

  # fetch qualifying drivers/teams for selected season & round
  quali_data_1 <- reactive({
    get_session_drivers_and_teams(input$season_1, input$round_1, "Q") |>
      mutate(driver_code = abbreviation) |>
      select(-abbreviation)
  })

  # populate driver selection for same session comparison
  output$driver_select_1 <- renderUI({
    data <- quali_data_1()

    # create named vector: display driver name, store driver code
    driver_choices <- setNames(data$driver_code, data$name)

    selectInput("driver_1", "Select Driver 1:", choices = driver_choices)
  })

  output$driver_select_2 <- renderUI({
    data <- quali_data_1()

    # create named vector: Display driver name, store driver code
    driver_choices <- setNames(data$driver_code, data$name)

    selectInput("driver_2", "Select Driver 2:", choices = driver_choices)
  })

  # display comparison table
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

    } else {
      data <- quali_data_1()
    }

    kable(quali_laps, format = "html") |>
      kable_styling("striped", full_width = FALSE)
  }, sanitize.text.function = function(x) x)
}

shinyApp(ui, server)
