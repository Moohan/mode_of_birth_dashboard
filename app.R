library(shiny)
library(plotly)
library(tidyverse)

data <- read_rds("data.rds")

# Define the UI for the dashboard
ui <- fillPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "mode",
        label = "mode of delivery",
        distinct(data, delivery),
        selected = "Spontaneous",
        multiple = TRUE
      ),
      selectInput(
        inputId = "induced",
        label = "Induction type",
        choices = distinct(data, induced),
        selected = "Not Induced",
        multiple = TRUE
      ),
      selectizeInput(
        inputId = "hb",
        label = "Health Board(s) (8 maximum)",
        choices = distinct(data, health_board),
        multiple = TRUE,
        options = list(maxItems = 8L)
      ),
      checkboxInput(inputId = "scotland_check", label = "Show Scotland trend", value = TRUE)
    ),

    mainPanel(
      plotOutput(outputId = "line_plot", width = "100%"),
      plotlyOutput(outputId = "plotly_plot")
    )
  )
)



# Define server logic required
server <- function(input, output) {

  output$line_plot <- renderPlot({

    HB_data <- data %>%
      filter(health_board %in% input$hb) %>%
      filter(induced %in% input$induced) %>%
      group_by(financial_year, health_board) %>%
      summarise(
        all = sum(livebirths),
        selected_mode = sum(livebirths[delivery %in% input$mode])
      ) %>%
      ungroup() %>%
      mutate(rate_per_1000_deliveries = (selected_mode / all) * 1000)

    Scotland_data <- data %>%
      filter(induced %in% input$induced) %>%
      mutate(health_board = "Scotland") %>%
      group_by(financial_year, health_board) %>%
      summarise(
        all = sum(livebirths),
        selected_mode = sum(livebirths[delivery %in% input$mode])
      ) %>%
      ungroup() %>%
      mutate(rate_per_1000_deliveries = (selected_mode / all) * 1000)

    if (input$scotland_check) {
      HB_data <- bind_rows(Scotland_data, HB_data) %>% mutate(health_board = as_factor(health_board))
    }

    plot <-
      ggplot(
        HB_data,
        aes(x = financial_year, y = rate_per_1000_deliveries)
      ) + xlab("Financial Year") +
      ylab("Rate per 1,000 live births") +
      geom_line(aes(group = health_board, colour = health_board), size = 1) +
      labs(colour = "Health Board") +
      ylim(0, NA) +
      # Make the x labs be angled so they don't overlap
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_colour_brewer(palette = "Set1")

    plot

  })
}

# Run the application
shinyApp(ui = ui, server = server)
