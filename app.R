#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("code.R")

# Define UI for application that draws a histogram
ui <- fluidPage(# Show a plot of the generated distribution
  mainPanel(
    selectInput(
      "mode",
      "mode of delivery",
      distinct(data, delivery),
      selected = "Spontaneous",
      multiple = TRUE
    ),
    selectInput(
      "induced",
      "Induction type",
      distinct(data, induced),
      selected = "Not Induced",
      multiple = TRUE
    ),
    selectInput(
      "hb",
      "Health Board(s)",
      c("Scotland", distinct(data, hb2014)),
      selected = "Scotland",
      multiple = TRUE
    ),
    plotOutput("linePlot")
  ))



# Define server logic required to draw a histogram
server <- function(input, output) {
  output$linePlot <- renderPlot({
    HB_data <- data %>%
      filter(hb2014 %in% input$hb) %>%
      filter(induced %in% input$induced) %>%
      group_by(financial_year, hb2014) %>%
      summarise(all = sum(livebirths),
                selected_mode = sum(livebirths[delivery %in% input$mode])) %>%
      ungroup() %>%
      mutate(rate_per_100_deliveries = (selected_mode / all) * 100)
    
    Scotland_data <- data %>%
      filter(induced %in% input$induced) %>%
      mutate(hb2014 = "Scotland") %>%
      group_by(financial_year, hb2014) %>%
      summarise(all = sum(livebirths),
                selected_mode = sum(livebirths[delivery %in% input$mode])) %>%
      ungroup() %>%
      mutate(rate_per_100_deliveries = (selected_mode / all) * 100)
    
    HB_data <- HB_data %>% bind_rows(Scotland_data)
    
    plot <-
      ggplot(HB_data,
             aes(x =  financial_year, y = rate_per_100_deliveries))
    
    plot + xlab("Financial Year") + ylab("Rate per 100 live births") +
      geom_line(aes(group = hb2014, colour = hb2014), size = 1) +
      labs(colour = "Health Board") + 
      theme_minimal() + 
      scale_colour_brewer(palette = "Accent")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
