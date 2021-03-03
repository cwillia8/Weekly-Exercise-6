library(shiny)
library(tidyverse)
library(rsconnect)
covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
ui <- fluidPage(selectInput(inputId = "state",
                            label = "Select State(s):",
                            choices = covid19 %>% distinct(state) %>% pull(state),
                            multiple = TRUE),
                submitButton(text = "Create your plot!"),
                plotOutput(outputId = "covidtrajectory"))

server <- function(input, output) {
  output$covidtrajectory <- renderPlot(
    covid19 %>% 
      filter(cases >= 20) %>% 
      group_by(state) %>% 
      mutate(days_since_20 = date - min(date)) %>% 
      filter(state %in% input$state) %>% 
      ggplot(aes(x = days_since_20,
                 y = cases,
                 color = state)) +
      geom_line() + 
      scale_y_log10() + 
      labs(title = "Total Cumulative Cases",
           y = "",
           x = "Days Since Case Count Reached 20",
           caption = "Made By: Caleb Williams") +
      theme_minimal()
      
       )
}
shinyApp(ui = ui, server = server)