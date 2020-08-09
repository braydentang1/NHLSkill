library(shiny)
library(tidyverse)
library(plotly)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    data <- reactive({
        all_forwards_gte[[input$year]]$data
    })
    
}
