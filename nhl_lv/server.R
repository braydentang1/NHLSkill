library(shiny)
library(tidyverse)
library(plotly)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    data <- reactive({
        all_forwards_gte[[input$year]]$data
    })
    
    observe({
        query <- getUrlHash(session = getDefaultReactiveDomain())
        print(query)
        if(query == "#indiv"){
            updateTabsetPanel(session, "tabset", selected = "indiv")
        }
    })
    
}
