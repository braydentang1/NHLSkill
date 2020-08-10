library(shiny)
library(tidyverse)
library(plotly)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observe({
        query <- getUrlHash(session = getDefaultReactiveDomain())
        print(query)
        if(query == "#indiv") {
            updateTabsetPanel(session, "tabset", selected = "indiv")
        }
    })
    
    lookup <- reactive({
    
        position <- all_players$position[which(input$player == all_players$player)]
        year <- as.character(input$year)
        
        if (position == "F") {
            
            scores <- all_forwards_gte[[year]]$factor_scores
            off_score <- scores$off_score[which(scores$player == input$player)]
            def_score <- scores$def_score[which(scores$player == input$player)]
            
        } else {
            
            scores <- all_defenceman_gte[[year]]$factor_scores
            off_score <- scores$off_score[which(scores$player == input$player)]
            def_score <- scores$def_score[which(scores$player == input$player)]
            
        }
        
        list(
            off_score = off_score,
            def_score = def_score
        )
        
    })

    output$player_off_score <- renderText({
        lookup()$off_score
    })
        
    output$player_def_score <- renderText({
        lookup()$def_score
    })
        
}
