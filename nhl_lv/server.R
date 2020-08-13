library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboard)

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
            off_score <- scores$off_contribution[which(scores$player == input$player)]
            def_score <- scores$def_contribution[which(scores$player == input$player)]
            
        } else {
            
            scores <- all_defenceman_gte[[year]]$factor_scores
            off_score <- scores$off_contribution[which(scores$player == input$player)]
            def_score <- scores$def_contribution[which(scores$player == input$player)]
            
        }
        
        list(
            off_score = off_score,
            def_score = def_score
        )
        
    })

    output$player_off_score <- renderInfoBox({
        infoBox("Offensive \n Score", round(lookup()$off_score, 2), icon = icon("list"), color = "purple", fill = TRUE)
    })
        
    output$player_def_score <- renderText({
        infoBox("Defensive \n Score", round(lookup()$def_score, 2), icon = icon("list"), color = "blue", fill = TRUE)
    })
        
}
