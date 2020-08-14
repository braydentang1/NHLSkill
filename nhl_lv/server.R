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
        year <- as.character(input$year_since)
        
        # Brent Burns changed to D and has to entries. Use the D entry.
        if (input$player == "brent burns") {
            position <- "D"
        }
        
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
            position = position,
            off_score = off_score,
            def_score = def_score
        )
        
    })

    output$player_off_score <- renderInfoBox({
        infoBox("Offensive \n Score", round(lookup()$off_score, 2), icon = icon("chevron-up", lib = "glyphicon"), color = "blue", fill = TRUE)
    })
        
    output$player_def_score <- renderInfoBox({
        infoBox("Defensive \n Score", round(lookup()$def_score, 2), icon = icon("tower", lib = "glyphicon"), color = "blue", fill = TRUE)
    })
    
    output$player <- renderText({
        input$player
    })
    
    position <- reactive({
        
        if (lookup()$position == "F") {
            position <- "Forward"
        } else {
            position <-  "Defenceman"
        }
        position    
    })
    
    output$profile <- renderUI({
        
        last_name <- str_split_fixed(input$player, pattern = " ", n = 2)[2]
        
        widgetUserBox(
            title = input$player,
            subtitle = position(),
            src = paste0("images/", "players/", last_name, ".jpg"), 
            boxToolSize = "lg",
            width = 12,
            collapsible = FALSE
        )
        
        })
    
    output$over_time <- renderPlotly({
        
        if (lookup()$position == "F") {
            
            all_scores <- map(all_forwards_gte, .f = function(x) {
                
                list(
                    off_scores = x$factor_scores$off_contribution[which(x$factor_scores$player == input$player)],
                    def_scores = x$factor_scores$def_contribution[which(x$factor_scores$player == input$player)] 
                )
            }) %>%
                bind_rows() %>%
                bind_cols(
                    year = seq(as.numeric(input$year), as.numeric(input$year) + nrow(.) - 1, 1),
                    .)
            
            
        
        }
        
        all_scores <- map()
        
    })
        
}
