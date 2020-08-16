library(shiny)
library(tidyverse)
library(shinydashboard)
library(ggthemes)

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
        
        # Brent Burns changed to defenceman and therefore,
        # has two entries. Use the D entry.
        if (input$player == "brent burns") {
            position <- "D"
        }
        
        if (position == "F") {
            
            if (input$gte == "Grouped") {
              year <- as.character(input$year_since)
              scores <- all_forwards_gte[[year]]$factor_scores
            } else {
              year <- as.character(input$year_indiv)
              scores <- all_forwards_indiv[[year]]$factor_scores
            }
          
        } else {
          
          if (input$gte == "Grouped") {
            year <- as.character(input$year_since)
            scores <- all_defenceman_gte[[year]]$factor_scores
          } else {
            year <- as.character(input$year_indiv)
            scores <- all_defenceman_indiv[[year]]$factor_scores
          }
        }

        off_score <- scores$off_contribution[which(scores$player == input$player)]
        def_score <- scores$def_contribution[which(scores$player == input$player)]
        
        list(
            position = position,
            off_score = off_score,
            def_score = def_score
        )
        
    })

    output$player_off_score <- renderInfoBox({
        display_off_score <- ifelse(length(lookup()$off_score) == 0, NA, lookup()$off_score)
        infoBox("Offensive \n Score", round(display_off_score, 2), icon = icon("chevron-up", lib = "glyphicon"), color = "blue", fill = TRUE)
    })
        
    output$player_def_score <- renderInfoBox({
        display_def_score <- ifelse(length(lookup()$def_score) == 0, NA, lookup()$def_score)
        infoBox("Defensive \n Score", round(display_def_score, 2), icon = icon("tower", lib = "glyphicon"), color = "blue", fill = TRUE)
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
    
    output$over_time <- renderPlot({

        # Brent Burns changed to defenceman and therefore,
        # has two entries. Use the D entry.
        if (input$player == "brent burns") {
            position <- "D"
        } else {
            position <- lookup()$position
        }
      
        if (position == "F") {
          
          if (input$gte == "Grouped") {
            type_of_data <- all_forwards_gte
            year_filter <- input$year_since
          } else {
            type_of_data <- all_forwards_indiv
            year_filter <- input$year_indiv
          }
          
        } else {

          if (input$gte == "Grouped") {
            type_of_data <- all_defenceman_gte
            year_filter <- input$year_since
          } else {
            type_of_data <- all_defenceman_indiv
            year_filter <- input$year_indiv
          }
        }
      
      all_scores <- map(type_of_data, .f = function(x) {
        
        list(
          off_scores = ifelse(
            length(x$factor_scores$off_contribution[which(x$factor_scores$player == input$player)]) == 0,
            NA,
            x$factor_scores$off_contribution[which(x$factor_scores$player == input$player)]),
          def_scores = ifelse(
            length(x$factor_scores$def_contribution[which(x$factor_scores$player == input$player)]) == 0,
            NA,
            x$factor_scores$def_contribution[which(x$factor_scores$player == input$player)])
        )
      }) %>%
        bind_rows() %>%
        bind_cols(
          year = seq(2014, 2014 + nrow(.) - 1, 1),
          .) %>%
        rename(
          Year = year,
          `Offensive Score` = off_scores,
          `Defensive Score` = def_scores) %>%
        filter(Year >= as.numeric(year_filter))

      ggplot(data = all_scores, aes(x = Year, y = `Offensive Score`)) +
                geom_hline(yintercept = 0, colour = "red", size = 1) +
                geom_line(stat = "identity", color = "white", size = 3) +
                geom_line(aes(x = Year, y = `Defensive Score`), stat = "identity", color = "#555555", alpha = 0.8, size = 3) + 
                labs(
                    title = "Scores Over Time",
                    subtitle = "Offensive Score in White, Defensive Score in Black, Red is Average",
                    x = "Using Data From Year",
                    y = "Score"
                    ) +
                theme_minimal() +
        theme(
          plot.background = element_rect(colour = "#e3e3e3", fill = "#39cacc"),
          plot.title = element_text(colour = "#555555", size = 20),
          axis.title = element_text(colour = "#555555", face = "bold"),
          plot.subtitle = element_text(colour = "#555555")
        )

    })
    
        
}
