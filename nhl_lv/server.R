library(shiny)
library(tidyverse)
library(shinydashboard)
library(ggthemes)
library(patchwork)

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
        
        if (all_players$year[which(all_players$player == input$player)] != 2020) {
          team <- "Inactive/Retired"
        } else {
          team <- team_lookup$team[which(team_lookup$accronym == all_players$team[which(input$player == all_players$player)])]
        }
        
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
            team = team,
            off_score = off_score,
            def_score = def_score
        )
        
    })

    output$player_off_score <- renderInfoBox({
        display_off_score <- ifelse(length(lookup()$off_score) == 0, NA, lookup()$off_score)
        infoBox(
          "Offensive \n Score",
          round(display_off_score, 2),
          icon = icon("chevron-up", lib = "glyphicon"),
          color = "blue",
          fill = TRUE
          )
    })
        
    output$player_def_score <- renderInfoBox({
        display_def_score <- ifelse(length(lookup()$def_score) == 0, NA, lookup()$def_score)
        infoBox(
            "Defensive \n Score",
            round(display_def_score, 2),
            icon = icon("tower", lib = "glyphicon"),
            color = "blue",
            fill = TRUE
            )
      
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
        
        full_name <- str_split(input$player, pattern = " ")[[1]]
        name_id <- paste(full_name[1:2], collapse = "_")
        
        widgetUserBox(
            title = input$player,
            subtitle = paste(position(), ", ", lookup()$team, sep = ""),
            src = paste0("images/", "players/", name_id, ".jpg"), 
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
            # If individual, just don't filter - no point.
            year_filter <- 2014
          }
          
        } else {

          if (input$gte == "Grouped") {
            type_of_data <- all_defenceman_gte
            year_filter <- input$year_since
          } else {
            type_of_data <- all_defenceman_indiv
            # If individual, just don't filter - no point.
            year_filter <- 2014
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
        filter(Year >= as.numeric(year_filter)) %>%
        gather(key = `Score Type`, value = Score, -Year)

      ggplot(data = all_scores, aes(x = as.factor(Year), y = Score, fill = `Score Type`)) +
                geom_hline(yintercept = 0, colour = "red", size = 1) +
                geom_bar(stat = "identity", position = "dodge2") +
                labs(
                    title = "Scores Over Time",
                    subtitle = "Red is Average",
                    x = ifelse(input$gte == "Grouped", "Using Data Since", "Year"),
                    y = "Score"
                    ) +
        scale_fill_manual(values = c("#555555", "#ffffff")) +
        theme_minimal() +
        theme(
          plot.background = element_rect(colour = "#e3e3e3", fill = "#39cacc"),
          plot.title = element_text(colour = "#555555", size = 20),
          axis.title = element_text(colour = "#555555", face = "bold"),
          plot.subtitle = element_text(colour = "#555555")
        )

    })
    
    output$distribution <- renderPlot({
      
      if (input$player == "brent burns") {
        position <- "D"
      } else {
        position <- lookup()$position
      }
      
      if (position == "F") {
        graphing_dist <- all_forwards_gte_u[[as.character(input$year_since)]] %>%
          filter(player == input$player) %>%
          mutate(
            off_contribution = as.numeric(off_contribution),
            def_contribution = as.numeric(def_contribution))
        
      } else {
        graphing_dist <- all_defenceman_gte_u[[as.character(input$year_since)]] %>%
          filter(player == input$player) %>%
          mutate(
            off_contribution = as.numeric(off_contribution),
            def_contribution = as.numeric(def_contribution)) 
      }
    
      off_plot <- ggplot(graphing_dist, aes(x = off_contribution)) +
        geom_density(fill = "#ffffff") +
        geom_vline(xintercept = lookup()$off_score) + 
        theme_minimal() + 
        labs(
          x = "Offensive Score",
          y = "Density"
        ) + 
        theme(
          plot.background = element_rect(colour = "#e3e3e3", fill = "#39cacc"),
          plot.title = element_text(colour = "#555555", size = 20),
          axis.title = element_text(colour = "#555555", face = "bold"),
          plot.subtitle = element_text(colour = "#555555")
        )
      
      def_plot <- ggplot(graphing_dist, aes(x = def_contribution)) +
        geom_density(fill = "#555555") +
        geom_vline(xintercept = lookup()$def_score) +
        theme_minimal() + 
        labs(
          x = "Defensive Score",
          y = "Density"
        ) +
        theme(
        plot.background = element_rect(colour = "#e3e3e3", fill = "#39cacc"),
        plot.title = element_text(colour = "#555555", size = 20),
        axis.title = element_text(colour = "#555555", face = "bold"),
        plot.subtitle = element_text(colour = "#555555")
        )
      
      off_plot + def_plot + plot_annotation(
        title = "Estimated Score Distributions"
      ) & theme_minimal() & theme(
        plot.background = element_rect(colour = "#e3e3e3", fill = "#39cacc"),
        plot.title = element_text(colour = "#555555", size = 20, hjust = 0.055),
        axis.title = element_text(colour = "#555555", face = "bold"),
        plot.subtitle = element_text(colour = "#555555")
      )
      
    })
  
  }