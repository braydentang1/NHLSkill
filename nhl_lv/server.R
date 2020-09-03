library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyalert)
library(shinyWidgets)
library(sROC)
library(ggthemes)
library(plotly)
library(DT)
library(patchwork)

server <- function(input, output, session) {
  ### TAB 1 ###
  # If the user clicks on an element that produces one of these tags as a query, move
  # to the correct tab in the viewer.
    observe({
        query <- getUrlHash(session = getDefaultReactiveDomain())
        print(query)
        if (query == "#indiv") {
            updateTabsetPanel(session, "tabset", selected = "indiv")
        } else if (query == "#comparison") {
          updateTabsetPanel(session, "tabset", selected = "comparison")
        } else if (query == "#leaderboard") {
          updateTabsetPanel(session, "tabset", selected = "leaderboard")
        }
    })

  # Lookup all relevant scores and stats for a particular player once selected. 
  # Mostly a convenience function.
    lookup <- reactive({
    
        position <- all_players$position[which(input$player == all_players$player)]
        
        # Players who had less than 600 minutes were not modelled. Therefore
        # if a player has <600 minutes in 2020 they are considered inactive.
        if (all_players$year[which(all_players$player == input$player)] != 2020) {
          team <- "Inactive or <600 TOI 2020"
        } else {
          team <- team_lookup$team[
            which(team_lookup$accronym == all_players$team[which(input$player == all_players$player)])
            ]
        }
        
        # Brent Burns changed to defenceman and therefore,
        # has two entries. Use the D entry.
        if (input$player == "brent burns") {
            position <- "D"
        }
        
        if (position == "F") {
          
          # Get relevant data from the global variables.  
          year <- as.character(input$year_since)
          scores <- all_forwards_gte[[year]]$factor_scores
          uncertainty <- all_forwards_gte_u[[year]]
          
          # In each .rds object, there is the data set that the scores were fit on.
          # The 2014 data set is a strict superset of all other years due to the grouping/
          # hierarchichal modelling approach I took. Therefore, only need 2014 data.
          relevant_data_off <- all_forwards_gte[["2014"]]$data %>%
            filter(player == input$player, year >= input$year_since) %>%
            select(year, ops, off_gar, x_gf, scf)
          
          relevant_data_def <- all_forwards_gte[["2014"]]$data %>%
            filter(player == input$player, year >= input$year_since) %>%
            select(year, dps, def_gar, x_ga, sca)
          
          # Fits kernel smoothed CDF's to avoid "0th percentile" and "100th percentile"
          # estimates.
          empirical_dist_off <- kCDF(all_forwards_gte[[year]]$factor_scores$off_contribution)
          empirical_dist_def <- kCDF(all_forwards_gte[[year]]$factor_scores$def_contribution)
          
        } else {
          
          # Get relevant data from the global variables.  
          year <- as.character(input$year_since)
          scores <- all_defenceman_gte[[year]]$factor_scores
          uncertainty <- all_defenceman_gte_u[[year]]
          
          # In each .rds object, there is the data set that the scores were fit on.
          # The 2014 data set is a strict superset of all other years due to the grouping/
          # hierarchichal modelling approach I took. Therefore, only need 2014 data.
          relevant_data_off <- all_defenceman_gte[["2014"]]$data %>%
            filter(player == input$player, year >= input$year_since) %>%
            select(year, ops, off_gar, x_gf, scf)
          
          relevant_data_def <- all_defenceman_gte[["2014"]]$data %>%
            filter(player == input$player, year >= input$year_since) %>%
            select(year, dps, def_gar, x_ga, sca)
          
          # Fits kernel smoothed CDF's to avoid "0th percentile" and "100th percentile"
          # estimates.
          empirical_dist_off <- kCDF(all_defenceman_gte[[year]]$factor_scores$off_contribution)
          empirical_dist_def <- kCDF(all_defenceman_gte[[year]]$factor_scores$def_contribution)
        
        }
        
        # Get the fitted offensive and defensive scores precomputed.
        off_score <- scores$off_contribution[which(scores$player == input$player)]
        def_score <- scores$def_contribution[which(scores$player == input$player)]
        
        # Get the closest cutpoint of the kCDF evaluation that is larger than desired point
        percentile_off <- empirical_dist_off$Fhat[which(off_score - empirical_dist_off$x < 0)[1]]
        percentile_def <- empirical_dist_def$Fhat[which(def_score - empirical_dist_def$x < 0)[1]]
        
        desired_quantile <- (1 - input$uncertain / 100) / 2 
        
        # Find the selected player's bootstrap samples. Calculate the needed
        # quantiles for the distribution plot of the factor scores.
        bootstrap_uncertainty <- uncertainty %>%
          filter(player == input$player) %>%
          summarise(
            lower_off = quantile(off_contribution,  desired_quantile),
            upper_off = quantile(off_contribution, 1 - desired_quantile),
            lower_def = quantile(def_contribution, desired_quantile),
            upper_def = quantile(def_contribution, 1 - desired_quantile)
          )
        
        # Output all required quantities in a list.
        list(
            position = position,
            team = team,
            off_score = off_score,
            def_score = def_score,
            relevant_data_off = relevant_data_off,
            relevant_data_def = relevant_data_def,
            ecdf_off = percentile_off,
            ecdf_def = percentile_def,
            bootstrap_uncertainty = bootstrap_uncertainty
        )
        
    })

    # Offensive Score info box.
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
    
    # Defensive Score info box.
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
    
    # Display the player's name in the profile/user box.
    output$player <- renderText({
        input$player
    })
    
    # Display the player's position in the profile/user box.
    position <- reactive({
        
        if (lookup()$position == "F") {
            position <- "Forward"
        } else {
            position <-  "Defenceman"
        }
        position    
    })
    
    # The profile/user box. Needs to be done in the server file since it is 
    # generated dynamically based on user input first.
    output$profile <- renderUI({
        
      # Get the file name of the player as generated by get-player-images.py
        full_name <- str_split(input$player, pattern = " ")[[1]]
        name_id <- paste(full_name[1:2], collapse = "_")
        # Lookup the percentile the player's offensive/defensive scores sit at
        # relative to other players of the same position.
        empirical_off <- lookup()$ecdf_off
        empirical_def <- lookup()$ecdf_def
        
        # Output.
        widgetUserBox(
            title = input$player,
            subtitle = paste(position(), ", ", lookup()$team, sep = ""),
            src = paste0("images/", "players/", name_id, ".jpg"), 
            p(HTML(paste0(round(empirical_off * 100, 2), "th", " percentile offensively",
                   "<br>", round(empirical_def * 100, 2), "th", " percentile defensively"))), 
            boxToolSize = "lg",
            width = 12,
            collapsible = FALSE
        )
        
        })
    
    # The plot showing the scores over time.
    output$over_time <- renderPlotly({

        # Brent Burns changed to defenceman and therefore,
        # has two entries. Use the D entry.
        if (input$player == "brent burns") {
            position <- "D"
        } else {
            position <- lookup()$position
        }
      
        if (position == "F") {
          
          type_of_data <- all_forwards_gte
          year_filter <- input$year_since
          
        } else {

          type_of_data <- all_defenceman_gte
          year_filter <- input$year_since
        
        }
      
      
      # Iterate over the preexisiting list and create another list that finds 
      # the selected player's offensive and defensive scores. If they don't exist,
      # example: player retired, then NA.
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
        mutate(Year = as.factor(Year)) %>%
        gather(key = `Score Type`, value = Score, -Year)

      # Output plotly graph with a bunch of styling to fit the theme.
      ggplotly(ggplot(data = all_scores, aes(x = Year, y = Score, fill = `Score Type`)) +
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
      ) %>% layout(plot_bgcolor = "#39cacc") %>% 
        layout(paper_bgcolor = "#39cacc")
    })
    
    # Distribution plots of offensive and defensive scores.
    output$distribution <- renderPlotly({
      
      if (input$player == "brent burns") {
        position <- "D"
      } else {
        position <- lookup()$position
      }
      
      if (position == "F") {
        graphing_dist <- all_forwards_gte_u[[as.character(input$year_since)]] %>%
          filter(player == input$player) %>%
          mutate(
            `Offensive Score` = as.numeric(off_contribution),
            `Defensive Score` = as.numeric(def_contribution))
        
      } else {
        graphing_dist <- all_defenceman_gte_u[[as.character(input$year_since)]] %>%
          filter(player == input$player) %>%
          mutate(
            `Offensive Score` = as.numeric(off_contribution),
            `Defensive Score` = as.numeric(def_contribution)) 
      }
      
      # For the (1-alpha/2)% CI's.
      uncertainty_df <- lookup()$bootstrap_uncertainty
      
      # Offensive score distribution plot with CI.
      off_plot <- ggplot(graphing_dist, aes(x = `Offensive Score`)) +
        geom_density(fill = "#ffffff") +
        geom_vline(
          data = tibble(
            Bound = c("Estimated Offensive Score", "Lower", "Upper"),
            Value = c(round(lookup()$off_score, 2), 
                      round(uncertainty_df$lower_off, 2),
                      round(uncertainty_df$upper_off, 2))),
          aes(xintercept = Value, color = Bound)
        ) +
        scale_colour_manual(values = c("black", "red", "red")) + 
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
      
      # Defensive score distribution plot with CI.
      def_plot <- ggplot(graphing_dist, aes(x = def_contribution)) +
        geom_density(fill = "#555555") +
        geom_vline(
          data = tibble(
            Bound = c("Estimated Defensive Score", "Lower", "Upper"),
            Value = c(round(lookup()$def_score, 2), 
                      round(uncertainty_df$lower_def, 2),
                      round(uncertainty_df$upper_def, 2))),
          aes(xintercept = Value, color = Bound)
        ) +
        scale_colour_manual(values = c("black", "red", "red")) + 
        theme_minimal() + 
        labs(
          x = "Defensive Score",
          y = "Density"
        ) +
        theme(
        plot.background = element_rect(colour = "#e3e3e3", fill = "#39cacc"),
        plot.title = element_text(colour = "#555555", size = 20),
        legend.title = element_text(colour = "#555555"),
        axis.title = element_text(colour = "#555555", face = "bold"),
        plot.subtitle = element_text(colour = "#555555")
        )
      
      # Combine the two plots so that they sit beside each other as one complete plot.
      # Have to add in the title as annotations since the overall package is strange.
      subplot(ggplotly(off_plot), ggplotly(def_plot), shareX = TRUE, shareY = TRUE) %>%
        add_annotations(
          yref = "paper", 
          xref = "paper", 
          y = 1.10, 
          x = 0, 
          text = "Estimated Offensive and Defensive Score Distributions", 
          showarrow = F,
          font = list(size = 20, color = "#555555")
        ) %>%
        add_annotations(
          yref = "paper",
          xref = "paper",
          y = 1,
          x = 0,
          text = "CI's Marked in Red",
          showarrow = F,
          font = list(size = 12, color = "#555555")
        ) %>%
        layout(
          plot_bgcolor = "#39cacc",
          paper_bgcolor = "#39cacc",
          title = list(x = 0.2)) %>%
        hide_legend()
      
    })
    
    # For the README button. This controls the output of what is actually displayed
    # to the user.
    observeEvent(input$faq, {
      show_alert(
        title = "FAQ",
        text = p(HTML("• A score above (below) 0 implies that the player is above (below) average for offence/defence. A player of the same position with a higher score has more offensive/defensive contribution than a player with a lower score. <br> <br>
        • All percentiles, scores, and distributions are in relation to other players of the same position (i.e. comparing a forward against a defenceman is meaningless). <br> <br>
        • The model is hierarchical (random intercept). Therefore, players who entered the league after the year that is selected still have their scores influenced by players who came before, due to the global pooling effect. <br> <br>
        • Players who have less than 600 minutes of time on ice in 2020 are considered inactive/retired. <br> <br>
        • I purposely abbreviate CI as depending on your perspective these intervals can either be confidence or credible intervals. <br> <br>
        • All statistics in the table are centered to have zero mean and unit variance per each year and position. <br> <br>
        • For the distribution plot, pressing autoscale in the plot toolbar (upper right corner of the plot) and adjusting the y-axis placement might be useful."), style = "text-align: justify; font-size: 16px"),
        type = "info", width = "1000px"
      )
    })
    
    # To allow the user to remove inactive players from the selectInput dropdown.
    observeEvent(input$active_only, {
      
      if (input$active_only == TRUE) {
      
        # If a player did not play in 2020, they are inactive. 
        all_players_2020_only <- all_players %>%
          filter(year == 2020) %>%
          select(player) %>%
          pull(.)
      
        updateSelectInput(
          session,
          inputId = "player",
          choices = all_players_2020_only,
          selected = "sidney crosby"
        )
        
      } else {
        # Don't do anything if the box isn't checked.
        updateSelectInput(
          session,
          inputId = "player",
          choices = sort(unique(c(
            all_forwards_gte[["2014"]]$data$player,
            all_defenceman_gte[["2014"]]$data$player))),
          selected = "sidney crosby"
        )
      }
      
    })
  
    # For output tables - display the statistics that are actually producing the
    # offensive and defensive scores. Includes some JavaScript for tooltip mouse
    # hovers.
    output$key_variables_off <- renderDataTable({
      
      # This is to produce a heatmap where darker colours correspond with larger values.
      brks <- quantile(lookup()$relevant_data_off[, -1], probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs <- round(seq(202, 115, length.out = length(brks) + 1), 0) %>%
        {paste0("rgb(57", ",", ., ",", "204)")}
      
      lookup()$relevant_data_off %>%
        mutate(year = as.factor(year)) %>%
        rename(Year = year, SCF = scf, XGF = x_gf, OPS = ops, `Off. GAR` = off_gar) %>%
        mutate_if(is.numeric, .funs = list(function(x) round(x, 2))) %>%
        datatable(., options = list(lengthChange = FALSE, searching = FALSE),
                  callback =  JS("
        var tips = ['Row Number', 'Year', 'Offensive Point Share', 'Offensive Goals Above Replacement', 
        'Expected Goals For', 'Scoring Chances For'],
        header = table.columns().header();
        for (var i = 0; i < tips.length; i++) {
          $(header[i]).attr('title', tips[i]);
        }
        ")) %>%
        formatStyle(
          columns = c("OPS", "Off. GAR", "XGF", "SCF"),
          backgroundColor = styleInterval(brks, clrs),
          fontWeight = "bold",
          color = styleInterval(cuts = 0, values = c("red", "#555555"))
        )
    }, width = "100%")
    
    # Same thing as above, but for offence.
    output$key_variables_def <- DT::renderDataTable({
      
      brks <- quantile(lookup()$relevant_data_def[, -1], probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs <- round(seq(202, 115, length.out = length(brks) + 1), 0) %>%
        {paste0("rgb(57", ",", ., ",", "204)")}
      
      lookup()$relevant_data_def %>%
        mutate(year = as.factor(year)) %>%
        rename(Year = year, SCA = sca, XGA = x_ga, DPS = dps, `Def. GAR` = def_gar) %>%
        mutate_if(is.numeric, .funs = list(function(x) round(x, 2))) %>%
        datatable(., options = list(lengthChange = FALSE, searching = FALSE),
                  callback = JS("
        var tips = ['Row Number', 'Year', 'Defensive Point Share', 'Defensive Goals Above Replacement', 
        'Expected Goals Against (lower is better)', 'Scoring Chances Against (lower is better)'],
        header = table.columns().header();
        for (var i = 0; i < tips.length; i++) {
          $(header[i]).attr('title', tips[i]);
        }
        ")) %>%
        formatStyle(
          columns = c("DPS", "Def. GAR", "XGA", "SCA"),
          backgroundColor = styleInterval(brks, clrs),
          fontWeight = "bold",
          color = styleInterval(cuts = 0, values = c("red", "#555555"))
        )
    }, width = "100%")
    
    ### TAB 2 ###
    # Update player selections if the user selects active only
    
    lookup_tab2 <- reactive({
    
      if (input$for_or_def == "Forwards") {
        
        # If the user selects same player 1 and same player 2, output NA.
        if (input$player1_for == input$player2_for) {
          
          list(
            off_scores_diff = NA,
            def_scores_diff = NA,
            actual_off_diff = NA,
            actual_def_diff = NA,
            off_lt_0 = NA,
            def_lt_0 = NA,
            quantiles = NA
          )
          
        } else {
        
        # Pivot long data to wide to make it easier to compute differences        
        all_scores_uncertainty <- all_forwards_gte_u[[as.character(input$year_since_tab2)]] %>%
          filter(player %in% c(input$player1_for, input$player2_for)) %>%
          pivot_wider(id_cols = seed_number, names_from = player, values_from = c(off_contribution, def_contribution)) 
        
        # Find the actual factor scores for player 1 and player 2.
        idx_p1_orig <- which(all_forwards_gte[[as.character(input$year_since_tab2)]]$factor_scores$player == input$player1_for)
        idx_p2_orig <- which(all_forwards_gte[[as.character(input$year_since_tab2)]]$factor_scores$player == input$player2_for)
        
        # If they don't appear, then set to NA to prevent graphing anything.
        if (length(idx_p1_orig) == 0 | length(idx_p2_orig) == 0) {
          
          actual_diff_off <- NA
          actual_diff_def <- NA
          
        } else {
          
          actual_diff_off <- all_defenceman_gte[[as.character(input$year_since_tab2)]]$factor_scores$off_contribution[idx_p1_orig] -
            all_defenceman_gte[[as.character(input$year_since_tab2)]]$factor_scores$off_contribution[idx_p2_orig]
          
          actual_diff_def <- all_defenceman_gte[[as.character(input$year_since_tab2)]]$factor_scores$def_contribution[idx_p1_orig] -
            all_defenceman_gte[[as.character(input$year_since_tab2)]]$factor_scores$def_contribution[idx_p2_orig]
          
        }
        
        actual_diff_off <- all_forwards_gte[[as.character(input$year_since_tab2)]]$factor_scores$off_contribution[idx_p1_orig] -
                           all_forwards_gte[[as.character(input$year_since_tab2)]]$factor_scores$off_contribution[idx_p2_orig]
        
        actual_diff_def <- all_forwards_gte[[as.character(input$year_since_tab2)]]$factor_scores$def_contribution[idx_p1_orig] -
                           all_forwards_gte[[as.character(input$year_since_tab2)]]$factor_scores$def_contribution[idx_p2_orig]
          
        
        # Find what columns correspond to player 1 and player 2, respectively.
        idx_player1 <- str_detect(string = colnames(all_scores_uncertainty), pattern = input$player1_for)
        idx_player2 <- str_detect(string = colnames(all_scores_uncertainty), pattern = input$player2_for)
        
        # Rename the columns to reference player 1 and player 2.
        colnames(all_scores_uncertainty)[idx_player1] <- c("player1_off_contrib", "player1_def_contrib")
        colnames(all_scores_uncertainty)[idx_player2] <- c("player2_off_contrib", "player2_def_contrib")
        
        # Compute the distribution of offensive and defensive scores. Output vectors in a list.
        off_scores <- all_scores_uncertainty %>%
          select(contains("off_contrib")) %>%
          drop_na(.) %>%
          transmute(diff = as.numeric(player1_off_contrib - player2_off_contrib)) %>%
          pull(.) 
        
        def_scores <- all_scores_uncertainty %>%
          select(contains("def_contrib")) %>%
          drop_na(.) %>%
          transmute(diff = as.numeric(player1_def_contrib - player2_def_contrib)) %>%
          pull(.) 
        
        # Fix the confidence level to 90% - this is intentional.
        desired_confidence <- 0.90
        
        # Get the empirical probability that observed offensive/defensive scores are greater than 0
        # which corresponds to the probability that Player 1 Off/Def Score > Player 2 Off/Def Score.
        prob_lt_0_off <- mean(off_scores >= 0)
        prob_lt_0_def <- mean(def_scores >= 0)
        
        # Get the upper and lower bounds on the 90% CI's for both offensive and defensive 
        # scores.
        lower_off <- quantile(off_scores, (1 - desired_confidence) / 2)
        upper_off <- quantile(off_scores, (1 + desired_confidence) / 2)
        
        lower_def <- quantile(def_scores,  (1 - desired_confidence) / 2)
        upper_def <- quantile(def_scores, (1 + desired_confidence) / 2)
        
        list(
          off_scores_diff = off_scores,
          def_scores_diff = def_scores,
          actual_off_diff = actual_diff_off,
          actual_def_diff = actual_diff_def,
          off_lt_0 = prob_lt_0_off,
          def_lt_0 = prob_lt_0_def,
          quantiles = tibble(
            Bound = c(
              "Lower 90% CI Offensive",
              "Upper 90% CI Offensive",
              "Lower 90% CI Defensive",
              "Lower 90% CI Defensive"
            ),
            Value = c(lower_off, upper_off, lower_def, upper_def)
          ) %>% mutate_at(list(function(x) round(x, 2)), .vars = "Value")
        )
        }
        
      } else {
        
        # This is exactly the same thing as above, except for defenceman instead of forwards.
        
        if (input$player1_def == input$player2_def) {
          
          list(
            off_scores_diff = NA,
            def_scores_diff = NA,
            actual_off_diff = NA,
            actual_def_diff = NA,
            off_lt_0 = NA,
            def_lt_0 = NA,
            quantiles = NA
          )
          
        } else {
        
        all_scores_uncertainty <- all_defenceman_gte_u[[as.character(input$year_since_tab2)]] %>%
          filter(player %in% c(input$player1_def, input$player2_def)) %>%
          pivot_wider(id_cols = seed_number, names_from = player, values_from = c(off_contribution, def_contribution)) 
        
        idx_p1_orig <- which(all_defenceman_gte[[as.character(input$year_since_tab2)]]$factor_scores$player == input$player1_def)
        idx_p2_orig <- which(all_defenceman_gte[[as.character(input$year_since_tab2)]]$factor_scores$player == input$player2_def)
        
        if (length(idx_p1_orig) == 0 | length(idx_p2_orig) == 0) {
          
          actual_diff_off <- NA
          actual_diff_def <- NA
        
        } else {
          
          actual_diff_off <- all_defenceman_gte[[as.character(input$year_since_tab2)]]$factor_scores$off_contribution[idx_p1_orig] -
            all_defenceman_gte[[as.character(input$year_since_tab2)]]$factor_scores$off_contribution[idx_p2_orig]
          
          actual_diff_def <- all_defenceman_gte[[as.character(input$year_since_tab2)]]$factor_scores$def_contribution[idx_p1_orig] -
            all_defenceman_gte[[as.character(input$year_since_tab2)]]$factor_scores$def_contribution[idx_p2_orig]
        
        }
        
        idx_player1 <- str_detect(string = colnames(all_scores_uncertainty), pattern = input$player1_def)
        idx_player2 <- str_detect(string = colnames(all_scores_uncertainty), pattern = input$player2_def)
        
        colnames(all_scores_uncertainty)[idx_player1] <- c("player1_off_contrib", "player1_def_contrib")
        colnames(all_scores_uncertainty)[idx_player2] <- c("player2_off_contrib", "player2_def_contrib")
        
        off_scores <- all_scores_uncertainty %>%
          select(contains("off_contrib")) %>%
          drop_na(.) %>%
          transmute(diff = as.numeric(player1_off_contrib - player2_off_contrib)) %>%
          pull(.) 
        
        def_scores <- all_scores_uncertainty %>%
          select(contains("def_contrib")) %>%
          drop_na(.) %>%
          transmute(diff = as.numeric(player1_def_contrib - player2_def_contrib)) %>%
          pull(.) 
        
        
        desired_confidence <- 0.90
        prob_lt_0_off <- mean(off_scores >= 0)
        prob_lt_0_def <- mean(def_scores >= 0)
        
        lower_off <- quantile(off_scores, (1 - desired_confidence) / 2)
        upper_off <- quantile(off_scores, (1 + desired_confidence) / 2)
        
        lower_def <- quantile(def_scores,  (1 - desired_confidence) / 2)
        upper_def <- quantile(def_scores, (1 + desired_confidence) / 2)
        
        list(
          off_scores_diff = off_scores,
          def_scores_diff = def_scores,
          actual_off_diff = actual_diff_off,
          actual_def_diff = actual_diff_def,
          off_lt_0 = prob_lt_0_off,
          def_lt_0 = prob_lt_0_def,
          quantiles = tibble(
            Bound = c(
              "Lower 90% CI Offensive",
              "Upper 90% CI Offensive",
              "Lower 90% CI Defensive",
              "Lower 90% CI Defensive"
              ),
            Value = c(lower_off, upper_off, lower_def, upper_def)
          ) %>% mutate_at(.funs = list(function(x) round(x, 2)), .vars = "Value")
        )
        
        }
      }
      
    })
    
    # Updates the player list if active only is selected.
    observeEvent(input$active_only_tab2, {
      
      if (input$active_only_tab2 == TRUE) {
      
        # If a player did not play in 2020, they are inactive. 
        all_players_2020_only <- all_players %>%
          filter(year == 2020) 
        
        # Find all forwards who have >600 min TOI in 2020
        forwards <- all_players_2020_only %>%
          filter(position == "F") %>%
          select(player) %>%
          pull(.)
        
        # find all defenceman who have >600 min TOI in 2020
        defenceman <- all_players_2020_only %>%
          filter(position == "D") %>%
          select(player) %>%
          pull(.)
        
        # Update forwards
        updateSelectInput(session, "player1_for", choices = forwards, selected = "sidney crosby")
        updateSelectInput(session, "player2_for", choices = forwards, selected = "alex ovechkin")
        
        # Update defenceman
        updateSelectInput(session, "player1_def", choices = defenceman, selected = "victor hedman")
        updateSelectInput(session, "player2_def", choices = defenceman, selected = "roman josi")
      
      } else {
        
        # Do nothing if the user has selected to show all active and inactive players
        original_choices_forwards <- sort(unique(c(
          all_forwards_gte[["2014"]]$data$player)))
          
        original_choices_defenceman <- sort(unique(c(
          all_defenceman_gte[["2014"]]$data$player)))  
        
        # Forwards updated
        updateSelectInput(session, "player1_for", choices = original_choices_forwards, selected = "sidney crosby")
        updateSelectInput(session, "player2_for", choices = original_choices_forwards, selected = "alex ovechkin")
        
        # Defenceman updated
        updateSelectInput(session, "player1_def", choices = original_choices_defenceman, selected = "victor hedman")
        updateSelectInput(session, "player2_def", choices = original_choices_defenceman, selected = "roman josi")
        
      }
            
    })
    
    # Get the team for the players
    tab2_profile_data <- reactive({
      
      if (input$for_or_def == "Forwards") {
        
        relevant_players <- all_players %>%
          filter(player %in% c(input$player1_for, input$player2_for)) 
        
        which(team)
        
      }
    })
    
    output$diff_distplot <- renderPlotly({
      
      graphing <- tibble(
        `Offensive Score Difference` = lookup_tab2()$off_scores_diff,
        `Defensive Score Difference` = lookup_tab2()$def_scores_diff
        )
      
      quantiles <- lookup_tab2()$quantiles 
      
      # If the quantiles lookup results in NA, this means that the user
      # has selected the same player input for player 1 and player 2. So we 
      # create dummy frames with NA values so that errors aren't thrown.
      if (all(is.na(quantiles))) {
        
        off_quantiles <- tibble(
          Bound = c(NA, NA),
          Value = c(NA, NA)
        )
        def_quantiles <- tibble(
          Bound = c(NA, NA),
          Value = c(NA, NA)
        )
        
      } else {
        
        off_quantiles <- quantiles[1:2, ]
        def_quantiles <- quantiles[3:4, ]
  
      }
      
      # Offensive Score Difference Density
      off_diff_density <- ggplot(data = graphing, mapping = aes(x = `Offensive Score Difference`)) +
        geom_density(fill = "#ffffff") +
        geom_vline(data = off_quantiles, aes(xintercept = Value, colour = Bound), colour = "red") +
        geom_vline(
          data = tibble(
            `Observed Offensive Score Difference` = c(round(lookup_tab2()$actual_off_diff, 2))
          ),
          aes(xintercept = `Observed Offensive Score Difference`),
          color = "black"
        ) + 
        labs(y = "Density") +
        theme(
          plot.background = element_rect(colour = "#e3e3e3", fill = "#39cacc"),
          plot.title = element_text(colour = "#555555", size = 20),
          axis.title = element_text(colour = "#555555", face = "bold"),
          plot.subtitle = element_text(colour = "#555555")
        )
      
      # Defensive Score Difference Density
      def_diff_density <- ggplot(data = graphing, mapping = aes(x = `Defensive Score Difference`)) +
        geom_density(fill = "#555555") +
        geom_vline(data = def_quantiles, aes(xintercept = Value, colour = Bound), colour = "red") +
        geom_vline(
          data = tibble(
            `Observed Defensive Score Difference` = c(round(lookup_tab2()$actual_def_diff, 2))
          ),
          aes(xintercept = `Observed Defensive Score Difference`),
          color = "black"
        ) +
        labs(y = "Density") +
        theme(
          plot.background = element_rect(colour = "#e3e3e3", fill = "#39cacc"),
          plot.title = element_text(colour = "#555555", size = 20),
          axis.title = element_text(colour = "#555555", face = "bold"),
          plot.subtitle = element_text(colour = "#555555")
        )
      
      # Combine the two plots so that they sit beside each other as one complete plot.
      # Have to add in the title as annotations since the overall package is strange.
      subplot(ggplotly(off_diff_density), ggplotly(def_diff_density), shareX = TRUE, shareY = TRUE) %>%
        add_annotations(
          yref = "paper", 
          xref = "paper", 
          y = 1.05, 
          x = 0, 
          text = "Estimated Difference in Offensive and Defensive Scores", 
          showarrow = F,
          font = list(size = 20, color = "#555555")
        ) %>%
        add_annotations(
          yref = "paper",
          xref = "paper",
          y = 1,
          x = 0,
          text = "Actual Observed Difference in Black, 90% CI in Red",
          showarrow = F,
          font = list(size = 12, color = "#555555")
        ) %>%
        layout(
          plot_bgcolor = "#39cacc",
          paper_bgcolor = "#39cacc",
          title = list(x = 0.2)) %>%
        hide_legend()
      
    })
    
    # For the title display that changes based on user selection
    
    output$title_tab2 <- renderUI({
      
      if (input$for_or_def == "Forwards") {
      
      titlePanel(
        title = h2(paste(input$player1_for, "vs.", input$player2_for)) 
        )
      
      } else {
        
        titlePanel(
          title = h2(paste(input$player1_def, "vs.", input$player2_def))
        )
        
      }
    })
    
    # Info Box for the the probability Player 1 Offensive Score > Player 2 Offensive Score
    output$gte_0_off <- renderInfoBox({
      
      display_off_prob <- ifelse(length(lookup_tab2()$off_lt_0) == 0, NA, lookup_tab2()$off_lt_0)
    
      if (input$for_or_def == "Forwards") {
      
      infoBox(
        paste0("Prob. Off. Score of ", input$player1_for, " >", " Off. Score of ", input$player2_for, ":"),
        round(display_off_prob, 2),
        icon = icon("chevron-up", lib = "glyphicon"),
        color = "blue",
        fill = TRUE
      )
      
      } else {
        
        infoBox(
          paste0("Prob. Off. Score of ", input$player1_def, " >", " Off. Score of ", input$player2_def, ":"),
          round(display_off_prob, 2),
          icon = icon("chevron-up", lib = "glyphicon"),
          color = "blue",
          fill = TRUE
        )
        
      }
      
    })
    
    
    # Info Box for the the probability Player 1 Defensive Score > Player 2 Defensive Score
    output$gte_0_def <- renderInfoBox({
      
      display_def_prob <- ifelse(length(lookup_tab2()$def_lt_0) == 0, NA, lookup_tab2()$def_lt_0)
      
      if (input$for_or_def == "Forwards") {
      
      infoBox(
        paste0("Prob. Def. Score of ", input$player1_for, " >", " Def. Score of ", input$player2_for, ":"),
        round(display_def_prob, 2),
        icon = icon("tower", lib = "glyphicon"),
        color = "blue",
        fill = TRUE
      )
        
      } else {
        
        infoBox(
          paste0("Prob. Def. Score of ", input$player1_def, " >", " Def. Score of ", input$player2_def, ":"),
          round(display_def_prob, 2),
          icon = icon("tower", lib = "glyphicon"),
          color = "blue",
          fill = TRUE
        )
        
      }
      
    })
    
    ### TAB 3: Leaderboard ###
    
    # The profile/user box. Needs to be done in the server file since it is 
    # generated dynamically based on user input first.
    output$profile1_tab2 <- renderUI({
      
      if (input$for_or_def == "Forwards") {
        
        # Get the file name of the player as generated by get-player-images.py
        full_name <- str_split(input$player1_for, pattern = " ")[[1]]
        name_id <- paste(full_name[1:2], collapse = "_")
        team <- all_players$team[which(all_players$player == input$player1_for)]
        full_team_name <- team_lookup$team[which(team_lookup$accronym == team)]
        
        # Output.
        widgetUserBox(
          title = input$player1_for,
          type = 2,
          subtitle = paste(full_team_name, sep = ""),
          src = paste0("images/", "players/", name_id, ".jpg"), 
          boxToolSize = "lg",
          width = 6,
          collapsible = FALSE
        )
      
      } else {
        
        # Get the file name of the player as generated by get-player-images.py
        full_name <- str_split(input$player1_def, pattern = " ")[[1]]
        name_id <- paste(full_name[1:2], collapse = "_")
        team <- all_players$team[which(all_players$player == input$player1_def)]
        full_team_name <- team_lookup$team[which(team_lookup$accronym == team)]
        
        # Output.
        widgetUserBox(
          title = input$player1_def,
          type = 2,
          subtitle = paste(full_team_name, sep = ""),
          src = paste0("images/", "players/", name_id, ".jpg"), 
          boxToolSize = "lg",
          width = 6,
          collapsible = FALSE
        )
        
        
      }
      
    })
    
    # The profile/user box. Needs to be done in the server file since it is 
    # generated dynamically based on user input first.
    output$profile2_tab2 <- renderUI({
      
      if (input$for_or_def == "Forwards") {
        
        # Get the file name of the player as generated by get-player-images.py
        full_name <- str_split(input$player2_for, pattern = " ")[[1]]
        name_id <- paste(full_name[1:2], collapse = "_")
        team <- all_players$team[which(all_players$player == input$player2_for)]
        full_team_name <- team_lookup$team[which(team_lookup$accronym == team)]
        
        # Output.
        widgetUserBox(
          title = input$player2_for,
          type = 2,
          subtitle = paste(full_team_name, sep = ""),
          src = paste0("images/", "players/", name_id, ".jpg"), 
          boxToolSize = "lg",
          width = 6,
          collapsible = FALSE
        )
      
      } else {
        
        # Get the file name of the player as generated by get-player-images.py
        full_name <- str_split(input$player2_def, pattern = " ")[[1]]
        name_id <- paste(full_name[1:2], collapse = "_")
        team <- all_players$team[which(all_players$player == input$player2_def)]
        full_team_name <- team_lookup$team[which(team_lookup$accronym == team)]
        
        # Output.
        widgetUserBox(
          title = input$player2_def,
          type = 2,
          subtitle = paste(full_team_name, sep = ""),
          src = paste0("images/", "players/", name_id, ".jpg"), 
          boxToolSize = "lg",
          width = 6,
          collapsible = FALSE
        )
        
        
      }
      
    })
    
}