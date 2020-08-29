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

# Uses a html file as a template. Defines other HTML classes that can be
# called directly in the HTML file to place the R Shiny object where needed on the page.
ui <- htmlTemplate(
    "www/index.html",
    dist_plot = tabsetPanel(id = "tabset",
                            
                            
      # Tab Panel: Individual Players
      tabPanel(
        title = "Individual Players",
        value = "indiv",
        sidebarLayout(
    	    sidebarPanel(
    		    selectInput(
    		      "player",
    					"Player of Interest:",
    					choices = sort(unique(c(
    					  all_forwards_gte[["2014"]]$data$player,
    					  all_defenceman_gte[["2014"]]$data$player))),
    					  selected = "sidney crosby"
    					),
    					prettyCheckbox("active_only", "Active Players Only", value = FALSE, animation = "smooth", fill = FALSE),
    		      tags$div(title = "Scores and distributions estimated using data from this selected year to the most recent year (2020)",
    		               sliderInput("year_since",
    		                "Using Data Since:", sep = "",
    		                min = 2014, step = 1, round = TRUE,
    		                max = 2019, value = 2014)),
    					tags$div(title = "For controlling the intervals (red bars) in the distribution plots", 
    					         sliderInput("uncertain", "Uncertainty Level:", min = 80, max = 99, step = 1, value = 90)),
    					actionButton("faq", "README", icon = icon("info")),
    	       ),
    	mainPanel(
    	  fluidRow(
    	    column(
    	      width = 12,
    	      uiOutput("profile")
    	      )
    	    ),
    	  fluidRow(
    	    column(
    	      width = 12,
    		    tags$div(title = "Estimated in relation to players of the same position",
    		             infoBoxOutput("player_off_score", width = 6)),
    		    tags$div(title = "Estimated in relation to players of the same position",
    		             infoBoxOutput("player_def_score", width = 6))
    	      )
    	    ),
    	  fluidRow(
    	    column(
    	      width = 12, 
    	      plotlyOutput("over_time", height = "280px")
    	      )
    	    ),
    	  br(),
    	  fluidRow(
    	    column(
    	      width = 12,
    	      plotlyOutput("distribution", height = "280px")
    	    )
    	   ),
    	  br(),
    	  fluidRow(
    	    column(
    	      width = 12,
    	      tabsetPanel(
    	        id = "tables",
    	        tabPanel(title = "Offensive Metrics", value = "off_metrics", 
    	                 DT::dataTableOutput("key_variables_off")),
    	        tabPanel(title = "Defensive Metrics", value = "def_metrics",
    	                 DT::dataTableOutput("key_variables_def")))
    	    )
    	  ),
    	  br()
    	 )
      )
    ),
    
    # Tab Panel #2: Player Comparison Tool
    tabPanel(
      title = "Player Comparison",
      value = "comparison",
      sidebarLayout(
        sidebarPanel(
          prettyRadioButtons("for_or_def", label = "Position", choices = c("Forwards", "Defencemen"), selected = "Forwards"),
          conditionalPanel("input.for_or_def == 'Forwards'",
            selectInput("player1", label = "Player 1:", choices = sort(unique(c(
              all_forwards_gte[["2014"]]$data$player,
              all_defenceman_gte[["2014"]]$data$player))),
              selected = "sidney crosby"),
            selectInput("player2", label = "Player 2:", choices = sort(unique(c(
              all_forwards_gte[["2014"]]$data$player,
              all_defenceman_gte[["2014"]]$data$player))),
              selected = "alex ovechkin"),
          conditionalPanel("input.for_or_def == 'Defencemen'",
            selectInput("player2", label = "Player 2:", choices = sort(unique(c(
              all_forwards_gte[["2014"]]$data$player,
              all_defenceman_gte[["2014"]]$data$player))),
              selected = "alex ovechkin"
          )
        )
      )
    ),
    mainPanel(
          
        )
      )
    )
  )
)
