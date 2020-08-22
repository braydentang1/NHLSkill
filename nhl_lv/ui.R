library(shiny)
library(tools)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)

# Define UI for application that draws a histogram
ui <- htmlTemplate(
    "www/index.html",
    dist_plot = tabsetPanel(id = "tabset",
      tabPanel(title = "Test"),
      tabPanel(title = "Individual Players", value = "indiv",
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
    		      tags$div(title = "Scores and distributions estimated using data from this selected year to the most recent year (2020)",
    		               sliderInput("year_since",
    		                "Using Data Since:", sep = "",
    		                min = 2014, step = 1, round = TRUE,
    		                max = 2019, value = 2014)),
    					prettyCheckbox("active_only", "Active Players Only", value = FALSE, animation = "smooth", fill = FALSE),
    					actionButton("faq", "FAQ", icon = icon("info"))
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
    	      plotlyOutput("over_time", height = "250px")
    	      )
    	    ),
    	  br(),
    	  fluidRow(
    	    column(
    	      width = 12,
    	      plotlyOutput("distribution", height = "250px")
    	    )
    	   ),
    	  br(),
    	 )
      )
    )
  )
)
