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
    		    prettyRadioButtons(
    		      "gte",
    		      "Data Type:", 
    		      selected = "Grouped",
    		      animation = "smooth",
    		      fill = FALSE,
    		      choices = c("Individual", "Grouped")
    		      ),
    		    conditionalPanel("input.gte == 'Grouped'",
    		      tags$div(title = "Scores and distributions estimated using data from this selected year to the most recent year (2020)",
    		               sliderInput("year_since",
    		                "Using Data Since:", sep = "",
    		                min = 2014, step = 1, round = TRUE,
    		                max = 2019, value = 2014)),
    		      ),
    	      conditionalPanel("input.gte == 'Individual'",
    	         tags$div(title = "Scores and distributions estimated using data only from this selected year",
    	                  selectInput("year_indiv",
    	                 "Year:", choices = c("2014", "2015", "2016", "2017", "2018", "2019", "2020")
    	                 )
    	               )
    	         ),
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
    	      plotOutput("over_time", height = "200px")
    	      )
    	    ),
    	  br(),
    	  conditionalPanel("input.gte == 'Grouped'",
    	  fluidRow(
    	    column(
    	      width = 12,
    	      plotOutput("distribution", height = "200px")
    	    )
    	   )
    	  )
    	 )
      )
    )
  )
)
