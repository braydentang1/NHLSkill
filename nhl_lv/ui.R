library(shiny)
library(tools)
library(shinyWidgets)
library(shinyBS)

# Define UI for application that draws a histogram
ui <- htmlTemplate(
    "www/index.html",
    dist_plot = tabsetPanel(
      tabPanel(title = "Test"),
      tabPanel(title = "Individual Players",
        sidebarLayout(
    	    sidebarPanel(
    		    selectInput("player",
    								    "Player of Interest:",
    								    choices = sort(unique(c(all_forwards_gte[["2014"]]$data$player,
    								                all_defenceman_gte[["2014"]]$data$player))),
    								    selected = "sidney crosby"),
    		    prettyRadioButtons("gte", "Data Type:", selected = "Grouped", animation = "smooth", fill = TRUE, choices = c("Individual", "Grouped")),
    		    conditionalPanel("input.gte == 'Grouped'",
    		      sliderInput("year",
    		                "Using Data Since:", sep = "",
    		                min = 2014, step = 1, round = TRUE,
    		                max = 2019, value = 2014)),
    	      conditionalPanel("input.gte == 'Individual'",
    	         selectInput("year",
    	                 "Year:", choices = c("2014", "2015", "2016", "2017", "2018", "2019", "2020")))),
    	mainPanel(
    		plotOutput("distPlot", width = '500px')
    	  )
      )
    )
  )
)
