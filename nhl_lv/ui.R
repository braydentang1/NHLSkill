library(shiny)
library(shinyWidgets)

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
    								    choices = unique(c(all_forwards_gte[["2014"]]$data$player,
    								                all_defenceman_gte[["2014"]]$data$player)),
    								    selected = "sidney crosby"),
    		    sliderInput("year",
    		                "Using Data Since:", sep = "",
    		                min = 2014, step = 1, round = TRUE,
    		                max = 2019, value = 2014),
    		    radioButtons("gte", "Data Type:", choiceNames = list(
    		      HTML("<input type='radio' id='radio-alpha' name='radio' checked>
												<label for='radio-alpha'>Individual Years</label>"),
    		      HTML("<input type='radio' id='radio-beta' name='radio' checked>
												<label for='radio-beta'>Grouped</label>")),
    		      choiceValues = list("Individual", "Grouped"))
    	    ),
    	mainPanel(
    		plotOutput("distPlot", width = '500px')
    	  )
      )
    )
  )
)
