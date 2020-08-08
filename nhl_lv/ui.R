library(shiny)

# Define UI for application that draws a histogram
ui <- htmlTemplate(
    "www/index.html",
    dist_plot = sidebarLayout(
    	sidebarPanel(
    		sliderInput("bins",
    								"Number of bins:",
    								min = 1,
    								max = 50,
    								value = 30)
    	),
    	
    	# Show a plot of the generated distribution
    	mainPanel(
    		plotOutput("distPlot", width = '1000px')
    	)
    )
)
