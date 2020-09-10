library(lavaan)
library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyalert)
library(shinyWidgets)
library(shinyjs)
library(sROC)
library(ggthemes)
library(plotly)
library(DT)

# Uses a html file as a template. Defines other HTML classes that can be
# called directly in the HTML file to place the R Shiny object where needed on the page.
ui <- fluidPage(
    useShinyjs(),
    div(
      id = "loading_page",
      img(src = "images/loader.gif"),
      h1("Loading..."),
      style = "text-align: center; margin-top: 50px"
    ),
    hidden(
      div(
          id = "main_content",
          htmlTemplate(
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
          					  all_forwards_gte[["2015"]]$data$player,
          					  all_defenceman_gte[["2015"]]$data$player))),
          					  selected = "sidney crosby"
          					),
          					prettyCheckbox("active_only", "Active Players Only", value = TRUE, animation = "smooth", fill = FALSE),
          		      tags$div(title = "Scores and distributions estimated using data from this selected year to the most recent year (2020)",
          		               sliderInput("year_since",
          		                "Using Data Since:", sep = "",
          		                min = 2015, step = 1, round = TRUE,
          		                max = 2019, value = 2015)),
          					tags$div(title = "For controlling the intervals (red bars) in the distribution plots", 
          					         sliderInput("uncertain", "Uncertainty Level:", min = 80, max = 99, step = 1, value = 90)),
          					tags$div(title = "For more information and guidelines",
          					         actionButton("faq", "README", icon = icon("info")))
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
                prettyRadioButtons("for_or_def", label = "Position:", choices = c("Forwards", "Defencemen"), selected = "Forwards"),
                prettyCheckbox("active_only_tab2", "Active Players Only:", value = TRUE, animation = "smooth", fill = FALSE),
                sliderInput("year_since_tab2", label = "Using Data Since:", min = 2015, max = 2019, step = 1, sep = "", value = 2015, round = TRUE),
                conditionalPanel("input.for_or_def == 'Forwards'",
                  selectInput("player1_for", label = "Player 1:", choices = sort(unique(c(
                    all_forwards_gte[["2015"]]$data$player))),
                    selected = "sidney crosby", multiple = FALSE),
                    selectInput("player2_for", label = "Player 2:", choices = sort(unique(c(
                    all_forwards_gte[["2015"]]$data$player))),
                    selected = "alex ovechkin", multiple = FALSE)
                ),
                conditionalPanel("input.for_or_def == 'Defencemen'",
                  selectInput("player1_def", label = "Player 1:", choices = sort(unique(c(
                    all_defenceman_gte[["2015"]]$data$player))),
                    selected = "victor hedman", multiple = FALSE),
                    selectInput("player2_def", label = "Player 2:", choices = sort(unique(c(
                    all_defenceman_gte[["2015"]]$data$player))),
                    selected = "roman josi", multiple = FALSE)
            ),
            tags$div(title = "For more information and guidelines",
                     actionButton("faq_tab2", "README", icon = icon("info")))
          ),
          mainPanel(
                fluidRow(
                  column(
                  uiOutput("title_tab2"),
                  width = 12
                  )
                ),
                fluidRow(
                  column(
                  uiOutput("profile1_tab2"),
                  uiOutput("profile2_tab2"),
                  width = 12)
                ),
                fluidRow(
                  column(
                  width = 12,
                  plotlyOutput("diff_distplot", height = "475px")
                  )
                ),
                br(),
                fluidRow(
                  column(
                    width = 12,
                    align = "center",
                    tags$div(title = "This is the probability that the difference in offensive scores is greater than 0 (favoring player 1)",
                            infoBoxOutput("gte_0_off", width = 12))),
                  column(
                    width = 12,
                    align = "center",
                    tags$div(title = "This is the probability that the difference in defensive scores is greater than 0 (favoring player 1)",
                            infoBoxOutput("gte_0_def", width = 12)))
                  )
              )
            )
          ),
          
          # Tab Panel #3: Leaderboard
          
          tabPanel(
            title = "Leaderboard",
            value = "leaderboard", 
            sidebarLayout(
              sidebarPanel(
                sliderInput(
                  "year_since_lb",
                  label = "Using Data Since:",
                  min = 2015,
                  max = 2019,
                  step = 1,
                  sep = "",
                  value = 2015,
                  round = TRUE
                ),
                prettyRadioButtons(
                  "for_or_def_lb",
                  label = "Position:",
                  choices = c("Forwards", "Defencemen"),
                  selected = "Forwards"
                  ),
                prettyCheckbox(
                  "active_only_lb",
                  "Active Players Only",
                  value = TRUE,
                  animation = "smooth",
                  fill = FALSE
                )
              ),
              mainPanel(
                DT::dataTableOutput("leaderboard"),
                br()
              )
            )
          )
        )
      )
    )
  )
)