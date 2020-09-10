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


load_data <- function() {
	Sys.sleep(45)
	hide("loading_page")
	show("main_content")
}

# Read in precomputed forward and defenceman hierarchical models.
all_forwards_gte <- read_rds("results/models/gte/forwards.rds")
all_defenceman_gte <- read_rds("results/models/gte/defenceman.rds")

# Define the years of the data that the models were fit on. Note: these
# are "greater than" cutoffs. So, 2015 means "using data > 2015", 2019 means 
# "using data > 2019 = 2019, 2020", and so on.
years <- seq(2015, 2019, 1)

# Iterate to read in the .rds files for the uncertainty measurements.
all_forwards_gte_u <- map(years, function(x) {
	read_rds(paste0("results/bootstrap/gte/forwards/", x, ".rds")) 
					 }) %>%
	set_names(as.character(years))

all_defenceman_gte_u <- map(years, function(x) {
	read_rds(paste0("results/bootstrap/gte/defenceman/", x, ".rds"))
}) %>%
	set_names(as.character(years))

# Get a list of all of the players that can be viewed for the dropdown menu.
# 2015 is a strict superset of all other years.
# Therefore, we only need to look at the oldest data (2015).
all_players <- all_forwards_gte[["2015"]]$factor_scores %>% 
	bind_cols(position = rep("F", nrow(all_forwards_gte[["2015"]]$factor_scores))) %>%
	bind_rows(all_defenceman_gte[["2015"]]$factor_scores) %>%
	mutate(position = ifelse(is.na(position), "D", position)) %>%
	left_join(., all_forwards_gte[["2015"]]$data %>% 
							bind_rows(., all_defenceman_gte[["2015"]]$data) %>%
							group_by(player) %>%
							filter(year == max(year)) %>%
							select(player, team, year), by = "player") 

# For presentation in the user profile/player box. This is to show the actual
# full team name instead of some accronym.
team_lookup <- tibble(
	accronym = unique(all_players$team),
	team = c(
		"Philadelphia Flyers",
		"Anaheim Ducks",
		"Florida Panthers",
		"Dallas Stars",
		"Edmonton Oilers",
		"Arizona Coyotes",
		"Tampa Bay Lightning",
		"Washington Capitals",
		"Carolina Hurricanes",
		"Ottawa Senators",
		"Chicago Blackhawks",
		"New York Islanders",
		"Montreal Canadiens",
		"Vancouver Canucks",
		"Los Angeles Kings",
		"Buffalo Sabres",
		"Winnipeg Jets",
		"Columbus Blue Jackets",
		"Toronto Maple Leafs",
		"Boston Bruins",
		"Detroit Red Wings",
		"St. Louis Blues",
		"San Jose Sharks",
		"New Jersey Devils",
		"New York Rangers",
		"Minnesota Wild",
		"Vegas Golden Knights",
		"Colorado Avalanche",
		"Pittsburgh Penguins",
		"Nashville Predators",
		"Calgary Flames")
) 

# Need the last year for the sliderInput.
last_year_gte <- names(all_forwards_gte)[length(all_forwards_gte)] 