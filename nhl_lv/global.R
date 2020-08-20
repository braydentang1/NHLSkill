library(shiny)
library(tools)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)

all_forwards_gte <- read_rds("results/models/gte/forwards.rds")
all_defenceman_gte <- read_rds("results/models/gte/defenceman.rds")

all_forwards_indiv <- read_rds("results/models/indiv/forwards.rds")
all_defenceman_indiv <- read_rds("results/models/indiv/defenceman.rds")

years <- seq(2014, 2019, 1)
all_forwards_gte_u <- map(years, function(x) {
	read_rds(paste0("results/bootstrap/gte/forwards/", x, ".rds")) 
					 }) %>%
	set_names(as.character(years))

all_defenceman_gte_u <- map(years, function(x) {
	read_rds(paste0("results/bootstrap/gte/defenceman/", x, ".rds"))
}) %>%
	set_names(as.character(years))

all_players <- all_forwards_gte[["2014"]]$factor_scores %>% 
	bind_cols(position = rep("F", nrow(all_forwards_gte[["2014"]]$factor_scores))) %>%
	bind_rows(all_defenceman_gte[["2014"]]$factor_scores) %>%
	mutate(position = ifelse(is.na(position), "D", position)) %>%
	left_join(., all_forwards_gte[["2014"]]$data %>% 
							bind_rows(., all_defenceman_gte[["2014"]]$data) %>%
							group_by(player) %>%
							filter(year == max(year)) %>%
							select(player, team, year), by = "player") 

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

last_year_gte <- names(all_forwards_gte)[length(all_forwards_gte)] 