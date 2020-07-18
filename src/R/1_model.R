library(tidyverse)
library(lavaan)
library(docopt)

# Read in Evolving Hockey Data

read_data <- function(site, directory) {
	
	all_files <- list.files(paste("data", site, directory, sep = "/"))
	
	all_data <- map(all_files, .f = function(x) read_csv(paste("data", site, directory, x, sep = "/"), na = c("-")) %>%
										janitor::clean_names() %>%
										mutate(year = as.numeric(rep(str_extract(x, "[^.]*")))) %>%
										relocate(year, after = season))
	
	bind_rows(all_data)
	
}

all_GAR <- read_data("eh", "gar")	%>%
	select(player, year, team, position, toi_all, war_60) 

all_xgar <- read_data("eh", "xgar") %>%
	select(player, year,)

select(-X1) %>%
	janitor::clean_names() %>%
	mutate(game_score_60 = 0.75 * goals_60 + 0.7 * first_assists_60 + 0.55 * second_assists_60 
				 + 0.075 * shots_60 + 0.05 * shots_blocked_60 + 0.15 * penalties_drawn_60 - 0.15 * total_penalties_60 
				 + 0.01 * faceoffs_won_60 - 0.01 * faceoffs_lost_60 + 0.05 * i_cf_60)
	