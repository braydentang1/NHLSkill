library(tidyverse)
library(shiny)

all_forwards_gte <- read_rds("results/models/gte/forwards.rds")
all_defenceman_gte <- read_rds("results/models/gte/defenceman.rds")

all_players <- all_forwards_gte[["2014"]]$factor_scores %>% 
	bind_cols(position = rep("F", nrow(all_forwards_gte[["2014"]]$factor_scores))) %>%
	bind_rows(all_defenceman_gte[["2014"]]$factor_scores) %>%
	mutate(position = ifelse(is.na(position), "D", position))

last_year_gte <- names(all_forwards_gte)[length(all_forwards_gte)] 