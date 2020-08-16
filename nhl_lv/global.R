library(tidyverse)
library(shiny)

all_forwards_gte <- read_rds("results/models/gte/forwards.rds")
all_defenceman_gte <- read_rds("results/models/gte/defenceman.rds")

all_forwards_indiv <- read_rds("results/models/indiv/forwards.rds")
all_defenceman_indiv <- read_rds("results/models/indiv/defenceman.rds")

years <- seq(2014, 2019, 1)
all_forwards_gte_u <- map(years, function(x) {
	read_rds(paste0("results/bootstrap/gte/forwards/", x, ".rds")) 
					 }) 

all_defenceman_gte_u <- map(years, function(x) {
	read_rds(paste0("results/bootstrap/gte/defenceman/", x, ".rds"))
})

all_players <- all_forwards_gte[["2014"]]$factor_scores %>% 
	bind_cols(position = rep("F", nrow(all_forwards_gte[["2014"]]$factor_scores))) %>%
	bind_rows(all_defenceman_gte[["2014"]]$factor_scores) %>%
	mutate(position = ifelse(is.na(position), "D", position))

last_year_gte <- names(all_forwards_gte)[length(all_forwards_gte)] 