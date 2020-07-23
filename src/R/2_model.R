library(lavaan)
library(tidyverse)

#' This function extracts the latent offensive and defensive skill of all players
#' in the data used to fit the model.
#'
#' @param model A lavaan fitted model.
#' @param position_player Either "D" or "F" for defenceman or forward, respectively.
#' @param data A tibble - the same dataset used to fit the lavaan model. 
#' 
#' @return A tibble with columns player, off_score, and def_score.
#' @export
#'
#' @examples
#' get_latent_vars(my_lavaan_model, "D", my_data)
get_latent_vars <- function(model, position_player, data) {
	
	tibble(
		player = unique(data %>% 
											filter(position == position_player) %>% 
											select(player) %>% 
											pull()),
		off_score = lavPredict(fit, level = 2)[[position_player]][, 1],
		def_score = lavPredict(fit, level = 2)[[position_player]][, 2]
		)
	
}

data <- read_rds("results/data/gte_2019.rds")

model_2019 <- '
group: F
	level: 1
		off_skill =~ off_gar_60 + x_gf_60 + scf_60_rel + total_points_60
		def_skill =~ def_gar_60 + x_ga_60 + sca_60_rel

		x_gf_60 ~~ scf_60_rel
		
	level: 2
		off_skill =~ off_gar_60 + x_gf_60 + scf_60_rel + total_points_60
		def_skill =~ def_gar_60 + x_ga_60 + sca_60_rel
		
		x_gf_60 ~~ scf_60_rel
		total_points_60 ~~ def_gar_60 + x_ga_60

group: D
		level: 1
		off_skill =~ off_gar_60 + x_gf_60 + scf_60_rel + total_points_60 
		def_skill =~ def_gar_60 + x_ga_60 + sca_60_rel 
		
		x_gf_60 ~~ scf_60_rel
		
	level: 2
		off_skill =~ off_gar_60 + x_gf_60 + scf_60_rel + total_points_60
		def_skill =~ def_gar_60 + x_ga_60 + sca_60_rel
		
		x_gf_60 ~~ scf_60_rel
		total_points_60 ~~ def_gar_60 + x_ga_60
		
'

fit <- sem(model = model, data = data, cluster = "player", group = "position")