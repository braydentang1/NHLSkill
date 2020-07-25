library(lavaan)
library(tidyverse)

#' This function extracts the latent offensive and defensive skill of all players
#' in the data used to fit the model.
#'
#' @param model A lavaan fitted model.
#' @return A tibble with columns player, off_score, and def_score.
#' @export
#'
#' @examples
#' get_latent_vars(my_lavaan_model)
get_latent_vars <- function(model) {
	
	tibble(
		player = model@Data@Lp[[1]]$cluster.id[[2]],
		off_score = lavPredict(model, level = 2)[, 1],
		def_score = lavPredict(model, level = 2)[, 2]
		)
	
}

#' Fit a lavvaan model with a given model code on data from a specific year.
#'
#' @param year An integer year describing what data to fit the SEM on. 
#' Must be 2014, 2015, 2016, 2017, 2018, or 2019.
#' @param model_code A string describing a structural equation model.
#'
#' @return A lavaan fitted model.
#' @export
#'
#' @examples
#' fit_model_year(2019, my_model_code)
fit_model_year <- function(year, model_code) {
	
	data <- read_rds(paste0("results/data/gte_", year, ".rds")) %>%
		filter(position == "F")
	
	model <- sem(model = model_code, data = data, cluster = "player")
	
	model
	
}


# MOVE THESE TO SEPARATE TEXT FILES!!!! THIS IS THE BEST 2019 MODEL.

model_2019 <- '
level: 1
	off_skill =~ off_gar + x_gf + scf + total_points
	def_skill =~ def_gar + x_ga
	
	x_gf ~~ scf
	off_gar ~~ x_gf
	
level: 2
	off_skill =~ off_gar + x_gf + scf + total_points
	def_skill =~ def_gar + x_ga
	
	x_gf ~~ scf
	
'

my_model <- fit_model_year(2019, model_2019)
modIndex <- modindices(my_model)
test <- get_latent_vars(my_model)
AIC(my_model)