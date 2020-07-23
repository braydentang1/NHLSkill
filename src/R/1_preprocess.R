"This script preprocesses the data into a usable format for structural equation modelling.
A variety of variables are selected that are commonly used for assessing the profile of a player,
but not all of the variables are actually used. The resulting data is centered and scaled. This 
script assumes it will be run from the root of this repository.

Usage: 1_preprocess.R --gte_years=<gte_years> --raw_data_path=<raw_data_path> --processed_out=<processed_out>

Options:
--gte_years=<gte_years> A string of years from which data from this season and onwards will be grabbed. Example: 2011,2012,2013
--raw_data_path=<raw_data_path> A file path that describes where all ofthe raw data is stored (the root), as a result of running 1_get-data_X.py
--processed_out=<processed_out> A file path that describes where to store the processed data.
" -> doc

library(tidyverse)
library(recipes)
library(docopt)

opt <- docopt(doc)

#' Read the saved Evolving Hockey Data - requires a Patreon account. Do not push
#' this data since you have to pay for it.
#'
#' @param data_path A file path that describes where all the raw EH data is being stored, relative to the root
#' directory.
#' @param directory A file path that describes where all of the .csv files are for a particular scenario. This
#' should be a relative path from data_path. 
#'
#' @return A tibble of all of the .csv files combined.
#' @export
#'
#' @examples
#' read_data_eh("data", "gar")
read_data_eh <- function(data_path, directory) {
	
	all_files <- list.files(paste(data_path, "eh", directory, sep = "/"))
	
	all_data <- map(
		all_files, .f = function(x) read_csv(paste(data_path, "eh", directory, x, sep = "/"), na = c("-")) %>%
										janitor::clean_names() %>%
										mutate(year = as.numeric(rep(str_extract(x, "[^.]*")))) %>%
										relocate(year, after = season))
	
	bind_rows(all_data)
	
}

#' Read the saved NaturalStatTrick data.
#'
#' @param data_path A file path that describes where all the raw data is being stored, relative to the
#' root directory.
#' @param directory A file path that describes where the saved NaturalStatTrick .csv files are. This should be
#' a relative path from data_path.
#'
#' @return A tibble of all of the .csv files combined.
#' @export
#'
#' @examples
#' read_data_nst("data", "on-ice-rel")
read_data_nst <- function(data_path, directory) {
	
	all_files <- list.files(paste(data_path, "nst", directory, sep = "/"))
	
	all_data <- map(
		all_files, .f = function(x) read_csv(paste(data_path, "nst", directory, x, sep = "/"), na = c("-")) %>%
										janitor::clean_names() %>%
										mutate(year = as.numeric(rep(str_extract(x, "[^_]*")))) %>%
										select(-x1) %>%
										relocate(year, after = player))
	
	bind_rows(all_data)
	
}

#' Finds names based on fuzzy matching - fixes inconsistencies between NaturalStatTrick
#' and Evolving Hockey.
#'
#' @param player The name of a player as provided by Evolving Hockey.
#' @param match_vector A character vector of player names from NaturalStatTrick.
#'
#' @return A character vector of length one representing the name of a player that
#' is hopefully the same person as player.
#' @export
#'
#' @examples 
#' find_names("mitch marner", vector_of_nst_names)
find_names <- function(player, match_vector) {
	
	if (player == "colin white (c)") {
		"colin white"
	} else if (player == "erik gustafsson (d)") {
		"erik gustafsson"
	} else {
		agrep(player, tolower(match_vector), value = TRUE, max = 3)[1]
	}
	
}

main <- function(gte_years, raw_data_path, processed_out) {

	# Read Evolving Hockey GAR data
	all_GAR <- read_data_eh(raw_data_path, "gar")	%>%
		select(player, year, team, position, toi_all, war_60, off_gar_60, def_gar_60) %>%
		filter(year != 2013)
	
	# Read Evolving Hockey RAPM data
	all_rapm <- read_data_eh(raw_data_path, "rapm") %>%
		select(player, year, x_gf_60, x_ga_60, cf_60, ca_60, gf_60, ga_60) %>%
		filter(year != 2013)
	
	# Bind all Evolving Hockey data together
	all_eh <- left_join(all_GAR, all_rapm, by = c("player", "year")) %>%
		mutate(player = tolower(player))
	
	# Read in Individual Natural Stat Trick (NST) data
	all_nst_individual <- read_data_nst(raw_data_path, "individual") %>%
		select(
			player, year, goals_60, first_assists_60, second_assists_60,
			total_points_60, shots_60, shots_blocked_60, penalties_drawn_60,
			total_penalties_60, faceoffs_won_60, faceoffs_lost_60, i_cf_60
			) 
	
	# Read in On Ice Relative Natural Stat Trick data
	all_nst_relative <- read_data_nst(raw_data_path, "on-ice-rel") %>%
		select(
			player, year, hdcf_60_rel, hdca_60_rel, off_zone_starts_60,
			def_zone_starts_60, scf_60_rel, sca_60_rel
			)
	
	# Read in Natural Stat Trick power play data
	all_nst_pp <- read_data_nst(raw_data_path, "powerplay") %>%
		select(player, year, toi_gp) %>%
		rename(toi_gp_pp = toi_gp) 
	
	# Read in Natural Stat Trick penalty kill data
	all_nst_pk <- read_data_nst(raw_data_path, "penaltykill") %>%
		select(player, year, toi_gp) %>%
		rename(toi_gp_pk = toi_gp)
	
	# Bind all NST data together. 
	all_nst <- reduce(
		list(all_nst_individual, all_nst_relative, all_nst_pp, all_nst_pk),
		.f = function(x, y) left_join(x, y, by = c("player", "year"))) %>%
		mutate(player = tolower(player),
					 toi_gp_pp = ifelse(is.na(toi_gp_pp), 0, toi_gp_pp),
					 toi_gp_pk = ifelse(is.na(toi_gp_pk), 0, toi_gp_pk)) %>%
		mutate(player = case_when(
			player == "michael matheson" ~ "mike matheson",
			player == "christopher tanev" ~ "chris tanev",
			player == "evgenii dadonov" ~ "evgeny dadonov",
			TRUE ~ player
		))
	
	# Combine all data together from both NST and Evolving Hockey
	all_data <- left_join(all_eh, all_nst, by = c("player", "year"))
	
	# Find the player who are missing due to different naming conventions and fix 
	# their names
	all_na <- all_data %>%
		filter(rowSums(is.na(.)) > 0) %>%
		rowwise() %>%
		mutate(player = find_names(player, all_nst$player)) %>%
		select_if(~sum(!is.na(.)) > 0) %>%
		left_join(., all_nst, by = c("player", "year"))
	
	# Remove the missing rows and add back the players with non-missing values
	# Group all forwards and defencemen together. Create the Game Score variable.
	all_data <- all_data %>% 
		filter(rowSums(is.na(.)) == 0) %>%
		bind_rows(all_na) %>%
		mutate(
			position = as.factor(case_when(
			position == "L" ~ "F",
			position == "R" ~ "F",
			position == "D/R" ~ "D",
			position == "C" ~ "F",
			position == "C/R"~ "F",
			position == "L/R"~ "F",
			position == "C/L" ~ "F",
			TRUE ~ position
			)),
			game_score_60 = 0.75 * goals_60 + 0.7 * first_assists_60 + 0.55 * second_assists_60 + 
			0.07 * shots_60 + 0.05 * shots_blocked_60 + 0.15 * penalties_drawn_60 -
			0.15 * total_penalties_60 + 0.01 * faceoffs_won_60 - 0.01 * faceoffs_lost_60 +
			0.05 * cf_60 - 0.05 * ca_60 + 0.15 * gf_60 - 0.15 * ga_60
			) 
	
	# Center and scale all data
	recipe_data <- recipe(x = all_data) %>%
		step_center(all_numeric(), -year) %>%
		step_scale(all_numeric(), -year)
	
	# Apply this processing to the data
	prepper <- prep(recipe_data, all_data)
	all_data_processed <- juice(prepper) 
	
	# Parse years from command line
	years <- as.numeric(str_split(gte_years, ",")[[1]])
	
	if (!dir.exists(processed_out)) {
		dir.create(processed_out)
	}
	
	# Write .csv files
	map(years, .f = function(x) {
		all_data_processed %>%
			filter(year >= x) %>%
			write_rds(., path = paste0(processed_out, "/", "gte_", x, ".rds"))
	})
	
}

main(
	gte_years = opt$gte_years,
	raw_data_path = opt$raw_data_path,
	processed_out = opt$processed_out
)