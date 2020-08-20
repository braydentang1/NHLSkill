"This script preprocesses the data into a usable format for structural equation modelling.
A variety of variables are selected that are commonly used for assessing the profile of a player,
but not all of the variables are actually used. The resulting data is centered and scaled. This 
script assumes it will be run from the root of this repository.

Usage: 1_preprocess.R --year_seasons_gte=<year_seasons_gte> --year_seasons_indiv=<year_seasons_indiv> --raw_data_path=<raw_data_path> --processed_out_gte=<processed_out_gte> --processed_out_indiv=<processed_out_indiv>

Options:
--year_seasons_gte=<year_seasons_gte>  A string of years indicating the seasons of data to preprocess (cumulative data, starting from X)
--year_seasons_indiv<year_seasons_indiv>  A string of years indicating the seasons of data to preprocess (individual years of data).
--raw_data_path=<raw_data_path>  A file path that describes where all ofthe raw data is stored (the root), as a result of running 1_get-data_X.py
--processed_out_gte=<processed_out_gte>  A file path that describes where to store the processed data for cumulative years.
--processed_out_indiv=<processed_out_indiv>  A file path that describes where to store the processed data for individual years.
" -> doc

library(tidyverse)
library(recipes)
library(fuzzyjoin)
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
		all_files,
		.f = function(x) read_csv(paste(data_path, "nst", directory, x, sep = "/"), na = c("-")) %>%
										janitor::clean_names() %>%
										mutate(year = as.numeric(rep(str_extract(x, "[^_]*")))) %>%
										select(-x1) %>%
										relocate(year, after = player))
	
	bind_rows(all_data)
	
}

#' Read the saved HockeyReference OPS/DPS data.
#'
#' @param data_path A file path that describes where all the raw HockeyReference data is being stored, relative to the root
#' directory.
#' @param directory A file path that describes where all of the .csv files are for a particular scenario. This
#' should be a relative path from data_path. 
#'
#' @return A tibble of all of the .csv files combined.
#' @export
#'
#' @examples
#' read_data_eh("data", "gar")
read_data_hr <- function(data_path, directory) {
	
	all_files <- list.files(paste(data_path, "hr", directory, sep = "/"))
	
	all_data <- map(
		all_files, .f = function(x) read_csv(paste(data_path, "hr", directory, x, sep = "/"), na = c("-")) %>%
			select(-X1) %>%
			janitor::clean_names() %>%
			mutate(year = as.numeric(rep(str_extract(x, "[^.]*")))))
	
	bind_rows(all_data)
	
}

main <- function(year_seasons_gte, year_seasons_indiv, raw_data_path,
								 processed_out_gte, processed_out_indiv) {

	# Read Evolving Hockey GAR data
	all_GAR <- read_data_eh(raw_data_path, "gar")	%>%
		select(player, year, team, position, toi_all, war, off_gar, def_gar) %>%
		filter(year != 2013)
	
	# Read Evolving Hockey RAPM data
	all_rapm <- read_data_eh(raw_data_path, "rapm") %>%
		select(player, year, x_gf, x_ga, cf, ca, gf, ga) %>%
		filter(year != 2013)
	
	# Bind all Evolving Hockey data together
	all_eh <- left_join(all_GAR, all_rapm, by = c("player", "year")) %>%
		mutate(player = tolower(player))
	
	# Read in Individual Natural Stat Trick (NST) data
	all_nst_individual <- read_data_nst(raw_data_path, "individual") %>%
		filter(!(player == "Sebastian Aho" & position == "D")) %>%
		select(
			player, year, goals, first_assists, second_assists,
			total_points, shots, shots_blocked, penalties_drawn,
			total_penalties, faceoffs_won, faceoffs_lost, i_cf
			)
	
	# Read in On Ice Relative Natural Stat Trick data
	all_nst_non_relative <- read_data_nst(raw_data_path, "on-ice-non-rel") %>%
		filter(!(player == "Sebastian Aho" & position == "D")) %>%
		select(
			player, year, hdcf, hdca, off_zone_starts,
			def_zone_starts, scf, sca
			)
	
	# Read in Natural Stat Trick power play data
	all_nst_pp <- read_data_nst(raw_data_path, "powerplay") %>%
		filter(!(player == "Sebastian Aho" & position == "D")) %>%
		select(player, year, toi) %>%
		rename(toi_pp = toi) 
	
	# Read in Natural Stat Trick penalty kill data
	all_nst_pk <- read_data_nst(raw_data_path, "penaltykill") %>%
		filter(!(player == "Sebastian Aho" & position == "D")) %>%
		select(player, year, toi) %>%
		rename(toi_pk = toi) 
	
	# Bind all NST data together and deal with alternate spelling for some players.
	all_nst <- reduce(
		list(all_nst_individual, all_nst_non_relative, all_nst_pp, all_nst_pk),
		.f = function(x, y) left_join(x, y, by = c("player", "year"))) %>%
		mutate(player = tolower(player),
					 toi_pp = ifelse(is.na(toi_pp), 0, toi_pp),
					 toi_pk = ifelse(is.na(toi_pk), 0, toi_pk)) %>%
		mutate(player = case_when(
			player == "michael matheson" ~ "mike matheson",
			player == "christopher tanev" ~ "chris tanev",
			player == "evgenii dadonov" ~ "evgeny dadonov",
			player == "anthony deangelo" ~ "tony deangelo",
			TRUE ~ player
		)) 
	
	# Combine all data together from both NST and Evolving Hockey
	all_data <- left_join(all_eh, all_nst, by = c("player", "year"))
	
	# Get all players who have missing NST data
	last_names_na <- tibble(player = as.character(
		all_data %>%
		filter(rowSums(is.na(.)) > 0) %>%
		select(player) %>%
		pull() %>%
		str_split(., " ") %>%
		map(., .f = function(x) x[[2]])
		)) %>%
		mutate(player = case_when(
			player == "st" ~ "st. louis",
			player == "white" ~ "colin white",
			TRUE ~ player
		)) 
	
	# Fuzzy join i.e. joins if year matches and last name is detected in the full
	# name of other skater
	all_na <- all_data %>%
		filter(rowSums(is.na(.)) > 0) %>%
		select(-player) %>%
		bind_cols(., last_names_na) %>%
		select_if(~sum(!is.na(.)) > 0) %>%
		fuzzy_left_join(
			all_nst,
			by = c("player", "year"),
			match_fun = list(function(x, y) str_detect(y, x), function(x, y) x == y)) %>%
		select(-player.x, -year.x) %>%
		rename(player = player.y, year = year.y)
	
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
			game_score = 0.75 * goals + 0.7 * first_assists + 0.55 * second_assists + 
			0.07 * shots + 0.05 * shots_blocked + 0.15 * penalties_drawn -
			0.15 * total_penalties + 0.01 * faceoffs_won - 0.01 * faceoffs_lost +
			0.05 * cf - 0.05 * ca + 0.15 * gf - 0.15 * ga
			) 
	
	# Read in OPS/DPS Hockey Reference Data
	all_hr <- read_data_hr(raw_data_path, "point_shares") %>%
		mutate(player = tolower(player)) 
	
	# Combined with the preprocessed combined EH/NST data
	combined_hr_data <- all_hr %>%
		right_join(., all_data, by = c("player", "year")) 
	
	# Find player names who are missing - fill in those with phonetic characters
	# or who have last names of another player in the same year (alexander nylander, talbot)
	last_names_na <- tibble(player = as.character(combined_hr_data %>%
		filter(is.na(ops)) %>%
		select(player) %>%
		pull() %>%		
		str_split(., " ") %>%
		map(., .f = function(x) x[[2]]))) %>%
		mutate(player = case_when(
			player == "nylander" ~ "alexander nylander",
			player == "talbot" ~ "maxime talbot",
			player == "borgstrom" ~ "henrik borgström",
			player == "kubalik" ~ "dominik kubalík",
			TRUE ~ player
		))
		
	# Fuzzy join again like before. I adressed the multiple rows per single player in a year
	# problem by just taking the row with the totals in 0_get-data-hr.py.
	all_na <- combined_hr_data %>%
		filter(rowSums(is.na(.)) > 0) %>%
		bind_cols(., last_names_na) %>%
		select_if(~sum(!is.na(.)) > 0) %>%
		rename("player" = player...37) %>%
		fuzzy_left_join(
			all_hr,
			by = c("player", "year"),
			match_fun = list(function(x, y) str_detect(y, x), function(x, y) x == y)) %>%
		select(-player.x, -player.y, -year.y) %>%
		rename(year = year.x) 
	
	# Bind back missing rows with actual matched data
	all_data <- combined_hr_data %>% 
		filter(rowSums(is.na(.)) == 0) %>%
		bind_rows(all_na)
	
	# Center and scale by year and position
	all_data_processed <- all_data %>%
		group_by(year, position, .drop = FALSE) %>%
		mutate_if(is.numeric, .funs = list(function(x) (x - mean(x)) / sd(x))) %>%
		ungroup()
	
	# Parse years from command line
	years <- as.numeric(str_split(year_seasons_gte, ",")[[1]])
	years_indiv <- as.numeric(str_split(year_seasons_indiv, ",")[[1]])
	
	if (!dir.exists(processed_out_gte)) {
		dir.create(processed_out_gte, recursive = TRUE)
	}
	
	if (!dir.exists(processed_out_indiv)) {
		dir.create(processed_out_indiv, recursive = TRUE)
	}
	
	# Write .csv files
	map(years, .f = function(x) {
		all_data_processed %>%
			filter(year >= x) %>%
			write_rds(., path = paste0(processed_out_gte, "/", x, ".rds"))
	})
	
	map(years_indiv, .f = function(x) {
		all_data_processed %>%
			filter(year == x) %>%
			write_rds(., path = paste0(processed_out_indiv, "/", x, ".rds"))
	})
	
}

main(
	year_seasons_gte = opt$year_seasons_gte,
	year_seasons_indiv = opt$year_seasons_indiv,
	raw_data_path = opt$raw_data_path,
	processed_out_gte = opt$processed_out_gte,
	processed_out_indiv = opt$processed_out_indiv
)