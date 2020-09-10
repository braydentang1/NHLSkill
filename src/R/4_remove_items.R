"This script deletes unused objects in the model fitting process to save memory
for shinyapps.io.

Usage: 4_remove_items.R --model_path_for=<model_path_for> --model_path_def=<model_path_def> --path_out=<path_out>

Options:
--model_path_for=<model_path_for> File path to where the fitted model .rds object is for forwards, the result of running 2_model.R.
--model_path_def=<model_path_def> File path to where the fitted model .rds object is for defenceman, the result of running 2_model.R.
--path_out=<path_out> A file path that describes where to output the modified objects for the web app.
" -> doc

library(tidyverse)
library(docopt)

opt <- docopt(doc)

main <- function(model_path_for, model_path_def, path_out) {
	
	# Create the output path if it doesn't exist.
	if (dir.exists(path_out) == FALSE) {
		dir.create(path_out)
	}
	
	# Read in the data from 2_model.R
	forwards <- read_rds(model_path_for)
	defenceman <- read_rds(model_path_def)
	
	# Get the years to loop over
	years <- names(forwards)
	
	for (i in seq_along(years)) {
		
		year_tmp <- years[i]
		
		# Set these stored items to NULL since they aren't used in the app.
		forwards[[year_tmp]]$model <- NULL 
		forwards[[year_tmp]]$aic <- NULL
		
		defenceman[[year_tmp]]$model <- NULL
		defenceman[[year_tmp]]$aic <- NULL
		
		# We need the 2014 year for the web application!
		if (year_tmp != "2015") {
			forwards[[year_tmp]]$data <- NULL 
			defenceman[[year_tmp]]$data <- NULL
		} 
		
	}
	
	# Output the files.
	saveRDS(forwards[!names(forwards) %in% c("2014")], paste0(path_out, "/forwards.rds"))
	saveRDS(defenceman[!names(defenceman) %in% c("2014")], paste0(path_out, "/defenceman.rds"))

}

main(
	opt$model_path_for,
	opt$model_path_def,
	opt$path_out
)