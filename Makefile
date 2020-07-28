# Get data. Assumes environment variables chromedriver_path, google_email, and google_password exist.
.PHONY: all_raw_data
all_raw_data: src/python/0_get-data-nst.py src/python/0_get-data-eh.py
	python src/python/0_get-data-eh.py --chromedriver ${chromedriver_path} \
	--path_out "data" --years "2011,2012,2013,2014,2015,2016,2017,2018,2019,2020" \
	--min_toi 600 --google_email ${google_email} --google_password ${google_password}
	python src/python/0_get-data-nst.py --chromedriver ${chromedriver_path} \
	--path_out "data" --years "2011,2012,2013,2014,2015,2016,2017,2018,2019,2020" \
	--min_toi 0

# Preprocess data 
results/data/gte/% results/data/indiv/%: src/R/1_preprocess.R all_raw_data
	Rscript src/R/1_preprocess.R --year_seasons_gte "2014,2015,2016,2017,2018,2019" \
--year_seasons_indiv "2014,2015,2016,2017,2018,2019,2020" \
--raw_data_path data --processed_out_gte results/data/gte --processed_out_indiv results/data/indiv
	
# Fit SEM's
results/models/%: src/R/2_model.R src/R/lavaan/% results/data/gte/% results/data/indiv/%
	Rscript src/R/2_model.R --path_data results/data/gte \
	--model_for_path src/R/lavaan/model_for.txt --model_def_path src/R/lavaan/model_def.txt \
	--path_out results/models