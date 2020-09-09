# Get data. Assumes environment variables chromedriver_path, google_email, and google_password exist.
.PHONY: all_raw_data
all_raw_data: src/python/0_get-data-nst.py src/python/0_get-data-eh.py
	python src/python/0_get-data-eh.py --chromedriver ${chromedriver_path} \
	--path_out "data" --years "2011,2012,2013,2014,2015,2016,2017,2018,2019,2020" \
	--min_toi 600 --google_email ${google_email} --google_password ${google_password}
	python src/python/0_get-data-nst.py --chromedriver ${chromedriver_path} \
	--path_out "data" --years "2011,2012,2013,2014,2015,2016,2017,2018,2019,2020" \
	--min_toi 0
	python src/python/0_get-data-hr.py --path_out "data/hr" --years "2011,2012,2013,2014,2015,2016,2017,2018,2019,2020"
	
# Preprocess data 
results/data/gte/% results/data/indiv/%: src/R/1_preprocess.R all_raw_data
	Rscript src/R/1_preprocess.R --year_seasons_gte "2014,2015,2016,2017,2018,2019" \
--year_seasons_indiv "2014,2015,2016,2017,2018,2019,2020" \
--raw_data_path data --processed_out_gte results/data/gte --processed_out_indiv results/data/indiv
	
# Fit SEM's
results/models/%: src/R/2_model.R src/R/lavaan/% results/data/gte/% results/data/indiv/%
	Rscript src/R/2_model.R --path_data results/data/gte \
	--model_for_path src/R/lavaan/model_for.txt --model_def_path src/R/lavaan/model_def.txt \
	--path_out results/models/gte -m Y
	
# Bootstrap factors

results/bootstrap/%: src/R/3_bootstrap.R results/models/% src/R/lavaan/%
	Rscript src/R/3_bootstrap.R --original_fitted_models_path results/models/gte/forwards.rds \
	--model_file_path src/R/lavaan/model_for.txt \
	--path_out results/bootstrap/gte/forwards \
	-n 6000 \
	-m Y
	Rscript src/R/3_bootstrap.R --original_fitted_models_path results/models/gte/defenceman.rds \
	--model_file_path src/R/lavaan/model_def.txt \
	--path_out results/bootstrap/gte/defenceman \
	-n 6000 \
	-m Y
	
# Remove unneeded objects from .rds files to save on memory

results/cleaned_models/%: src/R/4_remove_items.R results/models/gte/%
	Rscript src/R/4_remove_items.R --model_path_for results/models/gte/forwards.rds \
	--model_path_def results/models/gte/defenceman.rds \
	--path_out results/models/cleaned_models
