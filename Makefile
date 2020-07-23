# Get data. Assumes environment variables chromedriver_path, google_email, and google_password exist.
.PHONY: all_raw_data
all_raw_data: src/python/0_get-data-nst.py src/python/0_get-data-eh.py
	python src/python/0_get-data-eh.py --chromedriver ${chromedriver_path} --path_out "data" --years "2011,2012,2013,2014,2015,2016,2017,2018,2019,2020" --min_toi 600 --google_email ${google_email} --google_password ${google_password}
	python src/python/0_get-data-nst.py --chromedriver ${chromedriver_path} --path_out "data" --years "2011,2012,2013,2014,2015,2016,2017,2018,2019,2020" --min_toi 0

results/data/gte_2014.rds results/data/gte_2015.rds results/data/gte_2016.rds results/data/gte_2017.rds results/data/gte_2018.rds results/data/gte_2019.rds: src/R/1_preprocess.R all_raw_data
	Rscript src/R/1_preprocess.R --gte_years "2014,2015,2016,2017,2018,2019" --raw_data_path data --processed_out results/data