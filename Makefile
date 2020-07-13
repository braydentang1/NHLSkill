# Get data
.PHONY: all_raw_dat
all_raw_dat: src/python/0_get-data-nst.py
	python src/python/0_get-data-nst.py --chromedriver ${chromedriver_path} --path_out "data" --from_year 2020 --last_games 50 --min_toi 400
	python src/python/0_get-data-nst.py --chromedriver ${chromedriver_path} --path_out "data" --from_year 2019 --last_games 100 --min_toi 800
	python src/python/0_get-data-nst.py --chromedriver ${chromedriver_path} --path_out "data" --from_year 2018 --last_games 150 --min_toi 1200
	python src/python/0_get-data-nst.py --chromedriver ${chromedriver_path} --path_out "data" --from_year 2017 --last_games 200 --min_toi 1600