# Get data
results/data/raw_data.pickle: src/python/0_scrape.py
	python src/python/0_scrape.py --path_out "results/data" --years "2017,2018,2019,2020"
	
