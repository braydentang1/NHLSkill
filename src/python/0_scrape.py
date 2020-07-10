'''
This script scrapes player data from Natural Stat Trick.

Usage: 0_scrape.py --path_out=<path_out> --years=<years>

Options:
--path_out=<path_out> A file path that describes where to output the raw data.
--years=<years> A string of years to grab data from. Years should be comma separated with no whitespace (i.e. "2013,2014,2015")
'''

from bs4 import BeautifulSoup
import numpy as np
import pandas as pd
import requests
from urllib.request import urlopen
import os
import pickle
from docopt import docopt
import re
import time

opt = docopt(__doc__)

def main(path_out, years):
	
	# Define years
	all_years = [int(item) for item in re.split(',', years)]
	data_combined = {}

	for year in all_years:
		# Open the web page
		all_data = requests.get(
		'https://www.naturalstattrick.com/playerteams.php?fromseason=' + str(year - 1) + str(year) \
			+ '&thruseason=' + str(year - 1) + str(year) + \
			('&stype=2&sit=sva&score=all&stdoi=oi&rate=n&team=ALL&pos=S&loc=B&toi=0&gpfilt='  
			'none&fd=&td=&tgp=410&lines=single&draftteam=ALL')
		)
		time.sleep(10)		
		table_soup = BeautifulSoup(all_data.text)
		
		# Grab table of data
		player_data = table_soup.select('td')
		
		# Grab table names
		table_names = table_soup.select('th')

		# Get a sequence that describes the number of elements per each row, 
		# where the sequence is separated by number of columns.
		sequences = np.arange(start=0, stop=len(player_data) - len(table_names), step=len(table_names))
		cleaned_data = {}

		for i in enumerate(sequences):
			individual_data = player_data[i[1]:(i[1] + len(table_names))]
			parsed = [stat.text for stat in individual_data]
			cleaned_data[parsed[1]] = parsed[2:]

		final_data = pd.DataFrame.from_dict(cleaned_data, orient='index', columns=[column.text for column in table_names[2:]])
		data_combined[year] = pd.concat([pd.Series([year] * final_data.shape[0]), final_data.reset_index()], axis=1).rename(columns={0: 'Year', 'index': 'Player'})

	if not os.path.exists(path_out):
		os.makedirs(path_out)

	with open(path_out + "/raw_data.pickle", 'wb') as f:
		pickle.dump(data_combined, f)

main(opt['--path_out'], opt['--years'])