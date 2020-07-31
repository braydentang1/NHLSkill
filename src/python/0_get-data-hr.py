'''
This script scrapes OPS and DPS data from HockeyReference.

Usage: 0_scrape.py --path_out=<path_out> --years=<years>

Options:
--path_out=<path_out>  A file path that describes where to output the raw data.
--years=<years>  A string of comma separated values that describe the years of data to pull from. Example: "2016,2017,2018".
'''

import pandas as pd
import requests
from urllib.request import urlopen
from bs4 import BeautifulSoup
import os
from docopt import docopt
import re
import time

opt = docopt(__doc__)

def main(path_out, years):

	if not os.path.exists(path_out + '/point_shares'):
		os.makedirs(path_out + '/point_shares')

	all_years = [int(item) for item in re.split(',', years)]
	
	
	for year in all_years:
		
		# Retrive the data
		url = 'https://www.hockey-reference.com/leagues/NHL_' + str(year) + '_skaters-misc.html'
		page = requests.get(url)
		time.sleep(5)
		page_souped = BeautifulSoup(page.text)
		
		player_names = page_souped.select('th+ .left a')
		
		if (year < 2015):
			ops = page_souped.select('.right:nth-child(23)')	
			dps = page_souped.select('.right:nth-child(24)')
		else:
			ops = page_souped.select('.right:nth-child(26)')	
			dps = page_souped.select('.right:nth-child(27)')
		
		all_names = [name.text for name in player_names]
		all_ops = [float(ops_val.text) for ops_val in ops]
		all_dps = [float(dps_val.text) for dps_val in dps]
		
		all_data = pd.DataFrame({'player': all_names, 'ops': all_ops, 'dps': all_dps})
		all_data = all_data.groupby('player').first().reset_index()
		all_data.to_csv(path_out + "/point_shares/" + str(year) + '.csv')
	
main(
	opt['--path_out'],
	opt['--years']
)
