'''
This script automates the download of .csv's from NaturalStatTrick.

Usage: 0_scrape.py --chromedriver_path=<chromedriver_path> --path_out=<path_out> --years=<years> --min_toi=<min_toi>

Options:
--chromedriver_path=<chromedriver_path>  A file path that describes where the web driver executable is.
--path_out=<path_out>  A file path that describes where to output the raw data.
--years=<years>  A string of comma separated values that describe the years of data to pull from. Example: "2016,2017,2018".
--min_toi=<min_toi>  An integer representing minimum time of ice desired.
'''

import numpy as np
import pandas as pd
import re
import time
import os
from urllib.request import urlopen
from docopt import docopt
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.select import Select

opt = docopt(__doc__)


def main(chromedriver_path, path_out, years, min_toi):

	# Create the output file path if it doesn't exist.
	if not os.path.exists(path_out + '/raw'):
		os.makedirs(path_out + '/raw')
	
	# Process inputs from terminal.
	all_years = [int(item) for item in re.split(',', years)]
	min_toi = int(min_toi)
	options = Options()
	# Set the correct download path that matches the terminal output.
	options.add_experimental_option("prefs", {
  	"download.default_directory": os.getcwd() + '/' + path_out + '/raw',
  	"download.prompt_for_download": False,
  	"download.directory_upgrade": True,
  	"safebrowsing.enabled": True
	})
	
	# For performance improvements
	options.add_argument('--no-proxy-server')
	options.add_argument("--proxy-server='direct://'")
	options.add_argument("--proxy-bypass-list=*")

	driver = webdriver.Chrome(chromedriver_path, options=options)
	
	# These are all of the file paths for each type of stats we wish to pull in.
	file_paths = ['on-ice-rel', 'on-ice-non-rel', 'individual', 'powerplay', 'penaltykill']
	for path in file_paths:
		
		if not os.path.exists(path_out + '/nst/' + path):
			os.makedirs(path_out + '/nst/' + path)
	
	for year in all_years:
		
		if year == 2013:
			amount_2013_shortened = 82/48
			# 2013 was a shortened season
			min_toi_new = round(int(min_toi)/amount_2013_shortened)
		else:
			min_toi_new = min_toi
		
		# Navigate to page for on-ice	relative rates
		driver.get(
			'http://www.naturalstattrick.com/playerteams.php?fromseason=' + str(year-1) + str(year) + '&thruseason=' + str(year-1) + str(year) + '&stype=2&\
			sit=5v5&score=all&stdoi=oi&rate=r&team=ALL&pos=S&loc=B&toi=' + str(min_toi_new) + '&gpfilt=gpteam&fd=&td=&tgp=410' + '&lines=single&draftteam=ALL'
		)
	
		time.sleep(40)
		driver.find_elements_by_css_selector('.buttons-csv~ .buttons-csv span')[0].click()
		time.sleep(10)
		os.rename(path_out + '/raw/' + os.listdir(path_out + '/raw')[0], path_out + '/nst/on-ice-rel/' + str(year) + '_' + str(min_toi_new) + ".csv")
	
	# Navigate to path for on-ice totals, not relative
		driver.get(
			'http://www.naturalstattrick.com/playerteams.php?fromseason=' + str(year-1) + str(year)+ '&thruseason=' + str(year-1) + str(year) + '&stype=2&\
			sit=5v5&score=all&stdoi=oi&rate=n&team=ALL&pos=S&loc=B&toi=' + str(min_toi_new) + '&gpfilt=gpteam&fd=&td=&tgp=410' + '&lines=single&draftteam=ALL'
		)

		time.sleep(40)
		driver.find_elements_by_css_selector('.buttons-csv~ .buttons-csv span')[0].click()
		time.sleep(10)
		os.rename(path_out + '/raw/' + os.listdir(path_out + '/raw')[0], path_out + '/nst/on-ice-non-rel/' + str(year) + '_' + str(min_toi_new) + ".csv")

		# Navigate to page for individual totals

		driver.get(
			'http://www.naturalstattrick.com/playerteams.php?fromseason=' + str(year-1) + str(year)+ '&thruseason=' + str(year-1) + str(year) + '&stype=2&\
			sit=5v5&score=all&stdoi=std&rate=n&team=ALL&pos=S&loc=B&toi=' + str(min_toi_new) + '&gpfilt=gpteam&fd=&td=&tgp=410' + '&lines=single&draftteam=ALL'
		)

		time.sleep(40)
		driver.find_elements_by_css_selector('.buttons-csv~ .buttons-csv span')[0].click()
		time.sleep(10)
		os.rename(path_out + '/raw/' + os.listdir(path_out + '/raw')[0], path_out + '/nst/individual/' + str(year) + '_' + str(min_toi_new) + ".csv")
		
		# Get TOI stats for powerplay
		driver.get(
			'https://www.naturalstattrick.com/playerteams.php?fromseason=' + str(year-1) + str(year) + '&thruseason=' + str(year-1) + str(year) + '&\
			stype=2&sit=pp&score=all&stdoi=std&rate=n&team=ALL&pos=S&loc=B&toi=' + str(min_toi_new) + '&gpfilt=none&fd=&td=&tgp=410&lines=single&draftteam=ALL'
		)

		time.sleep(40)
		driver.find_elements_by_css_selector('.buttons-csv~ .buttons-csv span')[0].click()
		time.sleep(10)
		os.rename(path_out + '/raw/' + os.listdir(path_out + '/raw')[0], path_out + '/nst/powerplay/' + str(year) + '_' + str(min_toi_new) + ".csv")
		
		# Get TOI stats for penalty kill
		driver.get(
			'https://www.naturalstattrick.com/playerteams.php?fromseason=' + str(year-1) + str(year) + '&thruseason=' + str(year-1) + str(year) + '&\
			stype=2&sit=pk&score=all&stdoi=std&rate=n&team=ALL&pos=S&loc=B&toi=' + str(min_toi_new) + '&gpfilt=none&fd=&td=&tgp=410&lines=single&draftteam=ALL'
		)

		time.sleep(40)
		driver.find_elements_by_css_selector('.buttons-csv~ .buttons-csv span')[0].click()
		time.sleep(10)
		os.rename(path_out + '/raw/' + os.listdir(path_out + '/raw')[0], path_out + '/nst/penaltykill/' + str(year) + '_' + str(min_toi_new) + ".csv")
	
main(
	opt['--chromedriver_path'],
	opt['--path_out'],
	opt['--years'],
	opt['--min_toi']
)
