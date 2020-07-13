'''
This script automates the download of .csv's from NaturalStatTrick.

Usage: 0_scrape.py --chromedriver_path=<chromedriver_path> --path_out=<path_out> --from_year=<from_year> --last_games=<last_games> --min_toi=<min_toi>

Options:
--chromedriver_path=<chromedriver_path> A file path that describes where the web driver executable is.
--path_out=<path_out> A file path that describes where to output the raw data.
--from_year=<from_year> An integer representing the most recent season to start looking ahead from.
--last_games=<last_games> An integer k; obtain the last K team games played from parameter from_year. Example: if the user specifies 2017 as from_year, then player data is obtained over a time span of the last 100 games from the 2017 season and onwards.
--min_toi=<min_toi> An integer representing minimum time of ice desired.
'''

import numpy as np
import pandas as pd
import requests
from urllib.request import urlopen
from selenium import webdriver
import os
import pickle
from docopt import docopt
import re
import time
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.select import Select

opt = docopt(__doc__)

def main(chromedriver_path, path_out, from_year, last_games, min_toi):

	if not os.path.exists(path_out + '/raw'):
		os.makedirs(path_out + '/raw')

	from_year = int(from_year)
	last_games = int(last_games)
	min_toi = int(min_toi)
	options = Options()
	options.add_experimental_option("prefs", {
  	"download.default_directory": os.getcwd() + '/' + path_out + '/raw',
  	"download.prompt_for_download": False,
  	"download.directory_upgrade": True,
  	"safebrowsing.enabled": True
	})

	driver = webdriver.Chrome(chromedriver_path, options=options)

	if not os.path.exists(path_out + '/on-ice'):
		os.makedirs(path_out + '/on-ice')

	if not os.path.exists(path_out + '/individual'):
		os.makedirs(path_out + '/individual')

	num_seasons = int(np.ceil(last_games / 82))

	if num_seasons == 1:
		num_seasons = 0
	# Navigate to page for on-ice	
	driver.get(
		'http://www.naturalstattrick.com/playerteams.php?fromseason=' + str(from_year-1) + str(from_year)+ '&thruseason=' + str(from_year + num_seasons - 1) + str(from_year + num_seasons) + '&stype=2& \
		sit=5v5&score=all&stdoi=oi&rate=n&team=ALL&pos=S&loc=B&toi=' + str(min_toi) + '&gpfilt=gpteam&fd=&td=&tgp=' + str(last_games) + '&lines=single&draftteam=ALL'
	)

	time.sleep(40)
	driver.find_elements_by_css_selector('.buttons-csv~ .buttons-csv span')[0].click()
	time.sleep(10)
	os.rename(path_out + '/raw/' + os.listdir(path_out + '/raw')[0], path_out + '/on-ice/' + str(last_games) + '_' + str(from_year) + ".csv")

	# Navigate to page for individual

	driver.get(
		'http://www.naturalstattrick.com/playerteams.php?fromseason=' + str(from_year-1) + str(from_year)+ '&thruseason=' + str(from_year + num_seasons - 1) + str(from_year + num_seasons) + '&stype=2& \
		sit=5v5&score=all&stdoi=std&rate=n&team=ALL&pos=S&loc=B&toi=' + str(min_toi) + '&gpfilt=gpteam&fd=&td=&tgp=' + str(last_games) + '&lines=single&draftteam=ALL'
	)

	time.sleep(40)
	driver.find_elements_by_css_selector('.buttons-csv~ .buttons-csv span')[0].click()
	time.sleep(10)
	os.rename(path_out + '/raw/' + os.listdir(path_out + '/raw')[0], path_out + '/individual/' + str(last_games) + '_' + str(from_year) + ".csv")

	
main(
	opt['--chromedriver_path'],
	opt['--path_out'],
	opt['--from_year'],
	opt['--last_games'],
	opt['--min_toi']
)
