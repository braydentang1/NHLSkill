'''
This script automates the retrieval of player images from NHL.com.

Usage: get-player-images.py --chromedriver_path=<chromedriver_path> --path_out=<path_out> --player_list=<player_list>

Options:
--chromedriver_path=<chromedriver_path>  A file path that describes where the web driver executable is.
--path_out=<path_out>  A file path that describes where to output the images.
--player_list=<player_list> A .csv file of the full names of players. Assumes there exists a column named players. 
'''

import numpy as np
import pandas as pd
import urllib.request
import os
import re
import time
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.select import Select
from selenium import webdriver
from docopt import docopt

opt = docopt(__doc__)

def main(chromedriver_path, path_out, player_list):

	# Create output path if it doesn't exist.
	if not os.path.exists(path_out):
		os.makedirs(path_out)
	
	# Change the download output path to what is provided by the user.
	options = Options()
	options.add_experimental_option("prefs", {
  	"download.default_directory": os.getcwd() + '/' + path_out,
  	"download.prompt_for_download": False,
  	"download.directory_upgrade": True,
  	"safebrowsing.enabled": True
	})
	
	# For performance improvements
	options.add_argument('--no-proxy-server')
	options.add_argument("--proxy-server='direct://'")
	options.add_argument("--proxy-bypass-list=*")
	options.add_argument("window-size=1200x600")

	driver = webdriver.Chrome(chromedriver_path, options=options)
	# Read in player_list.csv and just grab the names of each player.
	player_list = pd.read_csv(player_list)['player'].tolist()
	
	for player in player_list:
		
		all_files = os.listdir(path_out)
		name_split = player.split(' ')
		
		# If the picture already exists, skip.
		if name_split[0] + '_' + name_split[1] + '.jpg' in all_files:
			continue
		else:
			# Some players have different spellings/inconsistent spellings. Since
			# there are so few, just change them manually.
			if player == 'j.t. brown':
				player = 'jt brown'
			elif player == 'mike cammalleri':
				player = 'michael cammalleri'
			elif player == 'nick shore':
				player = 'nicholas shore'
			elif player == 'patrick maroon':
				player = 'pat maroon'
			elif player == 'evgeny dadonov':
				player = 'evgenii dadonov'
			elif player == 'chris tanev':
				player = 'christopher tanev'
			elif player == 'anthony deangelo':
				player = 'tony deangelo'
			elif player == 'alex petrovic':
				player = 'alexander petrovic'
			elif player == 'matthew benning':
				player = 'matt benning'
			
			# Type in the player name in the search bar, click to actually go 
			# to that player's personal page, and download the .jpg file that is loaded.
			driver.get('https://www.nhl.com/player')
			time.sleep(15)
			driver.find_element_by_css_selector('#searchTerm').send_keys(player)
			time.sleep(40)
			driver.find_element_by_css_selector('.typeahead-search-hidden-els .search-result-highlight').click()
			time.sleep(20)
			img = driver.find_element_by_css_selector('.player-jumbotron--responsive .player-jumbotron-vitals__headshot-image')
			src = img.get_attribute('src')
			
			# Output to the correct folder specified in the terminal.
			urllib.request.urlretrieve(src, path_out + '/' + name_split[0] + '_' + name_split[1] + '.jpg')
		
main(
	opt['--chromedriver_path'],
	opt['--path_out'],
	opt['--player_list']
)
