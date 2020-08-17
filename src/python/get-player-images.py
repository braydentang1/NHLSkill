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
import requests
import urllib.request
from selenium import webdriver
import os
import pickle
from docopt import docopt
import re
import time
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.select import Select

opt = docopt(__doc__)

def main(chromedriver_path, path_out, player_list):

	if not os.path.exists(path_out):
		os.makedirs(path_out)
	
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
	player_list = pd.read_csv(player_list)['player'].tolist()
	
	for player in player_list:
		
		all_files = os.listdir(path_out)
		name_split = player.split(" ")
		
		if name_split[0] + "_" + name_split[1] + ".jpg" in all_files:
			continue
		else:
			
			if player == "j.t. brown":
				player = "jt brown"
	
			driver.get('https://www.nhl.com/player')
			time.sleep(15)
			driver.find_element_by_css_selector('#searchTerm').send_keys(player)
			time.sleep(40)
			driver.find_element_by_css_selector('.typeahead-search-hidden-els .search-result-highlight').click()
			time.sleep(20)
			img = driver.find_element_by_css_selector('.player-jumbotron--responsive .player-jumbotron-vitals__headshot-image')
			src = img.get_attribute('src')
			urllib.request.urlretrieve(src, path_out + "/" + name_split[0] + "_" + name_split[1] + ".jpg")
		
main(
	opt['--chromedriver_path'],
	opt['--path_out'],
    opt['--player_list']
)
