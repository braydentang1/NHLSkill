'''
This script automates the download of .csv's from EvolvingHockey. You must have a 
Google email address. If you have 2FA enabled, you must authenticate within 30 seconds of receiving the 
message or this script will fail.

Usage: 0_scrape.py --chromedriver_path=<chromedriver_path> --path_out=<path_out> --years=<years> --google_email=<google_email> --google_password=<google_password>

Options:
--chromedriver_path=<chromedriver_path> A file path that describes where the web driver executable is.
--path_out=<path_out> A file path that describes where to output the raw data.
--years=<years> A string of years to grab data from. Years should be comma separated with no whitespace (i.e. "2013,2014,2015")
--google_email=<google_email> Your Google email address that has Patreon enabled for EvolvingHockey.
--google_password=<google_password> Password for your Google email above.
'''

from bs4 import BeautifulSoup
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

opt = docopt(__doc__)

def main(chromedriver_path, path_out, years, google_email, google_password):

	all_years = [int(item) for item in re.split(',', years)]

	if not os.path.exists(path_out + '/raw'):
		os.makedirs(path_out + '/raw')

	options = Options()
	options.add_experimental_option("prefs", {
  	"download.default_directory": os.getcwd() + '/' + path_out + '/raw',
  	"download.prompt_for_download": False,
  	"download.directory_upgrade": True,
  	"safebrowsing.enabled": True
	})
	# First, sign in
	driver = webdriver.Chrome(chromedriver_path, options=options)
	driver.get(
		'https://evolving-hockey.com/'
	)
	time.sleep(10)
	driver.find_element_by_css_selector('#user_info a').click()
	time.sleep(3)
	driver.find_element_by_css_selector('.dUNWHJ .ghUBPl').click()
	time.sleep(3)
	windows = driver.window_handles
	driver.switch_to_window(windows[1])
	email = driver.find_element_by_id('identifierId')
	email.send_keys(google_email)
	driver.find_element_by_id('identifierNext').click()
	time.sleep(3)
	password = driver.find_element_by_css_selector('.ze9ebf .zHQkBf')
	password.send_keys(google_password)
	driver.find_element_by_id('passwordNext').click()
	windows = driver.window_handles
	driver.switch_to_window(windows[0])
	time.sleep(30)
	driver.find_element_by_css_selector('.patreon-button.patreon-button-action').click()
	time.sleep(5)
	driver.find_element_by_css_selector('.swal2-styled.swal2-confirm').click()

	all_tables = ['std', 'gar', 'xgar', 'rapm']

	for table in all_tables:
		
		if not os.path.exists(path_out + '/' + table):
			os.makedirs(path_out + '/' + table)
		
		for year in all_years:
			if table == "std":
				driver.get(
					'https://evolving-hockey.com/' \
					'?_inputs_&std_sk_range=%22Seasons%22&std_sk_pos=%22All%22&std_sk_str=%22All%22& \
					std_sk_age1=%2217%22&std_sk_season=%22' + str(year-1) + str(year) + \
					'%22&std_sk_span=%22Regular%22&std_sk_group=%22Team%2C%20Season%22&std_sk_type=%22Totals%22&dir=%22Skater%20Tables%22& \
					std_sk_toi=%2250%22&std_sk_dft_yr=%22All%22&std_sk_age2=%2250%22&std_sk_table=%22Box%20Score%22&std_sk_team= \
					%22All%22&std_sk_adj=%22No%20Adjustment%22&std_sk_info=%22No%22&std_sk_players=null'
				)
			elif table == "gar":
				driver.get(
					'https://evolving-hockey.com/?_inputs_&gar_sk_span=%22Regular%22&gar_sk_col \
					=%22Basic%22&dir=%22GAR%20Skater%20Tables%22&gar_sk_season=%22' + str(year-1) + str(year) + 
					'%22&gar_sk_group=%22Team%2C%20Season%22&gar_sk_base=%22Replacement%22&gar_sk_range= \
					%22Seasons%22&gar_sk_toi_ev=%220%22&gar_sk_age1=%2217%22&gar_sk_players=null&gar_sk_info=%22No%22&gar_sk_toi_sh=%220%22&gar_sk_toi_pp= \
					%220%22&gar_sk_dft_yr=%22All%22&gar_sk_pos=%22All%22&gar_sk_toi_all=%2250%22&gar_sk_team= \
					%22All%22&gar_sk_age2=%2250%22&gar_sk_type=%22Totals%22'
				)
			elif table == "xgar":
				driver.get(
					'https://evolving-hockey.com/?_inputs_&xgar_sk_season=%22' + str(year-1) + str(year) + '%22&xgar_sk_range= \
					%22Seasons%22&xgar_sk_col=%22Basic%22&xgar_sk_toi_all=%2250%22&xgar_sk_toi_pp=%220%22&xgar_sk_pos= \
					%22All%22&xgar_sk_players=null&xgar_sk_age1=%2217%22&dir=%22xGAR%20Skater%20Tables%22&xgar_sk_dft_yr= \
					%22All%22&xgar_sk_toi_sh=%220%22&xgar_sk_toi_ev=%220%22&xgar_sk_age2=%2250%22&xgar_sk_base= \
					%22Replacement%22&xgar_sk_team=%22All%22&xgar_sk_info=%22No%22&xgar_sk_group= \
					%22Team%2C%20Season%22&xgar_sk_type=%22Totals%22&xgar_sk_span=%22Regular%22'
				)
			else:
				driver.get(
					'https://evolving-hockey.com/?_inputs_&rapm_sk_age2=%2250%22&rapm_sk_table= \
					%22Single-Season%22&rapm_sk_season=%22' + str(year-1) + str(year) + '%22&rapm_sk_dft_yr= \
					%22All%22&dir=%22RAPM%20Skater%20Tables%22&rapm_sk_type=%22Rates%22&rapm_sk_info= \
					%22No%22&rapm_sk_pos=%22All%22&rapm_sk_span=%22Regular%22&rapm_sk_toi=%2250%22&rapm_sk_team= \
					%22All%22&rapm_sk_str=%22EV%22&rapm_sk_range=%22Seasons%22&rapm_sk_group= \
					%22Team%2C%20Season%22&rapm_sk_age1=%2217%22&rapm_sk_players=null'
				)

			time.sleep(30)
			driver.find_element_by_id(table + "_sk_download").click()
			time.sleep(5)

			os.rename(path_out + '/raw/' + os.listdir(path_out + '/raw')[0], path_out + '/' + table + '/' + str(year) + ".csv")

	
main(
	opt['--chromedriver_path'],
	opt['--path_out'],
	opt['--years'],
	opt['--google_email'],
	opt['--google_password']
)
