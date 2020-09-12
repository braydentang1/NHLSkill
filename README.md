# NHLSkill
Estimating offensive and defensive contributions of NHL hockey players since 2015, using hierarchical latent variable modeling.

The application can be viewed [here.](http://bit.ly/nhl_profiler)

# Motivation

Often, we hear countless debates regarding whether X player contributes more than Y in some specific aspect of a hockey game. Crosby vs. Ovechkin, Heiskanen vs. Makar, Hughes vs Kakko - the list goes on. In general, the majority of these debates come down to either the so called "eye-test" or excessive reliance on a single particular statistic to support what already wants to be seen. I wanted to provide a more statistically sound way to compare players that looked at all of these stats at once. 

This model is not perfect - there are still aspects of players that aren't being captured (zone entries for example). In addition, the "eye-test" is still useful - it is difficult to capture all contexts with just numbers.

Other individuals havve attempted similar ideas to what this application and model does - however, a common problem with some other models is that they are typically biased towards those who generate above average offence (specifically high Corsi/points per game players) while ignoring defensive contributions. Thus, players like Valeri Nichushkin or Zach Aston-Reese are often assigned poor or lack luster ratings even though both players are third liners who exhibit strong defensive play in their own end.

# Methods

- To be completed. Essentially, the scores are all estimated using hierarchical structural equation models.

# Dependencies (for analysis only)

- Python 3.7 or higher:
	- numpy==1.18.1
	- pandas==1.0.1
	- selenium==3.141.0
	- BeautifulSoup4==4.8.2
	- docopt==0.6.2
	- [chromedriver](https://chromedriver.chromium.org/)
	
- R 4.00 or higher:
	- tidyverse==1.3.0
	- recipes==0.1.13
	- fuzzyjoin==0.1.6
	- docopt==0.7.1
	- lavaan==0.6-6

In addition, you must be a Patreon subscriber to [Evolving Hockey](https://evolving-hockey.com/). 
	
To run the app locally, install [Docker](https://www.docker.com/) and then run the following command:

`docker run -p 3838:3838 btang101/nhl_lv`

Visit `0.0.0.0:3838` in a web browser to view the application.

# Data Sources:

- [Hockey Reference](https://www.hockey-reference.com/)
- [Natural Stat Trick](https://www.naturalstattrick.com/)
- [Evolving Hockey](https://evolving-hockey.com/)
- [NHL Official](https://www.nhl.com/)