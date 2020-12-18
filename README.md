# CODE_SEQUENCE_MIDTERMS_PAPER.txt
### a doc indicating the sequence in which code for NL's midterms paper ('Listening in on the Noise') was run. This includes code for:
- data filtering/collection
- data analysis  
*NL, 15/12/20*
*NL, 18/12/20 -- further adding to this doc*

## 1. FILTERING SMAPP COLLECTIONS FOR PERTINENT TWEETS

	- collections of interest were: 
		["us_election_general_2016", "us_election_jebbush_2016", "us_election_others_2016", "us_election_hillary_2016", "us_election_marcorubio_2016", "us_election_tedcruz_2016", "us_election_trump_2016"]
	- hashtags/keywords of interest were:
	    hashtags = ["#ivoted", "#myvote2016", "#myvote"]
		statements = ["i voted", "i will vote", "my vote", "vote for"] 

	SCRIPT: [NYU PRINCE] /home/nl1676/filtering_tweets.py
	--> moved to repo
	01_filtering_collections_for_pertinent_tweets.py

	DATA: tweet-jsons. [NYU PRINCE] /scratch/olympus/projects/hashtag_filtering
		hashtags_us_election_general_2016.json     statements_us_election_general_2016.json
		hashtags_us_election_hillary_2016.json     statements_us_election_hillary_2016.json
		hashtags_us_election_jebbush_2016.json     statements_us_election_jebbush_2016.json
		hashtags_us_election_marcorubio_2016.json  statements_us_election_marcorubio_2016.json
		hashtags_us_election_others_2016.json      statements_us_election_others_2016.json
		hashtags_us_election_tedcruz_2016.json     statements_us_election_tedcruz_2016.json
		hashtags_us_election_trump_2016.json       statements_us_election_trump_2016.json

## 2. EXTRACTING USER-IDS FROM PERTINENT TWEETS. 

	- going through tweets retrieved from previous method. 
	- removing tweets with verbatim duplicate tweet text [maybe this was a bad idea? -- maybe should have picked the one with the first date!]
	- removing RTs [this seems like a solid idea, because we want to identify folks that actually tweeted themselves that they are voting]. 

	SCRIPT: 
	--> moved to repo
	02_getting_user_ids.R

	DATA: Desktop/MIDTERMS_PAPER/data/panel_users_old.txt + panel_users.txt
	--> moved to Desktop/MIDTERMS_PAPER/data/

## 3. GEO-LOCATING USERS

	a) filtering all previously located users in geo dbs for user-ids in panel_users.txt, and writing out census-list and network-located matches.

		- run on geolocating machine 1
		- script: 03_get_previously_located_users.R
			--> moved to repo. 
		- resulting data: there were two files, located_panel.csv and net_located_panel.csv --> however these files are no longer retrievable. They were however concatenated with the data resulting from 3b)

	b) running the geo-locating pipeline on the remaining, so far not located users

		- this step simply ran the entire geo-locating pipeline on the remaining, unlocated users. there is a log-file documenting this, which has been moved to the repo: 
			--> 03_B_NL_social_panel_ids_RFF_colection.log

	c) sub-setting all located users to those that are locateable to the United States of America. This script then outputs the final 'social panel' of n=5176. 

		- script in repo: 03_C_removing_non_US_users.R
		- resulting data: ./data/located_us_panel_users.txt

## 4. USER-LEVEL DEMOGRAPHICS

	a) gender/race 
		- First step is to run existing R packages to estimate user-level sex, as well as user-level ethnicity. This is done using the packages 'gendeR' and 'wru'. Both packages work in the following way: extract users' names (first for gender, last for ethnicity) and match them to the US census. Then the package will return a probability of a given name (ie user) being male or female, or being white, black, asian, hispanic, or other. In both cases, I assign the given permutation of the given variable that has the highest probability. 

		- script in repo: 04_A_get_gender_race.R

	b) ideology
		- Here, I apply the principle outlined in Barbera, 2015, to attach a two-dimensional computed estimate of user-level political ideology ('affinity') to a given user. This is derived from 'elite' (i.e. media, politician, pundits, etc) the accounts a given user follows. If a user follows mostly right-wing accouns, then they will have a more right-wing pabloscore. 

		- script in repo: 04_B_merging_in_pabloscores.R
		- resulting data (from a and b): /data/social_panel_ideology.csv

	c) political-ness [this was not implemented in the paper]
		- Here, I wanted to see what proportion of a given users' tweets could be classified as political. This would then be a kind of proxy for measuring a given users' 'politicalness'. This was implemented by adapting code by Guess & Munger 2018, and also recycling their n=1000 training data to do the same. This uses a simple multinomial naive bayes algorithm on every given tweet. I didn't end up using these estimates in the paper. 

		- script in repo: 04_C_create_twitter_politics_model.R
		- resulting data: /data/user_politicalness_master.csv
