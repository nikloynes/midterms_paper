# CODE_SEQUENCE_MIDTERMS_PAPER
### a doc indicating the sequence in which code for NL's midterms paper ('Listening in on the Noise') was run. This includes code for:
- data filtering/collection
- data analysis

*NL, 15/12/20*  
*NL, 18/12/20 -- further adding to this doc*  
*NL, 04/01/21 -- further additions to this doc*
*NL, 05/01/21 -- completion of the doc*

**NOTE: This repo was compiled a considerable time after the code was originally written and run.** The original code was all stored and run from the author's personal cloud storage, and naming conventions and sequences were not necessarily adhered to in a perfect fashion. Hence, it is possible that comments in scripts describing the purpose of a given script may slightly differ from what is written in this readme. However, this is to be ignored, as this repo and accompanying readme contain all code required to replicate the analysis. If any issues arise, do not hesitate to contact niklasloynes[at]gmail[dot]com. 


## 1. FILTERING SMAPP COLLECTIONS FOR PERTINENT TWEETS

- collections of interest were: 
	["us_election_general_2016", "us_election_jebbush_2016", "us_election_others_2016", "us_election_hillary_2016", "us_election_marcorubio_2016", "us_election_tedcruz_2016", "us_election_trump_2016"]  
- hashtags/keywords of interest were:
	hashtags = ["#ivoted", "#myvote2016", "#myvote"]  
	statements = ["i voted", "i will vote", "my vote", "vote for"] 

- **SCRIPT**: [NYU PRINCE] /home/nl1676/filtering_tweets.py  
	--> moved to repo  
	**01_filtering_collections_for_pertinent_tweets.py**

- **DATA**: tweet-jsons. [NYU PRINCE] **/scratch/olympus/projects/hashtag_filtering**
		hashtags_us_election_general_2016.json  
		statements_us_election_general_2016.json  
		hashtags_us_election_hillary_2016.json       
		statements_us_election_hillary_2016.json  
		hashtags_us_election_jebbush_2016.json       
		statements_us_election_jebbush_2016.json  
		hashtags_us_election_marcorubio_2016.json    
		statements_us_election_marcorubio_2016.json  
		hashtags_us_election_others_2016.json        
		statements_us_election_others_2016.json  
		hashtags_us_election_tedcruz_2016.json       
		statements_us_election_tedcruz_2016.json  
		hashtags_us_election_trump_2016.json         
		statements_us_election_trump_2016.json  

## 2. EXTRACTING USER-IDS FROM PERTINENT TWEETS. 

- going through tweets retrieved from previous method. 
- removing tweets with verbatim duplicate tweet text [maybe this was a bad idea? -- maybe should have picked the one with the first date!]
- removing RTs [this seems like a solid idea, because we want to identify folks that actually tweeted themselves that they are voting]. 

- **SCRIPT**:   
	--> moved to repo  
	**02_getting_user_ids.R**

- **DATA**:   
	**Desktop/MIDTERMS_PAPER/data/panel_users_old.txt + panel_users.txt**  
	--> moved to Desktop/MIDTERMS_PAPER/data/

## 3. GEO-LOCATING USERS

a) filtering all previously located users in geo dbs for user-ids in panel_users.txt, and writing out census-list and network-located matches.

- run on geolocating machine 1
- script: 03_get_previously_located_users.R  
	--> moved to repo. 
- resulting data: there were two files, **located_panel.csv** and **net_located_panel.csv**. However, these files are no longer retrievable. They wereconcatenated with the data resulting from 3b)

b) running the geo-locating pipeline on the remaining, so far not located users

- this step simply ran the entire geo-locating pipeline on the remaining, unlocated users. there is a log-file documenting this, which has been moved to the repo: 
	- --> **03_B_NL_social_panel_ids_RFF_colection.log**

c) sub-setting all located users to those that are locateable to the United States of America. This script then outputs the final 'social panel' of n=5176. 

- script in repo: **03_C_removing_non_US_users.R**
- resulting data: **./data/located_us_panel_users.txt**

## 4. USER-LEVEL DEMOGRAPHICS

a) gender/race 
- First step is to run existing R packages to estimate user-level sex, as well as user-level ethnicity. This is done using the packages 'gendeR' and 'wru'. Both packages work in the following way: extract users' names (first for gender, last for ethnicity) and match them to the US census. Then the package will return a probability of a given name (ie user) being male or female, or being white, black, asian, hispanic, or other. In both cases, I assign the given permutation of the given variable that has the highest probability. 

- script in repo: **04_A_get_gender_race.R**

b) ideology
- Here, I apply the principle outlined in Barbera, 2015, to attach a two-dimensional computed estimate of user-level political ideology ('affinity') to a given user. This is derived from 'elite' (i.e. media, politician, pundits, etc) the accounts a given user follows. If a user follows mostly right-wing accouns, then they will have a more right-wing pabloscore. The data for my users was supplied to me privately by Pablo Barbera. 

- script in repo: **04_B_merging_in_pabloscores.R**
- resulting data (from a and b): **/data/social_panel_ideology.csv**

c) political-ness [this was not implemented in the paper]
- Here, I wanted to see what proportion of a given users' tweets could be classified as political. This would then be a kind of proxy for measuring a given users' 'politicalness'. This was implemented by adapting code by Guess & Munger 2018, and also recycling their n=1000 training data to do the same. This uses a simple multinomial naive bayes algorithm on every given tweet. I didn't end up using these estimates in the paper. The code and training data was supplied to me by Kevin Munger. 

- script in repo: **04_C_create_twitter_politics_model.R**
- resulting data: **/data/user_politicalness_master.csv**  


## 5. THE 'CONTROL PANEL'

- The 'control panel' is the second sample used in the analysis of this paper. 
- It constitutes a randomly selected sample of Twitter users, subset to contain only US Twitter users. The random sample was generated at NYU's SMaPP Lab using a random number generator to randomly generate Twitter user-id like numerical objects and then test if they are in fact registered users on Twitter. 
- These users were then located using the Loynes, 2021 geolocating method, resulting in a set of 30k US users. This was conducted in 2018 for a different project. 

a) From this source dataset, a 10k sample was randomly drawn, these are the user-ids constituting the control panel. 

- script in repo: **05_making_control_panel.R**  
- resulting data: **/data/control_panel_user_ids.txt**

b) The same code as for the social panel was applied in order to compute estimated gender and race variables for users in the control panel.

- script in repo: **05_B_get_gener_race.R**
- resulting data: **/data/ctrl_panel_users_gender_race.csv**

c) ideology. Same as for control panel, using data supplied by Pablo Barbera to merge in the ideology estimates for all users where this score is available. 

- script in repo: **05_C_merging_pabloscores.R**
- resulting data: **/data/ctrl_panel_all_demographics.csv**


## 6. COLLECTING TWEETS FOR BOTH SAMPLES

- In order to run the analysis outlined in this paper, it was first necessary to collect users' tweets. The time frame for tweets considered in this analysis was chosen as starting from 2 months leading up to the 2018 midterms (November 6th, 2020). Tweets were collected using R and the rtweet package, for both the social and control panels.

a) social panel - script that iterates through all the users, uses the *user-timeline* Twitter api endpoint, and gets their most recent n=3200 tweets. Then, tweets that aren't within the relevant time window (2 months leading up to election) are dumped. Tweets are then written to file in a per-day basis, so all tweets from a given day are saved as ./tweets/{USER_ID}/{DD_MM_YY}.csv

- script in repo: **06_A_get_tweets_social_panel.R**  
- resulting data: available upon request (see Twitter's T&Cs)

b) control panel - same as in a), but with different user ids. 

- script in repo: **06_B_get_tweets_control_panel.R**
- resulting data: available upon request (see Twitter's T&Cs)

## 7. PRE-PROCESSING: EXTRACTING TWEETS MATCHING CRITERIA FOR ANALYSIS

- this paper aims to predict individual-level vote choice of Twitter users through machine learning methods on text analysis, trained through distant supervision. As is outlined in the Paper in section 4, this entails filtering users' tweet text for a range of pre-defined keywords. These keywords are on the one side used for training the distantly supervised models ('the i'm voting keywords'), and on the other side used for predicting the previously observed quantities in users' tweets not featuring the training keywords ('trump', 'kavanaugh', 'dems_hashtags', 'reps_hashtags', 'midterms', 'democrats', 'republicans'). 

- at this stage of the data engineering for this paper, all users' tweets are filtered for these pertinent keywords, and if there is a match, their matches are written out to new (duplicate) csv files with the same naming conventions, allowing for easy access in future classification models. 

- **NOTE**: While initially conceiving of and running this code, I wanted to run this analysis in a wave-style approach, akin to longitudinal panel studies, in order to potentially detect individual-level change over time (i.e. as the election comes nearer). However, given the small-ish numbers for especially the training category, I later decided to drop this approach and analyse all the data en-bloc. Hence, the scripts prefixed with *07_* aren't nearly as efficient or as easy to understand as they could be, but this is how the research was conducted. Below are the dates specified for the individual waves. These can be inserted into the code to run all the code for the individual waves. 

WAVE      DATE RANGE  
WAVE 01   - 02/10  
WAVE 02   > 03/10 - 09/10  
WAVE 03   > 10/10 - 16/10  
WAVE 04   > 17/10 - 23/10  
WAVE 05   > 24/10 - 30/10  
WAVE 06   > 31/10 - 06/11  

a) social panel: tweet filtering scripts separated by waves. Here, I present the script for wave 6, whereby the *date_range* variable can be edited in order to re-run this code for the other waves with the date ranges specified above. 

- script in repo: **07_A_filtering_tweets_WAVE_0X_social_panel.R**  
- resulting data: Tweet subsets at the user-level, reflecting each of the keyword categories (separated by wave). Available upon request (see Twitter's T&Cs)

b) control panel: tweet filtering scripts run for all waves in one script (This script was run later, and knowing the research configuration better, hence the more efficiently specified code). 

- script in repo: **07_B_filtering_tweets_all_waves_control_panel.R**
- resulting data: Tweet subsets at the user-level, reflecting each of the keyword categories (separated by wave). Available upon request (see Twitter's T&Cs)

c) concatenating wave-level keyword tweets (for all users, not separated by users) into all-wave tweet csvs for each of the keywords. This is done for both the social and control panels. 

- scripts in repo: **07_C1_merging_keyword_tweets_social_panel.R** and **07_C2_merging_keyword_tweets_control_panel.R**

- resulting data: csv files, for each of the keywords, containing all tweets containing these keywords, for both panels. Available upon request (see Twitter's T&Cs). 

## 8. HAND-LABELLING TWEETS CONTAINING VOTING-RELEVANT KEYWORDS FOR USER-LEVEL VOTING INTENTION

- In order to train a dinstantly supervised text classification model on users' electorally salient tweets (containing keywords identified in 7.), it was first necessary to hand-annotate tweets containing the 'I'm voting' keyword family. The keywords which met the inclusion criteria: *"my vote", "i'm voting", "i am voting", "i'll be voting", "i will be voting", "i am going to vote", "i will vote", "we'll vote", "we will vote", "we are voting", "i choose to vote", "im voting", "ill be voting", "myvote", "myvote2018", "imvoting"*. 

a) For this purpose, all pertinent tweets were exported to json format for both panels, and then displayed to the expert human coder (me, the author) through the Collabortweet tweet coding interface (github.com/cbuntain/collabortweet) at the *user-level*, meaning that oftentimes there would be several tweets under consideration for labelling a given user's voting intention. 

- scripts in repo: **08_A1_extracting_imvoting_json.R** (unfortunately, I can't find the script I used to do the same for the social panel. However, the csv with all 'im_voting' tweets was collated in the *07_C* scripts, and converting this to json is rather trivial, so it doesn't really matter)

- resulting data: **./data/imvoting_coded.csv** - csvs for both panels with labels at the user-level indicating the party a user is likely to vote for, given what they've tweeted. Can also be 'RU' (relevant but unknown) or 'NA'. Data available upon request (see Twitter's T&Cs)  

b) cleaning, auditing, exploring and exploring descriptives for coded 'i'm voting' data for social panel, combining wave-level hand-labelled data to data for all waves. For the control panel, this step is undertaken in the next script, as data at the wave-level didn't have to be merged together here. 

- script in repo: **08_B1_exploring_labelled_data_social_panel.R**
- resulting data: **/data/panel_users_all_declared_voters.csv** - a csv with all panel members, their predicted socio-demographic metadata, and hand-labelled vote choice for those that it has been predicted for at this stage.   

## 9. TRAINING & TUNING MODELS AND DEFINING BEST MODEL SPECIFICATIONS

- Next, textmodels had to be trained and tuned. For each of the panels, this is done in one big script, for each of the textmodels. The output from these scripts are the model specifications for each configuration, i.e. the numbers chosen for each of the hyperparameters. Training data used in these scripts are the *imvoting_coded.csv* files obtained from steps outlined in 8. For the control panel, the concatenation of user-level vote choice and user-level demographics is undertaken in this step rather than step 8.

- This training and tuning step is done with the models predicting two different outcome categories. First, the probability of a vote being for the Democrats, *_dems*, and second, the probability of a vote being for the Republicans, *_reps*. This means there are a total of 2 tuning scripts for each panel. 

- scripts in repo: 
	- **09_A1_tune_models_dems_social_panel.R**
	- **09_A2_tune_models_reps_social_panel.R**
	- **09_B1_tune_models_dems_control_panel.R**
	- **09_B2_tune_models_reps_control_panel.R**

- resulting data:
	- **/data/social_panel_dems_model_specs.csv**
	- **/data/social_panel_reps_model_specs.csv**
	- **/data/ctrl_panel_dems_model_specs.csv**
	- **/data/ctrl_panel_reps_model_specs.csv**

## 10. RUNNING TEXTMODELS

- Now comes the core analysis step of the paper - the application of the trained textmodels (Naive Bayes) on previously unseen data, in order to predict a probability of a vote for a given party for as many users as possible (contingent on their having tweeted something caught by any of the keywords previously mentioned). 

- all 5 models - *trump, kavanuagh, midterms, democrat, republican* - are run in one big loop in each script. The output quantity for each script is a user-level probability of vote==dems or vote==reps. This is contingent on a given user having tweeted something that was captured by the keywords relevant to the 5 specific models. 

a) Prediction - vote==democrats. 

- scripts in repo: **10_A1_all_models_dems_social_panel.R** & **10_A2_all_models_dems_control_panel.R**
- resulting data: **/data/all_models_no_waves_dems-social_panel.csv** & **/data/all_models_no_waves_dems-ctrl_panel.csv**

b) Prediction - vote==republicans. 

- scripts in repo: **10_B1_all_models_reps_social_panel.R** & **10_B2_all_models_reps_control_panel.R**
- resulting data: **/data/all_models_no_waves_reps-social_panel.csv** & **/data/all_models_no_waves_reps-ctrl_panel.csv**

c) "out of sample classification" - Here, we run the trained model from the social panel on the control panel data and vice versa. Here, we only predict probability(vote==dems) - as, in any case we define a vote for democrats as prob(vote==dem)>.5 and a vote for the republicans as prob(vote==dem)<.5

- scripts in repo: **10_C1_all_models_dems_SPonCP.R** & **10_C2_all_models_dems_CPonSP** 
- resulting data: **SPonCP_nowaves_dems.csv** & **CPonSP_nowaves_dems.csv**

## 11. RUNNING LOGISTIC REGRESSION MODELS

- As a further tactic for exploring vote choice classification, I built a logistic regression model which predicts a binary vote choice value based on available user-level metadata. The scripts produce csv files with all predicted values - probabilities of vote==dem, vote==rep and resulting vote variables, in order to provide the maximum amount of information in a single dataset. 

- scripts in repo: **11_A_logit_social_panel.R** & **11_B_logit_control_panel.R**
- resulting data: **/data/all_vote_estimates-social_panel.csv** & **data/all_vote_estimates-ctrl_panel.csv**

## 12. AGGREGATING VOTES

- In the final analysis script for this paper, the predicted user-level vote choices are aggregated, and then state-level percentages are calculated for the four states in question for this paper, California, Texas, New York and Florida. This is done for all possible model configurations, in weighted and unweighted states. This is done for the social and control panels in one script.

- scripts in repo: **12_aggregating_votes.R**
- resulting data: **/data/state_level_aggregations.csv**
