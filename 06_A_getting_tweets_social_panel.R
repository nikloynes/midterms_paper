# getting_filtering_panel_tweets.R
# script to collect and filter tweets authored by members of the social panel
# 29/08/18, NL
# 04/09/18, NL
# 05/09/18, NL    working alpha
# 29/10/18, NL    adapted to get missing tweets [since 23/10], aka getting_missing_panel_tweets.R
# 30/10/18, NL    continuing with already completed users removed

# what do we want? 
# 1 - get tweets by the social panel
# 2 - sort tweets so that they are separated for each user
# 3 - filter tweets: 
#     a) is it a political tweet?
#     b) does the tweet contain a mention to 
#         - generic midterms keyword
#         - specific dem/rep midterms keyword
#         -
# 4 - write out the tweets for every individual panel member 
# 5 - write out the tweet metrics for every individual panel member

# This script is to be run on a daily basis, using the crontab

# NOTE: 
# first time this script was run (05/09/18), the row that filters the tweets for ones 
# from at least yesterday was commented out for a baseline

# STATUS: 05/09/18
# currently working: 
# - user-level tweet collection
# - user-level export of relevant metrics

# currently not working:
# - state-level export [adds new lines even though it should be changing existing ones, dates not quite working]
# - polsent-tweets export [not exporting *at all*]
# - issues with api limits
            
setwd("~/Dropbox/PhD/midterms_paper/tweets/panel/")

library(dplyr)
library(rtweet)
library(stringr)

my_token <- readRDS("PATH-TO-TOKEN")
          
# DATA IN: 
# social panel user ids

panel_users_df <- read.csv("../../social_panel_ideology.csv",
                           stringsAsFactors = FALSE)
panel_users_df$user_id <- as.character(panel_users_df$user_id)

panel_users <- panel_users_df$user_id 

# # relevant states
# 
# senate_relevant_states <- tolower(readLines(con = "../senate_relevant_states.txt"))
# governor_relevant_states <- tolower(readLines(con = "../governor_relevant_states.txt"))
# 
# # supplemental states
# 
# senate_supplemental_states <- tolower(state.name)
# senate_supplemental_states <- senate_supplemental_states[which(!(senate_supplemental_states %in% senate_relevant_states))]
# 
# governor_supplemental_states <- tolower(state.name)
# governor_supplemental_states <- governor_supplemental_states[which(!(governor_supplemental_states %in% governor_relevant_states))]
# 
# # state-level keywords
# # 1 - senate
# senate_keywords_df <- read.csv("../volume/senate/SENATE_forecast_paper_keywords.csv",
#                                stringsAsFactors = FALSE)
# 
# # 2 - governor
# governor_keywords_df <- read.csv("../volume/governor/GUBERNATORIAL_forecast_paper_keywords.csv",
#                                  stringsAsFactors = FALSE)
# 
# # generic keywords
# generic_relevant_terms <- tolower(readLines(con = "../generic_relevant_terms.txt"))

# CONSTANTS
# yesterdays_date <- as.character(Sys.Date()-1)
# todays_date <- as.character(Sys.Date())

# DIRECTORIES FOR DATA OUT

# for(i in 1:length(panel_users)){
#   dir.create(paste0("tweets/", panel_users[i]))
# }


# LOOP
# max calls is 900. this must be accounted for in the loop

for(i in 1:length(panel_users_contiuniung)){
  
  # WAIT for 10 seconds between users because of twitter api
  
  print("Sleeping for 10 seconds to manage API handling")
  Sys.sleep(10)

  # print API limits
  print("Rate limit status:")
  rate_limits_df <- rtweet::rate_limit(token = my_token)
  print(rate_limits_df[49,])
  
  # print status
  print(paste0("Now doing data collection and analysis for user ", panel_users_contiuniung[i], ". Time: ", Sys.time()))
  
  # get date of the last tweets
  
  all_dates <- list.files(paste0("tweets/", panel_users_contiuniung[i], "/"), pattern = "2018")
  all_dates <- as.Date(substr(all_dates, 1, 10))
  last_date <- sort(all_dates, decreasing = T)[1]
  
  # 1 - get twets for user i
  tweets_df <- get_timeline(user = panel_users_contiuniung[i], 
                            n = 3200,
                            token = my_token)
  
  # # 2 - remove tweets authored before the the previous day
  tweets_df <- tweets_df %>% filter(
    created_at > as.character(last_date)
  )
  
  # IF tweets_df is now empty, move on to next iteration
  if(nrow(tweets_df)==0){
    print(paste0("No new tweets for user ", panel_users_contiuniung[i], ". Moving on to next user. Time: ", Sys.time(), "."))
    rm(tweets_df)
    next
  }
  
  # subset tweets for each day
  
  all_tweet_dates <- unique(as.Date(tweets_df$created_at))
  
  # write out tweets for each day
  
  for(j in 1:length(all_tweet_dates)){
    
    tweets_out_df <- tweets_df %>% filter(
      as.Date(created_at)==all_tweet_dates[j]
    )
    
    write_as_csv(tweets_out_df, 
                 file_name = paste0("tweets/", panel_users_contiuniung[i], "/", all_tweet_dates[j], "_n_", nrow(tweets_out_df), ".csv"))
    
    print(paste0("Wrote out n=", nrow(tweets_out_df), " tweets for user ", panel_users_contiuniung[i], " for date ", all_tweet_dates[j], " to csv."))
    
  }
  

}
