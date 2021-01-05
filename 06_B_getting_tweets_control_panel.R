# getting_filtering_panel_tweets.R
# script to collect and filter tweets authored by members of the social panel
# 29/08/18, NL
# 04/09/18, NL
# 05/09/18, NL    working alpha
# 29/10/18, NL    adapted to get missing tweets [since 23/10], aka getting_missing_panel_tweets.R
# 30/10/18, NL    continuing with already completed users removed
# 04/11/18, NL    finalizing data collection, with slight changes, aka getting_missing_panel_tweets_02.R
# 05/11/18, NL    adapting for the control panel,   aka geting_control_panel_tweets.R

setwd("~/Dropbox/PhD/midterms_paper/tweets/control_panel/")

library(dplyr)
library(rtweet)
library(stringr)

my_token <- readRDS("PATH-TO-TOKEN")
          
# DATA IN: 
# control panel user ids

control_panel_users <- readLines("data/control_panel_user_ids.txt")

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

# for(i in 1:length(control_panel_users)){
#   dir.create(paste0("tweets/", control_panel_users[i]))
# }

# DATES FOR TWEETS
date_range <- seq(as.Date("2018-09-06"), by = 1, len = 61)

# LOOP
# max calls is 900. this must be accounted for in the loop

for(i in 5993:length(control_panel_users)){
  
  # WAIT for 10 seconds between users because of twitter api
  
  print("Sleeping for 10 seconds to manage API handling")
  Sys.sleep(5)

  print(paste0("On iteration ", i, " out out a total of ", length(control_panel_users)))
  # print API limits
  print("Rate limit status:")
  rate_limits_df <- rtweet::rate_limit(token = my_token)
  print(rate_limits_df[49,])
  
  # print status
  print(paste0("Now doing data collection and analysis for user ", control_panel_users[i], ". Time: ", Sys.time()))
  
  # get date of the last tweets
  
  all_dates <- list.files(paste0("tweets/", control_panel_users[i], "/"), pattern = "2018")
  all_dates <- as.Date(substr(all_dates, 1, 10))
  last_date <- sort(all_dates, decreasing = T)[1]
  
  # 1 - get twets for user i
  tweets_df <- get_timeline(user = control_panel_users[i], 
                            n = 3200,
                            token = my_token)
  
  # # 2 - remove tweets authored before the first date of collection
  tweets_df <- tweets_df %>% filter(
      as.Date(created_at) %in% date_range
  )
  
  # 3 - keep tweets that haven't been written out yet
  #unique_tweet_dates <- unique(as.Date(tweets_df$created_at))
  
  # IF tweets_df is now empty, move on to next iteration
  if(nrow(tweets_df)==0){
    print(paste0("No new tweets for user ", control_panel_users[i], ". Moving on to next user. Time: ", Sys.time(), "."))
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
                 file_name = paste0("tweets/", control_panel_users[i], "/", all_tweet_dates[j], "_n_", nrow(tweets_out_df), ".csv"))
    
    print(paste0("Wrote out n=", nrow(tweets_out_df), " tweets for user ", control_panel_users[i], " for date ", all_tweet_dates[j], " to csv."))
    
  }
  
}
