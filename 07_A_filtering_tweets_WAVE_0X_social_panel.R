# filtering_individual_tweets_for_WAVE_06.R
# script that filters individual tweets for each panel member in order to detect if there is 
# any voting-relevant mention in the tweets, in order to extract a baseline estimate
# of vote choice from the data. 
# NL, 09/10/18
# NL, 10/10/18    updates with filtering
# NL, 30/10/18    updated for Wave 05 
# NL, 05/11/18    updated for Wave 05
# NL, 05/11/18    updated for Wave 05
# NL, 05/11/18    updated for wave 05
# NL, 06/11/18    updated for wave 06

library(dplyr)
library(rtweet)

setwd("~/Dropbox/PhD/midterms_paper/")

# DATA IN 
# 1 - PANEL MEMBERS

panel_members_df <- read.csv("tweets/panel/panel_members_tweet_tracking_PRE.csv",
                             stringsAsFactors = F)
panel_members_df$user_id <- as.character(panel_members_df$user_id)
panel_members <- unique(panel_members_df$user_id)

# CONSTANTS 
# 1 - DATE RANGE

date_range <- seq(as.Date("2018-10-31"), by = 1, len = 5)
date_range <- as.character(date_range)

# 2 - PERSONAL-VOTING-RELEVANT KEYWORDS
personal_voting <- c("my vote", "i'm voting", "i am voting", "i'll be voting", "i will be voting",
                     "i am going to vote", "i will vote", "we'll vote", "we will vote", "we are voting",
                     "im voting", "ill be voting", "myvote", "myvote2018", "imvoting", "iPledgeToVote")

# 3 - GENERIC VOTING KEYWORDS
generic_voting <- c("vote", "voting")

# 4 - TRUMP-RELEVANT KEYWORDS
trump_relelvant <- c("trump", "donald", "realdonoldjtrump", "potus")

# 5 - KAVANAUGH-RELEVANT KEYWORDS
kavanaugh_relevant <- c("kavanaugh", "brett", "supreme court", "scotus")

# 6 - DEMOCRAT HASHTAGS
democrat_hashtags <- readLines("tweets/panel/new_filter_parameters/democrat_hashtags")

# 7 - REPUBLICAN HASHTAGS 
republican_hashtags <- readLines("tweets/panel/new_filter_parameters/republican_hashtags") 

# 8 - MIDTERMS SPECIFIC
midterms_specific <- c("midterms", "midterm", "2018 election", "gubernatorial election", "senate election", "house election")

# 9 - DEMOCRATS
democrat_mentions <- c("democrat", "dems")

# 10 - REPUBLICANS
republican_mentions <- c("republican", "gop", "reps")

# empty DF to write metrics to
panel_member_wave_six_metrics_df <- NULL

for(i in 1:length(panel_members)){
  
  # print status for current user
  print(paste0("Now on iteration ", i, " out of ", length(panel_members), ". Current user: ", panel_members[i], ". Time and date: ", Sys.time()))
  
  # make relevant directories for the different filtering parameters where relevant tweets will be written out to
  
  # 1 - i'm voting
  ifelse(dir.exists(path = paste0("tweets/panel/tweets/", panel_members[i], "/imvoting")), NA, 
         dir.create(path = paste0("tweets/panel/tweets/", panel_members[i], "/imvoting")))
  
  # 2 - generic vote
  ifelse(dir.exists(path = paste0("tweets/panel/tweets/", panel_members[i], "/generic_vote")), NA, 
         dir.create(path = paste0("tweets/panel/tweets/", panel_members[i], "/generic_vote")))
  
  # 3 - trump
  ifelse(dir.exists(path = paste0("tweets/panel/tweets/", panel_members[i], "/trump")), NA, 
         dir.create(path = paste0("tweets/panel/tweets/", panel_members[i], "/trump")))
  
  # 4 - kavanaugh
  ifelse(dir.exists(path = paste0("tweets/panel/tweets/", panel_members[i], "/kavanaugh")), NA, 
         dir.create(path = paste0("tweets/panel/tweets/", panel_members[i], "/kavanaugh")))
  
  # 5 - democrat hashtags
  ifelse(dir.exists(path = paste0("tweets/panel/tweets/", panel_members[i], "/dems_hashtags")), NA, 
         dir.create(path = paste0("tweets/panel/tweets/", panel_members[i], "/dems_hashtags")))
  
  # 6 - republican hashtags
  ifelse(dir.exists(path = paste0("tweets/panel/tweets/", panel_members[i], "/reps_hashtags")), NA, 
         dir.create(path = paste0("tweets/panel/tweets/", panel_members[i], "/reps_hashtags")))
  
  # 7 - midterms specific
  ifelse(dir.exists(path = paste0("tweets/panel/tweets/", panel_members[i], "/midterms")), NA, 
         dir.create(path = paste0("tweets/panel/tweets/", panel_members[i], "/midterms")))
  
  # 8 - democrats
  ifelse(dir.exists(path = paste0("tweets/panel/tweets/", panel_members[i], "/democrats")), NA, 
         dir.create(path = paste0("tweets/panel/tweets/", panel_members[i], "/democrats")))
  
  # 8 - republicans
  ifelse(dir.exists(path = paste0("tweets/panel/tweets/", panel_members[i], "/republicans")), NA, 
         dir.create(path = paste0("tweets/panel/tweets/", panel_members[i], "/republicans")))
  
  
  # list all tweet files in the date range
  user_tweet_files <- list.files(paste0("tweets/panel/tweets/", panel_members[i]))
  keep_these <- substr(user_tweet_files, 1, 10) %in% date_range
  user_tweet_files <- user_tweet_files[keep_these]
  rm(keep_these)
  
  # recursively read in and concatenate tweets
  tweets_df <- NULL
  
  if(length(user_tweet_files)==0){ 
    
    print(paste0("User ", panel_members[i], " has not tweeted during wave 06. Next user!"))
    
    newline_df <- data.frame(user_id = panel_members[i],
                             state = panel_members_df$admin[i],
                             ideology_group = panel_members_df$ideology_groups[i],
                             gender_pred = panel_members_df$gender[i],
                             politicalness = panel_members_df$politcalness_ratio[i],
                             wave_06_personal_vote = NA,
                             wave_06_vote_generic = NA,
                             wave_06_trump_mention = NA,
                             wave_06_kavanaugh_mention = NA,
                             wave_06_dem_hashtags = NA,
                             wave_06_rep_hastags = NA)
    
    panel_member_wave_six_metrics_df <- rbind(panel_member_wave_six_metrics_df, newline_df)
    rm(newline_df)
    
    next
  }
  
  for(j in 1:length(user_tweet_files)){
    
    pos1_df <- read_twitter_csv(paste0("tweets/panel/tweets/", panel_members[i], "/", user_tweet_files[j]))
    
    if(length(pos1_df)>88){
      
      pos1_df <- pos1_df[,1:88]
      
    }
    
    tweets_df <- rbind(tweets_df, pos1_df)
    rm(pos1_df)
    
  }
  
  # print status to console
  print(paste0("Read in ", nrow(tweets_df), " tweets for user ", panel_members[i], ". Now filtering for relevant parameters."))
  
  # FILTERING
  # 1 - i'm voting
  vote_mentioned <- grep(pattern = paste0(personal_voting, collapse = "|"), x = tolower(tweets_df$text))
  
  # print to console:
  print(paste0("User ", panel_members[i], " posted tweets with personal-voting relevant keywords ", length(vote_mentioned), " times during Wave 06."))
  
  # write out
  if(length(vote_mentioned>0)){
    pos1_df <- tweets_df[vote_mentioned,]
    write.csv(pos1_df, paste0("tweets/panel/tweets/", panel_members[i], "/imvoting/wave_06_041118.csv"))
    rm(pos1_df)
  }
  
  # 2 - generic vote
  generic_vote_mentioned <- grep(pattern = paste0(generic_voting, collapse = "|"), x = tolower(tweets_df$text))
  
  # print to console
  print(paste0("User ", panel_members[i], " posted tweets with generic-voting relevant keywords ", length(generic_vote_mentioned), " times during Wave 06."))
  
  # write out
  if(length(generic_vote_mentioned>0)){
    pos1_df <- tweets_df[generic_vote_mentioned,]
    write.csv(pos1_df, paste0("tweets/panel/tweets/", panel_members[i], "/generic_vote/wave_06_041118.csv"))
    rm(pos1_df)
  }
  
  # 3 - trump mention
  trump_mentioned <- grep(pattern = paste0(trump_relelvant, collapse = "|"), x = tolower(tweets_df$text))
  
  # print to console
  print(paste0("User ", panel_members[i], " posted tweets with Trump-relevant keywords ", length(trump_mentioned), " times during Wave 06."))
  
  # write out
  if(length(trump_mentioned>0)){
    pos1_df <- tweets_df[trump_mentioned,]
    write.csv(pos1_df, paste0("tweets/panel/tweets/", panel_members[i], "/trump/wave_06_041118.csv"))
    rm(pos1_df)
  }
  
  # 4 - kavanaugh mention
  kavanaugh_mentioned <- grep(pattern = paste0(kavanaugh_relevant, collapse = "|"), x = tolower(tweets_df$text))
  
  # print to console
  print(paste0("User ", panel_members[i], " posted tweets with Kavanaugh-relevant keywords ", length(kavanaugh_mentioned), " times during Wave 06."))
  
  # write out
  if(length(kavanaugh_mentioned>0)){ 
    pos1_df <- tweets_df[kavanaugh_mentioned,]
    write.csv(pos1_df, paste0("tweets/panel/tweets/", panel_members[i], "/kavanaugh/wave_06_041118.csv"))
    rm(pos1_df)
  }
  
  # 5 - democrat hashtags mention
  dem_hashtags_mentioned <- grep(pattern = paste0(democrat_hashtags, collapse = "|"), x = tolower(tweets_df$text))
  
  # print to console
  print(paste0("User ", panel_members[i], " posted tweets with Democrat-relevant hashtags ", length(dem_hashtags_mentioned), " times during Wave 06."))
  
  # write out
  if(length(dem_hashtags_mentioned>0)){
    pos1_df <- tweets_df[dem_hashtags_mentioned,]
    write.csv(pos1_df, paste0("tweets/panel/tweets/", panel_members[i], "/dems_hashtags/wave_06_041118.csv"))
    rm(pos1_df)
  }
  
  # 6 - republican hashtags mention
  rep_hashtags_mentioned <- grep(pattern = paste0(republican_hashtags, collapse = "|"), x = tolower(tweets_df$text))
  
  # print to console
  print(paste0("User ", panel_members[i], " posted tweets with Republican-relevant hashtags ", length(rep_hashtags_mentioned), " times during Wave 06."))
  
  # write out
  if(length(rep_hashtags_mentioned>0)){
    pos1_df <- tweets_df[rep_hashtags_mentioned,]
    write.csv(pos1_df, paste0("tweets/panel/tweets/", panel_members[i], "/reps_hashtags/wave_06_041118.csv"))
    rm(pos1_df)
  }
  
  # 7 - midterms mention
  midterms_specific_mentioned <- grep(pattern = paste0(midterms_specific, collapse = "|"), x = tolower(tweets_df$text))
  
  # print to console
  print(paste0("User ", panel_members[i], " posted tweets mentioning midterms-specific keywords ", length(midterms_specific_mentioned), " times during Wave 06."))
  
  # write out
  if(length(midterms_specific_mentioned>0)){
    pos1_df <- tweets_df[midterms_specific_mentioned,]
    write.csv(pos1_df, paste0("tweets/panel/tweets/", panel_members[i], "/midterms/wave_06_041118.csv"))
    rm(pos1_df)
  }
  
  # 8 - democrat mention
  democrats_mentioned <- grep(pattern = paste0(democrat_mentions, collapse = "|"), x = tolower(tweets_df$text))
  
  # print to console
  print(paste0("User ", panel_members[i], " posted tweets mentioning democrats ", length(democrat_mentions), " times during Wave 06."))
  
  # write out
  if(length(democrats_mentioned>0)){
    pos1_df <- tweets_df[democrats_mentioned,]
    write.csv(pos1_df, paste0("tweets/panel/tweets/", panel_members[i], "/democrats/wave_06_041118.csv"))
    rm(pos1_df)
  }
  
  # 9 - republican mention
  republicans_mentioned <- grep(pattern = paste0(republican_mentions, collapse = "|"), x = tolower(tweets_df$text))
  
  # print to console
  print(paste0("User ", panel_members[i], " posted tweets mentioning republicans ", length(republicans_mentioned), " times during Wave 06."))
  
  # write out
  if(length(republicans_mentioned>0)){
    pos1_df <- tweets_df[republicans_mentioned,]
    write.csv(pos1_df, paste0("tweets/panel/tweets/", panel_members[i], "/republicans/wave_06_041118.csv"))
    rm(pos1_df)
  }
  
  
  # now add a new line with these metrics to the data frame
  
  newline_df <- data.frame(user_id = panel_members[i],
                           state = panel_members_df$admin[i],
                           ideology_group = panel_members_df$ideology_groups[i],
                           gender_pred = panel_members_df$gender[i],
                           politicalness = panel_members_df$politcalness_ratio[i],
                           wave_06_personal_vote = length(vote_mentioned),
                           wave_06_vote_generic = length(generic_vote_mentioned),
                           wave_06_trump_mention = length(trump_mentioned),
                           wave_06_kavanaugh_mention = length(kavanaugh_mentioned),
                           wave_06_dem_hashtags = length(dem_hashtags_mentioned),
                           wave_06_rep_hastags = length(rep_hashtags_mentioned))
  
  panel_member_wave_six_metrics_df <- rbind(panel_member_wave_six_metrics_df, newline_df)
  rm(newline_df, rep_hashtags_mentioned, dem_hashtags_mentioned, kavanaugh_mentioned, trump_mentioned, generic_vote_mentioned, vote_mentioned)
  
}

# write out master_df for all panel members to csv
write.csv(panel_member_wave_five_metrics_df, "tweets/panel/wave_06_data/panel_members_relevant_mention_counts_wave06.csv")

print(paste0("Finished filtering tweets for relevant characteristics. Writing to file. Time and date: ", Sys.time()))

