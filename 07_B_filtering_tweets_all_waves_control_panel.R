# 03_filtering_individual_tweets_for_WAVE_01.R
# script that filters individual tweets for each panel member in order to detect if there is 
# any voting-relevant mention in the tweets, in order to extract a baseline estimate
# of vote choice from the data. 
# NL, 09/10/18
# NL, 10/10/18    updates with filtering
# NL, 30/10/18    updated for Wave 03 
# NL, 05/11/18    updated for wave 03
# NL, 23/01/19    adapted for the control panel
# NL, 24/01/19    updated to filter tweets for all waves in one loop,
#                 aka 03_filtering_tweets-all_waves.R

library(dplyr)
library(rtweet)

DATA_PATH <- "~/Dropbox/PhD/midterms_paper/tweets/control_panel/"

# DATA IN 
# 1 - PANEL MEMBERS

ctrl_panel_df <- read.csv(paste0(DATA_PATH, "data/ctrl_panel_users_gender_race.csv"), 
                          stringsAsFactors = F) %>% mutate(
                            X = NULL,
                            user_id = as.character(user_id)
                          )

# CONSTANTS 
# 1 - DATE RANGES

# WAVE      DATE RANGE
# WAVE 01   - 02/10
# WAVE 02   > 03/10 - 09/10
# WAVE 03   > 10/10 - 16/10
# WAVE 04   > 17/10 - 23/10
# WAVE 05   > 24/10 - 30/10
# WAVE 06   > 31/10 - 06/11

date_range_01 <- seq(as.Date("2018-09-06"), by = 1, len = 27)
date_range_02 <- seq(as.Date("2018-10-03"), by = 1, len = 7)
date_range_03 <- seq(as.Date("2018-10-10"), by = 1, len = 7)
date_range_04 <- seq(as.Date("2018-10-17"), by = 1, len = 7)
date_range_05 <- seq(as.Date("2018-10-24"), by = 1, len = 7)
date_range_06 <- seq(as.Date("2018-10-31"), by = 1, len = 7)

date_range_ls <- list(date_range_01, date_range_02, date_range_03,
                      date_range_04, date_range_05, date_range_06)

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
democrat_hashtags <- readLines("~/Dropbox/PhD/midterms_paper/tweets/panel/new_filter_parameters/democrat_hashtags")

# 7 - REPUBLICAN HASHTAGS 
republican_hashtags <- readLines("~/Dropbox/PhD/midterms_paper/tweets/panel/new_filter_parameters/republican_hashtags") 

# 8 - MIDTERMS SPECIFIC
midterms_specific <- c("midterms", "midterm", "2018 election", "gubernatorial election", "senate election", "house election")

# 9 - DEMOCRATS
democrat_mentions <- c("democrat", "dems")

# 10 - REPUBLICANS
republican_mentions <- c("republican", "gop", "reps")

# 11 - empty wave_metrics data frame
wave_metrics_df <- NULL

for(l in 1:length(date_range_ls)){
  
  print(paste0("Now extracting relevant tweets for wave ", l, ". Time:", Sys.time()))
  
  ####################################
  # cycle through all ctrl panel users
  ####################################
  
  current_wave_metrics_df <- NULL
  
  for(i in 1:nrow(ctrl_panel_df)){
    
    # print status for current user
    print(paste0("Now on user number ", i, " out of ", length(ctrl_panel_df$user_id), ". Current user_id: ", ctrl_panel_df$user_id[i], ". Time and date: ", Sys.time()))
    
    # make relevant directories for the different filtering parameters where relevant tweets will be written out to
    
    if(!dir.exists(paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i]))){
      
      print(paste0("No tweets for user ", ctrl_panel_df$user_id[i], ". Next user - consider removing this user from the list of ctrl panel users."))
      next
    }
    
    # 1 - i'm voting
    ifelse(dir.exists(path = paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/imvoting/")), NA, 
           dir.create(path = paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/imvoting/")))
    
    # 2 - generic vote
    ifelse(dir.exists(path = paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/generic_vote")), NA, 
           dir.create(path = paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/generic_vote")))
    
    # 3 - trump
    ifelse(dir.exists(path = paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/trump")), NA, 
           dir.create(path = paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/trump")))
    
    # 4 - kavanaugh
    ifelse(dir.exists(path = paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/kavanaugh")), NA, 
           dir.create(path = paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/kavanaugh")))
    
    # 5 - democrat hashtags
    ifelse(dir.exists(path = paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/dems_hashtags")), NA, 
           dir.create(path = paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/dems_hashtags")))
    
    # 6 - republican hashtags
    ifelse(dir.exists(path = paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/reps_hashtags")), NA, 
           dir.create(path = paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/reps_hashtags")))
    
    # 7 - midterms specific
    ifelse(dir.exists(path = paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/midterms")), NA, 
           dir.create(path = paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/midterms")))
    
    # 8 - democrats
    ifelse(dir.exists(path = paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/democrats")), NA, 
           dir.create(path = paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/democrats")))
    
    # 8 - republicans
    ifelse(dir.exists(path = paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/republicans")), NA, 
           dir.create(path = paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/republicans")))
    
    # list all tweet files in the date range
    user_tweet_files <- list.files(paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i]), pattern = ".csv")
    keep_these <- substr(user_tweet_files, 1, 10) %in% as.character(date_range_ls[[l]])
    user_tweet_files <- user_tweet_files[keep_these]
    rm(keep_these)
    
    # recursively read in and concatenate tweets
    tweets_df <- NULL
    
    if(length(user_tweet_files)==0){ 
      
      print(paste0("User ", ctrl_panel_df$user_id[i], " has not tweeted during wave 0", l, ". Next user!"))
      
      newline_df <- data.frame(user_id = ctrl_panel_df$user_id[i],
                               personal_vote = 0,
                               vote_generic = 0,
                               trump_mention = 0,
                               kavanaugh_mention = 0,
                               dem_hashtags = 0,
                               rep_hashtags = 0,
                               democrat_mentions = 0,
                               republican_mentions = 0,
                               stringsAsFactors = F)
      
      current_wave_metrics_df <- rbind(current_wave_metrics_df, newline_df)
      rm(newline_df)
      
      next
    }
    
    for(j in 1:length(user_tweet_files)){
      
      pos1_df <- read_twitter_csv(paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/", user_tweet_files[j]))
      
      if(length(pos1_df)>88){
        
        pos1_df <- pos1_df[,1:88]
        
      }
      
      tweets_df <- rbind(tweets_df, pos1_df)
      rm(pos1_df)
      
    }
    
    # print status to console
    print(paste0("Read in ", nrow(tweets_df), " tweets for user ", ctrl_panel_df$user_id[i], ". Now filtering for relevant parameters."))
    
    # FILTERING
    # 1 - i'm voting
    vote_mentioned <- grep(pattern = paste0(personal_voting, collapse = "|"), x = tolower(tweets_df$text))
    
    # print to console:
    print(paste0("User ", ctrl_panel_df$user_id[i], " posted tweets with personal-voting relevant keywords ", length(vote_mentioned), " times during Wave 0", l, "."))
    
    # write out
    if(length(vote_mentioned>0)){
      pos1_df <- tweets_df[vote_mentioned,]
      write.csv(pos1_df, paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/imvoting/wave_0", l, ".csv"))
      rm(pos1_df)
    }
    
    # 2 - generic vote
    generic_vote_mentioned <- grep(pattern = paste0(generic_voting, collapse = "|"), x = tolower(tweets_df$text))
    
    # print to console
    print(paste0("User ", ctrl_panel_df$user_id[i], " posted tweets with generic-voting relevant keywords ", length(generic_vote_mentioned), " times during Wave 0", l, "."))
    
    # write out
    if(length(generic_vote_mentioned>0)){
      pos1_df <- tweets_df[generic_vote_mentioned,]
      write.csv(pos1_df, paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/generic_vote/wave_0", l, ".csv"))
      rm(pos1_df)
    }
    
    # 3 - trump mention
    trump_mentioned <- grep(pattern = paste0(trump_relelvant, collapse = "|"), x = tolower(tweets_df$text))
    
    # print to console
    print(paste0("User ", ctrl_panel_df$user_id[i], " posted tweets with Trump-relevant keywords ", length(trump_mentioned), " times during Wave 0", l, "."))
    
    # write out
    if(length(trump_mentioned>0)){
      pos1_df <- tweets_df[trump_mentioned,]
      write.csv(pos1_df, paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/trump/wave_0", l, ".csv"))
      rm(pos1_df)
    }
    
    # 4 - kavanaugh mention
    kavanaugh_mentioned <- grep(pattern = paste0(kavanaugh_relevant, collapse = "|"), x = tolower(tweets_df$text))
    
    # print to console
    print(paste0("User ", ctrl_panel_df$user_id[i], " posted tweets with Kavanaugh-relevant keywords ", length(kavanaugh_mentioned), " times during Wave 0", l, "."))
    
    # write out
    if(length(kavanaugh_mentioned>0)){ 
      pos1_df <- tweets_df[kavanaugh_mentioned,]
      write.csv(pos1_df, paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/kavanaugh/wave_0", l, ".csv"))
      rm(pos1_df)
    }
    
    # 5 - democrat hashtags mention
    dem_hashtags_mentioned <- grep(pattern = paste0(democrat_hashtags, collapse = "|"), x = tolower(tweets_df$text))
    
    # print to console
    print(paste0("User ", ctrl_panel_df$user_id[i], " posted tweets with Democrat-relevant hashtags ", length(dem_hashtags_mentioned), " times during Wave 0", l, "."))
    
    # write out
    if(length(dem_hashtags_mentioned>0)){
      pos1_df <- tweets_df[dem_hashtags_mentioned,]
      write.csv(pos1_df, paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/dems_hashtags/wave_0", l, ".csv"))
      rm(pos1_df)
    }
    
    # 6 - republican hashtags mention
    rep_hashtags_mentioned <- grep(pattern = paste0(republican_hashtags, collapse = "|"), x = tolower(tweets_df$text))
    
    # print to console
    print(paste0("User ", ctrl_panel_df$user_id[i], " posted tweets with Republican-relevant hashtags ", length(rep_hashtags_mentioned), " times during Wave 0", l, "."))
    
    # write out
    if(length(rep_hashtags_mentioned>0)){
      pos1_df <- tweets_df[rep_hashtags_mentioned,]
      write.csv(pos1_df, paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/reps_hashtags/wave_0", l, ".csv"))
      rm(pos1_df)
    }
    
    # 7 - midterms mention
    midterms_specific_mentioned <- grep(pattern = paste0(midterms_specific, collapse = "|"), x = tolower(tweets_df$text))
    
    # print to console
    print(paste0("User ", ctrl_panel_df$user_id[i], " posted tweets mentioning midterms-specific keywords ", length(midterms_specific_mentioned), " times during Wave 0", l, "."))
    
    # write out
    if(length(midterms_specific_mentioned>0)){
      pos1_df <- tweets_df[midterms_specific_mentioned,]
      write.csv(pos1_df, paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/midterms/wave_0", l, ".csv"))
      rm(pos1_df)
    }
    
    # 8 - democrat mention
    democrats_mentioned <- grep(pattern = paste0(democrat_mentions, collapse = "|"), x = tolower(tweets_df$text))
    
    # print to console
    print(paste0("User ", ctrl_panel_df$user_id[i], " posted tweets mentioning democrats ", length(democrat_mentions), " times during Wave 0", l, "."))
    
    # write out
    if(length(democrats_mentioned>0)){
      pos1_df <- tweets_df[democrats_mentioned,]
      write.csv(pos1_df, paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/democrats/wave_0", l, ".csv"))
      rm(pos1_df)
    }
    
    # 9 - republican mention
    republicans_mentioned <- grep(pattern = paste0(republican_mentions, collapse = "|"), x = tolower(tweets_df$text))
    
    # print to console
    print(paste0("User ", ctrl_panel_df$user_id[i], " posted tweets mentioning republicans ", length(republicans_mentioned), " times during Wave 0", l, "."))
    
    # write out
    if(length(republicans_mentioned>0)){
      pos1_df <- tweets_df[republicans_mentioned,]
      write.csv(pos1_df, paste0(DATA_PATH, "tweets/", ctrl_panel_df$user_id[i], "/republicans/wave_0", l, ".csv"))
      rm(pos1_df)
    }
    
    
    # now add a new line with these metrics to the data frame
    
    newline_df <- data.frame(user_id = ctrl_panel_df$user_id[i],
                             personal_vote = length(vote_mentioned),
                             vote_generic = length(generic_vote_mentioned),
                             trump_mention = length(trump_mentioned),
                             kavanaugh_mention = length(kavanaugh_mentioned),
                             dem_hashtags = length(dem_hashtags_mentioned),
                             rep_hashtags = length(rep_hashtags_mentioned),
                             democrat_mentions = length(democrats_mentioned),
                             republican_mentions = length(republicans_mentioned),
                             stringsAsFactors = F)
    
    current_wave_metrics_df <- rbind(current_wave_metrics_df, newline_df)
    rm(newline_df, rep_hashtags_mentioned, dem_hashtags_mentioned, kavanaugh_mentioned, trump_mentioned, generic_vote_mentioned, vote_mentioned)
    
  }
  
  # rename variables in current_wave_metrics_df to correspond to wave
  colnames(current_wave_metrics_df) <- c("user_id",
                                         paste0("personal_vote_wave_0", l),
                                         paste0("vote_generic_wave_0", l),
                                         paste0("trump_mention_wave_0", l),
                                         paste0("kavanaugh_mention_wave_0", l),
                                         paste0("dem_hashtags_wave_0", l),
                                         paste0("rep_hashtags_wave_0", l),
                                         paste0("democrat_mentions_wave_0", l),
                                         paste0("republican_mentions_wave_0", l))
                                         
  # add current_wave_metrics_df onto wave_metrics
  if(is.null(wave_metrics_df)){
    
    wave_metrics_df <- rbind(wave_metrics_df, current_wave_metrics_df)
    
  } else {
    
    wave_metrics_df <- left_join(wave_metrics_df, current_wave_metrics_df, by = "user_id")
  }
  
}


# write out master_df for all panel members to csv
write.csv(wave_metrics_df, paste0(DATA_PATH, "all_wave_tweet_metrics.csv"))

print(paste0("Finished filtering tweets for relevant characteristics. Writing to file. Time and date: ", Sys.time()))

