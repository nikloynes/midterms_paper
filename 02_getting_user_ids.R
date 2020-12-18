# extracting_user_ids.R

# parsing tweets with relevant hashtags,
# extracting unique user ids, 
# but first filtering for unique tweet text, and removing RTs

# NL, 31/03/18
# NL, 15/05/18

library(streamR)
library(dplyr)
library(stringr)

FILES <- list.files(path = "~/Dropbox/PhD/midterms_paper/jsons", pattern = ".json", full.names = TRUE)

# recursively read in tweets, 
# filter, and write user ids to .txt

for(i in 1:length(FILES)){
  tweets_df <- parseTweets(FILES[i])
  
  # filter by duplicate tweet text
  filtered_tweets_df <- tweets_df %>% 
    distinct(text, .keep_all = TRUE) 
  
  # filter by retweets
  isRT <- grepl(pattern = "^RT ", x = filtered_tweets_df$text)
  
  filtered_tweets_df$isRT <- isRT
  
  filtered_tweets_df <- filtered_tweets_df %>% 
    filter(isRT==FALSE)
  
  # extract date and time for these tweets
  
  filtered_tweets_df$month <- ""
  filtered_tweets_df$day <- ""
  filtered_tweets_df$time <- ""
  filtered_tweets_df$year <- ""
  
  for(j in 1:length(filtered_tweets_df$created_at)){
    pos1 <- unlist(strsplit(filtered_tweets_df$created_at[j], split = " "))
    filtered_tweets_df$month[j] <- pos1[2]
    filtered_tweets_df$day[j] <- pos1[3]
    filtered_tweets_df$time[j] <- pos1[4]
    filtered_tweets_df$year[j] <- pos1[6]
    
    # purge
    rm(pos1)
  }
  rm(j)
  
  #print(paste0(FILES[i], ": ", table(filtered_tweets_df$year, table(filtered_tweets_df$month))))
  
  # extract user ids
  user_ids <- as.character(unique(filtered_tweets_df$user_id_str))
  
  # write to file
  write(user_ids, file = "~/Dropbox/PhD/midterms_paper/panel_users.txt", append = TRUE)
  
  # purge
  rm(user_ids, filtered_tweets_df, tweets_df, isRT)
}

