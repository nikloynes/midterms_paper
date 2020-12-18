# create_twitter_politics_model_NL.R
# NL, 31/05/18
# Adapted from work by Guess et al, 2018, done by Kevin Munger

# script that trains a model for classifying tweets as 
# political / non-political

# install.packages(c("foreign", "dplyr", "streamR", "jsonlite", "readtext", "corpus",
#                    "quanteda", "Hmisc", "stringr", "irr"))

library(dplyr)
library(jsonlite)
library(readtext)
library(corpus)
library(quanteda)
library(Hmisc)
library(stringr)

# local & UoM PC
setwd("~/Dropbox/PhD/midterms_paper/classifying_politicalness")

# hpc
# reading in pre-coded tweets

tweets_coded_1a<-read.csv("training_data/tweets_coded_1_a.csv", stringsAsFactors = F)

tweets_coded_1a$political[is.na(tweets_coded_1a$political)]<-0

tweets_coded_1b<-read.csv("training_data/tweets_coded_1_b.csv", stringsAsFactors = F)

tweets_coded_1b$political[is.na(tweets_coded_1b$political)]<-0

tweets_coded_1c<-read.csv("training_data/tweets_coded_1_c.csv", stringsAsFactors = F)

tweets_coded_1c$political[is.na(tweets_coded_1c$political)]<-0

tweets_coded<-tweets_coded_1a
tweets_coded$political<-round(rowMeans(data.frame(tweets_coded_1a$political, tweets_coded_1b$political, tweets_coded_1c$political)))

# make a corpus and dfm out of text
tweet_corpus<-corpus(x = tweets_coded$text, docvars = data.frame(label = tweets_coded$political))
upper_lower_stopwords <- c(stopwords("english"), capitalize(stopwords("english")), "https", "t", "RT")

tweet_dfm<-dfm(tweet_corpus, remove = upper_lower_stopwords, remove_symbols = T, remove_punct = T , remove_twitter = F, remove_numbers = T,
               remove_url = T)


##train the model

tweet_dfm<-dfm_trim(tweet_dfm, min_docfreq = 5)

cv_acc<-vector()
tp<-vector()
fp<-vector()
tn<-vector()
fn<-vector()

for(i in 1:1000){
  
  indices <- sample(x = 1:ndoc(tweet_dfm), size = ndoc(tweet_dfm)/10, replace = F)
  
  test <- tweet_dfm[indices]
  ytest <- docvars(tweet_dfm)[indices,1]
  train <- tweet_dfm[-indices]
  ytrain <- docvars(tweet_dfm)[-indices,1]
   
  model <- textmodel_nb(train, ytrain)
  predicty <- predict(model, newdata = test)
  predicty2 <- predict(model, newdata = test, type = "probability")
  
  fixed_predictionsA <- as.character(predicty)
  fixed_predictionsA <- as.numeric(fixed_predictionsA)
  fixed_predictionsA[(predicty2[,2] < 0.6)] <- 0
  
  cv_acc[i]<-1-sum(abs(ytest - as.numeric(fixed_predictionsA)))/length(ytest)
  tp[i]<-length(which(ytest==1 & as.numeric(fixed_predictionsA) == 1))/length(ytest)
  tn[i]<-length(which(ytest==0 & as.numeric(fixed_predictionsA) == 0))/length(ytest)
  fp[i]<-length(which(ytest==0 & as.numeric(fixed_predictionsA) == 1))/length(ytest)
  fn[i]<-length(which(ytest==1 & as.numeric(fixed_predictionsA) == 0))/length(ytest)
  
}
mean(cv_acc)
mean(tp)
mean(fp)
mean(tn)
mean(fn)

##out of sample model accuracy : 83%

# set up / read in user-politicalness df

# user_ids <- readLines("~/Dropbox/PhD/midterms_paper/panel_members_user_ids.txt")
# user_politicalness_df <- data.frame(user_id = as.character(user_ids),
#                                     total_n_tweets = NA,
#                                     oldest_tweet_date = NA,
#                                     newest_tweet_date = NA,
#                                     n_political_tweets = NA,
#                                     stringsAsFactors = FALSE)
# 
# write.csv(user_politicalness_df, file = "~/Dropbox/PhD/midterms_paper/classifying_politicalness/user_politicalness_master.csv")

user_politicalness_df <- read.csv("~/Dropbox/PhD/midterms_paper/classifying_politicalness/user_politicalness_master.csv",
                                  stringsAsFactors = FALSE)
user_politicalness_df$X <- NULL
user_politicalness_df$user_id <- as.character(user_politicalness_df$user_id) 

# LOOP!
# 1 - list files in dir where chunked random shuffed jsons are
# 2 - iteratively classify the tweet text in these shuffed jsons
# 3 - write out info to the master csv
# 4 - write out master csv

# 1 - list jsons
json_chunks <- list.files("~/Desktop/panel_tweets_separated", full.names = TRUE)

# RUNNING CLASSIFICATION LOOP! 
for(i in 1:length(json_chunks)){
  
  # print status to console
  print(paste0("Now reading in tweet chunk ", i, " out of ", length(json_chunks), ".",
               " Time:", Sys.time()))
  
  # STEP 1 - read in next chunk
  tweets_df <- jsonlite::stream_in(con = file(json_chunks[i]))
  tweets_df <- flatten(tweets_df, recursive = TRUE)
  
  # STEP 2 - MERGE with anchor tweets
  # A - read in anchor terms
  anchor_terms <- readLines("~/Dropbox/PhD/midterms_paper/classifying_politicalness/training_data/anchor_terms.txt")
  
  # B - label anchor tweets as such
  is_anchor_tweet <- logical(length(tweets_df$text))
  
  for(l in 1:length(anchor_terms)){
    these <- grep(anchor_terms[l], tolower(tweets_df$text))
    is_anchor_tweet[these] <- TRUE
  }
  rm(l)
  
  # C - merge anchor tweets into the tweets data as political==1
  tweets_df$political <- NA
  tweets_df$political[is_anchor_tweet] <- 1
  
  # D - re order the tweets so that the ones that are already labelled are at the top
  tweets_df <- tweets_df[with(tweets_df, order(-political)), ]
  
  # STEP 3 - MAKE CORPUS, DFM, ETC.
  # A - combine labelled tweets and the rest of the tweets
  all_tweets <- c(tweets_coded$text, tweets_df$text)
  
  # B - make corpus and dfm
  all_tweets_corpuse <- quanteda::corpus(x = all_tweets)
  upper_lower_stopwords <- c(quanteda::stopwords("english"), capitalize(quanteda::stopwords("english")), "https", "t", "RT")
  
  all_tweet_dfm <- dfm(all_tweets_corpuse, remove = upper_lower_stopwords, remove_symbols = T, remove_punct = T , remove_twitter = F, remove_numbers = T,
                       remove_url = T)
  
  # C - trim dfm -- check to see which min_docfreq works here!
  all_tweet_dfm <- dfm_trim(all_tweet_dfm, min_docfreq = 100)
  
  # D - create labels
  labels <- c(docvars(tweet_dfm)[,1], rep(1, sum(is_anchor_tweet)))
  
  # STEP 4 - RUN MODEL
  # A - model
  # print to console
  print(paste0("Now running classifier. Time: ", Sys.time()))
  tweet_model <- textmodel_nb(all_tweet_dfm[1:length(labels)], labels, prior = "uniform")
  
  # B - predict
  predictyy <- predict(tweet_model, newdata = all_tweet_dfm[(length(labels)+1):length(all_tweets)])
  predictyy2 <- predict(tweet_model, newdata = all_tweet_dfm[(length(labels)+1):length(all_tweets)], type = "probability")
  
  # C - adjustments
  fixed_predictions <- as.character(predictyy)
  fixed_predictions <- as.numeric(fixed_predictions)
  
  # slightly higher threshold for being political, based on looking at labels
  fixed_predictions[(predictyy2[,2] < 0.65)] <- 0
  
  # D - merge into the data
  tweets_df$political[(sum(is_anchor_tweet)+1):length(tweets_df$political)] <- fixed_predictions
  
  table(tweets_df$political)
  
  # STEP 5 - EXTRACT RELEVANT STATISTICS AND MERGE BACK ON MASTER CSV
  # A - loop through unique user-ids
  
  unique_users <- unique(as.character(tweets_df$user.id))
  # print to console
  print(paste0("Now pulling data and writing to master-csv for all users in chunk. Time: ", Sys.time()))
  
  for(j in 1:length(unique_users)){
    # print to console
    print(paste0("On sub-iteration ", j, " out of ", length(unique_users), ". Time: ", Sys.time()))
    
    user_tweets_df <- tweets_df %>% filter(
      user.id==unique_users[j]
    )
    
    # B - extract relevant stats for user i
    # B1 - total number of tweets
    total_n_tweets <- nrow(user_tweets_df)
    
    # B2 - earliest/latest tweet date for user
    dates <- user_tweets_df$created_at
    dates <- dates[order(as.Date(dates, format="%a %b %d %H:%M:%S %z %Y"))]
    oldest_tweet_date <- as.Date(dates[1], format="%a %b %d %H:%M:%S %z %Y")
    newest_tweet_date <- as.Date(dates[length(dates)], format="%a %b %d %H:%M:%S %z %Y")
    
    # B3 - number political tweets
    n_political_tweets <- length(user_tweets_df$political[user_tweets_df$political==1])
    
    # C - insert these data into the master-csv
    # C1 - total number of tweets
    
    if(is.na(user_politicalness_df$total_n_tweets[user_politicalness_df$user_id==unique_users[j]])){
      user_politicalness_df$total_n_tweets[user_politicalness_df$user_id==unique_users[j]] <- total_n_tweets
    } else {
      user_politicalness_df$total_n_tweets[user_politicalness_df$user_id==unique_users[j]] <- user_politicalness_df$total_n_tweets[user_politicalness_df$user_id==unique_users[j]]+total_n_tweets
    }
    
    # C2 - oldest and newest tweets
    try(previous_newest <- as.Date(user_politicalness_df$newest_tweet_date[user_politicalness_df$user_id==unique_users[j]], format="%a %b %d %H:%M:%S %z %Y"))
    try(previous_oldest <- as.Date(user_politicalness_df$oldest_tweet_date[user_politicalness_df$user_id==unique_users[j]], format="%a %b %d %H:%M:%S %z %Y"))
    
    if(is.na(previous_oldest)){
      rm(previous_oldest)
    }
    
    if(is.na(previous_newest)){
      rm(previous_newest)
    }
    
    # oldest tweet
    
    if(exists("previous_oldest")){
      # if this difference in time is negative,
      # the `previous_oldest` is older, and thus stays
      if(difftime(previous_oldest, oldest_tweet_date)<0){
        user_politicalness_df$oldest_tweet_date[user_politicalness_df$user_id==unique_users[j]] <- user_politicalness_df$oldest_tweet_date[user_politicalness_df$user_id==unique_users[j]]
      } else {
        user_politicalness_df$oldest_tweet_date[user_politicalness_df$user_id==unique_users[j]] <- dates[1]
      }
    } else {
      # if there is no `previous_oldest` date for a user, insert it here!
      user_politicalness_df$oldest_tweet_date[user_politicalness_df$user_id==unique_users[j]] <- dates[1]
    }
    
    # newest tweet
    if(exists("previous_newest")){
      # if this difference in time is negative,
      # the `previous_newest` is older, and thus must be replaced by the new one
      if(difftime(previous_newest, newest_tweet_date)<0){
        user_politicalness_df$newest_tweet_date[user_politicalness_df$user_id==unique_users[j]] <- dates[length(dates)]
      } else {
        user_politicalness_df$newest_tweet_date[user_politicalness_df$user_id==unique_users[j]] <- user_politicalness_df$newest_tweet_date[user_politicalness_df$user_id==unique_users[j]]
      }
    } else {
      # if there is no `previous_newest` date for a tweet, insert it here!
      user_politicalness_df$newest_tweet_date[user_politicalness_df$user_id==unique_users[j]] <- dates[length(dates)]
    }
    
    # C3 - number of political tweets
    
    if(is.na(user_politicalness_df$n_political_tweets[user_politicalness_df$user_id==unique_users[j]])){
      user_politicalness_df$n_political_tweets[user_politicalness_df$user_id==unique_users[j]] <- n_political_tweets
    } else {
      user_politicalness_df$n_political_tweets[user_politicalness_df$user_id==unique_users[j]] <- user_politicalness_df$n_political_tweets[user_politicalness_df$user_id==unique_users[j]]+n_political_tweets
    }
    
    # DONE - now purge!
    rm(previous_oldest, previous_newest, n_political_tweets, newest_tweet_date, oldest_tweet_date, dates, total_n_tweets, user_tweets_df)
  }
  rm(j)
  # now - write out the master user_politicalness_df to csv!
  write.csv(user_politicalness_df, file = "~/Dropbox/PhD/midterms_paper/classifying_politicalness/user_politicalness_master.csv" )
  
  # now - purge
  rm(tweets_df, fixed_predictions, predictyy, predictyy2, tweet_model, labels, all_tweet_dfm, all_tweets_corpuse, all_tweets, is_anchor_tweet)
}



# # UoM PC
# tweets_df <- jsonlite::stream_in(con = file("~/Dropbox/PhD/midterms_paper/jsons/"))
# tweets_df <- flatten(tweets_df, recursive = TRUE)
# 
# # make anchor tweets
# # 1 - read in anchor terms
# anchor_terms <- readLines("~/Dropbox/PhD/midterms_paper/classifying_politicalness/training_data/anchor_terms.txt")
# 
# # 2 - label anchor tweets as such
# is_anchor_tweet <- logical(length(tweets_df$text))
# 
# for(i in 1:length(anchor_terms)){
#   these <- grep(anchor_terms[i], tolower(tweets_df$text))
#   is_anchor_tweet[these] <- TRUE
# }
# 
# # merge anchor tweets into the tweets data as political==1
# tweets_df$political <- NA
# tweets_df$political[is_anchor_tweet] <- 1
# 
# # 3 - re order the tweets so that the ones that are already labelled are at the top
# tweets_df <- tweets_df[with(tweets_df, order(-political)), ]
# 
# ###combine labled tweets and the rest of the tweets
# 
# all_tweets<-c(tweets_coded$text, tweets_df$text)
# 
# ##create labels to be populated with predicted values
# 
# ### combine labelled tweets and the rest of the tweets
# 
# all_tweets_corpuse <- quanteda::corpus(x = all_tweets)
# upper_lower_stopwords <- c(quanteda::stopwords("english"), capitalize(quanteda::stopwords("english")), "https", "t", "RT")
# 
# all_tweet_dfm <- dfm(all_tweets_corpuse, remove = upper_lower_stopwords, remove_symbols = T, remove_punct = T , remove_twitter = F, remove_numbers = T,
#                      remove_url = T)
# 
# ## remove sparse terms -- mindocfreq = 1000
# all_tweet_dfm <- dfm_trim(all_tweet_dfm, min_docfreq = 10)
# 
# # run this once with trimmed dfm, once without, compare output
# 
# ## create labels
# labels <- c(docvars(tweet_dfm)[,1], rep(1, sum(is_anchor_tweet)))
# # train model on the labeled tweets
# tweet_model <- textmodel_nb(all_tweet_dfm[1:length(labels)], labels, prior = "uniform")
# 
# ##need to make ONE dfm, then split
# predictyy <- predict(tweet_model, newdata = all_tweet_dfm[(length(labels)+1):length(all_tweets)])
# predictyy2 <- predict(tweet_model, newdata = all_tweet_dfm[(length(labels)+1):length(all_tweets)], type = "probability")
# 
# fixed_predictions <- as.character(predictyy)
# fixed_predictions <- as.numeric(fixed_predictions)
# 
# # slightly higher threshold for being political, based on looking at labels
# fixed_predictions[(predictyy2[,2] < 0.65)] <- 0
# 
# table(fixed_predictions)
# 
# # replace `political` variable in tweets_df with the fixed_predictions var
# 
# tweets_df$political[(sum(is_anchor_tweet)+1):length(tweets_df$political)] <- fixed_predictions
# 
# table(tweets_df$political)/length(tweets_df$political)
# 
# out <- data.frame(text = tweets_df$text, political = tweets_df$political, screen_name = tweets_df$user.screen_name)
# write.csv(out, "test_labelled_tweets_trimmed.csv")
# 
# 
# ### 27% political tweets -- updated 
# 
# 
# write.csv(x = tweets_final_df, file ="machine_labeled_tweets_updated.csv")
# 
# 
# ##create tweets to validate 
# 
# x<-read.csv("machine_labeled_tweets.csv", stringsAsFactors = F)
# 
# 
# indices<-sample(seq(1,length(x$X)), 1000)
# 
# sample<-x[indices,]
# 
# 
# indices<-sample(seq(1, length(anchor_tweets$X)), 100)
# 
# gold<-anchor_tweets[indices,]
# 
# ####
# 
# 
# tweets_to_code<-data.frame(text = sample$text, political = sample$political)
# 
# gold_df<-data.frame(text = gold$text, political = rep(1, 100))
# 
# 
# ###
# tweets_to_code1<-rbind(tweets_to_code, gold_df)
# 
# 
# rand <- sample(nrow(tweets_to_code1))
# 
# tweets_to_code_labeled<-tweets_to_code1[rand,]
# 
# 
# 
# write.csv(x = tweets_to_code_labeled, file ="tweets_to_code_labeled.csv")
# 
# 
# 
# write.csv(x = tweets_to_code_labeled$text, file ="tweets_to_code_unlabeled.csv", fileEncoding = "UTF-8")
# 
# 
