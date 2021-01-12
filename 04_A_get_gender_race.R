#===============================================================================
#  File-Name:	06-get-gender-race.R
#  NL, 20/05/2018
#  Adapted from code by Andreu casas
#
#  Script that calculates socia-demographics for users in social panel
#
#  Data In: Social Panel geo-located users
#===============================================================================

# PACAKGES
#===============================================================================
library(rio)
library(dplyr)
library(wru)
library(qdap)
library(gender)
library(genderdata)
library(twitteR)
library(stringr)
source("~/Desktop/GEO_LOCATING_REPO/geolocation/functions/TW_API_HANDLING_DO.R")
source("~/Desktop/GEO_LOCATING_REPO/geolocation/functions/network_analysis_functions.R")


# twitter API keys

api_keys <- c("~/Dropbox/PhD/Data/US_Placenames/api_keys/TatiusMethodius/geo_tweeter_01_init.R")



# DATA IN
#===============================================================================

# social panel user_ids

social_panel <- readLines("~/Dropbox/PhD/midterms_paper/located_us_panel_users.txt")

# get full user objects

panel_user_objects.df <- NULL

social_panel.ls <- fillTweeterList(social_panel, max = 100)

source(api_keys[2])

for(i in 1:length(social_panel.ls)){
  # get user object
  users.ls <- lookupUsers(social_panel.ls[[i]], includeNA = TRUE) 
  # extract vars of interest
  for(j in 1:length(users.ls)){
    newline <- data.frame(user_id = users.ls[[j]]$id,
                          description = users.ls[[j]]$description,
                          statusesCount = users.ls[[j]]$statusesCount,
                          followersCount = users.ls[[j]]$followersCount,
                          friendsCount = users.ls[[j]]$friendsCount,
                          name = users.ls[[j]]$name,
                          screen_name = users.ls[[j]]$screenName,
                          created = users.ls[[j]]$created,
                          verified = users.ls[[j]]$verified,
                          language = users.ls[[j]]$lang,
                          stringsAsFactors = FALSE)
    
    # bind to DF
    panel_user_objects.df <- rbind(panel_user_objects.df, newline)
    
    # purge
    rm(newline)
  }
  rm(users.ls)
}


# DATA WRANGLING
#===============================================================================

# MAIN_01: PREDICTING RETWEETERS' RACE
#===============================================================================
# - trying to get the last name from the Twitter name description. Splitting by
#   space and treating everyting after the first space as the last name. If there
#   is only 1 string, treating that as "last name".

usernames <- as.character(panel_user_objects.df$name)

usernames <- strsplit(usernames, split = " ")

# remove non-alpha chracters
for(i in 1:length(usernames)){
  usernames[[i]] <- gsub(pattern = "[^[:alpha:]]", replacement = "", x = usernames[[i]])
}
rm(i)

# add first name and last name to data frame
# assumptions: 
# substring == 1 --> substring == first name
# substring == 2 --> [1] first, [2] surname
# substring == >2 --> [1] first, [last] surname

first_name <- NULL
sur_name <- NULL

for(i in 1:length(usernames)){
  # condition 0 (no username string)
  if(length(usernames[[i]]==0)){
    first_name[i] <- NA
    sur_name[i] <- NA
  }
  # condition 1
  if(length(usernames[[i]])==1){
    first_name[i] <- usernames[[i]][1]
    sur_name[i] <- NA
  }
  # condition 2
  if(length(usernames[[i]])==2){
    first_name[i] <- usernames[[i]][1]
    sur_name[i] <- usernames[[i]][2]
  }
  # condition 3
  if(length(usernames[[i]]>2)){
    first_name[i] <- usernames[[i]][1]
    sur_name[i] <- usernames[[i]][length(usernames[[i]])]
  }
}

# - merging this last name info with the full name

names_db <- data.frame(
  user_id = panel_user_objects.df$user_id,
  usernames = panel_user_objects.df$name,
  first_name = first_name,
  surname = sur_name,
  stringsAsFactors = FALSE
)

# - trying to predict last name race
predicted_race <- predict_race(voter.file = names_db, surname.only = TRUE )

# - giving NAs to those observations for which I no true outcome was returned
predicted_race[
  which(predicted_race$pred.whi == 0.6665000 & 
          predicted_race$pred.bla == 0.08530000),
  c("pred.whi", "pred.bla", "pred.his", "pred.asi", "pred.oth")
  ] <- NA

predicted_race$highest_value <- 0
predicted_race$highest_value_race <- ""


# alternative method - which has the highest value
for(i in 1:nrow(predicted_race)){
  
  if(is.na(predicted_race$pred.whi[i])){
    next
  }
  
  predicted_race$highest_value[i] <- max(c(predicted_race$pred.whi[i], 
                                           predicted_race$pred.bla[i], 
                                           predicted_race$pred.his[i], 
                                           predicted_race$pred.asi[i], 
                                           predicted_race$pred.oth[i]))
  
  if(predicted_race$highest_value[i]==predicted_race$pred.whi[i]){
    predicted_race$highest_value_race[i] <- "white"
  }
  if(predicted_race$highest_value[i]==predicted_race$pred.bla[i]){
    predicted_race$highest_value_race[i] <- "black"
  }
  if(predicted_race$highest_value[i]==predicted_race$pred.his[i]){
    predicted_race$highest_value_race[i] <- "hispanic"
  }
  if(predicted_race$highest_value[i]==predicted_race$pred.asi[i]){
    predicted_race$highest_value_race[i] <- "asian"
  }
  if(predicted_race$highest_value[i]==predicted_race$pred.oth[i]){
    predicted_race$highest_value_race[i] <- "other"
  }
}


# MAIN_02: PREDICTING RETWEETERS' GENDER
#===============================================================================
# - trying to predict gender
predicted_gender_raw <- gender(as.character(names_db$first_name),
                               method = "ssa", year = 1990)

predicted_gender <- unique(predicted_gender_raw) %>%  
  dplyr::select(name, proportion_male, proportion_female, gender) %>%
  rename(first_name = name,
         gender_male_pr = proportion_male,
         gender_female_pr = proportion_female) %>%
  mutate(first_name = as.character(first_name))

names_db$first_name <- as.character(names_db$first_name)
names_db_w_gender <- left_join(names_db, predicted_gender)

# merging gender and race

gender_race_db <- left_join(names_db_w_gender, predicted_race, by = "user_id")

# removing duplicate vars
gender_race_db$usernames.y <- NULL
gender_race_db$first_name.y <- NULL
gender_race_db$surname.y <- NULL


# DRAW 2 RANDOM SAMPLES with n=100
# 1 - totally random

set.seed(1234)

total_random_sample <- sample(1:nrow(gender_race_db), size = 100, replace = FALSE)
total_random_sample <- gender_race_db[total_random_sample,]

# 2 - gender and race are predicted

gender_race_nomissings_db <- gender_race_db %>% 
  filter(!is.na(gender)) %>% 
  filter(highest_value_race != "")

set.seed(4321)

random_nomiss <- sample(1:nrow(gender_race_nomissings_db), size = 100, replace = FALSE)
random_nomiss <- gender_race_nomissings_db[random_nomiss,]

# write out samples
write.csv(total_random_sample, "~/Dropbox/PhD/midterms_paper/gender_race_validation/total_random_sample.csv")
write.csv(random_nomiss, "~/Dropbox/PhD/midterms_paper/gender_race_validation/random_sample_nomiss.csv")

# write out overall gender-race-data

write.csv(gender_race_db, "~/Dropbox/PhD/midterms_paper/gender_race_panel_users.csv")


# PREPARING FINAL DATASET
#===============================================================================
names_db_w_gender <- names_db_w_gender %>%
  dplyr::select(-usernames)

predicted_race_final <- predicted_race_final %>%
  dplyr::select(-first_name, -surname, -usernames) %>%
  mutate(usernames = as.character(usernames))

final_db <- cbind(db[,c("trumps_tweet_id", "user_id")],
                  names_db_w_gender,
                  predicted_race_final)







# other method, andreu:
# - giving NAs to those observations for which I no true outcome was returned
predicted_race[
  which(predicted_race$pred.whi == 0.6665000 & 
          predicted_race$pred.bla == 0.08530000),
c("pred.whi", "pred.bla", "pred.his", "pred.asi", "pred.oth")
] <- NA

# - giving each person the race with the highest probability
predicted_race_final <- predicted_race %>%
  rename(race_white_pr = pred.whi,
         race_black_pr = pred.bla,
         race_hispanic_pr = pred.his,
         race_asian_pr = pred.asi,
         race_other_pr = pred.oth) %>%
  mutate(race = NA,
         race = ifelse(race_white_pr > 0.5, "white", as.character(race)),
         race = ifelse(race_black_pr > 0.5, "black", as.character(race)),
         race = ifelse(race_hispanic_pr > 0.5, "hispanic", as.character(race)),
         race = ifelse(race_asian_pr > 0.5, "asian", as.character(race)),
         race = ifelse(race_other_pr > 0.5, "other", as.character(race)))


# OUTPUT
#===============================================================================
# write.csv(final_db,
#           "../data/trump_retweeters_w_race_and_gender.csv", row.names = FALSE)
