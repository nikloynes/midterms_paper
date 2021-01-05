#  01b_get_gender_race.R
#  NL, 20/05/2018
#  NL, 09/01/2018 -- adapted for geo-locating analysis
#  [Adapted from code by Andreu Casas]
#  NL, 23/01/2018 -- adapted for control panel users [NL midterms paper]
#
#  Script that estimates socio-demographics (gender, race) for random users. 
#  This is for the geo-locating paper analysis section.
#
#  Data In: Random user-ids, in CSV format.
#  Data out: users with gender and race estimates. 
#===============================================================================

# PACAKGES
#===============================================================================
library(dplyr)
library(wru)
library(qdap)
library(gender)
library(genderdata)
library(rtweet)
library(stringr)

# CONSTANTS

# insert the path to the github repo's root directory here
#REPO <- "~/Desktop/geo_locating/geolocation/"

# insert your data path here, which may or may not be on the github repo 
DATA_PATH <- "~/Dropbox/PhD/midterms_paper/tweets/control_panel/data/"

# insert your api key/token path here
KEY_PATH <- "~/Dropbox/smapp_rtweet_keys/"


# DATA IN
#===============================================================================

# ctrl panel user_ids
control_panel_users_df <- read.csv(paste0(DATA_PATH, "control_panel_members.csv"), colClasses = "character") %>% mutate(
  X = NULL
)

# twitter token
my_token <- readRDS(paste0(KEY_PATH, "smapp_key_100.rds"))

# ANALYSIS
# get full user objects

ctrl_panel_users_full_objects_df <- NULL

counter <- 0

for(i in 1:(ceiling(nrow(control_panel_users_df)/90000))){
  
  # to ensure that a max of 90k users are retrieved
  counter_old <- counter
  counter <- counter + 89999
  
  # retrieving user objects
  pos1_df <- lookup_users(control_panel_users_df$user_id[counter:counter_old], token = my_token)
  
  # binding onto the big data frame
  ctrl_panel_users_full_objects_df <- rbind(ctrl_panel_users_full_objects_df, pos1_df)
  
  # calculating wait time until API endpoint resets, adding 10 seconds for wiggle-room
  wait_time <- difftime(rate_limits(token = my_token)[38,]$reset_at, Sys.time(), units = "secs")+10
  
  # suspend execution
  print(paste0("Sleeping for ", as.character(wait_time), " seconds until Twitter API resets. Time: ", Sys.time()))
  Sys.sleep(wait_time)
  
}
rm(pos1_df)

# write out
# insert your desired output location.
#write_as_csv(random_users_full_objects_df, file_name = paste0(DATA_PATH, "random_users_full_objects.csv"))

# DATA WRANGLING
#===============================================================================

# MAIN_01: PREDICTING USERS' RACE
#===============================================================================
# - trying to get the last name from the Twitter name description. Splitting by
#   space and treating everyting after the first space as the last name. If there
#   is only 1 string, treating that as "last name".

usernames <- as.character(ctrl_panel_users_full_objects_df$name)
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

names_db_df <- data.frame(
  user_id = ctrl_panel_users_full_objects_df$user_id,
  full_name = ctrl_panel_users_full_objects_df$name,
  first_name = first_name,
  surname = sur_name,
  stringsAsFactors = FALSE
)

# - trying to predict last name race
predicted_race <- predict_race(voter.file = names_db_df, surname.only = TRUE )

# - giving NAs to those observations for which no true outcome was returned
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

# this is the output:
# > prop.table(table(predicted_race$highest_value_race))
# 
# not classified   asian       black    hispanic       other       white 
# 0.488519816 0.063590521 0.021908510 0.079594661 0.001995403 0.344391089 


# MAIN_02: PREDICTING RETWEETERS' GENDER
#===============================================================================
# - trying to predict gender
predicted_gender_raw_df <- gender(as.character(names_db_df$first_name),
                               method = "ssa", year = 1990)

predicted_gender <- unique(predicted_gender_raw_df) %>%  
  dplyr::select(name, proportion_male, proportion_female, gender) %>%
  rename(first_name = name,
         gender_male_pr = proportion_male,
         gender_female_pr = proportion_female) %>%
  mutate(first_name = as.character(first_name))

names_db_df$first_name <- as.character(names_db_df$first_name)
names_db_w_gender_df <- left_join(names_db_df, predicted_gender)

# merging gender and race

gender_race_db_df <- left_join(names_db_w_gender_df, predicted_race, by = "user_id")

# removing duplicate vars
gender_race_db_df$usernames.y <- NULL
gender_race_db_df$first_name.y <- NULL
gender_race_db_df$surname.y <- NULL

write.csv(gender_race_db_df, file = paste0(DATA_PATH, "ctrl_panel_users_gender_race.csv"))


# WRITE OUT THE GENDER_RACE_DB


# # DRAW 2 RANDOM SAMPLES with n=100
# # 1 - totally random
# 
# set.seed(1234)
# 
# total_random_sample <- sample(1:nrow(gender_race_db), size = 100, replace = FALSE)
# total_random_sample <- gender_race_db[total_random_sample,]
# 
# # 2 - gender and race are predicted
# 
# gender_race_nomissings_db <- gender_race_db %>% 
#   filter(!is.na(gender)) %>% 
#   filter(highest_value_race != "")
# 
# set.seed(4321)
# 
# random_nomiss <- sample(1:nrow(gender_race_nomissings_db), size = 100, replace = FALSE)
# random_nomiss <- gender_race_nomissings_db[random_nomiss,]
# 
# # write out samples
# write.csv(total_random_sample, "~/Dropbox/PhD/midterms_paper/gender_race_validation/total_random_sample.csv")
# write.csv(random_nomiss, "~/Dropbox/PhD/midterms_paper/gender_race_validation/random_sample_nomiss.csv")
# 
# # write out overall gender-race-data
# 
# write.csv(gender_race_db, "~/Dropbox/PhD/midterms_paper/gender_race_panel_users.csv")
# 
# 
# # PREPARING FINAL DATASET
# #===============================================================================
# names_db_w_gender <- names_db_w_gender %>%
#   dplyr::select(-usernames)
# 
# predicted_race_final <- predicted_race_final %>%
#   dplyr::select(-first_name, -surname, -usernames) %>%
#   mutate(usernames = as.character(usernames))
# 
# final_db <- cbind(db[,c("trumps_tweet_id", "user_id")],
#                   names_db_w_gender,
#                   predicted_race_final)
# 
# 
# 
# 
# 
# 
# 
# # other method, andreu:
# # - giving NAs to those observations for which I no true outcome was returned
# predicted_race[
#   which(predicted_race$pred.whi == 0.6665000 & 
#           predicted_race$pred.bla == 0.08530000),
# c("pred.whi", "pred.bla", "pred.his", "pred.asi", "pred.oth")
# ] <- NA
# 
# # - giving each person the race with the highest probability
# predicted_race_final <- predicted_race %>%
#   rename(race_white_pr = pred.whi,
#          race_black_pr = pred.bla,
#          race_hispanic_pr = pred.his,
#          race_asian_pr = pred.asi,
#          race_other_pr = pred.oth) %>%
#   mutate(race = NA,
#          race = ifelse(race_white_pr > 0.5, "white", as.character(race)),
#          race = ifelse(race_black_pr > 0.5, "black", as.character(race)),
#          race = ifelse(race_hispanic_pr > 0.5, "hispanic", as.character(race)),
#          race = ifelse(race_asian_pr > 0.5, "asian", as.character(race)),
#          race = ifelse(race_other_pr > 0.5, "other", as.character(race)))
# 
# 
# # OUTPUT
# #===============================================================================
# # write.csv(final_db,
# #           "../data/trump_retweeters_w_race_and_gender.csv", row.names = FALSE)
