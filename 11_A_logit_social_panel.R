# 05_logistic_regression_voting_classification_socpan.R
# a script that merges all estimated vote choices for all users in the control panel
# and runs a logistic regression model aimed at estimating
# vote choice for users so far unclassified based on their tweets
# also --> outputs tables with logistic regression models for all predicted votes
# DATA IN: vote choice estimates for dem_vote==1 and rep_vote==1 for control panel users,
#          and all available computed demographics
# DATA OUT: latex table with regression model, csv with all demographics and all vote estimates, 
#           plus computed vote estimates from logistic models 
# NL, 06/03/19 
# NL, 07/03/19

library(dplyr)
library(stargazer)

DATA_PATH <- "~/Dropbox/PhD/midterms_paper/tweets/panel/making_forecast/data/"
TABLE_PATH <- "~/Dropbox/PhD/midterms_paper/tables/"

# DATA IN
socpan_dem_vote_estimates_df <- read.csv(paste0(DATA_PATH, "all_models_no_waves_dems.csv"),
                                             stringsAsFactors = F) %>% 
  mutate(X = NULL,
         user_id = as.character(user_id),
         dem_mean = round(rowMeans(select(., prob_dem_trump_model,
                                          prob_dem_kavanaugh_model,
                                          prob_dem_midterms_model,
                                          prob_dem_democrats_model,
                                          prob_dem_republicans_model),
                                   na.rm = T), digits = 3))



socpan_rep_vote_estimates_df <- read.csv(paste0(DATA_PATH, "all_models_no_waves_reps.csv"),
                                             stringsAsFactors = F) %>% 
  mutate(X = NULL,
         user_id = as.character(user_id),
         rep_mean = round(rowMeans(select(., prob_rep_trump_model,
                                          prob_rep_kavanaugh_model,
                                          prob_rep_midterms_model,
                                          prob_rep_democrats_model,
                                          prob_rep_republicans_model),
                                   na.rm = T), digits = 3)) %>% 
  select(user_id, rep_vote, prob_rep_trump_model,
         prob_rep_kavanaugh_model, prob_rep_midterms_model,
         prob_rep_democrats_model, prob_rep_republicans_model, rep_mean)



socpan_ctrl_panel_classifier_df <- read.csv(paste0(DATA_PATH, "CPonSP_no_waves_dems.csv"),
                                            stringsAsFactors = F) %>%
  mutate(X = NULL,
         user_id = as.character(user_id),
         CPonSP_dem_mean = round(rowMeans(select(., prob_dem_CPonSP_trump_model,
                                                 prob_dem_CPonSP_kavanaugh_model,
                                                 prob_dem_CPonSP_midterms_model,
                                                 prob_dem_CPonSP_democrats_model,
                                                 prob_dem_CPonSP_republicans_model),
                                          na.rm = T), digits = 3)) %>% 
  select(user_id, prob_dem_CPonSP_trump_model, prob_dem_CPonSP_kavanaugh_model,
         prob_dem_CPonSP_midterms_model, prob_dem_CPonSP_democrats_model,
         prob_dem_CPonSP_republicans_model, CPonSP_dem_mean)

socpan_all_demographics_df <- read.csv("~/Dropbox/PhD/midterms_paper/tweets/panel/panel_members_tweet_tracking_PRE.csv") %>% 
  mutate(X = NULL,
         user_id = as.character(user_id)) %>% 
  select(user_id, gender, highest_value_race, ideology, ideology_relevant_accounts_followed)

# remove ideology scores for users who follow fewer than 3 ideology-relevant accounts
socpan_all_demographics_df$ideology[socpan_all_demographics_df$ideology_relevant_accounts_followed<3] <- NA

# merge datasets

socpan_vote_estimates_df <- left_join(socpan_dem_vote_estimates_df, 
                                          socpan_rep_vote_estimates_df, by = "user_id") %>%
  left_join(socpan_ctrl_panel_classifier_df, by = "user_id") %>% 
  left_join(socpan_all_demographics_df, by = "user_id") 

socpan_vote_estimates_df$pred_dem_vote[socpan_vote_estimates_df$dem_mean>=0.5] <- 1
socpan_vote_estimates_df$pred_dem_vote[socpan_vote_estimates_df$dem_mean<0.5] <- 0
socpan_vote_estimates_df$pred_rep_vote[socpan_vote_estimates_df$rep_mean>=0.5] <- 1
socpan_vote_estimates_df$pred_rep_vote[socpan_vote_estimates_df$rep_mean<0.5] <- 0
socpan_vote_estimates_df$pred_dem_CPonSP[socpan_vote_estimates_df$CPonSP_dem_mean>=0.5] <- 1
socpan_vote_estimates_df$pred_dem_CPonSP[socpan_vote_estimates_df$CPonSP_dem_mean<0.5] <- 0

# dem model

dem_model <- glm(pred_dem_vote~ideology+gender+highest_value_race,
                 family = "binomial",
                 data = socpan_vote_estimates_df)

logit_predicted_dem_votes <- round(predict.glm(
  dem_model, newdata = socpan_vote_estimates_df, type = "response"))

socpan_vote_estimates_df <- cbind(socpan_vote_estimates_df, logit_predicted_dem_votes)

# rep model

rep_model <- glm(pred_rep_vote~ideology+gender+highest_value_race,
                 family = "binomial",
                 data = socpan_vote_estimates_df)

logit_predicted_rep_votes <- round(predict.glm(
  rep_model, newdata = socpan_vote_estimates_df, type = "response"))

socpan_vote_estimates_df <- cbind(socpan_vote_estimates_df, logit_predicted_rep_votes)

# cross_model dem model

cross_model_dem_model <- glm(pred_dem_CPonSP~ideology+gender+highest_value_race,
                             family = "binomial",
                             data = socpan_vote_estimates_df)

logit_predicted_cross_model_dem_vote <- round(predict.glm(
  cross_model_dem_model, newdata = socpan_vote_estimates_df, type = "response"))

socpan_vote_estimates_df <- cbind(socpan_vote_estimates_df, logit_predicted_cross_model_dem_vote)

# OUTPUT LATEX TABLES WITH LOGISTIC REGRESSION 

stargazer(dem_model, rep_model, cross_model_dem_model, 
          title = "Logit models", 
          dep.var.labels = c("V=D", "V=R", "V=D, SP->CP"), 
          covariate.labels = c("Tweetscores ideology", 
                               "Sex: male", 
                               "Ethnicity: Asian", 
                               "Ethnicity: Black", 
                               "Ethnicity: Latin/Hispanic", 
                               "Ethnicity: other", 
                               "Ethnicity: white"),
          out = paste0(TABLE_PATH, "socpan_logit_models.tex"))

# combined vote choice variables:
# textmodel-based and logistic regression based

socpan_vote_estimates_df$combined_dem_pred <- socpan_vote_estimates_df$pred_dem_vote
socpan_vote_estimates_df$combined_dem_pred[
  is.na(socpan_vote_estimates_df$pred_dem_vote)] <- socpan_vote_estimates_df$logit_predicted_dem_votes[
    is.na(socpan_vote_estimates_df$pred_dem_vote)]

socpan_vote_estimates_df$combined_rep_pred <- socpan_vote_estimates_df$pred_rep_vote
socpan_vote_estimates_df$combined_rep_pred[
  is.na(socpan_vote_estimates_df$pred_rep_vote)] <- socpan_vote_estimates_df$logit_predicted_rep_votes[
    is.na(socpan_vote_estimates_df$pred_rep_vote)]

socpan_vote_estimates_df$combined_cross_model_dem_pred <- socpan_vote_estimates_df$pred_dem_CPonSP
socpan_vote_estimates_df$combined_cross_model_dem_pred[
  is.na(socpan_vote_estimates_df$pred_dem_CPonSP)] <- socpan_vote_estimates_df$logit_predicted_cross_model_dem_vote[
    is.na(socpan_vote_estimates_df$pred_dem_CPonSP)]

# WRITE OUT

write.csv(socpan_vote_estimates_df, paste0(DATA_PATH, "all_vote_estimates.csv"))


