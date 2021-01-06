# 08_logistic_regression_voting_classification.R
# a script that merges all estimated vote choices for all users in the control panel
# and runs a logistic regression model aimed at estimating
# vote choice for users so far unclassified based on their tweets
# also --> outputs tables with logistic regression models for all predicted votes
# DATA IN: vote choice estimates for dem_vote==1 and rep_vote==1 for control panel users,
#          and all available computed demographics
# DATA OUT: latex table with regression model, csv with all demographics and all vote estimates, 
#           plus computed vote estimates from logistic models
# NL, 06/03/19 

library(dplyr)
library(stargazer)

DATA_PATH <- "~/Dropbox/PhD/midterms_paper/tweets/control_panel/data/"
TABLE_PATH <- "~/Dropbox/PhD/midterms_paper/tables/"

# DATA IN
ctrl_panel_dem_vote_estimates_df <- read.csv(paste0(DATA_PATH, "all_models_no_waves_dems.csv"),
                                             stringsAsFactors = F) %>% 
  mutate(X = NULL,
         user_id = as.character(user_id),
         dem_mean = round(rowMeans(select(., prob_dem_trump_model,
                                          prob_dem_kavanaugh_model,
                                          prob_dem_midterms_model,
                                          prob_dem_democrats_model,
                                          prob_dem_republicans_model),
                                   na.rm = T), digits = 3))



ctrl_panel_rep_vote_estimates_df <- read.csv(paste0(DATA_PATH, "all_models_no_waves_reps.csv"),
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



ctrl_panel_socpan_classifier_df <- read.csv(paste0(DATA_PATH, "SPonCP_no_waves_dems.csv"),
                                                stringsAsFactors = F) %>%
  mutate(X = NULL,
         user_id = as.character(user_id),
         SPonCP_dem_mean = round(rowMeans(select(., prob_dem_SPonCP_trump_model,
                                                 prob_dem_SPonCP_kavanaugh_model,
                                                 prob_dem_SPonCP_midterms_model,
                                                 prob_dem_SPonCP_democrats_model,
                                                 prob_dem_SPonCP_republicans_model),
                                          na.rm = T), digits = 3)) %>% 
  select(user_id, prob_dem_SPonCP_trump_model, prob_dem_SPonCP_kavanaugh_model,
         prob_dem_SPonCP_midterms_model, prob_dem_SPonCP_democrats_model,
         prob_dem_SPonCP_republicans_model, SPonCP_dem_mean)

ctrl_panel_all_demographics_df <- read.csv(paste0(DATA_PATH, "ctrl_panel_all_demographics.csv")) %>% 
  mutate(X = NULL,
         user_id = as.character(user_id)) %>% 
  select(user_id, gender, highest_value_race, ideology, ideology_relevant_accounts_followed)

# remove ideology scores for users who follow fewer than 3 ideology-relevant accounts
ctrl_panel_all_demographics_df$ideology[ctrl_panel_all_demographics_df$ideology_relevant_accounts_followed<3] <- NA

# merge datasets

ctrl_panel_vote_estimates_df <- left_join(ctrl_panel_dem_vote_estimates_df, 
                                          ctrl_panel_rep_vote_estimates_df, by = "user_id") %>%
  left_join(ctrl_panel_socpan_classifier_df, by = "user_id") %>% 
  left_join(ctrl_panel_all_demographics_df, by = "user_id") 

ctrl_panel_vote_estimates_df$pred_dem_vote[ctrl_panel_vote_estimates_df$dem_mean>=0.5] <- 1
ctrl_panel_vote_estimates_df$pred_dem_vote[ctrl_panel_vote_estimates_df$dem_mean<0.5] <- 0
ctrl_panel_vote_estimates_df$pred_rep_vote[ctrl_panel_vote_estimates_df$rep_mean>=0.5] <- 1
ctrl_panel_vote_estimates_df$pred_rep_vote[ctrl_panel_vote_estimates_df$rep_mean<0.5] <- 0
ctrl_panel_vote_estimates_df$pred_dem_SPonCP[ctrl_panel_vote_estimates_df$SPonCP_dem_mean>=0.5] <- 1
ctrl_panel_vote_estimates_df$pred_dem_SPonCP[ctrl_panel_vote_estimates_df$SPonCP_dem_mean<0.5] <- 0

# dem model

dem_model <- glm(pred_dem_vote~ideology+gender+highest_value_race,
                 family = "binomial",
                 data = ctrl_panel_vote_estimates_df)

logit_predicted_dem_votes <- round(predict.glm(
  dem_model, newdata = ctrl_panel_vote_estimates_df, type = "response"))

ctrl_panel_vote_estimates_df <- cbind(ctrl_panel_vote_estimates_df, logit_predicted_dem_votes)

# rep model

rep_model <- glm(pred_rep_vote~ideology+gender+highest_value_race,
                 family = "binomial",
                 data = ctrl_panel_vote_estimates_df)

logit_predicted_rep_votes <- round(predict.glm(
  rep_model, newdata = ctrl_panel_vote_estimates_df, type = "response"))

ctrl_panel_vote_estimates_df <- cbind(ctrl_panel_vote_estimates_df, logit_predicted_rep_votes)

# cross_model dem model

cross_model_dem_model <- glm(pred_dem_SPonCP~ideology+gender+highest_value_race,
                             family = "binomial",
                             data = ctrl_panel_vote_estimates_df)

logit_predicted_cross_model_dem_vote <- round(predict.glm(
  cross_model_dem_model, newdata = ctrl_panel_vote_estimates_df, type = "response"))

ctrl_panel_vote_estimates_df <- cbind(ctrl_panel_vote_estimates_df, logit_predicted_cross_model_dem_vote)

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
          out = paste0(TABLE_PATH, "ctrl_panel_logit_models.tex"))

# combined vote choice variables:
# textmodel-based and logistic regression based

ctrl_panel_vote_estimates_df$combined_dem_pred <- ctrl_panel_vote_estimates_df$pred_dem_vote
ctrl_panel_vote_estimates_df$combined_dem_pred[
  is.na(ctrl_panel_vote_estimates_df$pred_dem_vote)] <- ctrl_panel_vote_estimates_df$logit_predicted_dem_votes[
    is.na(ctrl_panel_vote_estimates_df$pred_dem_vote)]

ctrl_panel_vote_estimates_df$combined_rep_pred <- ctrl_panel_vote_estimates_df$pred_rep_vote
ctrl_panel_vote_estimates_df$combined_rep_pred[
  is.na(ctrl_panel_vote_estimates_df$pred_rep_vote)] <- ctrl_panel_vote_estimates_df$logit_predicted_rep_votes[
    is.na(ctrl_panel_vote_estimates_df$pred_rep_vote)]

ctrl_panel_vote_estimates_df$combined_cross_model_dem_pred <- ctrl_panel_vote_estimates_df$pred_dem_SPonCP
ctrl_panel_vote_estimates_df$combined_cross_model_dem_pred[
  is.na(ctrl_panel_vote_estimates_df$pred_dem_SPonCP)] <- ctrl_panel_vote_estimates_df$logit_predicted_cross_model_dem_vote[
    is.na(ctrl_panel_vote_estimates_df$pred_dem_SPonCP)]

# WRITE OUT

write.csv(ctrl_panel_vote_estimates_df, paste0(DATA_PATH, "all_vote_estimates.csv"))

