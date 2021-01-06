# 06_aggregating_votes.R and 08_aggregating_votes.R
# a script which aggregates votes up to target levels for different model specifications, 
# and compares to the popular vote totals in different states
# DATA IN: complete vote estimation csvs for all users for both samples
#          csv with vote totals by house district
# DATA OUT: a csv with all aggregated vote shares for a number of different models, 
#           for the 4 states and the US as a whole

# NL, 07/03/19

SOCPAN_PATH <- "~/Dropbox/PhD/midterms_paper/tweets/panel/making_forecast/data/"
CTRL_PANEL_PATH <- "~/Dropbox/PhD/midterms_paper/tweets/control_panel/data/"
ELECTION_RESULTS_PATH <- "~/Dropbox/PhD/midterms_paper/election_results/"
WEIGHTS_PATH <- "~/Dropbox/PhD/midterms_paper/state_populations.csv"

library(dplyr)
library(reshape)
library(beepr)


# DATA IN 

# 1 - socpan vote estimates
socpan_vote_estimates_df <- read.csv(paste0(SOCPAN_PATH, "all_vote_estimates.csv"),
                                     stringsAsFactors = F) %>% 
  mutate(X = NULL,
         user_id = as.character(user_id))

# 2 - ctrl_panel vote estimates
ctrl_panel_vote_estimates_df <- read.csv(paste0(CTRL_PANEL_PATH, "all_vote_estimates.csv"),
                                         stringsAsFactors = F) %>%
  mutate(X = NULL,
         user_id = as.character(user_id))

# 3 - election results
election_results_df <- read.csv(paste0(ELECTION_RESULTS_PATH, "2018 House Popular Vote Tracker.csv"),
                                stringsAsFactors = F)

election_results_df$State[1] <- "USA"
election_results_df <- rbind(election_results_df[1,], election_results_df[3:nrow(election_results_df),])
colnames(election_results_df) <- c("state", "cd", "cooks_pvi",
                                   "winning_person", "party", "dem_votes",
                                   "rep_votes", "other_votes", "dem_perc",
                                   "rep_perc", "other_perc", "dem_margin",
                                   "clinton_margin_2016", "swing_from_2016",
                                   "total_votes_cast_2016", "raw_votes_vs_2016",
                                   "final")

election_results_df$dem_votes <- as.numeric(gsub(",", "", election_results_df$dem_votes))
election_results_df$rep_votes <- as.numeric(gsub(",", "", election_results_df$rep_votes))
election_results_df$other_votes <- as.numeric(gsub(",", "", election_results_df$other_votes))

election_results_df$dem_perc <- as.numeric(gsub("%", "", election_results_df$dem_perc))
election_results_df$rep_perc <- as.numeric(gsub("%", "", election_results_df$rep_perc))
election_results_df$other_perc <- as.numeric(gsub("%", "", election_results_df$other_perc))

# another DF with popular vote by state

state_level_popular_votes_df <- NULL

for(i in 1:length(unique(election_results_df$state))){
  
  # subset
  pos1_df <- election_results_df %>% 
    filter(state == unique(election_results_df$state)[i])
  
  newline_df <- data.frame(state = unique(election_results_df$state)[i],
                           dem_votes = sum(pos1_df$dem_votes),
                           rep_votes = sum(pos1_df$rep_votes),
                           dem_perc = (sum(pos1_df$dem_votes)/
                                         (sum(pos1_df$dem_votes)+
                                         sum(pos1_df$rep_votes)))*100,
                           rep_perc = (sum(pos1_df$rep_votes)/
                                         (sum(pos1_df$dem_votes)+
                                            sum(pos1_df$rep_votes)))*100)
  
  state_level_popular_votes_df <- rbind(state_level_popular_votes_df, newline_df)
  
}

rm(pos1_df)

# 4 - state populations for weights
state_populations_df <- read.csv(WEIGHTS_PATH, stringsAsFactors = F)

# ANALYSIS
# create variables for weighting
# by combining proportions of all cases into a DF

socpan_state_proportions <- round(prop.table(table(socpan_vote_estimates_df$admin))*100, digits = 2)
ctrl_panel_state_proportiosn <- round(prop.table(table(ctrl_panel_vote_estimates_df$state))*100, digits = 2)

weights_df <- NULL

for(i in 1:length(state.name)){
  
  newline_df <- data.frame(state = as.character(state.name[i]),
                           real_pop_perc = state_populations_df$perc_of_total_us_pop[
                             state_populations_df$state==state.name[i]],
                           socpan_pop_perc = unname(socpan_state_proportions[
                             names(socpan_state_proportions)==state.name[i]]),
                           ctrl_panel_pop_perc = unname(ctrl_panel_state_proportiosn[
                             names(ctrl_panel_state_proportiosn)==state.name[i]]))
  
  weights_df <- rbind(weights_df, newline_df)
}

# generate weighting factors for all states

weights_df$socpan_weighting_factor <- weights_df$real_pop_perc/weights_df$socpan_pop_perc
weights_df$ctrl_panel_weighting_factor <- weights_df$real_pop_perc/weights_df$ctrl_panel_pop_perc
weights_df$state <- as.character(weights_df$state)

# NOW: to calculate weighted numbers by state:
# 1 - cycle through the 50 states
# 2 - set number of users to sample, 
#     this will be n(from state in sample) * weighting factor for state for sample
# 3 - draw a random sample of users *with* replacement of n(see 2)
#     write the numbers (vote choices for all model specs) out to a data frame for that state
# 4 - repeat 1000-10000 times
# 5 - record the average numbers over all iterations, as well as CIs
# 6 - write to state-level data frames

set.seed(1234)

ctrl_panel_weighted_state_numbers_df <- NULL
socpan_weighted_state_numbers_df <- NULL

for(i in 1:nrow(weights_df)){
  
  print(paste0("NOW running weighted vote tally estimation for the state of ", weights_df$state[i],
               ". Time: ", Sys.time()))
  
  # subset ctrl_panel for current state
  pos1_df <- ctrl_panel_vote_estimates_df %>% 
    filter(state == weights_df$state[i])
  # target sample number
  target_n_ctrl_panel <- ceiling(nrow(pos1_df)*weights_df$ctrl_panel_weighting_factor[i])
  
  # subset socpan for current state
  pos2_df <- socpan_vote_estimates_df %>% 
    filter(admin == weights_df$state[i])
  # target sample number 
  target_n_socpan <- ceiling(nrow(pos2_df)*weights_df$socpan_weighting_factor[i])

  # now, draw samples for 10k iterations and write out the distributions
  ctrl_panel_state_bootstrap_df <- NULL
  socpan_state_bootstrap_df <- NULL
  
  for(j in 1:2000){
    
    # draw sample for ctrl panel, write out metrics
    
    print(paste0("On bootstrap iteration ", j, " out of 2000 for the state of ", 
                  weights_df$state[i], ". Time: ", Sys.time()))
    
    ctrl_panel_sample_df <- sample_n(pos1_df, size = target_n_ctrl_panel,
                                   replace = T)
    
    ctrl_panel_newline_df <- data.frame(iteration = j,
                                        pred_dem_vote = sum(ctrl_panel_sample_df$pred_dem_vote, na.rm = T),
                                        pred_rep_vote = sum(ctrl_panel_sample_df$pred_rep_vote, na.rm = T),
                                        pred_dem_SPonCP = sum(ctrl_panel_sample_df$pred_dem_SPonCP, na.rm = T),
                                        pred_rep_SPonCP = sum(ctrl_panel_sample_df$pred_dem_SPonCP==0, na.rm = T),
                                        combined_dem_pred = sum(ctrl_panel_sample_df$combined_dem_pred, na.rm = T),
                                        combined_rep_pred = sum(ctrl_panel_sample_df$combined_rep_pred, na.rm = T),
                                        combined_cross_model_dem_pred = sum(
                                          ctrl_panel_sample_df$combined_cross_model_dem_pred, na.rm = T),
                                        combined_cross_model_rep_pred = sum(
                                          ctrl_panel_sample_df$combined_cross_model_dem_pred==0, na.rm = T),
                                        trump_dem = length(ctrl_panel_sample_df$prob_dem_trump_model[
                                          ctrl_panel_sample_df$prob_dem_trump_model>=0.5 &
                                            !is.na(ctrl_panel_sample_df$prob_dem_trump_model)]),
                                        trump_rep = length(ctrl_panel_sample_df$prob_rep_trump_model[
                                          ctrl_panel_sample_df$prob_rep_trump_model>=0.5 &
                                            !is.na(ctrl_panel_sample_df$prob_rep_trump_model)]),
                                        kavanaugh_dem = length(ctrl_panel_sample_df$prob_dem_kavanaugh_model[
                                          ctrl_panel_sample_df$prob_dem_kavanaugh_model>=0.5 &
                                            !is.na(ctrl_panel_sample_df$prob_dem_kavanaugh_model)]),
                                        kavanaugh_rep = length(ctrl_panel_sample_df$prob_rep_kavanaugh_model[
                                          ctrl_panel_sample_df$prob_rep_kavanaugh_model>=0.5 &
                                            !is.na(ctrl_panel_sample_df$prob_rep_kavanaugh_model)]),
                                        republicans_dem = length(ctrl_panel_sample_df$prob_dem_republicans_model[
                                          ctrl_panel_sample_df$prob_dem_republicans_model>=0.5 &
                                            !is.na(ctrl_panel_sample_df$prob_dem_republicans_model)]),
                                        republicans_rep = length(ctrl_panel_sample_df$prob_rep_republicans_model[
                                          ctrl_panel_sample_df$prob_rep_republicans_model>=0.5 &
                                            !is.na(ctrl_panel_sample_df$prob_rep_republicans_model)]),
                                        X_sample_trump_dem_SPonCP = length(ctrl_panel_sample_df$prob_dem_SPonCP_trump_model[
                                          ctrl_panel_sample_df$prob_dem_SPonCP_trump_model>=0.5 &
                                            !is.na(ctrl_panel_sample_df$prob_dem_SPonCP_trump_model)]),
                                        X_sample_trump_rep_SPonCP = length(ctrl_panel_sample_df$prob_dem_SPonCP_trump_model[
                                          ctrl_panel_sample_df$prob_dem_SPonCP_trump_model<0.5 &
                                            !is.na(ctrl_panel_sample_df$prob_dem_SPonCP_trump_model)]),
                                        X_sample_kavanaugh_dem_SPonCP = length(ctrl_panel_sample_df$prob_dem_SPonCP_kavanaugh_model[
                                          ctrl_panel_sample_df$prob_dem_SPonCP_kavanaugh_model>=0.5 &
                                            !is.na(ctrl_panel_sample_df$prob_dem_SPonCP_kavanaugh_model)]),
                                        X_sample_kavanaugh_rep_SPonCP = length(ctrl_panel_sample_df$prob_dem_SPonCP_kavanaugh_model[
                                          ctrl_panel_sample_df$prob_dem_SPonCP_kavanaugh_model<0.5 &
                                            !is.na(ctrl_panel_sample_df$prob_dem_SPonCP_kavanaugh_model)]),
                                        X_sample_republicans_dem_SPonCP = length(ctrl_panel_sample_df$prob_dem_SPonCP_republicans_model[
                                          ctrl_panel_sample_df$prob_dem_SPonCP_republicans_model>=0.5 &
                                            !is.na(ctrl_panel_sample_df$prob_dem_SPonCP_republicans_model)]),
                                        X_sample_republicans_rep_SPonCP = length(ctrl_panel_sample_df$prob_dem_SPonCP_republicans_model[
                                          ctrl_panel_sample_df$prob_dem_SPonCP_republicans_model<0.5 &
                                            !is.na(ctrl_panel_sample_df$prob_dem_SPonCP_republicans_model)]))
    
    socpan_sample_df <- sample_n(pos2_df, size = target_n_socpan, 
                                 replace = T)
    
    socpan_newline_df <- data.frame(iteration = j,
                                    pred_dem_vote = sum(socpan_sample_df$pred_dem_vote, na.rm = T),
                                    pred_rep_vote = sum(socpan_sample_df$pred_rep_vote, na.rm = T),
                                    pred_dem_CPonSP = sum(socpan_sample_df$pred_dem_CPonSP, na.rm = T),
                                    pred_rep_CPonSP = sum(socpan_sample_df$pred_dem_CPonSP==0, na.rm = T),
                                    combined_dem_pred = sum(socpan_sample_df$combined_dem_pred, na.rm = T),
                                    combined_rep_pred = sum(socpan_sample_df$combined_rep_pred, na.rm = T),
                                    combined_cross_model_dem_pred = sum(
                                      socpan_sample_df$combined_cross_model_dem_pred, na.rm = T),
                                    combined_cross_model_rep_pred = sum(
                                      socpan_sample_df$combined_cross_model_dem_pred==0, na.rm = T),
                                    trump_dem = length(socpan_sample_df$prob_dem_trump_model[
                                      socpan_sample_df$prob_dem_trump_model>=0.5 &
                                        !is.na(socpan_sample_df$prob_dem_trump_model)]),
                                    trump_rep = length(socpan_sample_df$prob_rep_trump_model[
                                      socpan_sample_df$prob_rep_trump_model>=0.5 &
                                        !is.na(socpan_sample_df$prob_rep_trump_model)]),
                                    kavanaugh_dem = length(socpan_sample_df$prob_dem_kavanaugh_model[
                                      socpan_sample_df$prob_dem_kavanaugh_model>=0.5 &
                                        !is.na(socpan_sample_df$prob_dem_kavanaugh_model)]),
                                    kavanaugh_rep = length(socpan_sample_df$prob_rep_kavanaugh_model[
                                      socpan_sample_df$prob_rep_kavanaugh_model>=0.5 &
                                        !is.na(socpan_sample_df$prob_rep_kavanaugh_model)]),
                                    republicans_dem = length(socpan_sample_df$prob_dem_republicans_model[
                                      socpan_sample_df$prob_dem_republicans_model>=0.5 &
                                        !is.na(socpan_sample_df$prob_dem_republicans_model)]),
                                    republicans_rep = length(socpan_sample_df$prob_rep_republicans_model[
                                      socpan_sample_df$prob_rep_republicans_model>=0.5 &
                                        !is.na(socpan_sample_df$prob_rep_republicans_model)]),
                                    X_sample_trump_dem_CPonSP = length(socpan_sample_df$prob_dem_CPonSP_trump_model[
                                      socpan_sample_df$prob_dem_CPonSP_trump_model>=0.5 &
                                        !is.na(socpan_sample_df$prob_dem_CPonSP_trump_model)]),
                                    X_sample_trump_rep_CPonSP = length(socpan_sample_df$prob_dem_CPonSP_trump_model[
                                      socpan_sample_df$prob_dem_CPonSP_trump_model<0.5 &
                                        !is.na(socpan_sample_df$prob_dem_CPonSP_trump_model)]),
                                    X_sample_kavanaugh_dem_CPonSP = length(socpan_sample_df$prob_dem_CPonSP_kavanaugh_model[
                                      socpan_sample_df$prob_dem_CPonSP_kavanaugh_model>=0.5 &
                                        !is.na(socpan_sample_df$prob_dem_CPonSP_kavanaugh_model)]),
                                    X_sample_kavanaugh_rep_CPonSP = length(socpan_sample_df$prob_dem_CPonSP_kavanaugh_model[
                                      socpan_sample_df$prob_dem_CPonSP_kavanaugh_model<0.5 &
                                        !is.na(socpan_sample_df$prob_dem_CPonSP_kavanaugh_model)]),
                                    X_sample_republicans_dem_CPonSP = length(socpan_sample_df$prob_dem_CPonSP_republicans_model[
                                      socpan_sample_df$prob_dem_CPonSP_republicans_model>=0.5 &
                                        !is.na(socpan_sample_df$prob_dem_CPonSP_republicans_model)]),
                                    X_sample_republicans_rep_CPonSP = length(socpan_sample_df$prob_dem_CPonSP_republicans_model[
                                      socpan_sample_df$prob_dem_CPonSP_republicans_model<0.5 &
                                        !is.na(socpan_sample_df$prob_dem_CPonSP_republicans_model)]))
    
    # add to bootstrapped DF
    socpan_state_bootstrap_df <- rbind(socpan_state_bootstrap_df, socpan_newline_df)
    ctrl_panel_state_bootstrap_df <- rbind(ctrl_panel_state_bootstrap_df, ctrl_panel_newline_df)
    #rm(ctrl_panel_newline_df, socpan_newline_df)
    # flush memory
    #gc()
  }
  
  rm(ctrl_panel_newline_df, socpan_newline_df)
  gc()
  
  ctrl_panel_state_means_df <- as.data.frame(colMeans(ctrl_panel_state_bootstrap_df[
    2:length(ctrl_panel_state_bootstrap_df)], na.rm = T))
  ctrl_panel_state_means_df$varnames <- rownames(ctrl_panel_state_means_df)
  rownames(ctrl_panel_state_means_df) <- NULL
  ctrl_panel_state_means_df$state <- weights_df$state[i]
  colnames(ctrl_panel_state_means_df) <- c("value", "varnames", "state")
  
  newline_ctrl_panel_df <- reshape(ctrl_panel_state_means_df,
                                  idvar = "state",
                                  timevar = "varnames",
                                  direction = "wide")
  
  socpan_state_means_df <- as.data.frame(colMeans(socpan_state_bootstrap_df[
    2:length(socpan_state_bootstrap_df)], na.rm = T))
  socpan_state_means_df$varnames <- rownames(socpan_state_means_df)
  rownames(socpan_state_means_df) <- NULL
  socpan_state_means_df$state <- weights_df$state[i]
  colnames(socpan_state_means_df) <- c("value", "varnames", "state")
  
  newline_socpan_df <- reshape(socpan_state_means_df,
                                   idvar = "state",
                                   timevar = "varnames",
                                   direction = "wide")
 
  # add on to big DFs
  ctrl_panel_weighted_state_numbers_df <- rbind(ctrl_panel_weighted_state_numbers_df, newline_ctrl_panel_df)
  socpan_weighted_state_numbers_df <- rbind(socpan_weighted_state_numbers_df, newline_socpan_df)
  
}

# NEW DATA FRAMES FROM WHICH TO MAKE BAR PLOTS

# 1 - USA
# data structure: 
# model - actual result, unweighted Trump, unweighted Kav, unweighted Reps,
#         unweighted standard, unweighted cross-model (for both models),
#         and the same weighted
#         plus everything weighted, plus the logistic regression model additions
# dem perc
# rep perc
# flip the axes, sorted. 

target_states <- c("USA", "California", "Texas", "New York", "Florida")

state_level_aggregations_df <- NULL

for(i in 1:length(target_states)){
  
  actual_result_df <- data.frame(state = target_states[i],
                                 model = "actual_result",
                                 dem_perc = state_level_popular_votes_df$dem_perc[
                                   state_level_popular_votes_df$state==target_states[i]],
                                 rep_perc = state_level_popular_votes_df$rep_perc[
                                   state_level_popular_votes_df$state==target_states[i]])
  
  if(target_states[i]=="USA"){
    pos1_df <- ctrl_panel_vote_estimates_df
    pos2_df <- socpan_vote_estimates_df
  } else {
    pos1_df <- ctrl_panel_vote_estimates_df %>% filter(
      state==target_states[i]
    )
    pos2_df <- socpan_vote_estimates_df %>% filter(
      admin==target_states[i]
    )
  }
  
  ######################
  ### UNWEIGHTED TRUMP
  ######################
  
  unweighted_trump_ctrl_panel_df <- data.frame(state = target_states[i],
                                               model = "unweighted_trump_model_ctrl_panel",
                                               dem_perc = (length(pos1_df$prob_dem_trump_model[pos1_df$prob_dem_trump_model>0.5 & 
                                                                                                 !is.na(pos1_df$prob_dem_trump_model)])/
                                                             (length(pos1_df$prob_dem_trump_model[pos1_df$prob_dem_trump_model>0.5 & !is.na(pos1_df$prob_dem_trump_model)])+
                                                                length(pos1_df$prob_dem_trump_model[pos1_df$prob_dem_trump_model<=0.5 & !is.na(pos1_df$prob_dem_trump_model)])))*100,
                                               rep_perc = 100-((length(pos1_df$prob_dem_trump_model[pos1_df$prob_dem_trump_model>0.5 & 
                                                                                                 !is.na(pos1_df$prob_dem_trump_model)])/
                                                             (length(pos1_df$prob_dem_trump_model[pos1_df$prob_dem_trump_model>0.5 & !is.na(pos1_df$prob_dem_trump_model)])+
                                                                length(pos1_df$prob_dem_trump_model[pos1_df$prob_dem_trump_model<=0.5 & !is.na(pos1_df$prob_dem_trump_model)])))*100))
  
  
  unweighted_trump_socpan_df <- data.frame(state = target_states[i],
                                               model = "unweighted_trump_model_socpan",
                                               dem_perc = (length(pos2_df$prob_dem_trump_model[pos2_df$prob_dem_trump_model>0.5 & 
                                                                                                 !is.na(pos2_df$prob_dem_trump_model)])/
                                                             (length(pos2_df$prob_dem_trump_model[pos2_df$prob_dem_trump_model>0.5 & !is.na(pos2_df$prob_dem_trump_model)])+
                                                                length(pos2_df$prob_dem_trump_model[pos2_df$prob_dem_trump_model<=0.5 & !is.na(pos2_df$prob_dem_trump_model)])))*100,
                                               rep_perc = 100-((length(pos2_df$prob_dem_trump_model[pos2_df$prob_dem_trump_model>0.5 & 
                                                                                                      !is.na(pos2_df$prob_dem_trump_model)])/
                                                                  (length(pos2_df$prob_dem_trump_model[pos2_df$prob_dem_trump_model>0.5 & !is.na(pos2_df$prob_dem_trump_model)])+
                                                                     length(pos2_df$prob_dem_trump_model[pos2_df$prob_dem_trump_model<=0.5 & !is.na(pos2_df$prob_dem_trump_model)])))*100))
  
  

  ######################
  ### UNWEIGHTED kavanaugh
  ######################
  
  unweighted_kavanaugh_ctrl_panel_df <- data.frame(state = target_states[i],
                                               model = "unweighted_kavanaugh_model_ctrl_panel",
                                               dem_perc = (length(pos1_df$prob_dem_kavanaugh_model[pos1_df$prob_dem_kavanaugh_model>0.5 & 
                                                                                                 !is.na(pos1_df$prob_dem_kavanaugh_model)])/
                                                             (length(pos1_df$prob_dem_kavanaugh_model[pos1_df$prob_dem_kavanaugh_model>0.5 & !is.na(pos1_df$prob_dem_kavanaugh_model)])+
                                                                length(pos1_df$prob_dem_kavanaugh_model[pos1_df$prob_dem_kavanaugh_model<=0.5 & !is.na(pos1_df$prob_dem_kavanaugh_model)])))*100,
                                               rep_perc = 100-((length(pos1_df$prob_dem_kavanaugh_model[pos1_df$prob_dem_kavanaugh_model>0.5 & 
                                                                                                      !is.na(pos1_df$prob_dem_kavanaugh_model)])/
                                                                  (length(pos1_df$prob_dem_kavanaugh_model[pos1_df$prob_dem_kavanaugh_model>0.5 & !is.na(pos1_df$prob_dem_kavanaugh_model)])+
                                                                     length(pos1_df$prob_dem_kavanaugh_model[pos1_df$prob_dem_kavanaugh_model<=0.5 & !is.na(pos1_df$prob_dem_kavanaugh_model)])))*100))
  
  
  unweighted_kavanaugh_socpan_df <- data.frame(state = target_states[i],
                                           model = "unweighted_kavanaugh_model_socpan",
                                           dem_perc = (length(pos2_df$prob_dem_kavanaugh_model[pos2_df$prob_dem_kavanaugh_model>0.5 & 
                                                                                             !is.na(pos2_df$prob_dem_kavanaugh_model)])/
                                                         (length(pos2_df$prob_dem_kavanaugh_model[pos2_df$prob_dem_kavanaugh_model>0.5 & !is.na(pos2_df$prob_dem_kavanaugh_model)])+
                                                            length(pos2_df$prob_dem_kavanaugh_model[pos2_df$prob_dem_kavanaugh_model<=0.5 & !is.na(pos2_df$prob_dem_kavanaugh_model)])))*100,
                                           rep_perc = 100-((length(pos2_df$prob_dem_kavanaugh_model[pos2_df$prob_dem_kavanaugh_model>0.5 & 
                                                                                                  !is.na(pos2_df$prob_dem_kavanaugh_model)])/
                                                              (length(pos2_df$prob_dem_kavanaugh_model[pos2_df$prob_dem_kavanaugh_model>0.5 & !is.na(pos2_df$prob_dem_kavanaugh_model)])+
                                                                 length(pos2_df$prob_dem_kavanaugh_model[pos2_df$prob_dem_kavanaugh_model<=0.5 & !is.na(pos2_df$prob_dem_kavanaugh_model)])))*100))
  
  
  ######################
  ### UNWEIGHTED republicans
  ######################
  
  unweighted_republicans_ctrl_panel_df <- data.frame(state = target_states[i],
                                                   model = "unweighted_republicans_model_ctrl_panel",
                                                   dem_perc = (length(pos1_df$prob_dem_republicans_model[pos1_df$prob_dem_republicans_model>0.5 & 
                                                                                                         !is.na(pos1_df$prob_dem_republicans_model)])/
                                                                 (length(pos1_df$prob_dem_republicans_model[pos1_df$prob_dem_republicans_model>0.5 & !is.na(pos1_df$prob_dem_republicans_model)])+
                                                                    length(pos1_df$prob_dem_republicans_model[pos1_df$prob_dem_republicans_model<=0.5 & !is.na(pos1_df$prob_dem_republicans_model)])))*100,
                                                   rep_perc = 100-((length(pos1_df$prob_dem_republicans_model[pos1_df$prob_dem_republicans_model>0.5 & 
                                                                                                              !is.na(pos1_df$prob_dem_republicans_model)])/
                                                                      (length(pos1_df$prob_dem_republicans_model[pos1_df$prob_dem_republicans_model>0.5 & !is.na(pos1_df$prob_dem_republicans_model)])+
                                                                         length(pos1_df$prob_dem_republicans_model[pos1_df$prob_dem_republicans_model<=0.5 & !is.na(pos1_df$prob_dem_republicans_model)])))*100))
  
  
  unweighted_republicans_socpan_df <- data.frame(state = target_states[i],
                                               model = "unweighted_republicans_model_socpan",
                                               dem_perc = (length(pos2_df$prob_dem_republicans_model[pos2_df$prob_dem_republicans_model>0.5 & 
                                                                                                     !is.na(pos2_df$prob_dem_republicans_model)])/
                                                             (length(pos2_df$prob_dem_republicans_model[pos2_df$prob_dem_republicans_model>0.5 & !is.na(pos2_df$prob_dem_republicans_model)])+
                                                                length(pos2_df$prob_dem_republicans_model[pos2_df$prob_dem_republicans_model<=0.5 & !is.na(pos2_df$prob_dem_republicans_model)])))*100,
                                               rep_perc = 100-((length(pos2_df$prob_dem_republicans_model[pos2_df$prob_dem_republicans_model>0.5 & 
                                                                                                          !is.na(pos2_df$prob_dem_republicans_model)])/
                                                                  (length(pos2_df$prob_dem_republicans_model[pos2_df$prob_dem_republicans_model>0.5 & !is.na(pos2_df$prob_dem_republicans_model)])+
                                                                     length(pos2_df$prob_dem_republicans_model[pos2_df$prob_dem_republicans_model<=0.5 & !is.na(pos2_df$prob_dem_republicans_model)])))*100))
  
  ######################
  ### UNWEIGHTED TRUMP cross-model
  ######################
  
  unweighted_trump_cross_model_ctrl_panel_df <- data.frame(state = target_states[i],
                                               model = "unweighted_trump_cross_model_ctrl_panel",
                                               dem_perc = (length(pos1_df$prob_dem_SPonCP_trump_model[pos1_df$prob_dem_SPonCP_trump_model>0.5 & 
                                                                                                 !is.na(pos1_df$prob_dem_SPonCP_trump_model)])/
                                                             (length(pos1_df$prob_dem_SPonCP_trump_model[pos1_df$prob_dem_SPonCP_trump_model>0.5 & !is.na(pos1_df$prob_dem_SPonCP_trump_model)])+
                                                                length(pos1_df$prob_dem_SPonCP_trump_model[pos1_df$prob_dem_SPonCP_trump_model<=0.5 & !is.na(pos1_df$prob_dem_SPonCP_trump_model)])))*100,
                                               rep_perc = 100-((length(pos1_df$prob_dem_SPonCP_trump_model[pos1_df$prob_dem_SPonCP_trump_model>0.5 & 
                                                                                                      !is.na(pos1_df$prob_dem_SPonCP_trump_model)])/
                                                                  (length(pos1_df$prob_dem_SPonCP_trump_model[pos1_df$prob_dem_SPonCP_trump_model>0.5 & !is.na(pos1_df$prob_dem_SPonCP_trump_model)])+
                                                                     length(pos1_df$prob_dem_SPonCP_trump_model[pos1_df$prob_dem_SPonCP_trump_model<=0.5 & !is.na(pos1_df$prob_dem_SPonCP_trump_model)])))*100))
  
  
  unweighted_trump_cross_model_socpan_df <- data.frame(state = target_states[i],
                                           model = "unweighted_trump_cross_model_socpan",
                                           dem_perc = (length(pos2_df$prob_dem_CPonSP_trump_model[pos2_df$prob_dem_CPonSP_trump_model>0.5 & 
                                                                                             !is.na(pos2_df$prob_dem_CPonSP_trump_model)])/
                                                         (length(pos2_df$prob_dem_CPonSP_trump_model[pos2_df$prob_dem_CPonSP_trump_model>0.5 & !is.na(pos2_df$prob_dem_CPonSP_trump_model)])+
                                                            length(pos2_df$prob_dem_CPonSP_trump_model[pos2_df$prob_dem_CPonSP_trump_model<=0.5 & !is.na(pos2_df$prob_dem_CPonSP_trump_model)])))*100,
                                           rep_perc = 100-((length(pos2_df$prob_dem_CPonSP_trump_model[pos2_df$prob_dem_CPonSP_trump_model>0.5 & 
                                                                                                  !is.na(pos2_df$prob_dem_CPonSP_trump_model)])/
                                                              (length(pos2_df$prob_dem_CPonSP_trump_model[pos2_df$prob_dem_CPonSP_trump_model>0.5 & !is.na(pos2_df$prob_dem_CPonSP_trump_model)])+
                                                                 length(pos2_df$prob_dem_CPonSP_trump_model[pos2_df$prob_dem_CPonSP_trump_model<=0.5 & !is.na(pos2_df$prob_dem_CPonSP_trump_model)])))*100))
  
  
  ######################
  ### UNWEIGHTED kavanaugh cross-model
  ######################
  
  unweighted_kavanaugh_cross_model_ctrl_panel_df <- data.frame(state = target_states[i],
                                                           model = "unweighted_kavanaugh_cross_model_ctrl_panel",
                                                           dem_perc = (length(pos1_df$prob_dem_SPonCP_kavanaugh_model[pos1_df$prob_dem_SPonCP_kavanaugh_model>0.5 & 
                                                                                                                    !is.na(pos1_df$prob_dem_SPonCP_kavanaugh_model)])/
                                                                         (length(pos1_df$prob_dem_SPonCP_kavanaugh_model[pos1_df$prob_dem_SPonCP_kavanaugh_model>0.5 & !is.na(pos1_df$prob_dem_SPonCP_kavanaugh_model)])+
                                                                            length(pos1_df$prob_dem_SPonCP_kavanaugh_model[pos1_df$prob_dem_SPonCP_kavanaugh_model<=0.5 & !is.na(pos1_df$prob_dem_SPonCP_kavanaugh_model)])))*100,
                                                           rep_perc = 100-((length(pos1_df$prob_dem_SPonCP_kavanaugh_model[pos1_df$prob_dem_SPonCP_kavanaugh_model>0.5 & 
                                                                                                                         !is.na(pos1_df$prob_dem_SPonCP_kavanaugh_model)])/
                                                                              (length(pos1_df$prob_dem_SPonCP_kavanaugh_model[pos1_df$prob_dem_SPonCP_kavanaugh_model>0.5 & !is.na(pos1_df$prob_dem_SPonCP_kavanaugh_model)])+
                                                                                 length(pos1_df$prob_dem_SPonCP_kavanaugh_model[pos1_df$prob_dem_SPonCP_kavanaugh_model<=0.5 & !is.na(pos1_df$prob_dem_SPonCP_kavanaugh_model)])))*100))
  
  
  unweighted_kavanaugh_cross_model_socpan_df <- data.frame(state = target_states[i],
                                                       model = "unweighted_kavanaugh_cross_model_socpan",
                                                       dem_perc = (length(pos2_df$prob_dem_CPonSP_kavanaugh_model[pos2_df$prob_dem_CPonSP_kavanaugh_model>0.5 & 
                                                                                                                !is.na(pos2_df$prob_dem_CPonSP_kavanaugh_model)])/
                                                                     (length(pos2_df$prob_dem_CPonSP_kavanaugh_model[pos2_df$prob_dem_CPonSP_kavanaugh_model>0.5 & !is.na(pos2_df$prob_dem_CPonSP_kavanaugh_model)])+
                                                                        length(pos2_df$prob_dem_CPonSP_kavanaugh_model[pos2_df$prob_dem_CPonSP_kavanaugh_model<=0.5 & !is.na(pos2_df$prob_dem_CPonSP_kavanaugh_model)])))*100,
                                                       rep_perc = 100-((length(pos2_df$prob_dem_CPonSP_kavanaugh_model[pos2_df$prob_dem_CPonSP_kavanaugh_model>0.5 & 
                                                                                                                     !is.na(pos2_df$prob_dem_CPonSP_kavanaugh_model)])/
                                                                          (length(pos2_df$prob_dem_CPonSP_kavanaugh_model[pos2_df$prob_dem_CPonSP_kavanaugh_model>0.5 & !is.na(pos2_df$prob_dem_CPonSP_kavanaugh_model)])+
                                                                             length(pos2_df$prob_dem_CPonSP_kavanaugh_model[pos2_df$prob_dem_CPonSP_kavanaugh_model<=0.5 & !is.na(pos2_df$prob_dem_CPonSP_kavanaugh_model)])))*100))
  
  ######################
  ### UNWEIGHTED republicans cross-model
  ######################
  
  unweighted_republicans_cross_model_ctrl_panel_df <- data.frame(state = target_states[i],
                                                               model = "unweighted_republicans_cross_model_ctrl_panel",
                                                               dem_perc = (length(pos1_df$prob_dem_SPonCP_republicans_model[pos1_df$prob_dem_SPonCP_republicans_model>0.5 & 
                                                                                                                            !is.na(pos1_df$prob_dem_SPonCP_republicans_model)])/
                                                                             (length(pos1_df$prob_dem_SPonCP_republicans_model[pos1_df$prob_dem_SPonCP_republicans_model>0.5 & !is.na(pos1_df$prob_dem_SPonCP_republicans_model)])+
                                                                                length(pos1_df$prob_dem_SPonCP_republicans_model[pos1_df$prob_dem_SPonCP_republicans_model<=0.5 & !is.na(pos1_df$prob_dem_SPonCP_republicans_model)])))*100,
                                                               rep_perc = 100-((length(pos1_df$prob_dem_SPonCP_republicans_model[pos1_df$prob_dem_SPonCP_republicans_model>0.5 & 
                                                                                                                                 !is.na(pos1_df$prob_dem_SPonCP_republicans_model)])/
                                                                                  (length(pos1_df$prob_dem_SPonCP_republicans_model[pos1_df$prob_dem_SPonCP_republicans_model>0.5 & !is.na(pos1_df$prob_dem_SPonCP_republicans_model)])+
                                                                                     length(pos1_df$prob_dem_SPonCP_republicans_model[pos1_df$prob_dem_SPonCP_republicans_model<=0.5 & !is.na(pos1_df$prob_dem_SPonCP_republicans_model)])))*100))
  
  
  unweighted_republicans_cross_model_socpan_df <- data.frame(state = target_states[i],
                                                           model = "unweighted_republicans_cross_model_socpan",
                                                           dem_perc = (length(pos2_df$prob_dem_CPonSP_republicans_model[pos2_df$prob_dem_CPonSP_republicans_model>0.5 & 
                                                                                                                        !is.na(pos2_df$prob_dem_CPonSP_republicans_model)])/
                                                                         (length(pos2_df$prob_dem_CPonSP_republicans_model[pos2_df$prob_dem_CPonSP_republicans_model>0.5 & !is.na(pos2_df$prob_dem_CPonSP_republicans_model)])+
                                                                            length(pos2_df$prob_dem_CPonSP_republicans_model[pos2_df$prob_dem_CPonSP_republicans_model<=0.5 & !is.na(pos2_df$prob_dem_CPonSP_republicans_model)])))*100,
                                                           rep_perc = 100-((length(pos2_df$prob_dem_CPonSP_republicans_model[pos2_df$prob_dem_CPonSP_republicans_model>0.5 & 
                                                                                                                             !is.na(pos2_df$prob_dem_CPonSP_republicans_model)])/
                                                                              (length(pos2_df$prob_dem_CPonSP_republicans_model[pos2_df$prob_dem_CPonSP_republicans_model>0.5 & !is.na(pos2_df$prob_dem_CPonSP_republicans_model)])+
                                                                                 length(pos2_df$prob_dem_CPonSP_republicans_model[pos2_df$prob_dem_CPonSP_republicans_model<=0.5 & !is.na(pos2_df$prob_dem_CPonSP_republicans_model)])))*100))
  
  
  
  ######################
  ### UNWEIGHTED MEAN
  ######################
  
  unweighted_mean_ctrl_panel_df <- data.frame(state = target_states[i],
                                                     model = "unweighted_mean_model_ctrl_panel",
                                                     dem_perc = (length(pos1_df$dem_mean[pos1_df$dem_mean>0.5 & 
                                                                                                             !is.na(pos1_df$dem_mean)])/
                                                                   (length(pos1_df$dem_mean[pos1_df$dem_mean>0.5 & !is.na(pos1_df$dem_mean)])+
                                                                      length(pos1_df$dem_mean[pos1_df$dem_mean<=0.5 & !is.na(pos1_df$dem_mean)])))*100,
                                                     rep_perc = 100-((length(pos1_df$dem_mean[pos1_df$dem_mean>0.5 & 
                                                                                                                  !is.na(pos1_df$dem_mean)])/
                                                                        (length(pos1_df$dem_mean[pos1_df$dem_mean>0.5 & !is.na(pos1_df$dem_mean)])+
                                                                           length(pos1_df$dem_mean[pos1_df$dem_mean<=0.5 & !is.na(pos1_df$dem_mean)])))*100))
  
  
  unweighted_mean_socpan_df <- data.frame(state = target_states[i],
                                                 model = "unweighted_mean_model_socpan",
                                                 dem_perc = (length(pos2_df$dem_mean[pos2_df$dem_mean>0.5 & 
                                                                                                         !is.na(pos2_df$dem_mean)])/
                                                               (length(pos2_df$dem_mean[pos2_df$dem_mean>0.5 & !is.na(pos2_df$dem_mean)])+
                                                                  length(pos2_df$dem_mean[pos2_df$dem_mean<=0.5 & !is.na(pos2_df$dem_mean)])))*100,
                                                 rep_perc = 100-((length(pos2_df$dem_mean[pos2_df$dem_mean>0.5 & 
                                                                                                              !is.na(pos2_df$dem_mean)])/
                                                                    (length(pos2_df$dem_mean[pos2_df$dem_mean>0.5 & !is.na(pos2_df$dem_mean)])+
                                                                       length(pos2_df$dem_mean[pos2_df$dem_mean<=0.5 & !is.na(pos2_df$dem_mean)])))*100))
  
  ######################
  ### UNWEIGHTED cross-model
  ######################
  
  unweighted_mean_cross_model_ctrl_panel_df <- data.frame(state = target_states[i],
                                              model = "unweighted_mean_cross_model_ctrl_panel",
                                              dem_perc = (length(pos1_df$SPonCP_dem_mean[pos1_df$SPonCP_dem_mean>0.5 & 
                                                                                    !is.na(pos1_df$SPonCP_dem_mean)])/
                                                            (length(pos1_df$SPonCP_dem_mean[pos1_df$SPonCP_dem_mean>0.5 & !is.na(pos1_df$SPonCP_dem_mean)])+
                                                               length(pos1_df$SPonCP_dem_mean[pos1_df$SPonCP_dem_mean<=0.5 & !is.na(pos1_df$SPonCP_dem_mean)])))*100,
                                              rep_perc = 100-((length(pos1_df$SPonCP_dem_mean[pos1_df$SPonCP_dem_mean>0.5 & 
                                                                                         !is.na(pos1_df$SPonCP_dem_mean)])/
                                                                 (length(pos1_df$SPonCP_dem_mean[pos1_df$SPonCP_dem_mean>0.5 & !is.na(pos1_df$SPonCP_dem_mean)])+
                                                                    length(pos1_df$SPonCP_dem_mean[pos1_df$SPonCP_dem_mean<=0.5 & !is.na(pos1_df$SPonCP_dem_mean)])))*100))
  
  
  unweighted_mean_cross_model_socpan_df <- data.frame(state = target_states[i],
                                          model = "unweighted_mean_cross_model_socpan",
                                          dem_perc = (length(pos2_df$CPonSP_dem_mean[pos2_df$CPonSP_dem_mean>0.5 & 
                                                                                !is.na(pos2_df$CPonSP_dem_mean)])/
                                                        (length(pos2_df$CPonSP_dem_mean[pos2_df$CPonSP_dem_mean>0.5 & !is.na(pos2_df$CPonSP_dem_mean)])+
                                                           length(pos2_df$CPonSP_dem_mean[pos2_df$CPonSP_dem_mean<=0.5 & !is.na(pos2_df$CPonSP_dem_mean)])))*100,
                                          rep_perc = 100-((length(pos2_df$CPonSP_dem_mean[pos2_df$CPonSP_dem_mean>0.5 & 
                                                                                     !is.na(pos2_df$CPonSP_dem_mean)])/
                                                             (length(pos2_df$CPonSP_dem_mean[pos2_df$CPonSP_dem_mean>0.5 & !is.na(pos2_df$CPonSP_dem_mean)])+
                                                                length(pos2_df$CPonSP_dem_mean[pos2_df$CPonSP_dem_mean<=0.5 & !is.na(pos2_df$CPonSP_dem_mean)])))*100))
  
  
  ######################
  ### UNWEIGHTED logit + mean
  ######################
  
  unweighted_combined_dem_logit_ctrl_panel_df <- data.frame(state = target_states[i],
                                                            model = "combined_logit_mean_model_ctrl_panel",
                                                            dem_perc = (sum(pos1_df$combined_dem_pred, na.rm = T)/
                                                                          length(pos1_df$combined_dem_pred[!is.na(pos1_df$combined_dem_pred)]))*100,
                                                            rep_perc = 100 - ((sum(pos1_df$combined_dem_pred, na.rm = T)/
                                                                                length(pos1_df$combined_dem_pred[!is.na(pos1_df$combined_dem_pred)]))*100))
  
  unweighted_combined_dem_logit_socpan_df <- data.frame(state = target_states[i],
                                                            model = "combined_logit_mean_model_socpan",
                                                            dem_perc = (sum(pos2_df$combined_dem_pred, na.rm = T)/
                                                                          length(pos2_df$combined_dem_pred[!is.na(pos2_df$combined_dem_pred)]))*100,
                                                            rep_perc = 100 - ((sum(pos2_df$combined_dem_pred, na.rm = T)/
                                                                                 length(pos2_df$combined_dem_pred[!is.na(pos2_df$combined_dem_pred)]))*100))
  
  
  #######################
  # WEIGHTED
  #######################
   
  if(target_states[i]=="USA"){
    
    pos3_df <- ctrl_panel_weighted_state_numbers_df
    pos4_df <- socpan_weighted_state_numbers_df
    
  } else {
    
    pos3_df <- ctrl_panel_weighted_state_numbers_df %>% filter(state == target_states[i])
    pos4_df <- socpan_weighted_state_numbers_df %>% filter(state == target_states[i])
    
  } 
  
  
  ###################
  # TRUMP
  ###################
  
  
  weighted_trump_model_ctrl_panel_df <- data.frame(state = target_states[i],
                                                   model = "weighted_trump_model_ctrl_panel",
                                                   dem_perc = (sum(pos3_df$value.trump_dem)/
                                                                 (sum(pos3_df$value.trump_dem)+sum(pos3_df$value.trump_rep)))*100,
                                                   rep_perc = (sum(pos3_df$value.trump_rep)/
                                                                 (sum(pos3_df$value.trump_dem)+sum(pos3_df$value.trump_rep)))*100)
  

  weighted_trump_model_socpan_df <- data.frame(state = target_states[i],
                                                   model = "weighted_trump_model_socpan",
                                                   dem_perc = (sum(pos4_df$value.trump_dem)/
                                                                 (sum(pos4_df$value.trump_dem)+sum(pos4_df$value.trump_rep)))*100,
                                                   rep_perc = (sum(pos4_df$value.trump_rep)/
                                                                 (sum(pos4_df$value.trump_dem)+sum(pos4_df$value.trump_rep)))*100)
  
  ###################
  # kavanaugh
  ###################
  
  
  weighted_kavanaugh_model_ctrl_panel_df <- data.frame(state = target_states[i],
                                                   model = "weighted_kavanaugh_model_ctrl_panel",
                                                   dem_perc = (sum(pos3_df$value.kavanaugh_dem)/
                                                                 (sum(pos3_df$value.kavanaugh_dem)+sum(pos3_df$value.kavanaugh_rep)))*100,
                                                   rep_perc = (sum(pos3_df$value.kavanaugh_rep)/
                                                                 (sum(pos3_df$value.kavanaugh_dem)+sum(pos3_df$value.kavanaugh_rep)))*100)
  
  
  weighted_kavanaugh_model_socpan_df <- data.frame(state = target_states[i],
                                               model = "weighted_kavanaugh_model_socpan",
                                               dem_perc = (sum(pos4_df$value.kavanaugh_dem)/
                                                             (sum(pos4_df$value.kavanaugh_dem)+sum(pos4_df$value.kavanaugh_rep)))*100,
                                               rep_perc = (sum(pos4_df$value.kavanaugh_rep)/
                                                             (sum(pos4_df$value.kavanaugh_dem)+sum(pos4_df$value.kavanaugh_rep)))*100)
  
  ###################
  # republicans
  ###################
  
  
  weighted_republicans_model_ctrl_panel_df <- data.frame(state = target_states[i],
                                                       model = "weighted_republicans_model_ctrl_panel",
                                                       dem_perc = (sum(pos3_df$value.republicans_dem)/
                                                                     (sum(pos3_df$value.republicans_dem)+sum(pos3_df$value.republicans_rep)))*100,
                                                       rep_perc = (sum(pos3_df$value.republicans_rep)/
                                                                     (sum(pos3_df$value.republicans_dem)+sum(pos3_df$value.republicans_rep)))*100)
  
  
  weighted_republicans_model_socpan_df <- data.frame(state = target_states[i],
                                                   model = "weighted_republicans_model_socpan",
                                                   dem_perc = (sum(pos4_df$value.republicans_dem)/
                                                                 (sum(pos4_df$value.republicans_dem)+sum(pos4_df$value.republicans_rep)))*100,
                                                   rep_perc = (sum(pos4_df$value.republicans_rep)/
                                                                 (sum(pos4_df$value.republicans_dem)+sum(pos4_df$value.republicans_rep)))*100)
  
  ###################
  # MEAN
  ###################
  
  weighted_mean_model_ctrl_panel_df <- data.frame(state = target_states[i],
                                                   model = "weighted_mean_model_ctrl_panel",
                                                   dem_perc = (sum(pos3_df$value.pred_dem_vote)/
                                                                 (sum(pos3_df$value.pred_dem_vote)+sum(pos3_df$value.pred_rep_vote)))*100,
                                                   rep_perc = (sum(pos3_df$value.pred_rep_vote)/
                                                                 (sum(pos3_df$value.pred_dem_vote)+sum(pos3_df$value.pred_rep_vote)))*100)
  
  weighted_mean_model_socpan_df <- data.frame(state = target_states[i],
                                                  model = "weighted_mean_model_socpan",
                                                  dem_perc = (sum(pos4_df$value.pred_dem_vote)/
                                                                (sum(pos4_df$value.pred_dem_vote)+sum(pos4_df$value.pred_rep_vote)))*100,
                                                  rep_perc = (sum(pos4_df$value.pred_rep_vote)/
                                                                (sum(pos4_df$value.pred_dem_vote)+sum(pos4_df$value.pred_rep_vote)))*100)
  
  
  ###################
  # cross-model MEAN
  ###################
  
  weighted_mean_cross_model_ctrl_panel_df <- data.frame(state = target_states[i],
                                                  model = "weighted_mean_cross_model_ctrl_panel",
                                                  dem_perc = (sum(pos3_df$value.pred_dem_SPonCP)/
                                                                (sum(pos3_df$value.pred_dem_SPonCP)+sum(pos3_df$value.pred_rep_SPonCP)))*100,
                                                  rep_perc = (sum(pos3_df$value.pred_rep_SPonCP)/
                                                                (sum(pos3_df$value.pred_dem_SPonCP)+sum(pos3_df$value.pred_rep_SPonCP)))*100)
  
  weighted_mean_cross_model_socpan_df <- data.frame(state = target_states[i],
                                              model = "weighted_mean_cross_model_socpan",
                                              dem_perc = (sum(pos4_df$value.pred_dem_CPonSP)/
                                                            (sum(pos4_df$value.pred_dem_CPonSP)+sum(pos4_df$value.pred_rep_CPonSP)))*100,
                                              rep_perc = (sum(pos4_df$value.pred_rep_CPonSP)/
                                                            (sum(pos4_df$value.pred_dem_CPonSP)+sum(pos4_df$value.pred_rep_CPonSP)))*100)
  

  ###################
  # combined normal model + logit MEAN
  ###################
  
  weighted_combined_model_ctrl_panel_df <- data.frame(state = target_states[i],
                                                  model = "weighted_combined_model_ctrl_panel",
                                                  dem_perc = (sum(pos3_df$value.combined_dem_pred)/
                                                                (sum(pos3_df$value.combined_dem_pred)+sum(pos3_df$value.combined_rep_pred)))*100,
                                                  rep_perc = (sum(pos3_df$value.combined_rep_pred)/
                                                                (sum(pos3_df$value.combined_dem_pred)+sum(pos3_df$value.combined_rep_pred)))*100)
  
  weighted_combined_model_socpan_df <- data.frame(state = target_states[i],
                                              model = "weighted_combined_model_socpan",
                                              dem_perc = (sum(pos4_df$value.combined_dem_pred)/
                                                            (sum(pos4_df$value.combined_dem_pred)+sum(pos4_df$value.combined_rep_pred)))*100,
                                              rep_perc = (sum(pos4_df$value.combined_rep_pred)/
                                                            (sum(pos4_df$value.combined_dem_pred)+sum(pos4_df$value.combined_rep_pred)))*100)
  
  
  ############################
  ############################
  ## BIND EVERYTHING TOGETHER TO A NEW DF
  ############################
  ############################
  
  state_level_aggregations_df <- rbind(state_level_aggregations_df,
                                       actual_result_df,
                                       unweighted_trump_ctrl_panel_df,
                                       unweighted_trump_socpan_df,
                                       unweighted_trump_cross_model_ctrl_panel_df,
                                       unweighted_trump_cross_model_socpan_df,
                                       weighted_trump_model_ctrl_panel_df,
                                       weighted_trump_model_socpan_df,
                                       unweighted_kavanaugh_ctrl_panel_df,
                                       unweighted_kavanaugh_socpan_df,
                                       unweighted_kavanaugh_cross_model_ctrl_panel_df,
                                       unweighted_kavanaugh_cross_model_socpan_df,
                                       weighted_kavanaugh_model_ctrl_panel_df,
                                       weighted_kavanaugh_model_socpan_df,
                                       unweighted_republicans_ctrl_panel_df,
                                       unweighted_republicans_socpan_df,
                                       unweighted_republicans_cross_model_ctrl_panel_df,
                                       unweighted_republicans_cross_model_socpan_df,
                                       weighted_republicans_model_ctrl_panel_df,
                                       weighted_republicans_model_socpan_df,
                                       unweighted_mean_ctrl_panel_df,
                                       unweighted_mean_socpan_df,
                                       unweighted_mean_cross_model_ctrl_panel_df,
                                       unweighted_mean_cross_model_socpan_df,
                                       unweighted_combined_dem_logit_ctrl_panel_df,
                                       unweighted_combined_dem_logit_socpan_df,
                                       weighted_mean_model_ctrl_panel_df,
                                       weighted_mean_model_socpan_df,
                                       weighted_mean_cross_model_ctrl_panel_df,
                                       weighted_mean_cross_model_socpan_df,
                                       weighted_combined_model_ctrl_panel_df,
                                       weighted_combined_model_socpan_df)
  
}

# WRITE OUT
write.csv(state_level_aggregations_df, paste0(SOCPAN_PATH, "state_level_aggregations.csv"))
write.csv(state_level_aggregations_df, paste0(CTRL_PANEL_PATH, "state_level_aggregations.csv"))



