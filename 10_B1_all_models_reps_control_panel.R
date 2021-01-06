# 03_all_models_no_waves_reps.R
# a script to run all binary naive bayes classification models on all panel members,
# for each wave, iteratively, as well as cumulatively.

# what are the models?
# trump, kavanuagh, midterms, democrat, republican 
# mentions in tweets as predictors of vote choice

# goal: write out probability of vote==rep for each panel user for each model for each wave. 

# NL, 06/11/18 [in the early hours of the morning, EST]
# NL, 08/11/18  updated to include model specifications extracted from model tune, 
#               plus making two classification scripts for dems and reps
# NL, 23/01/19  updated to run textmodels for all waves in one go, aka 03_all_models_no_waves-DEMS.R 
# NL, 11/02/19  -- fixed error where docvars were wrongly assigned to corpora, aka 03_all_models_no_waves_reps.R

# PROBLEM: WHEN RUNNING THE MODEL TO PREDICT VALUES THAT ALREADY EXIST,
#          THE RESULTING PREDICTED VALUES ARE REALLY OFF.
#          IS THIS JUST THE WAY IT IS, OR WAS THERE A MISTAKE IN THE TUNING PROCESS?

library(dplyr)
library(quanteda)
library(ROCR)
library(beepr)

DATA_PATH <- "~/Dropbox/PhD/midterms_paper/tweets/panel/"


# DATA IN 
# 1 - PANEL USERS WITH DECLARED VOTES WHERE AVAILABLE
panel_users_df <- read.csv(paste0(DATA_PATH, "panel_users_all_declared_voters.csv"),
                           stringsAsFactors = F) %>% mutate(
                             user_id = as.character(user_id),
                             X = NULL
                           )

# 2 - TUNED MODEL PARAMETERS
model_parameters_df <- read.csv(paste0(DATA_PATH, "making_forecast/data/model_specifications/all_waves_dems_model_specs.csv"),
                                stringsAsFactors = F) %>% mutate(
                                  X = NULL
                                )

# CONSTANTS
upper_lower_stopwords <- c(stopwords("english"), Hmisc::capitalize(stopwords("english")), "https", "t", "RT", "amp", "just", "like", "now", "get", "one")

# voting vars for each panel wave
panel_users_df$dem_vote[panel_users_df$vote_any_wave=="D"] <- 1
panel_users_df$dem_vote[panel_users_df$vote_any_wave=="R"] <- 0
panel_users_df$rep_vote[panel_users_df$vote_any_wave=="R"] <- 1
panel_users_df$rep_vote[panel_users_df$vote_any_wave=="D"] <- 0


for(i in 1:nrow(model_parameters_df)){
  
  # print model and wave
  print(paste0("Now doing classification for the ", model_parameters_df$model[i], " model."))
  
  # STEP 1 - subset all voting panel members for this wave 
  voting_panel_users_df <- panel_users_df %>% filter(
    !is.na(panel_users_df$rep_vote)
  ) 
  
  # STEP 2 - pull the relevant tweets for the model in question
  mentions_df <- read.csv(paste0(DATA_PATH, "wave_everything_data/", model_parameters_df$model[i], "_mention_tweets.csv"), colClasses = "character") %>% select(
    user_id, status_id, text
  ) %>% distinct(
    status_id, .keep_all = T
  )
  
  # STEP 3 - build corpus
  mention_text <- NULL
  
  for(j in 1:length(unique(mentions_df$user_id))){
    
    user_tweets_df <- mentions_df %>% filter(
      user_id == unique(mentions_df$user_id)[j]
    )
    
    pos1 <- paste0(user_tweets_df$text, collapse = " ")
    
    # add on to big vector
    
    mention_text[j] <- pos1
    names(mention_text)[j] <- unique(mentions_df$user_id)[j]
    
    rm(user_tweets_df, pos1)
  }
  
  # A - docvars
  
  # corpus mention text
  voters_mention_text <- mention_text[names(mention_text) %in% voting_panel_users_df$user_id]
  
  # docvars
  voters_docvars <- NULL
  
  for(l in 1:length(voters_mention_text)){
    voters_docvars[l] <- voting_panel_users_df$rep_vote[voting_panel_users_df$user_id==names(voters_mention_text)[l]]
    names(voters_docvars)[l] <- names(voters_mention_text)[l]
  }
  
  corpus_voters <- corpus(x = voters_mention_text,
                          docvars = data.frame(voters_docvars))
  
  corpus_nonvoters <- corpus(x = mention_text[which(!(names(mention_text) %in% voting_panel_users_df$user_id))])
  
  # voters_docvars <- voting_panel_users_df$dem_vote
  # names(voters_docvars) <- voting_panel_users_df$user_id
  # voters_docvars <- voters_docvars[names(voters_docvars) %in% names(mention_text)]
  # 
  # corpus_voters <- corpus(x = mention_text[which(names(mention_text) %in% voting_panel_users_df$user_id)],
  #                             docvars = data.frame(voters_docvars))
  # 
  # corpus_nonvoters <- corpus(x = mention_text[which(!(names(mention_text) %in% voting_panel_users_df$user_id))])
   
  # B - re-join the corpora
  all_users_corpus <- corpus_voters + corpus_nonvoters
  
  # STEP 4 - create & trim DFM
  mentions_dfm <- dfm(all_users_corpus,
                      remove = upper_lower_stopwords,
                      remove_symbols = T, remove_punct = T, remove_url = T, remove_numbers = T)
  
  mentions_dfm <- dfm_trim(mentions_dfm,
                           min_termfreq = model_parameters_df$min_terms[i],
                           max_termfreq = model_parameters_df$max_terms[i],
                           min_docfreq = model_parameters_df$min_docs[i],
                           max_docfreq = model_parameters_df$max_docs[i])
  
  # STEP 5 - train model
  model <- textmodel_nb(x = mentions_dfm[1:ndoc(corpus_voters)], y = docvars(mentions_dfm)[1:ndoc(corpus_voters), 1], prior = "uniform")
  
  # find a good cutoff point for probs
  # standard_predictions <- predict(model, type = "probability")
  # standard_predictions_df <- as.data.frame(standard_predictions) %>% mutate(
  #   user_id = rownames(standard_predictions),
  #   prob = round(`1`, digits = 4)
  # ) %>% select(
  #   user_id, prob
  # )
  # 
  # merging_df <- voting_panel_users_df %>% filter(
  #   user_id %in% unique(mentions_df$user_id)
  # ) %>% select(
  #   user_id, dem_vote
  # )
  # 
  # standard_predictions_df <- left_join(standard_predictions_df, merging_df, by = "user_id")
  
  # STEP 6 - predict outcome variable
  predicted_values <- as.numeric(as.character(predict(model, newdata = mentions_dfm[(ndoc(corpus_voters)+1):ndoc(mentions_dfm)])))
  predicted_probabilities <- predict(model, newdata = mentions_dfm[(ndoc(corpus_voters)+1):ndoc(mentions_dfm)], type = "probability")
  predicted_probabilities_df <- as.data.frame(predicted_probabilities)
  predicted_probabilities_df <- cbind(user_id = rownames(predicted_probabilities_df), predicted_probabilities_df)
  rownames(predicted_probabilities_df) <- NULL
  predicted_probabilities_df <- predicted_probabilities_df %>% mutate(
    prob = `1`,
    `1` = NULL,
    `0` = NULL
  )
  
  # STEP 7 - write out predicted probabilities
  panel_users_df <- left_join(panel_users_df, predicted_probabilities_df, by = "user_id") %>% mutate(
    prob = round(prob, digits = 4)
  )
  
  varname <- paste0("prob_rep_", model_parameters_df$model[i], "_model")
  panel_users_df[varname] <- panel_users_df$prob
  panel_users_df$prob <- NULL
  
}

panel_users_out_df <- panel_users_df %>% select(
  user_id, admin, ideology_groups, rep_vote, prob_rep_trump_model, 
  prob_rep_kavanaugh_model, prob_rep_midterms_model, 
  prob_rep_democrats_model, prob_rep_republicans_model
)

write.csv(panel_users_out_df, paste0(DATA_PATH, "making_forecast/data/all_models_no_waves_reps.csv"))
beep(sound = 8)
