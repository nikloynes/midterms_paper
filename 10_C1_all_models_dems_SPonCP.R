# 06c_all_models_megaloop_dems-SPonCP.R
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
# NL, 11/02/19  no waves, running for ctrl panel. AKA -- 06a_all_models_megaloop_dems.R
# NL, 26/02/19  no waves, running ctrl panel classifier on socpan data
# NL, 27/02/19  no waves, running socpan classifier on ctrl panel data

#########################
## CTRL PAN CLASSIFIER ON SOCPAN DATA
#########################

library(dplyr)
library(quanteda)

CTRL_PANEL_DATA_PATH <- "~/Dropbox/PhD/midterms_paper/tweets/control_panel/data/"
SOCPAN_DATA_PATH <- "~/Dropbox/PhD/midterms_paper/tweets/panel/"


# DATA IN 
# 1 - PANEL USERS WITH DECLARED VOTES WHERE AVAILABLE
ctrl_panel_df <- read.csv(paste0(CTRL_PANEL_DATA_PATH, "ctrl_panel_all_declared_voters.csv"),
                           stringsAsFactors = F) %>% mutate(
                             user_id = as.character(user_id),
                             X = NULL
                           )

socpan_users_df <- read.csv(paste0(SOCPAN_DATA_PATH, "panel_users_all_declared_voters.csv"),
                           stringsAsFactors = F) %>% mutate(
                             user_id = as.character(user_id),
                             X = NULL
                           )


# 2 - TUNED MODEL PARAMETERS
model_parameters_df <- read.csv(paste0(SOCPAN_DATA_PATH, 
                                       "making_forecast/data/model_specifications/all_waves_dems_model_specs.csv"),
                                stringsAsFactors = F) %>% mutate(
                                  X = NULL
                                )

# CONSTANTS
upper_lower_stopwords <- c(stopwords("english"), Hmisc::capitalize(stopwords("english")), "https", "t", "RT", "amp", "just", "like", "now", "get", "one")

# voting vars for each panel wave
socpan_users_df$dem_vote[socpan_users_df$vote_any_wave=="D"] <- 1
socpan_users_df$dem_vote[socpan_users_df$vote_any_wave=="R"] <- 0
socpan_users_df$rep_vote[socpan_users_df$vote_any_wave=="R"] <- 1
socpan_users_df$rep_vote[socpan_users_df$vote_any_wave=="D"] <- 0

set.seed(1234)

for(i in 1:nrow(model_parameters_df)){
  
  ###############
  # BUILD CLASSIFIER
  ###############
  
  # print model and wave
  print(paste0("Now doing classification for the ", model_parameters_df$model[i], " model."))
  
  # STEP 1 - subset all voting panel members for this wave 
  voting_socpan_users_df <- socpan_users_df %>% filter(
    !is.na(socpan_users_df$dem_vote)
  ) 
  
  # STEP 2 - pull the relevant tweets to build the classifier for the model in question
  mentions_df <- read.csv(paste0(SOCPAN_DATA_PATH, "wave_everything_data/", 
                                 model_parameters_df$model[i], "_mention_tweets.csv"), 
                          colClasses = "character") %>% 
    select(user_id, status_id, text) %>% 
    filter(user_id %in% unique(voting_socpan_users_df$user_id)) %>% 
    distinct(status_id, .keep_all = T)
  
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
  # voters_docvars <- voting_ctrl_panel_df$dem_vote
  # names(voters_docvars) <- voting_ctrl_panel_df$user_id
  # voters_docvars <- voters_docvars[names(voters_docvars) %in% names(mention_text)]
  # 
  # corpus_voters <- corpus(x = mention_text[which(names(mention_text) %in% voting_ctrl_panel_df$user_id)],
  #                             docvars = data.frame(voters_docvars))
  
  # corpus mention text
  voters_mention_text <- mention_text[names(mention_text) %in% voting_socpan_users_df$user_id]
  
  # docvars
  voters_docvars <- NULL
  
  for(l in 1:length(voters_mention_text)){
    voters_docvars[l] <- voting_socpan_users_df$dem_vote[voting_socpan_users_df$user_id==names(voters_mention_text)[l]]
    names(voters_docvars)[l] <- names(voters_mention_text)[l]
  }
  
  corpus_labelled_socpan <- corpus(x = mention_text,
                                      docvars = data.frame(voters_docvars))
  
  # STEP 4 - pull unlabelled text for control panel
  mentions_df <- read.csv(paste0(CTRL_PANEL_DATA_PATH, "wave_everything_data/", 
                                 model_parameters_df$model[i], "_mention_tweets.csv"), 
                          colClasses = "character") %>% 
    select(user_id, status_id, text) %>% 
    distinct(status_id, .keep_all = T)
  
  # STEP 5 - build corpus
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
  
  corpus_ctrl_panel <- corpus(mention_text)
  
  # STEP 6 - merge corpora and build DFM
  all_users_corpus <- corpus_labelled_socpan + corpus_ctrl_panel
  
  all_users_dfm <- dfm(all_users_corpus,
                       remove = upper_lower_stopwords,
                       remove_symbols = T, remove_punct = T, remove_url = T, remove_numbers = T) %>% 
    dfm_trim(min_termfreq = model_parameters_df$min_terms[i],
             max_termfreq = model_parameters_df$max_terms[i],
             min_docfreq = model_parameters_df$min_docs[i],
             max_docfreq = model_parameters_df$max_docs[i])
  
  # STEP 7 - train model
  model <- textmodel_nb(x = all_users_dfm[1:ndoc(corpus_labelled_socpan)], 
                        y = docvars(all_users_dfm)[1:ndoc(corpus_labelled_socpan), 1], 
                        prior = "uniform")
  
  # STEP 8 - predict outcome variable
  predicted_values <- as.numeric(
    as.character(predict(
      model, 
      newdata = all_users_dfm[(ndoc(corpus_labelled_socpan)+1):ndoc(all_users_dfm)])))
  predicted_probabilities <- predict(
    model, 
    newdata = all_users_dfm[(ndoc(corpus_labelled_socpan)+1):ndoc(all_users_dfm)], 
    type = "probability")
  predicted_probabilities_df <- as.data.frame(predicted_probabilities)
  predicted_probabilities_df <- cbind(user_id = rownames(predicted_probabilities_df), predicted_probabilities_df)
  rownames(predicted_probabilities_df) <- NULL
  predicted_probabilities_df <- predicted_probabilities_df %>% mutate(
    prob = `1`,
    `1` = NULL,
    `0` = NULL
  )
  
  # STEP 7 - write out predicted probabilities
  ctrl_panel_df <- left_join(ctrl_panel_df, predicted_probabilities_df, by = "user_id") %>% mutate(
    prob = round(prob, digits = 4)
  )
  
  varname <- paste0("prob_dem_SPonCP_", model_parameters_df$model[i], "_model")
  ctrl_panel_df[varname] <- ctrl_panel_df$prob
  ctrl_panel_df$prob <- NULL
  
}

ctrl_panel_out_df <- ctrl_panel_df %>% select(
  user_id, state, ideology_groups, prob_dem_SPonCP_trump_model, 
  prob_dem_SPonCP_kavanaugh_model, prob_dem_SPonCP_midterms_model, 
  prob_dem_SPonCP_democrats_model, prob_dem_SPonCP_republicans_model
)

write.csv(ctrl_panel_out_df, paste0(CTRL_PANEL_DATA_PATH, "SPonCP_no_waves_dems.csv"))