# 05X_tune_models_reps.R
# what are the models?
# trump, kavanuagh, midterms, democrat, republican 
# mentions in tweets as predictors of vote choice
# tune models to find the best possible model specifications for the republican vote classifier
# NL, 06/11/18 [in the early hours of the morning, EST]
# NL, 22/01/19 -- update to tune model for all waves cumulatively, aka 02X_tune_models.R
# NL, 10/02/19 -- update to adapt this to ctrl_panel
# NL, 10/02/19 -- same, but for reps models

library(dplyr)
library(quanteda)
library(beepr)

DATA_PATH <- "~/Dropbox/PhD/midterms_paper/tweets/control_panel/data/"

# DATA IN 
# 1 - CTRL PANEL USERS

ctrl_panel_df <- read.csv(paste0(DATA_PATH, "ctrl_panel_all_demographics.csv"),
                           stringsAsFactors = F) %>% mutate(
                             user_id = as.character(user_id),
                             X = NULL
                           )

# 1b - CTRL PANEL LOCATIONS

ctrl_panel_locs_df <- read.csv(paste0(DATA_PATH, "control_panel_members.csv"),
                               colClasses = "character") %>% mutate(
                                 X = NULL
                               ) 

ctrl_panel_df <- left_join(ctrl_panel_df, ctrl_panel_locs_df, by = "user_id")

# 2 - IMVOTING

imvoting_df <- read.csv(paste0(DATA_PATH, "wave_everything_data/imvoting_coded.csv"),
                        colClasses = "character") %>% mutate(
                          X = NULL
                        )

ctrl_panel_df <- left_join(ctrl_panel_df, imvoting_df, by = "user_id")


# write out ctrl_panel_df with declared voting variables

#write.csv(ctrl_panel_df, paste0(DATA_PATH, "ctrl_panel_all_declared_voters.csv"))

# ctrl_panel_df$rep_vote[is.na(ctrl_panel_df$rep_vote)] <- 0
# ctrl_panel_df$rep_vote[is.na(ctrl_panel_df$rep_vote)] <- 0

rm(imvoting_df)


# CONSTANTS

models <- c("trump", "kavanaugh", "midterms", "democrats", "republicans")
upper_lower_stopwords <- c(stopwords("english"), Hmisc::capitalize(stopwords("english")), "https", "t", "RT", "amp", "just", "like", "now", "get", "one")

# FUNCTIONS

# chunk_dfm() -- divide the dfm into 10 chunks, each of which is to be used as test set iteratively

chunk_dfm <- function(dfm, iterations){
  
  random_indeces <- sample(x = ndoc(dfm), size = ndoc(dfm), replace = F)
  
  # split
  split_indeces_ls <- split(random_indeces, cut(seq_along(random_indeces), iterations, labels = F))
  
  return(split_indeces_ls)
  
}

# run_full_textmodel() -- used for textmodel model tuning

run_full_textmodel <- function(dfm, train_indeces, test_indeces, min_terms, max_terms, min_docs, max_docs){
  
  # trim dfm
  trimmed_dfm <- dfm_trim(x = dfm,
                          min_termfreq = min_terms,
                          max_termfreq = max_terms,
                          min_docfreq = min_docs,
                          max_docfreq = max_docs)
  
  # run loop fitting textmodel on this trimmed dfm for 'n_iterations' iterations
  
  true_positives <- NULL
  false_positives <- NULL
  true_negatives <- NULL
  false_negatives <- NULL
  
  accuracy <- NULL
  precision <- NULL
  recall <- NULL
  
  # for(n in 1:n_iterations){
    
  # # sample
  # indices <- sample(x = 1:ndoc(trimmed_dfm), size = ceiling(ndoc(trimmed_dfm)/holdout_proportion), replace = F)
  
  # divide data
  test_set <- trimmed_dfm[test_indeces]
  test_labels <- docvars(trimmed_dfm)[test_indeces,1]
  training_set <- trimmed_dfm[train_indeces]
  training_labels <- docvars(trimmed_dfm)[train_indeces,1]
  
  # fit model
  model <- textmodel_nb(training_set, training_labels, prior = "uniform")
  predicted_values <- as.numeric(as.character(predict(model, newdata = test_set)))
  predicted_probabilities <- predict(model, newdata = test_set, type = "probability")
  
  # calculate precision/recall
  true_positives <- sum(test_labels==1 & predicted_values==1)
  false_positives <- sum(test_labels==0 & predicted_values==1)
  true_negatives <- sum(test_labels==0 & predicted_values==0)
  false_negatives <- sum(test_labels==1 & predicted_values==0)
  
  accuracy <- (true_positives + true_negatives)/(true_positives+false_positives+true_negatives+false_negatives)
  precision <- true_positives/(true_positives+false_positives)
  recall <- true_positives/(true_positives+false_negatives)
    
  # }
  
  # write mean precision/recall figures for all iterations to a new data frame line
  
  newline_df <- data.frame(accuracy = accuracy,
                           precision = precision,
                           recall = recall)
  
  # return the df
  
  return(newline_df)
  
}


get_test_parameters <- function(dfm){
  
  docs <- ndoc(dfm)
  features <- nfeat(dfm)
  
  # min terms: 1,2,3,4,5,10,15,20,0.1%,0.015%,0.2%
  min_terms <- c(1,3,5,10,15,ceiling(features*0.001), ceiling(features*0.0015))
  # max terms: 50%,60%,70%,80%,90%
  max_terms <- c(ceiling(features*0.5), ceiling(features*0.75), features)
  # min docs: 1,2,3,4,5,2%,3%,5%,10%,20%
  min_docs <- c(1,2,3,4,5, 10,ceiling(docs*0.02))
  # max docs: 50%,60%,70%,80%,90%
  max_docs <- c(ceiling(docs*0.5),ceiling(docs*0.5), docs)
  
  all_permutations_df <- expand.grid(min_terms = min_terms, 
                                  max_terms = max_terms,
                                  min_docs = min_docs,
                                  max_docs = max_docs)
  
  return(all_permutations_df)
  
}

#################
# TUNE MODELS
#################

model_specifications_df <- NULL

# SUBSET VOTERS 

voting_ctrl_panel_df <- ctrl_panel_df %>% filter(
  !is.na(ctrl_panel_df$rep_vote)
) 

set.seed(1234)

# 20% HOLDOUT SET

indices <- sample(x = 1:nrow(voting_ctrl_panel_df), size = nrow(voting_ctrl_panel_df)*0.2, replace = F)

holdout_set_df <- voting_ctrl_panel_df[indices,]
voting_ctrl_panel_df <- voting_ctrl_panel_df[-indices,]

for(i in 1:length(models)){
  
  # PRINT STATUS:
  print(paste0("Now tuning the ", models[i], " model. Time: ", Sys.time()))
  
  # STEP 1 - DATA IN -- relevant mention tweets for the current model
  mentions_df <- read.csv(paste0(DATA_PATH, "wave_everything_data/", models[i], "_mention_tweets.csv"), colClasses = "character") %>% select(
    user_id, status_id, text
  ) %>% distinct(
    status_id, .keep_all = T
  ) %>% filter(
    user_id %in% c(voting_ctrl_panel_df$user_id, holdout_set_df$user_id)
  )
  
  # Step 2 - build corpuses for training set and holdout set
  
  # A - TRAINING
  
  mention_text_voters <- NULL
  
  for(j in 1:length(unique(voting_ctrl_panel_df$user_id))){
    
    user_tweets_df <- mentions_df %>% filter(
      user_id == unique(voting_ctrl_panel_df$user_id)[j]
    )
    
    pos1 <- paste0(user_tweets_df$text, collapse = " ")
    
    # add on to big vector
    
    mention_text_voters[j] <- pos1
    names(mention_text_voters)[j] <- unique(voting_ctrl_panel_df$user_id)[j]
    
    rm(user_tweets_df, pos1)
  }
  
  # B - HOLDOUT
  
  mention_text_holdout <- NULL
  
  for(j in 1:length(unique(holdout_set_df$user_id))){
    
    user_tweets_df <- mentions_df %>% filter(
      user_id == unique(holdout_set_df$user_id)[j]
    )
    
    pos1 <- paste0(user_tweets_df$text, collapse = " ")
    
    # add on to big vector
    
    mention_text_holdout[j] <- pos1
    names(mention_text_holdout)[j] <- unique(holdout_set_df$user_id)[j]
    
    rm(user_tweets_df, pos1)
  }
  
  # C - DOCVARS!!
  # C1 - voters
  
  voters_docvars <- NULL
  
  for(l in 1:length(mention_text_voters)){
    voters_docvars[l] <- voting_ctrl_panel_df$rep_vote[voting_ctrl_panel_df$user_id==names(mention_text_voters)[l]]
    names(voters_docvars)[l] <- names(mention_text_voters)[l]
  }
  
  corpus_voters <- corpus(x = mention_text_voters,
                          docvars = data.frame(voters_docvars))
  

  # voters_docvars <- voting_ctrl_panel_df$rep_vote
  # names(voters_docvars) <- voting_ctrl_panel_df$user_id
  # voters_docvars <- voters_docvars[names(voters_docvars) %in% names(mention_text_voters)]
  # 
  # corpus_voters <- corpus(x = mention_text_voters[which(names(mention_text_voters) %in% voting_ctrl_panel_df$user_id)],
  # docvars = data.frame(voters_docvars))
  
  # C2 - holdout
  
  holdout_docvars <- NULL
  
  for(l in 1:length(mention_text_holdout)){
    holdout_docvars[l] <- holdout_set_df$rep_vote[holdout_set_df$user_id==names(mention_text_holdout)[l]]
    names(holdout_docvars)[l] <- names(mention_text_holdout)[l]
  }
  
  corpus_holdout <- corpus(x = mention_text_holdout,
                           docvars = data.frame(holdout_docvars))
  
  # holdout_docvars <- holdout_set_df$rep_vote
  # names(holdout_docvars) <- holdout_set_df$user_id
  # holdout_docvars <- holdout_docvars[names(holdout_docvars) %in% names(mention_text_holdout)]
  # 
  # corpus_holdout <- corpus(x = mention_text_holdout[which(names(mention_text_holdout) %in% holdout_set_df$user_id)],
  #                                docvars = data.frame(holdout_docvars))
  # 
  # D - DFM!
  
  voters_dfm <- dfm(corpus_voters,
                          remove = upper_lower_stopwords,
                          remove_symbols = T, remove_punct = T, remove_url = T, remove_numbers = T)
  
  holdout_dfm <- dfm(corpus_holdout,
                           remove = upper_lower_stopwords,
                           remove_symbols = T, remove_punct = T, remove_url = T, remove_numbers = T)
  
  # get test parameters
  
  all_permutations_df <- get_test_parameters(voters_dfm)
  
  # run different models
  
  precision_recall_df <- NULL
  
  for(l in 1:nrow(all_permutations_df)){
    
    print(paste0("On model ", l, " out of ", nrow(all_permutations_df), " possible models. Current time: ",
                 Sys.time()))
    
    # split dfm into 10 chunks, and run the model on each, 
    # outputting accuracy, precision and recall
    
    test_chunks <- chunk_dfm(voters_dfm, iterations = 10)
    
    chunk_test_df <- NULL
    
    for(k in 1:length(test_chunks)){
      
      test <- test_chunks[[k]]
      train <- unname(unlist(test_chunks[-k]))
      
      newline_df <- run_full_textmodel(dfm = voters_dfm, 
                                       test_indeces = test,
                                       train_indeces = train,
                                       min_terms = all_permutations_df$min_terms[l],
                                       max_terms = all_permutations_df$max_terms[l],
                                       min_docs = all_permutations_df$min_docs[l],
                                       max_docs = all_permutations_df$max_docs[l])
      
      chunk_test_df <- rbind(chunk_test_df, newline_df)
      
    }
    
    newline_df <- data.frame(accuracy = mean(chunk_test_df$accuracy),
                             precision = mean(chunk_test_df$precision),
                             recall = mean(chunk_test_df$recall))
    
    precision_recall_df <- rbind(precision_recall_df, newline_df)
    
    rm(newline_df)
  }
  
  models_accuracy_df <- cbind(all_permutations_df, precision_recall_df)
  # order by accuracy, followed by precision, then recall
  models_accuracy_df <- models_accuracy_df[with(models_accuracy_df, order(-accuracy, -precision, -recall)),]
  
  # Step 2 - run top 3 best performing models on holdout set. 
  # A - concatenate voters dfm and holdout dfm
  
  # re-sort
  
  corpus_holdout[["holdout_docvars"]] <- NULL
  corpus_voters[["voters_docvars"]] <- NULL
  
  all_corpus <- corpus_voters + corpus_holdout
  all_corpus[["rep_vote_dummy"]] <- c(unname(voters_docvars), unname(holdout_docvars))
  
  all_dfm <- dfm(all_corpus,
                 remove = upper_lower_stopwords,
                 remove_symbols = T, remove_punct = T, remove_url = T, remove_numbers = T)
  
  test_on_holdout_df <- NULL
  
  for(m in 1:3){
    
    newline_df <- run_full_textmodel(dfm = all_dfm,
                                     train_indeces = 1:ndoc(corpus_voters),
                                     test_indeces = (ndoc(corpus_holdout)+1):ndoc(all_dfm),
                                     min_terms = models_accuracy_df$min_terms[m],
                                     min_docs = models_accuracy_df$min_docs[m],
                                     max_terms = models_accuracy_df$max_terms[m],
                                     max_docs = models_accuracy_df$max_docs[m])
    
    test_on_holdout_df <- rbind(test_on_holdout_df, newline_df)
    
  }
  
  # let's print how well this does in comparison to performance as trained on the original data
  
  accuracy_difference_df <- data.frame(accuracy = round(test_on_holdout_df$accuracy - models_accuracy_df[1:3,]$accuracy, digits = 4),
                                       precision = round(test_on_holdout_df$precision - models_accuracy_df[1:3,]$precision, digits = 4),
                                       recall = round(test_on_holdout_df$recall - models_accuracy_df[1:3,]$recall, digits = 4))
  
  
  if(TRUE %in% accuracy_difference_df < -0.05){
    
    print(paste0(" Issue with selected classifier parameters for ", models[i], " model in wave ", i, ". At least one metric has performed at least 5% worse with the holdout set than before."))
    print("Difference matrix:")
    print(accuracy_difference_df)
    print("Classification on holdout set performance metrics:")
    print(test_on_holdout_df)
    
  } else {
    
    print(paste0("Performance testing on holdout set has same performance as without for ", models[i], " model in wave ", i, ". It is safe to use this model specification."))
    
  }
  
  # This process is now complete. we now know which precise model to pick for
  # the trump classifier in this wave. let's write this out. 
  
  newline_df <- data.frame(model = models[i],
                           wave = i)
  newline_df <- cbind(newline_df, models_accuracy_df[1,])
  
  model_specifications_df <- rbind(model_specifications_df, newline_df)
  
}

write.csv(model_specifications_df, paste0(DATA_PATH, "model_specifications/all_waves_reps_model_specs.csv"))
beep(sound = 8)

          
