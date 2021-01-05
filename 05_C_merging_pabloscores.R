# merging_in_pabloscores.R
# merging in pabloscores sent to me by pablo barbera to full social panel df
# 31 July 18, NL
# 23/01/19, NL -- adapted to run on ctrl panel data /w new pabloscores from pablo

DATA_PATH <- "~/Dropbox/PhD/midterms_paper/tweets/control_panel/"
  
# DATA IN
ctrl_panel_df <- read.csv(paste0(DATA_PATH, "data/ctrl_panel_users_gender_race.csv"), 
                          stringsAsFactors = F) %>% mutate(
                            X = NULL,
                            user_id = as.character(user_id)
                          )
# i had tried to create pabloscores here, but the API limits were killing it. but will just overwrite with
# pablos score and save under same name

pabloscores_df <- read.csv(paste0(DATA_PATH, "ideology/nik_user_set_ideal_points2_RAWIDEOLOGIES.csv"), header = FALSE)
colnames(pabloscores_df) <- c("user_id", "ideology", "ideology_relevant_accounts_followed")

pabloscores_df$ideology <- gsub("theta:", "", pabloscores_df$ideology)
pabloscores_df$ideology_relevant_accounts_followed <- gsub("accounts_followed:", "", pabloscores_df$ideology_relevant_accounts_followed)
pabloscores_df$ideology_relevant_accounts_followed <- gsub("}", "", pabloscores_df$ideology_relevant_accounts_followed)
pabloscores_df$ideology_relevant_accounts_followed <- as.numeric(pabloscores_df$ideology_relevant_accounts_followed)
pabloscores_df$ideology <- as.numeric(pabloscores_df$ideology)
pabloscores_df$user_id <- gsub("\\{id_str:", "", pabloscores_df$user_id)

# make groups for ideology-scores

# message from andreu casas, 31/07/18

    # I haven’t really used pabloscores before. I asked Greg once about some reference scores to split liberals-moderates-conservatives
    # > E$phi1[E$twitter == “foxnews”]
    # [1] 0.8026926
    # > E$phi1[E$twitter == “wsj”]
    # [1] -0.0002366962
    # > E$phi1[E$twitter == “washingtonpost”]
    # [1] -0.3396343
    # > E$phi1[E$twitter == “nytimes”]
    # [1] -0.4719454
    # Here the scores for some media outlets
    # I think in that case I considered people to the left of nytimes as liberals, 
    # people to the right of foxnews as conservatives, and people in between as moderates

pabloscores_df$ideology_groups <- ""
pabloscores_df$ideology_groups[which(pabloscores_df$ideology>0.8026926)] <- "right-wing"
pabloscores_df$ideology_groups[which(pabloscores_df$ideology<0.8026926 & pabloscores_df$ideology>-0.0002366962)] <- "centre-right"
pabloscores_df$ideology_groups[which(pabloscores_df$ideology<0.0002366962 & pabloscores_df$ideology>-0.4719454)] <- "centre-left"
pabloscores_df$ideology_groups[which(pabloscores_df$ideology< -0.4719454)] <- "left-wing"

# make a histogram of ideology where accounts_followed>2

# proper_ideologies <- as.numeric(pabloscores_df$ideology[which(pabloscores_df$ideology_relevant_accounts_followed>2)])
# hist(proper_ideologies)

# proper_ideological_groups <- pabloscores_df$ideology_groups[which(pabloscores_df$ideology_relevant_accounts_followed>2)]
# table(proper_ideological_groups)

# merge in to the big data

ctrl_panel_ideology_df <- left_join(ctrl_panel_df, pabloscores_df, by = "user_id")

# crosstab ideology group by state

# state_ideology <- table(social_panel_ideology_df$admin[which(social_panel_ideology_df$ideology_relevant_accounts_followed>2)],
#       social_panel_ideology_df$ideology_groups[which(social_panel_ideology_df$ideology_relevant_accounts_followed>2)])
# 
# prop.table(state_ideology, 1)

# # FINALLY - merge in the user-relative politicalness of tweets! 
# 
# user_politicalness_df <- read.csv("classifying_politicalness/user_politicalness_master.csv")
# user_politicalness_df$politcalness_ratio <- round(user_politicalness_df$n_political_tweets/user_politicalness_df$total_n_tweets, digits = 2)
# user_politicalness_df$user_id <- as.character(user_politicalness_df$user_id)

# histogram of user-politicalness
#hist(user_politicalness_df$politcalness_ratio)

# social_panel_ideology_df <- left_join(social_panel_ideology_df, user_politicalness_df, by = "user_id")
write.csv(ctrl_panel_ideology_df, paste0(DATA_PATH, "data/ctrl_panel_all_demographics.csv"))
