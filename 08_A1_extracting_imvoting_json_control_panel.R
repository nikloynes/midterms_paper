# 04b_extracting_imvoting_json.R
# extracting a json for the imvoting tweets
# NL, 08/02/19

library(dplyr)
library(rtweet)
library(jsonlite)

DATA_PATH <- "~/Dropbox/PhD/midterms_paper/tweets/control_panel/data/"

my_token <- readRDS("PATH-TO-TOKEN")

# DATA IN:
# imvoting.csv for CTRL panel

tweets_df <- read.csv(paste0(DATA_PATH, "wave_everything_data/imvoting.csv"), colClasses = "character")
tweet_ids <- unique(tweets_df$status_id)

# get full tweet objects for json

full_tweets_df <- lookup_statuses(tweet_ids, parse = T, token = my_token)

# necessary variables for collabortweet: 
# - 		["user"]["screen_name"]["text"]["id"]
#  full_tweets_df %>% select(
#   user_id, screen_name, text, status_id
# ) %>% mutate(
#   id = status_id,
#   user = user_id,
#   user_id = NULL,
#   status_id = NULL
# ) %>% stream_out(file(paste0(DATA_PATH, "imvoting.json")))

# mini_tweets_ls <- list(text = full_tweets_df$text, id_str = full_tweets_df$status_id, 
#                   user = list(screen_name = full_tweets_df$screen_name, id_str = full_tweets_df$user_id))
# 
# stream(mini_tweets_ls, pretty = TRUE, auto_unbox = TRUE)

stream_out(full_tweets_df, file(paste0(DATA_PATH, "imvoting.json")))
