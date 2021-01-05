# 01_making_control_panel.R
# script to generate a "control panel", i.e. a panel of Twitter users who were not specifically
# selected due to their having voted previously. This is to see how panel metrics and election
# forecasts extracted therefrom differ compared to the social panel. 
# NL, 30/10/18

library(dplyr)

setwd("~/Dropbox/PhD/midterms_paper/tweets/control_panel/")

# DATA IN - 
# 30k random geolocated US users

random_users_30k_df <- read.csv("data/us-users-census-SAMPLE-30k-US-ONLY.csv",
                                colClasses = "character")

# How big to draw the sample? 
# -> 10k

set.seed(1234)

indeces <- sample(x = nrow(random_users_30k_df), size = 10000, replace = F)
control_panel_mebers_df <- random_users_30k_df[indeces,] %>% select(
  user_id, municipality, state, country
)

write.csv(control_panel_mebers_df, "data/control_panel_members.csv")

# write out txt for pablo

writeLines(control_panel_mebers_df$user_id, "data/control_panel_user_ids.txt")
