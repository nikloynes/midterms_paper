# 01_all_declared_voters_all_waves.R
# an exploratory look at all labelled voting data for all waves
# - how many individuals total have stated that they'll vote?
# - how many ppl have said it in multiple waves?
# - how many ppl have changed their vote?
# - visualise the basic dynamic of declared voters, unweighted
# NL, 06/11/18 [in the early morning hours EST]
# NL, 14/01/19 -- minor replicability check and tiny edits in comments

setwd("~/Dropbox/PhD/midterms_paper/tweets/panel/")

library(dplyr)
library(ggplot2)

# DATA IN - panel users

panel_members_df <- read.csv("panel_members_tweet_tracking_PRE.csv", stringsAsFactors = F) %>% select(
  user_id, municipality, admin, country, first_name.x, surname.x, gender, highest_value_race, ideology, ideology_relevant_accounts_followed, ideology_groups, politcalness_ratio
) %>% mutate(
  user_id = as.character(user_id),
  vote_wave_01 = NA,
  vote_wave_02 = NA,
  vote_wave_03 = NA,
  vote_wave_04 = NA,
  vote_wave_05 = NA,
  vote_wave_06 = NA,
  vote_any_wave = NA,
  vote_numbers = 0
)

# DATA IN - voting data

voting_wave_01_df <- read.csv("wave_01_data/imvoting_coded.csv", colClasses = "character") %>% rename(
  vote = label
) %>% filter(
  vote =="R" | vote =="D"
) %>% distinct(
  status_id, .keep_all = T
)
voting_wave_02_df <- read.csv("wave_02_data/imvoting_coded.csv", colClasses = "character") %>% filter(
  vote =="R" | vote =="D"
) %>% distinct(
  status_id, .keep_all = T
)
voting_wave_03_df <- read.csv("wave_03_data/imvoting_coded.csv", colClasses = "character") %>% filter(
  vote =="R" | vote =="D"
) %>% distinct(
  status_id, .keep_all = T
)
voting_wave_04_df <- read.csv("wave_04_data/imvoting_coded.csv", colClasses = "character") %>% filter(
  vote =="R" | vote =="D"
) %>% distinct(
  status_id, .keep_all = T
)
voting_wave_05_df <- read.csv("wave_05_data/imvoting_coded.csv", colClasses = "character") %>% filter(
  vote =="R" | vote =="D"
) %>% distinct(
  status_id, .keep_all = T
)
voting_wave_06_df <- read.csv("wave_06_data/imvoting_coded.csv", colClasses = "character") %>% filter(
  vote =="R" | vote =="D"
) %>% distinct(
  status_id, .keep_all = T
)

# ANALYSIS
# STEP 1 - write out vote choice for each wave and all waves

all_voting_ls <- list(voting_wave_01_df,
                      voting_wave_02_df,
                      voting_wave_03_df,
                      voting_wave_04_df,
                      voting_wave_05_df,
                      voting_wave_06_df)

voting_vars <- c("vote_wave_01", "vote_wave_02", "vote_wave_03", "vote_wave_04", "vote_wave_05", "vote_wave_06")

for(i in 1:6){
  
  for(j in 1:nrow(panel_members_df)){
    
    user_vote_this_wave_df <- all_voting_ls[[i]] %>% filter(
      user_id == panel_members_df$user_id[j]
    )
    
    if(nrow(user_vote_this_wave_df)==0){
      
      next
      
    } else {
      
      # extract all voting scores and take the biggest one
      user_vote <- names(sort(table(user_vote_this_wave_df$vote), decreasing = T)[1])
      
      # write user vote to the panel members df
      panel_members_df[voting_vars[i]][j,] <- user_vote
  
    }
    
   rm(user_vote, user_vote_this_wave_df) 
    
  }
  
}

# write out number of vote mentions in all waves
# plus most common vote

for(i in 1:nrow(panel_members_df)){
  
  votes <- c(panel_members_df$vote_wave_01[i],
             panel_members_df$vote_wave_02[i],
             panel_members_df$vote_wave_03[i],
             panel_members_df$vote_wave_04[i],
             panel_members_df$vote_wave_05[i],
             panel_members_df$vote_wave_06[i])
  
  panel_members_df$vote_numbers[i] <- sum(!is.na(votes))
  
  vote_party <- names(sort(table(votes), decreasing = T)[1])
  
  if(is.null(vote_party)){
    
    vote_party <- NA
    
  }
  
  panel_members_df$vote_any_wave[i] <- vote_party

}

# STEP 2 - make a visualisation for the 6 waves
# A - make DF with cumulative 

voting_by_wave_df <- data.frame(wave = character(length = 7),
                                dem_votes = numeric(length = 7),
                                dem_votes_cum = numeric(length = 7),
                                dem_votes_cum_perc = numeric(length = 7),
                                rep_votes = numeric(length = 7),
                                rep_votes_cum = numeric(length = 7),
                                rep_votes_cum_perc = numeric(length = 7),
                                total_votes = numeric(length = 7),
                                stringsAsFactors = F)

for_now_df <- panel_members_df

for(i in 1:7){
  
  if(i==7){
    
    voting_by_wave_df$wave[i] <- "total"
    voting_by_wave_df$dem_votes[i] <- sum(voting_by_wave_df$dem_votes[1:6])
    voting_by_wave_df$rep_votes[i] <- sum(voting_by_wave_df$rep_votes[1:6])
    voting_by_wave_df$total_votes[i] <- voting_by_wave_df$dem_votes[i] + voting_by_wave_df$rep_votes[i]
    voting_by_wave_df$dem_votes_cum[i] <- voting_by_wave_df$dem_votes[i]
    voting_by_wave_df$rep_votes_cum[i] <- voting_by_wave_df$rep_votes[i]
    voting_by_wave_df$dem_votes_cum_perc[i] <- voting_by_wave_df$dem_votes_cum[i]/(voting_by_wave_df$dem_votes_cum[i]+voting_by_wave_df$rep_votes_cum[i])*100
    voting_by_wave_df$rep_votes_cum_perc[i] <- voting_by_wave_df$rep_votes_cum[i]/(voting_by_wave_df$dem_votes_cum[i]+voting_by_wave_df$rep_votes_cum[i])*100
    
    break
    
  }
  
  for_later_df <- for_now_df %>% filter(
    !is.na(for_now_df[voting_vars[i]])
  ) 
  
  if(i==1){
    
    voting_by_wave_df$wave[i] <- as.character(i)
    voting_by_wave_df$dem_votes[i] <- sum(for_later_df[voting_vars[i]]=="D")
    voting_by_wave_df$dem_votes_cum[i] <- voting_by_wave_df$dem_votes[i]
    voting_by_wave_df$rep_votes[i] <- sum(for_later_df[voting_vars[i]]=="R")
    voting_by_wave_df$rep_votes_cum[i] <- voting_by_wave_df$rep_votes[i]
    voting_by_wave_df$total_votes[i] <- voting_by_wave_df$dem_votes[i]+voting_by_wave_df$rep_votes[i]
    voting_by_wave_df$dem_votes_cum_perc[i] <- voting_by_wave_df$dem_votes_cum[i]/(voting_by_wave_df$dem_votes_cum[i]+voting_by_wave_df$rep_votes_cum[i])*100
    voting_by_wave_df$rep_votes_cum_perc[i] <- voting_by_wave_df$rep_votes_cum[i]/(voting_by_wave_df$dem_votes_cum[i]+voting_by_wave_df$rep_votes_cum[i])*100
    
  } else {
    
    voting_by_wave_df$wave[i] <- as.character(i)
    voting_by_wave_df$dem_votes[i] <- sum(for_later_df[voting_vars[i]]=="D")
    voting_by_wave_df$dem_votes_cum[i] <- voting_by_wave_df$dem_votes_cum[i-1] + voting_by_wave_df$dem_votes[i]
    voting_by_wave_df$rep_votes[i] <- sum(for_later_df[voting_vars[i]]=="R")
    voting_by_wave_df$rep_votes_cum[i] <- voting_by_wave_df$rep_votes_cum[i-1] + voting_by_wave_df$rep_votes[i]
    voting_by_wave_df$total_votes[i] <- voting_by_wave_df$dem_votes[i]+voting_by_wave_df$rep_votes[i]
    voting_by_wave_df$dem_votes_cum_perc[i] <- voting_by_wave_df$dem_votes_cum[i]/(voting_by_wave_df$dem_votes_cum[i]+voting_by_wave_df$rep_votes_cum[i])*100
    voting_by_wave_df$rep_votes_cum_perc[i] <- voting_by_wave_df$rep_votes_cum[i]/(voting_by_wave_df$dem_votes_cum[i]+voting_by_wave_df$rep_votes_cum[i])*100
    
  }
  
  
  for_now_df <- for_now_df %>% filter(
    !(user_id %in% for_later_df$user_id)
  )
  
}

library(reshape)

for_plot_df <- melt(voting_by_wave_df, id.vars = c("wave")) %>% filter(
  variable=="dem_votes_cum_perc" | variable == "rep_votes_cum_perc",
  wave!="total"
) %>% mutate(
  wave = as.numeric(wave)
)


# plot this change over time

lineplot <- ggplot(for_plot_df, aes(x=wave, y=value, group=variable)) +
            geom_line(aes(color = variable)) + 
            ylab("Percentage") +
            xlab("Wave") +
            scale_color_manual(values = c("blue", "red"), guide = FALSE) +
            scale_x_continuous(breaks = c(1,2,3,4,5,6)) + 
            theme_minimal()


# EXPORT
# DATA
write.csv(panel_members_df, "making_forecast/data/panel_users_all_declared_voters.csv")

# PLOT
ggsave("../../graphs/right_before_election/raw_voters_by_wave.pdf", plot = lineplot, device = "pdf")

