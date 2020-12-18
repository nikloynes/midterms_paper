# 01_C_removing_non_US_users.R

# filtering out NON-US users from social panel, 
# making new txt of users for whom we have nothing, 
# adding these to the geo-pipeline

# DATA IN: panel_users.txt, located_panel.csv, net_located_panel.csv
# DATA OUT: us_panel_users_locations.csv

library(twitteR)
library(dplyr)

# panel users located through census-list
located_panel.df <- read.csv("/home/geo_locating/geolocation/NL_VOTERS/located_panel.csv",
                             colClasses = "character")

# panel users located through network
net_located_panel.df <- read.csv("/home/geo_locating/geolocation/NL_VOTERS/net_located_panel.csv")

# all panel users
panel_users <- readLines("/home/geo_locating/geolocation/NL_VOTERS/panel_users.txt")


# REMOVING NON-US USERS
# 1  - census-panel
us_users_census.df <- located_panel.df[which(located_panel.df$country=="United States"),]

# 2 - network-panel
us_users_net.df <- net_located_panel.df[which(net_located_panel.df$country=="United States"),]

# MERGE! 
located_us_panel.df <- rbind(us_users_census.df, us_users_net.df)
# remove dup rows -> n=5176
located_us_panel.df = located_us_panel.df %>% distinct(user_id, .keep_all = TRUE)

# SUMMARY STATS: 
# sort(table(located_us_panel$admin))
# California         New York          Florida            Texas 
# 1056              708              352              320 
# Illinois     Pennsylvania             Ohio    Massachusetts 
# 198              176              139              124 
# Michigan       New Jersey         Missouri        Tennessee 
# 114              108               98               95 
# Washington, D.C.   North Carolina          Arizona         Virginia 
# 93               88               87               86 
# Georgia         Maryland         Colorado          Indiana 
# 85               83               70               67 
# Minnesota   South Carolina       Washington      Connecticut 
# 67               67               66               65 
# Alabama         Oklahoma    West Virginia        Wisconsin 
# 61               60               60               57 
# Kentucky    New Hampshire           Nevada            Maine 
# 54               49               48               47 
# Louisiana           Oregon     Rhode Island             Iowa 
# 43               40               28               26 
# Arkansas             Utah      Mississippi            Idaho 
# 24               23               17               16 
# Kansas         Nebraska       New Mexico           Hawaii 
# 15               15               14               13 
# Delaware          Vermont          Montana           Alaska 
# 11               11                7                5 
# South Dakota                      North Dakota          Wyoming 
# 4                3                2                2

# WRITE OUT FOR FURTHER USE WITH OTHER SCRIPTS
# 1 - csv
write.csv(located_us_panel.df, file = "/home/geo_locating/geolocation/NL_VOTERS/located_us_panel.csv")
# 2 - txt for user ids
writeLines(located_us_panel.df$user_id, con = "/home/geo_locating/geolocation/NL_VOTERS/located_us_panel_users.txt")

