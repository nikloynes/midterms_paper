# get_previously_located_users.R

# 15/05/18  NL  0.1

# scrip with query to send to the sql db on digital ocean machine to 
# get already located tweeters. 

# DATA IN: txt file with all users in social panel (NL midterms paper, 2018)
# DATA OUT: csvs with all available location data for all available users in sql table

# PACKAGE DEPENDENCIES
#===============================================================================
print("Installing-Loading dependencies...")
# - add any new dependency in here
dependencies <- c(
  "twitteR", "stringi", "dplyr", "RMySQL", "parallel", "rio"
)

# - check if all dependencies are installed, if not, doing so
not_installed <- dependencies[
  which(!(dependencies %in% rownames(installed.packages())))
  ]
if (length(not_installed) > 0) {
  cat("Installing missing dependencies")
  install.packages(not_installed)
}

# - load all dependencies
lapply(dependencies, FUN = function(X) {
  do.call("require", list(X)) 
})

# SQL
#===============================================================================

# [1] Checking if we already geolocated these users
print("Connecting to MySQL server")
# - connecting to MySQL server
con <- dbConnect(MySQL(),
                 user="root",
                 # - DEVELOPLEMTN
                 #password = "", 
                 # - PRODUCTION
                 password='~`ruYm4AZ+@R"z%5', 
                 dbname="geo_location", host="localhost")

# DATA IN
#===============================================================================
#tweeters <- readLines("~/Dropbox/PhD/midterms_paper/panel_users.txt")
tweeters <- readLines("/home/geo_locating/test-data/panel_users.txt")

# get users that are already located from user table

located_tweeters <- (dbGetQuery(con, "SELECT user_id FROM users;"))$user_id

network_located_tweeters <- (dbGetQuery(con, "SELECT user_id FROM users WHERE network_estimated_country =1;"))$user_id

# filter and reassign

keep_these <- which(tweeters %in% located_tweeters)
keep_these_net <- which(tweeters %in% network_located_tweeters)

located_tweeters <- tweeters[keep_these]

unlocated_tweeters <- tweeters[-keep_these]
  
network_located_tweeters <- tweeters[keep_these_net]

# remove network-located tweeters from located_tweeters
located_tweeters <- located_tweeters[-keep_these_net]

# empty data frames for writing out location info we have
located_tweeters.df <- NULL
network_located_tweeters.df <- NULL

# for(i in 1:length(located_tweeters)){
#   
#   print(paste0("On iteration ", i, "/", length(located_tweeters)))
#   
#   raw_location_id <- (dbGetQuery(con, paste0("SELECT raw_location_id FROM users WHERE user_id =",
#                                           located_tweeters[i], ";")))$raw_location_id
#   if(raw_location_id==0){
#     
#     # this suggests that the loc string was empty
#     # so - check first if we estimated a location using network_approach
#     
#     loc_string <- NA
#     location.df <- data.frame(municipality = NA,
#                               admin = NA,
#                               country = NA,
#                               longitude = NA,
#                               latitude = NA)
#   } else {
#     
#     location_id <- (dbGetQuery(con, paste0("SELECT location_id FROM raw_locations WHERE id =",
#                                            raw_location_id[1], ";")))$location_id
#     loc_string <- (dbGetQuery(con, paste0("SELECT raw_location FROM raw_locations WHERE id =",
#                                           raw_location_id[1], ";")))$raw_location
#     location.df <- (dbGetQuery(con, paste0("SELECT * FROM locations WHERE id =",
#                                            location_id, ";")))
#   }
#   
#   newline <- data.frame(
#     user_id = located_tweeters[i],
#     loc_string = loc_string,
#     municipality = location.df$municipality,
#     admin = location.df$admin,
#     country = location.df$country,
#     long = location.df$longitude,
#     lat = location.df$latitude,
#     stringsAsFactors = FALSE)
#   
#   located_tweeters.df <- rbind(located_tweeters.df, newline)
#   
#   # purge
#   rm(newline, location.df, loc_string, location_id, raw_location_id)
#   
#   write.csv(located_tweeters.df, file = "/home/geo_locating/geolocation/NL_VOTERS/located_panel.csv")
# }

# now write out what's available for network-located users

for(j in 1:length(network_located_tweeters)){
  
  print(paste0("On iteration ", j, "/", length(network_located_tweeters)))
  
  location_id <- (dbGetQuery(con, paste0("SELECT primary_location_id FROM users WHERE user_id = ",
                                         network_located_tweeters[j], ";")))$primary_location_id
  
  location.df <- (dbGetQuery(con, paste0("SELECT * FROM locations WHERE id =",
                                          location_id, ";")))
  
  newline <- data.frame(
    user_id = network_located_tweeters[j],
    loc_string = NA,
    municipality = location.df$municipality,
    admin = location.df$admin,
    country = location.df$country,
    long = location.df$longitude,
    lat = location.df$latitude,
    stringsAsFactors = FALSE)
  
  # append
  network_located_tweeters.df <- rbind(network_located_tweeters.df, newline)
  
  write.csv(network_located_tweeters.df, file = "/home/geo_locating/geolocation/NL_VOTERS/net_located_panel.csv")
  
  # purge
  rm(newline, location.df, location_id)
}