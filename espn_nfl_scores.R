###########################
# File: espn_nfl_scores.R
# Description: Cache HTML copies of NFL game results from ESPN.com
# Date: 1/05/2015
# Author: Jake Russ
# Notes:
#
# To do:
###########################

# Load packages
library("httr")
library("rvest")
library("stringr")
library("lubridate")
library("dplyr")
library("magrittr")

# Working directory
dir <- getwd()

# Set user agent
uagent <- "Mozilla/5.0"

# Base web address
base_url <- "http://scores.espn.go.com"

# Append scoreboard path to base url
scores_url <- paste0(base_url, "/nfl/scoreboard")

# Parameters
season <- 2014
weeks  <- 1:17

# Initialize an empty data frame to collect all of the game ids and 
# boxscore urls.
links_all <- data.frame()

# Loop through the pages to get all of the press release link urls
for (i in weeks){
  
  # Make POST request, res is short for response
  res <- POST(scores_url, user_agent(uagent),
              query = list(seasonYear = season,
                           seasonType = 2,
                           weekNumber = i))
  
  doc <- html(res)
  
  links <- html_nodes(x = doc, xpath = "//div[@class='more-links']/ul/li[1]/a") %>% 
    html_attr("href") %>%
    paste0(base_url, .)

  ids <- str_extract_all(string = links, pattern = "[0-9]+") %>% unlist()
  
  # Collect ids and urls into temporary data frame.
  tmp <- data.frame(season = season, week = i, game.id = ids, url = links, 
                    stringsAsFactors = FALSE)
  
  # Store results in the links_all data frame
  links_all <- rbind(links_all, tmp)
  
  # Be polite to server; between url calls, have the scraper rest for 
  # a few seconds. Adds time to the loop but you don't want to overload 
  # the website's server.
  Sys.sleep(time = 3)
  
}

# Grab only the rows with a box score (256)
links_all <- links_all %>%
  filter(str_detect(string = url, pattern = "boxscore"))

# Throw an error if the number of games does not equal 256
# 32 teams * 16 games / 2 = 256
stopifnot(nrow(links_all) == 256)

# Store a copy of links_all
write.csv(links_all, paste0(dir, "/scores/boxscore_links_", season, ".csv"), 
          row.names = FALSE)


# Directory for saving the html files
html_dir <- paste0(dir, "/scores/html")

# Loop through all of the box score urls and cache a copy of the html
for (i in links_all$game.id) {
  
  # File path to save a copy of the webpage
  file_path <- paste0(html_dir, "/boxscore_", i, ".html")
  
  # Location of the espn boxscore api
  boxscore_url <- "http://scores.espn.go.com/nfl/boxscore"
  
  # Since we have the direct url for each release, we can do a GET
  # request instead of a POST request. This time we'll also save a copy
  # of the website for each press release. 
  res <- POST(url = boxscore_url, user_agent(uagent),
              query = list(gameId = i),
              write_disk(path = file_path, overwrite = TRUE))
  
  # Again, be polite to server
  Sys.sleep(time = 3)
  
} # End second for loop

# Clean up the work space
rm(list = ls())
