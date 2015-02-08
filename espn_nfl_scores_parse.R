###########################
# File: espn_nfl_scores_parse.R
# Description: Parse HTML boxscores into a CSV file

# Date: 01/05/2015
# Author: Jake Russ
# Notes:
# To do:
############################

# Load packages
library("XML")
library("rvest")
library("stringr")
library("dplyr")
library("magrittr")

# Working directory
dir <- getwd()

# Location of saved html files
html_dir <- paste0(dir, "/scores/html/")

# Create a list of the files in the html subdirectory
html_list <- list.files(path = html_dir, pattern = "boxscore_")

# Headers for the line score table
cols_six   <- c("team.id", "qtr1", "qtr2", "qtr3", "qtr4", "total")
cols_seven <- c("team.id", "qtr1", "qtr2", "qtr3", "qtr4", "ot", "total")

# Initialize an empty data frame to collect all of the game details
combined <- data.frame()

for (i in html_list){
  
  box_id <- i %>% str_extract(pattern = "[0-9]+")
  
  # Read html document into R
  doc <- html(paste0(html_dir, i))
  
  # Extract the date and time 
  datetime <- doc %>%
  html_nodes(xpath = "//div[@class = 'game-time-location']/p[1]") %>% 
  html_text() %>%
  str_trim() %>%
  str_split(string = ., pattern = ", ", n = 2) %>% unlist()
  
  # Extract the stadium and city location
  location <- doc %>%
    html_nodes(xpath = "//div[@class = 'game-time-location']/p[2]") %>% 
    html_text() %>%
    str_replace(pattern = "Â", replacement = "") %>%
    str_trim() %>%
    str_split(string = ., pattern = ", ", n = 2) %>% unlist()
  
  linescore <- doc %>%
  html_node(xpath = "//table[@class = 'linescore']") %>%
    html_table(header = TRUE)
  
  # Set columns headings
  if (ncol(linescore) == 6){
    
    names(linescore) <- cols_six
    
    } else {
    
    names(linescore) <- cols_seven
    
  }
  
  # Extract the attendace string and turn it to numeric
  attendance <- doc %>%
  html_nodes(xpath = "//div[@class = 'clearfix']") %>% 
  html_text() %>%
  subset(., str_detect(string = ., pattern = "^Attendance")) %>%
  str_extract(pattern = "[1-9](?:\\d{0,2})(?:,\\d{3})*") %>% 
  str_replace(pattern = ",", replacement = "") %>%
  as.numeric()
  
  tmp <- data.frame(boxscore.id      = box_id,
                    game.date        = datetime[2],
                    game.time        = datetime[1],
                    game.stadium     = location[1],
                    game.city        = location[2],
                    espn.id.away     = linescore$team.id[1],
                    espn.id.home     = linescore$team.id[2],
                    team.score.away  = linescore$total[1],
                    team.score.home  = linescore$total[2],
                    attendance       = attendance,
                    stringsAsFactors = FALSE)
  
  # Store results in the links_all data frame
  combined <- rbind(combined, tmp)
  
} # End for loop

# Store a copy of the html links
write.csv(combined, paste0(dir, "/scores/nfl_scores.csv"), 
          row.names = FALSE)

