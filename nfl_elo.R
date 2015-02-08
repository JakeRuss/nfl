###########################
# File: nfl_elo.R
# Description: Mimic FiveThirtyEight's NFL ELO ratings

# Date: 02/07/2015
# Author: Jake Russ
# Notes:
# To do:
############################

# Load packages
library("lubridate")
#library("stringr")
library("dplyr")
library("magrittr")

# Working directory
dir <- getwd()

# Load tables
scores   <- read.csv(paste0(dir, "/scores/nfl_scores.csv"), as.is = TRUE)
ids_2013 <- read.csv(paste0(dir, "/scores/boxscore_links_2013.csv"), as.is = TRUE)
ids_2014 <- read.csv(paste0(dir, "/scores/boxscore_links_2014.csv"), as.is = TRUE)

# Join season and week varibales to game scores by season
results_2013 <- ids_2013 %>%
  select(season, week, game.id) %>%
  inner_join(., y = scores, by = c("game.id" = "boxscore.id"))

results_2014 <- ids_2014 %>%
  select(season, week, game.id) %>%
  inner_join(., y = scores, by = c("game.id" = "boxscore.id"))

# Create a vector of NFL team ids
teams <- unique(scores$espn.id.away)

# Create starter data frame for ELO
rankings_2013 <- data.frame(team = teams, week.0 = 1500)

# 

