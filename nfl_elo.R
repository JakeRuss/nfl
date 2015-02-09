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
library("tidyr")
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

results <- results_2013 %>%
  rbind_list(., results_2014) %>%
  group_by(season, week) %>%
  mutate(winner   = ifelse(team.score.away > team.score.home, espn.id.away, espn.id.home),
         loser    = ifelse(team.score.away > team.score.home, espn.id.home, espn.id.away),
         ptdiff   = team.score.home - team.score.away) %>%
  select(season, week, winner, loser, ptdiff)

# Create a vector of NFL team ids
teams <- unique(scores$espn.id.away)

# Final data frame to cache ELO results
elo <- data.frame(team = teams, stringsAsFactors = FALSE)

# Create starter data frame for ELO
elo_start <- data.frame(team = teams, start = 1500, stringsAsFactors = FALSE) 

# Calulate ELO
K <- 20 # According to FiveThirtyEight

for (s in 2013:2014) {
  
  for (w in 1:17) {
  
    wks_games <- results %>% filter(season == s, week == w)

    new_ratings <- data_frame()

    for (i in 1:nrow(wks_games)) {

      old_rating_w = elo_start[elo_start$team == wks_games$winner[i], ncol(elo_start)]
      old_rating_l = elo_start[elo_start$team == wks_games$loser[i], ncol(elo_start)]
  
      prior_prb_w  = 1 / (1 + 10^((old_rating_l - old_rating_w)/400))
      prior_prb_l  = 1 / (1 + 10^((old_rating_w - old_rating_l)/400))
  
      # Margin of victory mulitplier (FiveThirtyEight)
      movm <- log(abs(wks_games$ptdiff[i]) + 1) * (2.2/((old_rating_w - old_rating_l) * .001 + 2.2))
      
      new_rating_w = old_rating_w + (K * prior_prb_l * movm)
      new_rating_l = old_rating_l - (K * prior_prb_w * movm)
      
      tmp <- data_frame(team   = c(wks_games$winner[i], wks_games$loser[i]),
                        rating = c(new_rating_w, new_rating_l)) 
  
      new_ratings <- rbind_list(tmp, new_ratings)
  
  }
 
    # Handle bye teams
    teams_playing <- c(wks_games$winner, wks_games$loser)
    
    byes <- elo_start %>%
      filter(!(team %in% teams_playing)) %>%
      select(team, ncol(elo_start))
  
    colnames(byes)[2] <- "rating"
  
    new_ratings <- rbind(byes, new_ratings)
  
    elo_start <- elo_start %>%
      left_join(x = ., y = new_ratings, by = "team") %>%
      plyr::rename(., c("rating" = paste0("week.", w,".", s)))
  }
  
  elo <- left_join(elo, elo_start, by = "team")

  # After each season regress the final result 1500 by a third.
  # (i.e. if team's final ELO is 1800 they should start the next season at 1700)
  
  elo_start <- elo_start %>%
    select(team, ncol(elo_start)) %>%
    select(team, final = contains("week")) %>%
    mutate(reset = ifelse(test = final >= 1500,
                          yes  = final - ((final-1500)/3),
                          no   = final + abs((final-1500)/3)))

}

# Tidy the data
elo <- elo %>%
  select(-start, -final, -reset) %>%
  gather(key = variable, value = rating, convert = TRUE, -team) %>%
  separate(col = variable, into = c("name", "week", "season"), sep = "\\.") %>%
  select(season, week, team, rating, -name) %>%
  mutate(rating = round(rating)) %>%
  arrange(season, week, desc(rating))

# Write to csv
write.csv(elo, paste0(dir, "/elo/nfl_elo_538.csv"), row.names = FALSE)
