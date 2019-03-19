# 538 Riddler
# Justin Marschall
# https://fivethirtyeight.com/features/how-much-should-you-bid-for-that-painting/

# Riddler Express
# 
# How many games would we expect to be needed to complete a best-of-seven series 
# if each team has a 50 percent chance of winning each individual game? 
#
# How about if one team has a 60 percent chance of winning each game? 
#
# How about 70?

# create simulation function -----------------------------------

# a_pr = probability of team A winning
# a_wins = count of team A's wins 

riddler <- function(a_pr = 0.5) {
  a_wins <- 0
  b_wins <- 0
  
  while (a_wins < 4 && b_wins < 4) {
    game <- sample(c("A", "B"),
                   size = 1,
                   prob = c(a_pr, 1 - a_pr))
    
    if (game == "A") {
      a_wins <- a_wins + 1
    } else {
      b_wins <- b_wins + 1
    }
    
  }
  
  data.frame(a_pr = a_pr,
             n_games = a_wins + b_wins)
  
}

# simulation ---------------------------------------------------

library(tidyverse)

set.seed(20190411)

map_dfr(rep(c(0.5, 0.6, 0.7), each = 1000),
        riddler) %>% 
  group_by(a_pr) %>% 
  summarize(avg_games = mean(n_games),
            n = n())
