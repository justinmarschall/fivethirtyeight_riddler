

# Justin Marschall
# 538 Riddler
# 2019-09-06

# https://fivethirtyeight.com/features/what-are-your-chances-of-winning-the-u-s-open/


# Riddler Express ---------------------------------------------------------

# You and your friend are playing a game of “Acchi, Muite, Hoi” 
# (or what some on Twitter have been calling the “lookaway challenge”). 
# In the first round, you point in one of four directions: up, down, left or right. 
# At the exact same time, your friend also looks in one of those four directions. 
# If your friend looks in the same direction you’re pointing, you win! Otherwise, 
# you switch roles as the game continues to the next round — now your friend points in a direction and you try to look away. 
# As long as no one wins, you keep switching off who points and who looks.
# 
# It seems like your chances of winning should be 50 percent, since there are exactly two players. 
# But surely it’s not that simple. If both you and your friend choose your directions randomly in each round, 
# what are your chances of winning?

# set up
library(tidyverse)

# function to simulate single lookaway challenge (that is, until someone wins)
riddler <- function(x) {
  
  # initialize
  different_directions <- TRUE
  turn <- "a"
  n <- 1
  
  while (different_directions) {
    player_a <- sample(c("up", "down", "left", "right"), 1)
    player_b <- sample(c("up", "down", "left", "right"), 1)
    
    if(player_a == player_b) {
      different_directions <- FALSE
      break()
      
    }
    
    turn <- ifelse(turn == "a", "b", "a")
    n <- n + 1
    
  }
  
  return(tibble(n, winner = turn))
  
}

# audit
riddler()

# simulate; set second value to number of simulations
set.seed(20190906)
map_dfr(1:100000, riddler) %>% 
  count(winner) %>% 
  mutate(pct = n/sum(n))

# results
# A tibble: 2 x 3
# winner       n   pct
# <chr>    <int> <dbl>
# 1 a      57013 0.570
# 2 b      42987 0.430
