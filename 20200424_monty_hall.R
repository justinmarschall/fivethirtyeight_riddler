# Justin Marschall
# 538 Riddler Classic
# 20200424

# https://fivethirtyeight.com/features/can-you-beat-the-goat-monty-hall-problem/

# The Monty Hall problem is a classic case of conditional probability. 
# In the original problem, there are three doors, two of which have goats behind them, while the third has a prize. 
# You pick one of the doors, and then Monty (who knows in advance which door has the prize) will always open another door, 
# revealing a goat behind it. It’s then up to you to choose whether to stay with your initial guess or to switch to the remaining door. 
# Your best bet is to switch doors, in which case you will win the prize two-thirds of the time.
# 
# Now suppose Monty changes the rules. First, he will randomly pick a number of goats 
# to put behind the doors: zero, one, two or three, each with a 25 percent chance. 
# After the number of goats is chosen, they are assigned to the doors at random, and each door has at most one goat. 
# Any doors that don’t have a goat behind them have an identical prize behind them.
# 
# At this point, you choose a door. If Monty is able to open another door, revealing a goat, he will do so. 
# But if no other doors have goats behind them, he will tell you that is the case.
# 
# It just so happens that when you play, Monty is able to open another door, revealing a goat behind it. 
# Should you stay with your original selection or switch? And what are your chances of winning the prize?


# -------------------------------------------------------------------------

# set up
library(tidyverse)

# create simulation function
riddler <- function(switch_flag = TRUE) {
  # set goats to doors (TRUE means there is a goat)
  n_goats <- sample(0:3, 1)
  doors <- sample(c(rep(TRUE, n_goats), rep(FALSE, 3 - n_goats)), 3, replace = FALSE)
  
  # initial door pick
  choice <- sample(1:3, 1)
  
  # if no other doors have goats, game ends
  stopifnot(any(doors[-choice]))
  
  # switch or not and resolve game
  if(switch_flag) {
    win <- !all(doors[-choice])
    
  } else {
    win <- !doors[choice]
    
  }
  
  # return result
  tibble(doors = list(doors), choice = choice, switch_flag = switch_flag, win = win)
  
}

# simulate and allow for stopifnot() error
map_df(rep(c(TRUE, FALSE), each = 10000), 
       possibly(riddler, tibble(doors = NA, choice = NA, switch_flag = NA, win = NA))) %>% 
  # filter(!is.na(win)) %>% 
  group_by(switch_flag) %>% 
  summarise(win_pct = mean(win),
            n = n())
