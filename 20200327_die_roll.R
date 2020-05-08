# Justin Marschall
# 538 Riddler
# 20200327
# riddle: https://fivethirtyeight.com/features/can-you-get-the-gloves-out-of-the-box/
# solution: https://fivethirtyeight.com/features/can-you-tell-when-the-snow-started/


# Riddler Classic ---------------------------------------------------------

# You start with a fair 6-sided die and roll it six times, recording the results of each roll. 
# You then write these numbers on the six faces of another, unlabeled fair die. 
# For example, if your six rolls were 3, 5, 3, 6, 1 and 2, then your second die wouldn’t have a 4 on it; instead, it would have two 3s.
# 
# Next, you roll this second die six times. You take those six numbers and write them on the faces of yet another fair die, 
# and you continue this process of generating a new die from the previous one.
# 
# Eventually, you’ll have a die with the same number on all six faces. 
# What is the average number of rolls it will take to reach this state?

# set up
library(tidyverse)

# create simulation function
riddler <- function(die_faces) {
  rolls <- 0
  
  while(!list(die_faces) %in% map(1:6, rep, times = 6)) {
    die_faces <- sample(die_faces, 6, replace = TRUE)
    
    rolls <- rolls + 1
    
  }
  
  rolls
  
}

# simulate
set.seed(20200327)
sim_results <- replicate(100000, riddler(1:6))

# results
mean(sim_results)
summary(sim_results)

# plot
ggplot(tibble(sim_results), aes(sim_results)) +
  geom_histogram(bins = 75) +
  theme_minimal()
