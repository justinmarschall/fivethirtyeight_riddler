
# Justin Marschall

# riddle ------------------------------------------------------------------

# https://fivethirtyeight.com/features/can-you-win-the-loteria/
# 
# Riddler Express
# From Taylor Firman, the unluck of the draw:
#   
# Lotería is a traditional Mexican game of chance, akin to bingo. 
# Each player receives a four-by-four grid of images. 
# Instead of a comically large rotating bin of numbered balls, 
# the caller randomly draws a card from a deck containing all 54 possible images. 
# If a player has that image on their grid, they mark it off. 
# The exact rules can vary, but in this version, 
# the game ends when one of the players fills their entire card (and screams “¡Lotería!”). 
# Each of the 54 possible images can only show up once on each card, but other than that restriction, 
# assume that image selection and placement on each player’s grid is random.
# 
# One beautiful day, you and your friend Christina decide to face off in a friendly game of Lotería. 
# What is the probability that either of you ends the game with an empty grid, 
# i.e. none of your images was called? 
#   
# How does this probability change if there were more or fewer unique images? 
# Larger or smaller player grids?


# set up ------------------------------------------------------------------

library(tidyverse)

# create function ---------------------------------------------------------

riddler <- function(deck_size = 54, grid_x_length = 4) {
  if(deck_size <= grid_x_length**2) {
    stop("deck_size (n of unique cards) must be > grid_x_length**2 (number of cards each player takes)")
    
  }
  
  # randomize deck order and player cards
  deck <- sample(1:deck_size, deck_size, replace = FALSE)
  card_a <- sample(deck, grid_x_length**2, replace = FALSE)
  card_b <- sample(deck, grid_x_length**2, replace = FALSE)
  
  # set initial values for Loteria
  draw <- NULL
  i <- 1
  
  # play Loteria until one (or both) players match their cards with cards drawn
  while ((!all(card_a %in% draw)) && (!all(card_b %in% draw))) {
    draw <- append(draw, deck[i])
    
    i <- i + 1
    
    if(length(draw) == 100) break
    
  }
  
  # return results and metadata
  tibble(deck_size = deck_size,
         grid_x_length = grid_x_length,
         max_match = max(length(intersect(draw, card_a)), length(intersect(draw, card_b))),
         min_match = min(length(intersect(draw, card_a)), length(intersect(draw, card_b))),
         n_draw = length(draw),
         draw = list(draw),
         card_a = list(card_a),
         card_b = list(card_b))
  
}


# create protective function in case user inputs produce error
# so that simulation doesn't error out completely
riddler_possibly <- possibly(riddler, tibble(deck_size = NA,
                                             grid_x_length = NA,
                                             max_match = NA,
                                             min_match = NA,
                                             n_draw = NA,
                                             draw = NA,
                                             card_a = NA,
                                             card_b = NA), 
                             quiet = TRUE)

# simulation --------------------------------------------------------------

# set parameters for simulation
# n_sim = number of simulations per combination of deck_size and grid_x_length
# deck_size = number of unique cards in the deck
# grid_x_length = players create square grid where each side has this length (i.e. 2 means a 2x2 for a total of 4 cards)
n_sim <- 200
params <- expand.grid(deck_size = seq(10, 1000, 10), 
                      grid_x_length = 2:10)

# run simulation
sim <- 
  map2_dfr(rep(params$deck_size, each = n_sim), 
           rep(params$grid_x_length, each = n_sim),
           riddler_possibly)

# simulation results
sim %>% 
  group_by(deck_size, grid_x_length) %>% 
  summarise(mean_min_match = mean(min_match),
            min_match_0_pct = sum(min_match == 0) / n(),
            mean_n_draw = mean(n_draw),
            n_sim = n()) %>% 
  filter(!is.na(deck_size)) %>% 
  mutate(grid_x_length = paste0(grid_x_length, "*", grid_x_length),
         grid_x_length = factor(grid_x_length,
                                levels = c("2*2",
                                           "3*3",
                                           "4*4",
                                           "5*5",
                                           "6*6",
                                           "7*7",
                                           "8*8",
                                           "9*9",
                                           "10*10"))) %>% 
  ggplot(aes(deck_size, min_match_0_pct)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~grid_x_length) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Deck Size", 
       y = "Proportion w/ Empty Grid", 
       title = "Proportion of Games Resulting in Empty Grid by Varying Deck Size and Player Grid",
       subtitle = "FiveThirtyEight Riddler Express 2019-05-31") +
  theme_bw()
