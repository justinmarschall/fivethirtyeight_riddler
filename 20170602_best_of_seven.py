# fivethirtyeight riddler puzzle
# 20170602
# https://fivethirtyeight.com/features/how-much-should-you-bid-for-that-painting/
# python solution by justin marschall

# How many games would we expect to be needed to complete a best-of-seven series 
# if each team has a 50 percent chance of winning each individual game? 
# How about if one team has a 60 percent chance of winning each game? 
# How about 70?

# set up
import numpy as np
import pandas as pd
import itertools

# create function to simulate entire series
def riddler(a_pct = 0.5):
    "function to simulate best of seven series"
    
    # initialize team a and team b's wins
    a_wins, b_wins = 0, 0
    
    # simulate series, one game at a time (given user input for team a win prob)
    while (a_wins < 4) & (b_wins < 4):
        game = np.random.binomial(1, a_pct, 1)
        
        if int(game) == 1:
            a_wins += 1
        else:
            b_wins += 1
    
    # return a_pct and number of games played
    return a_pct, (a_wins + b_wins)

# simulation parameters
## n = number of simulations per probability
## prob = team a's probabilities to simulate
## iter_values = itertools object of all probabilities to simulate in map() call below
n = 10000
prob = [0.5, 0.6, 0.7]
iter_values = itertools.chain.from_iterable((itertools.repeat(i, n) for i in prob))

# create generator object for mapped simulation
sim = (i for i in map(riddler, iter_values))

# call generator object and format results in DataFrame()
df = pd.DataFrame(sim, columns = ['a_pct', 'n_games'])

# results
df.groupby('a_pct').agg(['mean', 'count'])
