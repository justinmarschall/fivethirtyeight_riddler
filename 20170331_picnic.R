# Justin Marschall
# 538 Riddler 20170331
# Puzzle: https://fivethirtyeight.com/features/what-are-the-chances-well-meet-for-lunch/


# Riddle ------------------------------------------------------------------

# On a lovely spring day, you and I agree to meet for a lunch picnic at the fountain in the center of our favorite park. 
# We agree that we'll each arrive sometime from noon and 1 p.m., and that whoever arrives first will wait up to 15 minutes for the other. 
# If the other person doesn't show by then, the first person will abandon the plans and spend the day with a more punctual friend. 
# If we both arrive at the fountain at an independently random time between noon and 1, what are the chances our picnic actually happens?

# Simulation --------------------------------------------------------------

set.seed(20170331)

results <- NULL
for(i in 1:10000){
  
  # random time that x1 shows up
  x1 <- sample(1:60, 1, replace = FALSE)
  
  # random time that x2 shows up
  x2 <- sample(1:60, 1, replace = FALSE)
  
  # finds first person to show up (minute)
  xfirst <- min(x1, x2)
  
  # finds second person to show up (minute)
  xsecond <- max(x1, x2)
  
  # show down
  meet <- data.frame(
              x1 = x1,
              x2 = x2,
              xfirst = xfirst,
              xsecond = xsecond,
              meet = ifelse(xsecond <= (xfirst + 15), 1, 0)
              )
  
  # results
  results <- rbind(results, as.data.frame(meet))
  
}


# Results -----------------------------------------------------------------

# proportion where picnic occurs
mean(results$meet)





