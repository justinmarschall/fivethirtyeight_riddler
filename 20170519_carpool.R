# Justin Marschall

# http://fivethirtyeight.com/features/the-battle-for-riddler-nation-round-2/

# Four co-workers carpool to work each day. 
# A driver is selected randomly for the drive to work and again randomly for the drive home. 
# Each of the drivers has a lead foot, and each has a chance of being ticketed for speeding. 
# Driver A has a 10 percent chance of getting a ticket each time he drives, 
# Driver B a 15 percent chance, Driver C a 20 percent chance, and Driver D a 25 percent chance. 
# The state will immediately revoke the license of a driver after his or her third ticket, 
# and a driver will stop driving in the carpool once his license is revoked. 
# Since there is only one police officer on the carpool rout, a maximum of one ticket will be issued per morning and a max of one per evening.
# 
# Assuming that all four drivers start with no tickets, how many days can we expect the carpool to last until all the drivers have lost their licenses?


# build function ----------------------------------------------------------

# function to simulate tickets until no drivers remain
riddler <- function() {

  # set initial values for tickets, probability of driving, and probability of getting ticket
  a_tickets <- 0
  b_tickets <- 0
  c_tickets <- 0
  d_tickets <- 0
  
  a_pick <- 1
  b_pick <- 1
  c_pick <- 1
  d_pick <- 1
  
  a_ticket_prob <- 0.10
  b_ticket_prob <- 0.15
  c_ticket_prob <- 0.20
  d_ticket_prob <- 0.25
  
  n <- 0
  
  # simulate individual drive
  while (sum(a_tickets, b_tickets, c_tickets, d_tickets) < 12) {
    driver <- sample(c("A", "B", "C", "D"), 1, replace = FALSE, prob = c(a_pick, b_pick, c_pick, d_pick))
    
    ticket_prob <- switch(driver,
                          A = a_ticket_prob,
                          B = b_ticket_prob,
                          C = c_ticket_prob,
                          D = d_ticket_prob)
    
    ticket <- sample(c(1, 0), 1, replace = FALSE, prob = c(ticket_prob, 1 - ticket_prob))
    
    if(driver == "A") {
      a_tickets <- a_tickets + ticket
      a_pick <- if(a_tickets == 3) 0 else 1
      
    } else if(driver == "B") {
      b_tickets <- b_tickets + ticket 
      b_pick <- if(b_tickets == 3) 0 else 1
      
    } else if(driver == "C") {
      c_tickets <- c_tickets + ticket
      c_pick <- if(c_tickets == 3) 0 else 1
      
    } else {
      d_tickets <- d_tickets + ticket
      d_pick <- if(d_tickets == 3) 0 else 1
      
    }
    
    n <- n + 0.5
    
  }
  
  n

}


# simulation --------------------------------------------------------------

set.seed(12345)
sim_result <- replicate(10000, riddler())

mean(sim_result)

ggplot(data.frame(x = sim_result), aes(x)) +
  geom_density() +
  scale_x_continuous(limits = c(0, 120))

range(sim_result)
