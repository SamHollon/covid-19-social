#
#
# FUNCTIONS
# Defines functions for use in other scripts.
#
#



# =============================================================================
# --- outbreak function ---

# ARGUMENTS
#    N is the total number of people in the population (default 1000)
#    I.prop is the proportion of people infected at the start of the run
#      (default 0.01)
#    t.max is the number of time steps before the model stops (default 3000)
#    gatherings.per.hour is the average number of gatherings each person
#       attends per hour (default 2/3)
#    p.transmit is the probability of transmission (default 0.1475)
#    gathering.size is the number of members per gathering (default 10)
#    t.recovery is the number of time steps until an infected person recovers
#       (default 240)

outbreak <- function(N = 1000,
                   I.prop = 0.01,
                   t.max = 3000,
                   gatherings.per.hour = 2/3,
                   p.transmit = 0.1475,
                   t.recovery = 240,
                   gathering.size = 10) {
  
  # Create the initial population
  I <- round(N * I.prop)  # initial number infected
  S <- N - I  # initial number susceptible
  
  # Record the states of each member of the population in a vector.
  pop <- c(rep(0, S), rep(2, I))
  # Value = 0 means susceptible
  # Value = 1 means removed
  # Value >= 2 means infected
  
  
  # Set the number of social gatherings per hour for the entire population
  num.groups <- round(N * gatherings.per.hour)
  
  # Save a new copy of the population
  pop.copy <- pop
  
  # Create a new data frame to store the population history.
  history <- data.frame(time = numeric(),
                        S = numeric(),
                        I = numeric(),
                        R = numeric())
  
  # Add a single row containing the counts of each population type.
  history[nrow(history) + 1,] <- list(0,
                                      length(pop.copy[pop.copy == 0]),
                                      length(pop.copy[pop.copy >= 2]),
                                      length(pop.copy[pop.copy == 1]))
  
  
  # BEGIN PROCEDURE FOR EACH TIME STEP
  
  for(timestep in 1:t.max) {
    # Randomly sample the population a number of times equal to num.groups.
    # Each sample corresponds to a social gathering.
    for(i in num.groups) {
      # Sample a number of indices equal to gathering.size
      indices <- sample(1:length(pop.copy), gathering.size)
      
      # Create a counter to store the number of infected people in the group
      I.here <- 0
      
      # Check whether each person in pop at the indices sampled is infected
      for(j in indices) {
        # If the person is infected, increase I.here by 1
        if(pop.copy[j] >= 2) {
          I.here <- I.here + 1
        }
      }
      
      # If I.here > 0...
      if(I.here > 0) {
        # Check whether each person in pop at the indices sampled is susceptible
        for (j in indices) {
          # If the person is susceptible...
          if(pop.copy[j] == 0) {
            # Change the person to infected with a probability of p.transmit
            # for EACH infected person in the group.
            if(min(runif(I.here, 0, 1)) < p.transmit) {
              pop.copy[j] <- 2
            }
          }
        }
      }
    }
    
    # Check if each person in the population is infected
    for(i in 1:length(pop.copy)) {
      # If the person is infected...
      if(pop.copy[i] >= 2) {
        # Increment the person's state by 1/t.recovery.
        pop.copy[i] <- pop.copy[i] + 1 / t.recovery
        
        # If the person's state >= 3, change the person's status to
        # removed.
        if(pop.copy[i] >= 3) {
          pop.copy[i] <- 1
        }
      }
    }
    
    # Append the updated S, I, and R counts to history
    # Add a single row containing the counts of each population type.
    history[nrow(history) + 1,] <- list(timestep,
                                        length(pop.copy[pop.copy == 0]),
                                        length(pop.copy[pop.copy >= 2]),
                                        length(pop.copy[pop.copy == 1]))
  }
  
  # END PROCEDURE FOR EACH TIME STEP
  
  
  # Output the population history. This output is a data frame.
  return(history)
}



# =============================================================================
# --- outbreak size function ---

# SUMMARY: Calculates I + R at last time step of an outbreak. This is the total
# number of cases that occur throughout the outbreak, i.e, what we're calling
# the "outbreak size."

# ARGUMENT
#    df is the output of the outbreak function (this is a data frame)

outbreak.size <- function(df) {
  return(df$I[length(df$I)] + df$R[length(df$R)]) 
}