# Create the initial population
N = 10000  # total population
I.prop = 0.05  # proportion infected
I = round(N * I.prop)  # initial number infected
S = N - I
R = 0
t.max = 1000  # maximum timestep (run duration)
pop = c(rep(0, S), rep(1, R), rep(2, I))
# Value = 0 means susceptible
# Value = 1 means removed
# Value >= 2 means infected

# Set other model parameters
p.transmit <- 1
num.groups <- 5000
group.size <- 25
t.recovery <- 30

# Spread function
# 1st arg = population vector for previous time period
# 2nd arg = current time step
spread <- function(population = pop, timestep = 0, history = NULL) {
  # Save a new copy of the population
  pop.copy <- population
  
  # If history is null
  if(is.null(history)) {
    # Create a new data frame to store the population history.
    hist.df <- data.frame(time = numeric(),
                          S = numeric(),
                          I = numeric(),
                          R = numeric())
  } else {
    hist.df = history
  }
  
  # Add a single row containing the counts of each population type.
  hist.df[nrow(hist.df) + 1,] <- list(timestep,
                                      length(pop.copy[pop.copy == 0]),
                                      length(pop.copy[pop.copy >= 2]),
                                      length(pop.copy[pop.copy == 1]))
  
  # Randomly sample the population a number of times equal to num.groups.
  # Each sample corresponds to a social gathering.
  for(i in num.groups) {
    # Sample a number of indices in population equal to group.size
    indices <- sample(1:length(pop.copy), group.size)
    
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
      # recovered.
      if(pop.copy[i] >= 3) {
        pop.copy[i] <- 1
      }
    }
  }
  
  # If we're about to the maximum time step...
  if (timestep == t.max - 1) {
    # Append the updated S, I, and R counts to hist.df
    # Add a single row containing the counts of each population type.
    hist.df[nrow(hist.df) + 1,] <- list(timestep + 1,
                                        length(pop.copy[pop.copy == 0]),
                                        length(pop.copy[pop.copy >= 2]),
                                        length(pop.copy[pop.copy == 1]))
    
    # Output the population history
    return(hist.df)
  } else {
    # Call this function recursively with the updated population and history
    # and with timestep incremented by 1.
    spread(pop.copy, timestep + 1, hist.df)
  }
}