#
#
# ANALYSIS
# Runs sensitivity analyses by calculating how varying the model parameters
# affects the simulated outbreak sizes. Saves the results as CSVs for use in
# other scripts.
#
#



# =============================================================================
# --- source scripts ---

source("model.R")



# =============================================================================
# --- test parameters ---

# number of times to repeat each (identical) model condition
reps <- 10

# Sequence of gathering sizes (number of members) to simulate
seq.gathering.size <- seq(from = 1, to = 20, by = 1)

# Sequence of gatherings per hour to simulate
seq.gatherings.per.hour <- seq(from = 0.2, to = 4, by = 0.2)

# Sequence of transmission probabilities to simulate
seq.p.transmit <- seq(from = 0.02, to = 0.4, by = 0.02)

# Sequence of transmission probabilities to simulate
seq.t.recovery <- seq(from = 48, to = 960, by = 48)



# =============================================================================
# --- sensitive analysis: gathering size ---

# Initialize an empty data frame to store results
results.gathering.size <- data.frame(gathering.size = numeric(),
                                     outbreak.size = numeric())

# Loop through values for gathering size
for(i in seq.gathering.size) {
  # Repeat a number of times equal to reps
  for(j in 1:reps) {
    
    # Create new data frame to store a single row of data for this run
    result <- data.frame(gathering.size = i,
                         outbreak.size =
                           outbreak.size(outbreak(gathering.size = i)))
    
    # Add the results to the results data frame
    results.gathering.size[nrow(results.gathering.size) + 1,] <- result
  }
}



# =============================================================================
# --- sensitive analysis: gatherings per hour ---

# Initialize an empty data frame to store results
results.gatherings.per.hour <- data.frame(gatherings.per.hour = numeric(),
                                          outbreak.size = numeric())

# Loop through values for gatherings per hour
for(i in seq.gatherings.per.hour) {
  # Repeat a number of times equal to reps
  for(j in 1:reps) {
    
    # Create new data frame to store a single row of data for this run
    result <- data.frame(gatherings.per.hour = i,
                         outbreak.size =
                           outbreak.size(outbreak(gatherings.per.hour = i)))
    
    # Add the results to the results data frame
    results.gatherings.per.hour[
      nrow(results.gatherings.per.hour) + 1,] <- result
  }
}



# =============================================================================
# --- sensitive analysis: transmission probability ---

# Initialize an empty data frame to store results
results.p.transmit <- data.frame(p.transmit = numeric(),
                                 outbreak.size = numeric())

# Loop through values for transmission probability
for(i in seq.p.transmit) {
  # Repeat a number of times equal to reps
  for(j in 1:reps) {
    
    # Create new data frame to store a single row of data for this run
    result <- data.frame(p.transmit = i,
                         outbreak.size =
                           outbreak.size(outbreak(p.transmit = i)))
    
    # Add the results to the results data frame
    results.p.transmit[
      nrow(results.p.transmit) + 1,] <- result
  }
}



# =============================================================================
# --- sensitive analysis: recovery time ---

# Initialize an empty data frame to store results
results.t.recovery <- data.frame(t.recovery = numeric(),
                                 outbreak.size = numeric())

# Loop through values for recovery time
for(i in seq.t.recovery) {
  # Repeat a number of times equal to reps
  for(j in 1:reps) {
    
    # Create new data frame to store a single row of data for this run
    result <- data.frame(t.recovery = i,
                         outbreak.size =
                           outbreak.size(outbreak(t.recovery = i)))
    
    # Add the results to the results data frame
    results.t.recovery[
      nrow(results.t.recovery) + 1,] <- result
  }
}



# =============================================================================
# --- sensitive analysis: gathering size & gatherings per hour ---

# Initialize an empty data frame to store results
results.gathering.size.gatherings.per.hour <-
  data.frame(gathering.size = numeric(),
             gatherings.per.hour = numeric(),
             outbreak.size = numeric())

# Loop through values for gathering size
for(i in seq.gathering.size) {
  # Loop through values for transmission probability
  for(j in seq.gatherings.per.hour) {
    # Create new data frame to store a single row of data for this run
    result <- data.frame(gathering.size = i,
                         gatherings.per.hour = j,
                         outbreak.size =
                           outbreak.size(outbreak(gathering.size = i,
                                                  gatherings.per.hour = j)))
    
    # Add the results to the results data frame
    results.gathering.size.gatherings.per.hour[
      nrow(results.gathering.size.gatherings.per.hour) + 1,] <- result
  }
}



# =============================================================================
# --- sensitive analysis: gathering size & transmission probability ---

# Initialize an empty data frame to store results
results.gathering.size.p.transmit <- data.frame(gathering.size = numeric(),
                                 p.transmit = numeric(),
                                 outbreak.size = numeric())

# Loop through values for gathering size
for(i in seq.gathering.size) {
  # Loop through values for transmission probability
  for(j in seq.p.transmit) {
    # Create new data frame to store a single row of data for this run
    result <- data.frame(gathering.size = i,
                         p.transmit = j,
                         outbreak.size =
                           outbreak.size(outbreak(gathering.size = i,
                                                  p.transmit = j)))
    
    # Add the results to the results data frame
    results.gathering.size.p.transmit[
      nrow(results.gathering.size.p.transmit) + 1,] <- result
  }
}


# =============================================================================
# --- sensitive analysis: gathering size & recovery time ---

# Initialize an empty data frame to store results
results.gathering.size.t.recovery <- data.frame(gathering.size = numeric(),
                                                t.recovery = numeric(),
                                                outbreak.size = numeric())

# Loop through values for gathering size
for(i in seq.gathering.size) {
  # Loop through values for transmission probability
  for(j in seq.t.recovery) {
    # Create new data frame to store a single row of data for this run
    result <- data.frame(gathering.size = i,
                         t.recovery = j,
                         outbreak.size =
                           outbreak.size(outbreak(gathering.size = i,
                                                  t.recovery = j)))
    
    # Add the results to the results data frame
    results.gathering.size.t.recovery[
      nrow(results.gathering.size.t.recovery) + 1,] <- result
  }
}



# =============================================================================
# --- save the results ---

write.csv(results.gathering.size,
          "results.gathering.size.csv")
write.csv(results.gatherings.per.hour,
          "results.gatherings.per.hour.csv")
write.csv(results.p.transmit,
          "results.p.transmit.csv")
write.csv(results.t.recovery,
          "results.t.recovery.csv")
write.csv(results.gathering.size.gatherings.per.hour,
          "results.gathering.size.gatherings.per.hour.csv")
write.csv(results.gathering.size.p.transmit,
          "results.gathering.size.p.transmit.csv")
write.csv(results.gathering.size.t.recovery,
          "results.gathering.size.t.recovery.csv")