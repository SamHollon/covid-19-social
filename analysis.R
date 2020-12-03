#
#
# ANALYSIS
# Runs sensitivity analyses by calculating how varying the model parameters
# affects the simulated outbreak sizes. Saves the results as CSVs and plots
# them as figures saved in a PDF.
#
#



# =============================================================================
# --- libraries ---

library(ggplot2)
library(viridis)



# =============================================================================
# --- source scripts ---

source("model.R")



# =============================================================================
# --- test parameters ---

# number of times to repeat each (identical) model condition
reps <- 10

# Sequence of gathering sizes (number of members) to simulate
seq.gathering.size <- seq(from = 1, to = 20, by = 1)

# Sequence of gatherings per hour (number of members) to simulate
seq.gatherings.per.hour <- seq(from = 0.2, to = 4, by = 0.2)


# =============================================================================
# --- sensitive analysis: gathering size ---

# Initialize an empty data frame to store results
results.gathering.size <- data.frame(group.size = numeric(),
                                     outbreak.size = numeric())

# Loop through values for group.size
for(i in seq.gathering.size) {
  # Repeat a number of times equal to reps
  for(j in 1:reps) {
    group.size <- i
    
    # Create new data frame to store a single row of data for this run
    result <- data.frame(group.size = i,
                         outbreak.size =
                           outbreak.size(outbreak(group.size = i)))
    
    results.gathering.size[nrow(results.gathering.size) + 1,] <- result
  }
}

# Visualize the results in a scatter plot with a polynomial fit
plot.gathering.size <- ggplot(data = results.gathering.size,
                              aes(x = group.size,
                                  y = outbreak.size)) +
  geom_point(size = 3, color = "#04bcc6", alpha = 0.15) +
  xlab("Members Per Gathering") +
  ylab("Cases Throughout Outbreak") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey99",
                                        colour = "grey80"),
        plot.title = element_text(hjust = 0.5))



# =============================================================================
# --- sensitive analysis: gatherings per hour ---

# Initialize an empty data frame to store results
results.gatherings.per.hour <- data.frame(gatherings.per.hour = numeric(),
                                     outbreak.size = numeric())

# Loop through values for group.size
for(i in seq.gatherings.per.hour) {
  # Repeat a number of times equal to reps
  for(j in 1:reps) {
    group.size <- i
    
    # Create new data frame to store a single row of data for this run
    result <- data.frame(gatherings.per.hour = i,
                         outbreak.size =
                           outbreak.size(outbreak(group.size = i)))
    
    results.gatherings.per.hour[
      nrow(results.gatherings.per.hour) + 1,] <- result
  }
}

# Visualize the results in a scatter plot with a polynomial fit
plot.gatherings.per.hour <- ggplot(data = results.gatherings.per.hour,
                              aes(x = gatherings.per.hour,
                                  y = outbreak.size)) +
  geom_point(size = 3, color = "#04bcc6", alpha = 0.15) +
  xlab("Gatherings Per Hour") +
  ylab("Cases Throughout Outbreak") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey99",
                                        colour = "grey80"),
        plot.title = element_text(hjust = 0.5))



# =============================================================================
# --- save the results and plots ---

# Save the results as CSVs
write.csv(results.gathering.size, "results.gathering.size.csv")

# Save the figures as a PDF
pdf("figures.pdf", height = 4, width = 6)
plot.gathering.size
dev.off()