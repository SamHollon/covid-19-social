# Libraries
library(ggplot2)
library(viridis)

# Number of times to repeat each model condition
reps <- 50

# Initialize an empty data frame in which store results
results2 <- data.frame(group.size = numeric(),
                       outbreak.size = numeric())

# Loop through values for group.size
for(i in seq(from = 1, to = 20, by = 1)) {
  # Repeat a number of times equal to reps
  for(j in 1:reps) {
    group.size <- i
    
    # Create new data frame to store a single row of data for this run
    result <- data.frame(group.size = i,
                         outbreak.size = outbreak.size(spread()))
    
    results2[nrow(results2) + 1,] <- result
  }
}


# Save the results
write.csv(results2, "results2.csv")


# Visualize the results and save them to a PDF
pdf("figures.pdf", height = 4, width = 6)
ggplot(data = results2, aes(x = group.size, y = outbreak.size)) +
  geom_point(size = 3, color = "#04bcc6", alpha = 0.1) +
  stat_smooth(method = lm,
              formula = y ~ poly(x, 11),
              se = F,
              size = 1.5,
              color = "#fe7370",
              alpha = 0.1) +
  xlab("Gathering Size (number of members)") +
  ylab("Outbreak Size (number of cases)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey99",
                                        colour = "grey80"),
        plot.title = element_text(hjust = 0.5))
dev.off()