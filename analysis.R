# Libraries
library(ggplot2)


# NO LONGER NEEDED -- VISUALIZES A SINGLE OUTBREAK
# # Call the spread function and save the result
# results <- spread()
# 
# # Plot the results
# ggplot(data = results, aes(x = time, y = I)) +
#   geom_line()
# 
# outbreak.size(results)

# Initialize an empty data frame in which store results
results <- data.frame(num.groups = numeric(),
                      group.size = numeric(),
                      outbreak.size = numeric())

# Loop through values for num.groups
for(i in seq(from = 500, to = 10000, by = 500)) {
  # Loop through values for group.size
  for(j in seq(from = 1, to = 20, by = 1)) {
    num.groups <- i
    group.size <- j
    
    # Create new data frame to store a single row of data for this run
    result <- data.frame(num.groups = i,
                         group.size = j,
                         outbreak.size = outbreak.size(spread()))

    results[nrow(results) + 1,] <- result
  }
}

# Save the results
write.csv(results, "results.csv")

# Visualize the results
ggplot(data = results, aes(x = num.groups,
                           y = group.size,
                           z = outbreak.size)) +
  stat_summary_2d(bins = c(19, 19))
