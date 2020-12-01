# Libraries
library(ggplot2)

# Call the spread function and save the result
results <- spread()

# Plot the results
ggplot(data = results, aes(x = time, y = I)) +
  geom_line()