#
#
# FIGURES
# Creates figures and saves them to a PDF
#
#

# =============================================================================
# --- libraries ---

library(ggplot2)
library(viridis)



# =============================================================================
# --- data ---

results.gathering.size <- read.csv("results.gathering.size.csv")
results.gatherings.per.hour <- read.csv("results.gatherings.per.hour.csv")
results.p.transmit <- read.csv("results.p.transmit.csv")
results.t.recovery <- read.csv("results.t.recovery.csv")



# =============================================================================
# --- sensitive analysis: gathering size ---

# Visualize the results in a scatter plot with a polynomial fit
plot.gathering.size <- ggplot(data = results.gathering.size,
                              aes(x = gathering.size,
                                  y = outbreak.size)) +
  geom_point(size = 3, color = "#04bcc6", alpha = 0.15) +
  xlab("Members Per Gathering") +
  ylab("Cases Throughout Outbreak") +
  ylim(0, 1000) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey99",
                                        colour = "grey80"),
        plot.title = element_text(hjust = 0.5))



# =============================================================================
# --- sensitive analysis: gatherings per hour ---

# Visualize the results in a scatter plot with a polynomial fit
plot.gatherings.per.hour <- ggplot(data = results.gatherings.per.hour,
                                   aes(x = gatherings.per.hour,
                                       y = outbreak.size)) +
  geom_point(size = 3, color = "#04bcc6", alpha = 0.15) +
  xlab("Gatherings Per Hour") +
  ylab("Cases Throughout Outbreak") +
  ylim(0, 1000) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey99",
                                        colour = "grey80"),
        plot.title = element_text(hjust = 0.5))



# =============================================================================
# --- sensitive analysis: transmission probability ---

# Visualize the results in a scatter plot with a polynomial fit
plot.p.transmit <- ggplot(data = results.p.transmit,
                          aes(x = p.transmit,
                              y = outbreak.size)) +
  geom_point(size = 3, color = "#04bcc6", alpha = 0.15) +
  xlab("Probability of Transmission") +
  ylab("Cases Throughout Outbreak") +
  ylim(0, 1000) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey99",
                                        colour = "grey80"),
        plot.title = element_text(hjust = 0.5))



# =============================================================================
# --- sensitive analysis: recovery time ---

# Visualize the results in a scatter plot with a polynomial fit
plot.t.recovery <- ggplot(data = results.t.recovery,
                          aes(x = t.recovery,
                              y = outbreak.size)) +
  geom_point(size = 3, color = "#04bcc6", alpha = 0.15) +
  xlab("Time Until Removal (timesteps)") +
  ylab("Cases Throughout Outbreak") +
  ylim(0, 1000) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey99",
                                        colour = "grey80"),
        plot.title = element_text(hjust = 0.5))



# =============================================================================
# --- save figures ---

# Begin PDF
pdf("figures.pdf", height = 4, width = 6)

# Add each plot
plot.gathering.size
plot.gatherings.per.hour
plot.p.transmit
plot.t.recovery

# End PDF
dev.off()