#
#
# FIGURES
# Creates figures and saves them to PDFs.
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
results.gathering.size.gatherings.per.hour <-
  read.csv("results.gathering.size.gatherings.per.hour.csv")
results.gathering.size.p.transmit <-
  read.csv("results.gathering.size.p.transmit.csv")
results.gathering.size.t.recovery <-
  read.csv("results.gathering.size.t.recovery.csv")


# =============================================================================
# --- sensitive analysis: gathering size ---

# Visualize the results in a scatter plot with a polynomial fit
plot.gathering.size <- ggplot(data = results.gathering.size,
                              aes(x = gathering.size,
                                  y = outbreak.size)) +
  geom_point(size = 3, color = "#04bcc6", alpha = 0.15) +
  xlab("Members Per Gathering") +
  ylab("Cases During Outbreak") +
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
  ylab("Cases During Outbreak") +
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
  ylab("Cases During Outbreak") +
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
  ylab("Cases During Outbreak") +
  ylim(0, 1000) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey99",
                                        colour = "grey80"),
        plot.title = element_text(hjust = 0.5))



# =============================================================================
# --- sensitive analysis: gathering size & gatherings per hour ---

# Classify outbreak sizes in results.gathering.size.gatherings.per.hour into
# three categories by outbreak size: under 150, over 850, or in between. Let's
# call this new classification the "outcome" of a run.
results.gathering.size.gatherings.per.hour$outcome <-
  cut(results.gathering.size.gatherings.per.hour$outbreak.size,
      breaks = c(0, 100, 900, Inf), labels = c("0-100", "101-900", "901-1000"))

# Graph outcomes by gathering size and gatherings per hour
plot.gathering.size.gatherings.per.hour <-
  ggplot(data = results.gathering.size.gatherings.per.hour,
         aes(x = gathering.size, y = gatherings.per.hour, col = outcome)) +
  geom_point(size = 4) +
  xlab("Members Per Gathering") +
  ylab("Gatherings Per Hour") +
  labs(color = "Cases During Outbreak") +
  scale_color_manual(values = c("#04bcc6", "#fbde74", "#fe7370")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey99",
                                        colour = "grey80"),
        plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank())



# =============================================================================
# --- sensitive analysis: gathering size & transmission probability ---

# Classify outbreak sizes in results.gathering.size.p.transmit into three
# categories by outbreak size: under 150, over 850, or in between. Let's call
# this new classification the "outcome" of a run.
results.gathering.size.p.transmit$outcome <-
  cut(results.gathering.size.p.transmit$outbreak.size,
      breaks = c(0, 100, 900, Inf), labels = c("0-100", "101-900", "901-1000"))

# Graph outcomes by gathering size and gatherings per hour
plot.gathering.size.p.transmit <-
  ggplot(data = results.gathering.size.p.transmit,
         aes(x = gathering.size, y = p.transmit, col = outcome)) +
  geom_point(size = 4) +
  xlab("Members Per Gathering") +
  ylab("Probability of Transmission") +
  labs(color = "Cases During Outbreak") +
  scale_color_manual(values = c("#04bcc6", "#fbde74", "#fe7370")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey99",
                                        colour = "grey80"),
        plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank())



# =============================================================================
# --- sensitive analysis: gathering size & recovery time ---

# Classify outbreak sizes in results.gathering.size.t.recovery into three
# categories by outbreak size: under 150, over 850, or in between. Let's call
# this new classification the "outcome" of a run.
results.gathering.size.t.recovery$outcome <-
  cut(results.gathering.size.t.recovery$outbreak.size,
      breaks = c(0, 100, 900, Inf), labels = c("0-100", "101-900", "901-1000"))

# Graph outcomes by gathering size and gatherings per hour
plot.gathering.size.t.recovery <-
  ggplot(data = results.gathering.size.t.recovery,
         aes(x = gathering.size, y = t.recovery, col = outcome)) +
  geom_point(size = 4) +
  xlab("Members Per Gathering") +
  ylab("Time Until Recovery (time steps)") +
  labs(color = "Cases During Outbreak") +
  scale_color_manual(values = c("#04bcc6", "#fbde74", "#fe7370")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey99",
                                        colour = "grey80"),
        plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank())



# =============================================================================
# --- save figures ---

# Begin PDF
pdf("figures.pdf", height = 4, width = 6)

# Add each plot
plot.gathering.size
plot.gatherings.per.hour
plot.p.transmit
plot.t.recovery
plot.gathering.size.gatherings.per.hour
plot.gathering.size.p.transmit
plot.gathering.size.t.recovery

# End PDF
dev.off()