# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: colony_all.R
# Last updated: 2025-05-28
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Libraries -------------------------------------------------------------
library(ggplot2)
library(tidyverse)

# Load data -------------------------------------------------------------
colony <- read_csv("./data/Mangrove_Bay_corals.csv")
colony <- subset(colony, Rank %in% c("Genus", "Species"))
colony <- colony %>%
  select(Age, Genus, `End-Start (Intercept)`)
# Summary stats
colony <- colony %>%
  group_by(Genus, Age) %>%
  mutate(median = median(`End-Start (Intercept)`),
         n = length(`End-Start (Intercept)`))

# Set factor levels
colony$Age <- factor(colony$Age, levels = c("Modern", "MIS5e"))

# n labels
modern <- unique(colony[which(colony$Age == "Modern"), ])
modern$n <- paste0("n = ", modern$n, " (Modern)")
mis5e  <- unique(colony[which(colony$Age == "MIS5e"), ])
mis5e$n <- paste0("n = ", mis5e$n, " (MIS5e)")
# Plot data -------------------------------------------------------------

ggplot(data = colony, aes(x = `End-Start (Intercept)`, y = after_stat(count))) +
  # Plot density
  geom_histogram(aes(fill = Age, colour = Age), bins = 30, position = "identity", alpha = 0.4) +
  # Add vertical line of median value
  geom_vline(aes(xintercept = median, colour = Age), linetype = 2) +
  # Add points of the median value
  geom_point(aes(x = median, y = 0, fill = Age), 
             colour = "black", shape = 23, size = 1) +
  # Add text label of the number of intercepts
  geom_text(data = modern, aes(x = 80, y = 47, label = n), 
            hjust = 0, size = 1.8, colour = "grey15") +
  geom_text(data = mis5e, aes(x = 80, y = 41, label = n), 
            hjust = 0, size = 1.8, colour = "grey15") +
  # Transform x-axis to log10
  scale_x_continuous(trans = "log10") +
  # Change label names
  scale_fill_discrete(labels = c("Modern" = "Modern", "MIS5e" = "MIS5e (Last Interglacial)")) +
  scale_colour_discrete(labels = c("Modern" = "Modern", "MIS5e" = "MIS5e (Last Interglacial)")) +
  # Y-axis lavel
  ylab(lab = "Number of Intercepts") +
  # X-axis label
  xlab(lab = "Colony Size (cm)") +
  # Create facets across taxa with free scales
  facet_wrap(~Genus, ncol = 4) +
  # Set themes
  theme_bw() +
  theme(
    legend.key.size = unit(1, 'cm'),
    legend.key.height = unit(1, 'cm'),
    legend.key.width = unit(1, 'cm'),
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "italic")
  )

ggsave(filename = "./figures/size-distribution-all.png",
       width = 200, height = 275, units = "mm", dpi = 300,
       bg = "white")
