# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: colony.R
# Last updated: 2024-09-24
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Libraries -------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(MetBrewer)

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
# Exclude genera with small number of intercepts
exclude <- colony[which(colony$n < 25), "Genus"]$Genus
colony <- subset(colony, !Genus %in% exclude)

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
  geom_histogram(aes(fill = Age, colour = Age), position="identity", alpha = 0.4) +
  # Add vertical line of median value
  geom_vline(aes(xintercept = median, colour = Age), linetype = 2) +
  # Add points of the median value
  geom_point(aes(x = median, y = 0, fill = Age), colour = "black", shape = 21) +
  # Add text label of the number of intercepts
  geom_text(data = modern, aes(x = 80, y = 24, label = n), 
            hjust = 0, size = 2.7, colour = "grey15") +
  geom_text(data = mis5e, aes(x = 80, y = 21, label = n), 
            hjust = 0, size = 2.7, colour = "grey15") +
  # Transform x-axis to log10
  scale_x_continuous(trans = "log10", limits = c(1, 600)) +
  # Add colour scale
  scale_colour_met_d("Hokusai2") +
  # Add fill scale
  scale_fill_met_d("Hokusai2") +
  # Y-axis lavel
  ylab(lab = "Number of Intercepts") +
  # X-axis label
  xlab(lab = "Colony Size (cm)") +
  # Create facets across taxa with free scales
  facet_wrap(~Genus, ncol = 2) +
  # Set themes
  theme_bw() +
  theme(
    legend.key.size = unit(1, 'cm'),
    legend.key.height = unit(1, 'cm'),
    legend.key.width = unit(1, 'cm'),
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.position = c(0.75, 0.15),
    strip.text = element_text(face = "italic")
  )

ggsave(filename = "./figures/size-distribution.png",
       width = 150, height = 150, units = "mm", dpi = 300,
       bg = "white")
