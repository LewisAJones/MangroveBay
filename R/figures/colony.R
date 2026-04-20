# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: colony.R
# Last updated: 2026-04-15
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Libraries -------------------------------------------------------------
library(ggplot2)
library(tidyverse)

# Load data -------------------------------------------------------------
colony <- read_csv("data/Mangrove_Bay_corals.csv")
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
  geom_point(aes(x = median, y = 0, fill = Age), colour = "black", shape = 23) +
  # Add text label of the number of intercepts
  geom_text(data = modern, aes(x = Inf, y = Inf, label = n), 
            hjust = 1.1, vjust = 2, size = 2, colour = "grey15") +
  geom_text(data = mis5e, aes(x = Inf, y = Inf, label = n), 
            hjust = 1.1, vjust = 4, size = 2, colour = "grey15") +
  # Set pretty labels
  scale_y_continuous(breaks = scales::breaks_pretty()) +
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
  facet_wrap(~Genus) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 8, face = "italic"),
        strip.background = element_blank())

ggsave(filename = "figures/colony_size/size-distribution-all.png",
       width = 297, height = 210, units = "mm", dpi = 300,
       bg = "white")

# Exclude genera with small number of intercepts
keepers <- colony %>%
  filter(n >= 25) %>%
  select(Age, Genus, n) %>%
  distinct() %>%
  group_by(Genus) %>%
  count() %>%
  filter(n == 2) %>%
  pull(Genus)
# Subset
colony <- colony %>% filter(Genus %in% keepers)

ggplot(data = colony, aes(x = `End-Start (Intercept)`, y = after_stat(count))) +
  # Plot density
  geom_histogram(aes(fill = Age, colour = Age), bins = 30, position = "identity", alpha = 0.4) +
  # Add vertical line of median value
  geom_vline(aes(xintercept = median, colour = Age), linetype = 2) +
  # Add points of the median value
  geom_point(aes(x = median, y = 0, fill = Age), colour = "black", shape = 23) +
  # Add text label of the number of intercepts
  geom_text(data = modern %>% filter(Genus %in% keepers), aes(x = Inf, y = Inf, label = n), 
            hjust = 1.1, vjust = 2, size = 3.5, colour = "grey15") +
  geom_text(data = mis5e %>% filter(Genus %in% keepers), aes(x = Inf, y = Inf, label = n), 
            hjust = 1.1, vjust = 4, size = 3.5, colour = "grey15") +
  # Transform x-axis to log10
  scale_x_continuous(trans = "log10") +
  # Set pretty labels
  scale_y_continuous(breaks = scales::breaks_pretty()) +
  # Change label names
  scale_fill_discrete(labels = c("Modern" = "Modern", "MIS5e" = "MIS5e (Last Interglacial)")) +
  scale_colour_discrete(labels = c("Modern" = "Modern", "MIS5e" = "MIS5e (Last Interglacial)")) +
  # Y-axis lavel
  ylab(lab = "Number of Intercepts") +
  # X-axis label
  xlab(lab = "Colony Size (cm)") +
  # Create facets across taxa with free scales
  facet_wrap(~Genus, ncol = 2, scales = "free_y") +
  # Set themes
  theme_bw() +
  theme(legend.position = c(0.75, 0.15),
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12, face = "italic"),
        strip.background = element_blank())

ggsave(filename = "figures/colony_size/size-distribution.png",
       width = 297, height = 210, units = "mm", dpi = 300,
       bg = "white")
