# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: metrics.R
# Last updated: 2024-08-11
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Libraries -------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(scales)
library(MetBrewer)

# Load data -------------------------------------------------------------
indices <- read.csv("./results/diversity_indices.csv")
indices <- indices %>%
  pivot_longer(Alpha:Pielou)

indices$Age <- factor(indices$Age, levels = c("Modern", "MIS5e"))
indices$name <- factor(indices$name, levels = c("Alpha","Shannon", "Simpson", "Pielou"))
indices$ReefZone <- factor(indices$ReefZone, levels = c("Reef edge", 
                                                        "Reef slope"))

# Plot data -------------------------------------------------------------

ggplot(data = indices, aes(x = ReefZone, y = value, 
                           fill = Age, shape = ReefZone)) +
  geom_point(colour = "black", size = 3, alpha = 0.75) +
  scale_fill_met_d("Hokusai2") +
  scale_shape_manual(values = c("Reef edge" = 21, "Reef slope" = 22)) +
  facet_grid(name~Age, scales = "free") +
  ylab("Metric") +
  xlab("Reef Zone") +
  scale_x_discrete(labels = wrap_format(12)) +
  theme_bw() +
  theme(
    legend.position = "none"
  )

# Save ------------------------------------------------------------------

ggsave("./figures/metrics.png",
       height = 175, width = 125, units = "mm",
       dpi = 300)

