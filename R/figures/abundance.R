# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: abundance.R
# Last updated: 2024-09-24
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Load packages ---------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(MetBrewer)

# Load data -------------------------------------------------------------
abundance <- read_csv("./results/abundance.csv")

# Convert to percentages
abundance$Abundance <- abundance$Abundance * 100

# Set factor levels
abundance$Age <- factor(abundance$Age, levels = c("Modern", "MIS5e"))
abundance$ReefZone <- factor(abundance$ReefZone, levels = c("Reef edge", 
                                                          "Reef slope"))

# Generate plots --------------------------------------------------------
ggplot(data = abundance, aes(x = 1, y = Abundance, fill = Genus)) +
  geom_bar(stat = "identity", linewidth = 0.5, colour = "black") +
  ylab("Abundance (%)") +
  scale_fill_manual(values = met.brewer(name="Hokusai2", n = 31, type="continuous")) +
  facet_grid(Age~ReefZone) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.margin = margin(10, 5, 5, 5, unit = "mm"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  guides(fill = guide_legend(nrow = 5, byrow = TRUE))


# Arrange and save ------------------------------------------------------
ggsave("figures/community_composition.png", dpi = 600,
       width = 200, height = 200, units = "mm", scale = 1)

