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
library(ggtext)

# Load data -------------------------------------------------------------
abundance <- read_csv("./results/abundance_site.csv")

# Convert to percentages
abundance$Abundance <- abundance$Abundance * 100

# Set factor levels
abundance$Age <- factor(abundance$Age, levels = c("Modern", "MIS5e"))
abundance$ReefZone <- factor(abundance$ReefZone, levels = c("Reef edge", 
                                                          "Reef slope"))

# Labels
labs <- c("Reef edge" = "Reef edge",
          "Reef slope" = "Reef slope",
          "Modern" = "Modern",
          "MIS5e" = "MIS5e (Last Interglacial)")

# Generate plots --------------------------------------------------------
ggplot(data = abundance, aes(x = Genus, y = Abundance, fill = Genus)) +
  geom_col(colour = "black") +
  ylab("Abundance (%)") +
  xlab ("Genus") +
  facet_grid(Age~ReefZone, labeller = as_labeller(labs)) +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.margin = margin(10, 5, 5, 5, unit = "mm"),
        axis.text.x = element_text(face = "italic", 
                                   angle = 90, vjust = 0.5, hjust = 1)) +
  guides(fill = guide_legend(byrow = TRUE))



# Arrange and save ------------------------------------------------------
ggsave("figures/community_composition_all.png", dpi = 600,
       width = 250, height = 200, units = "mm", scale = 1)

