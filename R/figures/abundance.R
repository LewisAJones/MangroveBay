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
                                                            "Shallow reef slope",
                                                            "Deeper reef slope"))

# Labels
labs <- c("Reef edge" = "Reef edge",
          "Shallow reef slope" = "Shallow reef slope",
          "Deeper reef slope" = "Deeper reef slope",
          "Modern" = "Modern",
          "MIS5e" = "MIS5e (Last Interglacial)")

# Calculate top five ----------------------------------------------------
# Summarise via age and reef zone
abundance <- abundance %>%
  group_by(Age, ReefZone) %>%
  mutate(Threshold = Abundance >= 10)
# Add index
abundance$Genus[which(abundance$Threshold == FALSE)] <- "Other"

abundance <- abundance %>%
  # Add transect proportion
  group_by(Age, ReefZone, Genus) %>%
  # Add transect proportion
  summarise(Abundance = sum(Abundance)) %>%
  as.data.frame()

# Set factor levels
abundance <- abundance %>%
  mutate(Genus = factor(abundance$Genus,
                        levels = c("Acropora", "Echinopora", "Galaxea",
                                   "Goniastrea", "Lobophyllia", "Millepora",
                                   "Pocillopora", "Porites", "Other")))

# Generate plots --------------------------------------------------------
ggplot(data = abundance, aes(x = Genus, y = Abundance, fill = Genus)) +
  geom_col(colour = "black") +
  geom_text(aes(x = Genus, 
                y = Abundance / 2, 
                label = paste0(round(Abundance, 2), "%")),
            colour = "black", size = 2.25, vjust = 1) +
  ylab("Abundance (%)") +
  xlab ("Genus") +
  facet_grid(Age~ReefZone, labeller = as_labeller(labs)) +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),,
        plot.margin = margin(10, 5, 5, 5, unit = "mm"),
        axis.text.x = element_text(face = c(rep("italic", 8), "plain"), 
                                   angle = 90, vjust = 0.5, hjust = 1)) +
  guides(fill = guide_legend(byrow = TRUE))

# Arrange and save ------------------------------------------------------
ggsave("figures/community_composition.png", dpi = 600,
       width = 300, height = 200, units = "mm", scale = 1)

