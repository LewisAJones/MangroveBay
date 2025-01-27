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
library(ggtext)

# Load data -------------------------------------------------------------
abundance <- read_csv("./results/abundance_site.csv")

# Convert to percentages
abundance$Abundance <- abundance$Abundance * 100

# Set factor levels
abundance$Age <- factor(abundance$Age, levels = c("Modern", "MIS5e"))
abundance$ReefZone <- factor(abundance$ReefZone, levels = c("Reef edge", 
                                                            "Reef slope"))

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

# Get the cumulative sum
abundance <- abundance %>%
  mutate(Genus = factor(abundance$Genus,
                        levels = c("Acropora", "Echinopora", "Galaxea",
                                   "Goniastrea", "Lobophyllia", "Millepora",
                                   "Pocillopora", "Porites", "Other"))) %>%
  group_by(Age, ReefZone) %>%
  arrange(desc(Genus)) %>%
  mutate(label_y_upper = cumsum(Abundance),
         label_y_lower = c(0, label_y_upper[1:length(label_y_upper)-1]),
         label_y = (label_y_upper + label_y_lower) / 2)

# Set font face
abundance$fontface <- "italic"
abundance$fontface[which(abundance$Genus == "Other")] <- "plain"

# Generate plots --------------------------------------------------------
ggplot(data = abundance, aes(x = 1, y = Abundance, fill = Genus)) +
  geom_col(linewidth = 0.5, colour = "black") + 
  geom_text(aes(y = label_y, label = Genus), 
            fontface = abundance$fontface, colour = "white") +
  ylab("Abundance (%)") +
  scale_fill_discrete(
    labels = c("*Acropora*", "*Echinopora*", "*Galaxea*",
                "*Goniastrea*", "*Lobophyllia*", "*Millepora*",
                "*Pocillopora*", "*Porites*", "Other")
  ) +
  #scale_fill_manual(values = met.brewer(name="Hokusai2", n = 31, type="continuous")) +
  facet_grid(Age~ReefZone) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_markdown(),
        plot.margin = margin(10, 5, 5, 5, unit = "mm"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  guides(fill = guide_legend(byrow = TRUE))

# Arrange and save ------------------------------------------------------
ggsave("figures/community_composition.png", dpi = 600,
       width = 200, height = 200, units = "mm", scale = 1)

