# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: abundance.R
# Last updated: 2026-03-10
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Load packages ---------------------------------------------------------
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(ggtext)

# Age -------------------------------------------------------------------
indices <- read.csv("results/abundance_age.csv")

indices %>%
  mutate(Abundance = Abundance * 100) %>%
  mutate(Age = factor(Age, levels = c("Modern", "MIS5e"))) %>%
  ggplot(., aes(x = Genus, y = Abundance, fill = Genus, label = round(Abundance, 2))) +
  geom_col(colour = "black") +
  geom_text(vjust = 1.25, size = 4) +
  ylab("Value") +
  facet_wrap(~Age, nrow = 2, strip.position = "right") + 
  theme_bw() +
  theme(legend.position = "none")
ggsave("figures/abundance_age.png", 
       height = 100, width = 150, units = "mm", dpi = 300)


# Zone ------------------------------------------------------------------


# Transect --------------------------------------------------------------




# Load data -------------------------------------------------------------
abundance <- read_csv("results/abundance.csv")
coverage <- read_csv("results/coverage.csv")

# Update names
coverage$ReefZone[which(coverage$Age == "MIS5e" & 
                          coverage$ReefZone == "Reef edge")] <- "Shallower reef horizon"
coverage$ReefZone[which(coverage$Age == "MIS5e" & 
                          coverage$ReefZone == "Shallow reef slope")] <- "Deeper reef horizon"

# Convert to percentages
abundance$Abundance <- abundance$Abundance * 100

# Join age/reefzone data
abundance <- left_join(x = abundance, y = coverage, by = "LT")

# Set factor levels
abundance$Age <- factor(x = abundance$Age, levels = c("Modern", "MIS5e"))
abundance$ReefZone <- factor(abundance$ReefZone, levels = c("Reef edge", 
                                                            "Shallow reef slope",
                                                            "Deeper reef slope",
                                                            "Shallower reef horizon",
                                                            "Deeper reef horizon"))

# Labels
labs <- c("Reef edge" = "Reef edge",
          "Shallow reef slope" = "Shallow reef slope",
          "Deeper reef slope" = "Deeper reef slope",
          "Shallower reef horizon" = "Shallower reef horizon",
          "Deeper reef horizon" = "Deeper reef horizon",
          "Modern" = "Modern",
          "MIS5e" = "MIS5e (Last Interglacial)")


# Summarise -------------------------------------------------------------
# Median and IQRs for Age and Reef Zone
abundance <- abundance %>%
  group_by(Age, ReefZone, Genus) %>%
  summarise(Median = median(Abundance),
            LIQR = quantile(x = Abundance, probs = 0.25),
            UIQR = quantile(x = Abundance, probs = 0.75))

# Top 10
# abundance <- abundance %>%
#   group_by(Age, ReefZone) %>%
#   mutate(rank = dense_rank(desc(Median))) %>%
#   filter(rank <= 10)
# Top 10 genera (note rank needs to be updated to capture top ten)
genera <- abundance %>%
  ungroup() %>%
  mutate(rank = dense_rank(desc(Median))) %>%
  filter(rank <= 23) %>%
  .$Genus %>%
  unique()

abundance <- abundance %>%
  filter(Genus %in% genera)

# Generate plots --------------------------------------------------------
p <- ggplot(data = abundance, aes(x = Genus, y = Median, colour = Genus, 
                                  fill = Genus)) +
  geom_col(colour = "black") +
  geom_point(size = 0.5, colour = "black") +
  geom_text(aes(x = Genus, 
                y = UIQR + 2.5, 
                label = paste0(round(Median, 2), "%"),),
            size = 2.75, angle = 90, vjust = 0.5, hjust = 0) +
  geom_errorbar(aes(x = Genus, ymin = LIQR, ymax = UIQR), colour = "black") +
  scale_y_continuous(limits = c(0, 100)) +
  ylab("Abundance (%)") +
  xlab ("Genus") +
  facet_wrap(Age~ReefZone, ncol = 1,
             strip.position = "right", labeller = as_labeller(labs)) +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.margin = margin(10, 5, 5, 5, unit = "mm"),
        axis.text.x = element_text(face = c("italic"), size = 10,
                                   angle = 90, vjust = 0.5, hjust = 1))

build <- ggplot_build(p)
default_colours <- unique(build$data[[1]][order(build$data[[1]]$x), ]$fill)

p + theme(axis.text.x = element_text(colour = default_colours))

# Arrange and save ------------------------------------------------------
ggsave("figures/community_composition.png", dpi = 600,
       width = 210, height = 297, units = "mm", scale = 1)

