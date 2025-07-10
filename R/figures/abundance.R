# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: abundance.R
# Last updated: 2024-09-24
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Load packages ---------------------------------------------------------
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(ggtext)

# Load data -------------------------------------------------------------
abundance <- read_csv("./results/abundance.csv")
coverage <- read_csv("./results/coverage.csv")

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

# Calculate top five ----------------------------------------------------
# Summarise via age and reef zone
# abundance <- abundance %>%
#   group_by(Age, ReefZone) %>%
#   mutate(Threshold = Median >= 10)
# # Add index
# abundance$Genus[which(abundance$Threshold == FALSE)] <- "Other"

# abundance <- abundance %>%
#   # Add transect proportion
#   group_by(Age, ReefZone, Genus) %>%
#   # Add transect proportion
#   summarise(Abundance = sum(Abundance)) %>%
#   as.data.frame()

# Set factor levels
# abundance$Genus <- factor(abundance$Genus, 
#                           levels = c("Acropora", "Echinopora", "Galaxea",
#                                    "Goniastrea", "Lobophyllia", "Millepora",
#                                    "Montipora",
#                                    "Pocillopora", "Porites", "Other"))


# Split datasets --------------------------------------------------------
modern <- subset(abundance, Age == "Modern")
MIS5e <- subset(abundance, Age == "MIS5e")

# Generate plots --------------------------------------------------------
p1 <- ggplot(data = modern, aes(x = Genus, y = Median, fill = Genus)) +
  geom_col(colour = "black") +
  geom_text(aes(x = Genus, 
                y = UIQR + 0.5, 
                label = paste0(round(Median, 2), "%")),
            colour = "black", size = 2.25, 
            angle = 90, vjust = 0.5, hjust = 0) +
  geom_errorbar(aes(x = Genus, ymin = LIQR, ymax = UIQR)) +
  scale_y_continuous(limits = c(0, 85)) +
  ylab("Abundance (%)") +
  xlab ("Genus") +
  facet_grid(Age~ReefZone, labeller = as_labeller(labs)) +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.margin = margin(10, 5, 5, 5, unit = "mm"),
        axis.text.x = element_text(face = c("italic"), size = 8,
                                   angle = 90, vjust = 0.5, hjust = 1)) +
  guides(fill = guide_legend(byrow = TRUE))

p2 <- ggplot(data = MIS5e, aes(x = Genus, y = Median, fill = Genus)) +
  geom_col(colour = "black") +
  geom_text(aes(x = Genus, 
                y = UIQR + 0.5, 
                label = paste0(round(Median, 2), "%")),
            colour = "black", size = 2.25, 
            angle = 90, vjust = 0.5, hjust = 0) +
  geom_errorbar(aes(x = Genus, ymin = LIQR, ymax = UIQR)) +
  scale_y_continuous(limits = c(0, 65)) +
  ylab("Abundance (%)") +
  xlab ("Genus") +
  facet_grid(Age~ReefZone, labeller = as_labeller(labs)) +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.margin = margin(10, 5, 5, 5, unit = "mm"),
        axis.text.x = element_text(face = c("italic"), size = 8,
                                   angle = 90, vjust = 0.5, hjust = 1)) +
  guides(fill = guide_legend(byrow = TRUE))

ggarrange(p1, p2, nrow = 2, labels = "AUTO")

# Arrange and save ------------------------------------------------------
ggsave("figures/community_composition.png", dpi = 600,
       width = 275, height = 300, units = "mm", scale = 1)

