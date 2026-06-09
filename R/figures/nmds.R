# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: nmds.R
# Last updated: 2025-07-11
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Load libraries --------------------------------------------------------
library(ggplot2)
library(ggrepel)
library(ggforce)
library(dplyr)

# Load data -------------------------------------------------------------
nmds <- readRDS("results/NMDS_plot_data.RDS") %>%
  mutate(LT = str_replace_all(string = LT, pattern = "_", replacement = ""))
stress <- round(readRDS(file = "results/nmds.RDS")$stress, 3)
stress <- paste0("Stress = ", stress)

nmds$ReefZone[which(nmds$Age == "MIS5e" & 
                      nmds$ReefZone == "Reef edge")] <- "Shallower reef horizon"
nmds$ReefZone[which(nmds$Age == "MIS5e" & 
                      nmds$ReefZone == "Shallow reef slope")] <- "Deeper reef horizon"

nmds$Age <- factor(x = nmds$Age, levels = c("Modern", "MIS5e"))
nmds$ReefZone <- factor(nmds$ReefZone, levels = c("Reef edge", 
                                                  "Shallow reef slope",
                                                  "Deeper reef slope",
                                                  "Shallower reef horizon",
                                                  "Deeper reef horizon"))

# Plot data -------------------------------------------------------------
ggplot(data = nmds, aes(x = NMDS1, y = NMDS2, shape = ReefZone, 
                        colour = Age, fill = Age)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "black") +
  geom_vline(xintercept = 0, linetype = 2, colour = "black") +
  geom_mark_hull(concavity = 10, expand = 0, radius = 0, 
                 aes(fill = Age, shape = NULL)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_label(data = NULL, aes(x = Inf, y = Inf, label = stress),
             size = 3.5, colour = "black", fill = "white",
             hjust = 1.1, vjust = 1.25) +
  geom_text_repel(aes(label = LT), colour = "black",
                  size = 2.5, min.segment.length = unit(0, 'cm'),
                  box.padding = 0.5, max.overlaps = 100) +
  scale_shape_manual(labels = c("Modern" = "Modern", 
                                "MIS5e" = "MIS5e (Last Interglacial)"),
                     values = c("Reef edge" = 21, 
                                "Shallow reef slope" = 22, 
                                "Deeper reef slope" = 23,
                                "Shallower reef horizon" = 24,
                                "Deeper reef horizon" = 25)) +
  # Change label names
  scale_fill_discrete(labels = c("Modern" = "Modern", 
                                 "MIS5e" = "MIS5e (Last Interglacial)")) +
  scale_colour_discrete(labels = c("Modern" = "Modern", 
                                   "MIS5e" = "MIS5e (Last Interglacial)")) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key.size = unit(5, "mm"),
    legend.box = "vertical", 
    legend.margin = margin(),
    legend.key = element_rect(fill = NA)
  )

# Save plot -------------------------------------------------------------
ggsave("figures/nmds/nmds.png",
       dpi = 300, width = 180, height = 180, units = "mm")

