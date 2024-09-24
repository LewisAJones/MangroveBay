# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: nmds.R
# Last updated: 2024-08-11
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Load libraries --------------------------------------------------------
library(ggplot2)
library(ggrepel)
library(ggforce)

# Load data -------------------------------------------------------------
nmds <- readRDS("./results/NMDS_plot_data.RDS")
stress <- round(readRDS(file = "./results/nmds.RDS")$stress, 3)
stress <- paste0("Stress = ", stress)

nmds$Age <- factor(x = nmds$Age, levels = c("Modern", "MIS5e"))
nmds$ReefZone <- factor(nmds$ReefZone, levels = c("Reef edge", 
                                                        "Reef slope",
                                                        "Shallow reef slope",
                                                        "Deeper reef slope"))

# Plot data -------------------------------------------------------------
ggplot(data = nmds, aes(x = NMDS1, y = NMDS2, shape = Age, 
                        colour = ReefZone, fill = ReefZone)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "black") +
  geom_vline(xintercept = 0, linetype = 2, colour = "black") +
  geom_mark_hull(concavity = 10, expand = 0, radius = 0, aes(fill = ReefZone, shape = NULL)) +
  geom_point(size = 3, alpha = 0.75) +
  geom_label(data = NULL, aes(x = -Inf, y = Inf, label = stress),
             size = 3.5, colour = "black", fill = "white",
             hjust = 0, vjust = 1) +
  geom_text_repel(aes(label = LT), colour = "black",
                  size = 2.5, min.segment.length = unit(0, 'cm'),
                  box.padding = 0.5, max.overlaps = 100) +
  #scale_colour_manual(values = c("Modern" = "#31a354", "MIS5e" = "#225ea8")) +
  scale_shape_manual(values = c("Modern" = 17, "MIS5e" = 19)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key.size = unit(5, "mm"),
    legend.key = element_rect(fill = NA)
  )

# Save plot -------------------------------------------------------------
ggsave("./figures/nmds.png",
       dpi = 300, width = 180, height = 180, units = "mm")

