# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: abundance.R
# Last updated: 2026-04-15
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Load packages ---------------------------------------------------------
library(ggplot2)
library(ggtext)
library(tidyverse)
library(tidytext)

# Age -------------------------------------------------------------------
# Load data
abundance <- read.csv("results/abundance_transect.csv")
# Group by age
# Summarise abundances
abundance <- abundance %>%
  mutate(Abundance = Abundance * 100) %>%
  mutate(Age = factor(Age, levels = c("Modern", "MIS5e"))) %>%
  group_by(Age, Genus) %>%
  summarise(Median = median(Abundance),
            LQR = quantile(Abundance, 0.25),
            UQR = quantile(Abundance, 0.75))
# Plot data
ggplot(abundance, aes(x = reorder_within(Genus, -Median, Age), 
                      y = Median, fill = Genus, label = round(Median, 2))) +
  geom_col(colour = "black") +
  geom_errorbar(aes(ymin = LQR, ymax = UQR), colour = "black") +
  geom_point(colour = "black", shape = 23) +
  geom_text(aes(y = UQR + 2.5, 
                label = paste0(round(Median, 2), "%"),),
            size = 3.25, angle = 90, vjust = 0.5, hjust = 0) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_reordered() +
  ylab("Abundance (%)") +
  xlab ("Genus") +
  facet_wrap(~Age, nrow = 2, ncol = 1, strip.position = "right", scales = "free_x") + 
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.margin = margin(10, 5, 5, 5, unit = "mm"),
        axis.text.x = element_text(face = c("italic"), size = 10,
                                   angle = 90, vjust = 0.5, hjust = 1),
        strip.background = element_blank())
# Save plot
ggsave("figures/abundance/age.png", 
       height = 297, width = 210, units = "mm", dpi = 300, scale = 0.8)

# Identify top ten in each age
genera <- abundance %>%
  group_by(Age) %>%
  mutate(rank = dense_rank(desc(Median))) %>%
  filter(rank <= 10) %>%
  select(Age, Genus) %>% 
  ungroup() %>%
  distinct()
# Filter to top ten in each age
modern <- abundance %>%
  filter(Age == "Modern") %>%
  filter(Genus %in% filter(genera, Age == "Modern")$Genus)
mis5e <- abundance %>%
  filter(Age == "MIS5e") %>%
  filter(Genus %in% filter(genera, Age == "MIS5e")$Genus)
abundance <- bind_rows(modern, mis5e)

# Plot data
ggplot(abundance, aes(x = reorder_within(Genus, -Median, Age), 
                      y = Median, fill = Genus, label = round(Median, 2))) +
  geom_col(colour = "black") +
  geom_errorbar(aes(ymin = LQR, ymax = UQR), colour = "black") +
  geom_point(colour = "black", shape = 23) +
  geom_richtext(aes(
                y = UQR + 2.5, 
                label = paste0(round(Median, 2), "%")),
                fill = NA, label.color = NA, size = 4, angle = 0, vjust = 0.3, hjust = 0.5) +
  scale_x_reordered() +
  ylab("Abundance (%)") +
  xlab ("Genus") +
  facet_wrap(~Age, nrow = 1, ncol = 2, strip.position = "top", scales = "free_x") + 
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.margin = margin(10, 5, 5, 5, unit = "mm"),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(face = c("italic"), size = 14,
                                   angle = 90, vjust = 0.5, hjust = 1),
        strip.background = element_blank())
# Save plot
ggsave("figures/abundance/age_top10.png", 
       height = 210, width = 297, units = "mm", dpi = 300)

# Zone ------------------------------------------------------------------
# Load data
abundance <- read.csv("results/abundance_transect.csv")
# Group by age and reef zone
# Summarise abundances
abundance <- abundance %>%
  mutate(Abundance = Abundance * 100) %>%
  mutate(Age = factor(Age, levels = c("Modern", "MIS5e")),
         ReefZone = str_replace(ReefZone, pattern = "Reef edge", replacement = "Reef Edge"),
         ReefZone = str_replace(ReefZone, pattern = "Shallow reef slope", replacement = "Shallow Reef Slope"),
         ReefZone = str_replace(ReefZone, pattern = "Deeper reef slope", replacement = "Deeper Reef Slope"),
         ReefZone = if_else(Age == "MIS5e" & ReefZone == "Reef Edge", "Shallower Reef Horizon", ReefZone),
         ReefZone = if_else(Age == "MIS5e" & ReefZone == "Shallow Reef Slope", "Deeper Reef Horizon", ReefZone),
         ReefZone = factor(ReefZone, levels = c("Reef Edge", "Shallower Reef Horizon", "Shallow Reef Slope", "Deeper Reef Horizon", "Deeper Reef Slope"))) %>%
  group_by(Age, ReefZone, Genus) %>%
  summarise(Median = median(Abundance),
            LQR = quantile(Abundance, 0.25),
            UQR = quantile(Abundance, 0.75))
# Plot data
ggplot(abundance, aes(x = reorder_within(Genus, -Median, list(Age, ReefZone)), y = Median, fill = Genus, label = round(Median, 2))) +
  geom_col(colour = "black") +
  geom_errorbar(aes(ymin = LQR, ymax = UQR), colour = "black") +
  geom_point(colour = "black", shape = 23) +
  geom_text(aes(y = UQR + 2.5, 
                label = paste0(round(Median, 2), "%"),),
            size = 3.25, angle = 90, vjust = 0.5, hjust = 0) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_reordered() +
  ylab("Abundance (%)") +
  xlab ("Genus") +
  facet_wrap(Age~ReefZone, scales = "free_x") + 
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.margin = margin(10, 5, 5, 5, unit = "mm"),
        axis.text.x = element_text(face = c("italic"), size = 10,
                                   angle = 90, vjust = 0.5, hjust = 1),
        strip.background = element_blank())
# Save plot
ggsave("figures/abundance/zone.png", 
       height = 210, width = 297, units = "mm", dpi = 300, scale = 1.35)

# Identify top ten in each age
genera <- abundance %>%
  group_by(Age, ReefZone) %>%
  mutate(rank = dense_rank(desc(Median))) %>%
  filter(rank <= 10) %>%
  select(Age, ReefZone, Genus) %>% 
  ungroup() %>%
  distinct()
# Filter to top ten in each age
modern_edge <- abundance %>%
  filter(Age == "Modern" & ReefZone == "Reef Edge") %>%
  filter(Genus %in% filter(genera, Age == "Modern" & ReefZone == "Reef Edge")$Genus)
modern_slope_shallow <- abundance %>%
  filter(Age == "Modern" & ReefZone == "Shallow Reef Slope") %>%
  filter(Genus %in% filter(genera, Age == "Modern" & ReefZone == "Shallow Reef Slope")$Genus)
modern_slope_deep <- abundance %>%
  filter(Age == "Modern" & ReefZone == "Deeper Reef Slope") %>%
  filter(Genus %in% filter(genera, Age == "Modern" & ReefZone == "Deeper Reef Slope")$Genus)
mis5e_shallow <- abundance %>%
  filter(Age == "MIS5e" & ReefZone == "Shallower Reef Horizon") %>%
  filter(Genus %in% filter(genera, Age == "MIS5e" & ReefZone == "Shallower Reef Horizon")$Genus)
mis5e_deep <- abundance %>%
  filter(Age == "MIS5e" & ReefZone == "Deeper Reef Horizon") %>%
  filter(Genus %in% filter(genera, Age == "MIS5e" & ReefZone == "Deeper Reef Horizon")$Genus)
abundance <- bind_rows(modern_edge, modern_slope_shallow, modern_slope_deep, mis5e_shallow, mis5e_deep)

# Plot data
ggplot(abundance, aes(x = reorder_within(Genus, -Median, list(Age, ReefZone)), y = Median, fill = Genus, label = round(Median, 2))) +
  geom_col(colour = "black") +
  geom_errorbar(aes(ymin = LQR, ymax = UQR), colour = "black") +
  geom_point(colour = "black", shape = 23) +
  geom_text(aes(y = UQR + 2.5, 
                label = paste0(round(Median, 2), "%"),),
            size = 3.25, angle = 0, vjust = 0, hjust = 0.5) +
  scale_x_reordered() +
  ylab("Abundance (%)") +
  xlab ("Genus") +
  facet_wrap(Age~ReefZone, ncol = 3, scales = "free_x") + 
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.margin = margin(10, 5, 5, 5, unit = "mm"),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(face = c("italic"), size = 14,
                                   angle = 90, vjust = 0.5, hjust = 1),
        strip.background = element_blank())
# Save plot
ggsave("figures/abundance/zone_top10.png", 
       height = 210, width = 297, units = "mm", dpi = 300, scale = 1.2)

# Transect --------------------------------------------------------------
# Load data
abundance <- read.csv("results/abundance_transect.csv")
# Group by age and reef zone
# Summarise abundances
abundance <- abundance %>%
  mutate(Abundance = Abundance * 100) %>%
  mutate(Age = factor(Age, levels = c("Modern", "MIS5e")),
         ReefZone = str_replace(ReefZone, pattern = "Reef edge", replacement = "Reef Edge"),
         ReefZone = str_replace(ReefZone, pattern = "Shallow reef slope", replacement = "Shallow Reef Slope"),
         ReefZone = str_replace(ReefZone, pattern = "Deeper reef slope", replacement = "Deeper Reef Slope"),
         ReefZone = factor(ReefZone, levels = c("Reef Edge", "Shallow Reef Slope", "Deeper Reef Slope")),
         LT = str_replace(LT, pattern = "_", replacement = "")) %>%
  group_by(LT, Age, ReefZone, Genus) %>%
  summarise(Median = median(Abundance),
            LQR = quantile(Abundance, 0.25),
            UQR = quantile(Abundance, 0.75))
# Plot data
ggplot(abundance, aes(x = reorder_within(Genus, -Median, list(LT, Age, ReefZone)), 
                                         y = Median, fill = Genus, label = round(Median, 2))) +
  geom_col(colour = "black") +
  geom_errorbar(aes(ymin = LQR, ymax = UQR), colour = "black") +
  geom_point(colour = "black", shape = 23) +
  geom_text(aes(y = UQR + 2.5, 
                label = paste0(round(Median, 2), "%"),),
            size = 2.75, angle = 90, vjust = 0.5, hjust = 0) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_reordered() +
  ylab("Abundance (%)") +
  xlab ("Genus") +
  facet_wrap(LT~Age+ReefZone, scales = "free") + 
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.margin = margin(10, 5, 5, 5, unit = "mm"),
        axis.text.x = element_text(face = c("italic"), size = 10,
                                   angle = 90, vjust = 0.5, hjust = 1),
        strip.background = element_blank())
# Save plot
ggsave("figures/abundance/transect.png", 
       height = 297, width = 210, units = "mm", dpi = 300, scale = 2)
