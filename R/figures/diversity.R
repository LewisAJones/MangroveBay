# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: diversity.R
# Last updated: 2026-03-10
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Libraries -------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(scales)
library(stringr)
library(forcats)

# Transect -------------------------------------------------------------
indices <- read.csv("results/diversity_indices_transect.csv")

indices %>%
  filter_out(Name == "Shannon") %>%
  mutate(Age = factor(Age, levels = c("Modern", "MIS5e")),
         ReefZone = str_replace(ReefZone, pattern = "Reef edge", replacement = "Reef Edge"),
         ReefZone = str_replace(ReefZone, pattern = "Shallow reef slope", replacement = "Shallow Reef Slope"),
         ReefZone = str_replace(ReefZone, pattern = "Deeper reef slope", replacement = "Deeper Reef Slope"),
         ReefZone = factor(ReefZone, levels = c("Reef Edge", "Shallow Reef Slope", "Deeper Reef Slope")),
         LT = str_replace(LT, pattern = "_", replacement = "")) %>%
  ggplot(., aes(x = LT, y = Median, fill = Age, label = round(Median, 2))) +
  geom_col(colour = "black") +
  geom_text(vjust = 1.5, size = 4.5) +
  ylab("Value") +
  xlab("Transect") +
  facet_grid(Name~ReefZone, scales = "free", space = "free_x") + 
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        strip.background = element_blank())
ggsave("figures/diversity/raw_diversity_transect.png", 
       height = 200, width = 350, units = "mm", dpi = 300)

# Habitat ---------------------------------------------------------------
indices <- read.csv("results/diversity_indices_zone.csv")

indices %>%
  filter_out(Name == "Shannon") %>%
  mutate(Age = factor(Age, levels = c("Modern", "MIS5e")),
         ReefZone = str_replace(ReefZone, pattern = "Reef edge", replacement = "Reef Edge"),
         ReefZone = str_replace(ReefZone, pattern = "Shallow reef slope", replacement = "Shallow Reef Slope"),
         ReefZone = str_replace(ReefZone, pattern = "Deeper reef slope", replacement = "Deeper Reef Slope"),
         ReefZone = factor(ReefZone, levels = c("Reef Edge", "Shallow Reef Slope", "Deeper Reef Slope"))) %>%
  ggplot(., aes(x = Age, y = Median, fill = Age, label = round(Median, 2))) +
  geom_col(colour = "black") +
  geom_errorbar(aes(x = Age, ymin = LQR, ymax = UQR), linewidth = 0.5) +
  geom_point(colour = "black", shape = 23) +
  geom_text(aes(y = 0), hjust = 0.5, vjust = -1, size = 4.5) +
  ylab("Value") +
  facet_grid(Name~ReefZone, scales = "free", space = "free_x") + 
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        strip.background = element_blank())
ggsave("figures/diversity/raw_diversity_reef_zone.png", 
       height = 200, width = 250, units = "mm", dpi = 300)

# Age -----------------------------------------------------------------
indices <- read.csv("results/diversity_indices_age.csv")

indices %>%
  filter_out(Name == "Shannon") %>%
  mutate(Age = factor(Age, levels = c("Modern", "MIS5e"))) %>%
  ggplot(., aes(x = Age, y = Median, fill = Age, label = round(Median, 2))) +
    geom_col(colour = "black") +
    geom_errorbar(aes(x = Age, ymin = LQR, ymax = UQR), linewidth = 0.5) +
    geom_point(colour = "black", shape = 23) +
    geom_text(aes(y = 0), hjust = 0.5, vjust = -1, size = 4.5) +
    ylab("Value") +
    facet_wrap(.~Name, scales = "free_y") + 
    theme_bw() +
    theme(legend.position = "none",
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          strip.text = element_text(size = 12),
          strip.background = element_blank())
ggsave("figures/diversity/raw_diversity_age.png", 
       height = 150, width = 200, units = "mm", dpi = 300)
  

