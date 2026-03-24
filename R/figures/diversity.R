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
library(MetBrewer)
library(stringr)

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
  scale_fill_met_d("Hiroshige") +
  ylab("Value") +
  xlab("Transect") +
  facet_grid(Name~ReefZone, scales = "free") + 
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("figures/diversity_transect.png", 
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
  geom_errorbar(aes(x = Age, ymin = LQR, ymax = UQR), width = 0.5) +
  scale_fill_met_d("Hiroshige") +
  ylab("Value") +
  facet_grid(Name~ReefZone, scales = "free") + 
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("figures/diversity_zone.png", 
       height = 200, width = 250, units = "mm", dpi = 300)


# Age -----------------------------------------------------------------
indices <- read.csv("results/diversity_indices_age.csv")

indices %>%
  filter_out(Name == "Shannon") %>%
  mutate(Age = factor(Age, levels = c("Modern", "MIS5e"))) %>%
  ggplot(., aes(x = Age, y = Median, fill = Age, label = round(Median, 2))) +
    geom_col(colour = "black") +
    geom_errorbar(aes(x = Age, ymin = LQR, ymax = UQR), width = 0.5) +
    scale_fill_met_d("Hiroshige") +
    ylab("Value") +
    facet_wrap(.~Name, scales = "free_y") + 
    theme_bw() +
    theme(legend.position = "none")
ggsave("figures/diversity_age.png", 
       height = 150, width = 200, units = "mm", dpi = 300)