# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: growth_form.R
# Last updated: 2026-03-25
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Load packages ---------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(tidytext)

# Age -------------------------------------------------------------------
# Load data
growth <- read.csv("results/growth_transect.csv")
# Group by age
# Summarise abundances
growth <- growth %>%
  mutate(Abundance = Abundance * 100) %>%
  mutate(Age = factor(Age, levels = c("Modern", "MIS5e"))) %>%
  group_by(Age, Category) %>%
  summarise(Median = median(Abundance),
            LQR = quantile(Abundance, 0.25),
            UQR = quantile(Abundance, 0.75))
# Plot data
ggplot(growth, aes(x = reorder_within(Category, -Median, Age), 
                   y = Median, fill = Category, label = round(Median, 2))) +
  geom_col(colour = "black") +
  geom_errorbar(aes(ymin = LQR, ymax = UQR), colour = "black") +
  geom_point(shape = 23, colour = "black") +
  geom_text(aes(y = UQR + 2.5, 
                label = paste0(round(Median, 2), "%"),),
            size = 3, angle = 0, vjust = 1, hjust = 0.5) +
  scale_x_reordered() +
  ylab("Dominance (%)") +
  xlab ("Growth Form") +
  facet_wrap(~Age, nrow = 1, ncol = 2, strip.position = "top", scales = "free_x") + 
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.margin = margin(10, 5, 5, 5, unit = "mm"),
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 14),
        strip.background = element_blank())
# Save plot
ggsave("figures/growth_form/age.png", 
       height = 210, width = 297, units = "mm", dpi = 300, scale = 0.8)

# Zone ------------------------------------------------------------------
# Load data
growth <- read.csv("results/growth_transect.csv")
# Group by age
# Summarise abundances
growth <- growth %>%
  mutate(Abundance = Abundance * 100) %>%
  mutate(Age = factor(Age, levels = c("Modern", "MIS5e")),
         ReefZone = str_replace(ReefZone, pattern = "Reef edge", replacement = "Reef Edge"),
         ReefZone = str_replace(ReefZone, pattern = "Shallow reef slope", replacement = "Shallow Reef Slope"),
         ReefZone = str_replace(ReefZone, pattern = "Deeper reef slope", replacement = "Deeper Reef Slope"),
         ReefZone = factor(ReefZone, levels = c("Reef Edge", "Shallow Reef Slope", "Deeper Reef Slope"))) %>%
  group_by(Age, ReefZone, Category) %>%
  summarise(Median = median(Abundance),
            LQR = quantile(Abundance, 0.25),
            UQR = quantile(Abundance, 0.75))
# Plot data
ggplot(growth, aes(x = reorder_within(Category, -Median, list(Age, ReefZone)), 
                   y = Median, fill = Category, label = round(Median, 2))) +
  geom_col(colour = "black") +
  geom_point(size = 0.5, colour = "black") +
  geom_errorbar(aes(ymin = LQR, ymax = UQR), colour = "black") +
  geom_text(aes(y = UQR + 2.5, 
                label = paste0(round(Median, 2), "%"),),
            size = 3.25, angle = 90, vjust = 0.5, hjust = 0) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_reordered() +
  ylab("Dominance (%)") +
  xlab ("Growth Form") +
  facet_wrap(Age~ReefZone, ncol = 3, scales = "free_x") + 
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.margin = margin(10, 5, 5, 5, unit = "mm"),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 1),
        strip.text = element_text(size = 12),
        strip.background = element_blank())
# Save plot
ggsave("figures/growth_form/zone.png", 
       height = 210, width = 297, units = "mm", dpi = 300)

# Transect --------------------------------------------------------------
# Load data
growth <- read.csv("results/growth_transect.csv")
# Group by age
# Summarise abundances
growth <- growth %>%
  mutate(Abundance = Abundance * 100) %>%
  mutate(Age = factor(Age, levels = c("Modern", "MIS5e")),
         ReefZone = str_replace(ReefZone, pattern = "Reef edge", replacement = "Reef Edge"),
         ReefZone = str_replace(ReefZone, pattern = "Shallow reef slope", replacement = "Shallow Reef Slope"),
         ReefZone = str_replace(ReefZone, pattern = "Deeper reef slope", replacement = "Deeper Reef Slope"),
         ReefZone = factor(ReefZone, levels = c("Reef Edge", "Shallow Reef Slope", "Deeper Reef Slope")),
         LT = str_replace(LT, pattern = "_", replacement = "")) %>%
  group_by(LT, Age, ReefZone, Category) %>%
  summarise(Median = median(Abundance),
            LQR = quantile(Abundance, 0.25),
            UQR = quantile(Abundance, 0.75))
# Plot data
ggplot(growth, aes(x = reorder_within(Category, -Median, list(LT, Age, ReefZone)), 
                   y = Median, fill = Category, label = round(Median, 2))) +
  geom_col(colour = "black") +
  geom_point(size = 0.5, colour = "black") +
  geom_errorbar(aes(ymin = LQR, ymax = UQR), colour = "black") +
  geom_text(aes(y = UQR + 2.5, 
                label = paste0(round(Median, 2), "%"),),
            size = 2.5, angle = 90, vjust = 0.5, hjust = 0) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_reordered() +
  ylab("Dominance (%)") +
  xlab ("Growth Form") +
  facet_wrap(LT~Age+ReefZone, scales = "free_x") + 
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.margin = margin(10, 5, 5, 5, unit = "mm"),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 1),
        strip.text = element_text(size = 12),
        strip.background = element_blank())
# Save plot
ggsave("figures/growth_form/transect.png", 
       height = 297, width = 210, units = "mm", dpi = 300, scale = 2)
