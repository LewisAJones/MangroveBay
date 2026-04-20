# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: iNEXT.R
# Last updated: 2026-04-20
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Libraries -------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(scales)
library(stringr)
library(forcats)

# Coverage-based rarefaction --------------------------------------------
asymptote <- readRDS("results/rarefied_diversity_asymptote.RDS")

asymptote$iNextEst$size_based %>%
  mutate(Age = "Modern") %>%
  mutate(Age = if_else(str_detect(Assemblage, pattern = "SB"), "MIS5e", Age)) %>%
  rename(LT = "Assemblage") %>%
  ggplot(., aes(x = m, y = qD, ymin = qD.LCL, ymax = qD.UCL, 
                colour = fct_rev(Age), fill = fct_rev(Age), linetype = fct_rev(Method))) +
  geom_ribbon(alpha = 0.5, colour = NA, linetype = 1) +
  geom_line(linewidth = 0.5) +
  geom_point(data = . %>% filter(Method == "Observed"), 
             shape = 23, size = 2, colour = "black") +
  xlab("Number of Observations (cm)") +
  ylab("Genus Richness") +
  facet_wrap(~LT) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 10),
        strip.background = element_blank())

ggsave("figures/diversity/asymptote_transect.png", 
       height = 210, width = 297, units = "mm", dpi = 300)

estimates <- readRDS("results/rarefied_diversity_estimates.RDS")

estimates %>%
  mutate(Age = "Modern") %>%
  mutate(Age = if_else(str_detect(Assemblage, pattern = "SB"), "MIS5e", Age)) %>%
  mutate(Method = "Rarefaction") %>%
  rename(LT = "Assemblage") %>%
  ggplot(., aes(x = LT, y = qD, ymin = qD.LCL, ymax = qD.UCL,
                fill = fct_rev(Age),
                label = round(qD, 2))) +
  geom_col(colour = "black") +
  geom_errorbar(linewidth = 0.5, colour = "black") +
  geom_point(size = 3, shape = 23, colour = "black") +
  geom_text(aes(y = 0), hjust = 0.5, vjust = -1, size = 3.5, colour = "black") +
  xlab("Transect") +
  ylab("Genus Richness (Size-Based Rarefaction)") +
  facet_wrap(~fct_rev(Age), scales = "free_x", space = "free_x") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        strip.background = element_blank())

ggsave("figures/diversity/rarefied_diversity_transect.png", 
       height = 210, width = 297, units = "mm", dpi = 300)

# Average across transects for age
estimates %>%
  mutate(Age = "Modern") %>%
  mutate(Age = if_else(str_detect(Assemblage, pattern = "SB"), "MIS5e", Age)) %>%
  rename(LT = "Assemblage") %>%
  group_by(Age) %>%
  summarise(Median = median(qD),
            LCL = min(qD.LCL),
            UCL = max(qD.UCL)) %>%
  ggplot(., aes(x = fct_rev(Age), y = Median, ymin = LCL, ymax = UCL,
                fill = fct_rev(Age), label = round(Median, 2))) +
  geom_col(colour = "black") +
  geom_errorbar(linewidth = 0.75, colour = "black") +
  geom_point(shape = 23, size = 3, colour = "black") +
  geom_text(aes(y = 0), hjust = 0.5, vjust = -1, size = 5.5, colour = "black") +
  xlab("Age") +
  ylab("Genus Richness (Size-Based Rarefaction)") +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        strip.background = element_blank())

ggsave("figures/diversity/rarefied_diversity_transect_by_age.png", 
       height = 210, width = 297, units = "mm", dpi = 300, scale = 0.8)
