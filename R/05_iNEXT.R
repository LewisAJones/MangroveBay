# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: 05_iNEXT.R
# Last updated: 2026-04-17
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay
# Libraries -------------------------------------------------------------
# install.packages("tidyverse")
# install.packages("iNEXT")
library(tidyverse)
library(iNEXT)

# Load data -------------------------------------------------------------
corals <- read_csv("data/Mangrove_Bay_corals.csv")

# Wrangle data ----------------------------------------------------------
# Create input
input <- corals %>%
  mutate(LT = str_replace(string = LT, pattern = "_", replacement = "")) %>%
  filter(Rank %in% c("Species", "Genus")) %>%
  mutate(Bins = floor((`End-Start (Intercept)`) / 1)) %>%
  group_by(LT, Genus) %>%
  summarise(Count = sum(Bins), .groups = "drop") %>%
  pivot_wider(names_from = LT, values_from = Count, values_fill = 0) %>%
  column_to_rownames(var = "Genus")

# Diversity analyses ----------------------------------------------------
# Get minimum length of coral transect
level <- min(corals$TransectCoralLength)
# Get asymptote
output <- iNEXT(x = input, 
                q = 0, 
                datatype = "abundance", 
                knots = 100,
                nboot = 100)
# Get estimates
estimates <- estimateD(x = input,
                       q = 0,
                       datatype = "abundance",
                       level = level,
                       base = "size",
                       nboot = 100)

# Save data -------------------------------------------------------------
saveRDS(output, "results/rarefied_diversity_asymptote.RDS")
saveRDS(estimates, "results/rarefied_diversity_estimates.RDS")
