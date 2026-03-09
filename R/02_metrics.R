# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: 02_metrics.R
# Last updated: 2026-03-09
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Libraries -------------------------------------------------------------
# install.packages("tidyverse")
# install.packages("vegan")
# install.packages("labdsv")
# install.packages("beepr")
library(tidyverse)
library(vegan)
library(labdsv)
library(beepr)

# Load data -------------------------------------------------------------
corals <- read_csv("data/Mangrove_Bay_corals.csv")

# Transects -------------------------------------------------------------
# Summarise per transects
transects <- corals %>%
  select(LT, Age, ReefZone, 
         NumberOfIntercepts, TransectLength, 
         NumberOfCoralIntercepts, TransectCoralLength) %>%
  group_by(LT) %>%
  mutate(ProportionCoral = TransectCoralLength / TransectLength) %>%
  distinct()
write.csv(transects, "results/transects.csv", row.names = FALSE)

# Summarise by age and reef zone
transects <- corals %>%
  group_by(Age, ReefZone) %>%
  summarise(TransectsPerZone = length(unique(LT)),
            TransectMeanLength = mean(unique(TransectLength)),
            TransectSDLength = sd(unique(TransectLength)),
            TransectMeanCoralLength = mean(unique(TransectCoralLength)),
            TransectSDCoralLength = sd(unique(TransectCoralLength)))
write.csv(transects, "results/transects_zone.csv", row.names = FALSE)

# Rank IDs --------------------------------------------------------------
# Summarise occurrence rank IDs
rank <- corals %>%
  group_by(Age, Rank) %>%
  summarise(RankID = length(ScientificName))
write.csv(rank, "results/rank_id.csv", row.names = FALSE)

# Filter data -----------------------------------------------------------
# Not all data is resolved to species-level
# We will use genus-level, exclude data resolved only to order/family
corals <- corals %>%
  filter(Rank %in% c("Species", "Genus"))

# Growth form -----------------------------------------------------------
# Calculate proportion of each growth form per transect
growth <- corals %>%
  group_by(LT, Age, ReefZone, Category) %>%
  count() %>%
  group_by(LT) %>%
  mutate(Total = sum(n),
         Abundance = n / sum(n))
write.csv(growth, "results/growth_transect.csv", row.names = FALSE)

# Calculate proportion of each growth form per age/reef zone
growth <- corals %>%
  group_by(Age, ReefZone, Category) %>%
  count() %>%
  group_by(Age, ReefZone) %>%
  mutate(Total = sum(n),
         Abundance = n / sum(n))
write.csv(growth, "results/growth_zone.csv", row.names = FALSE)

# Calculate proportion of each growth form per age
growth <- corals %>%
  group_by(Age, Category) %>%
  count() %>%
  group_by(Age) %>%
  mutate(Total = sum(n),
         Abundance = n / sum(n))
write.csv(growth, "results/growth_age.csv", row.names = FALSE)

# Abundance and diversity -----------------------------------------------
# Calculate abundance of each coral genus per transect
abundance <- corals %>%
  # Add transect proportion
  group_by(LT, Age, ReefZone) %>%
  # Add transect proportion
  mutate(TotalSamplingLength = sum(`End-Start (Intercept)`)) %>%
  mutate(SampleProportion = (`End-Start (Intercept)` / TotalSamplingLength)) %>%
  group_by(LT, Age, ReefZone, Genus) %>%
  summarise(RawAbundance = sum(`End-Start (Intercept)`),
            Abundance = sum(SampleProportion)) %>%
  group_by(LT) %>% 
  mutate(Total = sum(RawAbundance)) %>%
  as.data.frame()
write.csv(abundance, "results/abundance_transect.csv", row.names = FALSE)

# Abundance matrix per transect
abundance_mat <- abundance %>%
  select(LT, Genus, Abundance) %>%
  matrify() %>%
  as.data.frame() %>%
  mutate(LT = row.names(.)) %>%
  left_join(x = ., y = unique(corals[, c("Age", "LT", "ReefZone")]), by = "LT")
write.csv(abundance_mat, "results/abundance_matrix_transect.csv", row.names = FALSE)

# Calculate diversity metrics per transect
indices <- abundance_mat %>%
  select(-c("Age", "LT", "ReefZone")) %>%
  reframe(alpha = specnumber(.),
          shannon = diversity(x = ., index = "shannon"),
          pielou = shannon / log(alpha)) %>%
  mutate(LT = abundance_mat$LT,
         Age = abundance_mat$Age,
         ReefZone = abundance_mat$ReefZone)
write.csv(indices, "results/diversity_indices_transect.csv", row.names = FALSE)

# Calculate abundance of each coral genus per zone
abundance <- corals %>%
  # Add transect proportion
  group_by(Age, ReefZone) %>%
  # Add transect proportion
  mutate(TotalSamplingLength = sum(`End-Start (Intercept)`)) %>%
  mutate(SampleProportion = (`End-Start (Intercept)` / TotalSamplingLength)) %>%
  group_by(Age, ReefZone, Genus) %>%
  summarise(RawAbundance = sum(`End-Start (Intercept)`),
            Abundance = sum(SampleProportion)) %>%
  group_by(Age, ReefZone) %>% 
  mutate(Total = sum(RawAbundance)) %>%
  as.data.frame()
write.csv(abundance, "results/abundance_zone.csv", row.names = FALSE)

# Abundance matrix per zone
abundance_mat <- abundance %>%
  mutate(Site = paste0(Age, "-", ReefZone)) %>%
  select(Site, Genus, Abundance) %>%
  matrify() %>%
  as.data.frame() %>%
  mutate(Age = sub("\\-.*", "", row.names(.)),
         ReefZone = sub(".*\\-", "", row.names(.)))
write.csv(abundance_mat, "results/abundance_matrix_zone.csv", row.names = FALSE)

# Calculate diversity metrics per transect
indices <- abundance_mat %>%
  select(-c("Age", "ReefZone")) %>%
  reframe(alpha = specnumber(.),
          shannon = diversity(x = ., index = "shannon"),
          pielou = shannon / log(alpha)) %>%
  mutate(LT = abundance_mat$LT,
         Age = abundance_mat$Age,
         ReefZone = abundance_mat$ReefZone)
write.csv(indices, "results/diversity_indices_zone.csv", row.names = FALSE)

# Calculate abundance of each coral genus per age
abundance <- corals %>%
  # Add transect proportion
  group_by(Age) %>%
  # Add transect proportion
  mutate(TotalSamplingLength = sum(`End-Start (Intercept)`)) %>%
  mutate(SampleProportion = (`End-Start (Intercept)` / TotalSamplingLength)) %>%
  group_by(Age, Genus) %>%
  summarise(RawAbundance = sum(`End-Start (Intercept)`),
            Abundance = sum(SampleProportion)) %>%
  group_by(Age) %>% 
  mutate(Total = sum(RawAbundance)) %>%
  as.data.frame()
write.csv(abundance, "results/abundance_age.csv", row.names = FALSE)

# Abundance matrix per age
abundance_mat <- abundance %>%
  select(Age, Genus, Abundance) %>%
  matrify() %>%
  as.data.frame() %>%
  mutate(Age = row.names(.))
write.csv(abundance_mat, "results/abundance_matrix_zone.csv", row.names = FALSE)

# Calculate diversity metrics per transect
indices <- abundance_mat %>%
  select(-c("Age")) %>%
  reframe(alpha = specnumber(.),
          shannon = diversity(x = ., index = "shannon"),
          pielou = shannon / log(alpha)) %>%
  mutate(Age = abundance_mat$Age)
write.csv(indices, "results/diversity_indices_age.csv", row.names = FALSE)

# Wrap up ---------------------------------------------------------------
# Alert
beepr::beep(sound = 2)
# Reset environment
rm(list = ls())
# Restart R
.rs.restartR()

