# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: 02_metrics.R
# Last updated: 2024-09-24
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Libraries -------------------------------------------------------------
library(tidyverse)
library(vegan)
library(labdsv)

# Load data -------------------------------------------------------------
corals <- read_csv("./data/Mangrove_Bay_corals.csv")

# Transects -------------------------------------------------------------
# Summarise the number of transects
transects <- corals %>%
  group_by(Age, ReefZone) %>%
  summarise(TransectsPerZone = length(unique(LT)),
            TransectMeanLength = mean(unique(TransectLength)),
            TransectSDLength = sd(unique(TransectLength)))
write.csv(transects, "./results/transects.csv", row.names = FALSE)

# Rank IDs --------------------------------------------------------------
# Summarise occurrence rank IDs
rank <- corals %>%
  group_by(Age, Rank) %>%
  summarise(RankID = length(ScientificName))
write.csv(rank, "./results/rank_id.csv", row.names = FALSE)

# Coverage --------------------------------------------------------------
# Calculate proportional reef coverage
# Check totals equal 1
coverage <- corals %>%
  # Group by transect
  group_by(Age, LT, ReefZone) %>%
  # Summarise intercept lengths
  summarise(Coverage = sum(`End-Start (Intercept)` / TransectLength))
write.csv(coverage, "./results/coverage.csv", row.names = FALSE)

# Filter data -----------------------------------------------------------
# Not all data is resolved to species-level
# We will use genus-level, exclude data resolved only to order/family
corals <- corals %>%
  filter(Rank %in% c("Species", "Genus"))

# Abundance -------------------------------------------------------------
# Calculate abundance of each coral genus
abundance <- corals %>%
  # Add transect proportion
  group_by(ReefZone, Age) %>%
  # Add transect proportion
  mutate(TotalSamplingLength = sum(`End-Start (Intercept)`)) %>%
  mutate(SampleProportion = (`End-Start (Intercept)` / TotalSamplingLength)) %>%
  group_by(ReefZone, Age, Genus) %>%
  summarise(RawAbundance = sum(`End-Start (Intercept)`),
            Abundance = sum(SampleProportion)) %>%
  as.data.frame()

abundance %>%
  # Add transect proportion
  group_by(ReefZone, Age) %>%
  summarise(Proportion = sum(Abundance))

write.csv(abundance, "./results/abundance.csv", row.names = FALSE)

# Matrix ----------------------------------------------------------------
# Create matrix of abundance data
abundance_mat <- matrify(abundance[, c("LT", "Genus", "Abundance")])
write.csv(abundance_mat, "./results/abundance_matrix.csv", row.names = TRUE)

# Diversity -------------------------------------------------------------
# Calculate diversity metrics
alpha <- specnumber(abundance_mat)
shannon <- diversity(x = abundance_mat, index = "shannon")
simpson <- diversity(x = abundance_mat, index = "simpson")
pielou <- shannon / log(alpha)
lt <- names(alpha)
# Bind data
indices <- data.frame(LT = lt,
                      Alpha = alpha,
                      Shannon = shannon,
                      Simpson = simpson,
                      Pielou = pielou,
                      row.names = NULL)
# Add reef zone and coverage
indices <- left_join(x = coverage, y = indices, by = "LT")
write.csv(indices, "./results/diversity_indices.csv", row.names = FALSE)

