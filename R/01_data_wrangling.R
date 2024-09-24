# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: 01_data_wrangling.R
# Last updated: 2024-09-24
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Load libraries --------------------------------------------------------
# install.packages("tidyverse")
# install.packages("worrms")
# install.packages("beepr")
library(tidyverse)
library(magrittr)
library(worrms)
library(beepr)

# Load data -------------------------------------------------------------
corals <- read_csv("data/Mangrove_Bay_2024-08-12.csv")

# Wrangling -------------------------------------------------------------
# Add transect length
corals %<>%
  group_by(LT) %>%
  mutate(TransectLength = sum(`End-Start (Intercept)`)) %>%
  relocate(TransectLength, .after = "End-Start (Intercept)")

# Retain biological entities (those with AphiaID)
corals %<>% filter(!is.na(AphiaID))
corals$AphiaID <- as.numeric(corals$AphiaID)

## Update taxonomy using WoRMs ------------------------------------------
# Get unique AphiaIDs
AphiaID <- unique(corals$AphiaID)
# Get taxonomy based on WoRMs
tax <- wm_classification_(id = AphiaID)[, c("id", "rank", "scientificname")]
# Expand to wide-format
tax <- pivot_wider(data = tax,
                   names_from = rank,
                   values_from = scientificname)
# Update column name for merging
colnames(tax)[which(colnames(tax) == "id")] <- "AphiaID"
# Retain desired columns
tax <- tax[c("AphiaID", "Kingdom", "Phylum", "Class", "Order",
             "Family", "Genus", "Species")]
# Add rank
tax$Rank <- "Species"
tax[which(is.na(tax$Species)), "Rank"] <- "Genus"
tax[which(is.na(tax$Genus)), "Rank"] <- "Family"
tax[which(is.na(tax$Family)), "Rank"] <- "Order"
# Drop genus/species columns
corals %<>% select(-c("Genus", "Species"))
# Merge datasets
tax$AphiaID <- as.numeric(tax$AphiaID)
corals %<>% left_join(x = ., y = tax, by = "AphiaID")
# replace NAs for order
corals[which(corals$Rank == "Order"), "Family"] <- "fam."
corals[which(corals$Rank == "Order"), "Genus"] <- "gen."
corals[which(corals$Rank == "Order"), "Species"] <- "sp."
# replace NAs for family
corals[which(corals$Rank == "Family"), "Genus"] <- "gen."
corals[which(corals$Rank == "Family"), "Species"] <- "sp."
# replace NAs for genus
corals[which(corals$Rank == "Genus"), "Species"] <- "sp."
# Move AphiaID
corals %<>% relocate(AphiaID, .after = "Rank")
# Split species name
corals$Species <- sub(".* ", "", corals$Species)
# Add scientific name
corals$ScientificName <- paste0(corals$Genus, " ", corals$Species)
corals[which(corals$Rank == "Family"), "ScientificName"] <-
  corals[which(corals$Rank == "Family"), c("Family")]
corals[which(corals$Rank == "Order"), "ScientificName"] <-
  corals[which(corals$Rank == "Order"), c("Order")]
# Move ScientificName
corals %<>% relocate(ScientificName, .after = "Species")

## Retain only Scleractinia and Anthoathecata ---------------------------
corals <- subset(x = corals, Order %in% c("Scleractinia", "Anthoathecata"))

# Species groups --------------------------------------------------------
# Acropora
corals$ScientificName[which(corals$Genus == "Acropora")] <- "Acropora sp."
corals$Rank[which(corals$Genus == "Acropora")] <- "Genus"

# Porites
corals$ScientificName[which(corals$Genus == "Porites")] <- "Porites sp."
corals$Rank[which(corals$Genus == "Porites")] <- "Genus"

# Millepora
corals$ScientificName[which(corals$Genus == "Millepora")] <- "Millepora sp."
corals$Rank[which(corals$Genus == "Millepora")] <- "Genus"

## Exclude deeper reef slope --------------------------------------------
corals <- subset(x = corals, ReefZone != c("Deeper reef slope"))
# Rename shallow reef slope
corals$ReefZone[which(corals$ReefZone == "Shallow reef slope")] <- "Reef slope"

# Save ------------------------------------------------------------------
# Sort by index
corals <- corals[order(corals$Index), ]
# Add new index
corals$Index <- 1:nrow(corals)
# Save csv
write.csv(corals, "./data/Mangrove_Bay_corals.csv",
          row.names = FALSE)
# Alert
beepr::beep(sound = 2)
# Reset environment
rm(list = ls())
# Restart R
.rs.restartR()
