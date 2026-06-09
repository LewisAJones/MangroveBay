# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: 04_stats.R
# Last updated: 2025-01-27
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Load libraries --------------------------------------------------------
library(tidyverse)
library(vegan)

# Did colony size distribution decrease? --------------------------------
# Load data
colony <- read_csv("./data/Mangrove_Bay_corals.csv")
# Filter by rank
colony <- colony %>%
  filter(Rank %in% c("Species", "Genus"))
# How many unique genera?
length(unique(colony$Genus))
# Which taxa are present in both modern and fossil?
l <- split(x = colony, f = colony$Age)
lapply(l, function(x) length(unique(x$Genus)))

# Average size distribution 
colony <- subset(colony, Rank %in% c("Genus", "Species"))
colony <- colony %>%
  select(Age, Genus, `End-Start (Intercept)`)
# Summary stats
colony <- colony %>%
  group_by(Genus, Age) %>%
  mutate(median = median(`End-Start (Intercept)`),
         n = length(`End-Start (Intercept)`))
# Exclude genera with small number of intercepts
exclude <- colony[which(colony$n < 25), "Genus"]$Genus
colony <- subset(colony, !Genus %in% exclude)
unique(colony[, c("Age", "Genus", "median")])
# Split into list
colony$split <- paste0(colony$Genus, "_", colony$Age)
colony <- split(x = colony, f = colony$split)
# Perform tests
# MIS5e Acropora vs Modern Acropora
wilcox.test(x = colony$Acropora_MIS5e$`End-Start (Intercept)`, 
            y = colony$Acropora_Modern$`End-Start (Intercept)`,
            alternative = c("greater"))
# MIS5e Goniastrea vs Modern Goniastrea
wilcox.test(x = colony$Goniastrea_MIS5e$`End-Start (Intercept)`, 
            y = colony$Goniastrea_Modern$`End-Start (Intercept)`,
            alternative = c("greater"))
# MIS5e Millepora vs Modern Millepora
wilcox.test(x = colony$Millepora_MIS5e$`End-Start (Intercept)`, 
            y = colony$Millepora_Modern$`End-Start (Intercept)`,
            alternative = c("greater"))
# MIS5e Pocillopora vs Modern Pocillopora
wilcox.test(x = colony$Pocillopora_MIS5e$`End-Start (Intercept)`, 
            y = colony$Pocillopora_Modern$`End-Start (Intercept)`,
            alternative = c("greater"))
# MIS5e Porites vs Modern Porites
wilcox.test(x = colony$Porites_MIS5e$`End-Start (Intercept)`, 
              y = colony$Porites_Modern$`End-Start (Intercept)`,
            alternative = c("greater"))

# Which taxa dominated the reef? ----------------------------------------
# Load data
abundance <- read.csv("results/abundance_transect.csv")
# Convert to percentages
abundance$Abundance <- abundance$Abundance * 100
# Summarise via age and reef zone
abundance <- abundance %>%
  group_by(Age, ReefZone) %>%
  mutate(Threshold = Abundance >= 10)
# Add index
abundance$Genus[which(abundance$Threshold == FALSE)] <- "Other"
# Summarise for Reef Zone and Age
abundance %>% 
  filter(Age == "Modern", ReefZone == "Reef edge") %>%
  arrange(desc(Abundance))
abundance %>% 
  filter(Age == "MIS5e", ReefZone == "Reef edge") %>%
  arrange(desc(Abundance))
abundance %>% 
  filter(Age == "Modern", ReefZone == "Shallow reef slope") %>%
  arrange(desc(Abundance))
abundance %>% 
  filter(Age == "MIS5e", ReefZone == "Shallow reef slope") %>%
  arrange(desc(Abundance))

# Are MIS5e communities distinct from the Modern? -----------------------
mat <- read.csv("results/abundance_matrix_transect.csv")
# Extract columns
df <- mat[, c(37, 38, 39)]
# Drop columns
mat <- mat[, -c(37, 38, 39)]
# Create distance matrix
mat_dist <- vegdist(mat, method = "bray")
# Plot dispersion
# Age
disp <- betadisper(mat_dist, group = df$Age, type = "centroid")
plot(disp)
anova(disp)
# Reef Zone
disp <- betadisper(mat_dist, group = df$ReefZone, type = "centroid")
plot(disp)
anova(disp)
# Permanova
adonis2(formula = mat ~ as.factor(Age), 
        data = df, 
        permutations = 9999, 
        method = "bray")
adonis2(formula = mat ~ as.factor(ReefZone), 
        data = df, 
        permutations = 9999, 
        method = "bray")
