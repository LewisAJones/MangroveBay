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
# Was reef coral cover greater during MIS5e? ----------------------------
# Load data
coverage <- read.csv("./results/coverage.csv")
# Split into list
coverage$split <- paste0(coverage$Age, "_", coverage$ReefZone)
coverage <- split(x = coverage, f = coverage$split)
# Calculate median
median(coverage$`MIS5e_Reef edge`$Coverage)
median(coverage$`Modern_Reef edge`$Coverage)
median(coverage$`MIS5e_Reef slope`$Coverage)
median(coverage$`Modern_Reef slope`$Coverage)
# Calculate IQR
IQR(coverage$`MIS5e_Reef edge`$Coverage)
IQR(coverage$`Modern_Reef edge`$Coverage)
IQR(coverage$`MIS5e_Reef slope`$Coverage)
IQR(coverage$`Modern_Reef slope`$Coverage)
# Perform tests
# MIS5e reef edge vs Modern reef edge
wilcox.test(x = coverage$`MIS5e_Reef edge`$Coverage, 
            y = coverage$`Modern_Reef edge`$Coverage,
            alternative = c("greater"))
# MIS5e reef slope vs Modern reef slope
wilcox.test(x = coverage$`MIS5e_Reef slope`$Coverage, 
            y = coverage$`Modern_Reef slope`$Coverage,
            alternative = c("greater"))

# Did colony size distribution decrease? --------------------------------
# Load data
colony <- read_csv("./data/Mangrove_Bay_corals.csv")
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

# Was diversity higher in MIS5e? ----------------------------------------
# Load data
diversity <- read.csv("./results/diversity_indices.csv")
# Summarise across age and reef zone
diversity %>%
  group_by(Age, ReefZone) %>%
  summarise(Alpha = median(Alpha),
         Pielou = median(Pielou))

# Which taxa dominated the reef? ----------------------------------------
# Load data
abundance <- read.csv("./results/abundance_site.csv")
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
  filter(Age == "Modern", ReefZone == "Reef slope") %>%
  arrange(desc(Abundance))
abundance %>% 
  filter(Age == "MIS5e", ReefZone == "Reef slope") %>%
  arrange(desc(Abundance))

# Are MIS5e communities distinct from the Modern? -----------------------
mat <- read.csv("./results/abundance_matrix.csv")
# Extract columns
df <- mat[, c(32, 33, 34)]
# Drop columns
mat <- mat[, -c(32, 33, 34)]
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
