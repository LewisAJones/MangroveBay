# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: 03_NMDS.R
# Last updated: 2026-03-10
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Load libraries --------------------------------------------------------
# install.packages("vegan")
# install.packages("labdsv")
# install.packages("tidyverse")
library(vegan)
library(labdsv)
library(tidyverse)

# Load data -------------------------------------------------------------
abundance <- read.csv("results/abundance_matrix_transect.csv")
rownames(abundance) <- abundance$LT

# NMDS ------------------------------------------------------------------
# Calculate NMDS
nmds <- metaMDS(comm = abundance[, 1:(ncol(abundance)-3)], 
                distance = "bray", k = 2, trymax = 1000)
saveRDS(nmds, file = "results/nmds.RDS", compress = "xz")
# Report stress value
message("The stress value for this NMDS is: ", round(nmds$stress, 3))
# Plot stress plot
stressplot(nmds)
# Now we can plot the NMDS
plot(nmds, type = "p")
# Extract scores
nmds <- as.data.frame(vegan::scores(nmds)$sites)
# Create site columns
nmds$LT <- rownames(nmds)
# Drop row names
rownames(nmds) <- NULL
# Add reef zone and age
zone <- abundance[, (ncol(abundance)-2):(ncol(abundance))]
nmds <- left_join(x = zone, y = nmds, by = "LT")

# Save plot data
saveRDS(nmds, "./results/NMDS_plot_data.RDS")

# SIMPER ----------------------------------------------------------------
simp <- summary(simper(abundance[, 1:(ncol(abundance)-3)], abundance$Age))
write.csv(simp$Modern_MIS5e, "results/simper.csv", row.names = TRUE)
        