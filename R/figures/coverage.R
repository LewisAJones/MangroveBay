# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: coverage.R
# Last updated: 2024-09-24
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Libraries -------------------------------------------------------------
library(ggplot2)
library(MetBrewer)

# Load data -------------------------------------------------------------
coverage <- read.csv("./results/coverage.csv")
coverage$Coverage <- coverage$Coverage * 100

# Set factor levels
coverage$Age <- factor(coverage$Age, levels = c("Modern", "MIS5e"))
coverage$ReefZone <- factor(coverage$ReefZone, levels = c("Reef edge", 
                                                        "Reef slope"))

# Plot data -------------------------------------------------------------

ggplot(data = coverage, aes(x = ReefZone, y = Coverage, fill = ReefZone)) +
  geom_point(pch = 21, colour = "black", size = 3, alpha = 0.85) +
  scale_fill_met_d("Hokusai2") +
  facet_wrap(~Age, ncol = 1) +
  ylab("Coverage (%)") +
  xlab("Reef Zone") +
  theme_bw() +
  theme(
    legend.position = "none"
  )

# Save ------------------------------------------------------------------

ggsave("./figures/coverage.png",
       height = 125, width = 75, units = "mm",
       dpi = 300)
