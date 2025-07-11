# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: coverage.R
# Last updated: 2025-07-11
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Libraries -------------------------------------------------------------
library(ggplot2)
library(scales)

# Load data -------------------------------------------------------------
coverage <- read.csv("./results/coverage.csv")
coverage$Coverage <- coverage$Coverage * 100

# Update names
coverage$ReefZone[which(coverage$Age == "MIS5e" & 
                          coverage$ReefZone == "Reef edge")] <- "Shallower reef horizon"
coverage$ReefZone[which(coverage$Age == "MIS5e" & 
                          coverage$ReefZone == "Shallow reef slope")] <- "Deeper reef horizon"

# Set factor levels
coverage$Age <- factor(x = coverage$Age, levels = c("Modern", "MIS5e"))
coverage$ReefZone <- factor(coverage$ReefZone, levels = c("Reef edge", 
                                                          "Shallow reef slope",
                                                          "Deeper reef slope",
                                                          "Shallower reef horizon",
                                                          "Deeper reef horizon"))
# Calculate median
median <- coverage %>%
  group_by(Age, ReefZone) %>%
  summarise(Median = median(Coverage))

# Labels
labs <- c("Modern" = "Modern",
          "MIS5e" = "MIS5e (Last Interglacial)")

# Plot data -------------------------------------------------------------

ggplot(data = coverage, aes(x = ReefZone, y = Coverage, 
                            fill = Age, shape = ReefZone)) +
  geom_point(colour = "black", size = 3, alpha = 0.7) +
  geom_point(data = median, aes(x = ReefZone, y = Median),
             fill = "yellow", colour = "black", shape = 23, size = 1.5, alpha = 0.75) +
  scale_shape_manual(labels = c("Modern" = "Modern", 
                                "MIS5e" = "MIS5e (Last Interglacial)"),
                     values = c("Reef edge" = 21, 
                                "Shallow reef slope" = 22, 
                                "Deeper reef slope" = 23,
                                "Shallower reef horizon" = 24,
                                "Deeper reef horizon" = 25)) +
  scale_x_discrete(labels = wrap_format(14)) +
  facet_wrap(~Age, ncol = 2, scales = "free_x", labeller = as_labeller(labs)) +
  ylab("Coverage (%)") +
  xlab("Reef Zone") +
  theme_bw() +
  theme(
    legend.position = "none"
  )

# Save ------------------------------------------------------------------

ggsave("./figures/coverage.png",
       height = 75, width = 150, units = "mm",
       dpi = 300, scale = 1.5)
