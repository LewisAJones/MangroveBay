# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: coverage.R
# Last updated: 2024-09-24
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Libraries -------------------------------------------------------------
library(ggplot2)

# Load data -------------------------------------------------------------
coverage <- read.csv("./results/coverage.csv")
coverage$Coverage <- coverage$Coverage * 100

# Set factor levels
coverage$Age <- factor(coverage$Age, levels = c("Modern", "MIS5e"))
coverage$ReefZone <- factor(coverage$ReefZone, levels = c("Reef edge", 
                                                        "Reef slope"))
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
  geom_point(colour = "black", size = 3, alpha = 0.85) +
  geom_point(data = median, aes(x = ReefZone, y = Median),
             colour = "black", fill = "yellow", shape = 23, size = 1.5, alpha = 0.75) +
  scale_shape_manual(values = c("Reef edge" = 21, "Reef slope" = 22)) +
  facet_wrap(~Age, ncol = 1, labeller = as_labeller(labs)) +
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
