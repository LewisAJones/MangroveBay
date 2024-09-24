# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: study_site.R
# Last updated: 2024-09-24
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/MangroveBay

# Libraries -------------------------------------------------------------
# Package for generic plotting
library(ggplot2)
# Package for adding scale bar and north arrow
library(ggspatial)
# Package for text labels
library(ggrepel)
# Package for combining plots
library(cowplot)
# Packages for getting map data
library(rnaturalearth)
library(rnaturalearthdata)
library(MetBrewer)

# Load data -------------------------------------------------------------
sites <- read.csv("./data/Mangrove_Bay_corals.csv")
sites <- unique(data.frame(transect = sites$LT, 
                           lng = sites$GPS.Longitude.E,
                           lat = sites$GPS.Latitude.N,
                           Age = sites$Age,
                           ReefZone = sites$ReefZone))
sites$col <- "red"
sites$col[which(sites$ReefZone == "Reef slope")] <- "blue"

# Get polygons ----------------------------------------------------------
# World sf
world <- ne_countries(scale = "small", continent = "Africa",
                      returnclass = "sf")
# Define countries for plotting
countries <- c("egypt", "saudi arabia", "jordan", "isreal", "syria",
               "lebanon", "iraq", "iran", "kuwait")
# Get country sfs
countries <- ne_countries(scale = "large", country = countries,
                          returnclass = "sf")

# Generate map ----------------------------------------------------------
# Define colours for plotting
land <- "lightgrey"
sea <- "aliceblue"

# Make map of Africa
africa <- ggplot(data = world) +
  geom_sf(colour = land, fill = land) +
  geom_sf(data = countries, colour = land, fill = land) +
  geom_rect(aes(xmin = 33, xmax = 38, ymin = 24, ymax = 28),
            fill = NA, colour = "black", linewidth = 0.3) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_sf(xlim = c(-19, 50)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = sea),
        plot.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

# Make locality map
localities <- ggplot(data = countries) +
  geom_sf(colour = "black", fill = land) +
  geom_point(data = sites, aes(x = lng, y = lat), 
             colour = "black", fill = "#e09351", shape = 21, size = 4) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Longitude (º)") +
  ylab("Latitude (º)") +
  annotation_scale(text_cex = 1) +
  annotation_north_arrow(location = "bl",
                         pad_y = unit(1, "cm"),
                         height = unit(2, "cm"), width = unit(2, "cm")) +
  coord_sf(xlim = c(33, 38), ylim = c(24, 28)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = sea),
        plot.background = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        panel.grid = element_blank())

# Create inset
map <- ggdraw(localities) +
  draw_plot(
    {
      africa
    },
    x = 0.724, y = 0.677,
    width = 0.3, height = 0.3
  )
map
# Save map --------------------------------------------------------------
ggsave("./figures/study-site-map.png",
       bg = "white", dpi = 600,
       width = 200, height = 180, units = "mm")


