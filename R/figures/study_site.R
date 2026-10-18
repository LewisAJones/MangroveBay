# Header ----------------------------------------------------------------
# Project: MangroveBay
# File name: study_site.R
# Last updated: 2025-05-28
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
library(patchwork)
# Packages for getting map data
library(rnaturalearth)
library(rnaturalearthdata)
# Package for spatial operations
library(sf)
library(dplyr)


# Load data -------------------------------------------------------------
sites <- read.csv("data/Mangrove_Bay_corals.csv")
sites <- unique(data.frame(transect = sites$LT, 
                           lng = sites$GPS.Longitude.E,
                           lat = sites$GPS.Latitude.N,
                           Age = sites$Age,
                           ReefZone = sites$ReefZone))

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

# City reference points
cities <- data.frame(name = c("Hurghada", "El Qoseir", "Marsa Alam", "Mangrove Bay"),
                     lng = c(33.812811, 34.280984, 34.888994, 34.41968),
                     lat = c(27.254897, 26.102429, 25.068336, 25.87015))

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
  geom_point(data = cities, aes(x = lng, y = lat)) +
  geom_text_repel(data = cities, aes(x = lng, y = lat, label = name)) +
  geom_point(data = sites, aes(x = lng, y = lat), 
             colour = "black", fill = "#e09351", shape = 25, size = 4) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Longitude (º)") +
  ylab("Latitude (º)") +
  annotation_scale(text_cex = 1.1) +
  annotation_north_arrow(location = "bl",
                         pad_y = unit(1, "cm"),
                         height = unit(2, "cm"), width = unit(2, "cm")) +
  coord_sf(xlim = c(33, 38), ylim = c(24, 28)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = sea),
        plot.background = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        panel.grid = element_blank())

# Create inset
map <- localities +
  annotation_custom(grob = ggplotGrob(africa),
                    xmin = 36.7, xmax = 38,
                    ymin = 26.55, ymax = 28)
map


# Local-scale map -------------------------------------------------------

sites_sf <- st_as_sf(sites, coords = c("lng", "lat"), crs = 4326, remove = FALSE)

# Legend groups (with line breaks, as in the target figure)
leg_levels <- c("Modern - Current-exposed\n(3 depths; Transects A-C)",
                "Modern - Sheltered\n(3 depths; Transects D-F)",
                "MIS5e - Shallower reef horizon",
                "MIS5e - Deeper reef horizon")

sites_sf <- sites_sf %>%
  mutate(
    Group = substr(transect, 1, 1),            # A-F for modern; "S" for SB sites
    Site  = gsub("_", "", transect),           # SB_1 -> SB1
    Legend = case_when(
      Age == "Modern" & Group %in% c("A", "B", "C") ~ leg_levels[1],
      Age == "Modern" & Group %in% c("D", "E", "F") ~ leg_levels[2],
      Age == "MIS5e"  & ReefZone == "Reef edge"          ~ leg_levels[3],
      Age == "MIS5e"  & ReefZone == "Shallow reef slope" ~ leg_levels[4]
    ),
    Legend = factor(Legend, levels = leg_levels)
  )

# Collapse the replicate modern transects to one point per group.
# MIS5e sites all have unique coordinates, so they are kept as they are.
plot_sf <- sites_sf %>%
  group_by(Legend, lng, lat) %>%
  slice(1) %>%
  ungroup()

leg_fill  <- setNames(c("#DDB892", "#DDB892", "#5BC8D0", "#5BC8D0"), leg_levels)
leg_shape <- setNames(c(22, 23, 24, 25), leg_levels)

# Label positions for the MIS5e sites (offsets in degrees; tweak by eye).
# Positive nudge_x moves a label right, positive nudge_y moves it up.
label_sf <- plot_sf %>%
  filter(Age == "MIS5e") %>%
  left_join(
    tibble::tribble(
      ~Site, ~nudge_x,  ~nudge_y,
      "SB1",  0.00045,   0.00060,
      "SB2",  0.00080,   0.00045,
      "SB3",  0.00040,   0.00070,
      "SB4",  0.00090,   0.00000,
      "SB5",  0.00000,   0.00080,
      "SB6", -0.00040,   0.00065,
      "SB7", -0.00060,   0.00055
    ),
    by = "Site"
  )

local <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 18) +
  geom_sf(data = plot_sf, aes(fill = Legend, shape = Legend),
          colour = "black", size = 3.25, alpha = 0.7) +
  geom_text_repel(data = label_sf,
                  aes(label = Site, geometry = geometry),
                  stat = "sf_coordinates",
                  nudge_x = label_sf$nudge_x,
                  nudge_y = label_sf$nudge_y,
                  size = 4.2, segment.size = 1, segment.colour = "black",
                  min.segment.length = 0, box.padding = 0.1,
                  point.padding = 0.3, force = 0) +
  # ylim range = 0.8 x xlim range, matching panel A's aspect (4 / 5 degrees)
  coord_sf(xlim = c(34.412, 34.4225), ylim = c(25.8655, 25.8739), crs = 4326) +
  xlab("Longitude (º)") +
  ylab("Latitude (º)") +
  annotation_scale(text_cex = 1.1) +
  annotation_north_arrow(location = "bl",
                         pad_y = unit(1, "cm"),
                         height = unit(2, "cm"), width = unit(2, "cm")) +
  scale_fill_manual(name = NULL, values = leg_fill, breaks = leg_levels) +
  scale_shape_manual(name = NULL, values = leg_shape, breaks = leg_levels) +
  guides(fill  = guide_legend(override.aes = list(alpha = 0.7, size = 4)),
         shape = guide_legend(override.aes = list(alpha = 0.7, size = 4))) +
  theme_bw() +
  theme(panel.background = element_rect(fill = sea),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.position = "inside",
        legend.position.inside = c(0.99, 0.99),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "grey98", colour = "black",
                                         linewidth = 0.5),
        legend.key = element_blank(),
        legend.key.spacing.y = unit(4, "pt"),
        legend.text = element_text(size = 11),
        legend.margin = margin(8, 10, 8, 10))
#local

# Join maps -------------------------------------------------------------

final_map <- map + local +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 20, face = "bold"))
final_map

# Save map --------------------------------------------------------------
ggsave("figures/study-site-map.png", final_map,
       bg = "white", dpi = 600,
       width = 400, height = 180, units = "mm")
ggsave("figures/study-site-map.pdf", final_map, 
       bg = "white", dpi = 600,
       width = 400, height = 180, units = "mm")
