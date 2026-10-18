library(leaflet)
library(leafem)
library(htmlwidgets)
library(webshot)

sites <- read.csv("./data/Mangrove_Bay_corals.csv")
sites <- unique(data.frame(transect = sites$LT, 
                           lng = sites$GPS.Longitude.E,
                           lat = sites$GPS.Latitude.N,
                           Age = sites$Age,
                           ReefZone = sites$ReefZone))
sites$col <- "red"
sites$col[which(sites$ReefZone == "Reef slope")] <- "blue"

m <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addTiles() %>%
  # Set the default lat and long and zoom level
  setView(lng = 34.418, lat = 25.8685, zoom = 18) %>%
  addCircles(lng = sites$lng, lat = sites$lat, 
             color = sites$col) %>%
  addScaleBar() %>%
  # Grid lines every 0.001 degrees (adjust to suit your zoom level)
  addGraticule(interval = 0.01,
               style = list(color = "black", weight = 2))
m
## save html to png
saveWidget(m, "temp.html", selfcontained = FALSE)
webshot("temp.html", file = "./figures/local_map.png",
        cliprect = "viewport",
        vwidth = 2000,
        vheight = 1500)

library(ggplot2)
library(ggspatial)
library(sf)

sites_sf <- st_as_sf(sites, coords = c("lng", "lat"), crs = 4326)

ggplot() +
  annotation_map_tile(type = "osm", zoom = 12) +
  geom_sf(data = sites_sf, aes(colour = col), show.legend = TRUE) +
  scale_colour_identity() +
  coord_sf(xlim = c(34.412, 34.4225), ylim = c(25.8655, 25.8715), crs = 4326) +
  xlab("Longitude (º)") +
  ylab("Latitude (º)") +
  annotation_scale(text_cex = 1.1) +
  annotation_north_arrow(location = "bl",
                         pad_y = unit(1, "cm"),
                         height = unit(2, "cm"), width = unit(2, "cm"))
