library(leaflet)
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
  # Set the default lat and long and zoom level
  setView(lng = 34.418, lat = 25.8685, zoom = 18) %>%
  addCircles(lng = sites$lng, lat = sites$lat, 
             color = sites$col) %>%
  addScaleBar() %>%
  # Add default tile
  addTiles()
m
## save html to png
saveWidget(m, "temp.html", selfcontained = FALSE)
webshot("temp.html", file = "./figures/local_map.png",
        cliprect = "viewport",
        vwidth = 2000,
        vheight = 1500)
