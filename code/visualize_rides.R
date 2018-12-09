rm(list = ls())
options(device = "X11"); graphics.off()

source("code/read_gpx.R")
source("code/process_activity.R")

test <- parse_gpx(file = "activities/20160420-034316-Ride.gpx")

plot(test@lon, test@lat, type = "p")
plot(test@elevations, type = "p", col = get_elevationlevel(test), ylim = c(300, 600))
lines(get_speeds(test) + 400)







# using a leaflet ----------------------------------------------------------------- #
library(leaflet)

lons.range <- c(min(test@lon), max(test@lon))
lats.range <- c(min(test@lat), max(test@lat))

map <- leaflet()
addProviderTiles(map, "Thunderforest.OpenCycleMap", # nice: CartoDB.Positron, OpenMapSurfer.Grayscale, CartoDB.DarkMatterNoLabels 
                 options = providerTileOptions(noWrap = T))
fitBounds(map, lng1 = lons.range[1], lat1 = lats.range[2], lng2 = lons.range[2], lat2 = lats.range[2])

addPolylines(map, lng = test@lon,
             lat = test@lat,
             color = "black", opacity = 1/3, weight = 2)
# --------------------------------------------------------------------------------- #







