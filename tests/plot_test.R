rm(list = ls())
options(device = "X11"); graphics.off()
#
#
test <- parse_gpx(file = "../StravaR-code/activities/20160420-034316-Ride.gpx")
test2 <- parse_gpx(file = "../StravaR-code/activities/20160420-102514-Ride.gpx")
# test3 <- parse_gpx(file = "activities/20160421-170244-Ride.gpx")
# test4 <- parse_gpx(file = "activities/20160429-034350-Ride.gpx")
# test5 <- parse_gpx(file = "activities/20160429-153851-Ride.gpx")
# test6 <- parse_gpx(file = "activities/20160430-150349-Ride.gpx")
#
# plot(test@lon, test@lat, type = "p")
# plot(test@elevations, type = "p", col = get_elevationlevel(test), ylim = c(300, 600))
# lines(get_speeds(test) + 400)
#
# l_a <- c(test, test2, test3, test4, test5, test6)
# test <- merge_activity(list_activity = l_a)
#
#
# rm(list = ls())
# options(device = "X11"); graphics.off()
#
library(StravaR)
test <- load_activities(path = "../StravaR-code/activities/", merge = F)
test <- pload_activities(path = "../StravaR-code/activities/", merge = F)

#
plot.activity(test)
# lapply(test, lines)

#
#
#
#
#
#
#
# # using a leaflet ----------------------------------------------------------------- #
# library(leaflet)
#
# lons.range <- c(min(test@lon), max(test@lon))
# lats.range <- c(min(test@lat), max(test@lat))
#
# map <- leaflet()
# addProviderTiles(map, "Thunderforest.OpenCycleMap", # nice: CartoDB.Positron, OpenMapSurfer.Grayscale, CartoDB.DarkMatterNoLabels
#                  options = providerTileOptions(noWrap = T))
# fitBounds(map, lng1 = lons.range[1], lat1 = lats.range[2], lng2 = lons.range[2], lat2 = lats.range[2])
#
# addPolylines(map, lng = test@lon,
#              lat = test@lat,
#              color = "black", opacity = 1/3, weight = 2)
# # --------------------------------------------------------------------------------- #
#
#
# # using ggmap --------------------------------------------------------------------- #
# ggmap::qmplot(V2, V1, data = as.data.frame(cbind(test@lat, test@lon, test@elevations)),
#        maptype = "watercolor", colour = I("black"), size = I(2), darken = .1)
# # --------------------------------------------------------------------------------- #




# range(activity@lat)
# range(activity@lon)
#
# get_countries(activity)
#
# library(OpenStreetMap)
# ul <- c(47.515, 8.4) + c(.01, -.01)
# lr <- c(47.35, 8.6) + c(-.01, .01)
# map <- openmap(ul,lr,zoom=NULL,'stamen-watercolor')
#
#
# # plot(map)
#
# library(sp)
# library(rgdal)
# subscr <- data.frame(lat=activity@lon, lon=activity@lat, ele=activity@elevations)
# subscr <- data.frame(lat=c(8.544, 8.499,8.51), lon=c(47.50, 47.49,47.39), ele=c(58,12,150))
# # subscr <- subscr[c(1,100,300),]
# coordinates(subscr)<-~lat+lon
# # proj4string(subscr)<-CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")
# proj4string(subscr)<-CRS("+init=epsg:4326")
#
#
# tmp <- as.data.frame(spTransform(subscr, osm()))
#
# plot(map)
# lines(y = tmp$lon, x = tmp$lat, lwd = 3)
# # points(subscr,add=TRUE,col=(subscr$ele)+4)
#
# # lines(tmp$lon, tmp$lat, col = "yellow")
# # points(subscr)
# symbols(y = tmp$lon, x = tmp$lat, circles = tmp$ele, add = TRUE, inches = 0.2, bg = "darkgreen")
#
#
#
#
#
# subscr<-data.frame(lat=c(10.1237,10.2161,10.2993),
#                    lon=c(59.7567,59.7527,59.6863), pop=c(58,12,150))
# coordinates(subscr)<-~lat+lon
# proj4string(subscr)<-CRS("+init=epsg:4326")
# lat <- c(59.7916,59.6563)
# lon <- c(10.0937,10.3293)
# map1 <- openmap(c(lat[1],lon[1]),c(lat[2],lon[2]),zoom=10,'osm')
# plot(map)
# tmp <- as.data.frame(spTransform(subscr, osm()))
# symbols(y = tmp$lon, x = tmp$lat, circles = tmp$pop, add = TRUE,
#         inches = 0.2, bg = "darkgreen")
