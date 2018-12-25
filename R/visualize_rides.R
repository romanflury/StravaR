# rm(list = ls())
# options(device = "X11"); graphics.off()
#
# source("code/activity.R")
# source("code/read_gpx.R")
# source("code/process_activity.R")
#
# test <- parse_gpx(file = "activities/20160420-034316-Ride.gpx")
# test2 <- parse_gpx(file = "activities/20160420-102514-Ride.gpx")
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
# library(StravaR)
# test <- load_activities(path = "../StravaR-code/activities/", merge = T)
# plot_activity(test)
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





