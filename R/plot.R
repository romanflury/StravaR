
#' plot an activity, using OpenStreetMap.
#'
#' @param activity an object of S4 class activity.
#' @param osm_type openstreetmap type, see \code{\link{openmap}} for possible values.
#' @param padby pad upperleft and lowerright latitude/longuitude points by this value.
#'
#' @export
#'
#' @importFrom OpenStreetMap openmap
#' @importFrom OpenStreetMap osm
#' @importFrom sp coordinates
#' @importFrom sp proj4string
#' @importFrom sp CRS
#' @importFrom sp spTransform
plot_activity <- function(activity, osm_type = "stamen-watercolor", padby = 0.01) {
  check_activity(activity)

  ul <- c(max(activity@lat), min(activity@lon)) + padby*c(1, -1)
  lr <- c(min(activity@lat), max(activity@lon)) + padby*c(-1, 1)

  map <- OpenStreetMap::openmap(ul, lr, zoom = NULL, type = osm_type)

  route <- data.frame(lat = activity@lon, lon = activity@lat)
  sp::coordinates(route) <- ~lat+lon
  sp::proj4string(route) <- sp::CRS("+init=epsg:4326")
  route <- as.data.frame(sp::spTransform(route, OpenStreetMap::osm()))

  plot(map)
  lines(y = route$lon, x = route$lat, lwd = 3)
}

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