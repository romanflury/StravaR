
#' taking lat long coordinates of an object of S4 class activity and joining the corresponding points with line segments.
#'
#' @param activity an object of S4 class activity.
#'
#' @export
#'
#' @importFrom graphics lines
lines.activity <- function(activity, ...) {
  check_activity(activity)

  route <- data.frame(lat = activity@lon, lon = activity@lat)
  sp::coordinates(route) <- ~lat+lon
  sp::proj4string(route) <- sp::CRS("+init=epsg:4326")
  route <- as.data.frame(sp::spTransform(route, OpenStreetMap::osm()))

  lines(y = route$lon, x = route$lat, ...)
}

#' plot a Strava activtiy on OpenStreetmap
#'
#' @param x an object of S4 class activity.
#'
#' @export
setMethod(f = "lines",
          signature = c(x = "activity"),
          definition = function(x){ plot.lines(x) } )

#' plot an activity, using OpenStreetMap.
#'
#' @param activity an object of S4 class activity.
#' @param ul upperleft lat and long.
#' @param lr lowerright lat and long.
#' @param osm_type openstreetmap type, see \code{\link{openmap}} for possible values.
#' @param padby pad upperleft and lowerright lat long points by this value.
#' @param ... additional arguments for \code{plot}.
#'
#' @export
#'
#' @importFrom OpenStreetMap openmap osm
#' @importFrom sp coordinates proj4string CRS spTransform
#' @importFrom graphics plot
plot.activity <- function(activity, ul, lr, osm_type = "stamen-watercolor", padby = 0.01, ...) {
  check_activity(activity)

  if(missing(ul)) {
    ul <- c(max(activity@lat), min(activity@lon)) + padby*c(1, -1) } else {
    ul <- ul + padby*c(1, -1) }
  if(missing(lr)) {
    lr <- c(min(activity@lat), max(activity@lon)) + padby*c(-1, 1) } else {
    lr <- lr*c(1, -1) }

  map <- OpenStreetMap::openmap(ul, lr, zoom = NULL, type = osm_type)

  plot(map, ...)
  lines.activity(activity, lwd = 3)
}


#' plot a Strava activtiy on OpenStreetmap
#'
#' @param x an object of S4 class activity.
#' @param y ANY.
#' @param ul upperleft lat and long.
#' @param lr lowerright lat and long.
#' @param osm_type one of the slot names, "name", "type", "lat", "lon", "distances", "elevations", "times".
#' @param padby pad upperleft and lowerright lat long points by this value.
#' @param ... additional arguments for \code{plot}.
#'
#' @export
setMethod(f = "plot",
          signature = c(x = "activity", y = "ANY"),
          definition = function(x, y, ul, lr, osm_type, padby){ plot.activity(x, osm_type, padby) } )


#' plot a list of Strava Strava activities
#'
#' @param lactivity a list of activity objects.
#' @param ul upperleft lat and long.
#' @param lr lowerright lat and long.
#' @param osm_type one of the slot names, "name", "type", "lat", "lon", "distances", "elevations", "times".
#' @param padby pad upperleft and lowerright lat long points by this value.
#'
#' @export
plot_listactivities <- function(lactivity, ul, lr, osm_type = "stamen-watercolor", padby = 0.01, ...) {
  lapply(lactivity, check_activity)

  if(missing(ul) | missing(lr)){
    out <- sapply(lactivity, function(x) {
      c(max(x["lat"]), min(x["lat"]), max(x["lon"]), min(x["lon"])) })
    ul <- c(max(out[1, ]), min(out[4, ])) + padby*c(1, -1)
    lr <- c(min(out[2, ]), max(out[3, ])) + padby*c(-1, 1)
  } else {
    ul <- ul + padby*c(1, -1)
    lr <- lr + padby*c(1, -1)
  }

  map <- OpenStreetMap::openmap(ul, lr, zoom = NULL, type = osm_type)

  plot(map)
  sapply(lactivity, lines.activity)
}
