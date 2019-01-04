# utility function
#' @importFrom OpenStreetMap osm
#' @importFrom sp coordinates proj4string CRS spTransform
#' @importFrom stats setNames
latlon2osm <- function(lat, lon) {
  locs <- data.frame(lat = lat, lon = lon)
  sp::coordinates(locs) <- ~lon+lat
  sp::proj4string(locs) <- sp::CRS("+init=epsg:4326")
  locs.proj <- data.frame(sp::spTransform(locs, OpenStreetMap::osm()))
  locs.proj <- stats::setNames(locs.proj, c("lat", "lon"))

  return(locs.proj)
}


#' taking lat long coordinates of an object of S4 class activity and joining the corresponding points with line segments.
#'
#' @param activity an object of S4 class activity.
#' @param ... further arguments, see \code{\link{lines}}.
#'
#' @export
#'
#' @importFrom graphics lines
lines_activity <- function(activity, ...) {
  check_activity(activity)
  route <- latlon2osm(lon = activity@lon, lat = activity@lat)
  lines(y = route$lon, x = route$lat, ...)
}

#' plot a Strava activtiy on OpenStreetmap
#'
#' @param x an object of S4 class activity.
#'
#' @exportMethod lines
#' @docType methods
#' @aliases lines,activity,activity-method
setMethod(f = "lines",
          signature = c(x = "activity"),
          definition = function(x){ lines_activity(x) } )


#' add a circle to provide some privacy
#'
#' @param lat a vector of lattitude coordinates.
#' @param lon a vector of longuitude coordinates.
#' @param jitter a rondom jitter, such that the center of the circle is not at the exact location to protect.
#' @param inch determines the circle size.
#' @param col circle color.
#'
#' @export
#'
#' @importFrom graphics symbols
#' @importFrom stats runif
privatearea <- function(lat, lon, jitter = 0.00001, inch = 1/10, col = "white") {
  point <- latlon2osm(lon = lon + runif(length(lon), -jitter, jitter), lat = lat + runif(length(lat), -jitter, jitter))


  apply(point, MARGIN = 1, function(x) { symbols(x[1], x[2], circles = 1, add = TRUE, inches = inch, bg = col) })
}



#' plot an activity, using OpenStreetMap.
#'
#' @inheritParams lines_activity
#' @param ul upperleft lat and long.
#' @param lr lowerright lat and long.
#' @param osm_type openstreetmap type, see \code{\link{openmap}} for possible values.
#' @param padby pad upperleft and lowerright lat long points by this value.
#' @param ... additional arguments for \code{\link{plot.OpenStreetMap}}.
#' @param lwd line width.
#' @param lty line type.
#' @param lcol line color
#'
#' @export
#'
#' @importFrom OpenStreetMap openmap
#' @importFrom graphics plot
plot_activity <- function(activity, ul, lr, osm_type = "stamen-watercolor", padby = 0.01,
                          lwd = 3, lty = 1, lcol = "black", ...) {
  check_activity(activity)

  if(missing(ul)) {
    ul <- c(max(activity@lat), min(activity@lon)) + padby*c(1, -1) } else {
    ul <- ul + padby*c(1, -1) }
  if(missing(lr)) {
    lr <- c(min(activity@lat), max(activity@lon)) + padby*c(-1, 1) } else {
    lr <- lr*c(1, -1) }

  map <- OpenStreetMap::openmap(ul, lr, zoom = NULL, type = osm_type)

  plot(map, ...)
  lines_activity(activity, lwd = lwd, lty = lty, col = lcol)
}


#' plot a Strava activtiy on OpenStreetmap
#'
#' @param x an object of S4 class activity.
#' @param y missing.
#' @param ul upperleft lat and long.
#' @param lr lowerright lat and long.
#' @param osm_type openstreetmap type, see \code{\link{openmap}} for possible values.
#' @param padby pad upperleft and lowerright lat long points by this value.
#' @param ... additional arguments for \code{plot}.
#'
#' @exportMethod plot
#' @docType methods
#' @aliases plot,activity,activity-method
setMethod(f = "plot",
          signature = c(x = "activity", y = "missing"),
          definition = function(x, y, ul, lr, osm_type, padby){ plot_activity(x, osm_type, padby) } )


#' plot a list of Strava Strava activities
#'
#' @param lactivity a list of activity objects.
#' @inheritParams plot_activity
#'
#' @export
plot_listactivities <- function(lactivity, ul, lr, osm_type = "stamen-watercolor", padby = 0.01,
                                lwd = 3, lty = 1, lcol = "black", ...) {
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

  plot(map, ...)

  sapply(lactivity, lines_activity, lwd = lwd, lty = lty, col = lcol)
}
