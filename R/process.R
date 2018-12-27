#' Calculate the time difference between measured points in an activity
#'
#' @param activity an object of S4 class activity.
#' @return a numeric vecotor of the corresponding time differences.
#'
#' @export
get_timediffs <- function(activity) {
  check_activity(activity)

  activity_timediffs <- sapply(1:(length(activity@times)-1), function(x) {
    as.numeric(difftime(activity@times[x+1], activity@times[x], units = "hours")) })

  return(activity_timediffs)
}

#' Calculate the velocities (km/h) of an activity for every measured point.
#' Using the distance devided by \code{\link{get_timediffs}}.
#'
#' @param activity an object of S4 class activity.
#' @return the respective velocities.
#'
#' @export
get_speeds <- function(activity) {
  check_activity(activity)

  return(activity@distances[-1]/get_timediffs(activity))
}

#' Catecorize the slot elevations from an S4 activity object into different levels.
#'
#' @param activity an object of S4 class activity.
#' @param by numeric value to devide the elevations in meters.
#'        By default \code{by = 100}.
#' @param levels numeric vector to determine the corresponding levels.
#'        By default \code{levels = 3:30}.
#' @return the transfromed elevations slot.
#'
#' @export
get_elevationlevel <- function(activity, by = 100, levels = 3:30) {
  check_activity(activity)

  return(factor(round(activity@elevations/by), levels = levels))
}

#' Determine the country(ies) of the lat/lon coordinates of a Strava activity
#'
#' @param activity an object of S4 class activity.
#' @return character vector of the countries.
#'
#' @export
#'
#' @importFrom rworldmap getMap
#' @importFrom sp SpatialPoints
#' @importFrom sp CRS
#' @importFrom sp proj4string
#' @importFrom sp over
get_countries <- function(activity) {
  check_activity(activity)

  locations <- cbind(activity@lon, activity@lat)
  countriesSP <- rworldmap::getMap(resolution = "low")

  pointsSP = sp::SpatialPoints(coords = locations, proj4string = sp::CRS(sp::proj4string(countriesSP)))
  indices = sp::over(pointsSP, countriesSP)

  return(unique(indices$ADMIN))
}

#' Merge different objects of S4 class activtiy into one.
#'
#' @param list_activity a list of activty objects.
#' @param name the name of the merged activity.
#'        By default \code{name = "merged-activity"}.
#' @return an object of S4 class activtiy.
#'
#' @export
merge_activity <- function(list_activity, name = "merged-activity") {
  sapply(list_activity, check_activity)

  types <- unlist(sapply(list_activity, function(x) { x["type"] }))
  if(!all(types == types[1])) {
    warning(paste("the activities merged are not all of the same type:", as.character(types[1]), "is set")) }

  nactivities <- length(list_activity)
  v_nmeasurements <- c(1, sapply(list_activity, function(x) { length(x@lat) }))
  v_cumnmeas <- cumsum(v_nmeasurements)
  newlat <- newlon <- newdist <- newelevations <- vector(mode = "numeric",
                                                         length = v_cumnmeas[nactivities+1]-1)
  newtimes <- .POSIXct(character(v_cumnmeas[nactivities+1]-1))

  c_activityslot <- function(x, slot = character(), mode = "numeric", l = v_cumnmeas[nactivities+1]-1) {
    if (identical(mode, "numeric")) {
      tmp <- vector(mode, l) }
    if (identical(mode, "POSIXct")) {
      tmp <- .POSIXct(character(l)) }
    return(tmp[v_cumnmeas[x]:(v_cumnmeas[x+1]-1)] <- list_activity[[x]][slot])
  }

  newlat <- unlist(sapply(1:nactivities, c_activityslot, slot = "lat"))
  newlon <- unlist(sapply(1:nactivities, c_activityslot, slot = "lon"))
  newdist <- unlist(sapply(1:nactivities, c_activityslot, slot = "distances"))
  newelevations <- unlist(sapply(1:nactivities, c_activityslot, slot = "elevations"))
  newtimes <- unlist(sapply(1:nactivities, c_activityslot, slot = "times", mode = "POSIXct"))
  attributes(newtimes) <- list(class = "POSIXct", tzone = "GMT")

  return(new("activity", type = types[1], name = name, lat = newlat,
             lon = newlon, distances = newdist, elevations = newelevations,
             times = newtimes))
}
