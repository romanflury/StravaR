#' An S4 class to represent a Strava activity.
#'
#' @slot name the name of the Strava activity (character).
#'       By default \code{name = NA_character_}.
#' @slot type the type of the Strava activity (character), like "Ride", "Run", etc.
#'       By default \code{type = NA_character_}.
#' @slot lat a numeric vector, containing the recorded lattidue coordinates.
#'       By default \code{lat = NULL}.
#' @slot lon a numeric vector, containing the recorded longitude coodinates.
#'       By default \code{lon = NULL}.
#' @slot distances a numeric vector containing the distances between the lat/lon coodrinates in meter.
#'       Starts at zero.
#'       By default \code{distances = NULL}.
#' @slot elevations a numeric vector conaining the elevation in meters, for every recorded point.
#'       By default \code{elevations = NULL}.
#' @slot times a numeric vector containing the time differences between recorded points.
#'       Starts at zero.
#'       By default \code{times = NULL}.
#'
#' @export
setClass("activity",
         representation(name = "character", type = "character", lat = "numeric", lon = "numeric",
                        distances = "numeric", elevations = "numeric", times = "POSIXct"),
         prototype(name = NA_character_, type = NA_character_, lat = NULL, lon = NULL,
                   distances = NULL, elevations = NULL, times = NULL))

#' Definition of the subset operator for an object of S4 class activity.
#'
#' @param x an object of S4 class activity.
#' @param i one of the slot names, "name", "type", "lat", "lon", "distances", "elevations", "times".
#' @return the respective slot entries.
#'
#' @export
setMethod(f = "[",
          signature = "activity",
          definition = function(x, i){
            if (identical(i, "name"))       { return(x@name) }
            if (identical(i, "type"))       { return(x@type) }
            if (identical(i, "lat"))        { return(x@lat) }
            if (identical(i, "lon"))        { return(x@lon) }
            if (identical(i, "distances"))  { return(x@distances) }
            if (identical(i, "elevations")) { return(x@elevations) }
            if (identical(i, "times"))      { return(x@times) } }
)

# utility function
check_activity <- function(object) {
  if (class(object) != "activity")
    stop("invalid class for argument \"activity\"")
}
