#' Determine the activity type, based on the Strava gpx file title.
#' @export
#'
#' @param file a .gpx file from Strava.
#' @return the activity type, as character.
#'
#' @export
get_activitytype <- function(file) {
  dotgpx <- strsplit(file, "-")[[1]][3]
  activity <- strsplit(dotgpx, ".gpx")[[1]][1]

  return(activity)
}

#' Read and parse .gpx files from Strava.
#'
#' An object of S4 class activity is created.
#' Thereby, it is assumed, that the corresponding gpx file consists of one Strava activity.
#'
#' @param file a .gpx file from Strava.
#' @return an object of S4 class activity, containing the slots: name, type, lat, lon, distances, elevations, times.
#'
#' @export
#' @importFrom methods new
#' @importFrom XML htmlTreeParse
#' @importFrom XML xpathSApply
#' @importFrom XML xmlValue
#' @importFrom XML xmlAttrs
#' @importFrom sp spDists
parse_gpx <- function(file) {
  # read gpx file and decompose XML tree
  gpxfile <- XML::htmlTreeParse(file, useInternalNodes = TRUE)

  activity_name <- XML::xpathSApply(gpxfile, path = "//name", XML::xmlValue)

  coordinates <- XML::xpathSApply(gpxfile, path = "//trkpt", XML::xmlAttrs)
  # check that there are at least two coordinates.
  if (identical(length(coordinates), 0) || ncol(coordinates) < 2) { return(NULL) }

  lat <- as.numeric(coordinates["lat", ])
  lon <- as.numeric(coordinates["lon", ])

  ele <- as.numeric(XML::xpathSApply(gpxfile, path = "//trkpt/ele", XML::xmlValue))
  time <- XML::xpathSApply(gpxfile, path = "//trkpt/time", XML::xmlValue)

  activity_dists <- c(0, sp::spDists(x = cbind(lon, lat), longlat = TRUE, segments = TRUE))
  activity_times <- as.POSIXct(time, tz = "GMT", format = "%Y-%m-%dT%H:%M:%OS")

  result <- methods::new("activity", name = activity_name, type = get_activitytype(file),
                         lat = lat, lon = lon, distances = activity_dists, elevations = ele,
                         times = activity_times)

  return(result)
}

#' Load Strava activities from gpx files
#'
#' @param path the path from de current working directory to a directory containing the gpx files from Strava.
#' @param type to restrict to a certain type of activities, like \code{"Ride"}, \code{"Run"}, etc.
#'        By default \code{type = "all"}.
#' @param merge if TRUE, only an object of S4 class activity is returned, otherwise a list of activity object.
#'        By default \code{merge = FALSE}.
#' @param ... further arguments passed to the function \code{merge_activity} (only considered if \code{merge = TRUE}).
#' @return either a list or a single activity object.
#'
#' @export
load_activities <- function(path = "activities/", type = "all", merge = FALSE, ...) {
  files <- list.files(path)

  # remove not .gpx files
  files <- grep(".gpx", files, value = TRUE)

  # select for a certain activity type
  if(!identical(type, "all")) {
    types <- unlist(sapply(files, get_activitytype))
    files <- files[which(types == type)] }

  # it is assumed, that each .gpx file defines one activity
  list_activities <- sapply(files, function(x) {
    tmppath <- paste0(path, x)
    parse_gpx(tmppath) })

  if (!merge)
    return(list_activities)

  return(merge_activity(list_activities, ...))
}

#' @export
pload_activities <- function(path = "activities/", type = "all", merge = FALSE, ...) {
  files <- list.files(path)

  # remove not .gpx files
  files <- grep(".gpx", files, value = TRUE)

  # select for a certain activity type
  if(!identical(type, "all")) {
    types <- unlist(sapply(files, get_activitytype))
    files <- files[which(types == type)] }

  no_cores <- parallel::detectCores() - 1
  # Initiate cluster
  cl <- parallel::makeCluster(no_cores)
  parallel::clusterExport(cl, c("files", "path"), envir = environment())

  # it is assumed, that each .gpx file defines one activity
  list_activities <- parallel::parSapply(cl, files, function(x) {
    tmppath <- paste0(path, x)
    parse_gpx(tmppath) })

  if (!merge)
    return(list_activities)

  return(merge_activity(list_activities, ...))
}



#
#
# test <- sapply((lambda+1):(nn - lambda),
#                function(x) {
#                  parallel::parSapply(cl, (lambda+1):(mm - lambda),
#                                      function(y) {
#                                        xtmp <- seq(x-lambda, x+lambda)
#                                        ytmp <- seq(y-lambda, y+lambda)
#                                        tmp <- sampledata[xtmp, ytmp, ]
#                                        tmpM <- cbind(c(tmp[,,1])[c(m_radiusindices)], c(tmp[,,2])[c(m_radiusindices)], c(tmp[,,3])[c(m_radiusindices)])
#                                        tmpM <- tmpM[!rowSums(!is.finite(tmpM)),]               # only lines w/o NAs
#
#                                        switch (index,
#                                                "mean" = tryCatch(mean(tmpM, na.rm = TRUE), error = function(e) { return(NA) } ),
#                                                "richness" = tryCatch(geometry::convhulln(tmpM, options = 'FA')$vol, error = function(e) { return(NA) } ),
#                                                "evenness" = tryCatch(FEve(tmpM), error = function(e) { return(NA) }),
#                                                "divergence" = tryCatch(FDiv(tmpM), error = function(e) { return(NA) }) )
#                                      }) }, simplify = TRUE)
# parallel::stopCluster(cl)