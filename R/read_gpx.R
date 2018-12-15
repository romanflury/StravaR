# read file name and determine activity
files <- list.files(path = "activities/")

get_activitytype <- function(file) {
  dotgpx <- strsplit(file, "-")[[1]][3]
  activity <- strsplit(dotgpx, ".gpx")[[1]][1]

  return(activity)
}

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

  result <- new("activity", type = get_activitytype(file), name = activity_name, lat = lat,
                lon = lon, distances = activity_dists, elevations = ele, times = activity_times)

  return(result)
}

load_allactivities <- function(path = "activities/", type = "all", merge = FALSE, ...) {
  files <- list.files(path)

  # remove not .gpx files
  files <- base::grep(".gpx", files, value = TRUE)

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


