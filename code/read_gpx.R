# read file name and determine activity
files <- list.files(path = "activities/")

get_activity <- function(file) {
  dotgpx <- strsplit(file, "-")[[1]][3]
  activity <- strsplit(dotgpx, ".gpx")[[1]][1]

  return(activity)
}

## test get_activity
# table(sapply(files, get_activity))
#---------------------------------------------------------------------------------- #

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

  result <- new("activity", type = get_activity(file), name = activity_name, lat = lat,
                lon = lon, distances = activity_dists, elevations = ele, times = activity_times)

  return(result)
}

# test <- parse_gpx(file = "activities/20160420-034316-Ride.gpx")
