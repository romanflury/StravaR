# rm(list = ls())
# options(device = "X11"); graphics.off()
# 
# source("code/read_gpx.R")
# 
# test <- parse_gpx(file = "activities/20160420-034316-Ride.gpx")

get_timediffs <- function(activity) {
  if (class(activity) != "activity")
    stop("invalid class for argument \"activity\"")

  activity_timediffs <- sapply(1:(length(activity@times)-1), function(x) {
    as.numeric(difftime(activity@times[x+1], activity@times[x], units = "hours")) })

  return(activity_timediffs)
}

get_speeds <- function(activity) {
  if (class(activity) != "activity")
    stop("invalid class for argument \"activity\"")

  return(activity@distances[-1]/get_timediffs(activity))
}

get_elevationlevel <- function(activity) {
  if (class(activity) != "activity")
    stop("invalid class for argument \"activity\"")

  return(factor(round(test@elevations/100), levels = 3:30))
}


