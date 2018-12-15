
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

get_elevationlevel <- function(x, by = 100, levels = 3:30) {
  return(factor(round(x@elevations/by), levels = levels))
}

get_countries <- function(activity) {
  if (class(activity) != "activity")
    stop("invalid class for argument \"activity\"")

  locations <- cbind(activity@lon, activity@lat)
  countriesSP <- rworldmap::getMap(resolution = "low")

  pointsSP = sp::SpatialPoints(coords = locations, proj4string = sp::CRS(sp::proj4string(countriesSP)))
  indices = sp::over(pointsSP, countriesSP)

  return(unique(indices$ADMIN))
}

merge_activity <- function(list_activity, name = "merged-activity") {
  sapply(list_activity, function(x) { if(class(x) != "activity") {
    stop("invalid class for list entry in \"list_activity\"") } })

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

    return(tmp[v_cumnmeas[x]:(v_cumnmeas[x+1]-1)] <- list_activity[[x]][slot]) }

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



