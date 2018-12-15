setClass("activity",
         representation(type = "character", name = "character", lat = "numeric", lon = "numeric",
                        distances = "numeric", elevations = "numeric", times = "POSIXct"),
         prototype(type = NA_character_, name = NA_character_, lat = NULL, lon = NULL,
                   distances = NULL, elevations = NULL, times = NULL))

setMethod(f = "[",
          signature = "activity",
          definition=function(x,i,j,drop){
            if (identical(i, "type"))       { return(x@type) }
            if (identical(i, "name"))       { return(x@lat) }
            if (identical(i, "lat"))        { return(x@lat) }
            if (identical(i, "lon"))        { return(x@lon) }
            if (identical(i, "distances"))  { return(x@distances) }
            if (identical(i, "elevations")) { return(x@elevations) }
            if (identical(i, "times"))      { return(x@times) } }
)
