% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot_activity}
\alias{plot_activity}
\title{plot an activity, using OpenStreetMap.}
\usage{
plot_activity(activity, ul, lr, osm_type = "stamen-watercolor",
  padby = 0.01, lwd = 3, lty = 1, lcol = "black", ...)
}
\arguments{
\item{activity}{an object of S4 class activity.}

\item{ul}{upperleft lat and long.}

\item{lr}{lowerright lat and long.}

\item{osm_type}{openstreetmap type, see \code{\link{openmap}} for possible values.}

\item{padby}{pad upperleft and lowerright lat long points by this value.}

\item{lwd}{line width.}

\item{lty}{line type.}

\item{lcol}{line color}

\item{...}{additional arguments for \code{\link{plot.OpenStreetMap}}.}
}
\description{
plot an activity, using OpenStreetMap.
}
