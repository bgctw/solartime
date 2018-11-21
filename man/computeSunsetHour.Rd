\name{computeSunsetHour}
\alias{computeSunsetHour}
\title{computeSunsetHour}
\description{Compute the hour of sunrise for given day and coordinates}
\usage{computeSunsetHour(timestamp, latDeg, longDeg = NA, 
    timeZone = getHoursAheadOfUTC(timestamp), 
    ...)}
\arguments{
  \item{timestamp}{POSIXt vector}
  \item{latDeg}{Latitude in (decimal) degrees}
  \item{longDeg}{Longitude in (decimal) degrees
(not required if solar time is sufficient)}
  \item{timeZone}{Time zone (in hours) ahead
of UTC (Central Europe is +1) (not required if solar time is sufficient)}
  \item{\dots}{further arguments to \code{\link{computeSunsetHourDoy}}}
}

\value{result of \code{\link{computeSunsetHourDoy}}}

\author{Thomas Wutzler}





