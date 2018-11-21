\name{computeSunriseHour}
\alias{computeSunriseHour}
\title{computeSunriseHour}
\description{Compute the hour of sunrise for given day and coordinates}
\usage{computeSunriseHour(timestamp, latDeg, longDeg = NA, 
    timeZone = getHoursAheadOfUTC(timestamp), 
    ...)}
\arguments{
  \item{timestamp}{POSIXt vector}
  \item{latDeg}{Latitude in (decimal) degrees}
  \item{longDeg}{Longitude in (decimal) degrees
(not required if solar time is sufficient)}
  \item{timeZone}{Time zone (in hours) ahead
of UTC (Central Europe is +1) (not required if solar time is sufficient)}
  \item{\dots}{further arguments to \code{\link{computeSunriseHourDoy}}}
}

\value{result of \code{\link{computeSunriseHourDoy}}}

\author{Thomas Wutzler}





