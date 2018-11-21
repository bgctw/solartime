\name{computeIsDayByLocation}
\alias{computeIsDayByLocation}
\title{computeIsDayByLocation}
\description{tell for each timestamp, whether its daytime}
\usage{computeIsDayByLocation(timestamp, latDeg, 
    longDeg, timeZone = getHoursAheadOfUTC(timestamp), 
    duskOffset = 0, isCorrectSolartime = TRUE)}
\arguments{
  \item{timestamp}{POSIXct vector}
  \item{latDeg}{Latitude in (decimal) degrees}
  \item{longDeg}{Longitude in (decimal) degrees}
  \item{timeZone}{Time zone (in hours)
ahead of UTC (Central Europ is +1)}
  \item{duskOffset}{integer scalar: time in hours after dusk for
which records are still regarded as day}
  \item{isCorrectSolartime}{set to FALSE to omit correction between
local time and solar time, e.g. if coordinates cannot be provided}
}
\details{computes hour of sunrise and sunset from given date in timezone
hour (assuming dates are given in timezoen instead of solartime)}
\value{logical vector (length(date)): true if its daytime}

\author{Thomas Wutzler}




\examples{
dateSeq <- seq( as.POSIXct("2017-03-20", tz = "Etc/GMT-1")
                ,as.POSIXct("2017-03-21", tz = "Etc/GMT-1")
                , by = "30 min")
tmp <- computeIsDayByLocation(
  dateSeq, latDeg = 50.93, longDeg = 11.59, timeZone = 1)
plot( tmp ~ dateSeq )
yday <- as.POSIXlt(dateSeq[1])$yday + 1L
sunrise <- computeSunriseHourDoy(
  yday, latDeg = 50.93, longDeg = 11.59, timeZone = 1)
sunset <- computeSunsetHourDoy(
  yday, latDeg = 50.93, longDeg = 11.59, timeZone = 1)
abline( v = trunc(dateSeq[1], units = "days") + c(sunrise,sunset)*3600L )
}
