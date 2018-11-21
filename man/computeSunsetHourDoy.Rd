\name{computeSunsetHourDoy}
\alias{computeSunsetHourDoy}
\title{computeSunsetHourDoy}
\description{Compute the hour of sunrise for given day and coordinates}
\usage{computeSunsetHourDoy(doy, latDeg, longDeg = NA, 
    timeZone = NA, isCorrectSolartime = TRUE)}
\arguments{
  \item{doy}{integer vector with day of year [DoY, 1..366]}
  \item{latDeg}{Latitude in (decimal) degrees}
  \item{longDeg}{Longitude in (decimal) degrees
(not required if solar time is sufficient)}
  \item{timeZone}{Time zone (in hours) ahead
of UTC (Central Europe is +1) (not required if solar time is sufficient)}
  \item{isCorrectSolartime}{sunrise hour is computed first for solar time
(where noon is exactly at 12:00)
If TRUE (default) then sunrise hour is converted to local winter time,
based on timeZone and longitude.}
}

\value{numeric vector of length(doy) giving the time of sunset
in hours after midnight.
Polar night is indicated by 12h, polar day by 24h.}

\author{Thomas Wutzler}




\examples{
today <-
  as.POSIXlt(Sys.Date())$yday
(sunset <- computeSunsetHourDoy(today, latDeg = 51, isCorrectSolartime = FALSE))
(sunset <- computeSunsetHourDoy(today, latDeg = 51, longDeg = 11.586, timeZone = +1))
#
doy <- 1:366
plot( computeSunsetHourDoy(doy, latDeg = 51, isCorrectSolartime = FALSE) ~ doy )
# north pole: daylength 0 and 24 hours
plot( computeSunsetHourDoy( doy, latDeg = +80, isCorrectSolartime = FALSE) ~ doy )
plot( computeSunsetHourDoy( doy, latDeg = -80, isCorrectSolartime = FALSE) ~ doy )
}
