\name{computeSunPositionDoyHour}
\alias{computeSunPositionDoyHour}
\title{computeSunPositionDoyHour}
\description{Compute the position of the sun (solar angle)}
\usage{computeSunPositionDoyHour(doy, hour, latDeg, 
    longDeg = NA, timeZone = NA, isCorrectSolartime = TRUE)}
\arguments{
  \item{doy}{integer vector with day of year
[DoY, 1..366], same length as Hour or length 1}
  \item{hour}{numeric vector with local winter time
as decimal hour [0..24)}
  \item{latDeg}{Latitude in (decimal) degrees}
  \item{longDeg}{Longitude in (decimal) degrees}
  \item{timeZone}{Time zone (in hours) ahead of UTC
(Central Europe is +1)}
  \item{isCorrectSolartime}{by default corrects hour
(given in local winter time) for latitude to solar time
(where noon is exactly at 12:00). Set this to FALSE if times are
specified already as solar times.}
}
\details{This code assumes that Hour is given in local winter time zone.
By default, it corrects by longitude to solar time (where noon
is exactly at 12:00).
Set argument \code{isCorrectSolartime} to FALSE to use the given
local winter time instead.}
\value{named numeric matrix with one row for each time with entries
\item{hour}{Solar time in fractional hours after
midnight, (or given hour if isCorrectSolartime = FALSE).}
\item{declination}{Solar declination (rad)}
\item{elevation}{Solar elevation (rad)
with 0 at horizon increasing towards zenith}
\item{azimuth}{Solar azimuth (rad)
with 0 at North increasing eastwards
}}

\author{Thomas Wutzler}




\examples{
computeSunPositionDoyHour(
  160, hour = 0:24, latDeg = 51, longDeg = 13.6, timeZone = 1L)
}
