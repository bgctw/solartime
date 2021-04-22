\name{computeSolarToLocalTimeDifference}
\alias{computeSolarToLocalTimeDifference}
\title{computeSolarToLocalTimeDifference}
\description{computes the time difference in hours between (apparent) solar time and local time}
\usage{computeSolarToLocalTimeDifference(longDeg, 
    timeZone, doy = NA, fracYearInRad = 2 * 
        pi * (doy - 1)/365.24)}
\arguments{
  \item{longDeg}{Longitude in (decimal) degrees}
  \item{timeZone}{Time zone (in hours) ahead of UTC (Berlin is +1)}
  \item{doy}{integer vector with day of year [DoY, 1..366],
Specify NA get mean solar time across the year instead of apparent solar
time (i.e. with differences throughout the year due to eccentricity
of earth orbit)}
  \item{fracYearInRad}{may specify instead
of doy for efficiency.}
}

\value{time difference in hours to be added to local winter time
to get solar time}

\author{Thomas Wutzler}




\examples{
# Jena: 50.927222, 11.586111
longDeg <- 11.586
doi <- 1:366
# due to longitude: west of timezone meridian: sun culminates later,
# solar time is less than local time
(localDiff <- computeSolarToLocalTimeDifference(longDeg, 1L)*60)
# taking into account shift during the year due to earth orbit eccentricity
plot( computeSolarToLocalTimeDifference(longDeg, 1L, doi)*60 ~ doi )
abline(h = localDiff)
}
