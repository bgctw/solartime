\name{computeDayLengthDoy}
\alias{computeDayLengthDoy}
\title{computeDayLengthDoy}
\description{Compute the Day-length in hours for given time and coordinates}
\usage{computeDayLengthDoy(doy, latDeg)}
\arguments{
  \item{doy}{integer vector with day of year [DoY, 1..366],
same length as Hour or length 1}
  \item{latDeg}{Latitude in (decimal) degrees}
}

\value{numeric vector of length(doy) giving the
time between surise and sunset in hours}

\author{Thomas Wutzler}




\examples{
doy <- 1:366
plot( computeDayLengthDoy(doy, latDeg = 51) ~ doy)
# north pole: daylength 0 and 24 hours
plot( computeDayLengthDoy( doy, latDeg = +80) ~ doy )
plot( computeDayLengthDoy( doy, latDeg = -80) ~ doy )
}
