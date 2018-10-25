\name{solartime-package}

\alias{solartime-package}
\alias{solartime}

\title{solar time utilities.}

\description{
Provide utilities to work with solar time,
i.e. where noon is exactly when sun culminates.
Provides functions for computing sun position and times of sunrise and sunset.
}

\details{

Most fundamental funcions are
\itemize{
  \item corrected fractional hour \code{\link{getSolarTimeHour}}
    based on \code{\link{computeSolarToLocalTimeDifference}}
  \item computing position of the sun \code{\link{computeSunPosition}}
} %itemize

On this basis, properties are computed such as
\itemize{
  \item hour of sunrise and sunset:
       \code{\link{computeSunriseHour}},\code{\link{computeSunriseHour}}
  \item daylength in hours: \code{\link{computeDayLength}}
  \item flagging times as day or night:
     \code{\link{computeIsDayByHour}} and
     \code{\link{computeIsDayByLocation}} and
 } %itemize

More utils provide
\itemize{
\item get the hours ahead UTC: \code{\link{getHoursAheadOfUTC}}
\item get fractional hour of the day: \code{\link{getFractionalHours}}
}%itemize


Also have a look at the \href{../doc/index.html}{package vignettes}.
}%details

\author{Thomas Wutzler}



\keyword{ package }





