\name{setLocalTimeZone}
\alias{setLocalTimeZone}
\title{setLocalTimeZone}
\description{modify tzone attribute of timestamp to 'GMT+x' for local to given longitude}
\usage{setLocalTimeZone(timestamp, longDeg)}
\arguments{
  \item{timestamp}{POSIXct}
  \item{longDeg}{Longitude in (decimal) degrees}
}

\value{\code{timestamp} with modified tzone attribute. Its the same time
point expressed in another time zone. E.g. "2019-04-04 00:00:00 UTC"
becomes "2019-04-04 10:00:00 +10" for a longitude of +150 (Sydney, Australia)}

\author{Thomas Wutzler}





