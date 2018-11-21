\name{computeIsDayByHour}
\alias{computeIsDayByHour}
\title{computeIsDayByHour}
\description{tell for each date, whether its daytime}
\usage{computeIsDayByHour(date, sunriseHour = 7, 
    sunsetHour = 18, duskOffset = 0)}
\arguments{
  \item{date}{POSIXct vector}
  \item{sunriseHour}{sunrise as fractional hour (0..24)
(vector of length date or length 1)}
  \item{sunsetHour}{sunset as fractional hour
(vector of length date or length 1)}
  \item{duskOffset}{integer scalar: time in hours after dusk for
which records are still regarded as day}
}

\value{logical vector (length(date)): true if its daytime}

\author{Thomas Wutzler}





