#' @export
computeSunPosition <- function(
  ### Calculate the position of the sun
  timestamp          ##<< POSIXct
  , latDeg           ##<< Latitude in (decimal) degrees
  , longDeg          ##<< Longitude in (decimal) degrees
) {
  doy = yday(timestamp) #as.POSIXlt(timestamp)$yday + 1  ##<<
  ## Data vector with day of year (DoY) starting at 1
  ## , same length as Hour or length 1
  hour = getFractionalHours(timestamp)      ##<<
  ## Data vector with time as fractional decimal hour of local time zone
  timeZone = getHoursAheadOfUTC(timestamp) ##<< Time zone (in hours)
  ##value<< as returned by \code{\link{computeSunPositionDoyHour}}
  computeSunPositionDoyHour(doy, hour, latDeg, longDeg, timeZone)
}

#' @export
getHoursAheadOfUTC <- function(
  ### get the time difference to UTC in hours
  timestamp  ##<< POSIXt vector
){
  tUTC <- force_tz(timestamp, tzone = "UTC")
  ##value<< integer vector of how many hours noon of timestamp is ahead of
  ## noon in UTC
  as.integer((as.numeric(tUTC) - as.numeric(timestamp))/3600)
}

#' @export
getFractionalHours <- function(
  ### get the time difference to previous midnight in fractional hours
  timestamp  ##<< POSIXt vector
){
  ##value<< numeric vector of fractional hours
  (as.numeric(timestamp) -
     as.numeric(floor_date(timestamp, unit = "days"))) / 3600
}


#' @export
computeSunriseHour <- function(
  ### Compute the hour of sunrise for given day and coordinates
  doy	##<< integer vector with day of year [DoY, 1..366],
  ## same length as Hour or length 1
  , latDeg		                    ##<< Latitude in (decimal) degrees
  , longDeg=NA	                  ##<< Longitude in (decimal) degrees
  ## (not required if solar time is sufficient)
  , timeZone=NA               ##<< Time zone (in hours) ahead
  ## of UTC (Central Europe is +1) (not required if solar time is sufficient)
  , isCorrectSolartime = TRUE	##<< sunrise hour is computed first for solar time
  ## (where noon is exactly at 12:00)
  ## If TRUE (default) then sunrise hour is converted to local winter time,
  ## based on timeZone and longitude.
){
  if (isCorrectSolartime & any(!is.finite(c(longDeg, timeZone)))) stop(
    "if isCorrectSolartime, one needs to provide finite longDeg and timeZone")
  fracYearInRad <- 2 * pi * (doy - 1) / 365.24
  solDeclRad <- (
    (0.33281 - 22.984*cos(fracYearInRad)
     - 0.34990*cos(2*fracYearInRad) - 0.13980*cos(3*fracYearInRad)
     + 3.7872*sin(fracYearInRad) + 0.03205*sin(2*fracYearInRad)
     + 0.07187*sin(3*fracYearInRad))/180*pi )
  # compute time in radians
  # solved equation for SolElevRad in computeSunPositionDoyHour for elevation == 0
  # , i.e. sunrise
  solTimeRad <- {
    cosSolTimeRad <- pmax(-1, pmin(
      +1,(sin(solDeclRad) * sin(latDeg/180*pi)) /
        (cos(solDeclRad) * cos(latDeg/180*pi))
    ))
    acos( cosSolTimeRad )
  }
  #	# sunrise equation cos(solTimeRad) = -tan(latDeg) * tan(decl)
  #	sunriseRad <- {
  #		# https://www.quora.com/How-can-you-calculate-the-length-of-the-day-on-Earth-at-a-given-latitude-on-a-given-date-of-the-year
  #		cosSunriseRad <- -tan(latDeg/180*pi) * tan(solDeclRad)
  #		acos(cosSunriseRad)
  #	}
  #	sunsetHour <- 12 - sunriseRad/pi*12
  # convert to hours
  solTimeHour <- solTimeRad/pi*12
  if (!isCorrectSolartime) return( solTimeHour )
  hour <- solTimeHour - computeSolarToLocalTimeDifference(
    longDeg = longDeg
    , timeZone = timeZone, fracYearInRad = fracYearInRad)
  ##value<< numeric vector of length(doy) giving the time of sunrise
  ##in hours after midnight.
  ## Polar night is indicated by 12h, polar day by 0h.
  hour
}
attr(computeSunriseHour,"ex") <- function(){
  today <-
    as.POSIXlt(Sys.Date())$yday
  (sunrise <- computeSunriseHour(today, latDeg = 51, isCorrectSolartime = FALSE))
  (sunrise <- computeSunriseHour(today, latDeg = 51, longDeg = 11.586, timeZone = +1))
  # elevation near zero
  computeSunPositionDoyHour(160, sunrise, latDeg = 51, isCorrectSolartime = FALSE)
  #
  doy <- 1:366
  plot( computeSunriseHour(doy, latDeg = 51, isCorrectSolartime = FALSE) ~ doy )
  # north pole: daylength 0 and 24 hours
  plot( computeSunriseHour( doy, latDeg = +80, isCorrectSolartime = FALSE) ~ doy )
  plot( computeSunriseHour( doy, latDeg = -80, isCorrectSolartime = FALSE) ~ doy )
}

#' @export
computeSunsetHour <- function(
  ### Compute the hour of sunrise for given day and coordinates
  doy	##<< integer vector with day of year [DoY, 1..366],
  ## same length as Hour or length 1
  , latDeg		                    ##<< Latitude in (decimal) degrees
  , longDeg=NA	                  ##<< Longitude in (decimal) degrees
  ## (not required if solar time is sufficient)
  , timeZone=NA               ##<< Time zone (in hours) ahead
  ## of UTC (Central Europe is +1) (not required if solar time is sufficient)
  , isCorrectSolartime = TRUE	##<< sunrise hour is computed first for solar time
  ## (where noon is exactly at 12:00)
  ## If TRUE (default) then sunrise hour is converted to local winter time,
  ## based on timeZone and longitude.
){
  if (isCorrectSolartime & any(!is.finite(c(longDeg, timeZone)))) stop(
    "if isCorrectSolartime, one needs to provide finite longDeg and timeZone")
  # compute solar sunrise hour, that one is symmetric around noon
  sunriseSolarHour <- computeSunriseHour(
    doy, latDeg = latDeg, isCorrectSolartime = FALSE)
  sunsetSolarHour <- 24 - sunriseSolarHour
  sunsetHour <- if (isCorrectSolartime) {
    hourDiff <- computeSolarToLocalTimeDifference(
      longDeg, timeZone, doy = doy)
    sunsetSolarHour - hourDiff
  } else sunsetSolarHour
  ##value<< numeric vector of length(doy) giving the time of sunset
  ##in hours after midnight.
  ## Polar night is indicated by 12h, polar day by 24h.
  sunsetHour
}
attr(computeSunsetHour,"ex") <- function(){
  today <-
    as.POSIXlt(Sys.Date())$yday
  (sunset <- computeSunsetHour(today, latDeg = 51, isCorrectSolartime = FALSE))
  (sunset <- computeSunsetHour(today, latDeg = 51, longDeg = 11.586, timeZone = +1))
  #
  doy <- 1:366
  plot( computeSunsetHour(doy, latDeg = 51, isCorrectSolartime = FALSE) ~ doy )
  # north pole: daylength 0 and 24 hours
  plot( computeSunsetHour( doy, latDeg = +80, isCorrectSolartime = FALSE) ~ doy )
  plot( computeSunsetHour( doy, latDeg = -80, isCorrectSolartime = FALSE) ~ doy )
}

#' @export
computeSolarToLocalTimeDifference <- function(
  ### computes the time difference in hours between (apparent) solar time and local time
  longDeg		                  ##<< Longitude in (decimal) degrees
  , timeZone	              ##<< Time zone (in hours) ahead of UTC (Berlin is +1)
  , doy = integer(0)	##<< integer vector with day of year [DoY, 1..366],
  ## Specify NA get mean solar time across the year instead of apparent solar
  ## time (i.e. with differences throughout the year due to eccentricy
  ## of earth orbit)
  , fracYearInRad = 2 * pi * (doy - 1)/365.24 ##<< may specify instead
  ## of doy for efficiency.
){
  # convert solar time to local winter time
  # Equation of time in hours, accounting for changes in the time of solar noon
  # to local time zone
  eqTimeHour <- if (!length(fracYearInRad) || is.na((fracYearInRad))) 0 else
    (0.0072*cos(fracYearInRad) - 0.0528*cos(2*fracYearInRad)
     - 0.0012*cos(3*fracYearInRad) - 0.1229*sin(fracYearInRad)
     - 0.1565*sin(2*fracYearInRad) - 0.0041*sin(3*fracYearInRad))
  # Local time in hours
  localTimeHour <- (longDeg/15 - timeZone)
  ##value<< time difference in hours to be added to local winter time
  ## to get solar time
  localTimeHour + eqTimeHour
}
attr(computeSolarToLocalTimeDifference,"ex") <- function(){
  # Jena: 50.927222, 11.586111
  longDeg <- 11.586
  doi <- 1:366
  # due to longitude: west of timezone meridian: sun culminates later,
  # solar time is less than local time
  (localDiff <- computeSolarToLocalTimeDifference(longDeg, 1L)*60)
  # taking into account shift during the year due to earth orbit eccentricy
  plot( computeSolarToLocalTimeDifference(longDeg, 1L, doi)*60 ~ doi )
  abline(h = localDiff)
}

#' @export
computeDayLength <- function(
  ### Compute the Day-length in hours for given time and coordinates
  doy	      ##<< integer vector with day of year [DoY, 1..366],
  ## same length as Hour or length 1
  , latDeg		        ##<< Latitude in (decimal) degrees
){
  solTimeHour <- computeSunriseHour(
    doy = doy, latDeg = latDeg, isCorrectSolartime = FALSE)
  ##value<< numeric vector of length(doy) giving the
  ## time between surise and sunset in hours
  24 - 2*solTimeHour
}
attr(computeDayLength,"ex") <- function(){
  doy <- 1:366
  plot( computeDayLength(doy, latDeg = 51) ~ doy)
  # north pole: daylength 0 and 24 hours
  plot( computeDayLength( doy, latDeg = +80) ~ doy )
  plot( computeDayLength( doy, latDeg = -80) ~ doy )
}

#' @export
getSolarTimeHour <- function(
  ### Get the fractional hour of solar time
  timestamp      ##<< POSIXt vector in local time
  , longDeg	     ##<< Longitude in (decimal) degrees
){
  doy = yday(timestamp) #as.POSIXlt(timestamp)$yday + 1  ##<<
  ## Data vector with day of year (DoY) starting at 1
  ## , same length as Hour or length 1
  hour = getFractionalHours(timestamp)      ##<<
  ## Data vector with time as fractional decimal hour of local time zone
  timeZone = getHoursAheadOfUTC(timestamp) ##<< Time zone (in hours)
  # Fractional year in radians
  fracYearInRad <- 2 * pi * (doy - 1) / 365.24
  ##value<< fractional hour corrected by difference to local time
  hour + computeSolarToLocalTimeDifference(
      longDeg, timeZone, fracYearInRad = fracYearInRad)
}

#' @export
computeSunPositionDoyHour <- function(
  ### Compute the position of the sun (solar angle)
  doy	                   ##<< integer vector with day of year
  ## [DoY, 1..366], same length as Hour or length 1
  , hour		                   ##<< numeric vector with local winter time
  ## as decimal hour [0..24)
  , latDeg		                 ##<< Latitude in (decimal) degrees
  , longDeg=NA	               ##<< Longitude in (decimal) degrees
  , timeZone=NA                ##<< Time zone (in hours) ahead of UTC
  ## (Central Europ is +1)
  , isCorrectSolartime = TRUE	 ##<< by default corrects hour
  ## (given in local winter time) for latitude to solar time
  ## (where noon is exactly at 12:00). Set this to FALSE if times are
  ## specified already as solar times.
){
  # adapted from REddyProc, credits to Antje Moffat
  # and Alessandro Cescatti's C++ code
  #
  ##details<<
  ## This code assumes that Hour is given in local winter time zone.
  ## By default, it corrects by longitude to solar time (where noon
  ## is exactly at 12:00).
  ## Set argument \code{isCorrectSolartime} to FALSE to use the given
  ## local winter time instead.
  #
  if (isCorrectSolartime & any(!is.finite(c(longDeg, timeZone)))) stop(
    "if isCorrectSolartime, one needs to provide finite longDeg and timeZone")
  # Fractional year in radians
  fracYearInRad <- 2 * pi * (doy - 1) / 365.24
  # Solar time, corrected for local time and equation of time
  solarTimeHour <- if (!isCorrectSolartime ) hour else {
    hour + computeSolarToLocalTimeDifference(
      longDeg, timeZone, fracYearInRad = fracYearInRad)
  }
  # Conversion to radians
  # with correction for solar time < -pi to positive, important
  # for SolAzim_rad.V.n below
  SolTimeRad <- {
    SolTimeRad0 <- (solarTimeHour - 12) * pi / 12.0
    ifelse(SolTimeRad0 < -pi, SolTimeRad0 + 2*pi, SolTimeRad0)
  }
  #Solar declination in radians, accounting for the earth axis tilt
  SolDeclRad <- ((0.33281 - 22.984*cos(fracYearInRad)
                  - 0.34990*cos(2*fracYearInRad) - 0.13980*cos(3*fracYearInRad)
                  + 3.7872*sin(fracYearInRad) + 0.03205*sin(2*fracYearInRad)
                  + 0.07187*sin(3*fracYearInRad))/180*pi )
  # Solar elevation (vertical, zenithal angle) in radians with zero for horizon
  SolElevRad <-  asin( sin(SolDeclRad) * sin(latDeg/180*pi)
                       + cos(SolDeclRad) * cos(latDeg/180*pi) * cos(SolTimeRad))
  # Solar azimuth (horizontal angle) with zero for North
  SolAzimRad <- {
    SolAzimCos <- ((cos(SolDeclRad) * cos(SolTimeRad)
                    - sin(SolElevRad) * cos(latDeg/180*pi) )
                   / (sin(latDeg/180*pi) * cos(SolElevRad) ) )
    # Correction if off edge values
    SolAzimCos[SolAzimCos > +1] <- 1
    SolAzimCos[SolAzimCos < -1] <- 1
    # Conversion to radians
    SolAzimRad0 <- acos(SolAzimCos)
    # Determine if solar azimuth is East or West depending on solar time
    ifelse(SolTimeRad < 0, pi - SolAzimRad0, pi + SolAzimRad0)
  }
  ##value<< named numeric matrix with one row for each time with entries
  ans <- cbind( # cbind creates matrix also if components are single values
    hour = solarTimeHour		    ##<< Solar time in fractional hours after
    ## midnight, (or given hour if isCorrectSolartime = FALSE).
    , declination = SolDeclRad	##<< Solar declination (rad)
    , elevation = SolElevRad		##<< Solar elevation (rad)
    ## with 0 at horizon increasing towards zenith
    , azimuth = SolAzimRad		  ##<< Solar azimuth (rad)
    ## with 0 at North increasing eastwards
  )
  ans
}
attr(computeSunPositionDoyHour,"ex") <- function(){
  computeSunPositionDoyHour(
    160, hour = 0:24, latDeg = 51, longDeg = 13.6, timeZone = 1L)
}

#' @export
computeIsDayByHour <- function(
  ### tell for each date, whether its daytime
  date			         ##<< POSIXct vector
  , sunriseHour = 7	 ##<< sunrise as fractional hour (0..24)
  ## (vector of length date or length 1)
  , sunsetHour = 18	 ##<< sunset as fractional hour
  ## (vector of length date or length 1)
  , duskOffset = 0   ##<< integer scalar: time in hours after dusk for
  ## which records are still regarded as day
){
  # need to convert to numeric, otherwise minus may return in any unit
  # get fractional hour of the day
  hourOfDay <- (as.numeric(date) - as.numeric(trunc(date, units = "days")))/3600
  isDay <- hourOfDay >= sunriseHour & hourOfDay <= (sunsetHour + duskOffset)
  ##value<< logical vector (length(date)): true if its daytime
  isDay
}

#' @export
computeIsDayByLocation <- function(
  ### tell for each timestamp, whether its daytime
  timestamp			##<< POSIXct vector
  , latDeg		  ##<< Latitude in (decimal) degrees
  , longDeg		  ##<< Longitude in (decimal) degrees
  , timeZone = getHoursAheadOfUTC(timestamp)	 ##<< Time zone (in hours)
  ## ahead of UTC (Central Europ is +1)
  , duskOffset = 0  ##<< integer scalar: time in hours after dusk for
  ## which records are still regarded as day
  , isCorrectSolartime = TRUE	##<< set to FALSE to omit correction between
  ## local time and solar time, e.g. if coordinates cannot be provided
){
  ##details<< computes hour of sunrise and sunset from given date in timezone
  ## hour (assuming dates are given in timezoen instead of solartime)
  doy <- as.POSIXlt(timestamp)$yday + 1L
  # correct for solar time only afterwards to get symmetric hours around noon
  sunriseSolarHour <- computeSunriseHour(
    doy, latDeg = latDeg, isCorrectSolartime = FALSE)
  #sunriseLocal <- computeSunriseHour(
  #  doy, latDeg = latDeg, longDeg = longDeg, timeZone = timeZone)
  sunsetSolarHour <- 24 - sunriseSolarHour
  hourDiff <- if (!isCorrectSolartime) 0 else
    computeSolarToLocalTimeDifference(longDeg, timeZone, doy = doy)
  sunriseTimezoneHour <- sunriseSolarHour - hourDiff
  sunsetTimezoneHour <- sunsetSolarHour - hourDiff
  ##value<< logical vector (length(date)): true if its daytime
  computeIsDayByHour(
    timestamp, sunriseHour = sunriseTimezoneHour,
    sunsetHour = sunsetTimezoneHour, duskOffset = duskOffset	)
}
attr(computeIsDayByLocation,"ex") <- function(){
  dateSeq <- seq( as.POSIXct("2017-03-20", tz = "Etc/GMT-1")
                  ,as.POSIXct("2017-03-21", tz = "Etc/GMT-1")
                  , by = "30 min")
  tmp <- computeIsDayByLocation(
    dateSeq, latDeg = 50.93, longDeg = 11.59, timeZone = 1)
  plot( tmp ~ dateSeq )
  yday <- as.POSIXlt(dateSeq[1])$yday + 1L
  sunrise <- computeSunriseHour(
    yday, latDeg = 50.93, longDeg = 11.59, timeZone = 1)
  sunset <- computeSunsetHour(
    yday, latDeg = 50.93, longDeg = 11.59, timeZone = 1)
  abline( v = trunc(dateSeq[1], units = "days") + c(sunrise,sunset)*3600L )
}


