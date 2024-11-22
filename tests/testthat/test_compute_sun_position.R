.tmp.f <- function(){
  require(testthat)
  #require(ggplot2) # warnings on R CMD check if uncommented
}
context("compute_sun_position")

test_that("Jena case",{
  latDeg <- 50.93; longDeg <- 11.57
  localDiff <- computeSolarToLocalTimeDifference(longDeg, 1L)
  expect_equal(localDiff, -0.2287, tolerance = 0.01)
  #
  times <- seq(
    ISOdate(2018, 11, 21, 0, tz = "Etc/GMT-1"), by = "2 hour", length.out = 13)
  ans0 <- computeSunPosition(times, latDeg = latDeg, longDeg = longDeg)
  ans <- cbind(data.frame(timestamp = times), as.data.frame(ans0))
  expect_true(ans$elevation[1] < 0)
  # maximum elevation at noon
  imax = which.max(ans$elevation)
  expect_equal(as.POSIXlt(ans$timestamp[imax])$hour, 12)
})

test_that("setLocalTimeZone",{
  # for correct hour need to use local time zone - internally convert to UTC
  longDeg <- 150.745147
  timesUTC <- seq(
    ISOdate(2019, 4, 4, 0, tz = "UTC"), by = "2 hour", length.out = 3)
  times <- setLocalTimeZone(timesUTC, longDeg)
  expect_equal(attr(times, "tzone"),"Etc/GMT-10")
  expect_true(all(times - timesUTC == 0))
})

test_that("Sidney case",{
  # for correct hour need to use local time zone - internally convert to UTC
  latDeg <- -33.611627
  longDeg <- 150.745147
  localDiff <- computeSolarToLocalTimeDifference(longDeg, 10L)
  expect_equal(localDiff, 0.05, tolerance = 0.01)
  #
  timesUTC <- times <- seq(
    ISOdate(2019, 4, 4, 0, tz = "UTC"), by = "2 hour", length.out = 13)
  attr(times, "tzone") <- "Etc/GMT-10" # set corresponding time zone
  ans0l <- computeSunPosition(times, latDeg = latDeg, longDeg = longDeg)
  ans0UTC <- computeSunPosition(timesUTC, latDeg = latDeg, longDeg = longDeg)
  ansl <- cbind(data.frame(
    scen="local", timestamp = times, timesUTC = timesUTC), as.data.frame(ans0l))
  ansUTC <- cbind(data.frame(
    scen="UTC", timestamp = times, timesUTC = timesUTC), as.data.frame(ans0UTC))
  ans <- rbind(ansl, ansUTC )
  .tmp.f <- function(){
    ansl[,-1] - ansUTC[,-1]
    ggplot(ans, aes(timestamp, elevation, color = scen)) + geom_point()
    ggplot(ans, aes(timestamp, azimuth, color = scen, group = scen)) +
      geom_point(position = position_jitter())
    # hours >24 cause problems
    ggplot(ans, aes(hour, azimuth, color = scen, group = scen)) +
      geom_point(position = position_jitter())
  }
  expect_true(all(ans$hour > -1 & ans$hour <= 25))
})

test_that("error on providing timestamp wihtout time zone attribute",{
  latDeg <- -33.611627
  longDeg <- 150.745147
  times <- seq(
    ISOdate(2019, 4, 4, 0), by = "2 hour", length.out = 3)
  attr(times, "tzone") <- NULL
  expect_error(
    ans0 <- computeSunPosition(times, latDeg = latDeg, longDeg = longDeg)
    ,"timezone"
  )
  ans0 <- computeSunPosition(structure(times, tzone='UTC'), latDeg = latDeg, longDeg = longDeg)
})

test_that("warning on providing lat,long vectors rather than scalars",{
  times <- seq(
    ISOdate(2018, 11, 21, 0, tz = "Etc/GMT-1"), by = "2 hour", length.out = 13)
  latDeg <- rep(-33.611627, length(times))
  longDeg <- rep(150.745147, length(times))
  expect_warning(
    ans0 <- computeSunPosition(times, latDeg = latDeg, longDeg = longDeg)
  )
  ans1 <- computeSunPosition(times, latDeg = latDeg[1], longDeg = longDeg[1])
  expect_equal(ans0, ans1)
})
