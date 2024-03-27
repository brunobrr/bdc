test_that("test based on function example", {
  lat <- c(-21, -21.34, 23.567, 16.798, -10.46808)
  lon <- c(-55, -55.38, -13.897, 30.678, 90.67599)
  x <- data.frame(lat, lon)

  r <- bdc_coordinates_precision(
    data = x,
    lat = "lat",
    lon = "lon",
    ndec = 5
  )

  expect_equal(ncol(r), 3)
  expect_equal(sum(r$.rou), 1)

  r <- bdc_coordinates_precision(
    data = x,
    lat = "lat",
    lon = "lon",
    ndec = 0
  )

  expect_equal(sum(r$.rou), 5)
})
