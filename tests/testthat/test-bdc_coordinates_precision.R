test_that("test based on function example", {

  
  lat = c(-21.345, 23.567, 16.798, -10.46808)
  lon = c(-55.389, -13.897, 30.678, 90.67599)
  x <- data.frame(lat, lon)
  
  r <- bdc_coordinates_precision(
    data = x,
    lat = "lat",
    lon = "lon",  
    ndec = 5)
  
  expect_equal(ncol(r), 3)
  expect_equal(sum(r$.rou), 3)
  
  r <- bdc_coordinates_precision(
    data = x,
    lat = "lat",
    lon = "lon",  
    ndec = c(2))
  
  expect_equal(sum(r$.rou), 4)
  
  })
