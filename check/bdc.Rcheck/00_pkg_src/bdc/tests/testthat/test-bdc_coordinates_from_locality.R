test_that("test based on function example", {
  lat <- c(NA, NA, "")
  lon <- c("", NA, NA)
  x <- data.frame(lat = lat, lon = lon, locality = c("Brazil", "Argentina", "Chile"))

  expect_message(bdc_coordinates_from_locality(data = x, lat = "lat", lon = "lon", save_outputs = FALSE))

  r <- bdc_coordinates_from_locality(data = x, lat = "lat", lon = "lon", locality = "locality", save_outputs = FALSE)

  expect_equal(dim(r), c(3, 5))
})
