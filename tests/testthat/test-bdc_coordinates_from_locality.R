test_that("test based on function example", {
  lat <- c(NA, NA, "")
  lon <- c("", NA, NA)
  x <- data.frame(lat = lat, lon = lon, locality = c("Brazil", "Argentina", "Chile"))

  expect_message(bdc_coordinates_from_locality(data = x, lat = "lat", lon = "lon", save_outputs = FALSE))

  r <- bdc_coordinates_from_locality(data = x, lat = "lat", lon = "lon", locality = "locality", save_outputs = FALSE)

  expect_equal(dim(r), c(3, 5))
})

test_that("locality values are returned unchanged", {
  x <- data.frame(
    lat = c(NA, NA),
    lon = c(NA, NA),
    place = c("  Parque Nacional   Iguazu ", "")
  )

  r <- suppressMessages(
    bdc_coordinates_from_locality(x, lat = "lat", lon = "lon", locality = "place")
  )

  expect_equal(r$place, "  Parque Nacional   Iguazu ")
  expect_false("locality" %in% names(r))
})
