test_that("test with function example", {
  decimalLongitude <- c(-40.6003, -39.6, -20.5243, NA, -84.5)
  decimalLatitude <- c(19.9358, -13.016667, NA, "",  -33.742280)
  x <- data.frame(decimalLongitude, decimalLatitude)
  
  points(x[1:2])
  
  df <- bdc_coordinates_country_inconsistent(
    data = x,
    country_name = "Brazil",
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    dist = 0.1 # in decimal degrees
  )
  
  expect_true(".coordinates_country_inconsistent" %in% names(df))
  
  # With different values of dist
  df <- bdc_coordinates_country_inconsistent(
    data = x,
    country_name = "Brazil",
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    dist = 5 # in decimal degrees
  )
  # TODO correct or check dist argument in sf::st_buffer 
  expect_equal(df$.coordinates_country_inconsistent, c(FALSE, TRUE, TRUE, TRUE, FALSE))
})
