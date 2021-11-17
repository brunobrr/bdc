test_that("test with function example", {
  
  decimalLongitude <- c(-40.6003, -39.6, -20.5243, NA)
  decimalLatitude <- c(19.9358, -13.016667, NA, "")
  x <- data.frame(decimalLongitude, decimalLatitude)
  
  bdc_coordinates_country_inconsistent(
    data = x,
    country_name = "Brazil",
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    dist = 0.1 # in decimal degrees
  )
  
  bdc_coordinates_country_inconsistent(
    data = x,
    country_name = c("Argentina", "Brazil"),
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    dist = 0.2 # in decimal degrees
  )
  
})
