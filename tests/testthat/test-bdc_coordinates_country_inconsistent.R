test_that("test with function example", {
  metadata <- readr::read_csv(system.file("extdata", "input_files/gbif.csv", package = "bdc"))
  metadata$countryCode <- "Brazil"
  df <- bdc_coordinates_country_inconsistent(
    data = metadata,
    country_name = "Brazil",
    country = "countryCode",
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    dist = 0.1 # in decimal degrees
  )

  expect_true(".coordinates_country_inconsistent" %in% names(df))
  expect_equal(sum(!df$.coordinates_country_inconsistent), 0)

  # With different values of dist
  decimalLongitude <- c(-40.6003, -39.6, -20.5243, NA, -64.105)
  decimalLatitude <- c(19.9358, -13.016667, NA, "", -12.558)
  x <- data.frame(decimalLongitude, decimalLatitude)
  x$coutnry_suggested <- "Brazil" 
  df <- bdc_coordinates_country_inconsistent(
    data = x,
    country_name = c("Brazil"),
    country = "coutnry_suggested",
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    dist = 10 # in decimal degrees
  )
  df
  expect_equal(df$.coordinates_country_inconsistent, c(FALSE, TRUE, TRUE, TRUE, FALSE))
})
