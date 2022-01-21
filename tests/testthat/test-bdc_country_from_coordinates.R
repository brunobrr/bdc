
FUNC <- "bdc_country_from_coordinates"


test_that("it can return ountry names", {
  decimalLongitude <- c(-63.43333, -67.91667, -41.90000, -46.69778,-14.43333)
  decimalLatitude <- c(-17.90000, -14.43333, -13.25000, -13.82444,-67.91667 )
  country <- c("", "NA", NA, "Brazil", "")
  x <- data.frame(decimalLatitude, decimalLongitude, country)
  r<- bdc_country_from_coordinates(
     data = x,
     lat = "decimalLatitude",
     lon = "decimalLongitude",
     country = "country")

  expect_equal(r$country, c("Bolivia", "NA", "Brazil", "Brazil", ""))
  
  expect_error( bdc_country_from_coordinates(
    data = x,
    lat = "lat",
    lon = "long",
    country = "country"))
  
  
  expect_message (bdc_country_from_coordinates(
    data = x,
    lat = "decimalLatitude",
    lon = "decimalLongitude",
    country = "country"), "bdc_country_from_coordinates:
Country names were added to 2 records.")
  
  y <- data.frame(decimalLatitude, decimalLongitude)
  expect_message (
    bdc_country_from_coordinates(
    data = y,
    lat = "decimalLatitude",
    lon = "decimalLongitude"), "bdc_country_from_coordinates:
Country names were added to 4 records in a new collumn named 'country'.")
  
})
