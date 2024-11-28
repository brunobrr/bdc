skip_on_ci()
skip_on_cran()

decimalLongitude <- c(-63.43333, -67.91667, -41.90000, -46.69778, -14.43333)
decimalLatitude <- c(-17.90000, -14.43333, -13.25000, -13.82444, -67.91667)
country <- c("", "NA", NA, "Brazil", "")
x <- data.frame(decimalLatitude, decimalLongitude, country)

test_that("it can return country names", {
  r <- bdc_country_from_coordinates(
    data = x,
    lat = "decimalLatitude",
    lon = "decimalLongitude",
    country = "country"
  )
  
  expect_equal(r$country, c("Bolivia", NA, "Brazil", "Brazil", NA))
  
})

test_that("error: column names not found", {
  expect_error(bdc_country_from_coordinates(
    data = x,
    lat = "lat",
    lon = "long",
    country = "country"
  ))
  
})

test_that("return message", {
  expect_message(
    bdc_country_from_coordinates(
      data = x,
      lat = "decimalLatitude",
      lon = "decimalLongitude",
      country = "country"
    ),
    "bdc_country_from_coordinates:
Country names were added to 2 records."
  )
  
})

test_that("return country names when all contry names is missing", {
  y <- data.frame(decimalLatitude, decimalLongitude)
  expect_message(
    bdc_country_from_coordinates(data = y,
                                 lat = "decimalLatitude",
                                 lon = "decimalLongitude"),
    "bdc_country_from_coordinates:
Country names were added to 4 records in a new collumn named 'country'."
  )
  
})

decimalLatitude <-
  c(62.587273591263624,
    66.62443625769812,
    60.91266175537055,
    59.166132649248496)
decimalLongitude <-
  c(30.81351622904529,
    21.219645421093123,
    10.65224213789756,
    25.883878594365648)
country <- c("", "", "", "")
data <- data.frame(decimalLatitude, decimalLongitude, country)

result <- bdc_country_from_coordinates(data)

test_that("bdc_country_from_coordinates keeps rows order", {
  expect_equal(result$decimalLatitude, decimalLatitude)
  expect_equal(result$decimalLongitude, decimalLongitude)
  
})

test_that("bdc_country_from_coordinates correctly assigns country names",
          {
            expect_equal(result$country, c("Finland", "Sweden", "Norway", "Estonia"))
            
          })








