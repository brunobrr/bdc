skip_on_cran()
skip_if_no_natural_earth()

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

# Custom (non Darwin Core) column names ---------------------------------------

x_default <- data.frame(
  decimalLatitude = c(-17.90000, -14.43333, -13.25000, -13.82444, -67.91667),
  decimalLongitude = c(-63.43333, -67.91667, -41.90000, -46.69778, -14.43333),
  country = c("", "NA", NA, "Brazil", "")
)
x_custom <- data.frame(
  la = x_default$decimalLatitude,
  lo = x_default$decimalLongitude,
  pais = x_default$country
)

test_that("bdc_country_from_coordinates() fills a custom country column", {
  res_default <- suppressMessages(bdc_country_from_coordinates(x_default))
  res <- suppressMessages(
    bdc_country_from_coordinates(
      data = x_custom,
      lat = "la",
      lon = "lo",
      country = "pais"
    )
  )

  expect_named(res, names(x_custom))
  expect_equal(res$pais, c("Bolivia", NA, "Brazil", "Brazil", NA))
  expect_equal(res$pais, res_default$country)
  expect_equal(res$la, x_custom$la)
  expect_equal(res$lo, x_custom$lo)
})

test_that("bdc_country_from_coordinates() creates the custom country column when missing", {
  res <- suppressMessages(
    bdc_country_from_coordinates(
      data = x_custom[c("la", "lo")],
      lat = "la",
      lon = "lo",
      country = "pais"
    )
  )

  expect_named(res, c("la", "lo", "pais"))
  expect_equal(res$pais, c("Bolivia", "Bolivia", "Brazil", "Brazil", NA))
})

test_that("bdc_country_from_coordinates() adds no helper columns when nothing is done", {
  y <- x_custom
  y$pais <- "Brazil"

  expect_message(
    res <- bdc_country_from_coordinates(
      data = y,
      lat = "la",
      lon = "lo",
      country = "pais"
    ),
    "Nothing was done"
  )
  expect_named(res, names(y))
})
