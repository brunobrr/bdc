id <- c(1, 2, 3, 4)
scientificName <- c(
  "Rhinella major",
  "Scinax ruber",
  "Siparuna guianensis",
  "Psychotria vellosiana"
)
decimalLatitude <- c(63.43333, -14.43333, -41.90000, -46.69778)
decimalLongitude <- c(-17.90000, -67.91667, -13.25000, -13.82444)
country_suggested <- c("Bolivia", "Bolivia", "Brazil", "Brazil")
countryCode <- c("BO", "BO", "BR", "BR")
x <- data.frame(
  id, scientificName, decimalLatitude,
  decimalLongitude, country_suggested, countryCode
)


test_that("test based on function example dataset", {
  df <- bdc_coordinates_transposed(
    data = x,
    id = "id",
    sci_names = "scientificName",
    lat = "decimalLatitude",
    lon = "decimalLongitude",
    country = "country_suggested",
    countryCode = "countryCode",
    border_buffer = 0.2
  ) # in decimal degrees

  expect_equal(df$coordinates_transposed, c(FALSE, TRUE, FALSE, FALSE))
})


test_that("Misuse of arguments", {
  # Misuse of arguments
  expect_error(df <- bdc_coordinates_transposed(
    data = x,
    # id = "id",
    sci_names = "scientificName",
    lat = "decimalLatitude",
    lon = "decimalLongitude",
    country = "country_suggested",
    countryCode = "countryCode",
    border_buffer = 0.2
  ))
})

test_that("Misuse of column names", {
  expect_error(df <- bdc_coordinates_transposed(
    data = x,
    # id = "id",
    sci_names = "scientificName",
    lat = "decimalLatit",
    lon = "decimalLongit",
    country = "country_suggested",
    countryCode = "countryCode",
    border_buffer = 0.2
  ))
})
