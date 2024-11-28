skip_on_ci()
skip_on_cran()

metadata <- readr::read_csv(system.file("extdata", "input_files/gbif.csv", package = "bdc"), show_col_types = FALSE)
metadata$countryCode <- "Brazil"
df <- bdc_coordinates_country_inconsistent(
  data = metadata,
  country_name = "Brazil",
  country = "countryCode",
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  dist = 0.1 # in decimal degrees
)

test_that("test with function example", {
  expect_true(".coordinates_country_inconsistent" %in% names(df))
  expect_equal(sum(!df$.coordinates_country_inconsistent), 0)
})


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

test_that("test different values of dist", {
  expect_equal(df$.coordinates_country_inconsistent, 
               c(FALSE, TRUE, TRUE, TRUE, FALSE))
})

x0 <- data.frame(
  country = c("Brazil", "Brazil", "Bolivia", "Argentina", "Peru"),
  decimalLongitude = c(-40.6003, -39.6, -77.689288, -69.269926, -76.352930),
  decimalLatitude = c(-19.9358, -13.016667, -20.5243, -35.345940, -11.851872)
)

x1 <- 
  bdc_coordinates_country_inconsistent(
  data = x0,
  country_name = c("Brazil", "Argentina"),
  country = "country",
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  dist = 0.1
)

test_that("multiple countries", {
  expect_equal(x1$.coordinates_country_inconsistent, 
               c(TRUE, TRUE, FALSE, TRUE, FALSE))
})


x2 <- 
  bdc_coordinates_country_inconsistent(
    data = x0,
    country_name = c("Brazil", "Argentina", "Peru"),
    country = "country",
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    dist = 0.1
  )

test_that("multiple countries II", {
  expect_equal(x2$.coordinates_country_inconsistent, 
               c(TRUE, TRUE, FALSE, TRUE, TRUE))
})
