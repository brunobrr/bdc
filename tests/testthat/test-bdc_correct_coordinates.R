skip_on_ci()
skip_on_cran()

# decimalLongitude <- c(-63.43333, -67.91667, -41.90000, -46.69778,-14.43333)
# decimalLatitude <- c(-17.90000, -14.43333, -13.25000, -13.82444,-67.91667 )
# country <- c("bolivia", "bolivia", "brazil", "Brazil", "")
# x <- data.frame(decimalLatitude, decimalLongitude, country)


id <- c(1, 2, 3, 4)
scientificName <- c(
  "Rhinella major", "Scinax ruber",
  "Siparuna guianensis", "Psychotria vellosiana"
)
decimalLatitude <- c(63.43333, -14.43333, -41.90000, -46.69778)
decimalLongitude <- c(-17.90000, -67.91667, -13.25000, -13.82444)
country <- c("BOLIVIA", "bolivia", "Brasil", "Brazil")

x <- data.frame(
  id, scientificName, decimalLatitude,
  decimalLongitude, country
)

# Get country code
x <- bdc_country_standardized(data = x, country = "country")
worldmap <- bdc_get_world_map() # get world map and country iso
tt <- bdc_correct_coordinates(
  data = x,
  x = "decimalLongitude",
  y = "decimalLatitude",
  sp = "scientificName",
  id = "id",
  cntr_iso2 = "countryCode",
  world_poly = worldmap,
  world_poly_iso = "iso2c",
  border_buffer = 0.2
)


test_that("correct transposed", {
  expect_equal(tt$decimalLongitude_modified, c(-63.43333, -41.90000, -46.69778))
  expect_equal(tt$decimalLatitude_modified, c(-17.90000, -13.25000, -13.82444))
  expect_equal(length(colnames(tt)), 11)
})
