skip_on_ci()
skip_on_cran()

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
    border_buffer = 0.2, 
    save_outputs = FALSE
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
    border_buffer = 0.2, 
    save_outputs = FALSE
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
    border_buffer = 0.2, 
    save_outputs = FALSE
  ))
})

database <-
  readr::read_csv(
    system.file("extdata/outpus_vignettes/00_merged_database.csv", package = "bdc"),
    show_col_types = FALSE
  )

check_pf <-
  bdc_scientificName_empty(
    data = database,
    sci_name = "scientificName")

check_pf <- bdc_coordinates_empty(
  data = check_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")

check_pf <- bdc_coordinates_outOfRange(
  data = check_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")

check_pf <- bdc_basisOfRecords_notStandard(
  data = check_pf,
  basisOfRecord = "basisOfRecord",
  names_to_keep = "all")

check_pf <- bdc_country_from_coordinates(
  data = check_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude",
  country = "country")

check_pf <- bdc_country_standardized(
  data = check_pf,
  country = "country"
)


test_that("test one country without coordinate correction", {
  res <- bdc_coordinates_transposed(
    data = check_pf,
    id = "database_id",
    sci_names = "scientificName",
    lat = "decimalLatitude",
    lon = "decimalLongitude",
    country = "country_suggested",
    countryCode = "countryCode",
    border_buffer = 0.2, 
    save_outputs = FALSE
  ) # in decimal degrees
  
  expect_equal(dim(res), dim(check_pf))
})

