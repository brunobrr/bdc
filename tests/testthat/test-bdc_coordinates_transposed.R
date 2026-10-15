skip_on_cran()
skip_if_no_natural_earth()

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


# Custom (non Darwin Core) column names ---------------------------------------

x_custom <- data.frame(
  idx = id,
  sp = scientificName,
  la = decimalLatitude,
  lo = decimalLongitude,
  pais = country_suggested,
  cc = countryCode
)

run_transposed_custom <- function(data) {
  suppressMessages(
    bdc_coordinates_transposed(
      data = data,
      id = "idx",
      sci_names = "sp",
      lat = "la",
      lon = "lo",
      country = "pais",
      countryCode = "cc",
      border_buffer = 0.2,
      save_outputs = FALSE
    )
  )
}

test_that("bdc_coordinates_transposed() keeps custom column names", {
  res <- run_transposed_custom(x_custom)

  expect_named(res, c(names(x_custom), "coordinates_transposed"))
  expect_equal(res$coordinates_transposed, c(FALSE, TRUE, FALSE, FALSE))
})

test_that("bdc_coordinates_transposed() corrects coordinates in custom columns", {
  res_default <- suppressMessages(
    bdc_coordinates_transposed(
      data = x,
      id = "id",
      country = "country_suggested",
      save_outputs = FALSE
    )
  )
  res <- run_transposed_custom(x_custom)

  expect_equal(res$la, res_default$decimalLatitude)
  expect_equal(res$lo, res_default$decimalLongitude)
})

test_that("bdc_coordinates_transposed() keeps custom names when nothing is corrected", {
  res <- run_transposed_custom(x_custom[2, ])

  expect_named(res, names(x_custom))
})

test_that("coordinates_transposed is informational and stays out of .summary", {
  res <- run_transposed_custom(x_custom)
  res$.coordinates_empty <- TRUE
  res <- suppressMessages(bdc_summary_col(res))

  expect_false(".coordinates_transposed" %in% names(res))
  expect_equal(res$.summary, rep(TRUE, nrow(x_custom)))
})

test_that("bdc_create_report() and bdc_create_figures() detect coordinates_transposed", {
  local_temp_project()

  res <- suppressMessages(
    bdc_coordinates_transposed(
      data = x,
      id = "id",
      country = "country_suggested",
      save_outputs = TRUE
    )
  )
  res$.coordinates_empty <- c(TRUE, TRUE, TRUE, FALSE)

  report <- suppressMessages(
    bdc_create_report(
      data = res,
      database_id = "id",
      workflow_step = "prefilter",
      save_report = FALSE
    )
  )
  expect_true("coordinates_transposed" %in% report$x$data$Test_name)

  figures <- suppressMessages(suppressWarnings(
    bdc_create_figures(
      data = res,
      database_id = "id",
      workflow_step = "prefilter",
      save_figures = FALSE
    )
  ))
  expect_true("coordinates_transposed" %in% names(figures))
})
