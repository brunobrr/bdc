database_id <- c("test_1", "test_2", "test_3", "test_4", "test_5")
.scientificName_empty <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
.coordinates_empty <- c(TRUE, FALSE, FALSE, TRUE, FALSE)
.coordinates_outOfRange <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
.summary <- c(TRUE, FALSE, FALSE, FALSE, FALSE)

x <- data.frame(
  database_id,
  .scientificName_empty,
  .coordinates_empty,
  .coordinates_outOfRange,
  .summary
)

report <-
  bdc_create_report(
    data = x,
    database_id = "database_id",
    workflow_step = "prefilter",
    save_report = FALSE
  )


test_that("test report dimensions" , {
  testthat::expect_equal(dim(report$x$data), c(6, 5))
  
})

y <-
  data.frame(
    .coordinates_outOfRange = c(FALSE, FALSE, TRUE),
    transposed_coordinates = c(TRUE, FALSE, TRUE)
  )

test_that("test coordinates transposed" , {
  testthat::expect_equal(dim(report$x$data), c(6, 5))
  
})
 
# Transposed coordinates are read from the project root ----------------------

transposed_input <- data.frame(
  scientificName = c(
    "Rhinella major", "Scinax ruber",
    "Siparuna guianensis", "Psychotria vellosiana"
  ),
  decimalLatitude = c(63.43333, -14.43333, -41.90000, -46.69778),
  decimalLongitude = c(-17.90000, -67.91667, -13.25000, -13.82444),
  country_suggested = c("Bolivia", "Bolivia", "Brazil", "Brazil"),
  countryCode = c("BO", "BO", "BR", "BR"),
  database_id = c("gbif_1", "gbif_2", "gbif_3", "gbif_4"),
  .scientificName_empty = c(TRUE, FALSE, TRUE, TRUE)
)

flag_transposed <- function(data) {
  suppressMessages(
    bdc_coordinates_transposed(
      data = data,
      id = "database_id",
      sci_names = "scientificName",
      lat = "decimalLatitude",
      lon = "decimalLongitude",
      country = "country_suggested",
      countryCode = "countryCode",
      border_buffer = 0.2,
      save_outputs = TRUE
    )
  )
}

report_descriptions <- function(data) {
  report <- suppressMessages(
    bdc_create_report(data = data, workflow_step = "prefilter")
  )
  report$x$data$Description
}

test_that("bdc_create_report() lists transposed coordinates from a subfolder", {
  skip_on_cran()
  local_temp_project(subdir = "reports")
  flagged <- flag_transposed(transposed_input)

  expect_true(
    "Records with transposed geographic coordinates" %in%
      report_descriptions(flagged)
  )
})

test_that("bdc_create_report() lists transposed coordinates from the root", {
  skip_on_cran()
  local_temp_project()
  flagged <- flag_transposed(transposed_input)

  expect_true(
    "Records with transposed geographic coordinates" %in%
      report_descriptions(flagged)
  )
})
