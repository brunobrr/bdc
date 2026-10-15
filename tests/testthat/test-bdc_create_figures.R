skip_on_ci()
skip_on_cran()

database_id <- c("GBIF_01", "GBIF_02", "GBIF_03", "FISH_04", "FISH_05")
 lat <- c(-19.93580, -13.01667, -22.34161, -6.75000, -15.15806)
 lon <- c(-40.60030, -39.60000, -49.61017, -35.63330, -39.52861)
 .scientificName_emptys <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
 .coordinates_empty <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
 .invalid_basis_of_records <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
 .summary <- c(TRUE, FALSE, TRUE, FALSE, FALSE)

 x <- data.frame(
   database_id,
   lat,
   lon,
   .scientificName_emptys,
   .coordinates_empty,
   .invalid_basis_of_records,
   .summary
 )

figures <-
 bdc_create_figures(
   data = x,
   database_id = "database_id",
   workflow_step = "prefilter",
   save_figures = FALSE
 )

test_that("test figures object", {

  testthat::expect_equal(names(figures), c(".invalid_basis_of_records", ".summary", "summary_all_tests"))

})

# Transposed coordinates are read from the project root ----------------------

transposed_input <- data.frame(
  database_id = c("gbif_1", "gbif_2", "gbif_3", "gbif_4"),
  scientificName = c(
    "Rhinella major", "Scinax ruber",
    "Siparuna guianensis", "Psychotria vellosiana"
  ),
  decimalLatitude = c(63.43333, -14.43333, -41.90000, -46.69778),
  decimalLongitude = c(-17.90000, -67.91667, -13.25000, -13.82444),
  country_suggested = c("Bolivia", "Bolivia", "Brazil", "Brazil"),
  countryCode = c("BO", "BO", "BR", "BR"),
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

prefilter_figure_names <- function(data) {
  figures <- suppressMessages(suppressWarnings(
    bdc_create_figures(data = data, workflow_step = "prefilter")
  ))
  names(figures)
}

test_that("bdc_create_figures() maps transposed coordinates from a subfolder", {
  local_temp_project(subdir = "reports")
  flagged <- flag_transposed(transposed_input)

  expect_true("coordinates_transposed" %in% prefilter_figure_names(flagged))
})

test_that("bdc_create_figures() maps transposed coordinates from the root", {
  local_temp_project()
  flagged <- flag_transposed(transposed_input)

  expect_true("coordinates_transposed" %in% prefilter_figure_names(flagged))
})

# workflow_step validation ----------------------------------------------------

test_that("bdc_create_figures() requires a valid workflow_step", {
  local_temp_project()

  expect_error(bdc_create_figures(x), "prefilter.*space.*time")
  expect_error(
    bdc_create_figures(x, workflow_step = "taxonomy"),
    "prefilter.*space.*time"
  )
  expect_error(
    bdc_create_figures(x, workflow_step = c("prefilter", "space")),
    "prefilter.*space.*time"
  )
})
