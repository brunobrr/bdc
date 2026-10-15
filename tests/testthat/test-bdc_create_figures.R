skip_if_not_installed("cowplot")
skip_if_not_installed("hexbin")

# Fixtures --------------------------------------------------------------------

# Six records from two datasets (GBIF and SPLINK), all on land in Brazil, so
# every map has at least two distinct points to bin.
records <- function(...) {
  data.frame(
    database_id = c(
      "GBIF_1", "GBIF_2", "GBIF_3", "SPLINK_4", "SPLINK_5", "SPLINK_6"
    ),
    decimalLatitude = c(-19.9, -13.0, -22.3, -6.7, -15.1, -10.0),
    decimalLongitude = c(-40.6, -39.6, -49.6, -35.6, -39.5, -50.0),
    ...
  )
}

create_figures <- function(data, workflow_step, save_figures = FALSE) {
  suppressMessages(
    bdc_create_figures(
      data = data,
      workflow_step = workflow_step,
      save_figures = save_figures
    )
  )
}

saved_figures <- function(root) {
  list.files(file.path(root, "Output", "Figures"))
}

expect_all_ggplots <- function(figures) {
  is_plot <- vapply(figures, \(p) inherits(p, "ggplot"), logical(1))
  expect_true(all(is_plot), info = paste(names(figures)[!is_plot]))
}

# prefilter -------------------------------------------------------------------

prefilter_records <- records(
  .scientificName_empty = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE),
  .coordinates_empty = TRUE,
  .invalid_basis_of_records = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE),
  .summary = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)
)

test_that("bdc_create_figures() plots each flagged prefilter test", {
  local_temp_project()

  figures <- create_figures(prefilter_records, "prefilter")

  # .coordinates_empty flags nothing, so it gets no figure.
  expect_equal(
    names(figures),
    c(
      ".scientificName_empty", ".invalid_basis_of_records", ".summary",
      "summary_all_tests"
    )
  )
  expect_all_ggplots(figures)
})

test_that("bdc_create_figures() maps prefilter country inconsistencies", {
  skip_if_not_installed("rnaturalearthdata")
  local_temp_project()
  data <- records(
    .coordinates_country_inconsistent = c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE)
  )

  figures <- create_figures(data, "prefilter")

  # One bar plot, the summary of all tests, and one map.
  expect_length(figures, 3)
  expect_setequal(
    unique(names(figures)),
    c(".coordinates_country_inconsistent", "summary_all_tests")
  )
  expect_all_ggplots(figures)
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
  skip_if_no_natural_earth()
  local_temp_project(subdir = "reports")
  flagged <- flag_transposed(transposed_input)

  expect_true("coordinates_transposed" %in% prefilter_figure_names(flagged))
})

test_that("bdc_create_figures() maps transposed coordinates from the root", {
  skip_if_no_natural_earth()
  local_temp_project()
  flagged <- flag_transposed(transposed_input)

  expect_true("coordinates_transposed" %in% prefilter_figure_names(flagged))
})

test_that("bdc_create_figures() has no transposed map without its check file", {
  local_temp_project()

  figures <- create_figures(prefilter_records, "prefilter")

  expect_false("coordinates_transposed" %in% names(figures))
})

test_that("bdc_create_figures() says why the transposed map is missing", {
  skip("the function silences its own diagnostic messages (#323)")
  local_temp_project()

  expect_message(
    bdc_create_figures(prefilter_records, workflow_step = "prefilter"),
    "01_coordinates_transposed.csv' not found"
  )
})

test_that("bdc_create_figures() saves prefilter figures under the project root", {
  skip_if_no_natural_earth()
  root <- local_temp_project(subdir = "reports")
  flagged <- flag_transposed(transposed_input)

  figures <- create_figures(flagged, "prefilter", save_figures = TRUE)

  expect_equal(
    names(figures),
    c(".scientificName_empty", "summary_all_tests", "coordinates_transposed")
  )
  expect_setequal(
    saved_figures(root),
    c(
      "prefilter_.scientificName_empty_BAR.png",
      "prefilter_summary_all_tests_BAR.png",
      "prefilter_coordinates_transposed_BAR.png"
    )
  )
  expect_false(dir.exists(file.path(root, "reports", "Output")))
})

# space -----------------------------------------------------------------------

test_that("bdc_create_figures() plots each flagged space test", {
  root <- local_temp_project()
  data <- records(
    .zer = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE),
    .gbf = c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE),
    .summary = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE)
  )

  figures <- create_figures(data, "space", save_figures = TRUE)

  expect_equal(
    names(figures),
    c(".zer", ".gbf", ".summary", "summary_all_tests")
  )
  expect_all_ggplots(figures)
  expect_setequal(
    saved_figures(root),
    paste0("space_", names(figures), "_BAR.png")
  )
})

space_map_records <- records(
  .equ = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
  .cen = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE),
  .rou = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
  .summary = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
)

test_that("bdc_create_figures() maps flagged space records", {
  skip_if_not_installed("rnaturalearthdata")
  skip_if_not_installed("maps")
  local_temp_project()

  figures <- create_figures(space_map_records, "space")

  # A bar plot and a map for each of .equ, .cen and .rou, plus the .summary bar
  # plot and the summary of all tests.
  expect_length(figures, 8)
  expect_setequal(
    unique(names(figures)),
    c(".equ", ".cen", ".rou", ".summary", "summary_all_tests")
  )
  expect_all_ggplots(figures)
})

test_that("bdc_create_figures() saves each space map to its own file", {
  skip("maps reuse their bar plot's name and overwrite its file (#312)")
  skip_if_not_installed("rnaturalearthdata")
  skip_if_not_installed("maps")
  root <- local_temp_project()

  figures <- create_figures(space_map_records, "space", save_figures = TRUE)

  expect_equal(anyDuplicated(names(figures)), 0)
  expect_length(saved_figures(root), length(figures))
})

# time ------------------------------------------------------------------------

time_records <- records(
  year = c(1990, 2000, 1500, 2010, NA, 2020),
  .eventDate_empty = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),
  .year_outOfRange = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE),
  .summary = c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE)
)

test_that("bdc_create_figures() plots time tests and the year histogram", {
  root <- local_temp_project()

  figures <- create_figures(time_records, "time", save_figures = TRUE)

  expect_equal(
    names(figures),
    c(
      ".eventDate_empty", ".year_outOfRange", ".summary",
      "summary_all_tests", "year"
    )
  )
  expect_all_ggplots(figures)
  expect_setequal(
    saved_figures(root),
    paste0("time_", names(figures), "_BAR.png")
  )
})

test_that("bdc_create_figures() skips the year histogram without a year column", {
  skip("the time step always builds the year histogram (#314)")
  local_temp_project()
  data <- time_records
  data$year <- NULL

  expect_message(
    figures <- bdc_create_figures(data, workflow_step = "time"),
    "Column 'year' not found"
  )
  expect_false("year" %in% names(figures))
})

# Nothing to plot -------------------------------------------------------------

test_that("bdc_create_figures() returns no figures when nothing is flagged", {
  local_temp_project()
  data <- records(.scientificName_empty = TRUE, .summary = TRUE)

  messages <- capture_messages(
    figures <- bdc_create_figures(data, workflow_step = "space")
  )

  expect_match(messages, "No records flagged", all = FALSE)
  expect_length(figures, 0)
})

test_that("bdc_create_figures() returns no figures without test columns", {
  local_temp_project()

  messages <- capture_messages(
    figures <- bdc_create_figures(records(), workflow_step = "space")
  )

  expect_match(messages, "At least one column 'starting with '.'", all = FALSE)
  expect_length(figures, 0)
})

# workflow_step validation ----------------------------------------------------

test_that("bdc_create_figures() requires a valid workflow_step", {
  local_temp_project()

  expect_error(
    bdc_create_figures(prefilter_records),
    "prefilter.*space.*time"
  )
  expect_error(
    bdc_create_figures(prefilter_records, workflow_step = "taxonomy"),
    "prefilter.*space.*time"
  )
  expect_error(
    bdc_create_figures(
      prefilter_records,
      workflow_step = c("prefilter", "space")
    ),
    "prefilter.*space.*time"
  )
})
