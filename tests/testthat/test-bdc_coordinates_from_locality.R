test_that("test based on function example", {
  lat <- c(NA, NA, "")
  lon <- c("", NA, NA)
  x <- data.frame(lat = lat, lon = lon, locality = c("Brazil", "Argentina", "Chile"))

  expect_message(bdc_coordinates_from_locality(data = x, lat = "lat", lon = "lon", save_outputs = FALSE))

  r <- bdc_coordinates_from_locality(data = x, lat = "lat", lon = "lon", locality = "locality", save_outputs = FALSE)

  expect_equal(dim(r), c(3, 5))
})

test_that("save_outputs = TRUE writes the check file in the project's Output/Check", {
  pkg_file <- file.path(
    testthat::test_path("..", ".."),
    "Output", "Check", "01_coordinates_from_locality.csv"
  )
  pkg_file <- normalizePath(pkg_file, winslash = "/", mustWork = FALSE)
  pkg_file_before <- file.info(pkg_file)$mtime

  root <- local_temp_project()
  x <- data.frame(
    decimalLatitude = c(-15.79, NA, NA, 95),
    decimalLongitude = c(-47.88, NA, NA, -40.1),
    locality = c("Brasilia", "Parque Nacional Iguazu", "", "Serra do Cipo"),
    recordNumber = c(1, 2, 3, 4)
  )

  res <- suppressMessages(
    bdc_coordinates_from_locality(data = x, save_outputs = TRUE)
  )

  check_file <- file.path(root, "Output", "Check", "01_coordinates_from_locality.csv")
  expect_true(file.exists(check_file))
  saved <- readr::read_csv(check_file, show_col_types = FALSE)
  expect_named(
    saved,
    c(names(x), ".coordinates_empty", ".coordinates_outOfRange")
  )
  expect_equal(saved$recordNumber, c(2, 4))
  expect_equal(saved, res, ignore_attr = TRUE)

  expect_identical(file.info(pkg_file)$mtime, pkg_file_before)
})

test_that("save_outputs = FALSE writes nothing", {
  root <- local_temp_project()
  x <- data.frame(lat = NA, lon = NA, locality = "Parque Nacional Iguazu")

  suppressMessages(
    bdc_coordinates_from_locality(
      data = x, lat = "lat", lon = "lon", save_outputs = FALSE
    )
  )

  expect_false(dir.exists(file.path(root, "Output")))
})
