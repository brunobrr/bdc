datafake1 <- tibble::tribble(
  ~species, ~longitude, ~latitude, ~id,
  "Eragrostis maypurensis", -47.824956, -13.17078, 3120L,
  "Mimosa sensitiva", -43.651389, -20.390833, 2756L,
  "Aegiphila integrifolia", -48.434722, -7.195278, 2430L,
  "Rhodospatha venosa", -60, -2, 964L,
  "Ditassa succedanea", -43.615833, -18.007222, 2515L,
  "Sebastiania macrocarpa", -39.404722, -8.454722, 786L,
  "Caladium bicolor", -34.960278, -7.696667, 1928L,
  "Amaioua intermedia", -45.0711, -23.4339, 3493L,
  "Mikania officinalis", -48.801389, -17.024167, 3801L
)

datafake2 <- tibble::tribble(
  ~spp, ~lon, ~lat, ~id_number,
  "Eragrostis maypurensis", -47.824956, -13.17078, 3120L,
  "Mimosa sensitiva", -43.651389, -20.390833, 2756L,
  "Aegiphila integrifolia", -48.434722, -7.195278, 2430L,
  "Rhodospatha venosa", -60, -2, 964L,
  "Ditassa succedanea", -43.615833, -18.007222, 2515L,
  "Sebastiania macrocarpa", -39.404722, -8.454722, 786L,
  "Caladium bicolor", -34.960278, -7.696667, 1928L,
  "Amaioua intermedia", -45.0711, -23.4339, 3493L,
  "Mikania officinalis", -48.801389, -17.024167, 3801L
)

datafake3 <- tibble::tribble(
  ~nome_das_especies, ~x, ~y,
  "Eragrostis maypurensis", -47.824956, -13.17078,
  "Mimosa sensitiva", -43.651389, -20.390833,
  "Aegiphila integrifolia", -48.434722, -7.195278,
  "Rhodospatha venosa", -60, -2,
  "Ditassa succedanea", -43.615833, -18.007222,
  "Sebastiania macrocarpa", -39.404722, -8.454722,
  "Caladium bicolor", -34.960278, -7.696667,
  "Amaioua intermedia", -45.0711, -23.4339,
  "Mikania officinalis", -48.801389, -17.024167
)

datafake4 <- tibble::tribble(
  ~nome_das_especies, ~x,
  "Eragrostis maypurensis", -47.824956,
  "Mimosa sensitiva", -43.651389,
  "Aegiphila integrifolia", -48.434722,
  "Rhodospatha venosa", -60,
  "Ditassa succedanea", -43.615833,
  "Sebastiania macrocarpa", -39.404722,
  "Caladium bicolor", -34.960278,
  "Amaioua intermedia", -45.0711,
  "Mikania officinalis", -48.801389
)

tdir <- tempdir()

df1_path <- paste0(tdir, "/datafake1.csv")
df2_path <- paste0(tdir, "/datafake2.csv")
df3_path <- paste0(tdir, "/datafake3.csv")
df4_path <- paste0(tdir, "/datafake4.csv")

write_csv(datafake1, df1_path)
write_csv(datafake2, df2_path)
write_csv(datafake3, df3_path)
write_csv(datafake4, df4_path)

metadata <- tibble::tribble(
  ~datasetName, ~fileName, ~occurrenceID, ~scientificName, ~decimalLatitude, ~decimalLongitude,
  "datafake1", df1_path, "id", "species", "latitude", "longitude",
  "datafake2", df2_path, "id_number", "spp", "lat", "lon",
  "datafake3", df3_path, NA, "nome_das_especies", "y", "x"
)

metadata_missing_column <- tibble::tribble(
  ~datasetName, ~fileName, ~occurrenceID, ~scientificName, ~decimalLatitude,
  "datafake1", df1_path, "id", "species", "latitude",
  "datafake2", df2_path, "id_number", "spp", "lat",
  "datafake4", df3_path, NA, "nome_das_especies", "y"
)

wrong_metadata <- tibble::tribble(
  ~datasetName, ~fileName, ~occurrenceID, ~scientificName, ~decimalLatitude, ~decimalLongitude, ~missing_column, ~notes,
  "datafake4", df4_path, NA, "nome_das_especies", "y", "x", NA, "notes"
)

metadata_repeated_datasetName <- tibble::tribble(
  ~datasetName, ~fileName, ~occurrenceID, ~scientificName, ~decimalLatitude, ~decimalLongitude,
  "datafake1", df1_path, "id", "species", "latitude", "longitude",
  "datafake2", df2_path, "id_number", "spp", "lat", "lon",
  "datafake1", df1_path, "id", "species", "latitude", "longitude"
)

bdc_standardize_datasets(metadata = metadata, overwrite = TRUE, format = "qs", save_database = FALSE)

test_that("bdc_standardize_datasets can create qs files", {
  created_qs_files <-
    fs::dir_ls(path = here::here(tdir, "data", "temp_datasets"), glob = "*datafake*qs") %>%
    basename()

  expected_qs_files <-
    c("standard_datafake1.qs", "standard_datafake2.qs", "standard_datafake3.qs")

  expect_equal(created_qs_files, expected_qs_files)
})

test_that("datafake1 has the default column names", {
  created_qs_files <-
    fs::dir_ls(path = here::here(tdir, "data", "temp_datasets"), glob = "*datafake*qs")

  df1 <-
    qs::qread(created_qs_files[1]) %>%
    names()

  expect_equal(df1, c("database_id", "occurrenceID", "scientificName", "decimalLatitude", "decimalLongitude"))
})

test_that("datafake3 has the default column names", {
  created_qs_files <-
    fs::dir_ls(path = here::here(tdir, "data", "temp_datasets"), glob = "*datafake*qs")

  df3 <-
    qs::qread(created_qs_files[3]) %>%
    names()

  expect_equal(df3, c("database_id", "scientificName", "decimalLatitude", "decimalLongitude"))
})

bdc_standardize_datasets(metadata = metadata, overwrite = TRUE, format = "csv", save_database = FALSE)

test_that("bdc_standardize_datasets can create csv files", {
  created_csv_files <-
    fs::dir_ls(path = here::here(tdir, "data", "temp_datasets"), glob = "*datafake*csv") %>%
    basename()

  expected_qs_files <-
    c("standard_datafake1.csv", "standard_datafake2.csv", "standard_datafake3.csv")

  expect_equal(created_csv_files, expected_qs_files)
})

test_that("datafake1 has the default column names", {
  created_csv_files <-
    fs::dir_ls(path = here::here(tdir, "data", "temp_datasets"), glob = "*datafake*")

  df1 <-
    readr::read_csv(created_csv_files[1], show_col_types = FALSE) %>%
    names()

  expect_equal(df1, c("database_id", "occurrenceID", "scientificName", "decimalLatitude", "decimalLongitude"))
})


test_that("datafake3 has the default column names", {
  created_csv_files <-
    fs::dir_ls(path = here::here(tdir, "data", "temp_datasets"), glob = "*datafake*csv")

  df3 <-
    readr::read_csv(created_csv_files[3], show_col_types = FALSE) %>%
    names()

  expect_equal(df3, c("database_id", "scientificName", "decimalLatitude", "decimalLongitude"))
})

test_that("bdc_standardize_datasets missing required column", {
  result <- testthat::capture_error(bdc_standardize_datasets(metadata = metadata_missing_column, overwrite = TRUE, format = "csv", save_database = FALSE))

  expect_equal(any(class(result) %in% "error"), TRUE)
})

test_that("bdc_standardize_datasets already exist", {
  result_message <- testthat::capture_message(bdc_standardize_datasets(metadata = metadata, overwrite = FALSE, format = "csv", save_database = FALSE))

  expect_equal(any(class(result_message) %in% "condition"), TRUE)
})

test_that("bdc_standardize_datasets inform missing columns in the input file", {
  expect_message(bdc_standardize_datasets(metadata = wrong_metadata, overwrite = FALSE, format = "csv", save_database = FALSE), "defined in the metadata not present in the datafake4")

  expect_message(bdc_standardize_datasets(metadata = wrong_metadata, overwrite = FALSE, format = "csv", save_database = FALSE), "defined in the metadata do not match column names in the datafake4 file")
})

skip_on_cran()
skip_on_ci()

test_that("bdc_standardize_datasets can create 00_merged_datasets.csv", {
  bdc_standardize_datasets(metadata = metadata, overwrite = TRUE, format = "csv", save_database = TRUE)

  merged <- here::here("Output/Intermediate/00_merged_database.csv")

  expect_true(file.exists(merged))

  unlink(here::here("Output"), recursive = TRUE)
})

test_that("bdc_standardize_datasets can create 00_merged_datasets.qs", {
  bdc_standardize_datasets(metadata = metadata, overwrite = TRUE, format = "qs", save_database = TRUE)

  merged <- here::here("Output/Intermediate/00_merged_database.qs")

  expect_true(file.exists(merged))

  unlink(here::here("Output"), recursive = TRUE)
})

test_that("bdc_standardize_datasets throw an error when dataset names are not unique", {

  res <-
    capture_message(
      bdc_standardize_datasets(
        metadata = metadata_repeated_datasetName,
        overwrite = TRUE,
        format = "csv",
        save_database = FALSE
      )
    )

  expect_equal(res$message, "[ERROR]: Dataset names defined in the `datasetName` column must be unique.")

  unlink(here::here("Output"), recursive = TRUE)

})
