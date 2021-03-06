library(readr)
library(withr)
library(stringr)

context("standard dataset")

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

write_csv(datafake1, "datafake1.csv")
write_csv(datafake2, "datafake2.csv")
write_csv(datafake3, "datafake3.csv")

metadata <- tibble::tribble(
  ~datasetName, ~fileName, ~occurrenceID, ~scientificName, ~decimalLatitude, ~decimalLongitude,
  "datafake1", "datafake1.csv", "id", "species", "latitude", "longitude",
  "datafake2", "datafake2.csv", "id_number", "spp", "lat", "lon",
  "datafake3", "datafake3.csv", NA, "nome_das_especies", "y", "x"
)

with_dir(
  new = ".",
  code = {
    bdc_standardize_datasets(metadata = metadata)

    test_that("bdc_standardize_datasets can create qs files", {
      created_qs_files <-
        fs::dir_ls(path = here::here("data", "temp_datasets"), glob = "*datafake*") %>%
        basename()

      expected_qs_files <-
        c("standard_datafake1.qs", "standard_datafake2.qs", "standard_datafake3.qs")

      expect_equal(created_qs_files, expected_qs_files)
    })

    test_that("datafake1 has the default column names", {
      created_qs_files <-
        fs::dir_ls(path = here::here("data", "temp_datasets"), glob = "*datafake*")

      df1 <-
        qs::qread(created_qs_files[1]) %>%
        names()

      expect_equal(df1, c("database_id", "occurrenceID", "scientificName", "decimalLatitude", "decimalLongitude"))
    })

    test_that("datafake3 has the default column names", {
      created_qs_files <-
        fs::dir_ls(path = here::here("data", "temp_datasets"), glob = "*datafake*")

      df1 <-
        qs::qread(created_qs_files[3]) %>%
        names()

      expect_equal(df1, c("database_id", "scientificName", "decimalLatitude", "decimalLongitude"))
    })

    test_that("bdc_standardize_datasets can create 00_merged_datasets.qs", {
      merged <- here::here("Output/Intermediate/00_merged_database.qs")

      expect_true(file.exists(merged))
    })

    unlink(here::here("data"), recursive = TRUE)
    unlink(here::here("Output"), recursive = TRUE)
    csv <- fs::dir_ls(glob = "*.csv")
    unlink(csv)
  }
)
