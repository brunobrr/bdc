context("standard dataset")

datafake1 <- tibble::tribble(
                  ~species, ~longitude,  ~latitude,   ~id,
  "Eragrostis maypurensis", -47.824956,  -13.17078, 3120L,
        "Mimosa sensitiva", -43.651389, -20.390833, 2756L,
  "Aegiphila integrifolia", -48.434722,  -7.195278, 2430L,
      "Rhodospatha venosa",        -60,         -2,  964L,
      "Ditassa succedanea", -43.615833, -18.007222, 2515L,
  "Sebastiania macrocarpa", -39.404722,  -8.454722,  786L,
        "Caladium bicolor", -34.960278,  -7.696667, 1928L,
      "Amaioua intermedia",   -45.0711,   -23.4339, 3493L,
     "Mikania officinalis", -48.801389, -17.024167, 3801L
  )

datafake2 <- tibble::tribble(
                      ~spp,       ~lon,       ~lat, ~id_number,
  "Eragrostis maypurensis", -47.824956,  -13.17078,      3120L,
        "Mimosa sensitiva", -43.651389, -20.390833,      2756L,
  "Aegiphila integrifolia", -48.434722,  -7.195278,      2430L,
      "Rhodospatha venosa",        -60,         -2,       964L,
      "Ditassa succedanea", -43.615833, -18.007222,      2515L,
  "Sebastiania macrocarpa", -39.404722,  -8.454722,       786L,
        "Caladium bicolor", -34.960278,  -7.696667,      1928L,
      "Amaioua intermedia",   -45.0711,   -23.4339,      3493L,
     "Mikania officinalis", -48.801389, -17.024167,      3801L
  )

datafake3 <- tibble::tribble(
        ~nome_das_especies,         ~x,         ~y,
  "Eragrostis maypurensis", -47.824956,  -13.17078,
        "Mimosa sensitiva", -43.651389, -20.390833,
  "Aegiphila integrifolia", -48.434722,  -7.195278,
      "Rhodospatha venosa",        -60,         -2,
      "Ditassa succedanea", -43.615833, -18.007222,
  "Sebastiania macrocarpa", -39.404722,  -8.454722,
        "Caladium bicolor", -34.960278,  -7.696667,
      "Amaioua intermedia",   -45.0711,   -23.4339,
     "Mikania officinalis", -48.801389, -17.024167
  )

write_csv(datafake1, "datafake1.csv")
write_csv(datafake2, "datafake2.csv")
write_csv(datafake3, "datafake3.csv")

metadata <- tibble::tribble(
  ~datasetName, ~File_name_to_load, ~occurrenceID,     ~scientificName, ~decimalLatitude, ~decimalLongitude,
   "datafake1",    "tests/testthat/datafake1.csv",          "id",           "species",       "latitude",       "longitude",
   "datafake2",    "tests/testthat/datafake2.csv",   "id_number",               "spp",            "lat",             "lon",
   "datafake3",    "tests/testthat/datafake3.csv",            NA, "nome_das_especies",              "y",               "x"
  )

devtools::load_all()

# Install and load packages
ipak(
  c(
    "tidyverse",
    "here",
    "fs",
    "vroom",
    "CoordinateCleaner",
    "rnaturalearth",
    "dplyr",
    "xml2",
    "rvest",
    "qs",
    "sf",
    "rnaturalearth"
  )
)

bdc_standardize_datasets(metadata = metadata)

test_that("bdc_standardize_datasets can create qs files", {

  created_qs_files <-
    fs::dir_ls(path = here::here("data", "temp"), glob = "*datafake*") %>%
    basename()

  expected_qs_files <-
    c("standard_datafake1.qs", "standard_datafake2.qs", "standard_datafake3.qs")

  expect_equal(created_qs_files, expected_qs_files)

})

test_that("datafake1 has the default column names", {

  created_qs_files <-
    fs::dir_ls(path = here::here("data", "temp"), glob = "*datafake*")

  df1 <-
    qs::qread(created_qs_files[1]) %>%
    names()

  expect_equal(df1, c("database_id", "occurrenceID", "scientificName", "decimalLatitude", "decimalLongitude"))

})

test_that("datafake3 has the default column names", {

  created_qs_files <-
    fs::dir_ls(path = here::here("data", "temp"), glob = "*datafake*")

  df1 <-
    qs::qread(created_qs_files[3]) %>%
    names()

  expect_equal(df1, c("database_id", "scientificName", "decimalLatitude", "decimalLongitude"))

})

rm_test_files <-
  metadata %>%
  pull(File_name_to_load) %>%
  basename()

rm_temp_files <-
  rm_test_files %>%
  str_replace(., ".csv", ".qs") %>%
  paste0(here::here("data", "temp"), "/standard_", .)

for (i in seq_along(rm_test_files)) {
  unlink(rm_test_files[i])
  unlink(rm_temp_files[i])
}
