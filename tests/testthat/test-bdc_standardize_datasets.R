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

# Lacks the `y` column that the metadata maps to decimalLatitude.
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

# Run the calling test in a temporary project holding the four input CSVs.
# Returns the input paths, named by dataset.
local_fake_inputs <- function(.local_envir = parent.frame()) {
  root <- local_temp_project(.local_envir = .local_envir)
  input_dir <- file.path(root, "Input")
  dir.create(input_dir)

  inputs <- list(
    datafake1 = datafake1,
    datafake2 = datafake2,
    datafake3 = datafake3,
    datafake4 = datafake4
  )
  paths <- file.path(input_dir, paste0(names(inputs), ".csv"))
  names(paths) <- names(inputs)
  for (i in seq_along(inputs)) {
    readr::write_csv(inputs[[i]], paths[[i]])
  }
  paths
}

fake_metadata <- function(paths) {
  tibble::tribble(
    ~datasetName, ~fileName, ~occurrenceID, ~scientificName, ~decimalLatitude, ~decimalLongitude,
    "datafake1", paths[["datafake1"]], "id", "species", "latitude", "longitude",
    "datafake2", paths[["datafake2"]], "id_number", "spp", "lat", "lon",
    "datafake3", paths[["datafake3"]], NA, "nome_das_especies", "y", "x"
  )
}

id_prefixes <- function(x) {
  sort(unique(sub("_[0-9]+$", "", x$database_id)))
}

test_that("each call returns only the datasets in its own metadata", {
  paths <- local_fake_inputs()
  metadata <- fake_metadata(paths)

  first <- suppressMessages(
    bdc_standardize_datasets(metadata[1:2, ], format = "csv")
  )
  second <- suppressMessages(
    bdc_standardize_datasets(metadata[3, ], format = "csv")
  )

  expect_equal(id_prefixes(first), c("datafake1", "datafake2"))
  expect_equal(id_prefixes(second), "datafake3")
  expect_equal(nrow(second), 9)
})

test_that("a dataset with mismatched columns is excluded and named, never replaced by a stale copy", {
  paths <- local_fake_inputs()
  metadata <- fake_metadata(paths)

  # A successful run of `datafake3` leaves its standardized copy behind.
  suppressMessages(bdc_standardize_datasets(metadata, format = "csv"))

  # Now `datafake3` points at a file without the `y` column.
  broken <- metadata
  broken$fileName[3] <- paths[["datafake4"]]

  msgs <- capture_messages(
    result <- bdc_standardize_datasets(broken, format = "csv")
  )
  expect_match(msgs, "\\[ERROR\\].*datafake3", all = FALSE)
  expect_equal(id_prefixes(result), c("datafake1", "datafake2"))
})

test_that("an existing merged file is returned when overwrite = FALSE", {
  paths <- local_fake_inputs()
  metadata <- fake_metadata(paths)

  for (format in c("csv", "qs2")) {
    saved <- suppressMessages(bdc_standardize_datasets(
      metadata,
      format = format, overwrite = TRUE, save_database = TRUE
    ))
    merged_file <- here::here(
      "Output", "Intermediate", paste0("00_merged_database.", format)
    )
    expect_true(file.exists(merged_file))

    expect_message(
      reloaded <- bdc_standardize_datasets(
        metadata,
        format = format, overwrite = FALSE
      ),
      "already exists"
    )
    expect_false(is.null(reloaded), label = format)
    expect_equal(reloaded, saved, ignore_attr = TRUE, label = format)
  }
})

test_that("a metadata row missing a required field errors with the dataset and field names", {
  paths <- local_fake_inputs()
  metadata <- fake_metadata(paths)
  metadata$decimalLongitude[2] <- NA

  expect_error(
    suppressMessages(bdc_standardize_datasets(metadata, format = "csv")),
    "datafake2.*decimalLongitude"
  )
})

test_that("standardized datasets are renamed to Darwin Core in both formats", {
  paths <- local_fake_inputs()
  metadata <- fake_metadata(paths)

  for (format in c("csv", "qs2")) {
    result <- suppressMessages(
      bdc_standardize_datasets(metadata, format = format)
    )

    expect_equal(
      names(result),
      c("database_id", "occurrenceID", "scientificName", "decimalLatitude", "decimalLongitude"),
      label = format
    )
    expect_equal(nrow(result), 27, label = format)
    expect_equal(result$occurrenceID[1:2], c("3120", "2756"), label = format)
    expect_true(all(is.na(result$occurrenceID[19:27])), label = format)
    expect_equal(result$decimalLatitude[1], "-13.17078", label = format)
  }
})

test_that("the missing-column message lists the columns absent from the file", {
  paths <- local_fake_inputs()
  metadata <- fake_metadata(paths)
  metadata$fileName[3] <- paths[["datafake4"]]

  msgs <- capture_messages(
    bdc_standardize_datasets(metadata[3, ], format = "csv")
  )
  expect_match(
    msgs,
    "Column\\(s\\) `y` defined in the metadata not present in the datafake3",
    all = FALSE
  )
})

test_that("dataset names must be unique", {
  paths <- local_fake_inputs()
  metadata <- fake_metadata(paths)
  metadata$datasetName[3] <- "datafake1"

  expect_error(
    bdc_standardize_datasets(metadata, format = "csv"),
    "must be unique"
  )
})

test_that("a dataset whose file cannot be read is excluded and named", {
  paths <- local_fake_inputs()
  metadata <- fake_metadata(paths)
  metadata$fileName[2] <- file.path(dirname(paths[[1]]), "missing.csv")

  msgs <- capture_messages(
    result <- bdc_standardize_datasets(metadata, format = "csv")
  )
  expect_match(msgs, "\\[ERROR\\].*datafake2", all = FALSE)
  expect_equal(id_prefixes(result), c("datafake1", "datafake3"))
})
