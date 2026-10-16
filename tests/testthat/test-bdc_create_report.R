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

test_that("test coordinates transposed", {
  testthat::expect_equal(dim(report$x$data), c(6, 5))
})

# Transposed coordinates are read from the project root ----------------------

report_descriptions <- function(data) {
  report <- suppressMessages(
    bdc_create_report(data = data, workflow_step = "prefilter")
  )
  report$x$data$Description
}

test_that("bdc_create_report() lists transposed coordinates from a subfolder", {
  skip_on_cran()
  skip_if_no_natural_earth()
  local_temp_project(subdir = "reports")
  flagged <- flag_transposed(transposed_input)

  expect_true(
    "Records with transposed geographic coordinates" %in%
      report_descriptions(flagged)
  )
})

test_that("bdc_create_report() lists transposed coordinates from the root", {
  skip_on_cran()
  skip_if_no_natural_earth()
  local_temp_project()
  flagged <- flag_transposed(transposed_input)

  expect_true(
    "Records with transposed geographic coordinates" %in%
      report_descriptions(flagged)
  )
})

# workflow_step validation ----------------------------------------------------

test_that("bdc_create_report() requires a valid workflow_step", {
  local_temp_project()
  steps <- "prefilter.*taxonomy.*space.*time"

  expect_error(bdc_create_report(x), steps)
  # A partial match must not silently skip every step
  expect_error(bdc_create_report(x, workflow_step = "pre"), steps)
  expect_error(bdc_create_report(x, workflow_step = "taxa"), steps)
  expect_error(
    bdc_create_report(x, workflow_step = c("prefilter", "space")),
    steps
  )
})

# Report contents per workflow step -------------------------------------------
#
# The report is a DT widget. Its table, `$x$data`, starts with DT's row-name
# column and ends with a blank row and a footnote giving the record count.

report_table <- function(report) {
  report$x$data[-1]
}

report_body <- function(report) {
  utils::head(report_table(report), -2)
}

report_footnote <- function(report) {
  utils::tail(report_table(report)$Description, 1)
}

prefilter_input <- data.frame(
  database_id = paste0("gbif_", 1:5),
  .summary = TRUE,
  .coordinates_country_inconsistent = c(TRUE, TRUE, TRUE, TRUE, FALSE),
  .scientificName_empty = c(TRUE, TRUE, TRUE, FALSE, TRUE),
  .coordinates_empty = c(TRUE, FALSE, TRUE, TRUE, TRUE),
  .coordinates_outOfRange = TRUE,
  .basisOfRecords_notStandard = c(FALSE, TRUE, TRUE, FALSE, TRUE)
)

test_that("bdc_create_report() lists each prefilter test and the summary", {
  local_temp_project()
  report <- bdc_create_report(prefilter_input, workflow_step = "prefilter")
  body <- report_body(report)

  expect_equal(
    body$Test_name,
    c(
      ".scientificName_empty", ".coordinates_empty",
      ".coordinates_outOfRange", ".basisOfRecords_notStandard",
      ".coordinates_country_inconsistent", ".summary"
    )
  )
  # .summary is recomputed from the tests, not read from the input
  expect_equal(as.numeric(body$Records_flagged), c(1, 1, 0, 2, 1, 4))
  expect_equal(
    as.numeric(body[["perc_number_records(*)"]]),
    c(20, 20, 0, 40, 20, 80)
  )
  expect_equal(body$Description[6], "Summary of all tests")
  expect_match(report_footnote(report), "5 records")
})

space_input <- data.frame(
  database_id = paste0("gbif_", 1:4),
  .val = TRUE,
  .cap = c(TRUE, TRUE, TRUE, FALSE),
  .equ = c(FALSE, TRUE, TRUE, TRUE),
  .zer = c(FALSE, FALSE, TRUE, TRUE),
  .summary = c(FALSE, FALSE, TRUE, FALSE)
)

test_that("bdc_create_report() lists each space test", {
  local_temp_project()
  report <- bdc_create_report(space_input, workflow_step = "space")
  body <- report_body(report)
  tests <- body[body$Test_name != ".summary", ]

  # Documented order; .val is not one of the reported space tests
  expect_equal(tests$Test_name, c(".equ", ".zer", ".cap"))
  expect_equal(as.numeric(tests$Records_flagged), c(1, 2, 1))
  expect_equal(
    tests$Description,
    c(
      "Identical coordinates", "Plain zeros",
      "Records around country capital centroid"
    )
  )
  expect_match(report_footnote(report), "4 records")
})

test_that("bdc_create_report() summarises only the tests of the step", {
  local_temp_project()
  # .uncer_terms comes from the taxonomy step, so the space report ignores it
  x <- data.frame(
    database_id = paste0("gbif_", 1:4),
    .uncer_terms = c(FALSE, FALSE, TRUE, TRUE),
    .rou = c(TRUE, TRUE, TRUE, FALSE)
  )
  report <- bdc_create_report(x, workflow_step = "space")
  body <- report_body(report)

  expect_equal(body$Test_name, c(".rou", ".summary"))
  expect_equal(as.numeric(body$Records_flagged), c(1, 1))
  expect_equal(sum(!bdc_summary_col(x)$.summary), 3)
})

test_that("bdc_create_report() adds the summary row to the space report", {
  local_temp_project()
  report <- bdc_create_report(space_input, workflow_step = "space")
  body <- report_body(report)

  expect_equal(utils::tail(body$Test_name, 1), ".summary")
  expect_equal(as.numeric(utils::tail(body$Records_flagged, 1)), 3)
})

time_input <- data.frame(
  database_id = paste0("gbif_", 1:4),
  .year_outOfRange = c(TRUE, FALSE, FALSE, TRUE),
  .eventDate_empty = c(TRUE, FALSE, TRUE, TRUE)
)

test_that("bdc_create_report() lists each time test and the summary", {
  local_temp_project()
  report <- bdc_create_report(time_input, workflow_step = "time")
  body <- report_body(report)

  expect_equal(
    body$Test_name,
    c(".eventDate_empty", ".year_outOfRange", ".summary")
  )
  expect_equal(as.numeric(body$Records_flagged), c(1, 2, 2))
  expect_equal(as.numeric(body[["perc_number_records(*)"]]), c(25, 50, 50))
  expect_match(report_footnote(report), "4 records")
})

# Columns left by bdc_clean_names() (.uncer_terms) and bdc_query_names_taxadb()
# (notes)
taxonomy_input <- data.frame(
  database_id = paste0("gbif_", 1:8),
  scientificName = c(
    "Puma concolor", "Puma concolor", "Felis concolor", "Puma concolr",
    "Xus yus", "Ambiguus nomen", "Orphan synonym", "Puma cf. concolor"
  ),
  notes = c(
    "accepted", "accepted", "accepted | replaceSynonym",
    "accepted | wasMisspelled", "notFound", "multipleAccepted",
    "synonym | noAcceptedName", "accepted"
  ),
  .uncer_terms = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
)

test_that("bdc_create_report() lists the documented taxonomy entries", {
  local_temp_project()
  report <- bdc_create_report(taxonomy_input, workflow_step = "taxonomy")
  body <- report_body(report)

  expect_equal(
    body$notes,
    c(
      "accepted", "accepted | replaceSynonym", "accepted | wasMisspelled",
      "multipleAccepted", "notFound", "synonym | noAcceptedName",
      "taxo_uncer"
    )
  )
  expect_equal(as.numeric(body$number_of_records), c(3, 1, 1, 1, 1, 1, 1))
  expect_equal(
    as.numeric(body[["perc_number_records(*)"]]),
    c(37.5, 12.5, 12.5, 12.5, 12.5, 12.5, 12.5)
  )
  # Accepted names read as "valid", unresolved names as "invalid" and names
  # needing inspection as "check"
  expect_equal(
    sub(":.*", "", body$Description),
    c("valid", "valid", "valid", "invalid", "invalid", "check", "check")
  )
  expect_equal(
    body$Description[body$notes == "taxo_uncer"],
    "check: doubtful taxonomic identification"
  )
  expect_equal(
    body$Description[body$notes == "notFound"],
    paste(
      "invalid: name not found in the taxonomic database or a",
      "non-interpretable name"
    )
  )
  expect_match(report_footnote(report), "8 records")
})

test_that("bdc_create_report() reports taxonomy without name-cleaning flags", {
  local_temp_project()
  harmonized_only <- taxonomy_input[names(taxonomy_input) != ".uncer_terms"]
  report <- bdc_create_report(harmonized_only, workflow_step = "taxonomy")
  body <- report_body(report)

  expect_false("taxo_uncer" %in% body$notes)
  expect_equal(sum(as.numeric(body$number_of_records)), 8)
})

# save_report = TRUE ----------------------------------------------------------

read_saved_report <- function(path) {
  as.data.frame(readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = character(),
    progress = FALSE
  ))
}

expect_saved_report <- function(data, step, file) {
  root <- local_temp_project()
  expect_message(
    report <- bdc_create_report(data, workflow_step = step, save_report = TRUE),
    "Output.Report"
  )
  path <- file.path(root, "Output", "Report", file)

  expect_true(file.exists(path))
  expect_equal(read_saved_report(path), report_table(report), ignore_attr = TRUE)
}

test_that("bdc_create_report() saves the prefilter report it returns", {
  expect_saved_report(prefilter_input, "prefilter", "01_Report_Prefilter.csv")
})

test_that("bdc_create_report() saves the taxonomy report it returns", {
  expect_saved_report(taxonomy_input, "taxonomy", "02_Report_taxonomy.csv")
})

test_that("bdc_create_report() saves the space report it returns", {
  expect_saved_report(space_input, "space", "03_Report_space.csv")
})

test_that("bdc_create_report() saves the time report it returns", {
  expect_saved_report(time_input, "time", "04_Report_time.csv")
})

test_that("bdc_create_report() does not write a report by default", {
  root <- local_temp_project()
  bdc_create_report(prefilter_input, workflow_step = "prefilter")

  expect_false(dir.exists(file.path(root, "Output", "Report")))
})

# Documented errors -----------------------------------------------------------

test_that("bdc_create_report() refuses to report when nothing was flagged", {
  local_temp_project()
  all_pass <- list(
    prefilter = data.frame(database_id = "gbif_1", .coordinates_empty = TRUE),
    space = data.frame(database_id = "gbif_1", .equ = TRUE),
    time = data.frame(database_id = "gbif_1", .eventDate_empty = TRUE)
  )

  for (step in names(all_pass)) {
    expect_error(
      bdc_create_report(all_pass[[step]], workflow_step = step),
      "No records flagged"
    )
  }
})

test_that("bdc_create_report() requires test columns starting with '.'", {
  local_temp_project()
  no_tests <- data.frame(database_id = "gbif_1", scientificName = "Puma")

  for (step in c("prefilter", "space", "time")) {
    expect_error(
      bdc_create_report(no_tests, workflow_step = step),
      "At least one column"
    )
  }
})
