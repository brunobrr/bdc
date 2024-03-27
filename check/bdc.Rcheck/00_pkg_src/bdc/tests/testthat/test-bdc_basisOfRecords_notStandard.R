x <- data.frame(
  basisOfRecord = c(
    "FOSSIL_SPECIMEN",
    "UNKNOWN",
    "RON",
    NA,
    "Specimen",
    "PRESERVED_SPECIMEN"
  )
)


r2 <- bdc_basisOfRecords_notStandard(
  data = x,
  basisOfRecord = "basisOfRecord",
  names_to_keep = c("Fossil", "Ron", "RON", "S", "unknown", "FOSSEL")
)


r3 <- bdc_basisOfRecords_notStandard(
  data = x,
  basisOfRecord = "basisOfRecord"
)

test_that("bdc_basisOfRecords correct use of arguments", {
  expect_message(
    bdc_basisOfRecords_notStandard(
      data = x,
      basisOfRecord = "basisOfRecord",
      names_to_keep = "all"
    )
  )
})

test_that("bdc_basisOfRecords results using a selected names to keep", {
  expect_equal(r2$.basisOfRecords_notStandard, c(F, T, T, F, F, F))
})

test_that("bdc_basisOfRecords_notStandard number of expected column", {
  expect_equal(ncol(r2), 2)
})

test_that("bdc_basisOfRecords results with default names to keep", {
  expect_equal(r2$.basisOfRecords_notStandard, c(F, T, T, F, F, F))
})

test_that("bdc_basisOfRecords testing default argument", {
  expect_equal(r3$.basisOfRecords_notStandard, c(F, T, F, T, T, T))
})

test_that("bdc_basisOfRecords expected class", {
  expect_type(r3, "list")
})
