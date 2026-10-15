collection_date <-
  c(
    NA,
    "31/12/2015",
    "2013-06-13T00:00:00Z",
    "2013-06-20",
    "",
    "2013",
    "0001-01-00"
  )

x <- data.frame(collection_date)

test_that("bdc_year_outOfRange detect years out of range", {
  resultant <-
    bdc_year_outOfRange(data = x, eventDate = "collection_date")

  expect_equal(
    resultant$.year_outOfRange,
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
  )
})

future_year <- as.character(as.integer(format(Sys.Date(), "%Y")) + 1)

test_that("missing dates and the threshold year are compliant with a numeric threshold", {
  x <- data.frame(date = c(NA, "", "1900", "1901", future_year, "1899"))

  resultant <-
    suppressMessages(
      bdc_year_outOfRange(data = x, eventDate = "date", year_threshold = 1900)
    )

  expect_equal(
    resultant$.year_outOfRange,
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("missing dates and the lower limit are compliant when year_threshold is NULL", {
  x <- data.frame(date = c(NA, "", "1600", "1601", future_year, "1599"))

  resultant <-
    suppressMessages(
      bdc_year_outOfRange(data = x, eventDate = "date", year_threshold = NULL)
    )

  expect_equal(
    resultant$.year_outOfRange,
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("a non-numeric year_threshold raises the documented error", {
  x <- data.frame(date = c("2000", "1850"))

  expect_error(
    bdc_year_outOfRange(data = x, eventDate = "date", year_threshold = "1900"),
    "not numeric"
  )
})

test_that("a year_threshold without four digits raises the documented error", {
  x <- data.frame(date = c("2000", "1850"))

  expect_error(
    bdc_year_outOfRange(data = x, eventDate = "date", year_threshold = 190),
    "four digits"
  )
  expect_error(
    bdc_year_outOfRange(data = x, eventDate = "date", year_threshold = 19000),
    "four digits"
  )
})
