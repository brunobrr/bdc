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

test_that("bdc_year_from_eventDate can detect four-digits year", {
  resultant <-
    bdc_year_from_eventDate(data = x, eventDate = "collection_date")

  expect_equal(resultant$year, c(NA, 2015, 2013, 2013, NA, 2013, 1))
})
