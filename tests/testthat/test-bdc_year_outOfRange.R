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
