
collection_date <-
  c(
    NA,
    "31/12/2015",
    "2013-06-13T00:00:00Z",
    "2013-06-20",
    "",
    "2013",
    "0001-01-00",
    "-"
  )
x <- data.frame(collection_date)

res <- bdc_eventDate_empty(data = x, eventDate = "collection_date")

test_that("bdc_eventDate_empty output is a data frame?", {
  # A data frame is built from a list:
  expect_type(res, "list")
})

test_that("bdc_eventDate_empty: number of row of the input and output", {
  expect_identical(nrow(res), nrow(x))
})


test_that("bdc_eventDate_empty check if the output column was created", {
  expect_identical(res$.eventDate_empty, c(F, T, T, T, F, T, T, F))
})

test_that("bdc_eventDate_empty results", {
  expect_identical(colnames(res), c("collection_date", ".eventDate_empty"))
})
