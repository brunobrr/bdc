.missing_names <- c(TRUE, TRUE, TRUE, FALSE, FALSE)

.missing_coordinates <- c(TRUE, FALSE, FALSE, TRUE, FALSE)

x <- data.frame(.missing_names, .missing_coordinates)

test_that("bdc_summary_col can summarise the tested columns", {
  resultant <- bdc_summary_col(data = x)

  expect_equal(resultant$.summary, c(TRUE, FALSE, FALSE, FALSE, FALSE))
})
