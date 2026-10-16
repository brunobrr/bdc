test_that("bdc_reword_countries() rewords names without printing the column specification", {
  countries <- data.frame(name_long = c("Brazil", "Bolivia"))

  expect_no_message(result <- bdc_reword_countries(countries))
  expect_equal(result$name_long, c("Brazil", "Bolivia"))
})
