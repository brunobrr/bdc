fakedates <-
  data.frame(
    common_date = c(1499, 20, 1885, 1985, 200002, 3),
    string_date = letters[1:6],
    lubris_date = seq(as.Date("2019-04-01"), as.Date("2019-04-06"), by = "1 day")
  )

test_that("bdc_parse_date can parse numerical dates", {
  num_dates <- bdc_parse_date(data = fakedates, col_to_test = "common_date")

  expect_equal(num_dates$.year, c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))

  expect_equal(num_dates$year_corrected, c(1499, NA, 1885, 1985, 2000, NA))
})

test_that("bdc_parse_date can parse character dates", {
  char_dates <- bdc_parse_date(data = fakedates, col_to_test = "string_date")

  expect_equal(char_dates$.year, c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))

  expect_identical(char_dates$year_corrected, as.double(c(NA, NA, NA, NA, NA, NA)))
})

test_that("bdc_parse_date can parse lubridate-format dates", {
  lubri_dates <- bdc_parse_date(data = fakedates, col_to_test = "lubris_date")

  expect_equal(lubri_dates$.year, c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))

  expect_identical(lubri_dates$year_corrected, c(2019, 2019, 2019, 2019, 2019, 2019))
})


test_that("bdc_parse_date with a specific year", {
  lubri_dates <- bdc_parse_date(data = fakedates, col_to_test = "common_date", year_threshold = 1990)

  expect_equal(lubri_dates$.year, c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
})

test_that("misuse of year argument", {
  expect_error(bdc_parse_date(data = fakedates, col_to_test = "common_date", year_threshold = "1990"))
})
