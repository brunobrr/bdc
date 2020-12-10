




data <- data.frame(lon = c(-55.389, -13.897, 30.678, 90.675) , lat = c(-21.345, 23.567, 16.798, -10.468))


test_that("bdc_round_dec test round coordinates", {
  expect_equal(bdc_round_dec(data = data, lon = "lon", lat = "lat", ndec = 0), tibble(.ndec0 = c(TRUE, TRUE, TRUE, TRUE), .ndec_all = c(TRUE, TRUE, TRUE, TRUE)))
  expect_equal(bdc_round_dec(data = data, lon = "lon", lat = "lat", ndec = 1), tibble(.ndec1 = c(TRUE, TRUE, TRUE, TRUE), .ndec_all = c(TRUE, TRUE, TRUE, TRUE)))
  expect_equal(bdc_round_dec(data = data, lon = "lon", lat = "lat", ndec = 2), tibble(.ndec2 = c(TRUE, TRUE, TRUE, TRUE), .ndec_all = c(TRUE, TRUE, TRUE, TRUE)))
  expect_equal(bdc_round_dec(data = data, lon = "lon", lat = "lat", ndec = 3), tibble(.ndec3 = c(FALSE, FALSE, FALSE, FALSE), .ndec_all = c(FALSE, FALSE, FALSE, FALSE)))
  expect_equal(bdc_round_dec(data = data, lon = "lon", lat = "lat"), tibble(.ndec0 = c(TRUE, TRUE, TRUE, TRUE), .ndec1 = c(TRUE, TRUE, TRUE, TRUE), .ndec2 = c(TRUE, TRUE, TRUE, TRUE), .ndec_all = c(TRUE, TRUE, TRUE, TRUE)))
  }
)
