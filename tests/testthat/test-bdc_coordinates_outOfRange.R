test_that("test based on function example", {
  decimalLatitude <- c(-185.111, -43.34, "", -21.8069444)
  decimalLongitude <- c(-45.4, -39.6, -20.5243, -440.9055555)
  x <- data.frame(decimalLatitude, decimalLongitude)

  r <- bdc_coordinates_outOfRange(
    data = x,
    lat = "decimalLatitude",
    lon = "decimalLongitude"
  )

  expect_equal(r$.coordinates_outOfRange, c(FALSE, TRUE, TRUE, FALSE))
  expect_equal(
    names(r),
    c(
      "decimalLatitude",
      "decimalLongitude",
      ".coordinates_outOfRange"
    )
  )
})
