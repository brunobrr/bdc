df <-
  data.frame(
    long = c("",  -15, 0, -181,  NA, -45, 200, -1000),
    lat =  c(-1 , -15, 0,   NA, -15,  NA, 92,  45)
  )

test_that("bdc_coordinates_empty flag as TRUE empty coordinates", {

  found <- bdc_coordinates_empty(df, lat = "lat", lon = "long")

  expected_res <- c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE)

  expect_equal(found$.coordinates_empty, expected_res)

})

