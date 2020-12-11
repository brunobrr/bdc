context("invalid xy")

df <-
  data.frame(
    long = c("", -15, 0, -181,  NA, -45, 200, -1000),
    lat = c(-1 , -15, 0,   NA, -15,  NA, 92, 45)
  )

df_invalid <-
  df %>%
  bdc_flag_invalid_xy(long = "long", lat = "lat")

test_that("bdc_flag_invalid_xy create the column .invalid_xy", {

  expect_equal(names(df_invalid), c("long", "lat", ".invalid_xy"))

})


test_that("bdc_flag_invalid_xy flag as TRUE invalid coordinates", {

  found <- pull(df_invalid, .invalid_xy)
  expected <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)

  expect_equal(found, expected)

})

