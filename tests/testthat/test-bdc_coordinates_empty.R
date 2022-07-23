df <-
  data.frame(
    long = c("", -15, 0, -181, NA, -45, 200, -1000),
    lat = c(-1, -15, 0, NA, -15, NA, 92, "45"),
    locality = c(letters[1:7], "áàéèíìóòúÚçãâ")
  )

test_that("bdc_coordinates_empty flags as TRUE empty coordinates", {

  found <- bdc_coordinates_empty(df, lat = "lat", lon = "long")

  expected_res <- c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE)

  expect_equal(found$.coordinates_empty, expected_res)

})

test_that("bdc_coordinates_empty creates .coordinates_empty column", {

  found <- bdc_coordinates_empty(df, lat = "lat", lon = "long")

  expect_true(".coordinates_empty" %in% names(found))

})

test_that("bdc_coordinates_empty returns columns of the original dataset", {

  found <- bdc_coordinates_empty(df, lat = "lat", lon = "long")

  found$.coordinates_empty <- NULL

  expect_equal(names(df), names(found))

})

test_that("bdc_coordinates_empty works only with data.frame objects", {

  df <- letters

  expect_error(bdc_coordinates_empty(df, lat = "lat", lon = "long"))

  expect_error(bdc_coordinates_empty(df, lat = "lat", lon = "long"))

})

test_that("bdc_coordinates_empty reports wrong column names in the data", {

  expect_error(bdc_coordinates_empty(df, lat = "lati", lon = "long"))

  expect_error(bdc_coordinates_empty(df, lat = "lat", lon = "longi"))

})

