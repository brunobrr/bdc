context("outside xy")

# do not edit this data frame
df <-
  data.frame(
    database_id = c("a_1", "b_1", "c_1"),
    scientificName = c("eu", "voce", "odair"),
    decimalLongitude = c(-1, -181, 0),
    decimalLatitude = c(-1, -15, -92)
  )

df_outside <-
  df %>%
  bdc_flag_outside_xy()

test_that("bdc_flag_outside_xy create the column .outside_xy", {

  test_col <- sum(names(df_outside) %in% ".outside_xy")

  expect_equal(test_col, 1)

})

test_that("bdc_flag_invalid_xy assign .outside_xy as TRUE when longitude fall outside the world", {

  coord <-
    df_outside %>%
    filter(decimalLongitude == -181) %>%
    pull(.outside_xy)

  expect_equal(coord, TRUE)

})

test_that("bdc_flag_invalid_xy assign .outside_xy as TRUE when latitude fall outside the world", {

  coord <-
    df_outside %>%
    filter(decimalLatitude == -92) %>%
    pull(.outside_xy)

  expect_equal(coord, TRUE)

})
