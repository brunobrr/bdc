context("invalid xy")

# do not edit this data frame
df <-
  data.frame(
    database_id = c("a_1", "b_1", "c_1", "d_1", "e_1", "f_1"),
    scientificName = c("eu", "voce", "odair", NA, "dilma", "rainha"),
    decimalLongitude = c("", -15, 0, NA, NA, -45),
    decimalLatitude = c(-1, -15, 0, NA, -15, NA)
  )

df_invalid <-
  df %>%
  bdc_flag_invalid_xy(long = "decimalLongitude", lat = "decimalLatitude")

test_that("bdc_flag_invalid_xy create the column .invalid_xy", {

  test_col <- sum(names(df_invalid) %in% ".invalid_xy")

  expect_equal(test_col, 1)

})

test_that("bdc_flag_invalid_xy assign NA in decimalLatitude as TRUE in .invalid_xy", {

  coord <-
    df_invalid %>%
    filter(is.na(decimalLatitude)) %>%
    pull(.invalid_xy)

  expect_equal(coord, c(TRUE, TRUE))

})

test_that("bdc_flag_invalid_xy assign NA in decimalLongitude as TRUE in .invalid_xy", {

  coord <-
    df_invalid %>%
    filter(is.na(decimalLongitude)) %>%
    pull(.invalid_xy)

  expect_equal(coord, c(TRUE, TRUE, TRUE))

})
