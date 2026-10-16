# World borders come from the maps package.
skip_if_not_installed("maps")

# Five records with custom coordinate columns: three valid, one missing
# latitude and one latitude out of range.
records <- data.frame(
  lat = c(-19.9, -13.0, NA, 95, -10.0),
  lon = c(-40.6, -39.6, -49.6, -35.6, -50.0),
  .flag = c(TRUE, FALSE, TRUE, TRUE, FALSE)
)

points_layer <- function(map) {
  # Layer 1 holds the world borders, layer 2 the records.
  ggplot2::layer_data(map, 2)
}

test_that("bdc_quickmap() plots valid records in a single colour by default", {
  map <- bdc_quickmap(data = records, lat = "lat", lon = "lon")

  expect_s3_class(map, "ggplot")
  points <- points_layer(map)
  # Records with missing or out-of-range coordinates are removed.
  expect_equal(points$x, c(-40.6, -39.6, -50.0))
  expect_equal(points$y, c(-19.9, -13.0, -10.0))
  expect_length(unique(points$colour), 1)
})

test_that("bdc_quickmap() colours records by a logical flag column", {
  map <- bdc_quickmap(
    data = records,
    lat = "lat",
    lon = "lon",
    col_to_map = ".flag"
  )

  expect_s3_class(map, "ggplot")
  points <- points_layer(map)
  expect_equal(nrow(points), 3)
  # Valid records flagged TRUE, FALSE and FALSE.
  expect_equal(points$colour[2], points$colour[3])
  expect_false(points$colour[1] == points$colour[2])
})

test_that("bdc_quickmap() rejects a col_to_map that is neither a column nor a colour", {
  skip("a non-existent column only fails when the map is drawn (#331)")

  expect_error(
    bdc_quickmap(
      data = records,
      lat = "lat",
      lon = "lon",
      col_to_map = "not_a_column"
    ),
    "not_a_column"
  )
})
