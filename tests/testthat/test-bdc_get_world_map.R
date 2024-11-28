skip_on_ci()
skip_on_cran()

x <- bdc_get_world_map()

test_that("returning  a polygon data frame", {
  testthat::expect_equal(class(x)[1], "SpatialPolygonsDataFrame")
})

test_that("checking variables", {
  testthat::expect_equal(names(x), c("name_long", "iso2c"))
})
