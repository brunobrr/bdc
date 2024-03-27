polygon.x <- rnorm(100, -0, 50)
polygon.y <- rnorm(100, 20, 30)
group <- rnorm(100, 50, 50)
data <- data.frame(group, polygon.x, polygon.y)

bdc_coordinates_empty(data, lat = "polygon.y", lon = "polygon.x")

test_that("plot_quick_map", {
  plot_test <- bdc_quickmap(data = data, lon = "polygon.x", lat = "polygon.y")

  x <- try(print(plot_test))

  expect_equal(any(class(x) %in% "ggplot"), TRUE)
})
