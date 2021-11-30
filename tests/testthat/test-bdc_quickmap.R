context ("quick map")

lon = rnorm(100, -0,50)
lat = rnorm(100, 20, 30)
group = rnorm(100, 50, 50)
data <- data.frame(group, lat, lon)

data_empty <- bdc_coordinates_empty(data, lat = "lat", lon = "lon")

data_outOfRange <- bdc_coordinates_outOfRange(data, lat = "lat", lon = "lon")

data_empty_out <- bdc_coordinates_outOfRange(data_empty, lat = "lat", lon = "lon")

test_that("plot_quick_map", {
  
    plot_test <-  bdc_quickmap(data, lon= "lon", lat= "lat")
    
    x <- try(print(plot_test))
    
    expect_equal(any(class(x) %in% "ggplot"), TRUE)
    
})

test_that("color", {
  
  plot_test <-  bdc_quickmap(data, lon= "lon", lat= "lat", col_to_map = "blue")
  
  x <- try(print(plot_test))
  
  expect_equal(any(class(x) %in% "ggplot"), TRUE)
  
})

test_that("failing plot", {
  
  plot_test <-  bdc_quickmap(data, lon= "lon", lat= "lat", col_to_map = names(data))
  
  x <- try(print(plot_test))
  
  expect_equal(any(class(x) %in% "ggplot"), FALSE)
  
})

test_that("coordinate_empty and out of range test", {
  
  plot_test <- bdc_quickmap(data_empty_out, lon= "lon", lat= "lat")
  
  x <- try(print(plot_test))
  
  expect_equal(any(class(x) %in% "ggplot"), TRUE)
  
})

test_that("data with only coordinate_empty ", {
  
  plot_test <- testthat::capture_error(bdc_quickmap(data_empty, lon= "lon", lat= "lat"))
  
  expect_equal(any(class(plot_test) %in% "error"), TRUE)
  
})

test_that("data with only coordinates_outOfRange", {
  
  plot_test <- testthat::capture_error(bdc_quickmap(data_outOfRange, lon= "lon", lat= "lat"))
  
  expect_equal(any(class(plot_test) %in% "error"), TRUE)
  
})

