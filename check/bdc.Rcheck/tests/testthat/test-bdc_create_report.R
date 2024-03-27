database_id <- c("test_1", "test_2", "test_3", "test_4", "test_5")
.scientificName_empty <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
.coordinates_empty <- c(TRUE, FALSE, FALSE, TRUE, FALSE)
.coordinates_outOfRange <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
.summary <- c(TRUE, FALSE, FALSE, FALSE, FALSE)

x <- data.frame(
  database_id,
  .scientificName_empty,
  .coordinates_empty,
  .coordinates_outOfRange,
  .summary
)

report <-
  bdc_create_report(
    data = x,
    database_id = "database_id",
    workflow_step = "prefilter",
    save_report = FALSE
  )


test_that("test report dimensions" , {
  testthat::expect_equal(dim(report$x$data), c(6, 5))
  
})

y <-
  data.frame(
    .coordinates_outOfRange = c(FALSE, FALSE, TRUE),
    transposed_coordinates = c(TRUE, FALSE, TRUE)
  )

test_that("test coordinates transposed" , {
  testthat::expect_equal(dim(report$x$data), c(6, 5))
  
})
 