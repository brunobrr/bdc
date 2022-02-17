x <- data.frame(
  database_id = c("test_1", "test_2", "test_3", "test_4", "test_5"),
  .bdc_scientificName_empty = c(TRUE, TRUE, TRUE, FALSE, FALSE),
  .bdc_coordinates_empty = c(TRUE, FALSE, FALSE, FALSE, FALSE),
  .bdc_coordinates_outOfRange = c(TRUE, FALSE, FALSE, FALSE, FALSE),
  .summary = c(TRUE, FALSE, FALSE, FALSE, FALSE)
)

test_that("test if all columns starting with a dot were removed", {
  r <- bdc_filter_out_flags(data = x, col_to_remove = "all")
  expect_equal(ncol(r), 1)
})

test_that("test if only specific column is removed", {
  r <- bdc_filter_out_flags(data = x, col_to_remove = ".summary")
  expect_equal(ncol(r), 4)
})
