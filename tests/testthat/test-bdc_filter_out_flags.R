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

flags <- data.frame(
  id = c("a", "b"),
  kingdom = c("Plantae", "Animalia"),
  .scientificName_empty = c(TRUE, FALSE),
  .coordinates_empty = c(FALSE, TRUE),
  .summary = c(FALSE, FALSE)
)

test_that("col_to_remove = 'all' removes every dot-prefixed column and keeps the rest", {
  r <- suppressMessages(bdc_filter_out_flags(data = flags, col_to_remove = "all"))

  expect_equal(r, flags[c("id", "kingdom")])
})

test_that("col_to_remove = 'all' is the default", {
  r <- suppressMessages(bdc_filter_out_flags(data = flags))

  expect_equal(names(r), c("id", "kingdom"))
})

test_that("a specific name removes only that column, dot-prefixed or not", {
  r <- suppressMessages(
    bdc_filter_out_flags(data = flags, col_to_remove = ".coordinates_empty")
  )
  expect_equal(r, flags[c("id", "kingdom", ".scientificName_empty", ".summary")])

  r <- suppressMessages(
    bdc_filter_out_flags(data = flags, col_to_remove = "kingdom")
  )
  expect_equal(names(r), c("id", ".scientificName_empty", ".coordinates_empty", ".summary"))
})

test_that("a vector of names removes only those columns", {
  skip("bdc_filter_out_flags() errors on more than one name (#310)")
  r <- suppressMessages(
    bdc_filter_out_flags(
      data = flags,
      col_to_remove = c(".scientificName_empty", ".summary")
    )
  )

  expect_equal(r, flags[c("id", "kingdom", ".coordinates_empty")])
})

test_that("an unknown name is ignored and the data is returned unchanged", {
  expect_no_error(
    r <- suppressMessages(
      bdc_filter_out_flags(data = flags, col_to_remove = ".not_a_column")
    )
  )

  expect_equal(r, flags)
})
