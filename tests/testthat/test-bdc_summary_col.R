.missing_names <- c(TRUE, TRUE, TRUE, FALSE, FALSE)

.missing_coordinates <- c(TRUE, FALSE, FALSE, TRUE, FALSE)

x <- data.frame(.missing_names, .missing_coordinates)

test_that("bdc_summary_col can summarise the tested columns", {
  resultant <- bdc_summary_col(data = x)

  expect_equal(resultant$.summary, c(TRUE, FALSE, FALSE, FALSE, FALSE))
})

test_that("bdc_summary_col recomputes an existing .summary instead of duplicating it", {
  flags <- data.frame(
    id = 1:5,
    .missing_names = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    .missing_coordinates = c(TRUE, FALSE, FALSE, TRUE, FALSE)
  )
  # A stale summary that disagrees with the flags on every row but the first.
  stale <- flags
  stale$.summary <- c(TRUE, TRUE, TRUE, TRUE, TRUE)

  expect_message(
    updated <- bdc_summary_col(data = stale),
    "already exists"
  )
  fresh <- suppressMessages(bdc_summary_col(data = flags))

  expect_equal(sum(names(updated) == ".summary"), 1)
  expect_equal(names(updated), c("id", ".missing_names", ".missing_coordinates", ".summary"))
  expect_equal(updated$.summary, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(updated, fresh)
})

test_that("bdc_summary_col update path keeps .summary last when it was not", {
  x <- data.frame(
    .summary = c(FALSE, FALSE),
    id = c("a", "b"),
    .flag = c(TRUE, FALSE)
  )

  updated <- suppressMessages(bdc_summary_col(data = x))

  expect_equal(names(updated), c("id", ".flag", ".summary"))
  expect_equal(updated$.summary, c(TRUE, FALSE))
})
