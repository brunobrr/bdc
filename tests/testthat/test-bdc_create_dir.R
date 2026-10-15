output_dirs <- c("Check", "Intermediate", "Report", "Figures")

test_that("bdc_create_dir() creates the Output folders under the project root", {
  root <- local_temp_project()

  bdc_create_dir()

  expect_true(all(dir.exists(file.path(root, "Output", output_dirs))))
})

test_that("bdc_create_dir() writes to the project root when run from a subfolder", {
  root <- local_temp_project(subdir = "reports")

  bdc_create_dir()

  expect_true(all(dir.exists(file.path(root, "Output", output_dirs))))
  expect_false(dir.exists(file.path(root, "reports", "Output")))
})
