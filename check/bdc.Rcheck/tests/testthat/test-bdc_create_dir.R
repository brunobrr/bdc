## skip("dont run")
bdc_create_dir()

test_that("bdc dir exists", {
  expect_true(dir.exists(here::here("Output/Check")))
  expect_true(dir.exists(here::here("Output/Intermediate")))
  expect_true(dir.exists(here::here("Output/Report")))
  expect_true(dir.exists(here::here("Output/Figures")))

  unlink(here::here("Output"), recursive = TRUE)
})
