context("creating directories")

withr::with_dir(
  new = ".",
  code = {

    bdc_create_dir()

    test_that("bdc dir exists", {

      expect_true(dir.exists(here::here("Output/Check")))
      expect_true(dir.exists(here::here("Output/Intermediate")))
      expect_true(dir.exists(here::here("Output/Report")))
      expect_true(dir.exists(here::here("Output/Figures")))

    })

  }
)
