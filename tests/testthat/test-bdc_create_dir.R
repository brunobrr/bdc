context("creating directories")

if (!require("here")) install.packages("here")

Output_path <- here::here("Output")
Output <- file.exists(here::here("Output"))

test_that("file exist", {
  if(isFALSE(Output)){
    "folder Output not found. Please run the function bdc::bdc_create_dir"
  }
})

