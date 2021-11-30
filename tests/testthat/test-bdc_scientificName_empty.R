context("scientific name empty")

x <- data.frame(scientificName = c("Ocotea odorifera", NA, "Panthera onca", ""))



test_that("scientific name empty", {
  
  empty_scientific_name <- bdc_scientificName_empty(data = x, sci_names = "scientificName")
  
  expect_equal(empty_scientific_name, data.frame(scientificName = c("Ocotea odorifera", NA, "Panthera onca", ""), 
                                                 .scientificName_empty = c(TRUE, FALSE, TRUE, FALSE)))
  
})


test_that("data.frame test", {
  
  empty_scientific_name <- bdc_scientificName_empty(data = x, sci_names = "scientificName")
  
  expect_equal(class(empty_scientific_name), "data.frame")
  
})
