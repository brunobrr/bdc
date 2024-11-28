skip_on_ci()
skip_on_cran()

database_id <- c("GBIF_01", "GBIF_02", "GBIF_03", "FISH_04", "FISH_05")
 lat <- c(-19.93580, -13.01667, -22.34161, -6.75000, -15.15806)
 lon <- c(-40.60030, -39.60000, -49.61017, -35.63330, -39.52861)
 .scientificName_emptys <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
 .coordinates_empty <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
 .invalid_basis_of_records <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
 .summary <- c(TRUE, FALSE, TRUE, FALSE, FALSE)

 x <- data.frame(
   database_id,
   lat,
   lon,
   .scientificName_emptys,
   .coordinates_empty,
   .invalid_basis_of_records,
   .summary
 )

figures <-
 bdc_create_figures(
   data = x,
   database_id = "database_id",
   workflow_step = "prefilter",
   save_figures = FALSE
 )

test_that("test figures object", {

  testthat::expect_equal(names(figures), c(".invalid_basis_of_records", ".summary", "summary_all_tests"))

})
