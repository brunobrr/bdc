database_id <- c("test_1", "test_2", "test_3", "test_4", "test_5")
.missing_names <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
.missing_coordinates <- c(TRUE, FALSE, FALSE, TRUE, FALSE)
.invalid_basis_of_records <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
.summary <- c(TRUE, FALSE, FALSE, FALSE, FALSE)

x <- data.frame(
       database_id,
       .missing_names,
       .missing_coordinates,
       .invalid_basis_of_records,
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

   testthat::expect_equal(dim(report$x$data), c(4, 5))

 })
