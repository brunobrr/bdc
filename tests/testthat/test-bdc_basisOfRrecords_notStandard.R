test_that("correct use of artuments", {
  require(testthat)
  
  x <- data.frame(
    basisOfRecord = c(
      "FOSSIL_SPECIMEN",
      "UNKNOWN",
      "RON",
      NA,
      "Specimen",
      "PRESERVED_SPECIMEN"
    )
  )
  
  testthat::expect_message(
    r <- bdc_basisOfRrecords_notStandard(data = x,
                                         basisOfRecord = "basisOfRecord", names_to_keep = "all")
    
  )
  
  r <- bdc_basisOfRrecords_notStandard(data = x,
                                       basisOfRecord = "basisOfRecord", names_to_keep = "all")
  
  testthat::expect_equal(ncol(r), 2)
  })
