context("flag_xy_provenance")

sp<- c("Maria", "João Victor", "Alex", "Marina teste", "Lorena pislum", "Renato flores", "Mones")
prov<-c("HUMAN_OBSERVATION", "O", "0", NA, "", "Unknown", "armadilha")
e<-data.frame(sp, prov)



test_that("xy_provenance for default names_to_keep", {
  res<-bdc_flag_xy_provenance (e,basisOfRecord = "prov", 
                          names_to_keep = NULL )
  expect_equal(res, c(TRUE,  TRUE, FALSE,  TRUE,  TRUE, FALSE, FALSE))
  
})


names_keep<-c("Unknown", "armadilha","HUMAN_OBSERVATION", NA)


test_that("xy_provenance for user defined names_to_keep", {
  res<-bdc_flag_xy_provenance (e,basisOfRecord = "prov", 
                               names_to_keep = names_keep ) 
  expect_equal(res, c(TRUE, FALSE, FALSE,  TRUE, FALSE,  TRUE,  TRUE))
  
})

