

sp<- c(" ", "João Carlos", "ANA", NA, "NA", "", "Fred")
long <- c(20.02, 181.00, 20.02, 20.02, 20.02, 20.02, 20.02)
lat <- c("Ontem", "20.02", "NA", NA, ",", "20.2", "-92")
e <- data.frame(sp,long, lat)



test_that("bdc_flag_missing_names removes NA", {
  res<-bdc_flag_missing_names(data= e, sci_name="sp")
  expect_equal(res,c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE) )
})

