context("missing_xy")

sp<- c(" ", "João Carlos", "ANA", NA, "NA", "", "Fred")
long <- c(20.02, 181.00, 20.02, 20.02, 20.02, 20.02, 20.02)
lat <- c("Ontem", "20.02", "NA", NA, ",", "20.2", "-92")
e <- data.frame(sp,long, lat)



test_that("flag NAs and missing", {
  res<-bdc_flag_missing_xy(data=e, lon= "long" , lat= "lat")
  expect_equal(res, c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE))
})
