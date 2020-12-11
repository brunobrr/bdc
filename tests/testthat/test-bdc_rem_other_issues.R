context("rem_other_issues")

sp<- c ("Maria Gina", "maria gina", "maria gina ginais", "maria gina Ginais",
        "maria Gina Ginais", "maria Gina", "MAria", "maria", "Maria") 


test_that("multiplication works", {
  res<-bdc_rem_other_issues(sp)
  
  expect_equal(res, c("Maria gina","Maria gina", "Maria gina ginais","Maria gina ginais",
    "Maria gina ginais", "Maria gina", "Maria", "Maria", "Maria"))
  
})
