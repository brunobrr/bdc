context("rem_family_names")


sp<- c ("Fabaceae Maria gina", "Maria gina fabaceae", "Maria gina",
        "FABACEAE Maria gina","Leguminosae Maria gina", "LEGUMINOSAE Maria gina",
        "Maria gina LEGUMINOSAE ", "Maria gina Compositae", "Maria gina COMPOSITAE",
        "Maria gina Compositae")


test_that("removing famuily names is working", {
  res<-bdc_rem_family_names(sp)
  res<- res[,2]
  expect_equal(res, c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,FALSE, FALSE))
  
})
