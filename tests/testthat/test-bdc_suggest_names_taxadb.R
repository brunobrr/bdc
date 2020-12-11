context("suggest names")

sci_names <- c("Janusia occhioni", "Janusia", "Xilosma ciliatifolium",  "Crataeva benthamii", "Oxalis rhombeo ovata", "Cebus apella", "Puma concolar") 

test_that("bdc_suggest_names_taxadb suggest valid names", {
  res <- bdc_suggest_names_taxadb(sci_name = sci_names, provider = "gbif") 
  expect_equal(res,data.frame(suggested = c("Janusia occhionii", 
                                "Janusia", 
                                "Xylosma ciliatifolium", 
                                "Crateva benthamii", 
                                "Oxalis rhombeoovata", 
                                "Cebus apella", 
                                "Puma concolor"), 
                               distance = c(0.94, 1.00, 0.95, 0.94, 0.95, 1.00,0.92)
                              )
  )
}
)
