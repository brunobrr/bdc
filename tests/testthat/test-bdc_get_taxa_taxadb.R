context("bdc_get_taxa_taxadb")

sci_names <- c("Polystachya estrellensis" , "Tachigali rubiginosa", "Oxalis rhombeo ovata", "Axonopus canescens",
  "Prosopis", "Guapira opposita", "Clidemia naevula", "Poincianella pyramidalis", "Hymenophyllum polyanthos")
   

test <- bdc_get_taxa_taxadb(sci_names, suggestion.distance = 0.9, db = "gbif")

test_that("bdc_get_taxa_taxadb datase size", {
 
  expect_equal(length(test$scientificName),  length(sci_names))
}
)

test_that("bdc_get_taxa_taxadb datase order", {
  
  expect_equal(test$original.search,  sci_names)
}
)









#test all input names are returned