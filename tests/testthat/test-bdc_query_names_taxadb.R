context("bdc_query_names_taxadb")

sci_names <- c("Polystachya estrellensis" , "Tachigali rubiginosa", "Oxalis rhombeo ovata", "Axonopus canescens",
  "Prosopis", "Guapira opposita", "Clidemia naevula", "Poincianella pyramidalis", "Hymenophyllum polyanthos")
   

test <- bdc_query_names_taxadb(sci_names, suggestion_distance = 0.9, db = "gbif")

test_that("bdc_query_names_taxadb", {
 
  expect_equal(length(test$scientificName),  length(sci_names))
}
)

test_that("bdc_get_taxa_taxadb datase order", {
  
  expect_equal(test$original_search,  sci_names)
}
)

