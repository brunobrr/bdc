context("bdc_query_names_taxadb")

Sys.setenv("CONTENTID_REGISTRIES"  = "https://hash-archive.thelio.carlboettiger.info")

sci_names <- c("Polystachya estrellensis" , "Tachigali rubiginosa", "Oxalis rhombeo ovata", "Axonopus canescens",
  "Prosopis", "Guapira opposita", "Clidemia naevula", "Poincianella pyramidalis", "Hymenophyllum polyanthos")
   

test <- bdc_query_names_taxadb(sci_names, suggestion.distance = 0.9, db = "gbif")

test_that("bdc_query_names_taxadb", {
 
  expect_equal(length(test$scientificName),  length(sci_names))
}
)

test_that("bdc_get_taxa_taxadb datase order", {
  
  expect_equal(test$original.search,  sci_names)
}
)

