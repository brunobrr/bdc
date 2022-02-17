context("bdc_query_names_taxadb")

## skip("dont run")
skip_on_cran()
skip_if_not_installed("curl")

sci_names <-
  c(
    "Polystachya estrellensis",
    "Tachigali rubiginosa",
    "Oxalis rhombeo ovata",
    "Axonopus canescens",
    "Prosopis",
    "Guapira opposita",
    "Clidemia naevula",
    "Poincianella pyramidalis",
    "Hymenophyllum polyanthos",
    "Puma concolor"
  )


test <- bdc_query_names_taxadb(sci_names, suggestion_distance = 0.9, db = "gbif")

test_that("bdc_query_names_taxadb", {
  expect_equal(length(test$scientificName), length(sci_names))
})

test_that("bdc_get_taxa_taxadb datase order", {
  expect_equal(test$original_search, sci_names)
})


test <- bdc_query_names_taxadb(c("Puma concola", "Cebus apela"),
  suggestion_distance = 0.9,
  db = "gbif",
  suggest_names = FALSE,
  rank_name = "Mammalia",
  rank = "class"
)


expected <- tibble(data.frame(
  original_search = c("Puma concola", "Cebus apela"),
  suggested_name = c(NA, NA),
  distance = c(NA, NA),
  notes = c("notFound", "notFound"),
  taxonID = character(2),
  scientificName = character(2),
  taxonRank = character(2),
  taxonomicStatus = character(2),
  acceptedNameUsageID = character(2),
  kingdom = character(2),
  phylum = character(2),
  class = character(2),
  order = character(2),
  family = character(2),
  genus = character(2),
  specificEpithet = character(2),
  infraspecificEpithet = character(2),
  parentNameUsageID = character(2),
  originalNameUsageID = character(2),
  scientificNameAuthorship = character(2),
  vernacularName = character(2)
))

expected$original_search <- c("Puma concola", "Cebus apela")

for (i in 4:ncol(expected)) {
  expected[, i] <- as.character(c(NA, NA))
}

expected$notes <- rep("notFound", 2)


test_that("suggest_name FALSE", {
  testthat::expect_equal(test, expected)
})
