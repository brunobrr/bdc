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

test <-
  bdc_query_names_taxadb(
    sci_name = sci_names,
    suggestion_distance = 0.9,
    db = "gbif",
    export_accepted = FALSE
  )

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
  rank = "class",
  export_accepted = FALSE
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


## NOTE 2023-02-25: This modification is based on latest release of `{taxadb}` 0.2.0.
## CHECK 2023-02-25: https://github.com/ropensci/taxadb/commit/593c7856a603c802762829d60acb2a313ad7a6dd

## Testing database availability

test_that("availability of itis", {

  query_itis <- try(bdc_query_names_taxadb(sci_name = sci_names, db = "itis"))

  expect_true(class(query_itis)[3] == "data.frame")

})

## FIXME 2023-02-25: Consciously ignoring that database for now.
## test_that("availability of ncbi", {
##   query_ncbi <- try(bdc_query_names_taxadb(sci_name = sci_names, db = "ncbi"))
##   expect_true(class(query_ncbi)[3] == "data.frame")
## })

test_that("availability of col", {

  query_col <- try(bdc_query_names_taxadb(sci_name = sci_names, db = "col"))

  expect_true(class(query_col)[3] == "data.frame")

})

# test_that("availability of tpl", {
# 
#   query_tpl <- try(bdc_query_names_taxadb(sci_name = sci_names, db = "tpl"))
# 
#   expect_true(class(query_tpl) == "try-error")
# 
# })

test_that("availability of gbif", {

  query_gbif <- try(bdc_query_names_taxadb(sci_name = sci_names, db = "gbif"))

  expect_true(class(query_gbif)[3] == "data.frame")

})

# test_that("availability of fb", {
# 
#   query_fb <- try(bdc_query_names_taxadb(sci_name = sci_names, db = "fb"))
# 
#   expect_true(class(query_fb) == "try-error")
# 
# })

# test_that("availability of slb", {
# 
#   query_slb <- try(bdc_query_names_taxadb(sci_name = sci_names, db = "slb"))
# 
#   expect_true(class(query_slb) == "try-error")
# 
# })

# test_that("availability of wd", {
# 
#   query_wd <- try(bdc_query_names_taxadb(sci_name = sci_names, db = "wd"))
# 
#   expect_true(class(query_wd) == "try-error")
# 
# })

## ## FIXME 2023-02-25: Consciously ignoring that database for now.
## test_that("availability of ott", {
##   query_ott <- try(bdc_query_names_taxadb(sci_name = sci_names, db = "ott"))
##   expect_true(class(query_ott)[3] == "data.frame")
## })

# test_that("availability of iucn", {
# 
#   query_iucn <- try(bdc_query_names_taxadb(sci_name = sci_names, db = "iucn"))
# 
#   expect_true(class(query_iucn) == "try-error")
# 
# })

