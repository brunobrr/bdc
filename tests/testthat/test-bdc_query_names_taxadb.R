context("bdc_query_names_taxadb")

skip("dont run")

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
  expect_equal(length(test$scientificName),  length(sci_names))
})

test_that("bdc_get_taxa_taxadb datase order", {
  expect_equal(test$original_search,  sci_names)
}
)


test_that("suggest_name FALSE", {

  test <- bdc_query_names_taxadb(c("Puma concola", "Cebus apela"),
                               suggestion_distance = 0.9,
                               db = "gbif",
                               suggest_names = FALSE,
                               rank_name = "Mammalia",
                               rank = "class")


expected <- tibble(data.frame(original_search = c("Puma concola", "Cebus apela"),
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

for(i in 4:ncol(expected)){
  expected[, i] <- as.character(c(NA, NA))
}

expected$notes <- rep("notFound", 2) 


test_that("suggest_name FALSE", {
  
  testthat::expect_equal(test, expected)

}
)

# 1. itis: not working in bdc
test_itis <- bdc_query_names_taxadb(sci_names, suggestion_distance = 0.9, db = "itis")
itis <- taxadb::taxa_tbl("itis")
length(itis %>% pull(scientificName)) # tamanho da base 871435
test_itis_taxadb <- taxadb::filter_name(sci_names, provider = "itis")
colnames_itis <- colnames(itis) # 17
setdiff(colnames_gbif, colnames_itis)


# 2. ncbi: OK
test_ncbi <- bdc_query_names_taxadb(sci_names, suggestion_distance = 0.9, db = "ncbi")
ncbi <- taxadb::taxa_tbl("ncbi")
length(ncbi %>% pull(scientificName)) # tamanho da base 3461657
test_ncbi_taxadb <- taxadb::filter_name(sci_names, provider = "ncbi")
colnames_ncbi <-  colnames(ncbi) # 13


# 3. col: OK
test_col <- bdc_query_names_taxadb(sci_names, suggestion_distance = 0.9, db = "col")
col <- taxadb::taxa_tbl("col")
length(col %>% pull(scientificName)) # tamanho da base 3461657
test_col_taxadb <- taxadb::filter_name(sci_names, provider = "col")
colnames_col <-  colnames(col) # 13


# 4. gbif: OK
test_gbif <- bdc_query_names_taxadb(sci_names, suggestion_distance = 0.9, db = "gbif")
gbif <- taxa_tbl("gbif")
length(gbif %>% pull(scientificName)) # tamanho da base 6484800
colnames_gbif <- colnames(gbif) #17


# 5. tpl: (aparentemente há um problema com a versão da base de dados)
test_tpl <- bdc_query_names_taxadb(sci_names, suggestion_distance = 0.9, db = "tpl")
tpl <- taxadb::taxa_tbl("tpl")
length(tpl %>% pull(scientificName)) # tamanho da base 3461657
test_tpl_taxadb <- taxadb::filter_name(sci_names, provider = "tpl")
colnames_col <-  colnames(tpl) # 13
taxadb::td_create(provider = "tpl", schema = "dwc", version = 2019)


# 6. fb (could not find 2022_dwc_fb checking for older versions. 
# 2022_dwc_fb not available)
test_gbif <- bdc_query_names_taxadb(sci_names, suggestion_distance = 0.9, db = "fb")
# taxadb::td_create(provider = "tpl", schema = "dwc", version = 2019)


# 7. slb (could not find 2022_dwc_slb checking for older versions. 
# 2022_dwc_slb not available)
test_slb <- bdc_query_names_taxadb(sci_names, suggestion_distance = 0.9, db = "slb")

# 8. wd (could not find 2022_dwc_slb checking for older versions. 
# 2022_dwc_slb not available)
test_wd <- bdc_query_names_taxadb(sci_names, suggestion_distance = 0.9, db = "wd")

# 9. ott (baixou, mas há algum problema no bdc)
# Error in { : task 2 failed - "valor ausente onde TRUE/FALSE necessário"
test_ott <- bdc_query_names_taxadb(sci_names, suggestion_distance = 0.9, db = "ott")

# 9. iucn (baixou, mas há algum problema no bdc)
test_iucn <- bdc_query_names_taxadb(sci_names, suggestion_distance = 0.9, db = "iucn")
taxadb::td_create(provider = "iucn", schema = "dwc", version = 2019)

