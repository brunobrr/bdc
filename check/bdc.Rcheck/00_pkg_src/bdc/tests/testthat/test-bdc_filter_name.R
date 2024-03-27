## skip("dont run")
skip_on_cran()
skip_if_not_installed("curl")

sci_names <- c(
  "Polystachya estrellensis", "Tachigali rubiginosa", "Oxalis rhombeo ovata", "Axonopus canescens",
  "Prosopis", "Guapira opposita", "Clidemia naevula", "Poincianella pyramidalis", "Hymenophyllum polyanthos"
)

expected_cols <- c(
  "sort", "taxonID", "scientificName", "taxonRank",
  "taxonomicStatus", "acceptedNameUsageID", "kingdom", "phylum",
  "class", "order", "family", "genus", "specificEpithet", "infraspecificEpithet",
  "parentNameUsageID", "originalNameUsageID", "scientificNameAuthorship",
  "vernacularName", "input"
)

x <- bdc_filter_name("Puma concolor", db = "gbif")


test_that("testing filter name columns", {
  testthat::expect_equal(colnames(x), expected_cols)
})

x <- bdc_filter_name("Puma concoloar", db = "gbif")

test_that("testing not found names", {
  testthat::expect_equal(is.na(x$scientificName), TRUE)

  testthat::expect_equal(is.numeric(x$sort), TRUE)
})

x <- bdc_filter_name(sci_names, db = "gbif")

test_that("testing multiple names", {
  testthat::expect_equal(class(x), c("tbl_df", "tbl", "data.frame"))

  testthat::expect_equal(nrow(x) > 1, TRUE)
})
