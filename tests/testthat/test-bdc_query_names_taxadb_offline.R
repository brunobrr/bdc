# Offline tests for bdc_query_names_taxadb(): taxadb is replaced by the fake
# boundary in helper-taxadb.R, so these run without network or downloads.
# The network-backed integration tests live in test-bdc_query_names_taxadb.R.

test_that("bdc_query_names_taxadb() harmonizes sentence-case names offline", {
  local_fake_taxadb()
  sci_names <- c("Puma concolor", "Felis concolor", "Puma concolor")

  res <- suppressMessages(bdc_query_names_taxadb(sci_names, db = "gbif"))

  # Expected values match the real gbif 22.12 result for these names.
  expect_equal(nrow(res), 3)
  expect_equal(res$original_search, sci_names)
  expect_equal(res$scientificName, rep("Puma concolor", 3))
  expect_equal(res$taxonomicStatus, rep("accepted", 3))
  expect_equal(res$acceptedNameUsageID, rep("GBIF:2435099", 3))
  expect_equal(
    res$notes,
    c("accepted", "accepted | replaceSynonym", "accepted")
  )
  expect_equal(
    names(res),
    c(
      "original_search", "suggested_name", "distance", "notes", "taxonID",
      "scientificName", "taxonRank", "taxonomicStatus", "acceptedNameUsageID",
      "kingdom", "phylum", "class", "order", "family", "genus",
      "specificEpithet", "infraspecificEpithet", "parentNameUsageID",
      "originalNameUsageID", "scientificNameAuthorship", "vernacularName"
    )
  )
})
