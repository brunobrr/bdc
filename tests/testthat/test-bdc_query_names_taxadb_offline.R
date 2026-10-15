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

test_that("bdc_query_names_taxadb() returns one row per name, in input order", {
  local_fake_taxadb()
  sci_names <- c(
    "Puma concolor", "puma concolor", "PUMA CONCOLOR", "Puma Concolor",
    "", NA, "Felis concolor", "puma concolor", NA, ""
  )

  res <- suppressMessages(
    bdc_query_names_taxadb(sci_names, db = "gbif", suggest_names = FALSE)
  )

  expect_equal(nrow(res), length(sci_names))
  expect_identical(res$original_search, sci_names)
  # Exact matching is case-sensitive: only the sentence-case spellings match.
  expect_equal(
    res$scientificName,
    c(
      "Puma concolor", NA, NA, NA, NA, NA, "Puma concolor", NA, NA, NA
    )
  )
  expect_equal(
    res$notes,
    c(
      "accepted", "notFound", "notFound", "notFound", NA, NA,
      "accepted | replaceSynonym", "notFound", NA, NA
    )
  )
})

test_that("bdc_query_names_taxadb() keeps case variants aligned when suggesting names", {
  local_fake_taxadb()
  # Run fuzzy matching sequentially instead of on a cluster.
  local_mocked_bindings(
    makeCluster = function(...) NULL,
    stopCluster = function(...) invisible(NULL),
    .package = "parallel"
  )
  local_mocked_bindings(
    registerDoParallel = function(...) foreach::registerDoSEQ(),
    .package = "doParallel"
  )
  sci_names <- c(
    "Puma concolor", "puma concolor", "PUMA CONCOLOR", "Puma Concolor", NA
  )

  res <- suppressMessages(bdc_query_names_taxadb(sci_names, db = "gbif"))

  expect_equal(nrow(res), length(sci_names))
  expect_identical(res$original_search, sci_names)
  expect_equal(res$notes[1], "accepted")
})
